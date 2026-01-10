program JSONParserWithPointers;

{$mode Delphi}
{$CODEPAGE UTF8}

uses
  SysUtils;

const
  MAX_JSON_DEPTH  =  16;   { 最深嵌套 32 层，超过一般接口 10 倍 }
  MAX_STRING_LEN  = 255;   { 单段字符串 ≤255 字符，UTF-8 汉字 80 个左右 }
  MAX_ARRAY_SIZE  = 16;   { 数组元素 ≤256 个 }
  MAX_OBJECT_KEYS = 32;   { 单个对象键值对 ≤256 个 }
  HASH_TABLE_SIZE = 37;   { 质数，略大于 MAX_OBJECT_KEYS，降低冲突 }
  MAX_VALUES      = 32;  { 总 JSON 值节点 ≤1 K，足够容纳 4 层嵌套的中等报文 }
  MAX_HASH_NODES  = 32;  { 总哈希节点 ≤1 K，与 MAX_VALUES 1:1 预留 }
  MAX_INPUT_LEN   = 1024;  { 原始文本 ≤4 KB，约 2 页 A4 英文 }

type
  { JSON值类型枚举}
  TJSONType = (jtNull, jtBoolean, jtNumber, jtString, jtArray, jtObject);
  
  { 字符串类型}
  TJSONString = array[1..MAX_STRING_LEN] of Char;
  TInputBuffer = array[1..MAX_INPUT_LEN] of Char; { 输入缓冲区}
  
  { JSON数组}
  TJSONArray = record
    Items: array[1..MAX_ARRAY_SIZE] of Integer; { 存储值的索引，而不是指针}
    Count: Integer;
    
    { 添加元素到数组}
    procedure Add(ItemIndex: Integer);
    { 清空数组}
    procedure Clear();
  end;
  
  { Hash表中的键值对节点}
  THashNode = record
    Key: TJSONString;
    KeyLen: Integer;
    ValueIndex: Integer; { 存储值的索引，而不是指针}
    Next: Integer; { 存储下一个节点的索引，而不是指针}
  end;
  
  { JSON对象 (使用hash表实现)}
  TJSONObject = record
    HashTable: array[0..HASH_TABLE_SIZE-1] of Integer; { 存储节点索引，而不是指针}
    Count: Integer;
    
    { 添加键值对到对象}
    procedure Add(const Key: TJSONString; KeyLen: Integer; ValueIndex: Integer);
    { 清空对象}
    procedure Clear();
  end;
  
  { JSON值}
  TJSONValue = record
    JSONType: TJSONType;
    case TJSONType of
      jtNull: ();
      jtBoolean: (BoolValue: Boolean);
      jtNumber: (NumValue: Real);
      jtString: (StrValue: TJSONString; StrLen: Integer);
      jtArray: (ArrayValue: TJSONArray);
      jtObject: (ObjectValue: TJSONObject);
  end;
  
  { 词法分析器状态}
  TLexer = record
    Input: TInputBuffer; { 改为字符数组}
    Position: Integer;
    Length: Integer;
    CurrentChar: Char;
    CurrentDepth: Integer; { 添加深度跟踪}
    
    { 前进到下一个字符}
    procedure NextChar();
    { 跳过指定长度的字符}
    procedure SkipChars(Count: Integer);
    { 跳过空白字符}
    procedure SkipWhitespace();
    { 检查剩余字符串是否匹配}
    function CheckString(const Str: string): Boolean;
  end;

{ 全局变量}
var
  ParseError: Boolean;
  ErrorMessage: String;
  JSONValues: array[1..MAX_VALUES] of TJSONValue; { 静态分配的JSON值存储}
  HashNodes: array[1..MAX_HASH_NODES] of THashNode; { 静态分配的哈希节点存储}
  ValueCount: Integer; { 当前使用的JSON值数量}
  HashNodeCount: Integer; { 当前使用的哈希节点数量}
  FreeValueIndices: array[1..MAX_VALUES] of Integer; { 可用的JSON值索引}
  FreeHashNodeIndices: array[1..MAX_HASH_NODES] of Integer; { 可用的哈希节点索引}
  FreeValueCount: Integer; { 可用的JSON值索引数量}
  FreeHashNodeCount: Integer; { 可用的哈希节点索引数量}

  { 主程序入口点}
  TestJSON: String;
  ParsedValueIndex: Integer;

  StartTime, EndTime: Integer;
  ElapsedTime: Integer;

{ 前向声明}
function HashString(const Str: TJSONString; Len: Integer): Integer; forward;
function StrEqual(const S1: TJSONString; Len1: Integer; const S2: TJSONString; Len2: Integer): Boolean; forward;
function StrCopyCustom(const Src: TJSONString; SrcLen: Integer; var Dest: TJSONString): Integer; forward;
procedure FreeJSONValue(ValueIndex: Integer); forward;
function NewHashNode(): Integer; forward;

{ TJSONArray 方法实现}
procedure TJSONArray.Add(ItemIndex: Integer);
begin
  if (Count < MAX_ARRAY_SIZE) and (ItemIndex > 0) then
  begin
    Inc(Count);
    Items[Count] := ItemIndex;
  end;
end;

procedure TJSONArray.Clear();
var
  i: Integer;
begin
  Count := 0;
  for i := 1 to MAX_ARRAY_SIZE do
    Items[i] := 0;
end;

{ TJSONObject 方法实现}
procedure TJSONObject.Add(const Key: TJSONString; KeyLen: Integer; ValueIndex: Integer);
var
  Hash: Integer;
  NodeIndex, NewNodeIndex: Integer;
begin
  if (KeyLen <= 0) or (KeyLen > MAX_STRING_LEN) or (ValueIndex < 0) then
    Exit();
    
  Hash := HashString(Key, KeyLen);
  NodeIndex := HashTable[Hash];
  
  { 检查键是否已存在}
  while NodeIndex > 0 do
  begin
    if StrEqual(HashNodes[NodeIndex].Key, HashNodes[NodeIndex].KeyLen, Key, KeyLen) then
    begin
      { 释放旧值并更新}
      if HashNodes[NodeIndex].ValueIndex > 0 then
        FreeJSONValue(HashNodes[NodeIndex].ValueIndex);
      HashNodes[NodeIndex].ValueIndex := ValueIndex;
      Exit();
    end;
    NodeIndex := HashNodes[NodeIndex].Next;
  end;
  
  { 添加新节点}
  NewNodeIndex := NewHashNode();
  if NewNodeIndex < 0 then
    Exit();
    
  HashNodes[NewNodeIndex].KeyLen := StrCopyCustom(Key, KeyLen, HashNodes[NewNodeIndex].Key);
  HashNodes[NewNodeIndex].ValueIndex := ValueIndex;
  HashNodes[NewNodeIndex].Next := HashTable[Hash];
  HashTable[Hash] := NewNodeIndex;
  Inc(Count);
end;

procedure TJSONObject.Clear();
var
  i: Integer;
begin
  Count := 0;
  for i := 0 to HASH_TABLE_SIZE - 1 do
    HashTable[i] := 0;
end;

{ TLexer 方法实现}
procedure TLexer.NextChar();
begin
  Inc(Position);
  if Position >= Length then
    CurrentChar := #0  { #0 = NUL }
  else
    CurrentChar := Input[Position + 1]; { 数组索引从1开始}
end;

procedure TLexer.SkipChars(Count: Integer);
var
  i: Integer;
begin
  for i := 1 to Count do
  begin
    NextChar();
    if CurrentChar = #0 then  { #0 = NUL }
      Break;
  end;
end;

procedure TLexer.SkipWhitespace();
begin
  while (CurrentChar = #32) or (CurrentChar = #9) or  { #32 = ' ', #9 = TAB }
        (CurrentChar = #10) or (CurrentChar = #13) do  { #10 = LF, #13 = CR }
    NextChar();
end;

function TLexer.CheckString(const Str: string): Boolean;
var
  i: Integer;
  TempPos: Integer;
begin
  if Position + System.Length(Str) > Self.Length then
  begin
    CheckString := False;
    Exit();
  end;
  
  TempPos := Position;
  for i := 1 to System.Length(Str) do
  begin
    if Input[TempPos + 1] <> Str[i] then { 数组索引从1开始}
    begin
      CheckString := False;
      Exit();
    end;
    Inc(TempPos);
  end;
  
  CheckString := True;
end;

{ 初始化内存管理}
procedure InitMemoryManager();
var
  i: Integer;
begin
  ValueCount := 0;
  HashNodeCount := 0;
  FreeValueCount := 0;
  FreeHashNodeCount := 0;
  
  { 初始化所有值为未使用状态}
  for i := 1 to MAX_VALUES do
  begin
    JSONValues[i].JSONType := jtNull; { 标记为未使用}
  end;
  
  { 初始化所有哈希节点为未使用状态}
  for i := 1 to MAX_HASH_NODES do
  begin
    HashNodes[i].KeyLen := -1; { 标记为未使用}
  end;
end;

{ 设置错误信息}
procedure SetError(const Msg: String);
begin
  ParseError := True;
  ErrorMessage := Msg;
end;

{ 工具函数}
function StrCopyCustom(const Src: TJSONString; SrcLen: Integer; var Dest: TJSONString): Integer;
var
  i: Integer;
begin
  if SrcLen > MAX_STRING_LEN then
    SrcLen := MAX_STRING_LEN;
    
  for i := 1 to SrcLen do
    Dest[i] := Src[i];
  
  { 清零剩余部分}
  for i := SrcLen + 1 to MAX_STRING_LEN do
    Dest[i] := #0;  { #0 = NUL }
    
  StrCopyCustom := SrcLen;
end;

function StrEqual(const S1: TJSONString; Len1: Integer; const S2: TJSONString; Len2: Integer): Boolean;
var
  i: Integer;
begin
  if Len1 <> Len2 then
  begin
    StrEqual := False;
    Exit();
  end;
  
  for i := 1 to Len1 do
  begin
    if S1[i] <> S2[i] then
    begin
      StrEqual := False;
      Exit();
    end;
  end;
  
  StrEqual := True;
end;

{ 简单的hash函数}
function HashString(const Str: TJSONString; Len: Integer): Integer;
var
  i: Integer;
  Hash: Integer;
begin
  Hash := 0;
  for i := 1 to Len do
    Hash := ((Hash * 31) + Ord(Str[i])) mod HASH_TABLE_SIZE;
  if Hash < 0 then
    Hash := -Hash;
  HashString := Hash;
end;

{ 释放JSON值}
procedure FreeJSONValue(ValueIndex: Integer);
var
  i: Integer;
  NodeIndex, NextNodeIndex: Integer;
begin
  if (ValueIndex < 1) or (ValueIndex > MAX_VALUES) then
    Exit();
    
  case JSONValues[ValueIndex].JSONType of
    jtArray:
    begin
      for i := 1 to JSONValues[ValueIndex].ArrayValue.Count do
      begin
        if (i <= MAX_ARRAY_SIZE) and (JSONValues[ValueIndex].ArrayValue.Items[i] > 0) then
          FreeJSONValue(JSONValues[ValueIndex].ArrayValue.Items[i]);
      end;
      JSONValues[ValueIndex].ArrayValue.Clear();
    end;
    
    jtObject:
    begin
      for i := 0 to HASH_TABLE_SIZE - 1 do
      begin
        NodeIndex := JSONValues[ValueIndex].ObjectValue.HashTable[i];
        while NodeIndex > 0 do
        begin
          NextNodeIndex := HashNodes[NodeIndex].Next;
          if HashNodes[NodeIndex].ValueIndex > 0 then
            FreeJSONValue(HashNodes[NodeIndex].ValueIndex);
          
          { 将哈希节点标记为未使用}
          HashNodes[NodeIndex].KeyLen := -1;
          
          { 添加到空闲列表}
          Inc(FreeHashNodeCount);
          if FreeHashNodeCount <= MAX_HASH_NODES then
            FreeHashNodeIndices[FreeHashNodeCount] := NodeIndex;
            
          NodeIndex := NextNodeIndex;
        end;
      end;
      JSONValues[ValueIndex].ObjectValue.Clear();
    end;
  end;
  
  { 将值标记为未使用}
  JSONValues[ValueIndex].JSONType := jtNull;
  
  { 添加到空闲列表}
  Inc(FreeValueCount);
  if FreeValueCount <= MAX_VALUES then
    FreeValueIndices[FreeValueCount] := ValueIndex;
end;

{ 创建新的JSON值}
function NewJSONValue(JSONType: TJSONType): Integer;
var
  ValueIndex: Integer;
  i: Integer;
begin
  { 首先检查是否有可用的空闲索引}
  if FreeValueCount > 0 then
  begin
    ValueIndex := FreeValueIndices[FreeValueCount];
    Dec(FreeValueCount);
  end
  else
  begin
    { 没有空闲索引，分配新的}
    Inc(ValueCount);
    if ValueCount > MAX_VALUES then
    begin
      SetError('Out of memory for JSON values');
      NewJSONValue := -1;
      Exit();
    end;
    ValueIndex := ValueCount;
  end;
  
  JSONValues[ValueIndex].JSONType := JSONType;
  
  case JSONType of
    jtNull: ;
    jtBoolean: JSONValues[ValueIndex].BoolValue := False;
    jtNumber: JSONValues[ValueIndex].NumValue := 0.0;
    jtString: 
    begin
      JSONValues[ValueIndex].StrLen := 0;
      for i := 1 to MAX_STRING_LEN do
        JSONValues[ValueIndex].StrValue[i] := #0;  { #0 = NUL }
    end;
    jtArray: 
    begin
      JSONValues[ValueIndex].ArrayValue.Clear();
    end;
    jtObject: 
    begin
      JSONValues[ValueIndex].ObjectValue.Clear();
    end;
  end;
  
  NewJSONValue := ValueIndex;
end;

{ 创建新的哈希节点}
function NewHashNode(): Integer;
var
  NodeIndex: Integer;
begin
  { 首先检查是否有可用的空闲索引}
  if FreeHashNodeCount > 0 then
  begin
    NodeIndex := FreeHashNodeIndices[FreeHashNodeCount];
    Dec(FreeHashNodeCount);
  end
  else
  begin
    { 没有空闲索引，分配新的}
    Inc(HashNodeCount);
    if HashNodeCount > MAX_HASH_NODES then
    begin
      SetError('Out of memory for hash nodes');
      NewHashNode := -1;
      Exit();
    end;
    NodeIndex := HashNodeCount;
  end;
  
  NewHashNode := NodeIndex;
end;

{ 初始化词法分析器}
procedure InitLexer(var Lexer: TLexer; const Input: String; Len: Integer);
var
  i: Integer;
begin
  Lexer.Position := 0;
  Lexer.Length := Len;
  Lexer.CurrentDepth := 0;
  
  { 将字符串复制到字符数组}
  if Len > MAX_INPUT_LEN then
    Lexer.Length := MAX_INPUT_LEN;
    
  for i := 1 to Lexer.Length do
    Lexer.Input[i] := Input[i];
    
  { 填充剩余部分为空字符}
  for i := Lexer.Length + 1 to MAX_INPUT_LEN do
    Lexer.Input[i] := #0;  { #0 = NUL }
    
  if Len > 0 then
    Lexer.CurrentChar := Lexer.Input[1]
  else
    Lexer.CurrentChar := #0;  { #0 = NUL }
end;

{ 检查是否为有效的十六进制数字}
function IsHexDigit(c: Char): Boolean;
begin
  IsHexDigit := (c >= #48) and (c <= #57) or  { #48 = '0', #57 = '9' }
               (c >= #65) and (c <= #70) or  { #65 = 'A', #70 = 'F' }
               (c >= #97) and (c <= #102);   { #97 = 'a', #102 = 'f' }
end;

{ 将十六进制字符转换为数值}
function HexValue(c: Char): Integer;
begin
  if (c >= #48) and (c <= #57) then  { #48 = '0', #57 = '9' }
    HexValue := Ord(c) - Ord(#48)  { #48 = '0' }
  else if (c >= #65) and (c <= #70) then  { #65 = 'A', #70 = 'F' }
    HexValue := Ord(c) - Ord(#65) + 10  { #65 = 'A' }
  else if (c >= #97) and (c <= #102) then  { #97 = 'a', #102 = 'f' }
    HexValue := Ord(c) - Ord(#97) + 10  { #97 = 'a' }
  else
    HexValue := 0;
end;

{ 解析Unicode转义序列}
function ParseUnicodeEscape(var Lexer: TLexer): Char;
var
  i, Value: Integer;
begin
  Value := 0;
  for i := 1 to 4 do
  begin
    if not IsHexDigit(Lexer.CurrentChar) then
    begin
      SetError('Invalid unicode escape sequence');
      ParseUnicodeEscape := #0;  { #0 = NUL }
      Exit();
    end;
    Value := Value * 16 + HexValue(Lexer.CurrentChar);
    Lexer.NextChar();
  end;
  
  { 简化处理，只处理ASCII范围的字符}
  if Value < 128 then
    ParseUnicodeEscape := Chr(Value)
  else
    ParseUnicodeEscape := #63;  { #63 = '?' }
end;

{ 解析字符串}
function ParseString(var Lexer: TLexer; var Str: TJSONString; var Len: Integer): Boolean;
begin
  Len := 0;
  if Lexer.CurrentChar <> #34 then  { #34 = '"' }
  begin
    SetError('Expected string');
    ParseString := False;
    Exit();
  end;
  
  Lexer.NextChar(); { 跳过开始的引号}
  
  while (Lexer.CurrentChar <> #34) and (Lexer.CurrentChar <> #0) and (Len < MAX_STRING_LEN) do  { #34 = '"', #0 = NUL }
  begin
    if Lexer.CurrentChar = #92 then  { #92 = '\' }
    begin
      Lexer.NextChar();
      if Lexer.CurrentChar = #0 then  { #0 = NUL }
      begin
        SetError('Unexpected end of string');
        ParseString := False;
        Exit();
      end;
      
      case Lexer.CurrentChar of
        #34: begin  { #34 = '"' }
          Inc(Len);
          if Len <= MAX_STRING_LEN then Str[Len] := #34;  { #34 = '"' }
        end;
        #92: begin  { #92 = '\' }
          Inc(Len);
          if Len <= MAX_STRING_LEN then Str[Len] := #92;  { #92 = '\' }
        end;
        #47: begin  { #47 = '/' }
          Inc(Len);
          if Len <= MAX_STRING_LEN then Str[Len] := #47;  { #47 = '/' }
        end;
        #98: begin  { #98 = 'b' }
          Inc(Len);
          if Len <= MAX_STRING_LEN then Str[Len] := #8;  { #8 = Backspace }
        end;
        #102: begin  { #102 = 'f' }
          Inc(Len);
          if Len <= MAX_STRING_LEN then Str[Len] := #12;  { #12 = Form Feed }
        end;
        #110: begin  { #110 = 'n' }
          Inc(Len);
          if Len <= MAX_STRING_LEN then Str[Len] := #10;  { #10 = Line Feed }
        end;
        #114: begin  { #114 = 'r' }
          Inc(Len);
          if Len <= MAX_STRING_LEN then Str[Len] := #13;  { #13 = Carriage Return }
        end;
        #116: begin  { #116 = 't' }
          Inc(Len);
          if Len <= MAX_STRING_LEN then Str[Len] := #9;  { #9 = Tab }
        end;
        #117: begin  { #117 = 'u' }
          Lexer.NextChar();
          Inc(Len);
          if Len <= MAX_STRING_LEN then 
            Str[Len] := ParseUnicodeEscape(Lexer);
          Continue; { 已经在ParseUnicodeEscape中前进了字符}
        end;
        else
        begin
          Inc(Len);
          if Len <= MAX_STRING_LEN then 
            Str[Len] := Lexer.CurrentChar;
        end;
      end;
    end
    else
    begin
      Inc(Len);
      if Len <= MAX_STRING_LEN then
        Str[Len] := Lexer.CurrentChar;
    end;
    Lexer.NextChar();
  end;
  
  if Lexer.CurrentChar = #34 then  { #34 = '"' }
  begin
    Lexer.NextChar(); { 跳过结束的引号}
    ParseString := True;
  end
  else
  begin
    SetError('Unterminated string');
    ParseString := False;
  end;
end;

{ 解析数字 - 改进版支持科学计数法}
function ParseNumber(var Lexer: TLexer; var Num: Real): Boolean;
var
  Sign: Integer;
  IntPart, FracPart: Real;
  FracDiv: Real;
  ExpSign, Exp: Integer;
  HasExp: Boolean;
begin
  Sign := 1;
  IntPart := 0;
  FracPart := 0;
  FracDiv := 1;
  ExpSign := 1;
  Exp := 0;
  HasExp := False;
  
  if Lexer.CurrentChar = #45 then  { #45 = '-' }
  begin
    Sign := -1;
    Lexer.NextChar();
  end;
  
  if not ((Lexer.CurrentChar >= #48) and (Lexer.CurrentChar <= #57)) then  { #48 = '0', #57 = '9' }
  begin
    SetError('Invalid number format');
    ParseNumber := False;
    Exit();
  end;
  
  { 解析整数部分}
  if Lexer.CurrentChar = #48 then  { #48 = '0' }
  begin
    Lexer.NextChar();
    { JSON规范：0后面不能直接跟数字}
    if (Lexer.CurrentChar >= #48) and (Lexer.CurrentChar <= #57) then  { #48 = '0', #57 = '9' }
    begin
      SetError('Invalid number: leading zeros not allowed');
      ParseNumber := False;
      Exit();
    end;
  end
  else
  begin
    while (Lexer.CurrentChar >= #48) and (Lexer.CurrentChar <= #57) do  { #48 = '0', #57 = '9' }
    begin
      IntPart := IntPart * 10 + (Ord(Lexer.CurrentChar) - Ord(#48));  { #48 = '0' }
      Lexer.NextChar();
    end;
  end;
  
  { 解析小数部分}
  if Lexer.CurrentChar = #46 then  { #46 = '.' }
  begin
    Lexer.NextChar();
    if not ((Lexer.CurrentChar >= #48) and (Lexer.CurrentChar <= #57)) then  { #48 = '0', #57 = '9' }
    begin
      SetError('Invalid number: decimal point must be followed by digits');
      ParseNumber := False;
      Exit();
    end;
    
    while (Lexer.CurrentChar >= #48) and (Lexer.CurrentChar <= #57) do  { #48 = '0', #57 = '9' }
    begin
      FracDiv := FracDiv * 10;
      FracPart := FracPart * 10 + (Ord(Lexer.CurrentChar) - Ord(#48));  { #48 = '0' }
      Lexer.NextChar();
    end;
  end;
  
  { 解析指数部分}
  if (Lexer.CurrentChar = #101) or (Lexer.CurrentChar = #69) then  { #101 = 'e', #69 = 'E' }
  begin
    HasExp := True;
    Lexer.NextChar();
    
    if (Lexer.CurrentChar = #43) or (Lexer.CurrentChar = #45) then  { #43 = '+', #45 = '-' }
    begin
      if Lexer.CurrentChar = #45 then  { #45 = '-' }
        ExpSign := -1;
      Lexer.NextChar();
    end;
    
    if not ((Lexer.CurrentChar >= #48) and (Lexer.CurrentChar <= #57)) then  { #48 = '0', #57 = '9' }
    begin
      SetError('Invalid number: exponent must contain digits');
      ParseNumber := False;
      Exit();
    end;
    
    while (Lexer.CurrentChar >= #48) and (Lexer.CurrentChar <= #57) do  { #48 = '0', #57 = '9' }
    begin
      Exp := Exp * 10 + (Ord(Lexer.CurrentChar) - Ord(#48));  { #48 = '0' }
      Lexer.NextChar();
    end;
    Exp := ExpSign * Exp;
  end;
  
  { 计算最终结果}
  Num := Sign * (IntPart + FracPart / FracDiv);
  if HasExp then
  begin
    { 简单的指数处理}
    if Exp > 0 then
      while Exp > 0 do
      begin
        Num := Num * 10;
        Dec(Exp);
      end
    else
      while Exp < 0 do
      begin
        Num := Num / 10;
        Inc(Exp);
      end;
  end;
  
  ParseNumber := True;
end;

{ 前向声明}
function ParseValue(var Lexer: TLexer): Integer; forward;

{ 解析数组}
function ParseArray(var Lexer: TLexer): Integer;
var
  ArrayValueIndex: Integer;
  ItemIndex: Integer;
begin
  { 检查嵌套深度}
  Inc(Lexer.CurrentDepth);
  if Lexer.CurrentDepth > MAX_JSON_DEPTH then
  begin
    SetError('JSON nesting too deep');
    ParseArray := -1;
    Exit();
  end;
  
  ArrayValueIndex := NewJSONValue(jtArray);
  if ArrayValueIndex < 0 then
  begin
    Dec(Lexer.CurrentDepth);
    ParseArray := -1;
    Exit();
  end;
  
  if Lexer.CurrentChar <> #91 then  { #91 = '[' }
  begin
    SetError('Expected [');
    Dec(Lexer.CurrentDepth);
    ParseArray := -1;
    Exit();
  end;
  
  Lexer.NextChar(); { 跳过 '['}
  Lexer.SkipWhitespace();
  
  if Lexer.CurrentChar = #93 then  { #93 = ']' }
  begin
    Lexer.NextChar();
    Dec(Lexer.CurrentDepth);
    ParseArray := ArrayValueIndex;
    Exit();
  end;
  
  while True do
  begin
    ItemIndex := ParseValue(Lexer);
    if (ItemIndex < 0) or ParseError then
    begin
      if ArrayValueIndex > 0 then
        FreeJSONValue(ArrayValueIndex);
      Dec(Lexer.CurrentDepth);
      ParseArray := -1;
      Exit();
    end;
    
    if JSONValues[ArrayValueIndex].ArrayValue.Count >= MAX_ARRAY_SIZE then
    begin
      SetError('Array too large');
      FreeJSONValue(ItemIndex);
      FreeJSONValue(ArrayValueIndex);
      Dec(Lexer.CurrentDepth);
      ParseArray := -1;
      Exit();
    end;
    
    JSONValues[ArrayValueIndex].ArrayValue.Add(ItemIndex);
    
    Lexer.SkipWhitespace();
    
    if Lexer.CurrentChar = #93 then  { #93 = ']' }
    begin
      Lexer.NextChar();
      Break;
    end
    else if Lexer.CurrentChar = #44 then  { #44 = ',' }
    begin
      Lexer.NextChar();
      Lexer.SkipWhitespace();
      { 检查尾随逗号}
      if Lexer.CurrentChar = #93 then  { #93 = ']' }
      begin
        SetError('Trailing comma in array');
        FreeJSONValue(ArrayValueIndex);
        Dec(Lexer.CurrentDepth);
        ParseArray := -1;
        Exit();
      end;
    end
    else
    begin
      SetError('Expected , or ]');
      FreeJSONValue(ArrayValueIndex);
      Dec(Lexer.CurrentDepth);
      ParseArray := -1;
      Exit();
    end;
  end;
  
  Dec(Lexer.CurrentDepth);
  ParseArray := ArrayValueIndex;
end;

{ 解析对象}
function ParseObject(var Lexer: TLexer): Integer;
var
  ObjectValueIndex: Integer;
  Key: TJSONString;
  KeyLen: Integer;
  ValueIndex: Integer;
begin
  { 检查嵌套深度}
  Inc(Lexer.CurrentDepth);
  if Lexer.CurrentDepth > MAX_JSON_DEPTH then
  begin
    SetError('JSON nesting too deep');
    ParseObject := -1;
    Exit();
  end;
  
  ObjectValueIndex := NewJSONValue(jtObject);
  if ObjectValueIndex < 0 then
  begin
    Dec(Lexer.CurrentDepth);
    ParseObject := -1;
    Exit();
  end;
  
  if Lexer.CurrentChar <> #123 then  (* #123 = '{' *)
  begin
    SetError('Expected {');
    Dec(Lexer.CurrentDepth);
    ParseObject := -1;
    Exit();
  end;
  
  Lexer.NextChar(); { 跳过 left bracket }
  Lexer.SkipWhitespace();
  
  if Lexer.CurrentChar = #125 then  (* #125 = '}' *)
  begin
    Lexer.NextChar();
    Dec(Lexer.CurrentDepth);
    ParseObject := ObjectValueIndex;
    Exit();
  end;
  
  while True do
  begin
    { 解析键}
    if Lexer.CurrentChar <> #34 then  { #34 = '"' }
    begin
      SetError('Expected string key');
      FreeJSONValue(ObjectValueIndex);
      Dec(Lexer.CurrentDepth);
      ParseObject := -1;
      Exit();
    end;
    
    if not ParseString(Lexer, Key, KeyLen) or ParseError then
    begin
      FreeJSONValue(ObjectValueIndex);
      Dec(Lexer.CurrentDepth);
      ParseObject := -1;
      Exit();
    end;
    
    Lexer.SkipWhitespace();
    
    if Lexer.CurrentChar <> #58 then  { #58 = ':' }
    begin
      SetError('Expected :');
      FreeJSONValue(ObjectValueIndex);
      Dec(Lexer.CurrentDepth);
      ParseObject := -1;
      Exit();
    end;
    
    Lexer.NextChar(); { 跳过 ':'}
    Lexer.SkipWhitespace();
    
    { 解析值}
    ValueIndex := ParseValue(Lexer);
    if (ValueIndex < 0) or ParseError then
    begin
      FreeJSONValue(ObjectValueIndex);
      Dec(Lexer.CurrentDepth);
      ParseObject := -1;
      Exit();
    end;
    
    if JSONValues[ObjectValueIndex].ObjectValue.Count >= MAX_OBJECT_KEYS then
    begin
      SetError('Object has too many keys');
      FreeJSONValue(ValueIndex);
      FreeJSONValue(ObjectValueIndex);
      Dec(Lexer.CurrentDepth);
      ParseObject := -1;
      Exit();
    end;
    
    JSONValues[ObjectValueIndex].ObjectValue.Add(Key, KeyLen, ValueIndex);
    
    Lexer.SkipWhitespace();
    
    if Lexer.CurrentChar = #125 then  (* #125 = '}' *)
    begin
      Lexer.NextChar();
      Break;
    end
    else if Lexer.CurrentChar = #44 then  { #44 = ',' }
    begin
      Lexer.NextChar();
      Lexer.SkipWhitespace();
      { 检查尾随逗号}
      if Lexer.CurrentChar = #125 then  (* #125 = '}' *)
      begin
        SetError('Trailing comma in object');
        FreeJSONValue(ObjectValueIndex);
        Dec(Lexer.CurrentDepth);
        ParseObject := -1;
        Exit();
      end;
    end
    else
    begin
      SetError('Expected , or }');
      FreeJSONValue(ObjectValueIndex);
      Dec(Lexer.CurrentDepth);
      ParseObject := -1;
      Exit();
    end;
  end;
  
  Dec(Lexer.CurrentDepth);
  ParseObject := ObjectValueIndex;
end;

{ 解析JSON值}
function ParseValue(var Lexer: TLexer): Integer;
var
  ValueIndex: Integer;
  Str: TJSONString;
  StrLen: Integer;
  Num: Real;
begin
  Lexer.SkipWhitespace();
  
  if ParseError then
  begin
    ParseValue := -1;
    Exit();
  end;
  
  case Lexer.CurrentChar of
    #110: { #110 = 'n' null}
    begin
      if Lexer.CheckString('null') then
      begin
        ValueIndex := NewJSONValue(jtNull);
        Lexer.SkipChars(4);
        ParseValue := ValueIndex;
      end
      else
      begin
        SetError('Invalid literal');
        ParseValue := -1;
      end;
    end;
    
    #116: { #116 = 't' true}
    begin
      if Lexer.CheckString('true') then
      begin
        ValueIndex := NewJSONValue(jtBoolean);
        JSONValues[ValueIndex].BoolValue := True;
        Lexer.SkipChars(4);
        ParseValue := ValueIndex;
      end
      else
      begin
        SetError('Invalid literal');
        ParseValue := -1;
      end;
    end;
    
    #102: { #102 = 'f' false}
    begin
      if Lexer.CheckString('false') then
      begin
        ValueIndex := NewJSONValue(jtBoolean);
        JSONValues[ValueIndex].BoolValue := False;
        Lexer.SkipChars(5);
        ParseValue := ValueIndex;
      end
      else
      begin
        SetError('Invalid literal');
        ParseValue := -1;
      end;
    end;
    
    #34: { #34 = '"' string}
    begin
      if ParseString(Lexer, Str, StrLen) and not ParseError then
      begin
        ValueIndex := NewJSONValue(jtString);
        JSONValues[ValueIndex].StrLen := StrCopyCustom(Str, StrLen, JSONValues[ValueIndex].StrValue);
        ParseValue := ValueIndex;
      end
      else
        ParseValue := -1;
    end;
    
    #91: { #91 = '[' array}
    begin
      ParseValue := ParseArray(Lexer);
    end;
    
    #123: (* #123 = '{' object *)
    begin
      ParseValue := ParseObject(Lexer);
    end;
    
    #0:  { #0 = NUL }
    begin
      SetError('Unexpected end of input');
      ParseValue := -1;
    end;
    
    else { number}
    begin
      if ((Lexer.CurrentChar = #45) or  { #45 = '-' }
          ((Lexer.CurrentChar >= #48) and (Lexer.CurrentChar <= #57))) and  { #48 = '0', #57 = '9' }
         ParseNumber(Lexer, Num) and not ParseError then
      begin
        ValueIndex := NewJSONValue(jtNumber);
        JSONValues[ValueIndex].NumValue := Num;
        ParseValue := ValueIndex;
      end
      else
      begin
        if not ParseError then
          SetError('Invalid character');
        ParseValue := -1;
      end;
    end;
  end;
end;

{ 主解析函数}
function ParseJSON(const Input: String; Len: Integer): Integer;
var
  Lexer: TLexer;
  ResultIndex: Integer;
begin
  ParseError := False;
  ErrorMessage := '';
  
  InitLexer(Lexer, Input, Len);
  ResultIndex := ParseValue(Lexer);
  
  if not ParseError and (ResultIndex > 0) then
  begin
    { 检查是否有剩余字符}
    Lexer.SkipWhitespace();
    if Lexer.CurrentChar <> #0 then  { #0 = NUL }
    begin
      SetError('Unexpected characters after JSON');
      FreeJSONValue(ResultIndex);
      ResultIndex := -1;
    end;
  end;
  
  ParseJSON := ResultIndex;
end;

{ 安全的字符输出函数}
procedure SafeWriteChar(c: Char);
begin
  if (Ord(c) >= 32) and (Ord(c) <= 126) then
    Write(c)
  else
    case c of
      #8: Write('\b');   { #8 = Backspace }
      #9: Write('\t');   { #9 = Tab }
      #10: Write('\n');  { #10 = Line Feed }
      #12: Write('\f');  { #12 = Form Feed }
      #13: Write('\r');  { #13 = Carriage Return }
      else Write('?');
    end;
end;

{ 打印JSON值 (用于测试)}
procedure PrintValue(ValueIndex: Integer; Indent: Integer); forward;

procedure PrintIndent(Indent: Integer);
var
  i: Integer;
begin
  for i := 1 to Indent do
    Write(#32, #32);  { #32 = ' ' }
end;

procedure PrintValue(ValueIndex: Integer; Indent: Integer);
var
  i: Integer;
  NodeIndex: Integer;
  j: Integer;
  k: Integer;
begin
  if (ValueIndex < 1) or (ValueIndex > MAX_VALUES) then
  begin
    Write('null');
    Exit();
  end;
  
  case JSONValues[ValueIndex].JSONType of
    jtNull: Write('null');
    jtBoolean: 
      if JSONValues[ValueIndex].BoolValue then Write('true') else Write('false');
    jtNumber: Write(JSONValues[ValueIndex].NumValue:0:0);
    jtString:
    begin
      Write(#34);  { #34 = '"' }
      if (JSONValues[ValueIndex].StrLen > 0) and (JSONValues[ValueIndex].StrLen <= MAX_STRING_LEN) then
        for i := 1 to JSONValues[ValueIndex].StrLen do
          if (JSONValues[ValueIndex].StrValue[i] <> #0) then  { #0 = NUL }
            SafeWriteChar(JSONValues[ValueIndex].StrValue[i]);
      Write(#34);  { #34 = '"' }
    end;
    jtArray:
    begin
      WriteLn(#91);  { #91 = '[' }
      for i := 1 to JSONValues[ValueIndex].ArrayValue.Count do
      begin
        if i <= MAX_ARRAY_SIZE then
        begin
          PrintIndent(Indent + 1);
          PrintValue(JSONValues[ValueIndex].ArrayValue.Items[i], Indent + 1);
          if i < JSONValues[ValueIndex].ArrayValue.Count then
            WriteLn(#44)  { #44 = ',' }
          else
            Writeln();
        end;
      end;
      PrintIndent(Indent);
      Write(#93);  { #93 = ']' }
    end;
    
    jtObject:
    begin
      WriteLn(#123);  (* #123 = '{' *)
      j := 0;
      for i := 0 to HASH_TABLE_SIZE - 1 do
      begin
        NodeIndex := JSONValues[ValueIndex].ObjectValue.HashTable[i];
        while NodeIndex > 0 do
        begin
          Inc(j);
          PrintIndent(Indent + 1);
          Write(#34);  { #34 = '"' }
          for k := 1 to HashNodes[NodeIndex].KeyLen do
            SafeWriteChar(HashNodes[NodeIndex].Key[k]);
          Write(#34, #58, #32);  (* #34 = '"', #58 = ':', #32 = ' ' *)
          PrintValue(HashNodes[NodeIndex].ValueIndex, Indent + 1);
          if j < JSONValues[ValueIndex].ObjectValue.Count then
            WriteLn(#44)  { #44 = ',' }
          else
            Writeln();
          NodeIndex := HashNodes[NodeIndex].Next;
        end;
      end;
      PrintIndent(Indent);
      Write(#125);  (* #125 = '}' *)
    end;
  end;
end;


begin

  { 记录开始时间（单位：毫秒）}
  StartTime := GetTickCount64();

  { 初始化内存管理器}
  InitMemoryManager();
  
  { 示例JSON字符串}
  TestJSON := '{"name":"John","age":30,"city":"New York","isStudent":false,"courses":["Math","Physics"],"address":{"street":"123 Main St","zipcode":"10001"}}';
  
  WriteLn('Parsing JSON: ', TestJSON);
  Writeln();
  
  ParsedValueIndex := ParseJSON(TestJSON, Length(TestJSON));
  
  if ParseError then
  begin
    WriteLn('Error parsing JSON: ', ErrorMessage);
  end
  else
  begin
    WriteLn('Parsed JSON:');
    PrintValue(ParsedValueIndex, 0);
    Writeln();
  end;
  
  { 清理内存}
  if ParsedValueIndex > 0 then
    FreeJSONValue(ParsedValueIndex);

  { 记录结束时间 }
  EndTime := GetTickCount64();

  { 计算耗时 }
  ElapsedTime := EndTime - StartTime;
  
  WriteLn('========== 执行结果 ==========');
  WriteLn('总耗时: ', ElapsedTime, ' 毫秒');
  WriteLn('总耗时: ', ElapsedTime div 1000, ' 秒');
  WriteLn();
end.
