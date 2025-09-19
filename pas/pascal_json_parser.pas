program JSONParserNoPointers;

const
  MAX_JSON_DEPTH = 100;
  MAX_STRING_LEN = 1000;
  MAX_ARRAY_SIZE = 1000;
  MAX_OBJECT_KEYS = 1000;
  HASH_TABLE_SIZE = 1009;
  MAX_VALUES = 5000; { 最大JSON值数量}
  MAX_HASH_NODES = 5000; { 最大哈希节点数量}
  MAX_INPUT_LEN = 10000; { 最大输入长度}

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
function StrCopy(const Src: TJSONString; SrcLen: Integer; var Dest: TJSONString): Integer;
var
  i: Integer;
begin
  if SrcLen > MAX_STRING_LEN then
    SrcLen := MAX_STRING_LEN;
    
  for i := 1 to SrcLen do
    Dest[i] := Src[i];
  
  { 清零剩余部分}
  for i := SrcLen + 1 to MAX_STRING_LEN do
    Dest[i] := #0;
    
  StrCopy := SrcLen;
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
  Hash: LongInt;
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
        JSONValues[ValueIndex].ObjectValue.HashTable[i] := 0;
      end;
      JSONValues[ValueIndex].ObjectValue.Count := 0;
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
        JSONValues[ValueIndex].StrValue[i] := #0;
    end;
    jtArray: 
    begin
      JSONValues[ValueIndex].ArrayValue.Count := 0;
      for i := 1 to MAX_ARRAY_SIZE do
        JSONValues[ValueIndex].ArrayValue.Items[i] := 0;
    end;
    jtObject: 
    begin
      JSONValues[ValueIndex].ObjectValue.Count := 0;
      for i := 0 to HASH_TABLE_SIZE-1 do
        JSONValues[ValueIndex].ObjectValue.HashTable[i] := 0;
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
    Lexer.Input[i] := #0;
    
  if Len > 0 then
    Lexer.CurrentChar := Lexer.Input[1]
  else
    Lexer.CurrentChar := #0;
end;

{ 前进到下一个字符}
procedure NextChar(var Lexer: TLexer);
begin
  Inc(Lexer.Position);
  if Lexer.Position >= Lexer.Length then
    Lexer.CurrentChar := #0
  else
    Lexer.CurrentChar := Lexer.Input[Lexer.Position + 1]; { 数组索引从1开始}
end;

{ 检查剩余字符串是否匹配}
function CheckString(var Lexer: TLexer; const Str: string): Boolean;
var
  i: Integer;
  TempPos: Integer;
begin
  if Lexer.Position + Length(Str) > Lexer.Length then
  begin
    CheckString := False;
    Exit();
  end;
  
  TempPos := Lexer.Position;
  for i := 1 to Length(Str) do
  begin
    if Lexer.Input[TempPos + 1] <> Str[i] then { 数组索引从1开始}
    begin
      CheckString := False;
      Exit();
    end;
    Inc(TempPos);
  end;
  
  CheckString := True;
end;

{ 跳过指定长度的字符}
procedure SkipChars(var Lexer: TLexer; Count: Integer);
var
  i: Integer;
begin
  for i := 1 to Count do
  begin
    NextChar(Lexer);
    if Lexer.CurrentChar = #0 then
      Break;
  end;
end;

{ 跳过空白字符}
procedure SkipWhitespace(var Lexer: TLexer);
begin
  while (Lexer.CurrentChar = ' ') or (Lexer.CurrentChar = #9) or 
        (Lexer.CurrentChar = #10) or (Lexer.CurrentChar = #13) do
    NextChar(Lexer);
end;

{ 检查是否为有效的十六进制数字}
function IsHexDigit(c: Char): Boolean;
begin
  IsHexDigit := (c in ['0'..'9']) or (c in ['A'..'F']) or (c in ['a'..'f']);
end;

{ 将十六进制字符转换为数值}
function HexValue(c: Char): Integer;
begin
  if c in ['0'..'9'] then
    HexValue := Ord(c) - Ord('0')
  else if c in ['A'..'F'] then
    HexValue := Ord(c) - Ord('A') + 10
  else if c in ['a'..'f'] then
    HexValue := Ord(c) - Ord('a') + 10
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
      ParseUnicodeEscape := #0;
      Exit();
    end;
    Value := Value * 16 + HexValue(Lexer.CurrentChar);
    NextChar(Lexer);
  end;
  
  { 简化处理，只处理ASCII范围的字符}
  if Value < 128 then
    ParseUnicodeEscape := Chr(Value)
  else
    ParseUnicodeEscape := '?';
end;

{ 解析字符串}
function ParseString(var Lexer: TLexer; var Str: TJSONString; var Len: Integer): Boolean;
begin
  Len := 0;
  if Lexer.CurrentChar <> '"' then
  begin
    SetError('Expected string');
    ParseString := False;
    Exit();
  end;
  
  NextChar(Lexer); { 跳过开始的引号}
  
  while (Lexer.CurrentChar <> '"') and (Lexer.CurrentChar <> #0) and (Len < MAX_STRING_LEN) do
  begin
    if Lexer.CurrentChar = '\' then
    begin
      NextChar(Lexer);
      if Lexer.CurrentChar = #0 then
      begin
        SetError('Unexpected end of string');
        ParseString := False;
        Exit();
      end;
      
      case Lexer.CurrentChar of
        '"': begin Inc(Len); if Len <= MAX_STRING_LEN then Str[Len] := '"'; end;
        '\': begin Inc(Len); if Len <= MAX_STRING_LEN then Str[Len] := '\'; end;
        '/': begin Inc(Len); if Len <= MAX_STRING_LEN then Str[Len] := '/'; end;
        'b': begin Inc(Len); if Len <= MAX_STRING_LEN then Str[Len] := #8; end;
        'f': begin Inc(Len); if Len <= MAX_STRING_LEN then Str[Len] := #12; end;
        'n': begin Inc(Len); if Len <= MAX_STRING_LEN then Str[Len] := #10; end;
        'r': begin Inc(Len); if Len <= MAX_STRING_LEN then Str[Len] := #13; end;
        't': begin Inc(Len); if Len <= MAX_STRING_LEN then Str[Len] := #9; end;
        'u': begin 
          NextChar(Lexer);
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
    NextChar(Lexer);
  end;
  
  if Lexer.CurrentChar = '"' then
  begin
    NextChar(Lexer); { 跳过结束的引号}
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
  
  if Lexer.CurrentChar = '-' then
  begin
    Sign := -1;
    NextChar(Lexer);
  end;
  
  if not (Lexer.CurrentChar in ['0'..'9']) then
  begin
    SetError('Invalid number format');
    ParseNumber := False;
    Exit();
  end;
  
  { 解析整数部分}
  if Lexer.CurrentChar = '0' then
  begin
    NextChar(Lexer);
    { JSON规范：0后面不能直接跟数字}
    if Lexer.CurrentChar in ['0'..'9'] then
    begin
      SetError('Invalid number: leading zeros not allowed');
      ParseNumber := False;
      Exit();
    end;
  end
  else
  begin
    while Lexer.CurrentChar in ['0'..'9'] do
    begin
      IntPart := IntPart * 10 + (Ord(Lexer.CurrentChar) - Ord('0'));
      NextChar(Lexer);
    end;
  end;
  
  { 解析小数部分}
  if Lexer.CurrentChar = '.' then
  begin
    NextChar(Lexer);
    if not (Lexer.CurrentChar in ['0'..'9']) then
    begin
      SetError('Invalid number: decimal point must be followed by digits');
      ParseNumber := False;
      Exit();
    end;
    
    while Lexer.CurrentChar in ['0'..'9'] do
    begin
      FracDiv := FracDiv * 10;
      FracPart := FracPart * 10 + (Ord(Lexer.CurrentChar) - Ord('0'));
      NextChar(Lexer);
    end;
  end;
  
  { 解析指数部分}
  if (Lexer.CurrentChar = 'e') or (Lexer.CurrentChar = 'E') then
  begin
    HasExp := True;
    NextChar(Lexer);
    
    if (Lexer.CurrentChar = '+') or (Lexer.CurrentChar = '-') then
    begin
      if Lexer.CurrentChar = '-' then
        ExpSign := -1;
      NextChar(Lexer);
    end;
    
    if not (Lexer.CurrentChar in ['0'..'9']) then
    begin
      SetError('Invalid number: exponent must contain digits');
      ParseNumber := False;
      Exit();
    end;
    
    while Lexer.CurrentChar in ['0'..'9'] do
    begin
      Exp := Exp * 10 + (Ord(Lexer.CurrentChar) - Ord('0'));
      NextChar(Lexer);
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
  
  if Lexer.CurrentChar <> '[' then
  begin
    SetError('Expected [');
    Dec(Lexer.CurrentDepth);
    ParseArray := -1;
    Exit();
  end;
  
  NextChar(Lexer); { 跳过 '['}
  SkipWhitespace(Lexer);
  
  if Lexer.CurrentChar = ']' then
  begin
    NextChar(Lexer);
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
    
    Inc(JSONValues[ArrayValueIndex].ArrayValue.Count);
    JSONValues[ArrayValueIndex].ArrayValue.Items[JSONValues[ArrayValueIndex].ArrayValue.Count] := ItemIndex;
    
    SkipWhitespace(Lexer);
    
    if Lexer.CurrentChar = ']' then
    begin
      NextChar(Lexer);
      Break;
    end
    else if Lexer.CurrentChar = ',' then
    begin
      NextChar(Lexer);
      SkipWhitespace(Lexer);
      { 检查尾随逗号}
      if Lexer.CurrentChar = ']' then
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

{ 向对象中添加键值对}
procedure AddToObject(var Obj: TJSONObject; const Key: TJSONString; KeyLen: Integer; ValueIndex: Integer);
var
  Hash: Integer;
  NodeIndex, NewNodeIndex: Integer;
begin
  if (KeyLen <= 0) or (KeyLen > MAX_STRING_LEN) or (ValueIndex < 0) then
    Exit();
    
  Hash := HashString(Key, KeyLen);
  NodeIndex := Obj.HashTable[Hash];
  
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
    
  HashNodes[NewNodeIndex].KeyLen := StrCopy(Key, KeyLen, HashNodes[NewNodeIndex].Key);
  HashNodes[NewNodeIndex].ValueIndex := ValueIndex;
  HashNodes[NewNodeIndex].Next := Obj.HashTable[Hash];
  Obj.HashTable[Hash] := NewNodeIndex;
  Inc(Obj.Count);
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
  
  if Lexer.CurrentChar <> '{' then
  begin
    SetError('Expected {');
    Dec(Lexer.CurrentDepth);
    ParseObject := -1;
    Exit();
  end;
  
  NextChar(Lexer); { 跳过 left bracket }
  SkipWhitespace(Lexer);
  
  if Lexer.CurrentChar = '}' then
  begin
    NextChar(Lexer);
    Dec(Lexer.CurrentDepth);
    ParseObject := ObjectValueIndex;
    Exit();
  end;
  
  while True do
  begin
    { 解析键}
    if Lexer.CurrentChar <> '"' then
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
    
    SkipWhitespace(Lexer);
    
    if Lexer.CurrentChar <> ':' then
    begin
      SetError('Expected :');
      FreeJSONValue(ObjectValueIndex);
      Dec(Lexer.CurrentDepth);
      ParseObject := -1;
      Exit();
    end;
    
    NextChar(Lexer); { 跳过 ':'}
    SkipWhitespace(Lexer);
    
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
    
    AddToObject(JSONValues[ObjectValueIndex].ObjectValue, Key, KeyLen, ValueIndex);
    
    SkipWhitespace(Lexer);
    
    if Lexer.CurrentChar = '}' then
    begin
      NextChar(Lexer);
      Break;
    end
    else if Lexer.CurrentChar = ',' then
    begin
      NextChar(Lexer);
      SkipWhitespace(Lexer);
      { 检查尾随逗号}
      if Lexer.CurrentChar = '}' then
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
  SkipWhitespace(Lexer);
  
  if ParseError then
  begin
    ParseValue := -1;
    Exit();
  end;
  
  case Lexer.CurrentChar of
    'n': { null}
    begin
      if CheckString(Lexer, 'null') then
      begin
        ValueIndex := NewJSONValue(jtNull);
        SkipChars(Lexer, 4);
        ParseValue := ValueIndex;
      end
      else
      begin
        SetError('Invalid literal');
        ParseValue := -1;
      end;
    end;
    
    't': { true}
    begin
      if CheckString(Lexer, 'true') then
      begin
        ValueIndex := NewJSONValue(jtBoolean);
        JSONValues[ValueIndex].BoolValue := True;
        SkipChars(Lexer, 4);
        ParseValue := ValueIndex;
      end
      else
      begin
        SetError('Invalid literal');
        ParseValue := -1;
      end;
    end;
    
    'f': { false}
    begin
      if CheckString(Lexer, 'false') then
      begin
        ValueIndex := NewJSONValue(jtBoolean);
        JSONValues[ValueIndex].BoolValue := False;
        SkipChars(Lexer, 5);
        ParseValue := ValueIndex;
      end
      else
      begin
        SetError('Invalid literal');
        ParseValue := -1;
      end;
    end;
    
    '"': { string}
    begin
      if ParseString(Lexer, Str, StrLen) and not ParseError then
      begin
        ValueIndex := NewJSONValue(jtString);
        JSONValues[ValueIndex].StrLen := StrCopy(Str, StrLen, JSONValues[ValueIndex].StrValue);
        ParseValue := ValueIndex;
      end
      else
        ParseValue := -1;
    end;
    
    '[': { array}
    begin
      ParseValue := ParseArray(Lexer);
    end;
    
    '{': { object}
    begin
      ParseValue := ParseObject(Lexer);
    end;
    
    #0:
    begin
      SetError('Unexpected end of input');
      ParseValue := -1;
    end;
    
    else { number}
    begin
      if (Lexer.CurrentChar in ['-', '0'..'9']) and ParseNumber(Lexer, Num) and not ParseError then
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
    SkipWhitespace(Lexer);
    if Lexer.CurrentChar <> #0 then
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
      #8: Write('\b');
      #9: Write('\t');
      #10: Write('\n');
      #12: Write('\f');
      #13: Write('\r');
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
    Write('  ');
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
    jtNumber: Write(JSONValues[ValueIndex].NumValue:0:6);
    jtString:
    begin
      Write('"');
      if (JSONValues[ValueIndex].StrLen > 0) and (JSONValues[ValueIndex].StrLen <= MAX_STRING_LEN) then
        for i := 1 to JSONValues[ValueIndex].StrLen do
          if (JSONValues[ValueIndex].StrValue[i] <> #0) then
            SafeWriteChar(JSONValues[ValueIndex].StrValue[i]);
      Write('"');
    end;
    jtArray:
    begin
      WriteLn('[');
      for i := 1 to JSONValues[ValueIndex].ArrayValue.Count do
      begin
        if i <= MAX_ARRAY_SIZE then
        begin
          PrintIndent(Indent + 1);
          PrintValue(JSONValues[ValueIndex].ArrayValue.Items[i], Indent + 1);
          if i < JSONValues[ValueIndex].ArrayValue.Count then
            WriteLn(',')
          else
            Writeln();
        end;
      end;
      PrintIndent(Indent);
      Write(']');
    end;
    
    jtObject:
    begin
      WriteLn('{');
      j := 0;
      for i := 0 to HASH_TABLE_SIZE - 1 do
      begin
        NodeIndex := JSONValues[ValueIndex].ObjectValue.HashTable[i];
        while NodeIndex > 0 do
        begin
          Inc(j);
          PrintIndent(Indent + 1);
          Write('"');
          for k := 1 to HashNodes[NodeIndex].KeyLen do
            SafeWriteChar(HashNodes[NodeIndex].Key[k]);
          Write('": ');
          PrintValue(HashNodes[NodeIndex].ValueIndex, Indent + 1);
          if j < JSONValues[ValueIndex].ObjectValue.Count then
            WriteLn(',')
          else
            Writeln();
          NodeIndex := HashNodes[NodeIndex].Next;
        end;
      end;
      PrintIndent(Indent);
      Write('}');
    end;
  end;
end;

{ 主程序入口点}
var
  TestJSON: String;
  ParsedValueIndex: Integer;
  
begin
  { 初始化内存管理器}
  InitMemoryManager;
  
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
end.