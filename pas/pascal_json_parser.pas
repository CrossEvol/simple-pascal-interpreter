program JSONParser;

const
  MAX_JSON_DEPTH = 100;
  MAX_STRING_LEN = 1000;
  MAX_ARRAY_SIZE = 1000;
  MAX_OBJECT_KEYS = 1000;
  HASH_TABLE_SIZE = 1009;

type
  // JSON值类型枚举
  TJSONType = (jtNull, jtBoolean, jtNumber, jtString, jtArray, jtObject);
  
  // 字符串类型
  TJSONString = array[1..MAX_STRING_LEN] of Char;
  
  // 前向声明
  PJSONValue = ^TJSONValue;
  PHashNode = ^THashNode;
  
  // JSON数组
  TJSONArray = record
    Items: array[1..MAX_ARRAY_SIZE] of PJSONValue;
    Count: Integer;
  end;
  
  // Hash表中的键值对节点
  THashNode = record
    Key: TJSONString;
    KeyLen: Integer;
    Value: PJSONValue;
    Next: PHashNode;
  end;
  
  // JSON对象 (使用hash表实现)
  TJSONObject = record
    HashTable: array[0..HASH_TABLE_SIZE-1] of PHashNode;
    Count: Integer;
  end;
  
  // JSON值
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
  
  // 词法分析器状态
  TLexer = record
    Input: PChar;
    Position: Integer;
    Length: Integer;
    CurrentChar: Char;
    CurrentDepth: Integer; // 添加深度跟踪
  end;

// 全局变量用于错误处理
var
  ParseError: Boolean;
  ErrorMessage: String;

// 设置错误信息
procedure SetError(const Msg: String);
begin
  ParseError := True;
  ErrorMessage := Msg;
end;

// 工具函数
function StrCopy(const Src: TJSONString; SrcLen: Integer; var Dest: TJSONString): Integer;
var
  i: Integer;
begin
  if SrcLen > MAX_STRING_LEN then
    SrcLen := MAX_STRING_LEN;
    
  for i := 1 to SrcLen do
    Dest[i] := Src[i];
  
  // 清零剩余部分
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
    Exit;
  end;
  
  for i := 1 to Len1 do
  begin
    if S1[i] <> S2[i] then
    begin
      StrEqual := False;
      Exit;
    end;
  end;
  
  StrEqual := True;
end;

// 简单的hash函数
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

// 释放JSON值的内存
procedure FreeJSONValue(Value: PJSONValue);
var
  i: Integer;
  Node, NextNode: PHashNode;
begin
  if Value = nil then
    Exit;
    
  case Value^.JSONType of
    jtArray:
    begin
      for i := 1 to Value^.ArrayValue.Count do
      begin
        if (i <= MAX_ARRAY_SIZE) and (Value^.ArrayValue.Items[i] <> nil) then
          FreeJSONValue(Value^.ArrayValue.Items[i]);
      end;
    end;
    
    jtObject:
    begin
      for i := 0 to HASH_TABLE_SIZE - 1 do
      begin
        Node := Value^.ObjectValue.HashTable[i];
        while Node <> nil do
        begin
          NextNode := Node^.Next;
          if Node^.Value <> nil then
            FreeJSONValue(Node^.Value);
          Dispose(Node);
          Node := NextNode;
        end;
      end;
    end;
  end;
  
  Dispose(Value);
end;

// 创建新的JSON值
function NewJSONValue(JSONType: TJSONType): PJSONValue;
var
  Value: PJSONValue;
  i: Integer;
begin
  New(Value);
  Value^.JSONType := JSONType;
  
  case JSONType of
    jtNull: ;
    jtBoolean: Value^.BoolValue := False;
    jtNumber: Value^.NumValue := 0.0;
    jtString: 
    begin
      Value^.StrLen := 0;
      for i := 1 to MAX_STRING_LEN do
        Value^.StrValue[i] := #0;
    end;
    jtArray: 
    begin
      Value^.ArrayValue.Count := 0;
      for i := 1 to MAX_ARRAY_SIZE do
        Value^.ArrayValue.Items[i] := nil;
    end;
    jtObject: 
    begin
      Value^.ObjectValue.Count := 0;
      for i := 0 to HASH_TABLE_SIZE-1 do
        Value^.ObjectValue.HashTable[i] := nil;
    end;
  end;
  
  NewJSONValue := Value;
end;

// 初始化词法分析器
procedure InitLexer(var Lexer: TLexer; Input: PChar; Len: Integer);
begin
  Lexer.Input := Input;
  Lexer.Position := 0;
  Lexer.Length := Len;
  Lexer.CurrentDepth := 0;
  if Len > 0 then
    Lexer.CurrentChar := Input^
  else
    Lexer.CurrentChar := #0;
end;

// 前进到下一个字符
procedure NextChar(var Lexer: TLexer);
begin
  Inc(Lexer.Position);
  if Lexer.Position >= Lexer.Length then
    Lexer.CurrentChar := #0
  else
  begin
    Inc(Lexer.Input);
    Lexer.CurrentChar := Lexer.Input^;
  end;
end;

// 检查剩余字符串是否匹配
function CheckString(var Lexer: TLexer; const Str: string): Boolean;
var
  i: Integer;
  TempInput: PChar;
begin
  if Lexer.Position + Length(Str) > Lexer.Length then
  begin
    CheckString := False;
    Exit;
  end;
  
  TempInput := Lexer.Input;
  for i := 1 to Length(Str) do
  begin
    if TempInput^ <> Str[i] then
    begin
      CheckString := False;
      Exit;
    end;
    Inc(TempInput);
  end;
  
  CheckString := True;
end;

// 跳过指定长度的字符
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

// 跳过空白字符
procedure SkipWhitespace(var Lexer: TLexer);
begin
  while (Lexer.CurrentChar = ' ') or (Lexer.CurrentChar = #9) or 
        (Lexer.CurrentChar = #10) or (Lexer.CurrentChar = #13) do
    NextChar(Lexer);
end;

// 检查是否为有效的十六进制数字
function IsHexDigit(c: Char): Boolean;
begin
  IsHexDigit := (c in ['0'..'9']) or (c in ['A'..'F']) or (c in ['a'..'f']);
end;

// 将十六进制字符转换为数值
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

// 解析Unicode转义序列
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
      Exit;
    end;
    Value := Value * 16 + HexValue(Lexer.CurrentChar);
    NextChar(Lexer);
  end;
  
  // 简化处理，只处理ASCII范围的字符
  if Value < 128 then
    ParseUnicodeEscape := Chr(Value)
  else
    ParseUnicodeEscape := '?';
end;

// 解析字符串
function ParseString(var Lexer: TLexer; var Str: TJSONString; var Len: Integer): Boolean;
begin
  Len := 0;
  if Lexer.CurrentChar <> '"' then
  begin
    SetError('Expected string');
    ParseString := False;
    Exit;
  end;
  
  NextChar(Lexer); // 跳过开始的引号
  
  while (Lexer.CurrentChar <> '"') and (Lexer.CurrentChar <> #0) and (Len < MAX_STRING_LEN) do
  begin
    if Lexer.CurrentChar = '\' then
    begin
      NextChar(Lexer);
      if Lexer.CurrentChar = #0 then
      begin
        SetError('Unexpected end of string');
        ParseString := False;
        Exit;
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
          Continue; // 已经在ParseUnicodeEscape中前进了字符
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
    NextChar(Lexer); // 跳过结束的引号
    ParseString := True;
  end
  else
  begin
    SetError('Unterminated string');
    ParseString := False;
  end;
end;

// 解析数字 - 改进版支持科学计数法
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
    Exit;
  end;
  
  // 解析整数部分
  if Lexer.CurrentChar = '0' then
  begin
    NextChar(Lexer);
    // JSON规范：0后面不能直接跟数字
    if Lexer.CurrentChar in ['0'..'9'] then
    begin
      SetError('Invalid number: leading zeros not allowed');
      ParseNumber := False;
      Exit;
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
  
  // 解析小数部分
  if Lexer.CurrentChar = '.' then
  begin
    NextChar(Lexer);
    if not (Lexer.CurrentChar in ['0'..'9']) then
    begin
      SetError('Invalid number: decimal point must be followed by digits');
      ParseNumber := False;
      Exit;
    end;
    
    while Lexer.CurrentChar in ['0'..'9'] do
    begin
      FracDiv := FracDiv * 10;
      FracPart := FracPart * 10 + (Ord(Lexer.CurrentChar) - Ord('0'));
      NextChar(Lexer);
    end;
  end;
  
  // 解析指数部分
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
      Exit;
    end;
    
    while Lexer.CurrentChar in ['0'..'9'] do
    begin
      Exp := Exp * 10 + (Ord(Lexer.CurrentChar) - Ord('0'));
      NextChar(Lexer);
    end;
    Exp := ExpSign * Exp;
  end;
  
  // 计算最终结果
  Num := Sign * (IntPart + FracPart / FracDiv);
  if HasExp then
  begin
    // 简单的指数处理
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

// 前向声明
function ParseValue(var Lexer: TLexer): PJSONValue; forward;

// 解析数组
function ParseArray(var Lexer: TLexer): PJSONValue;
var
  ArrayValue: PJSONValue;
  Item: PJSONValue;
begin
  // 检查嵌套深度
  Inc(Lexer.CurrentDepth);
  if Lexer.CurrentDepth > MAX_JSON_DEPTH then
  begin
    SetError('JSON nesting too deep');
    ParseArray := nil;
    Exit;
  end;
  
  ArrayValue := NewJSONValue(jtArray);
  
  if Lexer.CurrentChar <> '[' then
  begin
    SetError('Expected [');
    Dec(Lexer.CurrentDepth);
    ParseArray := nil;
    Exit;
  end;
  
  NextChar(Lexer); // 跳过 '['
  SkipWhitespace(Lexer);
  
  if Lexer.CurrentChar = ']' then
  begin
    NextChar(Lexer);
    Dec(Lexer.CurrentDepth);
    ParseArray := ArrayValue;
    Exit;
  end;
  
  while True do
  begin
    Item := ParseValue(Lexer);
    if (Item = nil) or ParseError then
    begin
      if ArrayValue <> nil then
        FreeJSONValue(ArrayValue);
      Dec(Lexer.CurrentDepth);
      ParseArray := nil;
      Exit;
    end;
    
    if ArrayValue^.ArrayValue.Count >= MAX_ARRAY_SIZE then
    begin
      SetError('Array too large');
      FreeJSONValue(Item);
      FreeJSONValue(ArrayValue);
      Dec(Lexer.CurrentDepth);
      ParseArray := nil;
      Exit;
    end;
    
    Inc(ArrayValue^.ArrayValue.Count);
    ArrayValue^.ArrayValue.Items[ArrayValue^.ArrayValue.Count] := Item;
    
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
      // 检查尾随逗号
      if Lexer.CurrentChar = ']' then
      begin
        SetError('Trailing comma in array');
        FreeJSONValue(ArrayValue);
        Dec(Lexer.CurrentDepth);
        ParseArray := nil;
        Exit;
      end;
    end
    else
    begin
      SetError('Expected , or ]');
      FreeJSONValue(ArrayValue);
      Dec(Lexer.CurrentDepth);
      ParseArray := nil;
      Exit;
    end;
  end;
  
  Dec(Lexer.CurrentDepth);
  ParseArray := ArrayValue;
end;

// 向对象中添加键值对
procedure AddToObject(var Obj: TJSONObject; const Key: TJSONString; KeyLen: Integer; Value: PJSONValue);
var
  Hash: Integer;
  Node, NewNode: PHashNode;
begin
  if (KeyLen <= 0) or (KeyLen > MAX_STRING_LEN) then
    Exit;
    
  Hash := HashString(Key, KeyLen);
  Node := Obj.HashTable[Hash];
  
  // 检查键是否已存在
  while Node <> nil do
  begin
    if StrEqual(Node^.Key, Node^.KeyLen, Key, KeyLen) then
    begin
      // 释放旧值并更新
      if Node^.Value <> nil then
        FreeJSONValue(Node^.Value);
      Node^.Value := Value;
      Exit;
    end;
    Node := Node^.Next;
  end;
  
  // 添加新节点
  New(NewNode);
  NewNode^.KeyLen := StrCopy(Key, KeyLen, NewNode^.Key);
  NewNode^.Value := Value;
  NewNode^.Next := Obj.HashTable[Hash];
  Obj.HashTable[Hash] := NewNode;
  Inc(Obj.Count);
end;

// 解析对象
function ParseObject(var Lexer: TLexer): PJSONValue;
var
  ObjectValue: PJSONValue;
  Key: TJSONString;
  KeyLen: Integer;
  Value: PJSONValue;
begin
  // 检查嵌套深度
  Inc(Lexer.CurrentDepth);
  if Lexer.CurrentDepth > MAX_JSON_DEPTH then
  begin
    SetError('JSON nesting too deep');
    ParseObject := nil;
    Exit;
  end;
  
  ObjectValue := NewJSONValue(jtObject);
  
  if Lexer.CurrentChar <> '{' then
  begin
    SetError('Expected {');
    Dec(Lexer.CurrentDepth);
    ParseObject := nil;
    Exit;
  end;
  
  NextChar(Lexer); // 跳过 '{'
  SkipWhitespace(Lexer);
  
  if Lexer.CurrentChar = '}' then
  begin
    NextChar(Lexer);
    Dec(Lexer.CurrentDepth);
    ParseObject := ObjectValue;
    Exit;
  end;
  
  while True do
  begin
    // 解析键
    if Lexer.CurrentChar <> '"' then
    begin
      SetError('Expected string key');
      FreeJSONValue(ObjectValue);
      Dec(Lexer.CurrentDepth);
      ParseObject := nil;
      Exit;
    end;
    
    if not ParseString(Lexer, Key, KeyLen) or ParseError then
    begin
      FreeJSONValue(ObjectValue);
      Dec(Lexer.CurrentDepth);
      ParseObject := nil;
      Exit;
    end;
    
    SkipWhitespace(Lexer);
    
    if Lexer.CurrentChar <> ':' then
    begin
      SetError('Expected :');
      FreeJSONValue(ObjectValue);
      Dec(Lexer.CurrentDepth);
      ParseObject := nil;
      Exit;
    end;
    
    NextChar(Lexer); // 跳过 ':'
    SkipWhitespace(Lexer);
    
    // 解析值
    Value := ParseValue(Lexer);
    if (Value = nil) or ParseError then
    begin
      FreeJSONValue(ObjectValue);
      Dec(Lexer.CurrentDepth);
      ParseObject := nil;
      Exit;
    end;
    
    if ObjectValue^.ObjectValue.Count >= MAX_OBJECT_KEYS then
    begin
      SetError('Object has too many keys');
      FreeJSONValue(Value);
      FreeJSONValue(ObjectValue);
      Dec(Lexer.CurrentDepth);
      ParseObject := nil;
      Exit;
    end;
    
    AddToObject(ObjectValue^.ObjectValue, Key, KeyLen, Value);
    
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
      // 检查尾随逗号
      if Lexer.CurrentChar = '}' then
      begin
        SetError('Trailing comma in object');
        FreeJSONValue(ObjectValue);
        Dec(Lexer.CurrentDepth);
        ParseObject := nil;
        Exit;
      end;
    end
    else
    begin
      SetError('Expected , or }');
      FreeJSONValue(ObjectValue);
      Dec(Lexer.CurrentDepth);
      ParseObject := nil;
      Exit;
    end;
  end;
  
  Dec(Lexer.CurrentDepth);
  ParseObject := ObjectValue;
end;

// 解析JSON值
function ParseValue(var Lexer: TLexer): PJSONValue;
var
  Value: PJSONValue;
  Str: TJSONString;
  StrLen: Integer;
  Num: Real;
begin
  SkipWhitespace(Lexer);
  
  if ParseError then
  begin
    ParseValue := nil;
    Exit;
  end;
  
  case Lexer.CurrentChar of
    'n': // null
    begin
      if CheckString(Lexer, 'null') then
      begin
        Value := NewJSONValue(jtNull);
        SkipChars(Lexer, 4);
        ParseValue := Value;
      end
      else
      begin
        SetError('Invalid literal');
        ParseValue := nil;
      end;
    end;
    
    't': // true
    begin
      if CheckString(Lexer, 'true') then
      begin
        Value := NewJSONValue(jtBoolean);
        Value^.BoolValue := True;
        SkipChars(Lexer, 4);
        ParseValue := Value;
      end
      else
      begin
        SetError('Invalid literal');
        ParseValue := nil;
      end;
    end;
    
    'f': // false
    begin
      if CheckString(Lexer, 'false') then
      begin
        Value := NewJSONValue(jtBoolean);
        Value^.BoolValue := False;
        SkipChars(Lexer, 5);
        ParseValue := Value;
      end
      else
      begin
        SetError('Invalid literal');
        ParseValue := nil;
      end;
    end;
    
    '"': // string
    begin
      if ParseString(Lexer, Str, StrLen) and not ParseError then
      begin
        Value := NewJSONValue(jtString);
        Value^.StrLen := StrCopy(Str, StrLen, Value^.StrValue);
        ParseValue := Value;
      end
      else
        ParseValue := nil;
    end;
    
    '[': // array
    begin
      ParseValue := ParseArray(Lexer);
    end;
    
    '{': // object
    begin
      ParseValue := ParseObject(Lexer);
    end;
    
    #0:
    begin
      SetError('Unexpected end of input');
      ParseValue := nil;
    end;
    
    else // number
    begin
      if (Lexer.CurrentChar in ['-', '0'..'9']) and ParseNumber(Lexer, Num) and not ParseError then
      begin
        Value := NewJSONValue(jtNumber);
        Value^.NumValue := Num;
        ParseValue := Value;
      end
      else
      begin
        if not ParseError then
          SetError('Invalid character');
        ParseValue := nil;
      end;
    end;
  end;
end;

// 主解析函数
function ParseJSON(Input: PChar; Len: Integer): PJSONValue;
var
  Lexer: TLexer;
  Result: PJSONValue;
begin
  ParseError := False;
  ErrorMessage := '';
  
  InitLexer(Lexer, Input, Len);
  Result := ParseValue(Lexer);
  
  if not ParseError and (Result <> nil) then
  begin
    // 检查是否有剩余字符
    SkipWhitespace(Lexer);
    if Lexer.CurrentChar <> #0 then
    begin
      SetError('Unexpected characters after JSON');
      FreeJSONValue(Result);
      Result := nil;
    end;
  end;
  
  ParseJSON := Result;
end;

// 安全的字符输出函数
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

// 打印JSON值 (用于测试)
procedure PrintValue(Value: PJSONValue; Indent: Integer); forward;

procedure PrintIndent(Indent: Integer);
var
  i: Integer;
begin
  for i := 1 to Indent do
    Write('  ');
end;

procedure PrintValue(Value: PJSONValue; Indent: Integer);
var
  i: Integer;
  Node: PHashNode;
  j: Integer;
  k: Integer;
begin
  if Value = nil then
  begin
    Write('null');
    Exit;
  end;
  
  case Value^.JSONType of
    jtNull: Write('null');
    jtBoolean: 
      if Value^.BoolValue then Write('true') else Write('false');
    jtNumber: Write(Value^.NumValue:0:6);
    jtString:
    begin
      Write('"');
      if (Value^.StrLen > 0) and (Value^.StrLen <= MAX_STRING_LEN) then
        for i := 1 to Value^.StrLen do
          if (Value^.StrValue[i] <> #0) then
            SafeWriteChar(Value^.StrValue[i]);
      Write('"');
    end;
    jtArray:
    begin
      WriteLn('[');
      for i := 1 to Value^.ArrayValue.Count do
      begin
        if i <= MAX_ARRAY_SIZE then
        begin
          PrintIndent(Indent + 1);
          PrintValue(Value^.ArrayValue.Items[i], Indent + 1);
          if i < Value^.ArrayValue.Count then
            WriteLn(',')
          else
            WriteLn;
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
        Node := Value^.ObjectValue.HashTable[i];
        while Node <> nil do
        begin
          Inc(j);
          PrintIndent(Indent + 1);
          Write('"');
          for k := 1 to Node^.KeyLen do
            SafeWriteChar(Node^.Key[k]);
          Write('": ');
          PrintValue(Node^.Value, Indent + 1);
          if j < Value^.ObjectValue.Count then
            WriteLn(',')
          else
            WriteLn;
          Node := Node^.Next;
        end;
      end;
      PrintIndent(Indent);
      Write('}');
    end;
  end;
end;

// 主程序入口点
var
  TestJSON: String;
  ParsedValue: PJSONValue;
  
begin
  // 示例JSON字符串
  TestJSON := '{"name":"John","age":30,"city":"New York","isStudent":false,"courses":["Math","Physics"],"address":{"street":"123 Main St","zipcode":"10001"}}';
  
  WriteLn('Parsing JSON: ', TestJSON);
  WriteLn;
  
  ParsedValue := ParseJSON(@TestJSON[1], Length(TestJSON));
  
  if ParseError then
  begin
    WriteLn('Error parsing JSON: ', ErrorMessage);
  end
  else
  begin
    WriteLn('Parsed JSON:');
    PrintValue(ParsedValue, 0);
    WriteLn;
  end;
  
  // 清理内存
  if ParsedValue <> nil then
    FreeJSONValue(ParsedValue);
end.