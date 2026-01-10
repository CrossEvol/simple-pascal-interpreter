program JSONParserWithClasses;

{$CODEPAGE UTF8}
{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

const
  MAX_JSON_DEPTH  = 16;
  MAX_STRING_LEN  = 255;
  MAX_ARRAY_SIZE  = 16;
  MAX_OBJECT_KEYS = 32;
  HASH_TABLE_SIZE = 37;
  MAX_INPUT_LEN   = 1024;

type
  { 前向声明 }
  TJSONValue = class;
  
  { JSON 字符串类型 }
  TJSONString = array[1..MAX_STRING_LEN] of Char;
  TInputBuffer = array[1..MAX_INPUT_LEN] of Char;
  
  { 基础 JSON 值类 - 抽象基类 }
  TJSONValue = class
  public
    function GetTypeName: string; virtual; abstract;
    procedure Print(Indent: Integer); virtual; abstract;
    destructor Destroy; override;
  end;
  
  { Null 值类 }
  TJSONNull = class(TJSONValue)
  public
    function GetTypeName: string; override;
    procedure Print(Indent: Integer); override;
  end;
  
  { Boolean 值类 }
  TJSONBoolean = class(TJSONValue)
  private
    FValue: Boolean;
  public
    constructor Create(AValue: Boolean);
    property Value: Boolean read FValue write FValue;
    function GetTypeName: string; override;
    procedure Print(Indent: Integer); override;
  end;
  
  { Number 值类 }
  TJSONNumber = class(TJSONValue)
  private
    FValue: Real;
  public
    constructor Create(AValue: Real);
    property Value: Real read FValue write FValue;
    function GetTypeName: string; override;
    procedure Print(Indent: Integer); override;
  end;
  
  { String 值类 }
  TJSONStringValue = class(TJSONValue)
  private
    FValue: TJSONString;
    FLength: Integer;
  public
    constructor Create(const AValue: TJSONString; ALength: Integer);
    property Value: TJSONString read FValue;
    property Length: Integer read FLength;
    function GetTypeName: string; override;
    procedure Print(Indent: Integer); override;
  end;
  
  { Array 值类 }
  TJSONArray = class(TJSONValue)
  private
    FItems: array[1..MAX_ARRAY_SIZE] of TJSONValue;
    FCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Item: TJSONValue): Boolean;
    function GetItem(Index: Integer): TJSONValue;
    property Count: Integer read FCount;
    function GetTypeName: string; override;
    procedure Print(Indent: Integer); override;
  end;
  
  { Hash 节点类 }
  THashNode = class
  private
    FKey: TJSONString;
    FKeyLength: Integer;
    FValue: TJSONValue;
    FNext: THashNode;
  public
    constructor Create(const AKey: TJSONString; AKeyLength: Integer; AValue: TJSONValue);
    destructor Destroy; override;
    property Key: TJSONString read FKey;
    property KeyLength: Integer read FKeyLength;
    property Value: TJSONValue read FValue write FValue;
    property Next: THashNode read FNext write FNext;
  end;
  
  { Object 值类 }
  TJSONObject = class(TJSONValue)
  private
    FHashTable: array[0..HASH_TABLE_SIZE-1] of THashNode;
    FCount: Integer;
    function HashString(const Str: TJSONString; Len: Integer): Integer;
    function StrEqual(const S1: TJSONString; Len1: Integer; const S2: TJSONString; Len2: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddPair(const Key: TJSONString; KeyLen: Integer; Value: TJSONValue);
    function GetValue(const Key: TJSONString; KeyLen: Integer): TJSONValue;
    property Count: Integer read FCount;
    function GetTypeName: string; override;
    procedure Print(Indent: Integer); override;
  end;
  
  { 词法分析器类 }
  TLexer = class
  private
    FInput: TInputBuffer;
    FPosition: Integer;
    FLength: Integer;
    FCurrentChar: Char;
    FCurrentDepth: Integer;
    procedure NextChar;
    procedure SkipWhitespace;
    function CheckString(const Str: string): Boolean;
    procedure SkipChars(Count: Integer);
    function IsHexDigit(c: Char): Boolean;
    function HexValue(c: Char): Integer;
    function ParseUnicodeEscape: Char;
  public
    constructor Create(const Input: String; Len: Integer);
    property CurrentChar: Char read FCurrentChar;
    property CurrentDepth: Integer read FCurrentDepth write FCurrentDepth;
  end;
  
  { JSON 解析器类 }
  TJSONParser = class
  private
    FLexer: TLexer;
    FParseError: Boolean;
    FErrorMessage: String;
    procedure SetError(const Msg: String);
    function ParseString(var Str: TJSONString; var Len: Integer): Boolean;
    function ParseNumber(var Num: Real): Boolean;
    function ParseValue: TJSONValue;
    function ParseArray: TJSONArray;
    function ParseObject: TJSONObject;
  public
    constructor Create;
    destructor Destroy; override;
    function Parse(const Input: String): TJSONValue;
    property HasError: Boolean read FParseError;
    property ErrorMessage: String read FErrorMessage;
  end;

var
  GlobalParseError: Boolean;
  GlobalErrorMessage: String;

{ ============================================================================ }
{ TJSONValue Implementation }
{ ============================================================================ }

destructor TJSONValue.Destroy;
begin
  inherited Destroy;
end;

{ ============================================================================ }
{ TJSONNull Implementation }
{ ============================================================================ }

function TJSONNull.GetTypeName: string;
begin
  Result := 'null';
end;

procedure TJSONNull.Print(Indent: Integer);
begin
  Write('null');
end;

{ ============================================================================ }
{ TJSONBoolean Implementation }
{ ============================================================================ }

constructor TJSONBoolean.Create(AValue: Boolean);
begin
  inherited Create;
  FValue := AValue;
end;

function TJSONBoolean.GetTypeName: string;
begin
  Result := 'boolean';
end;

procedure TJSONBoolean.Print(Indent: Integer);
begin
  if FValue then
    Write('true')
  else
    Write('false');
end;

{ ============================================================================ }
{ TJSONNumber Implementation }
{ ============================================================================ }

constructor TJSONNumber.Create(AValue: Real);
begin
  inherited Create;
  FValue := AValue;
end;

function TJSONNumber.GetTypeName: string;
begin
  Result := 'number';
end;

procedure TJSONNumber.Print(Indent: Integer);
begin
  Write(FValue:0:6);
end;

{ ============================================================================ }
{ TJSONStringValue Implementation }
{ ============================================================================ }

constructor TJSONStringValue.Create(const AValue: TJSONString; ALength: Integer);
var
  i: Integer;
begin
  inherited Create;
  FLength := ALength;
  if FLength > MAX_STRING_LEN then
    FLength := MAX_STRING_LEN;
  for i := 1 to FLength do
    FValue[i] := AValue[i];
  for i := FLength + 1 to MAX_STRING_LEN do
    FValue[i] := #0;
end;

function TJSONStringValue.GetTypeName: string;
begin
  Result := 'string';
end;

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

procedure TJSONStringValue.Print(Indent: Integer);
var
  i: Integer;
begin
  Write('"');
  for i := 1 to FLength do
    if FValue[i] <> #0 then
      SafeWriteChar(FValue[i]);
  Write('"');
end;

{ ============================================================================ }
{ TJSONArray Implementation }
{ ============================================================================ }

constructor TJSONArray.Create;
var
  i: Integer;
begin
  inherited Create;
  FCount := 0;
  for i := 1 to MAX_ARRAY_SIZE do
    FItems[i] := nil;
end;

destructor TJSONArray.Destroy;
var
  i: Integer;
begin
  for i := 1 to FCount do
    if FItems[i] <> nil then
      FItems[i].Free;
  inherited Destroy;
end;

function TJSONArray.Add(Item: TJSONValue): Boolean;
begin
  if FCount >= MAX_ARRAY_SIZE then
  begin
    Result := False;
    Exit;
  end;
  Inc(FCount);
  FItems[FCount] := Item;
  Result := True;
end;

function TJSONArray.GetItem(Index: Integer): TJSONValue;
begin
  if (Index < 1) or (Index > FCount) then
    Result := nil
  else
    Result := FItems[Index];
end;

function TJSONArray.GetTypeName: string;
begin
  Result := 'array';
end;

procedure PrintIndent(Indent: Integer);
var
  i: Integer;
begin
  for i := 1 to Indent do
    Write('  ');
end;

procedure TJSONArray.Print(Indent: Integer);
var
  i: Integer;
begin
  WriteLn('[');
  for i := 1 to FCount do
  begin
    PrintIndent(Indent + 1);
    if FItems[i] <> nil then
      FItems[i].Print(Indent + 1)
    else
      Write('null');
    if i < FCount then
      WriteLn(',')
    else
      WriteLn;
  end;
  PrintIndent(Indent);
  Write(']');
end;

{ ============================================================================ }
{ THashNode Implementation }
{ ============================================================================ }

constructor THashNode.Create(const AKey: TJSONString; AKeyLength: Integer; AValue: TJSONValue);
var
  i: Integer;
begin
  inherited Create;
  FKeyLength := AKeyLength;
  if FKeyLength > MAX_STRING_LEN then
    FKeyLength := MAX_STRING_LEN;
  for i := 1 to FKeyLength do
    FKey[i] := AKey[i];
  for i := FKeyLength + 1 to MAX_STRING_LEN do
    FKey[i] := #0;
  FValue := AValue;
  FNext := nil;
end;

destructor THashNode.Destroy;
begin
  if FValue <> nil then
    FValue.Free;
  if FNext <> nil then
    FNext.Free;
  inherited Destroy;
end;

{ ============================================================================ }
{ TJSONObject Implementation }
{ ============================================================================ }

constructor TJSONObject.Create;
var
  i: Integer;
begin
  inherited Create;
  FCount := 0;
  for i := 0 to HASH_TABLE_SIZE - 1 do
    FHashTable[i] := nil;
end;

destructor TJSONObject.Destroy;
var
  i: Integer;
begin
  for i := 0 to HASH_TABLE_SIZE - 1 do
    if FHashTable[i] <> nil then
      FHashTable[i].Free;
  inherited Destroy;
end;

function TJSONObject.HashString(const Str: TJSONString; Len: Integer): Integer;
var
  i: Integer;
  Hash: Integer;
begin
  Hash := 0;
  for i := 1 to Len do
    Hash := ((Hash * 31) + Ord(Str[i])) mod HASH_TABLE_SIZE;
  if Hash < 0 then
    Hash := -Hash;
  Result := Hash;
end;

function TJSONObject.StrEqual(const S1: TJSONString; Len1: Integer; const S2: TJSONString; Len2: Integer): Boolean;
var
  i: Integer;
begin
  if Len1 <> Len2 then
  begin
    Result := False;
    Exit;
  end;
  
  for i := 1 to Len1 do
    if S1[i] <> S2[i] then
    begin
      Result := False;
      Exit;
    end;
  
  Result := True;
end;

procedure TJSONObject.AddPair(const Key: TJSONString; KeyLen: Integer; Value: TJSONValue);
var
  Hash: Integer;
  Node, NewNode: THashNode;
begin
  if (KeyLen <= 0) or (KeyLen > MAX_STRING_LEN) or (Value = nil) then
    Exit;
  
  Hash := HashString(Key, KeyLen);
  Node := FHashTable[Hash];
  
  { 检查键是否已存在 }
  while Node <> nil do
  begin
    if StrEqual(Node.Key, Node.KeyLength, Key, KeyLen) then
    begin
      { 替换现有值 }
      if Node.Value <> nil then
        Node.Value.Free;
      Node.Value := Value;
      Exit;
    end;
    Node := Node.Next;
  end;
  
  { 添加新节点 }
  NewNode := THashNode.Create(Key, KeyLen, Value);
  NewNode.Next := FHashTable[Hash];
  FHashTable[Hash] := NewNode;
  Inc(FCount);
end;

function TJSONObject.GetValue(const Key: TJSONString; KeyLen: Integer): TJSONValue;
var
  Hash: Integer;
  Node: THashNode;
begin
  Hash := HashString(Key, KeyLen);
  Node := FHashTable[Hash];
  
  while Node <> nil do
  begin
    if StrEqual(Node.Key, Node.KeyLength, Key, KeyLen) then
    begin
      Result := Node.Value;
      Exit;
    end;
    Node := Node.Next;
  end;
  
  Result := nil;
end;

function TJSONObject.GetTypeName: string;
begin
  Result := 'object';
end;

procedure TJSONObject.Print(Indent: Integer);
var
  i, j, k: Integer;
  Node: THashNode;
begin
  WriteLn('{');
  j := 0;
  for i := 0 to HASH_TABLE_SIZE - 1 do
  begin
    Node := FHashTable[i];
    while Node <> nil do
    begin
      Inc(j);
      PrintIndent(Indent + 1);
      Write('"');
      for k := 1 to Node.KeyLength do
        SafeWriteChar(Node.Key[k]);
      Write('": ');
      if Node.Value <> nil then
        Node.Value.Print(Indent + 1)
      else
        Write('null');
      if j < FCount then
        WriteLn(',')
      else
        WriteLn;
      Node := Node.Next;
    end;
  end;
  PrintIndent(Indent);
  Write('}');
end;

{ ============================================================================ }
{ TLexer Implementation }
{ ============================================================================ }

constructor TLexer.Create(const Input: String; Len: Integer);
var
  i: Integer;
begin
  inherited Create;
  FPosition := 0;
  FLength := Len;
  FCurrentDepth := 0;
  
  if FLength > MAX_INPUT_LEN then
    FLength := MAX_INPUT_LEN;
  
  for i := 1 to FLength do
    FInput[i] := Input[i];
  
  for i := FLength + 1 to MAX_INPUT_LEN do
    FInput[i] := #0;
  
  if Len > 0 then
    FCurrentChar := FInput[1]
  else
    FCurrentChar := #0;
end;

procedure TLexer.NextChar;
begin
  Inc(FPosition);
  if FPosition >= FLength then
    FCurrentChar := #0
  else
    FCurrentChar := FInput[FPosition + 1];
end;

procedure TLexer.SkipWhitespace;
begin
  while (FCurrentChar = ' ') or (FCurrentChar = #9) or 
        (FCurrentChar = #10) or (FCurrentChar = #13) do
    NextChar;
end;

function TLexer.CheckString(const Str: string): Boolean;
var
  i: Integer;
  TempPos: Integer;
begin
  if FPosition + Length(Str) > FLength then
  begin
    Result := False;
    Exit;
  end;
  
  TempPos := FPosition;
  for i := 1 to Length(Str) do
  begin
    if FInput[TempPos + 1] <> Str[i] then
    begin
      Result := False;
      Exit;
    end;
    Inc(TempPos);
  end;
  
  Result := True;
end;

procedure TLexer.SkipChars(Count: Integer);
var
  i: Integer;
begin
  for i := 1 to Count do
  begin
    NextChar;
    if FCurrentChar = #0 then
      Break;
  end;
end;

function TLexer.IsHexDigit(c: Char): Boolean;
begin
  Result := (c in ['0'..'9']) or (c in ['A'..'F']) or (c in ['a'..'f']);
end;

function TLexer.HexValue(c: Char): Integer;
begin
  if c in ['0'..'9'] then
    Result := Ord(c) - Ord('0')
  else if c in ['A'..'F'] then
    Result := Ord(c) - Ord('A') + 10
  else if c in ['a'..'f'] then
    Result := Ord(c) - Ord('a') + 10
  else
    Result := 0;
end;

function TLexer.ParseUnicodeEscape: Char;
var
  i, Value: Integer;
begin
  Value := 0;
  for i := 1 to 4 do
  begin
    if not IsHexDigit(FCurrentChar) then
    begin
      Result := #0;
      Exit;
    end;
    Value := Value * 16 + HexValue(FCurrentChar);
    NextChar;
  end;
  
  if Value < 128 then
    Result := Chr(Value)
  else
    Result := '?';
end;

{ ============================================================================ }
{ TJSONParser Implementation }
{ ============================================================================ }

constructor TJSONParser.Create;
begin
  inherited Create;
  FLexer := nil;
  FParseError := False;
  FErrorMessage := '';
end;

destructor TJSONParser.Destroy;
begin
  if FLexer <> nil then
    FLexer.Free;
  inherited Destroy;
end;

procedure TJSONParser.SetError(const Msg: String);
begin
  FParseError := True;
  FErrorMessage := Msg;
end;

function TJSONParser.ParseString(var Str: TJSONString; var Len: Integer): Boolean;
begin
  Len := 0;
  if FLexer.CurrentChar <> '"' then
  begin
    SetError('Expected string');
    Result := False;
    Exit;
  end;
  
  FLexer.NextChar;
  
  while (FLexer.CurrentChar <> '"') and (FLexer.CurrentChar <> #0) and (Len < MAX_STRING_LEN) do
  begin
    if FLexer.CurrentChar = '\' then
    begin
      FLexer.NextChar;
      if FLexer.CurrentChar = #0 then
      begin
        SetError('Unexpected end of string');
        Result := False;
        Exit;
      end;
      
      case FLexer.CurrentChar of
        '"': begin Inc(Len); if Len <= MAX_STRING_LEN then Str[Len] := '"'; end;
        '\': begin Inc(Len); if Len <= MAX_STRING_LEN then Str[Len] := '\'; end;
        '/': begin Inc(Len); if Len <= MAX_STRING_LEN then Str[Len] := '/'; end;
        'b': begin Inc(Len); if Len <= MAX_STRING_LEN then Str[Len] := #8; end;
        'f': begin Inc(Len); if Len <= MAX_STRING_LEN then Str[Len] := #12; end;
        'n': begin Inc(Len); if Len <= MAX_STRING_LEN then Str[Len] := #10; end;
        'r': begin Inc(Len); if Len <= MAX_STRING_LEN then Str[Len] := #13; end;
        't': begin Inc(Len); if Len <= MAX_STRING_LEN then Str[Len] := #9; end;
        'u': begin 
          FLexer.NextChar;
          Inc(Len); 
          if Len <= MAX_STRING_LEN then 
            Str[Len] := FLexer.ParseUnicodeEscape;
          Continue;
        end;
        else
        begin
          Inc(Len); 
          if Len <= MAX_STRING_LEN then 
            Str[Len] := FLexer.CurrentChar;
        end;
      end;
    end
    else
    begin
      Inc(Len);
      if Len <= MAX_STRING_LEN then
        Str[Len] := FLexer.CurrentChar;
    end;
    FLexer.NextChar;
  end;
  
  if FLexer.CurrentChar = '"' then
  begin
    FLexer.NextChar;
    Result := True;
  end
  else
  begin
    SetError('Unterminated string');
    Result := False;
  end;
end;

function TJSONParser.ParseNumber(var Num: Real): Boolean;
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
  
  if FLexer.CurrentChar = '-' then
  begin
    Sign := -1;
    FLexer.NextChar;
  end;
  
  if not (FLexer.CurrentChar in ['0'..'9']) then
  begin
    SetError('Invalid number format');
    Result := False;
    Exit;
  end;
  
  if FLexer.CurrentChar = '0' then
  begin
    FLexer.NextChar;
    if FLexer.CurrentChar in ['0'..'9'] then
    begin
      SetError('Invalid number: leading zeros not allowed');
      Result := False;
      Exit;
    end;
  end
  else
  begin
    while FLexer.CurrentChar in ['0'..'9'] do
    begin
      IntPart := IntPart * 10 + (Ord(FLexer.CurrentChar) - Ord('0'));
      FLexer.NextChar;
    end;
  end;
  
  if FLexer.CurrentChar = '.' then
  begin
    FLexer.NextChar;
    if not (FLexer.CurrentChar in ['0'..'9']) then
    begin
      SetError('Invalid number: decimal point must be followed by digits');
      Result := False;
      Exit;
    end;
    
    while FLexer.CurrentChar in ['0'..'9'] do
    begin
      FracDiv := FracDiv * 10;
      FracPart := FracPart * 10 + (Ord(FLexer.CurrentChar) - Ord('0'));
      FLexer.NextChar;
    end;
  end;
  
  if (FLexer.CurrentChar = 'e') or (FLexer.CurrentChar = 'E') then
  begin
    HasExp := True;
    FLexer.NextChar;
    
    if (FLexer.CurrentChar = '+') or (FLexer.CurrentChar = '-') then
    begin
      if FLexer.CurrentChar = '-' then
        ExpSign := -1;
      FLexer.NextChar;
    end;
    
    if not (FLexer.CurrentChar in ['0'..'9']) then
    begin
      SetError('Invalid number: exponent must contain digits');
      Result := False;
      Exit;
    end;
    
    while FLexer.CurrentChar in ['0'..'9'] do
    begin
      Exp := Exp * 10 + (Ord(FLexer.CurrentChar) - Ord('0'));
      FLexer.NextChar;
    end;
    Exp := ExpSign * Exp;
  end;
  
  Num := Sign * (IntPart + FracPart / FracDiv);
  if HasExp then
  begin
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
  
  Result := True;
end;

function TJSONParser.ParseArray: TJSONArray;
var
  Arr: TJSONArray;
  Item: TJSONValue;
begin
  Inc(FLexer.FCurrentDepth);
  if FLexer.CurrentDepth > MAX_JSON_DEPTH then
  begin
    SetError('JSON nesting too deep');
    Result := nil;
    Exit;
  end;
  
  Arr := TJSONArray.Create;
  
  if FLexer.CurrentChar <> '[' then
  begin
    SetError('Expected [');
    Arr.Free;
    Dec(FLexer.FCurrentDepth);
    Result := nil;
    Exit;
  end;
  
  FLexer.NextChar;
  FLexer.SkipWhitespace;
  
  if FLexer.CurrentChar = ']' then
  begin
    FLexer.NextChar;
    Dec(FLexer.FCurrentDepth);
    Result := Arr;
    Exit;
  end;
  
  while True do
  begin
    Item := ParseValue;
    if (Item = nil) or FParseError then
    begin
      Arr.Free;
      Dec(FLexer.FCurrentDepth);
      Result := nil;
      Exit;
    end;
    
    if not Arr.Add(Item) then
    begin
      SetError('Array too large');
      Item.Free;
      Arr.Free;
      Dec(FLexer.FCurrentDepth);
      Result := nil;
      Exit;
    end;
    
    FLexer.SkipWhitespace;
    
    if FLexer.CurrentChar = ']' then
    begin
      FLexer.NextChar;
      Break;
    end
    else if FLexer.CurrentChar = ',' then
    begin
      FLexer.NextChar;
      FLexer.SkipWhitespace;
      if FLexer.CurrentChar = ']' then
      begin
        SetError('Trailing comma in array');
        Arr.Free;
        Dec(FLexer.FCurrentDepth);
        Result := nil;
        Exit;
      end;
    end
    else
    begin
      SetError('Expected , or ]');
      Arr.Free;
      Dec(FLexer.FCurrentDepth);
      Result := nil;
      Exit;
    end;
  end;
  
  Dec(FLexer.FCurrentDepth);
  Result := Arr;
end;

function TJSONParser.ParseObject: TJSONObject;
var
  Obj: TJSONObject;
  Key: TJSONString;
  KeyLen: Integer;
  Value: TJSONValue;
begin
  Inc(FLexer.FCurrentDepth);
  if FLexer.CurrentDepth > MAX_JSON_DEPTH then
  begin
    SetError('JSON nesting too deep');
    Result := nil;
    Exit;
  end;
  
  Obj := TJSONObject.Create;
  
  if FLexer.CurrentChar <> '{' then
  begin
    SetError('Expected {');
    Obj.Free;
    Dec(FLexer.FCurrentDepth);
    Result := nil;
    Exit;
  end;
  
  FLexer.NextChar;
  FLexer.SkipWhitespace;
  
  if FLexer.CurrentChar = '}' then
  begin
    FLexer.NextChar;
    Dec(FLexer.FCurrentDepth);
    Result := Obj;
    Exit;
  end;
  
  while True do
  begin
    if FLexer.CurrentChar <> '"' then
    begin
      SetError('Expected string key');
      Obj.Free;
      Dec(FLexer.FCurrentDepth);
      Result := nil;
      Exit;
    end;
    
    if not ParseString(Key, KeyLen) or FParseError then
    begin
      Obj.Free;
      Dec(FLexer.FCurrentDepth);
      Result := nil;
      Exit;
    end;
    
    FLexer.SkipWhitespace;
    
    if FLexer.CurrentChar <> ':' then
    begin
      SetError('Expected :');
      Obj.Free;
      Dec(FLexer.FCurrentDepth);
      Result := nil;
      Exit;
    end;
    
    FLexer.NextChar;
    FLexer.SkipWhitespace;
    
    Value := ParseValue;
    if (Value = nil) or FParseError then
    begin
      Obj.Free;
      Dec(FLexer.FCurrentDepth);
      Result := nil;
      Exit;
    end;
    
    if Obj.Count >= MAX_OBJECT_KEYS then
    begin
      SetError('Object has too many keys');
      Value.Free;
      Obj.Free;
      Dec(FLexer.FCurrentDepth);
      Result := nil;
      Exit;
    end;
    
    Obj.AddPair(Key, KeyLen, Value);
    
    FLexer.SkipWhitespace;
    
    if FLexer.CurrentChar = '}' then
    begin
      FLexer.NextChar;
      Break;
    end
    else if FLexer.CurrentChar = ',' then
    begin
      FLexer.NextChar;
      FLexer.SkipWhitespace;
      if FLexer.CurrentChar = '}' then
      begin
        SetError('Trailing comma in object');
        Obj.Free;
        Dec(FLexer.FCurrentDepth);
        Result := nil;
        Exit;
      end;
    end
    else
    begin
      SetError('Expected , or }');
      Obj.Free;
      Dec(FLexer.FCurrentDepth);
      Result := nil;
      Exit;
    end;
  end;
  
  Dec(FLexer.FCurrentDepth);
  Result := Obj;
end;

function TJSONParser.ParseValue: TJSONValue;
var
  Str: TJSONString;
  StrLen: Integer;
  Num: Real;
begin
  FLexer.SkipWhitespace;
  
  if FParseError then
  begin
    Result := nil;
    Exit;
  end;
  
  case FLexer.CurrentChar of
    'n':
    begin
      if FLexer.CheckString('null') then
      begin
        FLexer.SkipChars(4);
        Result := TJSONNull.Create;
      end
      else
      begin
        SetError('Invalid literal');
        Result := nil;
      end;
    end;
    
    't':
    begin
      if FLexer.CheckString('true') then
      begin
        FLexer.SkipChars(4);
        Result := TJSONBoolean.Create(True);
      end
      else
      begin
        SetError('Invalid literal');
        Result := nil;
      end;
    end;
    
    'f':
    begin
      if FLexer.CheckString('false') then
      begin
        FLexer.SkipChars(5);
        Result := TJSONBoolean.Create(False);
      end
      else
      begin
        SetError('Invalid literal');
        Result := nil;
      end;
    end;
    
    '"':
    begin
      if ParseString(Str, StrLen) and not FParseError then
        Result := TJSONStringValue.Create(Str, StrLen)
      else
        Result := nil;
    end;
    
    '[':
    begin
      Result := ParseArray;
    end;
    
    '{':
    begin
      Result := ParseObject;
    end;
    
    #0:
    begin
      SetError('Unexpected end of input');
      Result := nil;
    end;
    
    else
    begin
      if (FLexer.CurrentChar in ['-', '0'..'9']) and ParseNumber(Num) and not FParseError then
        Result := TJSONNumber.Create(Num)
      else
      begin
        if not FParseError then
          SetError('Invalid character');
        Result := nil;
      end;
    end;
  end;
end;

function TJSONParser.Parse(const Input: String): TJSONValue;
var
  ResultValue: TJSONValue;
begin
  FParseError := False;
  FErrorMessage := '';
  
  if FLexer <> nil then
    FLexer.Free;
  
  FLexer := TLexer.Create(Input, Length(Input));
  ResultValue := ParseValue;
  
  if not FParseError and (ResultValue <> nil) then
  begin
    FLexer.SkipWhitespace;
    if FLexer.CurrentChar <> #0 then
    begin
      SetError('Unexpected characters after JSON');
      ResultValue.Free;
      ResultValue := nil;
    end;
  end;
  
  // 更新全局变量
  GlobalParseError := FParseError;
  GlobalErrorMessage := FErrorMessage;
  
  Result := ResultValue;
end;

{ ============================================================================ }
{ Main Program }
{ ============================================================================ }

var
  TestJSON: String;
  Parser: TJSONParser;
  ParsedValue: TJSONValue;
  StartTime, EndTime, ElapsedTime: Integer;

begin
  StartTime := GetTickCount64();
  
  TestJSON := '{"name":"John","age":30,"city":"New York","isStudent":false,"courses":["Math","Physics"],"address":{"street":"123 Main St","zipcode":"10001"}}';
  
  WriteLn('Parsing JSON: ', TestJSON);
  WriteLn;
  
  Parser := TJSONParser.Create;
  try
    ParsedValue := Parser.Parse(TestJSON);
    
    if Parser.HasError then
    begin
      WriteLn('Error parsing JSON: ', Parser.ErrorMessage);
      WriteLn('Global Error: ', GlobalErrorMessage);
    end
    else
    begin
      WriteLn('Parsed JSON:');
      if ParsedValue <> nil then
      begin
        ParsedValue.Print(0);
        WriteLn;
      end;
    end;
    
    if ParsedValue <> nil then
      ParsedValue.Free;
  finally
    Parser.Free;
  end;
  
  EndTime := GetTickCount64();
  ElapsedTime := EndTime - StartTime;
  
  WriteLn('========== 执行结果 ==========');
  WriteLn('总耗时: ', ElapsedTime, ' 毫秒');
  WriteLn('总耗时: ', ElapsedTime div 1000, ' 秒');
  WriteLn;
end.