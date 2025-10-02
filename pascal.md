

# Pascal Syntax Subset Summary

This Pascal JSON parser program uses core syntax features of the Pascal language. The following is a summary organized by syntax category, along with example code for each syntax point.

Additionally, I have created a separate `pascal_examples` directory containing independent example programs for each Pascal syntax feature, making it convenient for you to learn and test each syntax point.

## 1. Program Structure

### program statement
```pascal
program JSONParser;
```

## 2. Data Types

### Basic Data Types
- `Integer` - Integer type
- `Real` - Floating-point type
- `Boolean` - Boolean type
- `Char` - Character type
- `String` - String type

### Enumeration Type
```pascal
type
  TJSONType = (jtNull, jtBoolean, jtNumber, jtString, jtArray, jtObject);
```

### Array Type
```pascal
type
  TJSONString = array[1..MAX_STRING_LEN] of Char;
```

### Record (Struct) Type
```pascal
type
  TJSONArray = record
    Items: array[1..MAX_ARRAY_SIZE] of PJSONValue;
    Count: Integer;
  end;
```

### Variant Record Type
```pascal
type
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
```

## 3. Constant Definitions

```pascal
const
  MAX_JSON_DEPTH = 100;
  MAX_STRING_LEN = 1000;
```

## 4. Variable Declarations

### Global Variables
```pascal
var
  ParseError: Boolean;
  ErrorMessage: String;
```

### Local Variables
```pascal
procedure Example;
var
  i: Integer;
  Value: Real;
begin
  // ...
end;
```

## 5. Procedures and Functions

### Procedure Definition
```pascal
procedure SetError(const Msg: String);
begin
  ParseError := True;
  ErrorMessage := Msg;
end;
```

### Function Definition
```pascal
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
```

### Parameter Passing
- `const` parameters: Passed by reference but cannot be modified
- `var` parameters: Passed by reference and can be modified
- Default parameters: Passed by value (simple types) or by reference (complex types)

### Forward Declaration
```pascal
function ParseValue(var Lexer: TLexer): PJSONValue; forward;
```

## 6. Control Structures

### Conditional Statements

#### if statement
```pascal
if SrcLen > MAX_STRING_LEN then
  SrcLen := MAX_STRING_LEN;
```

#### if-else statement
```pascal
if Value = nil then
  Exit
else
begin
  // Handle Value
end;
```

#### case statement
```pascal
case Value^.JSONType of
  jtNull: Write('null');
  jtBoolean: 
    if Value^.BoolValue then Write('true') else Write('false');
  jtNumber: Write(Value^.NumValue:0:6);
  jtString:
  begin
    // Handle string
  end;
  jtArray: 
  begin
    // Handle array
  end;
end;
```

### Loop Statements

#### for loop (counting loop)
```pascal
for i := 1 to SrcLen do
  Dest[i] := Src[i];
```

#### while loop
```pascal
while (Lexer.CurrentChar = ' ') or (Lexer.CurrentChar = #9) or 
      (Lexer.CurrentChar = #10) or (Lexer.CurrentChar = #13) do
  NextChar(Lexer);
```

## 7. Expressions and Operators

### Arithmetic Operators
- `+` Addition
- `-` Subtraction
- `*` Multiplication
- `/` Division
- `mod` Modulo

### Comparison Operators
- `=` Equal to
- `<>` Not equal to
- `<` Less than
- `>` Greater than
- `<=` Less than or equal to
- `>=` Greater than or equal to

### Logical Operators
- `and` Logical AND
- `or` Logical OR
- `not` Logical NOT

### Set Operators
```pascal
if c in ['0'..'9'] then
```

### Character and String Handling
- `Ord(c)` - Get ASCII code of a character
- `Chr(value)` - Convert ASCII code to character
- `Length(str)` - Get string length

## 8. Standard Functions and Procedures

### Input/Output
- `Write()` - Output (without newline)
- `WriteLn()` - Output (with newline)

### Other Functions
- `Inc(var)` - Increment variable by 1
- `Dec(var)` - Decrement variable by 1
- `Exit` - Exit procedure or function
- `SetLength(arr, count)`
- `Ord(ch)`
- `Chr(ch)`
- `Length(collection)`

## 9. Special Syntax

### Comments
```pascal
{ Only single-line comments are supported }
```

### Character Constants
```pascal
#0   // Null character
#8   // Backspace
#10  // Line feed
```

### Formatted Output (not supported)
```pascal
Write(Value^.NumValue:0:6);  // Output floating-point number with 6 decimal places
```