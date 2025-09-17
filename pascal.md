# Pascal语法子集总结

这个Pascal JSON解析器程序使用了Pascal语言的核心语法特性。以下是按语法类别整理的总结，以及每个语法点的示例代码。

此外，我还创建了一个独立的`pascal_examples`目录，其中包含了每个Pascal语法特性的独立示例程序，方便您学习和测试每个语法点。

## 1. 程序结构

### program语句
```pascal
program JSONParser;
```

### uses子句（未使用）
此程序未使用任何单元（units）。

## 2. 数据类型

### 基本数据类型
- `Integer` - 整数类型
- `Real` - 浮点数类型
- `Boolean` - 布尔类型
- `Char` - 字符类型
- `String` - 字符串类型

### 枚举类型
```pascal
type
  TJSONType = (jtNull, jtBoolean, jtNumber, jtString, jtArray, jtObject);
```

### 数组类型
```pascal
type
  TJSONString = array[1..MAX_STRING_LEN] of Char;
```

### 记录（结构体）类型
```pascal
type
  TJSONArray = record
    Items: array[1..MAX_ARRAY_SIZE] of PJSONValue;
    Count: Integer;
  end;
```

### 指针类型
```pascal
type
  PJSONValue = ^TJSONValue;
```

### 变体记录类型
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

## 3. 常量定义

```pascal
const
  MAX_JSON_DEPTH = 100;
  MAX_STRING_LEN = 1000;
```

## 4. 变量声明

### 全局变量
```pascal
var
  ParseError: Boolean;
  ErrorMessage: String;
```

### 局部变量
```pascal
procedure Example;
var
  i: Integer;
  Value: Real;
begin
  // ...
end;
```

## 5. 过程和函数

### 过程定义
```pascal
procedure SetError(const Msg: String);
begin
  ParseError := True;
  ErrorMessage := Msg;
end;
```

### 函数定义
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

### 参数传递
- `const`参数：按引用传递但不可修改
- 默认参数：按值传递（简单类型）或按引用传递（复杂类型）

### 前向声明
```pascal
function ParseValue(var Lexer: TLexer): PJSONValue; forward;
```

## 6. 控制结构

### 条件语句

#### if语句
```pascal
if SrcLen > MAX_STRING_LEN then
  SrcLen := MAX_STRING_LEN;
```

#### if-else语句
```pascal
if Value = nil then
  Exit
else
begin
  // 处理Value
end;
```

#### case语句
```pascal
case Value^.JSONType of
  jtNull: Write('null');
  jtBoolean: 
    if Value^.BoolValue then Write('true') else Write('false');
  jtNumber: Write(Value^.NumValue:0:6);
  jtString:
  begin
    // 处理字符串
  end;
  jtArray: 
  begin
    // 处理数组
  end;
end;
```

### 循环语句

#### for循环（计数循环）
```pascal
for i := 1 to SrcLen do
  Dest[i] := Src[i];
```

#### while循环
```pascal
while (Lexer.CurrentChar = ' ') or (Lexer.CurrentChar = #9) or 
      (Lexer.CurrentChar = #10) or (Lexer.CurrentChar = #13) do
  NextChar(Lexer);
```

#### repeat循环（未使用）

## 7. 表达式和运算符

### 算术运算符
- `+` 加法
- `-` 减法
- `*` 乘法
- `/` 除法
- `mod` 取模

### 比较运算符
- `=` 等于
- `<>` 不等于
- `<` 小于
- `>` 大于
- `<=` 小于等于
- `>=` 大于等于

### 逻辑运算符
- `and` 逻辑与
- `or` 逻辑或
- `not` 逻辑非

### 集合运算符
```pascal
if c in ['0'..'9'] then
```

### 字符和字符串处理
- `Ord(c)` - 获取字符的ASCII码
- `Chr(value)` - 将ASCII码转换为字符
- `Length(str)` - 获取字符串长度

## 8. 标准函数和过程

### 输入输出
- `Write()` - 输出（不换行）
- `WriteLn()` - 输出（换行）

### 动态内存管理
- `New(ptr)` - 分配内存
- `Dispose(ptr)` - 释放内存

### 其他函数
- `Inc(var)` - 变量加1
- `Dec(var)` - 变量减1
- `Exit` - 退出过程或函数

## 9. 特殊语法

### 注释
```pascal
// 单行注释

{
  多行注释
}
```

### 字符常量
```pascal
#0   // 空字符
#8   // 退格符
#10  // 换行符
```

### 格式化输出
```pascal
Write(Value^.NumValue:0:6);  // 输出浮点数，保留6位小数
```