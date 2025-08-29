# Simple Pascal Interpreter 项目总结

## 项目结构

```
.
├── .gitignore
├── .vscode/
│   └── launch.json
├── Makefile
├── auto_type.py
├── gen_ast_dot.py
├── main.py
├── mypy.ini
├── pas/
│   ├── data_types/
│   ├── decision_making/
│   ├── function/
│   ├── io/
│   ├── loop/
│   └── operation/
├── pyproject.toml
├── readme.md
├── ruff.toml
├── sample.pas
├── spi.py
├── src/
│   ├── error.py
│   ├── globals.py
│   ├── interpreter.py
│   ├── lexer.py
│   ├── object.py
│   ├── parser.py
│   ├── sematic_analyzer.py
│   ├── spi_ast.py
│   ├── spi_token.py
│   ├── symbol.py
│   ├── util.py
│   └── visitor.py
└── test_interpreter.py
```

## 项目概述

这是一个基于树遍历解析器实现的Pascal子集解释器，源自[Let's build a simple interpreter](https://github.com/rspivak/lsbasi)项目。该项目实现了Pascal语言的一个子集，支持多种数据类型、控制结构、函数和过程、类等特性。

## 实现的Pascal子集

### 数据类型

1. **基本类型**
   - integer (整数)
   - real (实数)
   - bool (布尔值)

2. **派生类型**
   - enum (枚举)
   - const (常量)

3. **复合类型**
   - string (字符串)
   - class (类)
   - record (记录)
   - array (数组)

注意：不支持类继承，仅支持类字段、方法、构造函数和析构函数。

### 输入输出

支持以下IO操作：
```pascal
Write(a);
WriteLn(a);
WriteLn(a+b);
Read(a);
ReadLn(a);
WriteLn('a = ',a,' ','b = ',b);
```

不支持string.format()功能。

### 函数和过程

支持过程(procedure)和函数(function)，当内部参数与外部参数同名时，内部参数会隐藏外部参数。

### 控制流

- for 循环
- while 循环
- if-else 条件语句
- case-of 选择语句

### 运算

- 比较运算
- 逻辑运算

### Object Pascal

支持以下Object Pascal指令（仅在复制粘贴到Pascal编译器时有用）：
```pascal
{$mode objfpc} // 定义类的指令
{$m+}         // 使用构造函数的指令
```

### 内置函数

- READ
- READLN
- WRITE
- WRITELN
- LENGTH
- SETLENGTH
- LOW
- HIGH
- ORD

## 项目组件

### 词法分析器 (Lexer)
- 位于 `src/lexer.py`
- 负责将Pascal源代码转换为标记(tokens)
- 支持关键字、标识符、数字、字符串、运算符等的识别

### 语法分析器 (Parser)
- 位于 `src/parser.py`
- 实现递归下降解析
- 将标记流转换为抽象语法树(AST)
- 支持完整的Pascal语法结构解析

### 语义分析器 (Semantic Analyzer)
- 位于 `src/sematic_analyzer.py`
- 进行类型检查和符号表管理
- 检查变量声明、作用域、类型一致性等

### 解释器 (Interpreter)
- 位于 `src/interpreter.py`
- 遍历AST执行程序
- 管理调用栈和激活记录
- 实现所有内置函数和操作

### 错误处理
- 位于 `src/error.py`
- 定义各种错误类型，包括词法错误、语法错误、语义错误和解释错误

### 符号表
- 位于 `src/symbol.py`
- 管理程序中的标识符和类型信息
- 支持作用域链和符号查找

## 测试

项目包含完整的测试套件 `test_interpreter.py`，涵盖了：
- 词法分析测试
- 语法分析测试
- 语义分析测试
- 解释执行测试

## 已知限制

1. 无法区分没有赋值操作符的函数调用和过程调用：
```pascal
begin
    call();  // 会被解析为过程调用
end.
```
需要写成以下形式才能正确识别为函数调用：
```pascal
begin
   a := call();  // 可以正确识别为函数调用
end.
```

2. 枚举变量不会占用全局标识符，应该使用全名如"Day.Sun"而不是"Sun"。
        