# Design Document

## Overview

本设计文档描述了为Pascal子集解释器添加break和continue循环控制语句的技术实现方案。该功能将通过扩展现有的AST节点、解析器、语义分析器和解释器来实现，确保与现有架构的无缝集成。

## Architecture

### 核心组件修改

1. **AST节点扩展** - 添加新的Break和Continue语句节点
2. **词法分析器扩展** - 添加break和continue关键字识别
3. **解析器扩展** - 添加break和continue语句的语法解析
4. **语义分析器扩展** - 添加循环作用域跟踪和验证
5. **解释器扩展** - 添加循环控制流异常处理机制

### 控制流机制

使用Python异常机制来实现循环控制流：
- `BreakSignal` - 用于break语句的控制流转移
- `ContinueSignal` - 用于continue语句的控制流转移

这种方法利用Python的异常处理机制，能够自然地处理嵌套循环场景。

## Components and Interfaces

### 1. AST节点定义

```python
class BreakStatement(Statement):
    """表示break语句的AST节点"""
    def __init__(self, token: Token):
        self.token = token

class ContinueStatement(Statement):
    """表示continue语句的AST节点"""
    def __init__(self, token: Token):
        self.token = token
```

### 2. 控制流信号类

```python
class BreakSignal(Exception):
    """用于break语句的控制流信号"""
    pass

class ContinueSignal(Exception):
    """用于continue语句的控制流信号"""
    pass
```

### 3. 词法分析器扩展

在`TokenType`枚举中添加：
- `BREAK` - break关键字
- `CONTINUE` - continue关键字

在保留字字典中添加对应映射。

### 4. 解析器扩展

在`Parser`类中添加：
- `statement()` 方法中添加break和continue语句的解析分支
- `break_statement()` 方法 - 解析break语句
- `continue_statement()` 方法 - 解析continue语句

### 5. 语义分析器扩展

在`SemanticAnalyzer`类中添加：
- 循环作用域栈跟踪机制
- `visit_BreakStatement()` 方法 - 验证break语句的使用
- `visit_ContinueStatement()` 方法 - 验证continue语句的使用
- 循环语句访问时的作用域管理

### 6. 解释器扩展

在`Interpreter`类中添加：
- `visit_BreakStatement()` 方法 - 抛出BreakSignal
- `visit_ContinueStatement()` 方法 - 抛出ContinueSignal
- 修改`visit_WhileStatement()`和`visit_ForStatement()`方法以捕获和处理控制流信号

## Data Models

### 循环作用域跟踪

```python
class LoopScope:
    """循环作用域信息"""
    def __init__(self, loop_type: str):
        self.loop_type = loop_type  # 'for' 或 'while'
```

语义分析器维护一个循环作用域栈：
```python
self.loop_stack: list[LoopScope] = []
```

### Token扩展

```python
# 在TokenType枚举中添加
BREAK = 'BREAK'
CONTINUE = 'CONTINUE'

# 在保留字字典中添加
RESERVED_KEYWORDS = {
    # ... 现有关键字
    'BREAK': Token(BREAK, 'BREAK'),
    'CONTINUE': Token(CONTINUE, 'CONTINUE'),
}
```

## Error Handling

### 语义错误

1. **循环外使用错误**
   - 错误码：`BREAK_OUTSIDE_LOOP` / `CONTINUE_OUTSIDE_LOOP`
   - 消息：`"Break/Continue statement must be inside a loop"`

2. **语法错误**
   - 缺少分号：使用现有的语法错误处理机制

### 错误检测流程

1. 语义分析阶段检测break/continue是否在循环内
2. 如果不在循环内，抛出`SemanticError`
3. 提供清晰的错误位置和消息

## Testing Strategy

### 单元测试

1. **词法分析器测试**
   - 测试break和continue关键字的正确识别
   - 测试大小写敏感性

2. **解析器测试**
   - 测试break和continue语句的正确解析
   - 测试语法错误情况（缺少分号等）

3. **语义分析器测试**
   - 测试循环内使用的正确验证
   - 测试循环外使用的错误检测
   - 测试嵌套循环的正确处理

4. **解释器测试**
   - 测试break语句的正确执行
   - 测试continue语句的正确执行
   - 测试嵌套循环中的行为

### 集成测试

1. **简单循环测试**
   - for循环中的break和continue
   - while循环中的break和continue

2. **嵌套循环测试**
   - 内层循环的break不影响外层循环
   - 内层循环的continue不影响外层循环

3. **边界条件测试**
   - 循环第一次迭代就break/continue
   - 循环最后一次迭代的break/continue

### 测试用例示例

```pascal
// 基本break测试
program TestBreak;
var i: integer;
begin
    for i := 1 to 10 do
    begin
        if i = 5 then
            break;
        writeln(i);
    end;
end.

// 基本continue测试
program TestContinue;
var i: integer;
begin
    for i := 1 to 10 do
    begin
        if i mod 2 = 0 then
            continue;
        writeln(i);
    end;
end.

// 嵌套循环测试
program TestNested;
var i, j: integer;
begin
    for i := 1 to 3 do
    begin
        for j := 1 to 3 do
        begin
            if j = 2 then
                break;  // 只退出内层循环
            writeln(i, j);
        end;
        writeln('外层循环继续');
    end;
end.
```

## Implementation Notes

### 信号处理机制

使用Python的异常处理机制实现控制流转移是最自然和高效的方法：

1. **性能考虑** - 信号处理在正常控制流中性能开销可接受
2. **代码清晰性** - 信号机制使控制流逻辑清晰明了
3. **嵌套处理** - 自然支持嵌套循环的正确处理

### 向后兼容性

所有修改都是增量式的，不会影响现有功能：
- 新增的关键字不会与现有标识符冲突
- AST节点扩展不影响现有节点
- 解析器扩展不改变现有语法规则

### 扩展性考虑

设计支持未来可能的扩展：
- 可以轻松添加带标签的break/continue（如果需要）
- 控制流信号机制可用于其他控制流语句（如return）