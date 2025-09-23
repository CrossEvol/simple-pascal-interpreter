# Design Document

## Overview

This design implements subrange types, set literals, and the `in` operator for the Pascal interpreter. The implementation extends the existing AST, lexer, parser, semantic analyzer, and interpreter components to support:

1. **Subrange Types**: Range expressions using `..` operator (e.g., `1..10`)
2. **Set Literals**: Temporary sets with elements and ranges (e.g., `[1, 3..5, 8]`)
3. **In Operator**: Membership testing for sets and ranges (e.g., `value in [1, 3, 5]`)
4. **Array Integration**: Refactoring array bounds to use subrange types

## Architecture

### Component Changes

1. **Token System**: Add `IN` token for the `in` operator
2. **AST Nodes**: Add `SubrangeType`, `SetLiteral`, and `InOperator` nodes
3. **Lexer**: Recognize `IN` keyword and handle set literal brackets
4. **Parser**: Parse subrange expressions, set literals, and `in` operations
5. **Semantic Analyzer**: Validate set element types and simple subrange bounds
6. **Interpreter**: Evaluate subranges, sets, and membership operations
7. **Array Types**: Modify `ArrayType` to use `SubrangeType` for bounds

## Components and Interfaces

### AST Node Definitions

```python
class SubrangeType(Type):
    """Represents a subrange type like 1..10"""
    def __init__(self, token: Token, lower: Expression, upper: Expression):
        super().__init__(token)
        self.lower = lower
        self.upper = upper
    
    def contains(self, value: int) -> bool:
        """Check if value is within the subrange bounds"""
        pass

class SetLiteral(Expression):
    """Represents a set literal like [1, 3..5, 8]"""
    def __init__(self, token: Token, elements: list[Expression]):
        self.token = token
        self.elements = elements  # Mix of individual values and SubrangeType

class InOperator(Expression):
    """Represents membership test like 'value in set'"""
    def __init__(self, value: Expression, set_expr: Expression, token: Token):
        self.value = value
        self.set_expr = set_expr
        self.token = token
```

### Token Extensions

```python
# Add to TokenType enum
IN = "IN"

# Add to reserved keywords in lexer
```

### Parser Grammar Extensions

```
subrange_type: summation_expr RANGE summation_expr

set_literal: LBRACKET (set_element (COMMA set_element)*)? RBRACKET

set_element: summation_expr (RANGE summation_expr)?

in_expression: summation_expr IN (set_literal | subrange_type | variable)

comparison_expr: summation_expr ((EQ | NE | LT | LE | GT | GE | IN) summation_expr)*
```

### Semantic Analysis

1. **Simple Subrange Validation**: Only validate bounds for literal numbers and characters (no complex expressions)
2. **Set Element Types**: Validate all elements are compatible types
3. **In Operator Types**: Ensure left operand is compatible with set element type
4. **Array Bounds**: Convert existing array range expressions to SubrangeType

### Interpreter Evaluation

1. **Subrange Objects**: Create runtime objects that can check containment
2. **Complex Subrange Validation**: Evaluate and validate subrange bounds at runtime
3. **Set Expansion**: Expand ranges in set literals to actual sets
4. **Membership Testing**: Implement efficient `in` operation
5. **Array Integration**: Use subrange bounds for array index validation

## Data Models

### SubrangeObject

```python
class SubrangeObject(Object):
    def __init__(self, lower: int, upper: int):
        self.lower = lower
        self.upper = upper
    
    def contains(self, value: int) -> bool:
        return self.lower <= value <= self.upper
    
    def to_set(self) -> set[int]:
        return set(range(self.lower, self.upper + 1))
```

### SetObject

```python
class SetObject(Object):
    def __init__(self, elements: set[int]):
        self.elements = elements
    
    def contains(self, value: int) -> bool:
        return value in self.elements
```

## Error Handling

### New Error Codes

```python
SEMANTIC_SUBRANGE_INVALID = "Invalid subrange bounds"
SEMANTIC_SET_TYPE_MISMATCH = "Set element type mismatch"
INTERPRETER_SUBRANGE_VIOLATION = "Value outside subrange bounds"
INTERPRETER_IN_OPERATOR_INVALID = "Invalid operands for 'in' operator"
```

### Error Scenarios

1. **Invalid Subrange**: Lower bound > upper bound
2. **Type Mismatch**: Incompatible types in set literals
3. **Runtime Bounds**: Value assignment outside subrange
4. **Invalid In Operation**: Non-set/range right operand

## Testing Strategy

### Unit Tests

1. **Lexer Tests**: Verify `IN` token recognition
2. **Parser Tests**: Parse subrange, set literals, and `in` expressions
3. **Semantic Tests**: Validate type checking and error detection
4. **Interpreter Tests**: Evaluate operations and check results

### Integration Tests

1. **Array Compatibility**: Ensure existing array tests pass
2. **Complex Expressions**: Nested sets and ranges
3. **Type System**: Subrange type declarations and usage
4. **Error Handling**: Comprehensive error scenario coverage

### Regression Tests

1. **Existing Array Tests**: All current array functionality must continue working
2. **Parser Compatibility**: Existing parsing behavior preserved
3. **Interpreter Behavior**: No changes to non-subrange/set operations

## Implementation Phases

### Phase 1: Core AST and Token Support
- Add new AST nodes
- Add `IN` token
- Basic lexer support

### Phase 2: Parser Extensions
- Parse subrange expressions
- Parse set literals
- Parse `in` operator expressions

### Phase 3: Semantic Analysis
- Simple subrange bounds validation (literals only)
- Type check set elements
- Validate `in` operator usage

### Phase 4: Interpreter Implementation
- Evaluate subranges and sets
- Complex subrange bounds validation at runtime
- Implement membership testing
- Runtime bounds checking

### Phase 5: Array Integration
- Refactor ArrayType to use SubrangeType
- Update array bounds checking
- Ensure test compatibility

## Backward Compatibility

The implementation maintains full backward compatibility:

1. **Existing Array Syntax**: `array[1..10] of integer` continues to work
2. **Range Operator**: Current `..` usage in arrays is preserved
3. **Test Suite**: All existing tests must pass without modification
4. **API Stability**: No breaking changes to existing interfaces

The refactoring of array bounds to use subrange types is internal and transparent to users.