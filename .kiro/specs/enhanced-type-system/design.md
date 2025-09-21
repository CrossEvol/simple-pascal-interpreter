# Design Document

## Overview

The enhanced type system will introduce comprehensive TypeSymbol classes for all Pascal data types and implement type operations for stronger semantic checking. This design maintains clear separation between compile-time type symbols (used in SemanticAnalyzer) and runtime objects (used in Interpreter), while adding support for const/var mutability tracking and type-level operations.

## Architecture

### Type Symbol Hierarchy

The enhanced type system will extend the existing Symbol hierarchy with specific TypeSymbol classes:

```
Symbol (base class)
├── TypeSymbol (new abstract base for all type symbols)
│   ├── NeverSymbol (new, replaces None for type safety)
│   ├── PrimitiveTypeSymbol (new abstract base for primitive types)
│   │   ├── IntegerTypeSymbol (new)
│   │   ├── RealTypeSymbol (new)
│   │   ├── BooleanTypeSymbol (new)
│   │   └── CharTypeSymbol (new)
│   ├── StringTypeSymbol (existing, will inherit from TypeSymbol)
│   ├── ArrayTypeSymbol (existing, will inherit from TypeSymbol)
│   ├── EnumTypeSymbol (existing, will inherit from TypeSymbol)
│   ├── RecordTypeSymbol (existing, will inherit from TypeSymbol)
│   ├── ProcedureTypeSymbol (new, for procedure type checking)
│   ├── FunctionTypeSymbol (new, for function type checking)
│   ├── TypeAliasSymbol (new, for handling chained type aliases)
│   └── BuiltinTypeSymbol (existing, will be refactored)
├── VarSymbol (existing, will be enhanced with mutability)
├── ProcedureSymbol (existing)
└── FunctionSymbol (existing)
```

### Type Operation System

TypeSymbol classes will implement arithmetic and comparison operations following Pascal's type compatibility rules:

- **Type Promotion**: INTEGER + REAL → REAL
- **Type Compatibility**: Operations between compatible types
- **Error Handling**: Semantic errors for incompatible operations

### Mutability System

VarSymbol will be enhanced to track mutability:
- `is_mutable: bool` - indicates if variable can be modified
- `is_const: bool` - indicates if variable was declared as const

## Components and Interfaces

### 1. TypeSymbol Base Class

```python
class TypeSymbol(Symbol):
    """Abstract base class for all type symbols"""
    
    def __init__(self, name: str):
        super().__init__(name)
    
    def is_compatible_with(self, other: 'TypeSymbol') -> bool:
        """Check if this type is compatible with another type"""
        pass
    
    def can_assign_from(self, other: 'TypeSymbol') -> bool:
        """Check if a value of other type can be assigned to this type"""
        pass
    
    def get_result_type(self, operation: str, other: 'TypeSymbol') -> 'TypeSymbol':
        """Get the result type of an operation with another type"""
        pass
    
    def resolve_final_type(self) -> 'TypeSymbol':
        """Resolve through type alias chain to get the final concrete type"""
        return self
```

### 2. Primitive Type Symbols

```python
class PrimitiveTypeSymbol(TypeSymbol):
    """Abstract base for primitive types"""
    pass

class IntegerTypeSymbol(PrimitiveTypeSymbol):
    """Type symbol for INTEGER type"""
    
    def __add__(self, other: TypeSymbol) -> TypeSymbol:
        """INTEGER + INTEGER → INTEGER, INTEGER + REAL → REAL"""
        pass
    
    def __sub__(self, other: TypeSymbol) -> TypeSymbol:
        pass
    
    # ... other operations

class RealTypeSymbol(PrimitiveTypeSymbol):
    """Type symbol for REAL type"""
    pass

class BooleanTypeSymbol(PrimitiveTypeSymbol):
    """Type symbol for BOOLEAN type"""
    pass

class CharTypeSymbol(PrimitiveTypeSymbol):
    """Type symbol for CHAR type"""
    pass
```

### 3. Enhanced VarSymbol and Special Symbols

```python
class NeverSymbol(TypeSymbol):
    """Special symbol representing 'never' type (replacement for None)"""
    
    def __init__(self):
        super().__init__("NEVER")
    
    def is_compatible_with(self, other: TypeSymbol) -> bool:
        """Never type is compatible with nothing"""
        return False
    
    def can_assign_from(self, other: TypeSymbol) -> bool:
        """Never type cannot be assigned from anything"""
        return False

# Singleton instance
NEVER_SYMBOL = NeverSymbol()

class VarSymbol(Symbol):
    def __init__(self, name: str, type: Symbol | None, is_mutable: bool = True):
        super().__init__(name, type or NEVER_SYMBOL)
        self.is_mutable = is_mutable
        self.is_initialized = False  # Track if const has been initialized
    
    def can_modify(self) -> bool:
        """Check if this variable can be modified"""
        if not self.is_mutable and self.is_initialized:
            return False
        return self.is_mutable
    
    def mark_initialized(self):
        """Mark const variable as initialized"""
        self.is_initialized = True
    
    @property
    def is_const(self) -> bool:
        """Check if this is a const variable"""
        return not self.is_mutable
```

### 4. Type Alias and Chained Resolution

```python
class TypeAliasSymbol(TypeSymbol):
    """Symbol for type aliases that can chain to other types"""
    
    def __init__(self, name: str, target_type: TypeSymbol):
        super().__init__(name)
        self.target_type = target_type
    
    def resolve_final_type(self) -> TypeSymbol:
        """Resolve through alias chain to get final concrete type"""
        visited = {self.name}
        current = self.target_type
        
        while isinstance(current, TypeAliasSymbol):
            if current.name in visited:
                raise SemanticError("Circular type alias detected")
            visited.add(current.name)
            current = current.target_type
        
        return current
    
    def is_compatible_with(self, other: TypeSymbol) -> bool:
        """Delegate to final resolved type"""
        return self.resolve_final_type().is_compatible_with(other)

class ProcedureTypeSymbol(TypeSymbol):
    """Type symbol for procedure types (for type checking procedure parameters)"""
    
    def __init__(self, name: str, param_types: list[TypeSymbol]):
        super().__init__(name)
        self.param_types = param_types
    
    def is_compatible_with(self, other: TypeSymbol) -> bool:
        """Check if procedure signatures are compatible"""
        if not isinstance(other, ProcedureTypeSymbol):
            return False
        
        if len(self.param_types) != len(other.param_types):
            return False
        
        return all(
            p1.is_compatible_with(p2) 
            for p1, p2 in zip(self.param_types, other.param_types)
        )

class FunctionTypeSymbol(TypeSymbol):
    """Type symbol for function types (for type checking function parameters and return type)"""
    
    def __init__(self, name: str, param_types: list[TypeSymbol], return_type: TypeSymbol):
        super().__init__(name)
        self.param_types = param_types
        self.return_type = return_type
    
    def is_compatible_with(self, other: TypeSymbol) -> bool:
        """Check if function signatures are compatible"""
        if not isinstance(other, FunctionTypeSymbol):
            return False
        
        if len(self.param_types) != len(other.param_types):
            return False
        
        if not self.return_type.is_compatible_with(other.return_type):
            return False
        
        return all(
            p1.is_compatible_with(p2) 
            for p1, p2 in zip(self.param_types, other.param_types)
        )

### 5. Type Registry

```python
class TypeRegistry:
    """Central registry for type symbols and operations"""
    
    def __init__(self):
        self._builtin_types = {}
        self._init_builtin_types()
    
    def _init_builtin_types(self):
        """Initialize built-in type symbols"""
        self._builtin_types['INTEGER'] = IntegerTypeSymbol('INTEGER')
        self._builtin_types['REAL'] = RealTypeSymbol('REAL')
        self._builtin_types['BOOLEAN'] = BooleanTypeSymbol('BOOLEAN')
        self._builtin_types['CHAR'] = CharTypeSymbol('CHAR')
    
    def get_builtin_type(self, name: str) -> TypeSymbol:
        """Get a built-in type symbol by name"""
        return self._builtin_types.get(name.upper())
```

## Data Models

### Type Compatibility Matrix

| Left Type | Right Type | Addition | Subtraction | Multiplication | Division | Comparison |
|-----------|------------|----------|-------------|----------------|----------|------------|
| INTEGER   | INTEGER    | INTEGER  | INTEGER     | INTEGER        | REAL     | BOOLEAN    |
| INTEGER   | REAL       | REAL     | REAL        | REAL           | REAL     | BOOLEAN    |
| REAL      | INTEGER    | REAL     | REAL        | REAL           | REAL     | BOOLEAN    |
| REAL      | REAL       | REAL     | REAL        | REAL           | REAL     | BOOLEAN    |
| BOOLEAN   | BOOLEAN    | ERROR    | ERROR       | ERROR          | ERROR    | BOOLEAN    |
| CHAR      | CHAR       | ERROR    | ERROR       | ERROR          | ERROR    | BOOLEAN    |
| STRING    | STRING     | STRING   | ERROR       | ERROR          | ERROR    | BOOLEAN    |

### Mutability Rules

- Variables declared with `VAR` are mutable (`is_mutable = True`)
- Variables declared with `CONST` are immutable (`is_mutable = False`)
- Const variables can be assigned once during initialization (`is_initialized = False` initially)
- After initialization, const variables cannot be modified (`is_initialized = True`)
- Assignment to initialized const variables raises `SEMANTIC_CONST_ASSIGNMENT_ERROR`
- Loop control variables are temporarily marked as immutable during loop execution
- Mutability is tracked in the VarSymbol itself, not in the symbol table
- `is_const` is a computed property based on `is_mutable` to avoid redundancy

### Type Safety Rules

- `NeverSymbol` replaces `None` for type safety
- `NeverSymbol` is incompatible with all other types
- Variables with `NeverSymbol` type indicate unresolved or error states
- Type operations with `NeverSymbol` always result in semantic errors

### Type Alias Resolution Rules

- Type aliases can chain to other type aliases
- Circular type alias references are detected and raise errors
- Type operations on aliases are delegated to the final resolved type
- Alias resolution is performed during semantic analysis, not at runtime

## Error Handling

### New Error Codes

```python
class ErrorCode(Enum):
    # Existing error codes...
    SEMANTIC_TYPE_MISMATCH = "Type mismatch in operation"
    SEMANTIC_INCOMPATIBLE_TYPES = "Incompatible types for operation"
    SEMANTIC_CONST_ASSIGNMENT = "Cannot assign to const variable"
    SEMANTIC_CONST_NOT_INITIALIZED = "Const variable must be initialized"
    SEMANTIC_INVALID_TYPE_OPERATION = "Invalid operation for type"
    SEMANTIC_CIRCULAR_TYPE_ALIAS = "Circular type alias detected"
    SEMANTIC_PROCEDURE_TYPE_MISMATCH = "Procedure signature mismatch"
    SEMANTIC_FUNCTION_TYPE_MISMATCH = "Function signature mismatch"
```

### Error Reporting

Type errors will include detailed information about:
- Expected type vs actual type
- Operation being performed
- Variable mutability status
- Suggested fixes where applicable

## Testing Strategy

### Unit Tests

1. **Type Symbol Creation Tests**
   - Test creation of all primitive type symbols
   - Test type symbol properties and methods
   - Test type compatibility checks

2. **Type Operation Tests**
   - Test arithmetic operations between compatible types
   - Test error cases for incompatible operations
   - Test type promotion rules

3. **Mutability Tests**
   - Test const variable declaration and validation
   - Test assignment restrictions for const variables
   - Test loop control variable restrictions

4. **Integration Tests**
   - Test semantic analyzer with enhanced type system
   - Test error reporting with type information
   - Test backward compatibility with existing code

### Test Coverage Areas

- All primitive type operations
- Type compatibility matrix validation
- Const/var mutability enforcement
- Error message quality and accuracy
- Performance impact of type operations

## Implementation Phases

### Phase 1: Core Type Symbols
- Implement TypeSymbol base class
- Implement primitive type symbols (Integer, Real, Boolean, Char)
- Update existing type symbols to inherit from TypeSymbol

### Phase 2: Type Operations
- Implement arithmetic operations for type symbols
- Implement comparison operations for type symbols
- Add type compatibility checking methods

### Phase 3: Mutability System
- Enhance VarSymbol with mutability tracking
- Update semantic analyzer to handle const declarations
- Add validation for const assignment attempts

### Phase 4: Integration and Testing
- Integrate type operations into semantic analyzer
- Update error handling and reporting
- Comprehensive testing and validation

## Migration Strategy

### Backward Compatibility
- Existing BuiltinTypeSymbol will be refactored to use new type symbols internally
- All existing tests must continue to pass
- No changes to the Interpreter or Object system
- Gradual migration of semantic analyzer to use type operations

### Performance Considerations
- Type operations will only occur during semantic analysis phase
- Minimal memory overhead for type symbols
- Efficient type compatibility checking algorithms
- Caching of frequently used type operation results