# Design Document

## Overview

This design refactors the parser's type resolution system to eliminate module loading responsibilities and create a cleaner separation of concerns. The parser will focus solely on syntactic parsing, while type validation and resolution will be moved to the semantic analysis and interpretation phases. This approach improves maintainability, reduces coupling, and makes the system more modular.

## Architecture

### Current Architecture Issues

The current implementation has several problems:
1. **Tight Coupling**: Parser directly accesses module registry and performs type validation
2. **Mixed Responsibilities**: Parser handles both syntax parsing and semantic validation
3. **Complex Error Handling**: Type errors are mixed with syntax errors
4. **Performance Issues**: Module loading during parsing can be expensive

### New Architecture

The refactored architecture will have three distinct phases:
1. **Parsing Phase**: Pure syntax parsing, creates AST nodes without validation
2. **Semantic Analysis Phase**: Type validation and resolution (future enhancement)
3. **Interpretation Phase**: Runtime type checking and error reporting

## Components and Interfaces

### 1. Enhanced Type AST Nodes

Create a new generic type node for unresolved types:

```python
class UnresolvedType(Type):
    """
    Represents a type that needs to be resolved during semantic analysis.
    Used when parser encounters an ID that could be a type name.
    """
    def __init__(self, token: Token, type_name: str):
        super().__init__(token)
        self.type_name = type_name
        self.resolved_type: Type | None = None  # Will be set during semantic analysis
```

### 2. Simplified Parser Type Resolution

The parser's `type_spec()` method will be simplified:

```python
def type_spec(self) -> Type:
    """
    Simplified type_spec that focuses on syntax parsing only.
    Type validation is deferred to semantic analysis/interpretation.
    """
    if self.current_token.type in (TokenType.INTEGER, TokenType.REAL, TokenType.BOOLEAN):
        return self.primitive_type_spec()
    elif self.current_token.type == TokenType.STRING:
        return self.string_type_spec()
    elif self.current_token.type == TokenType.ARRAY:
        return self.array_type_spec()
    elif self.current_token.type == TokenType.ID:
        # Create unresolved type node - validation happens later
        type_name = self.current_token.value
        token = self.current_token
        self.eat(TokenType.ID)
        return UnresolvedType(token, type_name)
    else:
        raise SyntaxError(f"Expected type specification, got {self.current_token.type}")
```

### 3. Type Resolution Service

Create a dedicated service for type resolution during interpretation:

```python
class TypeResolver:
    """
    Handles type resolution during semantic analysis and interpretation.
    """
    def __init__(self, module_registry: ModuleRegistry = None):
        self.module_registry = module_registry
        self.local_types = {
            'classes': [],
            'enums': [], 
            'records': []
        }
    
    def resolve_type(self, unresolved_type: UnresolvedType, context: dict) -> Type:
        """
        Resolve an unresolved type to a concrete type.
        
        Args:
            unresolved_type: The unresolved type node from parsing
            context: Current resolution context (imported modules, local types, etc.)
            
        Returns:
            Concrete type node (ClassType, EnumType, RecordType)
            
        Raises:
            TypeResolutionError: If type cannot be resolved
        """
        type_name = unresolved_type.type_name
        
        # Check local types first
        if type_name in context.get('classes', []):
            return ClassType(unresolved_type.token)
        elif type_name in context.get('enums', []):
            return EnumType(unresolved_type.token)
        elif type_name in context.get('records', []):
            return RecordType(unresolved_type.token)
        
        # Check imported modules
        if self.module_registry:
            resolved_type = self._resolve_from_modules(type_name, context.get('imported_modules', []))
            if resolved_type:
                return resolved_type
        
        # Type not found
        raise TypeResolutionError(
            f"Unknown type '{type_name}' at line {unresolved_type.token.lineno}",
            suggestions=self._get_type_suggestions(type_name, context)
        )
    
    def _resolve_from_modules(self, type_name: str, imported_modules: list[str]) -> Type | None:
        """Resolve type from imported modules."""
        # Implementation similar to current _is_type_in_imported_modules
        # but returns concrete Type objects
        pass
    
    def _get_type_suggestions(self, type_name: str, context: dict) -> list[str]:
        """Provide suggestions for similar type names."""
        # Implementation for fuzzy matching available types
        pass
```

### 4. Enhanced Interpreter Integration

Update the interpreter to handle type resolution:

```python
class Interpreter:
    def __init__(self, tree: AST, module_registry: ModuleRegistry = None):
        # ... existing initialization
        self.type_resolver = TypeResolver(module_registry)
    
    def visit_UnresolvedType(self, node: UnresolvedType) -> Type:
        """
        Resolve unresolved types during interpretation.
        """
        if node.resolved_type is None:
            context = {
                'classes': self.classes,
                'enums': self.enums,
                'records': self.records,
                'imported_modules': self.imported_modules
            }
            node.resolved_type = self.type_resolver.resolve_type(node, context)
        
        return node.resolved_type
    
    def __initArray(self, node: Type) -> tuple[dict[Any, Any], ElementType]:
        """
        Enhanced array initialization with proper type resolution.
        """
        if isinstance(node, ArrayType):
            # Resolve element type if it's unresolved
            element_type = node.element_type
            if isinstance(element_type, UnresolvedType):
                element_type = self.visit_UnresolvedType(element_type)
            
            # Continue with existing logic using resolved type
            # ... rest of implementation
```

## Data Models

### Type Resolution Context

```python
@dataclass
class TypeResolutionContext:
    """Context information for type resolution."""
    current_module: str | None
    imported_modules: list[str]
    local_classes: list[str]
    local_enums: list[str]
    local_records: list[str]
    module_registry: ModuleRegistry | None
```

### Type Resolution Result

```python
@dataclass
class TypeResolutionResult:
    """Result of type resolution attempt."""
    success: bool
    resolved_type: Type | None
    error_message: str | None
    suggestions: list[str]
```

## Error Handling

### New Error Classes

```python
class TypeResolutionError(Exception):
    """Raised when type resolution fails during semantic analysis."""
    def __init__(self, message: str, suggestions: list[str] = None):
        super().__init__(message)
        self.suggestions = suggestions or []

class SyntaxError(Exception):
    """Raised for pure syntax errors during parsing."""
    pass
```

### Error Reporting Strategy

1. **Parsing Errors**: Only report syntax issues (malformed type syntax)
2. **Type Resolution Errors**: Report during interpretation with context
3. **Clear Error Messages**: Distinguish between syntax and semantic errors
4. **Helpful Suggestions**: Provide available types when resolution fails

### Error Message Examples

```
# Syntax Error (during parsing)
SyntaxError: Expected type specification at line 15, column 8

# Type Resolution Error (during interpretation)  
TypeResolutionError: Unknown type 'MyClss' at line 15, column 8
  Did you mean: MyClass, MyRecord?
  Available types: Integer, Real, Boolean, MyClass, MyRecord
  Imported from modules: Map.TMap, Math.Number
```

## Testing Strategy

### Unit Tests

1. **Parser Tests**
   - Test parsing of ID tokens as UnresolvedType nodes
   - Verify no module loading during parsing
   - Test syntax error handling for malformed types

2. **Type Resolver Tests**
   - Test resolution of local types (classes, enums, records)
   - Test resolution from imported modules
   - Test error handling and suggestions

3. **Interpreter Integration Tests**
   - Test array initialization with unresolved element types
   - Test type resolution during variable declarations
   - Test error reporting during interpretation

### Integration Tests

1. **End-to-End Type Resolution**
   - Parse program with various type references
   - Verify correct resolution during interpretation
   - Test error scenarios with helpful messages

2. **Module Integration**
   - Test type resolution with imported modules
   - Verify proper error handling for missing modules
   - Test transitive type dependencies

### Performance Tests

1. **Parser Performance**
   - Verify parsing speed improvement without module loading
   - Test memory usage reduction
   - Compare before/after parsing times

2. **Type Resolution Performance**
   - Test resolution speed for large type hierarchies
   - Verify caching effectiveness
   - Test with multiple imported modules

## Implementation Notes

### Migration Strategy

1. **Phase 1**: Add UnresolvedType class and update parser
2. **Phase 2**: Implement TypeResolver service
3. **Phase 3**: Update interpreter to use type resolution
4. **Phase 4**: Remove old module checking code from parser
5. **Phase 5**: Update error handling and messages

### Backward Compatibility

- Existing AST structure remains compatible
- Parser interface unchanged (still returns Type nodes)
- Interpreter behavior unchanged from user perspective
- Error messages improved but maintain essential information

### Performance Considerations

- Lazy type resolution (only when needed)
- Caching of resolved types to avoid repeated resolution
- Minimal memory overhead for UnresolvedType nodes
- Faster parsing due to eliminated module loading