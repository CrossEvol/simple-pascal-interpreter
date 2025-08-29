# Design Document

## Overview

The Pascal module system will extend the existing interpreter architecture to support unit-based code organization. The design integrates seamlessly with the current lexer-parser-interpreter pipeline while adding new components for module management, symbol resolution, and built-in module support.

## Architecture

### Core Components

1. **Module Registry**: Central registry for managing loaded modules and their dependencies
2. **Unit Parser**: Extended parser functionality for unit syntax (`unit`, `interface`, `implementation`)
3. **Module Symbol Table**: Enhanced symbol table system with module-aware scoping
4. **Standard Library Modules**: External Pascal unit files for Map, Math, and ArrayUtils functionality
5. **Dependency Resolver**: Component for handling module dependencies and circular dependency detection

### Integration Points

The module system integrates with existing components:
- **Lexer**: Add new tokens (`UNIT`, `INTERFACE`, `IMPLEMENTATION`, `USES`)
- **Parser**: Extend grammar to handle unit declarations and uses clauses
- **Symbol Table**: Enhance with module-aware symbol resolution
- **Interpreter**: Add module loading and symbol lookup capabilities

## Components and Interfaces

### 1. Module Registry

```python
class ModuleRegistry:
    def __init__(self):
        self.loaded_modules: Dict[str, Module] = {}
        self.dependency_graph: Dict[str, List[str]] = {}
        self.search_paths: List[str] = [".", "./stdlib"]  # Current dir and standard library
    
    def load_module(self, name: str, file_path: str = None) -> Module
    def find_module_file(self, name: str) -> str
    def get_module(self, name: str) -> Module
    def resolve_dependencies(self, module_name: str) -> List[str]
    def check_circular_dependencies(self, module_name: str) -> bool
```

### 2. Module and Unit Classes

```python
class Module:
    def __init__(self, name: str, file_path: str):
        self.name = name
        self.file_path = file_path
        self.interface_symbols: SymbolTable = SymbolTable()
        self.implementation_symbols: SymbolTable = SymbolTable()
        self.dependencies: List[str] = []
        self.is_loaded = False

class Unit(Module):
    def __init__(self, name: str, file_path: str):
        super().__init__(name, file_path)
        self.interface_ast: Block = None
        self.implementation_ast: Block = None
```

### 3. Enhanced Symbol Table

```python
class ModuleSymbolTable(SymbolTable):
    def __init__(self, module_name: str = None):
        super().__init__()
        self.module_name = module_name
        self.imported_modules: Dict[str, SymbolTable] = {}
    
    def import_module_symbols(self, module_name: str, symbol_table: SymbolTable)
    def lookup_with_modules(self, name: str) -> Symbol
    def resolve_qualified_name(self, module_name: str, symbol_name: str) -> Symbol
```

### 4. Standard Library Units

Standard library units will be implemented as external Pascal files:

```pascal
// Map.pas - Map data structure unit
unit Map;

interface
  type
    TMap = class
      procedure Put(key: string; value: integer);
      function Get(key: string): integer;
      procedure Remove(key: string);
      function Keys(): array of string;
      function Values(): array of integer;
    end;

implementation
  // Implementation details
end.

// Math.pas - Mathematical operations unit  
unit Math;

interface
  function ADD(a, b: integer): integer;
  function MUL(a, b: integer): integer;
  function SUB(a, b: integer): integer;
  function DIV(a, b: integer): integer;
  function MOD(a, b: integer): integer;

implementation
  // Implementation details
end.

// ArrayUtils.pas - Array utility functions unit
unit ArrayUtils;

interface
  procedure Sort(var arr: array of integer);
  function Find(arr: array of integer; value: integer): integer;
  procedure Copy(source: array of integer; var dest: array of integer);
  function Size(arr: array of integer): integer;

implementation
  // Implementation details
end.
```

### 5. Extended Parser

The parser will be extended with new methods:

```python
def unit_declaration(self) -> Unit
def interface_section(self) -> Block
def implementation_section(self) -> Block
def uses_clause(self) -> List[str]
def program_with_uses(self) -> Program
```

## Data Models

### Module Dependency Graph

```python
@dataclass
class DependencyNode:
    module_name: str
    dependencies: List[str]
    dependents: List[str]
    load_order: int
```

### Symbol Resolution Context

```python
@dataclass
class SymbolContext:
    current_module: str
    imported_modules: List[str]
    symbol_visibility: Dict[str, VisibilityLevel]
    
class VisibilityLevel(Enum):
    INTERFACE = "interface"
    IMPLEMENTATION = "implementation"
    PRIVATE = "private"
```

## Error Handling

### Module-Specific Errors

```python
class ModuleError(Exception):
    pass

class ModuleNotFoundError(ModuleError):
    def __init__(self, module_name: str, search_paths: List[str])

class CircularDependencyError(ModuleError):
    def __init__(self, dependency_chain: List[str])

class SymbolNotFoundInModuleError(ModuleError):
    def __init__(self, symbol_name: str, module_name: str, available_symbols: List[str])

class InterfaceImplementationMismatchError(ModuleError):
    def __init__(self, symbol_name: str, interface_sig: str, impl_sig: str)
```

### Error Recovery Strategies

1. **Missing Module**: Suggest similar module names, show search paths
2. **Circular Dependencies**: Display dependency chain, suggest refactoring
3. **Symbol Conflicts**: Show conflicting modules, suggest qualification
4. **Interface Mismatches**: Show expected vs actual signatures

## Testing Strategy

### Unit Tests

1. **Module Registry Tests**
   - Module registration and retrieval
   - Dependency resolution
   - Circular dependency detection

2. **Parser Extension Tests**
   - Unit declaration parsing
   - Uses clause parsing
   - Interface/implementation section parsing

3. **Symbol Resolution Tests**
   - Cross-module symbol lookup
   - Visibility enforcement
   - Symbol conflict resolution

4. **Standard Library Module Tests**
   - Map unit operations (put, get, remove, keys, values)
   - Math unit operations (ADD, MUL, SUB, DIV, MOD)
   - ArrayUtils unit operations (sort, find, copy, size)

### Integration Tests

1. **End-to-End Module Loading**
   - Load program with multiple unit dependencies
   - Verify correct symbol resolution
   - Test transitive dependencies

2. **Error Handling Integration**
   - Missing module scenarios
   - Circular dependency scenarios
   - Symbol conflict scenarios

3. **Standard Library Integration**
   - Use standard library modules in Pascal programs
   - Verify correct function calls and return values
   - Test error conditions

### Test File Structure

```
tests/
├── unit/
│   ├── test_module_registry.py
│   ├── test_unit_parser.py
│   ├── test_symbol_resolution.py
│   └── test_stdlib_modules.py
├── integration/
│   ├── test_module_loading.py
│   ├── test_error_handling.py
│   └── test_stdlib_integration.py
└── fixtures/
    ├── units/
    │   ├── simple_unit.pas
    │   ├── math_unit.pas
    │   └── circular_dep_unit.pas
    └── programs/
        ├── program_with_uses.pas
        └── builtin_usage.pas
```

## Implementation Notes

### File Organization

- Module files will use `.pas` extension
- Units will be searched in current directory and `./stdlib` directory
- Standard library modules (Map, Math, ArrayUtils) will be implemented as Pascal unit files in `./stdlib`

### Performance Considerations

- Lazy loading of modules (load only when needed)
- Symbol table caching to avoid repeated parsing
- Dependency graph optimization for large projects

### Backward Compatibility

- Existing Pascal programs without modules will continue to work
- New module features are additive and don't break existing syntax
- Standard library modules are optional and don't affect core language features