# Design Document

## Overview

This design outlines the decoupling of the Interpreter from the SemanticAnalyzer by creating runtime objects for procedures and functions, and removing symbol dependencies from AST nodes. The key insight is to move procedure and function resolution from compile-time (semantic analysis) to runtime (interpretation), making the interpreter self-sufficient.

## Architecture

### Current Architecture Issues
- `ProcedureCall` and `FunctionCall` AST nodes contain `proc_symbol` and `func_symbol` fields populated by SemanticAnalyzer
- Interpreter depends on these symbols to execute procedure/function calls
- Tight coupling between semantic analysis and interpretation phases

### New Architecture
- Remove symbol fields from `ProcedureCall` and `FunctionCall` AST nodes
- Create `ProcedureObject` and `FunctionObject` runtime objects in the interpreter
- Implement runtime resolution of procedure/function calls by name
- Maintain separate registries for user-defined and built-in procedures/functions

## Components and Interfaces

### 1. New Runtime Objects (in `src/spi/object.py`)

#### ProcedureObject
```python
class ProcedureObject(Object):
    """Runtime object representing a user-defined procedure"""
    
    def __init__(self, name: str, formal_params: list[Param], block_ast: Block):
        super().__init__()
        self.name = name
        self.formal_params = formal_params  # List of Param AST nodes
        self.block_ast = block_ast
        
    def get_param_names(self) -> list[str]:
        """Get list of formal parameter names"""
        
    def get_param_count(self) -> int:
        """Get number of formal parameters"""
```

#### FunctionObject
```python
class FunctionObject(Object):
    """Runtime object representing a user-defined function"""
    
    def __init__(self, name: str, formal_params: list[Param], return_type: Type, block_ast: Block):
        super().__init__()
        self.name = name
        self.formal_params = formal_params  # List of Param AST nodes
        self.return_type = return_type
        self.block_ast = block_ast
        
    def get_param_names(self) -> list[str]:
        """Get list of formal parameter names"""
        
    def get_param_count(self) -> int:
        """Get number of formal parameters"""
```

### 2. Modified AST Nodes (in `src/spi/ast_and_symbol.py`)

#### ProcedureCall (Modified)
```python
class ProcedureCall(Statement):
    def __init__(self, proc_name: str, actual_params: list[Expression], token: Token) -> None:
        self.proc_name = proc_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        # Remove: self.proc_symbol: ProcedureSymbol | None = None
```

#### FunctionCall (Modified)
```python
class FunctionCall(Expression):
    def __init__(self, func_name: str, actual_params: list[Expression], token: Token) -> None:
        self.func_name = func_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        # Remove: self.func_symbol: FunctionSymbol | None = None
```

### 3. Enhanced Interpreter (in `src/spi/interpreter.py`)

#### Runtime Registries
```python
class Interpreter(NodeVisitor):
    def __init__(self, tree: Program) -> None:
        # ... existing initialization ...
        
        # Runtime procedure and function registries
        self.user_procedures: dict[str, ProcedureObject] = {}
        self.user_functions: dict[str, FunctionObject] = {}
```

#### Declaration Visitors
```python
def visit_ProcedureDecl(self, node: ProcedureDecl) -> None:
    """Create and register ProcedureObject for runtime use"""
    proc_obj = ProcedureObject(
        name=node.proc_name,
        formal_params=node.formal_params,
        block_ast=node.block_node
    )
    self.user_procedures[node.proc_name.upper()] = proc_obj

def visit_FunctionDecl(self, node: FunctionDecl) -> None:
    """Create and register FunctionObject for runtime use"""
    func_obj = FunctionObject(
        name=node.func_name,
        formal_params=node.formal_params,
        return_type=node.return_type,
        block_ast=node.block_node
    )
    self.user_functions[node.func_name.upper()] = func_obj
```

#### Call Visitors
```python
def visit_ProcedureCall(self, node: ProcedureCall) -> None:
    """Execute procedure call using runtime resolution"""
    proc_name = node.proc_name.upper()
    
    # Check built-in procedures first
    if proc_name in BUILTIN_PROCEDURES:
        # Handle built-in procedure (existing logic)
        handler = BUILTIN_PROCEDURES[proc_name]
        # ... existing built-in handling ...
    
    # Check user-defined procedures
    elif proc_name in self.user_procedures:
        proc_obj = self.user_procedures[proc_name]
        # Create activation record and execute
        # ... procedure execution logic ...
    
    else:
        raise InterpreterError(f"Unknown procedure: {node.proc_name}")

def visit_FunctionCall(self, node: FunctionCall) -> Object:
    """Execute function call using runtime resolution"""
    func_name = node.func_name.upper()
    
    # Check built-in functions first
    if func_name in BUILTIN_FUNCTIONS:
        # Handle built-in function (existing logic)
        handler = BUILTIN_FUNCTIONS[func_name]
        # ... existing built-in handling ...
    
    # Check user-defined functions
    elif func_name in self.user_functions:
        func_obj = self.user_functions[func_name]
        # Create activation record and execute
        # ... function execution logic ...
    
    else:
        raise InterpreterError(f"Unknown function: {node.func_name}")
```

## Data Models

### Runtime Object Storage
- `user_procedures: dict[str, ProcedureObject]` - Maps uppercase procedure names to ProcedureObject instances
- `user_functions: dict[str, FunctionObject]` - Maps uppercase function names to FunctionObject instances
- Case-insensitive lookup using uppercase keys for Pascal compatibility

### Parameter Handling
- Formal parameters stored as AST `Param` nodes in runtime objects
- Parameter matching done at runtime by iterating through formal/actual parameter lists
- Type checking responsibility moves from semantic analyzer to interpreter (if needed)

### Activation Record Management
- Existing `ActivationRecord` and `CallStack` classes remain unchanged
- Runtime objects provide the necessary information for creating activation records
- Nesting level calculation based on current call stack depth

## Error Handling

### New Error Scenarios
1. **Unknown Procedure/Function**: When a call references a non-existent procedure/function
2. **Parameter Mismatch**: When actual parameters don't match formal parameters (count/type)
3. **Recursive Call Depth**: Stack overflow protection for deep recursion

### Error Messages
- Clear error messages indicating the procedure/function name and location
- Parameter mismatch errors showing expected vs actual parameter counts
- Maintain existing error code structure and InterpreterError usage

## Testing Strategy

### Unit Tests
1. **Object Creation Tests**: Verify ProcedureObject and FunctionObject creation
2. **Registration Tests**: Ensure procedures/functions are properly registered during declaration visits
3. **Resolution Tests**: Verify runtime resolution of procedure/function calls
4. **Parameter Tests**: Test parameter passing and activation record creation

### Integration Tests
1. **Existing Test Compatibility**: All existing tests in `InterpreterFunctionInvokeTestCase` and `InterpreterTestCase` must pass
2. **File Execution Tests**: `procedure.pas` and `function.pas` must execute correctly
3. **Built-in Compatibility**: Built-in procedures/functions continue to work

### Test Cases to Maintain
- Simple procedure calls with parameters
- Function calls with return values
- Nested procedure/function calls
- Variable scoping across procedure/function boundaries
- Built-in procedure/function calls (WRITE, WRITELN, LENGTH, etc.)

## Implementation Phases

### Phase 1: Create Runtime Objects
1. Add `ProcedureObject` and `FunctionObject` classes to `src/spi/object.py`
2. Add runtime registries to `Interpreter` class
3. Implement `visit_ProcedureDecl` and `visit_FunctionDecl` to create and register objects

### Phase 2: Remove Symbol Dependencies
1. Remove `proc_symbol` and `func_symbol` fields from AST nodes
2. Update any code that references these fields
3. Ensure parser doesn't set these fields

### Phase 3: Implement Runtime Resolution
1. Modify `visit_ProcedureCall` to use runtime resolution
2. Modify `visit_FunctionCall` to use runtime resolution
3. Maintain existing built-in procedure/function handling

### Phase 4: Testing and Validation
1. Run existing test suites
2. Test with `procedure.pas` and `function.pas` files
3. Fix any issues and ensure all functionality is preserved

## Design Decisions and Rationales

### Runtime vs Compile-time Resolution
- **Decision**: Move procedure/function resolution from compile-time to runtime
- **Rationale**: Eliminates dependency on semantic analyzer, allows interpreter to be self-sufficient
- **Trade-off**: Slight performance cost for runtime lookup vs compile-time binding

### Object-based Storage
- **Decision**: Create dedicated ProcedureObject and FunctionObject classes
- **Rationale**: Encapsulates all necessary information for execution, follows existing object model
- **Alternative**: Could store raw AST nodes, but objects provide better abstraction

### Case-insensitive Lookup
- **Decision**: Use uppercase keys for procedure/function name lookup
- **Rationale**: Pascal is case-insensitive, maintains compatibility with existing built-in handling
- **Implementation**: Convert all names to uppercase during registration and lookup

### Preserve Built-in Handling
- **Decision**: Keep existing built-in procedure/function registry system
- **Rationale**: Minimizes changes to working code, maintains performance for built-ins
- **Implementation**: Check built-ins first, then user-defined procedures/functions