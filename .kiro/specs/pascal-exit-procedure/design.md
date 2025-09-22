# Design Document

## Overview

The Exit procedure implementation will use a control flow exception mechanism to provide clean early termination from Pascal procedures and functions. This approach is more robust than using boolean flags because it properly handles nested control structures and maintains clean separation of concerns.

The design leverages the existing built-in procedure infrastructure while introducing a specialized exception class for Exit control flow. This ensures Exit integrates seamlessly with the current architecture while providing the necessary control flow semantics.

## Architecture

### Control Flow Exception Approach

Instead of using boolean flags (as suggested in the original plan), we will implement Exit using a specialized exception class. This approach provides several advantages:

1. **Clean Control Flow**: Exceptions naturally unwind the call stack to the appropriate level
2. **Nested Structure Support**: Works correctly within loops, conditionals, and nested calls
3. **Maintainable Code**: No need to add exit checks throughout the interpreter
4. **Pascal Semantics**: Matches the expected behavior of Pascal's Exit procedure

### Signal-Based Design

```python
class ExitSignal(Exception):
    """Special signal used for Exit procedure control flow"""
    def __init__(self, exit_type: str = "procedure"):
        self.exit_type = exit_type  # "procedure" or "function"
        super().__init__(f"Exit from {exit_type}")
```

This signal will be:
- Raised by the Exit procedure handler
- Caught at the appropriate procedure/function boundary
- Not propagated beyond the current procedure/function scope

## Components and Interfaces

### 1. Native Method Registration

**Location**: `src/spi/native.py`
- The EXIT enum value is already present in NativeMethod
- No changes needed to the native method enumeration

### 2. Exit Signal Class

**Location**: `src/spi/error.py`
- New `ExitSignal` class for control flow
- Inherits from base Exception (not from Error class to avoid error handling)
- Lightweight design focused on control flow

### 3. Exit Procedure Handler

**Location**: `src/spi/interpreter.py`
- `handle_exit()` function implementation
- Raises ExitException to trigger control flow
- Follows existing built-in procedure patterns
- Supports parameterless Exit() calls

### 4. Signal Handling Integration

**Location**: `src/spi/interpreter.py`
- Modify `visit_ProcedureCall()` to catch ExitSignal
- Modify `visit_FunctionCall()` to catch ExitSignal  
- Ensure signals are only caught at procedure/function boundaries
- Preserve function return values when Exit is called

### 5. Built-in Procedure Registration

**Location**: `src/spi/interpreter.py`
- Register handle_exit with the EXIT native method
- Integration with existing BUILTIN_PROCEDURES registry

## Data Models

### ExitSignal Class

```python
class ExitSignal(Exception):
    """Signal used for Pascal Exit procedure control flow"""
    
    def __init__(self, exit_type: str = "procedure"):
        self.exit_type = exit_type
        super().__init__(f"Exit from {exit_type}")
```

**Properties**:
- `exit_type`: String indicating whether exiting from "procedure" or "function"
- Inherits from Exception for proper signal handling
- Lightweight with minimal overhead

### Handler Function Signature

```python
def handle_exit(interpreter, node: ProcedureCall) -> None:
    """Handle EXIT built-in procedure"""
```

**Parameters**:
- `interpreter`: Current interpreter instance
- `node`: ProcedureCall AST node containing Exit call details

## Error Handling

### Signal Propagation Strategy

1. **Controlled Propagation**: ExitSignal only propagates to the immediate procedure/function boundary
2. **Proper Cleanup**: Call stack operations complete normally before signal handling
3. **Logging Preservation**: Exit calls are logged like other built-in procedures
4. **Error Isolation**: ExitSignal is distinct from error conditions

### Signal Catching Points

1. **User-Defined Procedures**: Caught in `visit_ProcedureCall()` after `self.visit(proc_obj.block_ast)`
2. **User-Defined Functions**: Caught in `visit_FunctionCall()` after `self.visit(func_obj.block_ast)`
3. **Built-in Procedures**: Not applicable (Exit cannot be called from built-in procedures)

### Function Return Value Preservation

When Exit is called in a function:
1. The current function return value (stored in activation record) is preserved
2. ExitSignal is caught at the function boundary
3. The preserved return value is returned normally
4. No special handling needed for return value assignment

## Testing Strategy

### Supported Exit Scenarios

The design supports all required Exit scenarios from the test files:

1. **Exit in Procedures**: `Exit()` in Proc3 terminates only Proc3, allowing Proc2 to continue normally
2. **Exit in Functions**: `Exit()` in Sum3 preserves the assigned return value (3) and terminates Sum3
3. **Exit within If Statements**: `Exit()` works correctly within conditional blocks due to exception unwinding
4. **Nested Procedure Calls**: Exit only affects the immediate procedure/function, not the entire call chain
5. **Function Return Value Preservation**: Function values assigned before Exit are preserved and returned

### Unit Tests

1. **Basic Exit Functionality**
   - Test Exit() in simple procedures
   - Test Exit() in simple functions
   - Verify proper control flow termination

2. **Nested Control Structures**
   - Test Exit() within for loops
   - Test Exit() within while loops  
   - Test Exit() within if statements
   - Test Exit() within nested compound statements

3. **Function Return Value Tests**
   - Verify function return values are preserved when Exit is called
   - Test Exit() called before and after return value assignment
   - Test Exit() in functions with different return types

4. **Call Stack Integration**
   - Test Exit() in nested procedure calls
   - Verify Exit() only affects immediate procedure/function
   - Test proper activation record cleanup

5. **Error Conditions**
   - Verify Exit() with parameters is handled gracefully
   - Test Exit() behavior with malformed calls

### Integration Tests

1. **Existing Test Compatibility**
   - Run existing interpreter tests to ensure no regression
   - Verify other built-in procedures still work correctly

2. **Complex Scenarios**
   - Test Exit() in recursive functions
   - Test Exit() with multiple nested procedure calls
   - Test Exit() combined with other control flow statements

### Test Files

The existing test files `exit_procedure.pas` and `exit_function.pas` provide excellent integration test cases:

- **exit_procedure.pas**: Tests Exit() in nested procedure calls with proper count verification
- **exit_function.pas**: Tests Exit() in nested function calls with return value verification

## Implementation Sequence

### Phase 1: Core Signal Infrastructure
1. Add ExitSignal class to error.py
2. Implement handle_exit() function
3. Register Exit procedure handler

### Phase 2: Signal Handling Integration  
1. Add ExitSignal catching to visit_ProcedureCall()
2. Add ExitSignal catching to visit_FunctionCall()
3. Ensure proper logging and cleanup

### Phase 3: Testing and Validation
1. Run existing test cases
2. Add comprehensive unit tests
3. Validate against Pascal specification behavior

This design provides a clean, maintainable implementation that integrates well with the existing interpreter architecture while providing the correct Pascal Exit procedure semantics.