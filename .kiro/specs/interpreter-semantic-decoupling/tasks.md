# Implementation Plan

- [x] 1. Create ProcedureObject and FunctionObject classes in object.py





  - Add ProcedureObject class with name, formal_params, and block_ast attributes
  - Add FunctionObject class with name, formal_params, return_type, and block_ast attributes
  - Implement helper methods for parameter access (get_param_names, get_param_count)
  - Write unit tests for object creation and method functionality
  - _Requirements: 1.1, 3.1, 3.2, 3.3_

- [ ] 2. Add runtime registries to Interpreter class
  - Add user_procedures and user_functions dictionaries to Interpreter.__init__
  - Initialize empty registries for storing runtime procedure and function objects
  - Ensure case-insensitive lookup by using uppercase keys
  - _Requirements: 1.1, 1.2, 6.3, 6.4_

- [ ] 3. Implement visit_ProcedureDecl to create and register ProcedureObject
  - Modify visit_ProcedureDecl to create ProcedureObject from AST node
  - Register the procedure object in user_procedures registry with uppercase key
  - Store formal parameters, block AST, and procedure name in the object
  - Write tests to verify procedure registration during declaration visits
  - _Requirements: 1.1, 3.1_

- [ ] 4. Implement visit_FunctionDecl to create and register FunctionObject
  - Modify visit_FunctionDecl to create FunctionObject from AST node
  - Register the function object in user_functions registry with uppercase key
  - Store formal parameters, return type, block AST, and function name in the object
  - Write tests to verify function registration during declaration visits
  - _Requirements: 1.2, 3.2_

- [ ] 5. Remove proc_symbol field from ProcedureCall AST node
  - Remove proc_symbol field from ProcedureCall class constructor and attributes
  - Update any references to proc_symbol in the codebase to ensure no compilation errors
  - Verify that parser doesn't attempt to set proc_symbol field
  - _Requirements: 2.1, 2.3_

- [ ] 6. Remove func_symbol field from FunctionCall AST node
  - Remove func_symbol field from FunctionCall class constructor and attributes
  - Update any references to func_symbol in the codebase to ensure no compilation errors
  - Verify that parser doesn't attempt to set func_symbol field
  - _Requirements: 2.2, 2.3_

- [ ] 7. Implement runtime procedure resolution in visit_ProcedureCall
  - Modify visit_ProcedureCall to resolve procedures by name at runtime
  - Check built-in procedures first using existing BUILTIN_PROCEDURES registry
  - Check user-defined procedures using user_procedures registry
  - Raise InterpreterError for unknown procedures
  - _Requirements: 1.3, 2.4, 6.1, 6.3_

- [ ] 8. Implement user-defined procedure execution logic
  - Create activation record for user-defined procedure calls
  - Map actual parameters to formal parameters using ProcedureObject information
  - Execute procedure block AST and manage call stack properly
  - Handle parameter passing and variable scoping correctly
  - _Requirements: 1.3, 3.3_

- [ ] 9. Implement runtime function resolution in visit_FunctionCall
  - Modify visit_FunctionCall to resolve functions by name at runtime
  - Check built-in functions first using existing BUILTIN_FUNCTIONS registry
  - Check user-defined functions using user_functions registry
  - Raise InterpreterError for unknown functions
  - _Requirements: 1.4, 2.4, 6.2, 6.4_

- [ ] 10. Implement user-defined function execution logic
  - Create activation record for user-defined function calls
  - Map actual parameters to formal parameters using FunctionObject information
  - Execute function block AST and capture return value
  - Handle parameter passing, return value assignment, and call stack management
  - _Requirements: 1.4, 3.3_

- [ ] 11. Update error handling for unknown procedures and functions
  - Add appropriate error messages for unknown procedure/function calls
  - Ensure error messages include procedure/function name and location information
  - Use existing InterpreterError structure with appropriate error codes
  - Test error scenarios with invalid procedure/function names
  - _Requirements: 1.3, 1.4_

- [ ] 12. Run and fix existing test suites
  - Execute InterpreterFunctionInvokeTestCase tests and fix any failures
  - Execute InterpreterTestCase tests and fix any failures
  - Ensure all existing functionality is preserved after refactoring
  - Debug and resolve any issues with parameter passing or execution flow
  - _Requirements: 4.1, 4.2_

- [ ] 13. Test with procedure.pas and function.pas files
  - Run `make run file=procedure.pas` and verify "Hello, Pascal" output
  - Run `make run file=function.pas` and verify "Sum: 8" output
  - Debug any issues with file execution and ensure correct behavior
  - Validate that built-in procedures (WriteLn) continue to work correctly
  - _Requirements: 4.3, 4.4_

- [ ] 14. Verify semantic analyzer independence
  - Ensure ProcedureSymbol and FunctionSymbol are only used in SemanticAnalyzer
  - Verify that interpreter doesn't access any Symbol objects from semantic analyzer
  - Test that interpreter can function without semantic analysis symbol information
  - Confirm clear separation between compile-time symbols and runtime objects
  - _Requirements: 5.1, 5.2, 5.3, 5.4_