# Requirements Document

## Introduction

This feature aims to decouple the Interpreter from the SemanticAnalyzer by removing symbol dependencies from AST nodes and creating runtime objects for procedures and functions directly in the interpreter. Currently, the interpreter relies on proc_symbol and func_symbol fields that are populated by the SemanticAnalyzer, creating tight coupling between these components. The goal is to make the interpreter self-sufficient in handling procedure and function calls while maintaining all existing functionality.

## Requirements

### Requirement 1

**User Story:** As a developer working on the Pascal interpreter, I want the Interpreter to be decoupled from the SemanticAnalyzer so that the interpreter can function independently without relying on symbol information populated by semantic analysis.

#### Acceptance Criteria

1. WHEN the interpreter visits a ProcedureDecl node THEN it SHALL create a ProcedureObject that contains all necessary information for procedure calls
2. WHEN the interpreter visits a FunctionDecl node THEN it SHALL create a FunctionObject that contains all necessary information for function calls
3. WHEN the interpreter encounters a ProcedureCall THEN it SHALL resolve and execute the procedure using only information available at runtime without relying on proc_symbol
4. WHEN the interpreter encounters a FunctionCall THEN it SHALL resolve and execute the function using only information available at runtime without relying on func_symbol

### Requirement 2

**User Story:** As a developer maintaining the codebase, I want ProcedureCall and FunctionCall AST nodes to be free of symbol references so that the AST remains independent of semantic analysis results.

#### Acceptance Criteria

1. WHEN a ProcedureCall node is created THEN it SHALL NOT contain a proc_symbol field
2. WHEN a FunctionCall node is created THEN it SHALL NOT contain a func_symbol field
3. WHEN the parser creates procedure or function call nodes THEN they SHALL only contain the procedure/function name and actual parameters
4. WHEN the interpreter processes these calls THEN it SHALL resolve the procedure/function by name at runtime

### Requirement 3

**User Story:** As a developer working with the object system, I want ProcedureObject and FunctionObject classes that encapsulate all runtime information needed for procedure and function execution.

#### Acceptance Criteria

1. WHEN a ProcedureObject is created THEN it SHALL contain the procedure name, formal parameters, and block AST
2. WHEN a FunctionObject is created THEN it SHALL contain the function name, formal parameters, return type, and block AST
3. WHEN these objects are created THEN they SHALL provide methods for parameter matching and execution
4. WHEN the interpreter stores these objects THEN they SHALL be accessible by name for runtime resolution

### Requirement 4

**User Story:** As a developer ensuring code quality, I want all existing tests to continue passing after the decoupling refactoring so that no functionality is lost.

#### Acceptance Criteria

1. WHEN the refactoring is complete THEN all tests in InterpreterFunctionInvokeTestCase SHALL pass
2. WHEN the refactoring is complete THEN all tests in InterpreterTestCase SHALL pass
3. WHEN running `make run file=procedure.pas` THEN it SHALL output "Hello, Pascal"
4. WHEN running `make run file=function.pas` THEN it SHALL output "Sum: 8"

### Requirement 5

**User Story:** As a developer maintaining separation of concerns, I want ProcedureSymbol and FunctionSymbol to only be used within the SemanticAnalyzer so that symbol information doesn't leak into the interpreter.

#### Acceptance Criteria

1. WHEN the SemanticAnalyzer processes procedure declarations THEN it SHALL continue to create and use ProcedureSymbol objects for type checking
2. WHEN the SemanticAnalyzer processes function declarations THEN it SHALL continue to create and use FunctionSymbol objects for type checking
3. WHEN the interpreter runs THEN it SHALL NOT access or depend on any Symbol objects from the semantic analyzer
4. WHEN the refactoring is complete THEN there SHALL be clear separation between compile-time symbols and runtime objects

### Requirement 6

**User Story:** As a developer working on the interpreter, I want the runtime procedure and function resolution to handle both built-in and user-defined procedures/functions correctly.

#### Acceptance Criteria

1. WHEN the interpreter encounters a built-in procedure call THEN it SHALL continue to use the existing BUILTIN_PROCEDURES registry
2. WHEN the interpreter encounters a built-in function call THEN it SHALL continue to use the existing BUILTIN_FUNCTIONS registry
3. WHEN the interpreter encounters a user-defined procedure call THEN it SHALL resolve it using the ProcedureObject created during declaration visit
4. WHEN the interpreter encounters a user-defined function call THEN it SHALL resolve it using the FunctionObject created during declaration visit