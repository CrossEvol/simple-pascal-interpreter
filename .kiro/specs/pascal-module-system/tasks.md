# Implementation Plan

- [x] 1. Add module-related tokens to the lexer








  - Add new token types: UNIT, INTERFACE, IMPLEMENTATION, USES to TokenType enum
  - Update reserved keywords dictionary to include new tokens
  - Write unit tests for new token recognition in test_interpreter.py
  - _Requirements: 1.1, 2.1_

- [x] 2. Create core module data structures




  - Implement Module class with name, file_path, interface_symbols, implementation_symbols
  - Implement Unit class extending Module with interface_ast and implementation_ast
  - Create ModuleRegistry class with loaded_modules dictionary and search_paths
  - Write unit tests for module data structures
  - _Requirements: 3.1, 4.1_

- [x] 3. Extend parser to handle uses clause




  - Add new syntax grammar to parser.py.
  - Add uses_clause() method to parse comma-separated module names
  - Modify program() method to optionally parse uses clause after PROGRAM declaration
  - Add error handling for malformed uses clauses
  - Write unit tests for uses clause parsing
  - _Requirements: 2.1, 2.2_

- [x] 4. Implement unit declaration parsing




  - Add unit_declaration() method to parse "unit UnitName;" syntax
  - Add interface_section() method to parse interface declarations
  - Add implementation_section() method to parse implementation code
  - Modify parser to handle unit files vs program files
  - Write unit tests for unit declaration parsing
  - _Requirements: 1.1, 1.2, 1.5_

- [x] 5. Create module file discovery system




  - Implement find_module_file() method in ModuleRegistry to search for .pas files
  - Add support for searching current directory and ./stdlib directory
  - Create clear error messages for missing module files
  - Write unit tests for module file discovery
  - _Requirements: 2.2, 2.5, 6.4_

- [ ] 6. Implement basic module loading
  - Add load_module() method to ModuleRegistry that parses unit files
  - Create separate lexer/parser instances for each module
  - Store parsed AST in Unit objects (interface_ast, implementation_ast)
  - Write unit tests for module loading
  - _Requirements: 2.2, 2.3_

- [ ] 7. Enhance symbol table for module support
  - Create ModuleSymbolTable class extending SymbolTable
  - Add imported_modules dictionary to track imported symbols
  - Implement import_module_symbols() method to merge interface symbols
  - Write unit tests for module symbol table operations
  - _Requirements: 3.1, 4.2, 4.3_

- [ ] 8. Implement symbol visibility enforcement
  - Add visibility tracking to symbols (interface vs implementation)
  - Modify symbol lookup to respect interface/implementation boundaries
  - Ensure implementation symbols are not accessible from outside module
  - Write unit tests for symbol visibility rules
  - _Requirements: 4.2, 4.3_

- [ ] 9. Add cross-module symbol resolution
  - Implement lookup_with_modules() method for symbol resolution across modules
  - Add support for resolving symbols from imported modules
  - Handle symbol precedence (current scope, then imported modules)
  - Write unit tests for cross-module symbol resolution
  - _Requirements: 3.1, 3.2, 3.3_

- [ ] 10. Implement dependency resolution
  - Add resolve_dependencies() method to determine module load order
  - Create dependency graph tracking for loaded modules
  - Implement topological sort for dependency ordering
  - Write unit tests for dependency resolution
  - _Requirements: 3.4_

- [ ] 11. Add circular dependency detection
  - Implement check_circular_dependencies() method using graph traversal
  - Create clear error messages showing dependency chain
  - Add recovery suggestions for circular dependency errors
  - Write unit tests for circular dependency detection
  - _Requirements: 6.2_

- [ ] 12. Create module-specific error classes
  - Implement ModuleNotFoundError with module name and search paths
  - Implement CircularDependencyError with dependency chain display
  - Implement SymbolNotFoundInModuleError with symbol suggestions
  - Implement InterfaceImplementationMismatchError for signature mismatches
  - Write unit tests for error classes
  - _Requirements: 6.1, 6.2, 6.3, 6.5_

- [ ] 13. Integrate module system with interpreter
  - Modify interpreter to handle module loading during program execution
  - Update visit_Program() to process uses clauses and load modules
  - Ensure proper symbol table setup for modules
  - Write integration tests for interpreter module support
  - _Requirements: 2.3, 2.4, 3.1_

- [ ] 14. Create Map standard library unit
  - Write Map.pas unit file with TMap class definition
  - Implement Put, Get, Remove, Keys, Values methods in interface
  - Provide complete implementation section with internal data structures
  - Write unit tests for Map functionality
  - _Requirements: 5.2_

- [ ] 15. Create Math standard library unit
  - Write Math.pas unit file with mathematical function declarations
  - Implement ADD, MUL, SUB, DIV, MOD functions in interface
  - Provide complete implementation section with function bodies
  - Write unit tests for Math functionality
  - _Requirements: 5.3_

- [ ] 16. Create ArrayUtils standard library unit
  - Write ArrayUtils.pas unit file with array utility function declarations
  - Implement Sort, Find, Copy, Size functions in interface
  - Provide complete implementation section with algorithm implementations
  - Write unit tests for ArrayUtils functionality
  - _Requirements: 5.4_

- [ ] 17. Add comprehensive error handling and reporting
  - Enhance error messages with line numbers and file names for module errors
  - Add symbol suggestion system for undefined symbols in modules
  - Implement helpful error recovery for common module mistakes
  - Write integration tests for error handling scenarios
  - _Requirements: 6.1, 6.3, 6.4, 6.5_

- [ ] 18. Create end-to-end integration tests
  - Write test programs that use multiple modules with dependencies
  - Test transitive dependencies (A uses B, B uses C)
  - Verify correct symbol resolution across module boundaries
  - Test standard library module usage in real programs
  - _Requirements: 2.4, 3.4, 5.1_

- [ ] 19. Add module system documentation and examples
  - Create example Pascal programs demonstrating module usage
  - Write documentation for unit syntax and module system features
  - Provide examples of using Map, Math, and ArrayUtils modules
  - Create troubleshooting guide for common module issues
  - _Requirements: 5.1, 6.4_