# Implementation Plan

- [x] 1. Create UnresolvedType AST node class





  - Add UnresolvedType class to spi_ast.py extending Type base class
  - Include type_name field and optional resolved_type field for caching
  - Write unit tests for UnresolvedType node creation and properties
  - _Requirements: 1.3, 3.3_

- [x] 2. Add TypeResolutionError exception class





  - Create TypeResolutionError class in error.py with suggestions field
  - Add clear error message formatting with type suggestions
  - Write unit tests for error creation and message formatting
  - _Requirements: 2.2, 4.2, 4.3_

- [x] 3. Refactor parser type_spec method to remove module checking





  - Simplify type_spec() method to return UnresolvedType for ID tokens
  - Remove _is_type_in_imported_modules() method and related module checking code
  - Remove module_registry dependency from Parser constructor
  - Update parser to only handle syntax validation for type specifications
  - Write unit tests for simplified type_spec parsing behavior
  - _Requirements: 1.1, 1.2, 1.3, 1.4_

- [ ] 4. Create TypeResolver service class
  - Implement TypeResolver class with resolve_type() method
  - Add support for resolving types from local scope (classes, enums, records)
  - Implement module-based type resolution using module registry
  - Add type suggestion system for unknown types using fuzzy matching
  - Write unit tests for type resolution scenarios and error cases
  - _Requirements: 2.1, 2.2, 3.1, 3.2, 4.4_

- [ ] 5. Add UnresolvedType visitor method to interpreter
  - Implement visit_UnresolvedType() method in interpreter
  - Integrate TypeResolver to resolve types during interpretation
  - Add caching mechanism to avoid repeated resolution of same types
  - Write unit tests for unresolved type handling in interpreter
  - _Requirements: 2.1, 2.3, 3.3_

- [ ] 6. Update array initialization to handle unresolved element types
  - Modify __initArray() method to resolve UnresolvedType element types
  - Ensure proper type resolution before array element initialization
  - Add error handling for type resolution failures in array context
  - Write unit tests for array initialization with various element types
  - _Requirements: 2.3, 2.4_

- [ ] 7. Update variable declaration handling for unresolved types
  - Modify visit_VarDecl() to resolve UnresolvedType nodes
  - Ensure proper type validation during variable declaration processing
  - Add context information for better error messages
  - Write unit tests for variable declarations with unresolved types
  - _Requirements: 2.1, 2.4, 4.3_

- [ ] 8. Remove module loading code from parser
  - Delete _load_and_analyze_module() method from parser
  - Remove imported_modules tracking from parser (move to interpreter)
  - Clean up unused imports and module registry references
  - Update parser constructor to remove module_registry parameter
  - Write unit tests to verify parser no longer loads modules
  - _Requirements: 1.2, 1.4, 3.4_

- [ ] 9. Enhance error messages for type resolution failures
  - Implement clear distinction between syntax and semantic errors
  - Add contextual information to type resolution error messages
  - Include line numbers and column information in error reports
  - Provide helpful suggestions for similar type names
  - Write unit tests for error message formatting and content
  - _Requirements: 4.1, 4.2, 4.3, 4.4, 4.5_

- [ ] 10. Update interpreter initialization for type resolution
  - Add TypeResolver instance to interpreter constructor
  - Pass module registry and context information to type resolver
  - Ensure proper initialization of type resolution context
  - Write unit tests for interpreter setup with type resolution
  - _Requirements: 2.1, 3.1, 3.2_

- [ ] 11. Add comprehensive integration tests
  - Write end-to-end tests for parsing and type resolution workflow
  - Test programs with various type references (local and imported)
  - Verify error handling for unknown types with helpful messages
  - Test performance improvements from removing parser module loading
  - _Requirements: 1.5, 2.5, 3.4, 4.5_

- [ ] 12. Update existing tests for parser changes
  - Modify existing parser tests to expect UnresolvedType nodes for ID tokens
  - Update interpreter tests to handle new type resolution workflow
  - Fix any broken tests due to parser refactoring changes
  - Ensure all existing functionality continues to work correctly
  - _Requirements: 1.1, 1.3, 2.1_