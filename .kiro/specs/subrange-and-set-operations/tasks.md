# Implementation Plan

- [x] 1. Add core AST nodes and token support




  - Create SubrangeType, SetLiteral, and InOperator AST node classes
  - Add IN token to TokenType enum and reserved keywords
  - Update lexer to recognize IN keyword
  - _Requirements: 1.1, 2.1, 3.1_

- [x] 2. Implement subrange type parsing





  - [x] 2.1 Add subrange parsing to parser


    - Implement subrange_type parsing method for lower..upper expressions
    - Update type_spec method to handle subrange types
    - Write unit tests for subrange parsing
    - _Requirements: 1.1_

  - [x] 2.2 Update array type parsing to use subranges


    - Modify array_type_spec to create SubrangeType for bounds instead of separate expressions
    - Ensure backward compatibility with existing array syntax
    - Write tests to verify existing array parsing still works
    - _Requirements: 4.1, 4.3_

- [x] 3. Implement set literal parsing





  - [x] 3.1 Add set literal parsing support


    - Implement set_literal parsing method for [elem1, elem2..elem3, ...] syntax
    - Handle empty sets and mixed individual elements with ranges
    - Write unit tests for various set literal formats
    - _Requirements: 2.1, 2.2, 2.3, 2.4_



  - [x] 3.2 Add set element parsing
    - Implement set_element parsing for individual values and ranges within sets
    - Handle both single expressions and range expressions as set elements
    - Write tests for complex set element combinations
    - _Requirements: 2.2, 2.4_

- [x] 4. Implement in operator parsing





  - Add in_expression parsing method for membership testing syntax
  - Update comparison_expr to include IN operator precedence
  - Integrate in operator into expression parsing hierarchy
  - Write unit tests for in operator parsing with various operands
  - _Requirements: 3.1, 3.2, 3.3_

- [ ] 5. Add semantic analysis for new constructs
  - [ ] 5.1 Implement subrange semantic validation
    - Add visit_SubrangeType method to semantic analyzer
    - Validate simple literal bounds (numbers and characters only)
    - Skip complex expression validation (defer to interpreter)
    - Write tests for subrange semantic validation
    - _Requirements: 1.2, 1.4_

  - [ ] 5.2 Implement set literal semantic validation
    - Add visit_SetLiteral method to semantic analyzer
    - Validate type compatibility of all set elements
    - Handle mixed types and provide appropriate error messages
    - Write tests for set type validation scenarios
    - _Requirements: 2.4_

  - [ ] 5.3 Implement in operator semantic validation
    - Add visit_InOperator method to semantic analyzer
    - Validate left operand type compatibility with set element types
    - Ensure right operand is a valid set or subrange
    - Write tests for in operator type validation
    - _Requirements: 3.4_

- [ ] 6. Create runtime objects for subranges and sets
  - [ ] 6.1 Implement SubrangeObject class
    - Create SubrangeObject with lower/upper bounds and contains method
    - Add to_set method for converting subranges to sets when needed
    - Write unit tests for SubrangeObject functionality
    - _Requirements: 1.1, 3.2_

  - [ ] 6.2 Implement SetObject class
    - Create SetObject with elements set and contains method
    - Add methods for set operations and element access
    - Write unit tests for SetObject functionality
    - _Requirements: 2.1, 3.1_

- [ ] 7. Implement interpreter evaluation for new constructs
  - [ ] 7.1 Add subrange type evaluation
    - Implement visit_SubrangeType method in interpreter
    - Evaluate lower and upper bounds at runtime
    - Validate that lower bound ≤ upper bound for complex expressions
    - Create and return SubrangeObject instances
    - Write tests for subrange evaluation and validation
    - _Requirements: 1.1, 1.3_

  - [ ] 7.2 Add set literal evaluation
    - Implement visit_SetLiteral method in interpreter
    - Expand range elements to individual values
    - Combine all elements into a single set
    - Create and return SetObject instances
    - Write tests for set literal evaluation with various element types
    - _Requirements: 2.1, 2.2, 2.3_

  - [ ] 7.3 Add in operator evaluation
    - Implement visit_InOperator method in interpreter
    - Handle membership testing for both sets and subranges
    - Return boolean results for membership tests
    - Write tests for in operator evaluation with different operand types
    - _Requirements: 3.1, 3.2, 3.3_

- [ ] 8. Update array implementation to use subranges
  - [ ] 8.1 Modify ArrayType to use SubrangeType
    - Update ArrayType AST node to store SubrangeType instead of separate bounds
    - Modify array creation logic to work with SubrangeType bounds
    - Ensure array bounds checking uses subrange containment
    - Write tests to verify array functionality with subrange bounds
    - _Requirements: 4.1, 4.2_

  - [ ] 8.2 Update array bounds validation
    - Modify array bounds checking to use SubrangeObject.contains method
    - Update error messages to reference subrange violations
    - Ensure dynamic arrays continue to work properly
    - Write tests for array bounds validation with subranges
    - _Requirements: 4.2_

- [ ] 9. Add comprehensive error handling
  - Add new error codes for subrange and set operations
  - Implement proper error messages for invalid subrange bounds
  - Add error handling for type mismatches in sets and in operations
  - Write tests for all error scenarios
  - _Requirements: 1.3, 2.4, 3.4_

- [ ] 10. Run regression tests and ensure compatibility
  - Execute all existing test suites to ensure no breaking changes
  - Verify that all existing array functionality continues to work
  - Fix any compatibility issues discovered during testing
  - Add integration tests for complex scenarios combining new features
  - _Requirements: 4.3_