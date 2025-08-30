# Requirements Document

## Introduction

This feature refactors the parser's type resolution system to remove module loading responsibilities and simplify type handling. The parser should focus on syntax parsing without validating type existence, deferring type validation to later stages in the compilation pipeline. This separation of concerns will improve maintainability and reduce coupling between parsing and module systems.

## Requirements

### Requirement 1

**User Story:** As a developer maintaining the Pascal interpreter, I want the parser to handle unknown type IDs without module validation, so that parsing and module loading are decoupled.

#### Acceptance Criteria

1. WHEN the parser encounters an ID token in type_spec() THEN it SHALL create a type node without validating the type exists
2. WHEN parsing type specifications THEN the parser SHALL NOT check imported modules for type validation
3. WHEN an unknown type ID is encountered THEN the parser SHALL return a generic type node that can be resolved later
4. WHEN parsing completes THEN all type validation SHALL be deferred to semantic analysis or interpretation phases
5. IF a type ID cannot be parsed syntactically THEN the parser SHALL only report syntax errors, not semantic errors

### Requirement 2

**User Story:** As a developer working on the interpreter, I want type validation to occur during semantic analysis or interpretation, so that the parser remains focused on syntax validation only.

#### Acceptance Criteria

1. WHEN the interpreter processes type nodes THEN it SHALL validate type existence against available modules
2. WHEN type resolution fails during interpretation THEN the system SHALL provide clear error messages with context
3. WHEN processing array types THEN the interpreter SHALL resolve element types during execution, not parsing
4. WHEN a type is not found THEN the error SHALL indicate the interpretation phase, not parsing phase
5. IF type resolution fails THEN the system SHALL provide suggestions for available types

### Requirement 3

**User Story:** As a developer extending the type system, I want a clean separation between syntax parsing and semantic validation, so that I can modify type validation logic without affecting parser code.

#### Acceptance Criteria

1. WHEN adding new type validation rules THEN changes SHALL only affect semantic analysis components
2. WHEN the parser creates type nodes THEN they SHALL contain only syntactic information
3. WHEN type nodes are processed later THEN they SHALL contain sufficient information for semantic validation
4. WHEN refactoring type validation THEN the parser code SHALL remain unchanged
5. IF new type categories are added THEN only semantic analysis code needs modification

### Requirement 4

**User Story:** As a developer debugging type-related issues, I want clear error messages that distinguish between syntax and semantic errors, so that I can quickly identify the root cause of problems.

#### Acceptance Criteria

1. WHEN a syntax error occurs in type parsing THEN the error SHALL clearly indicate it's a parsing issue
2. WHEN a semantic error occurs in type validation THEN the error SHALL clearly indicate it's a type resolution issue
3. WHEN type validation fails THEN the error SHALL include the context where validation was attempted
4. WHEN suggesting fixes THEN the system SHALL provide different suggestions for syntax vs semantic errors
5. IF both syntax and semantic errors exist THEN syntax errors SHALL be reported first