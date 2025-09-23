# Requirements Document

## Introduction

This feature implements subrange types, set literals, and the `in` operator for the Pascal interpreter. Subrange types allow defining ranges of values (e.g., 1..10), set literals enable creating temporary sets with elements and ranges (e.g., [1, 3..5, 8]), and the `in` operator checks membership in sets or ranges. This enhancement will also refactor the existing array range implementation to properly use subrange types.

## Requirements

### Requirement 1

**User Story:** As a Pascal developer, I want to define subrange types using the range operator (..), so that I can create constrained integer types for better type safety and documentation.

#### Acceptance Criteria

1. WHEN I write `1..10` THEN the system SHALL parse it as a subrange type with lower bound 1 and upper bound 10
2. WHEN I use a subrange type in variable declarations THEN the system SHALL enforce the range constraints during assignment
3. WHEN I assign a value outside the subrange bounds THEN the system SHALL raise a runtime error
4. WHEN I use subrange types in type declarations THEN the system SHALL store the subrange definition for later use

### Requirement 2

**User Story:** As a Pascal developer, I want to create set literals using square brackets with individual elements and ranges, so that I can define collections of values for membership testing.

#### Acceptance Criteria

1. WHEN I write `[1, 3, 5]` THEN the system SHALL create a set containing the values 1, 3, and 5
2. WHEN I write `[1..5, 8, 10..12]` THEN the system SHALL create a set containing values 1, 2, 3, 4, 5, 8, 10, 11, and 12
3. WHEN I use empty set `[]` THEN the system SHALL create an empty set
4. WHEN I mix individual elements and ranges in a set literal THEN the system SHALL correctly expand all ranges and combine all elements

### Requirement 3

**User Story:** As a Pascal developer, I want to use the `in` operator to check if a value exists in a set or range, so that I can perform membership tests in conditional expressions.

#### Acceptance Criteria

1. WHEN I write `value in [1, 3, 5]` THEN the system SHALL return true if value is 1, 3, or 5, and false otherwise
2. WHEN I write `value in 1..10` THEN the system SHALL return true if value is between 1 and 10 inclusive, and false otherwise
3. WHEN I use the `in` operator in if statements THEN the system SHALL evaluate the membership test and execute the appropriate branch
4. WHEN I use the `in` operator with invalid operands THEN the system SHALL raise an appropriate error

### Requirement 4

**User Story:** As a Pascal developer, I want array declarations to use proper subrange types for index ranges, so that the array implementation is consistent with the subrange type system.

#### Acceptance Criteria

1. WHEN I declare `array[1..10] of integer` THEN the system SHALL use a subrange type for the index bounds
2. WHEN I access an array with an index outside the declared range THEN the system SHALL perform bounds checking using the subrange
3. WHEN I use existing array code THEN the system SHALL continue to work and all existing test cases SHALL pass
4. WHEN I inspect array type information THEN the system SHALL show the proper subrange bounds