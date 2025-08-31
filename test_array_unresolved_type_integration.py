#!/usr/bin/env python3

"""
Unit tests for array initialization with unresolved element types.
Tests the enhanced __initArray method functionality.
"""

import unittest
from unittest.mock import patch, MagicMock

from src.interpreter import Interpreter, ActivationRecord, ARType
from src.spi_ast import *
from src.spi_token import Token, TokenType, ElementType
from src.object import *
from src.error import TypeResolutionError
from src.type_resolver import TypeResolver, TypeResolutionContext


class ArrayUnresolvedTypeIntegrationTestCase(unittest.TestCase):
    """Test cases for array initialization with unresolved element types."""

    def setUp(self):
        """Set up test fixtures."""
        # Create a minimal program AST
        program_token = Token(TokenType.PROGRAM, "TestProgram", 1, 1)
        program_name = "TestProgram"
        block = Block([], Compound.of([]))
        self.program = Program(program_name, block, program_token)

        # Create interpreter
        self.interpreter = Interpreter(self.program)

        # Create activation record
        self.ar = ActivationRecord("TestProgram", ARType.PROGRAM, 1)
        self.interpreter.call_stack.push(self.ar)

    def test_array_init_with_unresolved_primitive_type(self):
        """Test array initialization with unresolved primitive element type."""
        # Create array type with unresolved Integer element type
        lower_token = Token(TokenType.INTEGER_CONST, 1, 1, 1)
        upper_token = Token(TokenType.INTEGER_CONST, 3, 1, 1)
        element_type_token = Token(TokenType.ID, "Integer", 1, 1)
        unresolved_element_type = UnresolvedType(element_type_token, "Integer")

        lower_bound = Num(lower_token)
        upper_bound = Num(upper_token)

        array_type = ArrayType(
            token=Token(TokenType.ARRAY, "array", 1, 1),
            lower=lower_bound,
            upper=upper_bound,
            element_type=unresolved_element_type,
            dynamic=False,
        )

        # Mock the type resolution to return Integer primitive type
        with patch.object(self.interpreter, "visit_UnresolvedType") as mock_visit:
            mock_visit.return_value = PrimitiveType(
                Token(TokenType.INTEGER, "Integer", 1, 1)
            )

            # Initialize the array
            elements, element_type = self.interpreter._Interpreter__initArray(
                array_type
            )

            # Verify results
            self.assertEqual(element_type, ElementType.INTEGER)
            self.assertEqual(len(elements), 3)  # Array from 1 to 3

            # Check that all elements are IntegerObjects with default value 0
            for i in range(1, 4):
                self.assertIn(i, elements)
                self.assertIsInstance(elements[i], IntegerObject)
                self.assertEqual(elements[i].value, 0)

    def test_array_init_with_unresolved_string_type(self):
        """Test array initialization with unresolved string element type."""
        # Create array type with unresolved String element type
        lower_token = Token(TokenType.INTEGER_CONST, 0, 1, 1)
        upper_token = Token(TokenType.INTEGER_CONST, 2, 1, 1)
        element_type_token = Token(TokenType.ID, "String", 1, 1)
        unresolved_element_type = UnresolvedType(element_type_token, "String")

        lower_bound = Num(lower_token)
        upper_bound = Num(upper_token)

        array_type = ArrayType(
            token=Token(TokenType.ARRAY, "array", 1, 1),
            lower=lower_bound,
            upper=upper_bound,
            element_type=unresolved_element_type,
            dynamic=False,
        )

        # Mock the type resolution to return String type
        with patch.object(self.interpreter, "visit_UnresolvedType") as mock_visit:
            string_limit = Num(Token(TokenType.INTEGER_CONST, 255, 1, 1))
            mock_visit.return_value = StringType(
                Token(TokenType.STRING, "String", 1, 1), string_limit
            )

            # Initialize the array
            elements, element_type = self.interpreter._Interpreter__initArray(
                array_type
            )

            # Verify results
            self.assertEqual(element_type, ElementType.STRING)
            self.assertEqual(len(elements), 3)  # Array from 0 to 2

            # Check that all elements are StringObjects with empty default value
            for i in range(0, 3):
                self.assertIn(i, elements)
                self.assertIsInstance(elements[i], StringObject)
                self.assertEqual(elements[i].value, "")

    def test_array_init_with_unresolved_class_type(self):
        """Test array initialization with unresolved class element type."""
        # Set up a class declaration in the activation record
        class_decl = ClassDecl("TMyClass", [], [], None, None)
        # Store using the transformed class name that SpiUtil.toClassName would generate
        self.ar["Class[MyClass]"] = class_decl

        # Create array type with unresolved class element type
        lower_token = Token(TokenType.INTEGER_CONST, 1, 1, 1)
        upper_token = Token(TokenType.INTEGER_CONST, 2, 1, 1)
        element_type_token = Token(TokenType.ID, "MyClass", 1, 1)
        unresolved_element_type = UnresolvedType(element_type_token, "MyClass")

        lower_bound = Num(lower_token)
        upper_bound = Num(upper_token)

        array_type = ArrayType(
            token=Token(TokenType.ARRAY, "array", 1, 1),
            lower=lower_bound,
            upper=upper_bound,
            element_type=unresolved_element_type,
            dynamic=False,
        )

        # Mock the type resolution to return Class type
        with patch.object(self.interpreter, "visit_UnresolvedType") as mock_visit:
            mock_visit.return_value = ClassType(Token(TokenType.CLASS, "MyClass", 1, 1))

            # Initialize the array
            elements, element_type = self.interpreter._Interpreter__initArray(
                array_type
            )

            # Verify results
            self.assertEqual(element_type, ElementType.INSTANCE)
            self.assertEqual(len(elements), 2)  # Array from 1 to 2

            # Check that all elements are InstanceObjects
            for i in range(1, 3):
                self.assertIn(i, elements)
                self.assertIsInstance(elements[i], InstanceObject)
                self.assertEqual(elements[i].class_name, "TMyClass")

    def test_array_init_with_unresolved_record_type(self):
        """Test array initialization with unresolved record element type."""
        # Set up a record declaration in the activation record
        record_class = RecordClassObject("MyRecord", {})
        self.ar["MyRecord"] = record_class

        # Create array type with unresolved record element type
        lower_token = Token(TokenType.INTEGER_CONST, 1, 1, 1)
        upper_token = Token(TokenType.INTEGER_CONST, 2, 1, 1)
        element_type_token = Token(TokenType.ID, "MyRecord", 1, 1)
        unresolved_element_type = UnresolvedType(element_type_token, "MyRecord")

        lower_bound = Num(lower_token)
        upper_bound = Num(upper_token)

        array_type = ArrayType(
            token=Token(TokenType.ARRAY, "array", 1, 1),
            lower=lower_bound,
            upper=upper_bound,
            element_type=unresolved_element_type,
            dynamic=False,
        )

        # Mock the type resolution to return Record type
        with patch.object(self.interpreter, "visit_UnresolvedType") as mock_visit:
            mock_visit.return_value = RecordType(
                Token(TokenType.RECORD, "MyRecord", 1, 1)
            )

            # Initialize the array
            elements, element_type = self.interpreter._Interpreter__initArray(
                array_type
            )

            # Verify results
            self.assertEqual(element_type, ElementType.RECORD_CLASS)
            self.assertEqual(len(elements), 2)  # Array from 1 to 2

            # Check that all elements are RecordInstanceObjects
            for i in range(1, 3):
                self.assertIn(i, elements)
                self.assertIsInstance(elements[i], RecordInstanceObject)
                self.assertEqual(elements[i].record_name, "MyRecord")

    def test_array_init_with_unresolved_enum_type(self):
        """Test array initialization with unresolved enum element type."""
        # Set up an enum declaration in the activation record
        enum_decl = EnumDecl("MyEnum", {0: "VALUE1", 1: "VALUE2"})
        self.ar["MyEnum"] = enum_decl

        # Create array type with unresolved enum element type
        lower_token = Token(TokenType.INTEGER_CONST, 1, 1, 1)
        upper_token = Token(TokenType.INTEGER_CONST, 2, 1, 1)
        element_type_token = Token(TokenType.ID, "MyEnum", 1, 1)
        unresolved_element_type = UnresolvedType(element_type_token, "MyEnum")

        lower_bound = Num(lower_token)
        upper_bound = Num(upper_token)

        array_type = ArrayType(
            token=Token(TokenType.ARRAY, "array", 1, 1),
            lower=lower_bound,
            upper=upper_bound,
            element_type=unresolved_element_type,
            dynamic=False,
        )

        # Mock the type resolution to return Enum type
        with patch.object(self.interpreter, "visit_UnresolvedType") as mock_visit:
            mock_visit.return_value = EnumType(Token(TokenType.ENUM, "MyEnum", 1, 1))

            # Initialize the array
            elements, element_type = self.interpreter._Interpreter__initArray(
                array_type
            )

            # Verify results
            self.assertEqual(element_type, ElementType.ENUM)
            self.assertEqual(len(elements), 2)  # Array from 1 to 2

            # Check that all elements are EnumObjects
            for i in range(1, 3):
                self.assertIn(i, elements)
                self.assertIsInstance(elements[i], EnumObject)
                self.assertEqual(elements[i].name, "VALUE1")  # First enum value
                self.assertEqual(elements[i].index, 0)  # First enum index

    def test_array_init_with_nested_unresolved_array_type(self):
        """Test array initialization with nested unresolved array element type."""
        # Create nested array: array[1..2] of array[1..3] of Integer
        inner_element_type_token = Token(TokenType.ID, "Integer", 1, 1)
        inner_unresolved_element_type = UnresolvedType(
            inner_element_type_token, "Integer"
        )

        inner_lower_bound = Num(Token(TokenType.INTEGER_CONST, 1, 1, 1))
        inner_upper_bound = Num(Token(TokenType.INTEGER_CONST, 3, 1, 1))

        inner_array_type = ArrayType(
            token=Token(TokenType.ARRAY, "array", 1, 1),
            lower=inner_lower_bound,
            upper=inner_upper_bound,
            element_type=inner_unresolved_element_type,
            dynamic=False,
        )

        outer_lower_bound = Num(Token(TokenType.INTEGER_CONST, 1, 1, 1))
        outer_upper_bound = Num(Token(TokenType.INTEGER_CONST, 2, 1, 1))

        outer_array_type = ArrayType(
            token=Token(TokenType.ARRAY, "array", 1, 1),
            lower=outer_lower_bound,
            upper=outer_upper_bound,
            element_type=inner_array_type,
            dynamic=False,
        )

        # Mock the type resolution for the inner array's element type
        with patch.object(self.interpreter, "visit_UnresolvedType") as mock_visit:
            mock_visit.return_value = PrimitiveType(
                Token(TokenType.INTEGER, "Integer", 1, 1)
            )

            # Initialize the outer array
            elements, element_type = self.interpreter._Interpreter__initArray(
                outer_array_type
            )

            # Verify results
            self.assertEqual(element_type, ElementType.ARRAY)
            self.assertEqual(len(elements), 2)  # Outer array from 1 to 2

            # Check that all elements are ArrayObjects
            for i in range(1, 3):
                self.assertIn(i, elements)
                self.assertIsInstance(elements[i], ArrayObject)
                self.assertEqual(elements[i].element_type, ElementType.INTEGER)
                self.assertEqual(
                    len(elements[i].elements), 3
                )  # Inner array from 1 to 3

    def test_array_init_type_resolution_error(self):
        """Test array initialization with type resolution failure."""
        # Create array type with unresolved element type that will fail to resolve
        lower_token = Token(TokenType.INTEGER_CONST, 1, 1, 1)
        upper_token = Token(TokenType.INTEGER_CONST, 3, 1, 1)
        element_type_token = Token(TokenType.ID, "UnknownType", 1, 5)
        unresolved_element_type = UnresolvedType(element_type_token, "UnknownType")

        lower_bound = Num(lower_token)
        upper_bound = Num(upper_token)

        array_type = ArrayType(
            token=Token(TokenType.ARRAY, "array", 1, 1),
            lower=lower_bound,
            upper=upper_bound,
            element_type=unresolved_element_type,
            dynamic=False,
        )

        # Mock the type resolution to raise TypeResolutionError
        with patch.object(self.interpreter, "visit_UnresolvedType") as mock_visit:
            original_error = TypeResolutionError(
                "Unknown type 'UnknownType'",
                suggestions=["Integer", "Real", "Boolean"],
                token=element_type_token,
            )
            mock_visit.side_effect = original_error

            # Attempt to initialize the array and expect enhanced error
            with self.assertRaises(TypeResolutionError) as context:
                self.interpreter._Interpreter__initArray(array_type)

            # Verify the error message includes array context
            error_message = str(context.exception)
            self.assertIn(
                "Failed to resolve array element type 'UnknownType'", error_message
            )
            self.assertIn("line 1", error_message)

    def test_array_init_missing_class_error(self):
        """Test array initialization with missing class declaration."""
        # Create array type with unresolved class element type (but no class in AR)
        lower_token = Token(TokenType.INTEGER_CONST, 1, 1, 1)
        upper_token = Token(TokenType.INTEGER_CONST, 2, 1, 1)
        element_type_token = Token(TokenType.ID, "MissingClass", 1, 1)
        unresolved_element_type = UnresolvedType(element_type_token, "MissingClass")

        lower_bound = Num(lower_token)
        upper_bound = Num(upper_token)

        array_type = ArrayType(
            token=Token(TokenType.ARRAY, "array", 1, 1),
            lower=lower_bound,
            upper=upper_bound,
            element_type=unresolved_element_type,
            dynamic=False,
        )

        # Mock the type resolution to return Class type (but class not in AR)
        with patch.object(self.interpreter, "visit_UnresolvedType") as mock_visit:
            mock_visit.return_value = ClassType(
                Token(TokenType.CLASS, "MissingClass", 1, 1)
            )

            # Attempt to initialize the array and expect error
            with self.assertRaises(TypeResolutionError) as context:
                self.interpreter._Interpreter__initArray(array_type)

            # Verify the error message
            error_message = str(context.exception)
            self.assertIn(
                "Class 'MissingClass' not found for array element initialization",
                error_message,
            )

    def test_array_init_dynamic_array(self):
        """Test dynamic array initialization with unresolved element type."""
        # Create dynamic array type with unresolved element type
        lower_token = Token(TokenType.INTEGER_CONST, 1, 1, 1)
        upper_token = Token(TokenType.INTEGER_CONST, 5, 1, 1)
        element_type_token = Token(TokenType.ID, "Integer", 1, 1)
        unresolved_element_type = UnresolvedType(element_type_token, "Integer")

        lower_bound = Num(lower_token)
        upper_bound = Num(upper_token)

        array_type = ArrayType(
            token=Token(TokenType.ARRAY, "array", 1, 1),
            lower=lower_bound,
            upper=upper_bound,
            element_type=unresolved_element_type,
            dynamic=True,  # Dynamic array
        )

        # Mock the type resolution to return Integer primitive type
        with patch.object(self.interpreter, "visit_UnresolvedType") as mock_visit:
            mock_visit.return_value = PrimitiveType(
                Token(TokenType.INTEGER, "Integer", 1, 1)
            )

            # Initialize the array
            elements, element_type = self.interpreter._Interpreter__initArray(
                array_type
            )

            # Verify results - dynamic arrays should have empty elements dict
            self.assertEqual(element_type, ElementType.INTEGER)
            self.assertEqual(len(elements), 0)  # Dynamic array starts empty

    def test_array_init_unsupported_resolved_type(self):
        """Test array initialization with unsupported resolved type."""
        # Create array type with unresolved element type
        lower_token = Token(TokenType.INTEGER_CONST, 1, 1, 1)
        upper_token = Token(TokenType.INTEGER_CONST, 2, 1, 1)
        element_type_token = Token(TokenType.ID, "CustomType", 1, 1)
        unresolved_element_type = UnresolvedType(element_type_token, "CustomType")

        lower_bound = Num(lower_token)
        upper_bound = Num(upper_token)

        array_type = ArrayType(
            token=Token(TokenType.ARRAY, "array", 1, 1),
            lower=lower_bound,
            upper=upper_bound,
            element_type=unresolved_element_type,
            dynamic=False,
        )

        # Mock the type resolution to return an unsupported type
        class UnsupportedType(Type):
            def __init__(self, token):
                super().__init__(token)

        with patch.object(self.interpreter, "visit_UnresolvedType") as mock_visit:
            mock_visit.return_value = UnsupportedType(
                Token(TokenType.ID, "CustomType", 1, 1)
            )

            # Attempt to initialize the array and expect error
            with self.assertRaises(TypeResolutionError) as context:
                self.interpreter._Interpreter__initArray(array_type)

            # Verify the error message
            error_message = str(context.exception)
            self.assertIn(
                "Unsupported array element type 'UnsupportedType'", error_message
            )
            self.assertIn("for type 'CustomType'", error_message)


if __name__ == "__main__":
    unittest.main()
