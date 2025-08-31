"""
Unit tests for variable declaration handling with unresolved types.

Tests the enhanced visit_VarDecl method and related functionality for handling
unresolved types during variable declaration processing.
"""

import unittest
from unittest.mock import Mock, patch

from src.interpreter import Interpreter, ActivationRecord, ARType
from src.spi_ast import (
    UnresolvedType, ClassType, EnumType, RecordType, PrimitiveType, StringType, 
    VarDecl, Var, ArrayType, ClassDecl, EnumDecl
)
from src.spi_token import Token, TokenType, ElementType
from src.error import TypeResolutionError, InterpreterError
from src.object import (
    IntegerObject, RealObject, BooleanObject, StringObject, ArrayObject,
    InstanceObject, RecordInstanceObject, RecordClassObject
)
from src.util import SpiUtil


class VarDeclUnresolvedTypesTestCase(unittest.TestCase):
    """Test cases for variable declaration with unresolved types."""

    def setUp(self):
        """Set up test fixtures."""
        # Create a mock program tree
        self.mock_program = Mock()
        self.mock_program.uses_clause = ['TestModule']
        
        # Create interpreter instance
        self.interpreter = Interpreter(self.mock_program)
        
        # Create test activation record
        self.ar = ActivationRecord(
            name="test_program",
            type=ARType.PROGRAM,
            nesting_level=1
        )
        self.interpreter.call_stack.push(self.ar)

    def test_var_decl_with_unresolved_integer_type(self):
        """Test variable declaration with unresolved Integer type."""
        # Create variable declaration with unresolved Integer type
        var_token = Token(TokenType.ID, "myInt", 1, 1)
        type_token = Token(TokenType.ID, "Integer", 1, 8)
        unresolved_type = UnresolvedType(type_token, "Integer")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        # Mock the type resolution
        with patch.object(self.interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.return_value = PrimitiveType(Token(TokenType.INTEGER, "Integer", 1, 8))
            
            # Visit the variable declaration
            self.interpreter.visit_VarDecl(var_decl)
            
            # Check that variable was created with correct type
            self.assertIn("myInt", self.ar.members)
            self.assertIsInstance(self.ar.members["myInt"], IntegerObject)
            self.assertEqual(self.ar.members["myInt"].value, 0)
            
            # Check metadata
            meta = self.ar.get_meta("myInt")
            self.assertEqual(meta.type, ElementType.INTEGER)

    def test_var_decl_with_unresolved_real_type(self):
        """Test variable declaration with unresolved Real type."""
        var_token = Token(TokenType.ID, "myReal", 1, 1)
        type_token = Token(TokenType.ID, "Real", 1, 8)
        unresolved_type = UnresolvedType(type_token, "Real")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        with patch.object(self.interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.return_value = PrimitiveType(Token(TokenType.REAL, "Real", 1, 8))
            
            self.interpreter.visit_VarDecl(var_decl)
            
            self.assertIn("myReal", self.ar.members)
            self.assertIsInstance(self.ar.members["myReal"], RealObject)
            self.assertEqual(self.ar.members["myReal"].value, 0.0)
            
            meta = self.ar.get_meta("myReal")
            self.assertEqual(meta.type, ElementType.REAL)

    def test_var_decl_with_unresolved_boolean_type(self):
        """Test variable declaration with unresolved Boolean type."""
        var_token = Token(TokenType.ID, "myBool", 1, 1)
        type_token = Token(TokenType.ID, "Boolean", 1, 8)
        unresolved_type = UnresolvedType(type_token, "Boolean")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        with patch.object(self.interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.return_value = PrimitiveType(Token(TokenType.BOOLEAN, "Boolean", 1, 8))
            
            self.interpreter.visit_VarDecl(var_decl)
            
            self.assertIn("myBool", self.ar.members)
            self.assertIsInstance(self.ar.members["myBool"], BooleanObject)
            self.assertEqual(self.ar.members["myBool"].value, False)
            
            meta = self.ar.get_meta("myBool")
            self.assertEqual(meta.type, ElementType.BOOL)

    def test_var_decl_with_unresolved_string_type(self):
        """Test variable declaration with unresolved String type."""
        var_token = Token(TokenType.ID, "myStr", 1, 1)
        type_token = Token(TokenType.ID, "String", 1, 8)
        unresolved_type = UnresolvedType(type_token, "String")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        with patch.object(self.interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.return_value = StringType(Token(TokenType.STRING, "String", 1, 8))
            
            self.interpreter.visit_VarDecl(var_decl)
            
            self.assertIn("myStr", self.ar.members)
            self.assertIsInstance(self.ar.members["myStr"], StringObject)
            self.assertEqual(self.ar.members["myStr"].value, "")
            
            meta = self.ar.get_meta("myStr")
            self.assertEqual(meta.type, ElementType.STRING)
            self.assertEqual(meta.limit, 255)  # Default string limit

    def test_var_decl_with_unresolved_class_type(self):
        """Test variable declaration with unresolved class type."""
        var_token = Token(TokenType.ID, "myObj", 1, 1)
        type_token = Token(TokenType.ID, "MyClass", 1, 8)
        unresolved_type = UnresolvedType(type_token, "MyClass")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        # Set up a mock class declaration in the activation record
        mock_class_decl = Mock(spec=ClassDecl)
        mock_class_decl.class_name = "MyClass"
        mock_class_decl.fields = {"field1": "value1"}
        self.ar[SpiUtil.toClassName("MyClass")] = mock_class_decl
        
        with patch.object(self.interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.return_value = ClassType(type_token)
            
            self.interpreter.visit_VarDecl(var_decl)
            
            self.assertIn("myObj", self.ar.members)
            self.assertIsInstance(self.ar.members["myObj"], InstanceObject)
            self.assertEqual(self.ar.members["myObj"].class_name, "MyClass")
            
            meta = self.ar.get_meta("myObj")
            self.assertEqual(meta.type, ElementType.INSTANCE)
            self.assertTrue(meta.is_instance)
            self.assertEqual(meta.ref_class_name, "MyClass")

    def test_var_decl_with_unresolved_record_type(self):
        """Test variable declaration with unresolved record type."""
        var_token = Token(TokenType.ID, "myRec", 1, 1)
        type_token = Token(TokenType.ID, "MyRecord", 1, 8)
        unresolved_type = UnresolvedType(type_token, "MyRecord")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        # Set up a mock record class in the activation record
        mock_record_class = Mock(spec=RecordClassObject)
        mock_record_class.record_name = "MyRecord"
        mock_record_class.fields = {"field1": "value1", "field2": "value2"}
        self.ar["MyRecord"] = mock_record_class
        
        with patch.object(self.interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.return_value = RecordType(type_token)
            
            self.interpreter.visit_VarDecl(var_decl)
            
            self.assertIn("myRec", self.ar.members)
            self.assertIsInstance(self.ar.members["myRec"], RecordInstanceObject)
            self.assertEqual(self.ar.members["myRec"].record_name, "MyRecord")
            
            meta = self.ar.get_meta("myRec")
            self.assertEqual(meta.type, ElementType.RECORD_CLASS)
            self.assertTrue(meta.is_record_instance)
            self.assertEqual(meta.ref_record_name, "MyRecord")

    def test_var_decl_with_unresolved_enum_type(self):
        """Test variable declaration with unresolved enum type."""
        var_token = Token(TokenType.ID, "myEnum", 1, 1)
        type_token = Token(TokenType.ID, "MyEnum", 1, 8)
        unresolved_type = UnresolvedType(type_token, "MyEnum")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        # Set up a mock enum declaration in the activation record
        mock_enum_decl = Mock(spec=EnumDecl)
        mock_enum_decl.entries = {0: "VALUE1", 1: "VALUE2"}
        self.ar["MyEnum"] = mock_enum_decl
        
        with patch.object(self.interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.return_value = EnumType(type_token)
            
            self.interpreter.visit_VarDecl(var_decl)
            
            self.assertIn("myEnum", self.ar.members)
            self.assertEqual(self.ar.members["myEnum"], mock_enum_decl)
            
            meta = self.ar.get_meta("myEnum")
            self.assertEqual(meta.type, ElementType.ENUM)
            self.assertTrue(meta.is_enum)

    def test_var_decl_type_resolution_error_with_context(self):
        """Test that type resolution errors include proper context information."""
        var_token = Token(TokenType.ID, "myVar", 5, 10)
        type_token = Token(TokenType.ID, "UnknownType", 5, 18)
        unresolved_type = UnresolvedType(type_token, "UnknownType")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        # Mock type resolution to raise an error
        with patch.object(self.interpreter, 'visit_UnresolvedType') as mock_visit:
            original_error = TypeResolutionError(
                "Unknown type 'UnknownType'",
                suggestions=["Integer", "Real"],
                token=type_token
            )
            mock_visit.side_effect = original_error
            
            with self.assertRaises(TypeResolutionError) as context:
                self.interpreter.visit_VarDecl(var_decl)
            
            error = context.exception
            # Check that the error message includes variable context
            self.assertIn("Failed to declare variable 'myVar'", str(error))
            self.assertIn("line 5", str(error))
            self.assertIn("UnknownType", str(error))
            
            # Check that suggestions are preserved
            self.assertEqual(error.suggestions, ["Integer", "Real"])

    def test_var_decl_class_not_found_error(self):
        """Test error handling when resolved class type is not found in scope."""
        var_token = Token(TokenType.ID, "myObj", 3, 5)
        type_token = Token(TokenType.ID, "MissingClass", 3, 12)
        unresolved_type = UnresolvedType(type_token, "MissingClass")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        with patch.object(self.interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.return_value = ClassType(type_token)
            
            with self.assertRaises(TypeResolutionError) as context:
                self.interpreter.visit_VarDecl(var_decl)
            
            error = context.exception
            self.assertIn("Failed to declare variable 'myObj'", str(error))
            self.assertIn("Class 'MissingClass' not found in current scope", str(error))

    def test_var_decl_record_not_found_error(self):
        """Test error handling when resolved record type is not found in scope."""
        var_token = Token(TokenType.ID, "myRec", 4, 8)
        type_token = Token(TokenType.ID, "MissingRecord", 4, 15)
        unresolved_type = UnresolvedType(type_token, "MissingRecord")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        with patch.object(self.interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.return_value = RecordType(type_token)
            
            with self.assertRaises(TypeResolutionError) as context:
                self.interpreter.visit_VarDecl(var_decl)
            
            error = context.exception
            self.assertIn("Failed to declare variable 'myRec'", str(error))
            self.assertIn("Record 'MissingRecord' not found in current scope", str(error))

    def test_var_decl_enum_not_found_error(self):
        """Test error handling when resolved enum type is not found in scope."""
        var_token = Token(TokenType.ID, "myEnum", 6, 3)
        type_token = Token(TokenType.ID, "MissingEnum", 6, 11)
        unresolved_type = UnresolvedType(type_token, "MissingEnum")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        with patch.object(self.interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.return_value = EnumType(type_token)
            
            with self.assertRaises(TypeResolutionError) as context:
                self.interpreter.visit_VarDecl(var_decl)
            
            error = context.exception
            self.assertIn("Failed to declare variable 'myEnum'", str(error))
            self.assertIn("Enum 'MissingEnum' not found in current scope", str(error))

    def test_var_decl_unsupported_resolved_type(self):
        """Test error handling for unsupported resolved types."""
        var_token = Token(TokenType.ID, "myVar", 7, 1)
        type_token = Token(TokenType.ID, "WeirdType", 7, 8)
        unresolved_type = UnresolvedType(type_token, "WeirdType")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        # Create a mock type that's not supported
        class UnsupportedType:
            def __init__(self, token):
                self.token = token
        
        with patch.object(self.interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.return_value = UnsupportedType(type_token)
            
            with self.assertRaises(TypeResolutionError) as context:
                self.interpreter.visit_VarDecl(var_decl)
            
            error = context.exception
            self.assertIn("Failed to declare variable 'myVar'", str(error))
            self.assertIn("Unsupported resolved type 'UnsupportedType'", str(error))
            self.assertIn("WeirdType", str(error))

    def test_var_decl_unsupported_primitive_type(self):
        """Test error handling for unsupported primitive types."""
        var_token = Token(TokenType.ID, "myVar", 8, 1)
        type_token = Token(TokenType.ID, "WeirdPrimitive", 8, 8)
        unresolved_type = UnresolvedType(type_token, "WeirdPrimitive")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        with patch.object(self.interpreter, 'visit_UnresolvedType') as mock_visit:
            # Create a primitive type with an unsupported token type
            weird_token = Token(TokenType.ID, "WeirdPrimitive", 8, 8)  # ID instead of primitive type
            mock_visit.return_value = PrimitiveType(weird_token)
            
            with self.assertRaises(TypeResolutionError) as context:
                self.interpreter.visit_VarDecl(var_decl)
            
            error = context.exception
            self.assertIn("Failed to declare variable 'myVar'", str(error))
            self.assertIn("Unsupported primitive type", str(error))

    def test_var_decl_unexpected_error_handling(self):
        """Test handling of unexpected errors during variable declaration."""
        var_token = Token(TokenType.ID, "myVar", 9, 1)
        type_token = Token(TokenType.ID, "SomeType", 9, 8)
        unresolved_type = UnresolvedType(type_token, "SomeType")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        with patch.object(self.interpreter, 'visit_UnresolvedType') as mock_visit:
            # Simulate an unexpected error (not TypeResolutionError)
            mock_visit.side_effect = RuntimeError("Unexpected runtime error")
            
            with self.assertRaises(TypeResolutionError) as context:
                self.interpreter.visit_VarDecl(var_decl)
            
            error = context.exception
            self.assertIn("Failed to declare variable 'myVar'", str(error))
            self.assertIn("line 9", str(error))
            self.assertIn("Unexpected error resolving type 'SomeType'", str(error))
            self.assertIn("Unexpected runtime error", str(error))

    def test_var_decl_concrete_record_type_error(self):
        """Test error handling for concrete record types when record is not found."""
        var_token = Token(TokenType.ID, "myRec", 10, 1)
        type_token = Token(TokenType.RECORD, "MissingRecord", 10, 8)
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, Mock(token=type_token))
        
        # Don't add the record to the activation record
        
        with self.assertRaises(TypeResolutionError) as context:
            self.interpreter.visit_VarDecl(var_decl)
        
        error = context.exception
        self.assertIn("Failed to declare variable 'myRec'", str(error))
        self.assertIn("Record type 'MissingRecord' not found", str(error))

    def test_var_decl_concrete_class_type_error(self):
        """Test error handling for concrete class types when class is not found."""
        var_token = Token(TokenType.ID, "myObj", 11, 1)
        type_token = Token(TokenType.CLASS, "MissingClass", 11, 8)
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, Mock(token=type_token))
        
        # Don't add the class to the activation record
        
        with self.assertRaises(TypeResolutionError) as context:
            self.interpreter.visit_VarDecl(var_decl)
        
        error = context.exception
        self.assertIn("Failed to declare variable 'myObj'", str(error))
        self.assertIn("Class type 'MissingClass' not found", str(error))


if __name__ == '__main__':
    unittest.main()