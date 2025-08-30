"""
Unit tests for UnresolvedType handling in the interpreter.

Tests the visit_UnresolvedType method and related functionality for resolving
types during interpretation phase.
"""

import unittest
from unittest.mock import Mock, patch

from src.interpreter import Interpreter, ActivationRecord, ARType
from src.spi_ast import UnresolvedType, ClassType, EnumType, RecordType, PrimitiveType, StringType, VarDecl, Var, ArrayType
from src.spi_token import Token, TokenType
from src.error import TypeResolutionError as ErrorTypeResolutionError


class UnresolvedTypeInterpreterTestCase(unittest.TestCase):
    """Test cases for UnresolvedType handling in interpreter."""

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

    def test_visit_unresolved_type_caching(self):
        """Test that resolved types are cached to avoid repeated resolution."""
        # Create an UnresolvedType node
        token = Token(TokenType.ID, "Integer", 1, 1)
        unresolved_type = UnresolvedType(token, "Integer")
        
        # First resolution should call the type resolver
        with patch.object(self.interpreter.type_resolver, 'resolve_type') as mock_resolve:
            mock_resolve.return_value = PrimitiveType(Token(TokenType.INTEGER, "Integer", 1, 1))
            
            # First call
            result1 = self.interpreter.visit_UnresolvedType(unresolved_type)
            
            # Second call should use cached result
            result2 = self.interpreter.visit_UnresolvedType(unresolved_type)
            
            # Type resolver should only be called once
            self.assertEqual(mock_resolve.call_count, 1)
            
            # Both results should be the same
            self.assertEqual(result1, result2)
            
            # The node should have cached the resolved type
            self.assertIsNotNone(unresolved_type.resolved_type)

    def test_visit_unresolved_type_primitive_resolution(self):
        """Test resolution of primitive types."""
        # Test Integer type
        token = Token(TokenType.ID, "Integer", 1, 1)
        unresolved_type = UnresolvedType(token, "Integer")
        
        with patch.object(self.interpreter.type_resolver, 'resolve_type') as mock_resolve:
            expected_type = PrimitiveType(Token(TokenType.INTEGER, "Integer", 1, 1))
            mock_resolve.return_value = expected_type
            
            result = self.interpreter.visit_UnresolvedType(unresolved_type)
            
            self.assertIsInstance(result, PrimitiveType)
            self.assertEqual(result.token.type, TokenType.INTEGER)

    def test_visit_unresolved_type_class_resolution(self):
        """Test resolution of class types."""
        token = Token(TokenType.ID, "MyClass", 1, 1)
        unresolved_type = UnresolvedType(token, "MyClass")
        
        with patch.object(self.interpreter.type_resolver, 'resolve_type') as mock_resolve:
            expected_type = ClassType(token)
            mock_resolve.return_value = expected_type
            
            result = self.interpreter.visit_UnresolvedType(unresolved_type)
            
            self.assertIsInstance(result, ClassType)

    def test_visit_unresolved_type_enum_resolution(self):
        """Test resolution of enum types."""
        token = Token(TokenType.ID, "MyEnum", 1, 1)
        unresolved_type = UnresolvedType(token, "MyEnum")
        
        with patch.object(self.interpreter.type_resolver, 'resolve_type') as mock_resolve:
            expected_type = EnumType(token)
            mock_resolve.return_value = expected_type
            
            result = self.interpreter.visit_UnresolvedType(unresolved_type)
            
            self.assertIsInstance(result, EnumType)

    def test_visit_unresolved_type_record_resolution(self):
        """Test resolution of record types."""
        token = Token(TokenType.ID, "MyRecord", 1, 1)
        unresolved_type = UnresolvedType(token, "MyRecord")
        
        with patch.object(self.interpreter.type_resolver, 'resolve_type') as mock_resolve:
            expected_type = RecordType(token)
            mock_resolve.return_value = expected_type
            
            result = self.interpreter.visit_UnresolvedType(unresolved_type)
            
            self.assertIsInstance(result, RecordType)

    def test_visit_unresolved_type_resolution_error(self):
        """Test handling of type resolution errors."""
        token = Token(TokenType.ID, "UnknownType", 1, 1)
        unresolved_type = UnresolvedType(token, "UnknownType")
        
        with patch.object(self.interpreter.type_resolver, 'resolve_type') as mock_resolve:
            mock_resolve.side_effect = ErrorTypeResolutionError(
                "Unknown type 'UnknownType'",
                suggestions=["Integer", "Real"],
                token=token
            )
            
            with self.assertRaises(ErrorTypeResolutionError) as context:
                self.interpreter.visit_UnresolvedType(unresolved_type)
            
            self.assertIn("UnknownType", str(context.exception))

    def test_build_type_resolution_context(self):
        """Test building type resolution context from activation record."""
        # Set up activation record with various types
        self.ar.set_meta("MyClass", Mock())
        self.ar.set_is_class("MyClass", True)
        
        self.ar.set_meta("MyEnum", Mock())
        self.ar.set_is_enum("MyEnum")
        
        self.ar.set_meta("MyRecord", Mock())
        self.ar.set_is_record("MyRecord")
        
        # Build context
        context = self.interpreter._build_type_resolution_context(self.ar)
        
        # Verify context contents
        self.assertIn("MyClass", context.local_classes)
        self.assertIn("MyEnum", context.local_enums)
        self.assertIn("MyRecord", context.local_records)
        self.assertEqual(context.imported_modules, ['TestModule'])
        self.assertEqual(context.module_registry, self.interpreter.module_registry)

    def test_var_decl_with_unresolved_primitive_type(self):
        """Test variable declaration with unresolved primitive type."""
        # Create variable declaration with unresolved type
        var_token = Token(TokenType.ID, "myVar", 1, 1)
        type_token = Token(TokenType.ID, "Integer", 1, 5)
        unresolved_type = UnresolvedType(type_token, "Integer")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        # Mock the type resolution
        with patch.object(self.interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.return_value = PrimitiveType(Token(TokenType.INTEGER, "Integer", 1, 5))
            
            # Visit the variable declaration
            self.interpreter.visit_VarDecl(var_decl)
            
            # Check that variable was created with correct type
            self.assertIn("myVar", self.ar.members)
            from src.object import IntegerObject
            self.assertIsInstance(self.ar.members["myVar"], IntegerObject)

    def test_var_decl_with_unresolved_string_type(self):
        """Test variable declaration with unresolved string type."""
        # Create variable declaration with unresolved string type
        var_token = Token(TokenType.ID, "myStr", 1, 1)
        type_token = Token(TokenType.ID, "String", 1, 5)
        unresolved_type = UnresolvedType(type_token, "String")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        # Mock the type resolution
        with patch.object(self.interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.return_value = StringType(Token(TokenType.STRING, "String", 1, 5))
            
            # Visit the variable declaration
            self.interpreter.visit_VarDecl(var_decl)
            
            # Check that variable was created with correct type
            self.assertIn("myStr", self.ar.members)
            from src.object import StringObject
            self.assertIsInstance(self.ar.members["myStr"], StringObject)

    def test_array_init_with_unresolved_element_type(self):
        """Test array initialization with unresolved element type."""
        # Create array type with unresolved element type
        lower_token = Token(TokenType.INTEGER_CONST, 1, 1, 1)
        upper_token = Token(TokenType.INTEGER_CONST, 5, 1, 1)
        element_type_token = Token(TokenType.ID, "Integer", 1, 1)
        unresolved_element_type = UnresolvedType(element_type_token, "Integer")
        
        from src.spi_ast import Num
        lower_bound = Num(lower_token)
        upper_bound = Num(upper_token)
        
        array_type = ArrayType(
            token=Token(TokenType.ARRAY, "array", 1, 1),
            lower=lower_bound,
            upper=upper_bound,
            element_type=unresolved_element_type,
            dynamic=False
        )
        
        # Mock the type resolution
        with patch.object(self.interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.return_value = PrimitiveType(Token(TokenType.INTEGER, "Integer", 1, 1))
            
            # Initialize the array
            elements, element_type = self.interpreter._Interpreter__initArray(array_type)
            
            # Check that array was initialized correctly
            from src.spi_token import ElementType
            self.assertEqual(element_type, ElementType.INTEGER)
            self.assertEqual(len(elements), 5)  # Array from 1 to 5
            
            # Check that all elements are IntegerObjects
            from src.object import IntegerObject
            for element in elements.values():
                self.assertIsInstance(element, IntegerObject)


if __name__ == '__main__':
    unittest.main()