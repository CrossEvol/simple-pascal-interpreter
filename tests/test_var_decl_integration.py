"""
Integration tests for variable declaration with unresolved types.

Tests the interpreter functionality for variable declarations with unresolved types.
Note: These tests focus on the interpreter layer and skip semantic analysis
since the semantic analyzer hasn't been updated to handle UnresolvedType nodes yet.
"""

import unittest
from unittest.mock import Mock, patch

from src.interpreter import Interpreter, ActivationRecord, ARType
from src.spi_ast import (
    Program, Block, Compound, VarDecl, Var, UnresolvedType, 
    PrimitiveType, StringType, Assign, Num
)
from src.spi_token import Token, TokenType
from src.error import TypeResolutionError
from src.object import IntegerObject, RealObject, BooleanObject, StringObject


class VarDeclIntegrationTestCase(unittest.TestCase):
    """Integration test cases for variable declaration with unresolved types."""

    def setUp(self):
        """Set up test fixtures."""
        # Create a minimal program structure for testing
        self.program_token = Token(TokenType.PROGRAM, "TestProgram", 1, 1)
        self.compound = Compound()
        self.block = Block(declarations=[], compound_statement=self.compound)
        self.program = Program("TestProgram", self.block)

    def test_interpreter_var_decl_with_unresolved_integer(self):
        """Test interpreter handling of variable declaration with unresolved Integer type."""
        # Create variable declaration with unresolved Integer type
        var_token = Token(TokenType.ID, "myInt", 2, 5)
        type_token = Token(TokenType.ID, "Integer", 2, 12)
        unresolved_type = UnresolvedType(type_token, "Integer")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        # Add the declaration to the program
        self.block.declarations = [var_decl]
        
        # Create interpreter
        interpreter = Interpreter(self.program)
        
        # Capture the activation record during interpretation
        captured_ar = None
        
        def capture_ar_after_var_decl(node):
            nonlocal captured_ar
            # Call the original visit_VarDecl method
            original_result = interpreter.__class__.visit_VarDecl(interpreter, node)
            # Capture the activation record after variable declaration
            captured_ar = interpreter.call_stack.peek()
            return original_result
        
        # Mock the type resolution to return a primitive Integer type
        with patch.object(interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.return_value = PrimitiveType(Token(TokenType.INTEGER, "Integer", 2, 12))
            
            # Patch visit_VarDecl to capture the activation record
            with patch.object(interpreter, 'visit_VarDecl', side_effect=capture_ar_after_var_decl):
                # Interpret the program
                interpreter.interpret()
            
            # Check that variable was created correctly
            self.assertIsNotNone(captured_ar)
            self.assertIn("myInt", captured_ar.members)
            self.assertIsInstance(captured_ar.members["myInt"], IntegerObject)
            self.assertEqual(captured_ar.members["myInt"].value, 0)

    def test_interpreter_var_decl_with_unresolved_string(self):
        """Test interpreter handling of variable declaration with unresolved String type."""
        # Create variable declaration with unresolved String type
        var_token = Token(TokenType.ID, "myStr", 2, 5)
        type_token = Token(TokenType.ID, "String", 2, 12)
        unresolved_type = UnresolvedType(type_token, "String")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        # Add the declaration to the program
        self.block.declarations = [var_decl]
        
        # Create interpreter
        interpreter = Interpreter(self.program)
        
        # Capture the activation record during interpretation
        captured_ar = None
        
        def capture_ar_after_var_decl(node):
            nonlocal captured_ar
            # Call the original visit_VarDecl method
            original_result = interpreter.__class__.visit_VarDecl(interpreter, node)
            # Capture the activation record after variable declaration
            captured_ar = interpreter.call_stack.peek()
            return original_result
        
        # Mock the type resolution to return a String type
        with patch.object(interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.return_value = StringType(Token(TokenType.STRING, "String", 2, 12))
            
            # Patch visit_VarDecl to capture the activation record
            with patch.object(interpreter, 'visit_VarDecl', side_effect=capture_ar_after_var_decl):
                # Interpret the program
                interpreter.interpret()
            
            # Check that variable was created correctly
            self.assertIsNotNone(captured_ar)
            self.assertIn("myStr", captured_ar.members)
            self.assertIsInstance(captured_ar.members["myStr"], StringObject)
            self.assertEqual(captured_ar.members["myStr"].value, "")

    def test_interpreter_var_decl_type_resolution_error(self):
        """Test interpreter error handling for unresolved type that cannot be resolved."""
        # Create variable declaration with unresolved unknown type
        var_token = Token(TokenType.ID, "myVar", 2, 5)
        type_token = Token(TokenType.ID, "UnknownType", 2, 12)
        unresolved_type = UnresolvedType(type_token, "UnknownType")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        # Add the declaration to the program
        self.block.declarations = [var_decl]
        
        # Create interpreter
        interpreter = Interpreter(self.program)
        
        # Mock the type resolution to raise an error
        with patch.object(interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.side_effect = TypeResolutionError(
                "Unknown type 'UnknownType'",
                suggestions=["Integer", "Real"],
                token=type_token
            )
            
            # Interpret the program - should raise TypeResolutionError
            with self.assertRaises(TypeResolutionError) as context:
                interpreter.interpret()
            
            error = context.exception
            self.assertIn("Failed to declare variable 'myVar'", str(error))
            self.assertIn("UnknownType", str(error))

    def test_interpreter_multiple_var_decls_with_mixed_types(self):
        """Test interpreter handling of multiple variable declarations with mixed resolved/unresolved types."""
        # Create multiple variable declarations
        declarations = []
        
        # Concrete Integer type
        var1_token = Token(TokenType.ID, "concreteInt", 2, 5)
        int_type_token = Token(TokenType.INTEGER, "INTEGER", 2, 18)
        concrete_int_type = Mock()
        concrete_int_type.token = int_type_token
        var1_node = Var(var1_token)
        var_decl1 = VarDecl(var1_node, concrete_int_type)
        declarations.append(var_decl1)
        
        # Unresolved Real type
        var2_token = Token(TokenType.ID, "unresolvedReal", 3, 5)
        real_type_token = Token(TokenType.ID, "Real", 3, 20)
        unresolved_real_type = UnresolvedType(real_type_token, "Real")
        var2_node = Var(var2_token)
        var_decl2 = VarDecl(var2_node, unresolved_real_type)
        declarations.append(var_decl2)
        
        # Add the declarations to the program
        self.block.declarations = declarations
        
        # Create interpreter
        interpreter = Interpreter(self.program)
        
        # Capture the activation record during interpretation
        captured_ar = None
        
        def capture_ar_after_var_decl(node):
            nonlocal captured_ar
            # Call the original visit_VarDecl method
            original_result = interpreter.__class__.visit_VarDecl(interpreter, node)
            # Capture the activation record after variable declaration
            captured_ar = interpreter.call_stack.peek()
            return original_result
        
        # Mock the type resolution for the unresolved type
        with patch.object(interpreter, 'visit_UnresolvedType') as mock_visit:
            mock_visit.return_value = PrimitiveType(Token(TokenType.REAL, "Real", 3, 20))
            
            # Patch visit_VarDecl to capture the activation record
            with patch.object(interpreter, 'visit_VarDecl', side_effect=capture_ar_after_var_decl):
                # Interpret the program
                interpreter.interpret()
            
            # Check that both variables were created correctly
            self.assertIsNotNone(captured_ar)
            
            # Check concrete type variable
            self.assertIn("concreteInt", captured_ar.members)
            self.assertIsInstance(captured_ar.members["concreteInt"], IntegerObject)
            
            # Check unresolved type variable
            self.assertIn("unresolvedReal", captured_ar.members)
            self.assertIsInstance(captured_ar.members["unresolvedReal"], RealObject)

    def test_interpreter_error_context_preservation(self):
        """Test that error context is properly preserved through the interpreter layers."""
        # Create variable declaration with problematic unresolved type
        var_token = Token(TokenType.ID, "problematicVar", 5, 10)
        type_token = Token(TokenType.ID, "ProblematicType", 5, 25)
        unresolved_type = UnresolvedType(type_token, "ProblematicType")
        var_node = Var(var_token)
        var_decl = VarDecl(var_node, unresolved_type)
        
        # Add the declaration to the program
        self.block.declarations = [var_decl]
        
        # Create interpreter
        interpreter = Interpreter(self.program)
        
        # Mock the type resolution to raise an error with specific context
        with patch.object(interpreter, 'visit_UnresolvedType') as mock_visit:
            original_error = TypeResolutionError(
                "Type 'ProblematicType' not found in module 'TestModule'",
                suggestions=["Integer", "String", "TestModule.SomeType"],
                token=type_token
            )
            mock_visit.side_effect = original_error
            
            # Interpret the program - should raise enhanced TypeResolutionError
            with self.assertRaises(TypeResolutionError) as context:
                interpreter.interpret()
            
            error = context.exception
            
            # Check that the error message includes variable declaration context
            self.assertIn("Failed to declare variable 'problematicVar'", str(error))
            self.assertIn("line 5", str(error))
            
            # Check that original error information is preserved
            self.assertIn("ProblematicType", str(error))
            self.assertIn("TestModule", str(error))
            
            # Check that suggestions are preserved
            self.assertEqual(error.suggestions, ["Integer", "String", "TestModule.SomeType"])
            
            # Check that token information is preserved
            self.assertEqual(error.token, type_token)


if __name__ == '__main__':
    unittest.main()