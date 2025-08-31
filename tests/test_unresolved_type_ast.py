import unittest
from src.spi_ast import UnresolvedType, Type, PrimitiveType
from src.spi_token import Token, TokenType


class UnresolvedTypeTestCase(unittest.TestCase):
    """Test cases for UnresolvedType AST node."""
    
    def setUp(self):
        """Set up test fixtures."""
        self.token = Token(TokenType.ID, "MyType", lineno=1, column=1)
        self.type_name = "MyType"
    
    def test_unresolved_type_creation(self):
        """Test creating an UnresolvedType node."""
        unresolved_type = UnresolvedType(self.token, self.type_name)
        
        # Test basic properties
        self.assertEqual(unresolved_type.type_name, "MyType")
        self.assertEqual(unresolved_type.token, self.token)
        self.assertEqual(unresolved_type.value, "MyType")
        self.assertIsNone(unresolved_type.resolved_type)
    
    def test_unresolved_type_extends_type(self):
        """Test that UnresolvedType properly extends Type base class."""
        unresolved_type = UnresolvedType(self.token, self.type_name)
        
        # Should be instance of Type
        self.assertIsInstance(unresolved_type, Type)
        
        # Should have Type's properties
        self.assertEqual(unresolved_type.token, self.token)
        self.assertEqual(unresolved_type.value, self.token.value)
    
    def test_unresolved_type_str_representation(self):
        """Test string representation of UnresolvedType."""
        unresolved_type = UnresolvedType(self.token, self.type_name)
        
        expected_str = "UnresolvedType[MyType]"
        self.assertEqual(str(unresolved_type), expected_str)
    
    def test_resolved_type_caching(self):
        """Test that resolved_type field can be used for caching."""
        unresolved_type = UnresolvedType(self.token, self.type_name)
        
        # Initially None
        self.assertIsNone(unresolved_type.resolved_type)
        
        # Can be set to a resolved type
        resolved_token = Token(TokenType.INTEGER, "INTEGER", lineno=1, column=1)
        resolved_type = PrimitiveType(resolved_token)
        unresolved_type.resolved_type = resolved_type
        
        # Should be cached
        self.assertEqual(unresolved_type.resolved_type, resolved_type)
        self.assertIsInstance(unresolved_type.resolved_type, PrimitiveType)
    
    def test_different_type_names(self):
        """Test UnresolvedType with different type names."""
        test_cases = [
            ("Integer", "Integer"),
            ("MyClass", "MyClass"),
            ("TMap", "TMap"),
            ("UserRecord", "UserRecord"),
        ]
        
        for type_name, expected_name in test_cases:
            token = Token(TokenType.ID, type_name, lineno=1, column=1)
            unresolved_type = UnresolvedType(token, type_name)
            
            self.assertEqual(unresolved_type.type_name, expected_name)
            self.assertEqual(str(unresolved_type), f"UnresolvedType[{expected_name}]")
    
    def test_token_properties(self):
        """Test that token properties are properly inherited."""
        token = Token(TokenType.ID, "TestType", lineno=5, column=10)
        unresolved_type = UnresolvedType(token, "TestType")
        
        self.assertEqual(unresolved_type.token.lineno, 5)
        self.assertEqual(unresolved_type.token.column, 10)
        self.assertEqual(unresolved_type.token.type, TokenType.ID)
        self.assertEqual(unresolved_type.token.value, "TestType")


if __name__ == '__main__':
    unittest.main()