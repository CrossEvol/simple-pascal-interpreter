"""Tests for AST node classes"""

import unittest
from spi.ast import BreakStatement, ContinueStatement
from spi.token import Token, TokenType


class TestBreakStatement(unittest.TestCase):
    """Test cases for BreakStatement AST node"""

    def test_break_statement_creation(self):
        """Test that BreakStatement can be created with a token"""
        token = Token(TokenType.BREAK, "break", 1, 1)
        break_stmt = BreakStatement(token)
        
        self.assertIsInstance(break_stmt, BreakStatement)
        self.assertEqual(break_stmt.token, token)
        self.assertEqual(break_stmt.token.type, TokenType.BREAK)
        self.assertEqual(break_stmt.token.value, "break")

    def test_break_statement_token_attributes(self):
        """Test that BreakStatement preserves token attributes"""
        token = Token(TokenType.BREAK, "break", 5, 10)
        break_stmt = BreakStatement(token)
        
        self.assertEqual(break_stmt.token.lineno, 5)
        self.assertEqual(break_stmt.token.column, 10)


class TestContinueStatement(unittest.TestCase):
    """Test cases for ContinueStatement AST node"""

    def test_continue_statement_creation(self):
        """Test that ContinueStatement can be created with a token"""
        token = Token(TokenType.CONTINUE, "continue", 1, 1)
        continue_stmt = ContinueStatement(token)
        
        self.assertIsInstance(continue_stmt, ContinueStatement)
        self.assertEqual(continue_stmt.token, token)
        self.assertEqual(continue_stmt.token.type, TokenType.CONTINUE)
        self.assertEqual(continue_stmt.token.value, "continue")

    def test_continue_statement_token_attributes(self):
        """Test that ContinueStatement preserves token attributes"""
        token = Token(TokenType.CONTINUE, "continue", 3, 7)
        continue_stmt = ContinueStatement(token)
        
        self.assertEqual(continue_stmt.token.lineno, 3)
        self.assertEqual(continue_stmt.token.column, 7)


if __name__ == "__main__":
    unittest.main()