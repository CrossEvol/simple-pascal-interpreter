import unittest

from spi.error import LexerError
from spi.lexer import Lexer
from spi.token import TokenType


class LexerTestCase(unittest.TestCase):
    def makeLexer(self, text):
        lexer = Lexer(text)
        return lexer

    def test_tokens(self):
        records = (
            ("234", TokenType.INTEGER_CONST, 234),
            ("3.14", TokenType.REAL_CONST, 3.14),
            ("*", TokenType.MUL, "*"),
            ("DIV", TokenType.INTEGER_DIV, "DIV"),
            ("/", TokenType.FLOAT_DIV, "/"),
            ("+", TokenType.PLUS, "+"),
            ("-", TokenType.MINUS, "-"),
            ("(", TokenType.LPAREN, "("),
            (")", TokenType.RPAREN, ")"),
            ("[", TokenType.LBRACKET, "["),
            ("]", TokenType.RBRACKET, "]"),
            (":=", TokenType.ASSIGN, ":="),
            (".", TokenType.DOT, "."),
            ("number", TokenType.ID, "number"),
            (";", TokenType.SEMI, ";"),
            ("BEGIN", TokenType.BEGIN, "BEGIN"),
            ("END", TokenType.END, "END"),
            ("PROCEDURE", TokenType.PROCEDURE, "PROCEDURE"),
            ("FUNCTION", TokenType.FUNCTION, "FUNCTION"),
            ("true", TokenType.TRUE, "TRUE"),
            ("false", TokenType.FALSE, "FALSE"),
            ("and", TokenType.AND, "AND"),
            ("or", TokenType.OR, "OR"),
            ("not", TokenType.NOT, "NOT"),
            ("TRUE", TokenType.TRUE, "TRUE"),
            ("FALSE", TokenType.FALSE, "FALSE"),
            ("AND", TokenType.AND, "AND"),
            ("OR", TokenType.OR, "OR"),
            ("NOT", TokenType.NOT, "NOT"),
            ("=", TokenType.EQ, "="),
            ("<>", TokenType.NE, "<>"),
            ("<", TokenType.LT, "<"),
            (">", TokenType.GT, ">"),
            ("<=", TokenType.LE, "<="),
            (">=", TokenType.GE, ">="),
            ("if", TokenType.IF, "IF"),
            ("then", TokenType.THEN, "THEN"),
            ("else", TokenType.ELSE, "ELSE"),
            ("IF", TokenType.IF, "IF"),
            ("THEN", TokenType.THEN, "THEN"),
            ("ELSE", TokenType.ELSE, "ELSE"),
            ("WHILE", TokenType.WHILE, "WHILE"),
            ("DO", TokenType.DO, "DO"),
            ("TO", TokenType.TO, "TO"),
            ("FOR", TokenType.FOR, "FOR"),
            ("ARRAY", TokenType.ARRAY, "ARRAY"),
            ("OF", TokenType.OF, "OF"),
            ("CASE", TokenType.CASE, "CASE"),
            ("..", TokenType.RANGE, ".."),
            ("STRING", TokenType.STRING, "STRING"),
            ("CHAR", TokenType.CHAR, "CHAR"),
            ("'abc'", TokenType.STRING_CONST, "abc"),
            ("'a'", TokenType.STRING_CONST, "a"),
            ("#65", TokenType.CHAR_CONST, "A"),
            ("#97", TokenType.CHAR_CONST, "a"),
            ("#32", TokenType.CHAR_CONST, " "),
            ("#0", TokenType.CHAR_CONST, "\0"),
        )
        for text, tok_type, tok_val in records:
            lexer = self.makeLexer(text)
            token = lexer.get_next_token()
            self.assertEqual(token.type, tok_type)
            self.assertEqual(token.value, tok_val)

    def test_lexer_exception(self):
        lexer = self.makeLexer("!")
        with self.assertRaises(LexerError):
            lexer.get_next_token()

    def test_lexer_char_const_error(self):
        # Test # without digits
        lexer = self.makeLexer("#")
        with self.assertRaises(LexerError):
            lexer.get_next_token()

        # Test # with non-digits
        lexer = self.makeLexer("#abc")
        with self.assertRaises(LexerError):
            lexer.get_next_token()


if __name__ == "__main__":
    unittest.main()
