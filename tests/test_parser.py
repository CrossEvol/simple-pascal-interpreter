import unittest

from spi.error import ErrorCode, ParserError
from spi.lexer import Lexer
from spi.parser import Parser


class ParserTestCase(unittest.TestCase):
    def makeParser(self, text):
        lexer = Lexer(text)
        parser = Parser(lexer)
        return parser

    def test_expression_invalid_syntax_01(self):
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                a : INTEGER;
            BEGIN
               a := 10 * ;  {Invalid syntax}
            END.
            """
        )
        with self.assertRaises(ParserError) as cm:
            parser.parse()
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.UNEXPECTED_TOKEN)
        self.assertEqual(the_exception.token.value, ";")
        self.assertEqual(the_exception.token.lineno, 6)

    def test_expression_invalid_syntax_02(self):
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                a : INTEGER;
            BEGIN
               a := 1 (1 + 2); {Invalid syntax}
            END.
            """
        )
        with self.assertRaises(ParserError) as cm:
            parser.parse()
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.UNEXPECTED_TOKEN)
        self.assertEqual(the_exception.token.value, "(")
        self.assertEqual(the_exception.token.lineno, 6)

    def test_maximum_one_VAR_block_is_allowed(self):
        # zero VARs
        parser = self.makeParser(
            """
            PROGRAM Test;
            BEGIN
            END.
            """
        )
        parser.parse()

        # one VAR
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                a : INTEGER;
            BEGIN
            END.
            """
        )
        parser.parse()

        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                a : INTEGER;
            VAR
                b : INTEGER;
            BEGIN
               a := 5;
               b := a + 10;
            END.
            """
        )
        with self.assertRaises(ParserError) as cm:
            parser.parse()
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.UNEXPECTED_TOKEN)
        self.assertEqual(the_exception.token.value, "VAR")
        self.assertEqual(the_exception.token.lineno, 5)  # second VAR

    def test_case_item_must_have_semi_end(self):
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                a : INTEGER;
            BEGIN
               case a of 
                1 : a := 2
            END.
            """
        )
        with self.assertRaises(ParserError) as cm:
            parser.parse()
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.UNEXPECTED_TOKEN)
        self.assertEqual(the_exception.token.value, "END")
        self.assertEqual(the_exception.token.lineno, 8)

    def test_case_of_statement_must_have_semi_end_for_else_statement(self):
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                a : INTEGER;
            BEGIN
               case a of 
                1 : a := 2;
               else
                a := 3
            END.
            """
        )
        with self.assertRaises(ParserError) as cm:
            parser.parse()
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.UNEXPECTED_TOKEN)
        self.assertEqual(the_exception.token.value, "END")
        self.assertEqual(the_exception.token.lineno, 10)


if __name__ == "__main__":
    unittest.main()
