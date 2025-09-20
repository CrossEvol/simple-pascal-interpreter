import unittest

from spi.error import ErrorCode, SemanticError
from spi.lexer import Lexer
from spi.parser import Parser
from spi.semantic_analyzer import SemanticAnalyzer


class SemanticAnalyzerTestCase(unittest.TestCase):
    def runSemanticAnalyzer(self, text):
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()

        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.visit(tree)
        return semantic_analyzer

    def test_semantic_duplicate_id_error(self):
        with self.assertRaises(SemanticError) as cm:
            self.runSemanticAnalyzer(
                """
            PROGRAM Test;
            VAR
                a : INTEGER;
                a : REAL;  {Duplicate identifier}
            BEGIN
               a := 5;
            END.
            """
            )
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.DUPLICATE_ID)
        self.assertEqual(the_exception.token.value, "a")
        self.assertEqual(the_exception.token.lineno, 5)

    def test_loop_control_variable_can_not_modified(self):
        with self.assertRaises(SemanticError) as cm:
            self.runSemanticAnalyzer(
                """
            program forLoop;
            var
                a, sum: integer;
            begin
                for a := 1 to 10 do
                begin
                    a := a + 1; {can not modified loop_control_var}
                    sum := sum + a;
                end;
            end.
            """
            )
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.MODIFY_LOOP_VAR_NOT_ALLOW)
        self.assertEqual(the_exception.token.value, "a")
        self.assertEqual(the_exception.token.lineno, 8)

    def test_semantic_id_not_found_error(self):
        with self.assertRaises(SemanticError) as cm:
            self.runSemanticAnalyzer(
                """
            PROGRAM Test;
            VAR
                a : INTEGER;
            BEGIN
               a := 5 + b;
            END.
            """
            )
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.ID_NOT_FOUND)
        self.assertEqual(the_exception.token.value, "b")

    def test_semantic_char_too_many_chars_string_literal(self):
        with self.assertRaises(SemanticError) as cm:
            self.runSemanticAnalyzer(
                """
            PROGRAM Test;
            VAR
                c : CHAR;
            BEGIN
               c := 'ab';  {Too many characters for CHAR}
            END.
            """
            )
        the_exception = cm.exception
        self.assertEqual(
            the_exception.error_code, ErrorCode.SEMANTIC_CHAR_TOO_MANY_CHARS
        )
        self.assertEqual(the_exception.token.value, "ab")

    def test_semantic_duplicate_case_label_for_integer(self):
        with self.assertRaises(SemanticError) as cm:
            self.runSemanticAnalyzer(
                """
            PROGRAM Test;
            VAR
                a : INTEGER;
            BEGIN
                a := 1;
                case a of
                    1: a := 2;
                    1: a := 2;
                end;
            END.
            """
            )
        the_exception = cm.exception
        self.assertEqual(
            the_exception.error_code, ErrorCode.SEMANTIC_DUPLICATE_CASE_LABEL
        )
        self.assertEqual(the_exception.token.value, "a")

    def test_semantic_duplicate_case_label_for_enum(self):
        with self.assertRaises(SemanticError) as cm:
            self.runSemanticAnalyzer(
                """
            PROGRAM Test;
            type
                TColor = (Red , Green , Blue);
            VAR
                color : TColor;
                i : Integer;
            BEGIN
                color := Red;
                case color of
                    Red: i := 0;
                    Green: i := 1;
                    Blue: i := 2;
                    Green: i := 3;
                end;
            END.
            """
            )
        the_exception = cm.exception
        self.assertEqual(
            the_exception.error_code, ErrorCode.SEMANTIC_DUPLICATE_CASE_LABEL
        )
        self.assertEqual(the_exception.token.value, "color")

    def test_semantic_unsupported_string_type_for_case_statement(self):
        with self.assertRaises(SemanticError) as cm:
            self.runSemanticAnalyzer(
                """
            PROGRAM Test;
            VAR
                name : String;
                i : Integer;
            BEGIN
                name := 'a';
                case name of
                    'a': i := 0;
                end;
            END.
            """
            )
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.SEMANTIC_UNSUPPORTED_TYPE)
        self.assertEqual(the_exception.token.value, "name")

    def test_semantic_unsupported_array_type_for_case_statement(
        self,
    ):
        with self.assertRaises(SemanticError) as cm:
            self.runSemanticAnalyzer(
                """
            PROGRAM Test;
            VAR
                arr : array of Integer;
                i : Integer;
            BEGIN
                case arr of 
                    1: i:= 10;
                end;
                writeln(i);
            END.
            """
            )
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.SEMANTIC_UNSUPPORTED_TYPE)
        self.assertEqual(the_exception.token.value, "arr")

    def test_semantic_incompatible_type_for_enum_for_case_statement(self):
        with self.assertRaises(SemanticError) as cm:
            self.runSemanticAnalyzer(
                """
            PROGRAM Test;
            type
                TColor = (Red , Green , Blue);
            VAR
                color : TColor;
                i : Integer;
            BEGIN
                color := Red;
                case color of
                    Red: i := 0;
                    Green: i := 1;
                    Blue: i := 2;
                    3: i:= 3;
                end;
            END.
            """
            )
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.SEMANTIC_UNKNOWN_ENUM)
        self.assertEqual(the_exception.token.value, "color")

    def test_semantic_incompatible_type_for_integer_in_case_statement(self):
        with self.assertRaises(SemanticError) as cm:
            self.runSemanticAnalyzer(
                """
            PROGRAM Test;
            type
                TColor = (Red , Green , Blue);
            VAR
                color : TColor;
                i : Integer;
            BEGIN
                case i of
                    0: color := 0;
                    1: color := 1;
                    2: color := 2;
                    Red: color:= 3;
                end;
            END.
            """
            )
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.SEMANTIC_INCOMPATIBLE_TYPE)
        self.assertEqual(the_exception.token.value, "i")

    def test_semantic_type_alias_refer_to_undefined_type(self):
        with self.assertRaises(SemanticError) as cm:
            self.runSemanticAnalyzer(
                """
            PROGRAM TypeAliasErrorTest;

            { Test error case: Reference to undefined type }
            TYPE
            StringAlias = String;
            BadAlias = NonExistentType;

            VAR
            x: BadAlias;

            BEGIN
            x := 42;
            END.
            """
            )
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.ID_NOT_FOUND)
        self.assertEqual(the_exception.token.value, "NonExistentType")


if __name__ == "__main__":
    unittest.main()
