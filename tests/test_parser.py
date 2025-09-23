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

    def test_multi_VAR_block_is_allowed(self):
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
        parser.parse()

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

    def test_forward_procedure_decl_should_not_have_block(self):
        parser = self.makeParser(
            """
            program ForwardInvalidExample;

            procedure Proc1(); forward;
            begin
            end;
            
            begin
            end;

            """
        )
        with self.assertRaises(ParserError) as cm:
            parser.parse()
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.PARSER_UNEXPECTED_TOKEN)
        self.assertEqual(the_exception.token.value, "BEGIN")
        self.assertEqual(the_exception.token.lineno, 5)

    def test_forward_procedure_valid(self):
        parser = self.makeParser(
            """
        program ForwardSafeExample;

        procedure Proc1(); forward;

        procedure Proc1();
        begin
        end;

        { 主程序 }
        begin
            Proc1();
        end.

            """
        )
        parser.parse()

    def test_forward_function_decl_should_not_have_block(self):
        parser = self.makeParser(
            """
            program ForwardFunction;

            function Add(a, b: Integer): Integer; forward; 
            begin
            end;

            function Add(a, b: Integer): Integer;
            begin
            Add := a + b;
            end;

            begin
            end.

            """
        )
        with self.assertRaises(ParserError) as cm:
            parser.parse()
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.PARSER_UNEXPECTED_TOKEN)
        self.assertEqual(the_exception.token.value, "BEGIN")
        self.assertEqual(the_exception.token.lineno, 5)

    def test_forward_function_valid(self):
        parser = self.makeParser(
            """
            program ForwardFunction;

            function Add(a, b: Integer): Integer; forward; 

            function Add(a, b: Integer): Integer;
            begin
            Add := a + b;
            end;

            begin
            end.

            """
        )
        parser.parse()

    def test_const_declaration_parsing(self):
        """Test that const declarations are parsed correctly"""
        parser = self.makeParser(
            """
            PROGRAM TestConst;
            CONST
                PI = 3.14159;
                MAX_SIZE = 100;
            VAR
                x : INTEGER;
            BEGIN
                x := MAX_SIZE;
            END.
            """
        )
        # Should parse without errors
        tree = parser.parse()
        self.assertIsNotNone(tree)

    def test_const_and_var_declarations_together(self):
        """Test that const and var declarations can coexist"""
        parser = self.makeParser(
            """
            PROGRAM TestConstVar;
            CONST
                PI = 3.14159;
            VAR
                radius : REAL;
                area : REAL;
            BEGIN
                radius := 5.0;
                area := PI * radius * radius;
            END.
            """
        )
        # Should parse without errors
        tree = parser.parse()
        self.assertIsNotNone(tree)

    def test_const_parameter_parsing(self):
        """Test that const parameters are parsed correctly"""
        parser = self.makeParser(
            """
            PROGRAM TestConstParams;
            VAR
                x, y : INTEGER;
            
            PROCEDURE TestProc(CONST a : INTEGER; VAR b : INTEGER; c : INTEGER);
            BEGIN
                b := a + c;
            END;
            
            BEGIN
                TestProc(x, y, 5);
            END.
            """
        )
        # Should parse without errors
        tree = parser.parse()
        self.assertIsNotNone(tree)

    def test_function_const_parameter_parsing(self):
        """Test that const parameters in functions are parsed correctly"""
        parser = self.makeParser(
            """
            PROGRAM TestFuncConstParams;
            VAR
                result : INTEGER;
            
            FUNCTION TestFunc(CONST a : INTEGER; b : INTEGER) : INTEGER;
            BEGIN
                TestFunc := a * b;
            END;
            
            BEGIN
                result := TestFunc(5, 10);
            END.
            """
        )
        # Should parse without errors
        tree = parser.parse()
        self.assertIsNotNone(tree)

    def test_mixed_parameter_modes_parsing(self):
        """Test that mixed parameter modes (const, var, value) are parsed correctly"""
        parser = self.makeParser(
            """
            PROGRAM TestMixedParams;
            VAR
                x, y, z : INTEGER;
            
            PROCEDURE MixedProc(CONST param1 : INTEGER; VAR param2 : INTEGER; param3 : INTEGER);
            BEGIN
                param2 := param1 + param3;
            END;
            
            FUNCTION MixedFunc(CONST a : INTEGER; VAR b : INTEGER) : INTEGER;
            BEGIN
                b := a * 2;
                MixedFunc := a + b;
            END;
            
            BEGIN
                MixedProc(x, y, z);
            END.
            """
        )
        # Should parse without errors
        tree = parser.parse()
        self.assertIsNotNone(tree)

    def test_break_statement_parsing(self):
        """Test that break statements are parsed correctly"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                i : INTEGER;
            BEGIN
                FOR i := 1 TO 10 DO
                BEGIN
                    IF i = 5 THEN
                        break;
                END;
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

    def test_while_statement_with_one_statement(self):
        """Test that while statements are parsed correctly"""
        parser = self.makeParser(
            """
            program WhileWithOneStatement;

            var 
                i : Integer;

            begin
                i := 0;
                while i < 10 do 
                    i := i + 1;
            end.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

    def test_if_statement_with_one_statement(self):
        """Test that if statements are parsed correctly"""
        parser = self.makeParser(
            """
            program forLoopWithOnlyOneStatement;
            var
            i: integer;
            sum : integer;

            begin
            for i := 1 to 10 do
                sum := sum + i;
            end.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

    def test_continue_statement_parsing(self):
        """Test that continue statements are parsed correctly"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                i : INTEGER;
            BEGIN
                FOR i := 1 TO 10 DO
                BEGIN
                    IF i = 5 THEN
                        continue;
                END;
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

    def test_break_statement_in_while_loop(self):
        """Test break statement in while loop"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                i : INTEGER;
            BEGIN
                i := 1;
                WHILE i <= 10 DO
                BEGIN
                    IF i = 5 THEN
                        break;
                    i := i + 1;
                END;
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

    def test_continue_statement_in_while_loop(self):
        """Test continue statement in while loop"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                i : INTEGER;
            BEGIN
                i := 1;
                WHILE i <= 10 DO
                BEGIN
                    IF i = 5 THEN
                    BEGIN
                        i := i + 1;
                        continue;
                    END;
                    i := i + 1;
                END;
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

    def test_break_statement_missing_semicolon_between_statements(self):
        """Test that break statement without semicolon between statements raises error"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                i : INTEGER;
            BEGIN
                FOR i := 1 TO 10 DO
                BEGIN
                    break
                    i := i + 1;
                END;
            END.
            """
        )
        with self.assertRaises(ParserError) as cm:
            parser.parse()
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.UNEXPECTED_TOKEN)

    def test_continue_statement_missing_semicolon_between_statements(self):
        """Test that continue statement without semicolon between statements raises error"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                i : INTEGER;
            BEGIN
                FOR i := 1 TO 10 DO
                BEGIN
                    continue
                    i := i + 1;
                END;
            END.
            """
        )
        with self.assertRaises(ParserError) as cm:
            parser.parse()
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.UNEXPECTED_TOKEN)

    def test_nested_loops_with_break_continue(self):
        """Test break and continue in nested loops"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                i, j : INTEGER;
            BEGIN
                FOR i := 1 TO 3 DO
                BEGIN
                    FOR j := 1 TO 3 DO
                    BEGIN
                        IF j = 2 THEN
                            break;
                        IF i = 2 THEN
                            continue;
                    END;
                END;
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

    def test_break_continue_ast_node_creation(self):
        """Test that break and continue statements create correct AST nodes"""
        from spi.ast import BreakStatement, ContinueStatement, ForStatement, IfStatement

        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                i : INTEGER;
            BEGIN
                FOR i := 1 TO 10 DO
                BEGIN
                    IF i = 5 THEN
                        break;
                    IF i = 3 THEN
                        continue;
                END;
            END.
            """
        )
        tree = parser.parse()

        # Navigate to the FOR statement
        for_stmt = tree.block.compound_statement.children[0]
        self.assertIsInstance(for_stmt, ForStatement)

        # Navigate to the compound statement inside the FOR loop
        compound_stmt = for_stmt.block

        # Check the first IF statement contains a break
        first_if = compound_stmt.children[0]
        self.assertIsInstance(first_if, IfStatement)
        break_stmt = first_if.then_branch
        self.assertIsInstance(break_stmt, BreakStatement)

        # Check the second IF statement contains a continue
        second_if = compound_stmt.children[1]
        self.assertIsInstance(second_if, IfStatement)
        continue_stmt = second_if.then_branch
        self.assertIsInstance(continue_stmt, ContinueStatement)

    def test_array_range_with_expression(self):
        """Test break and continue in nested loops"""
        parser = self.makeParser(
            """
            program ArrayLength;

            const 
                LOW = 1;
                HIGH = 5;

            var
                arr: array[LOW..HIGH - 1] of Integer; 
            i: Integer;
            begin
                i := Length(arr);
            end.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)


if __name__ == "__main__":
    unittest.main()
