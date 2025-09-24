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

    def test_subrange_type_parsing_simple(self):
        """Test parsing simple subrange types like 1..10"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            TYPE
                Range1 = 1..10;
                Range2 = 'A'..'Z';
            BEGIN
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

        # Check that we have type declarations
        type_decls = [
            decl for decl in tree.block.declarations if hasattr(decl, "type_def")
        ]
        self.assertEqual(len(type_decls), 2)

        # Check first subrange type (1..10)
        from spi.ast import SubrangeType, Num

        range1_decl = type_decls[0]
        self.assertIsInstance(range1_decl.type_def, SubrangeType)
        self.assertIsInstance(range1_decl.type_def.lower, Num)
        self.assertIsInstance(range1_decl.type_def.upper, Num)
        self.assertEqual(range1_decl.type_def.lower.value, 1)
        self.assertEqual(range1_decl.type_def.upper.value, 10)

        # Check second subrange type ('A'..'Z')
        from spi.ast import String

        range2_decl = type_decls[1]
        self.assertIsInstance(range2_decl.type_def, SubrangeType)
        self.assertIsInstance(range2_decl.type_def.lower, String)
        self.assertIsInstance(range2_decl.type_def.upper, String)
        self.assertEqual(range2_decl.type_def.lower.value, "A")
        self.assertEqual(range2_decl.type_def.upper.value, "Z")

    def test_subrange_type_parsing_expressions(self):
        """Test parsing subrange types with expressions"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            CONST
                MIN = 5;
                MAX = 15;
            TYPE
                Range = MIN..MAX;
            BEGIN
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

        # Check that we have type declarations
        type_decls = [
            decl for decl in tree.block.declarations if hasattr(decl, "type_def")
        ]
        self.assertEqual(len(type_decls), 1)

        # Check subrange type with variable expressions
        from spi.ast import SubrangeType, Var

        range_decl = type_decls[0]
        self.assertIsInstance(range_decl.type_def, SubrangeType)
        self.assertIsInstance(range_decl.type_def.lower, Var)
        self.assertIsInstance(range_decl.type_def.upper, Var)
        self.assertEqual(range_decl.type_def.lower.value, "MIN")
        self.assertEqual(range_decl.type_def.upper.value, "MAX")

    def test_subrange_type_parsing_in_variable_declaration(self):
        """Test parsing subrange types in variable declarations"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                count : 1..100;
                grade : 0..10;
            BEGIN
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

        # Check that we have variable declarations with subrange types
        var_decls = [
            decl for decl in tree.block.declarations if hasattr(decl, "var_node")
        ]
        self.assertEqual(len(var_decls), 2)

        # Check first variable with subrange type
        from spi.ast import SubrangeType

        count_decl = var_decls[0]
        self.assertIsInstance(count_decl.type_node, SubrangeType)
        self.assertEqual(count_decl.type_node.lower.value, 1)
        self.assertEqual(count_decl.type_node.upper.value, 100)

        # Check second variable with subrange type
        grade_decl = var_decls[1]
        self.assertIsInstance(grade_decl.type_node, SubrangeType)
        self.assertEqual(grade_decl.type_node.lower.value, 0)
        self.assertEqual(grade_decl.type_node.upper.value, 10)

    def test_array_type_uses_subrange_bounds(self):
        """Test that array types now use SubrangeType for bounds"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                arr1 : ARRAY[1..10] OF INTEGER;
                arr2 : ARRAY OF INTEGER;
            BEGIN
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

        # Check that we have variable declarations
        var_decls = [
            decl for decl in tree.block.declarations if hasattr(decl, "var_node")
        ]
        self.assertEqual(len(var_decls), 2)

        # Check first array with bounds
        from spi.ast import ArrayType, SubrangeType, Num

        arr1_decl = var_decls[0]
        self.assertIsInstance(arr1_decl.type_node, ArrayType)
        self.assertFalse(arr1_decl.type_node.dynamic)
        self.assertIsNotNone(arr1_decl.type_node.bounds)
        self.assertIsInstance(arr1_decl.type_node.bounds, SubrangeType)

        # Verify bounds values through SubrangeType
        bounds = arr1_decl.type_node.bounds
        self.assertIsInstance(bounds.lower, Num)
        self.assertIsInstance(bounds.upper, Num)
        self.assertEqual(bounds.lower.value, 1)
        self.assertEqual(bounds.upper.value, 10)

        # Verify backward compatibility properties
        self.assertEqual(arr1_decl.type_node.lower.value, 1)
        self.assertEqual(arr1_decl.type_node.upper.value, 10)

        # Check second array (dynamic)
        arr2_decl = var_decls[1]
        self.assertIsInstance(arr2_decl.type_node, ArrayType)
        self.assertTrue(arr2_decl.type_node.dynamic)
        self.assertIsNone(arr2_decl.type_node.bounds)

        # Verify backward compatibility for dynamic arrays
        self.assertEqual(arr2_decl.type_node.lower.value, 0)
        self.assertEqual(arr2_decl.type_node.upper.value, 0)

    def test_set_literal_parsing_empty_set(self):
        """Test parsing empty set literal []"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                s : INTEGER;
            BEGIN
                s := [];
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

        # Get the assignment statement
        assignment = tree.block.compound_statement.children[0]
        from spi.ast import Assign, SetLiteral

        self.assertIsInstance(assignment, Assign)
        self.assertIsInstance(assignment.right, SetLiteral)

        # Check empty set
        set_literal = assignment.right
        self.assertEqual(len(set_literal.elements), 0)

    def test_set_literal_parsing_individual_elements(self):
        """Test parsing set literal with individual elements [1, 3, 5]"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                s : INTEGER;
            BEGIN
                s := [1, 3, 5];
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

        # Get the assignment statement
        assignment = tree.block.compound_statement.children[0]
        from spi.ast import Assign, SetLiteral, Num

        self.assertIsInstance(assignment, Assign)
        self.assertIsInstance(assignment.right, SetLiteral)

        # Check set elements
        set_literal = assignment.right
        self.assertEqual(len(set_literal.elements), 3)

        # Verify individual elements
        self.assertIsInstance(set_literal.elements[0], Num)
        self.assertIsInstance(set_literal.elements[1], Num)
        self.assertIsInstance(set_literal.elements[2], Num)
        self.assertEqual(set_literal.elements[0].value, 1)
        self.assertEqual(set_literal.elements[1].value, 3)
        self.assertEqual(set_literal.elements[2].value, 5)

    def test_set_literal_parsing_range_elements(self):
        """Test parsing set literal with range elements [1..5, 8..10]"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                s : INTEGER;
            BEGIN
                s := [1..5, 8..10];
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

        # Get the assignment statement
        assignment = tree.block.compound_statement.children[0]
        from spi.ast import Assign, SetLiteral, SubrangeType, Num

        self.assertIsInstance(assignment, Assign)
        self.assertIsInstance(assignment.right, SetLiteral)

        # Check set elements
        set_literal = assignment.right
        self.assertEqual(len(set_literal.elements), 2)

        # Verify first range element (1..5)
        self.assertIsInstance(set_literal.elements[0], SubrangeType)
        range1 = set_literal.elements[0]
        self.assertIsInstance(range1.lower, Num)
        self.assertIsInstance(range1.upper, Num)
        self.assertEqual(range1.lower.value, 1)
        self.assertEqual(range1.upper.value, 5)

        # Verify second range element (8..10)
        self.assertIsInstance(set_literal.elements[1], SubrangeType)
        range2 = set_literal.elements[1]
        self.assertIsInstance(range2.lower, Num)
        self.assertIsInstance(range2.upper, Num)
        self.assertEqual(range2.lower.value, 8)
        self.assertEqual(range2.upper.value, 10)

    def test_set_literal_parsing_mixed_elements(self):
        """Test parsing set literal with mixed individual and range elements [1, 3..5, 8, 10..12]"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                s : INTEGER;
            BEGIN
                s := [1, 3..5, 8, 10..12];
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

        # Get the assignment statement
        assignment = tree.block.compound_statement.children[0]
        from spi.ast import Assign, SetLiteral, SubrangeType, Num

        self.assertIsInstance(assignment, Assign)
        self.assertIsInstance(assignment.right, SetLiteral)

        # Check set elements
        set_literal = assignment.right
        self.assertEqual(len(set_literal.elements), 4)

        # Verify first element (1)
        self.assertIsInstance(set_literal.elements[0], Num)
        self.assertEqual(set_literal.elements[0].value, 1)

        # Verify second element (3..5)
        self.assertIsInstance(set_literal.elements[1], SubrangeType)
        range1 = set_literal.elements[1]
        self.assertEqual(range1.lower.value, 3)
        self.assertEqual(range1.upper.value, 5)

        # Verify third element (8)
        self.assertIsInstance(set_literal.elements[2], Num)
        self.assertEqual(set_literal.elements[2].value, 8)

        # Verify fourth element (10..12)
        self.assertIsInstance(set_literal.elements[3], SubrangeType)
        range2 = set_literal.elements[3]
        self.assertEqual(range2.lower.value, 10)
        self.assertEqual(range2.upper.value, 12)

    def test_set_literal_parsing_character_elements(self):
        """Test parsing set literal with character elements ['A', 'C'..'F', 'Z']"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                s : CHAR;
            BEGIN
                s := ['A', 'C'..'F', 'Z'];
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

        # Get the assignment statement
        assignment = tree.block.compound_statement.children[0]
        from spi.ast import Assign, SetLiteral, SubrangeType, String

        self.assertIsInstance(assignment, Assign)
        self.assertIsInstance(assignment.right, SetLiteral)

        # Check set elements
        set_literal = assignment.right
        self.assertEqual(len(set_literal.elements), 3)

        # Verify first element ('A')
        self.assertIsInstance(set_literal.elements[0], String)
        self.assertEqual(set_literal.elements[0].value, "A")

        # Verify second element ('C'..'F')
        self.assertIsInstance(set_literal.elements[1], SubrangeType)
        range1 = set_literal.elements[1]
        self.assertIsInstance(range1.lower, String)
        self.assertIsInstance(range1.upper, String)
        self.assertEqual(range1.lower.value, "C")
        self.assertEqual(range1.upper.value, "F")

        # Verify third element ('Z')
        self.assertIsInstance(set_literal.elements[2], String)
        self.assertEqual(set_literal.elements[2].value, "Z")

    def test_in_operator_parsing_with_set_literal(self):
        """Test parsing in operator with set literal"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                x : INTEGER;
                result : BOOLEAN;
            BEGIN
                result := x in [1, 3, 5];
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

        # Get the assignment statement
        compound = tree.block.compound_statement
        assignment = compound.children[0]
        
        from spi.ast import InOperator, Var, SetLiteral
        
        # Check that the right side is an InOperator
        self.assertIsInstance(assignment.right, InOperator)
        in_op = assignment.right
        
        # Check left operand (value being tested)
        self.assertIsInstance(in_op.value, Var)
        self.assertEqual(in_op.value.value, "x")
        
        # Check right operand (set literal)
        self.assertIsInstance(in_op.set_expr, SetLiteral)
        set_literal = in_op.set_expr
        self.assertEqual(len(set_literal.elements), 3)

    def test_in_operator_parsing_with_subrange(self):
        """Test parsing in operator with subrange"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                x : INTEGER;
                result : BOOLEAN;
            BEGIN
                result := x in 1..10;
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

        # Get the assignment statement
        compound = tree.block.compound_statement
        assignment = compound.children[0]
        
        from spi.ast import InOperator, Var, SubrangeType
        
        # Check that the right side is an InOperator
        self.assertIsInstance(assignment.right, InOperator)
        in_op = assignment.right
        
        # Check left operand (value being tested)
        self.assertIsInstance(in_op.value, Var)
        self.assertEqual(in_op.value.value, "x")
        
        # Check right operand (subrange)
        self.assertIsInstance(in_op.set_expr, SubrangeType)

    def test_in_operator_parsing_with_variable(self):
        """Test parsing in operator with variable"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                x : INTEGER;
                mySet : ARRAY[1..10] OF INTEGER;
                result : BOOLEAN;
            BEGIN
                result := x in mySet;
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

        # Get the assignment statement
        compound = tree.block.compound_statement
        assignment = compound.children[0]
        
        from spi.ast import InOperator, Var
        
        # Check that the right side is an InOperator
        self.assertIsInstance(assignment.right, InOperator)
        in_op = assignment.right
        
        # Check left operand (value being tested)
        self.assertIsInstance(in_op.value, Var)
        self.assertEqual(in_op.value.value, "x")
        
        # Check right operand (variable)
        self.assertIsInstance(in_op.set_expr, Var)
        self.assertEqual(in_op.set_expr.value, "mySet")

    def test_in_operator_parsing_in_if_statement(self):
        """Test parsing in operator in if statement"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                x : INTEGER;
            BEGIN
                IF x in [1, 2, 3] THEN
                    x := 0;
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

        # Get the if statement
        compound = tree.block.compound_statement
        if_stmt = compound.children[0]
        
        from spi.ast import IfStatement, InOperator, Var, SetLiteral
        
        # Check that it's an if statement
        self.assertIsInstance(if_stmt, IfStatement)
        
        # Check that the condition is an InOperator
        self.assertIsInstance(if_stmt.condition, InOperator)
        in_op = if_stmt.condition
        
        # Check operands
        self.assertIsInstance(in_op.value, Var)
        self.assertEqual(in_op.value.value, "x")
        self.assertIsInstance(in_op.set_expr, SetLiteral)

    def test_in_operator_parsing_complex_expression(self):
        """Test parsing in operator with complex expressions"""
        parser = self.makeParser(
            """
            PROGRAM Test;
            VAR
                x, y : INTEGER;
                result : BOOLEAN;
            BEGIN
                result := (x + y) in [1..5, 10, 15..20];
            END.
            """
        )
        tree = parser.parse()
        self.assertIsNotNone(tree)

        # Get the assignment statement
        compound = tree.block.compound_statement
        assignment = compound.children[0]
        
        from spi.ast import InOperator, BinOp, SetLiteral
        
        # Check that the right side is an InOperator
        self.assertIsInstance(assignment.right, InOperator)
        in_op = assignment.right
        
        # Check left operand is a binary operation (x + y)
        self.assertIsInstance(in_op.value, BinOp)
        
        # Check right operand is a set literal
        self.assertIsInstance(in_op.set_expr, SetLiteral)
        set_literal = in_op.set_expr
        self.assertEqual(len(set_literal.elements), 3)  # [1..5, 10, 15..20]


if __name__ == "__main__":
    unittest.main()
