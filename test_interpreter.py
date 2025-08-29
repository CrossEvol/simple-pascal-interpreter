import unittest


class LexerTestCase(unittest.TestCase):
    def makeLexer(self, text):
        from src.lexer import Lexer

        lexer = Lexer(text)
        return lexer

    def test_tokens(self):
        from src.spi_token import TokenType

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
            ("..", TokenType.RANGE, ".."),
            ("STRING", TokenType.STRING, "STRING"),
            ("'abc'", TokenType.STRING_CONST, "abc"),
            ("TYPE", TokenType.TYPE, "TYPE"),
            ("CLASS", TokenType.CLASS, "CLASS"),
            ("PRIVATE", TokenType.PRIVATE, "PRIVATE"),
            ("PUBLIC", TokenType.PUBLIC, "PUBLIC"),
            ("CONSTRUCTOR", TokenType.CONSTRUCTOR, "CONSTRUCTOR"),
            ("_this_self", TokenType.ID, "_this_self"),
            ("RECORD", TokenType.RECORD, "RECORD"),
            ("CONST", TokenType.CONST, "CONST"),
            ("CASE", TokenType.CASE, "CASE"),
            ("UNIT", TokenType.UNIT, "UNIT"),
            ("unit", TokenType.UNIT, "UNIT"),
            ("INTERFACE", TokenType.INTERFACE, "INTERFACE"),
            ("interface", TokenType.INTERFACE, "INTERFACE"),
            ("IMPLEMENTATION", TokenType.IMPLEMENTATION, "IMPLEMENTATION"),
            ("implementation", TokenType.IMPLEMENTATION, "IMPLEMENTATION"),
            ("USES", TokenType.USES, "USES"),
            ("uses", TokenType.USES, "USES"),
        )
        for text, tok_type, tok_val in records:
            lexer = self.makeLexer(text)
            token = lexer.get_next_token()
            self.assertEqual(token.type, tok_type)
            self.assertEqual(token.value, tok_val)

    def test_lexer_exception(self):
        from src.error import LexerError

        lexer = self.makeLexer("!")
        with self.assertRaises(LexerError):
            lexer.get_next_token()


class ParserTestCase(unittest.TestCase):
    def makeParser(self, text):
        from src.lexer import Lexer
        from src.parser import Parser

        lexer = Lexer(text)
        parser = Parser(lexer)
        return parser

    def test_expression_invalid_syntax_01(self):
        from src.error import ErrorCode, ParserError

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
        from src.error import ErrorCode, ParserError

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

    def test_real_const_is_invalid_case(self):
        from src.error import InvalidCaseStatementError, ParserError

        parser = self.makeParser(
            """
            program checkCase;
            var
            grade: real;
            begin
                grade := 1.0;
                case (grade) of
                    1.1 : writeln('Excellent!' );
                    1.2, 'C': writeln('Well done' );
                    1.3 : writeln('You passed' );
                else
                    writeln('You really did not study right!' );
                end; 
            end.
            """
        )
        with self.assertRaises(ParserError) as cm:
            parser.parse()
        the_exception = cm.exception
        self.assertIsInstance(the_exception, InvalidCaseStatementError)

    def test_maximum_one_VAR_block_is_allowed(self):
        from src.error import ParserError, VarDuplicateInScopeError

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
        self.assertIsInstance(the_exception, VarDuplicateInScopeError)


class SemanticAnalyzerTestCase(unittest.TestCase):
    def runSemanticAnalyzer(self, text):
        from src.lexer import Lexer
        from src.parser import Parser
        from src.sematic_analyzer import SemanticAnalyzer

        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()

        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.visit(tree)
        return semantic_analyzer

    def test_semantic_duplicate_id_error(self):
        from src.error import ErrorCode, SemanticError

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
        from src.error import ErrorCode, SemanticError

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

    def test_const_can_not_modified(self):
        from src.error import ErrorCode, SemanticError

        with self.assertRaises(SemanticError) as cm:
            self.runSemanticAnalyzer(
                """
            program const_example;

            const
            PI = 3.14;

            begin
            PI := 5.2;
            end.
            """
            )
        the_exception = cm.exception
        self.assertEqual(the_exception.error_code, ErrorCode.MODIFY_CONST_NOT_ALLOW)
        self.assertEqual(the_exception.token.value, "PI")
        self.assertEqual(the_exception.token.lineno, 8)

    def test_semantic_id_not_found_error(self):
        from src.error import ErrorCode, SemanticError

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


class TestCallStack:
    def __init__(self):
        self._records = []

    def push(self, ar):
        self._records.append(ar)

    def pop(self):
        # do nothing
        pass

    def peek(self):
        return self._records[-1]


class InterpreterTestCase(unittest.TestCase):
    def makeInterpreter(self, text):
        from src.interpreter import Interpreter
        from src.lexer import Lexer
        from src.parser import Parser
        from src.sematic_analyzer import SemanticAnalyzer

        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()

        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.visit(tree)

        interpreter = Interpreter(tree)
        interpreter.call_stack = TestCallStack()
        return interpreter

    def test_integer_arithmetic_expressions(self):
        for expr, result in (
            ("3", 3),
            ("2 + 7 * 4", 30),
            ("7 - 8 DIV 4", 5),
            ("14 + 2 * 3 - 6 DIV 2", 17),
            ("7 + 3 * (10 DIV (12 DIV (3 + 1) - 1))", 22),
            ("7 + 3 * (10 DIV (12 DIV (3 + 1) - 1)) DIV (2 + 3) - 5 - 3 + (8)", 10),
            ("7 + (((3 + 2)))", 12),
            ("- 3", -3),
            ("+ 3", 3),
            ("5 - - - + - 3", 8),
            ("5 - - - + - (3 + 4) - +2", 10),
        ):
            interpreter = self.makeInterpreter(
                """PROGRAM Test;
                   VAR
                       a : INTEGER;
                   BEGIN
                       a := %s
                   END.
                """
                % expr
            )
            interpreter.interpret()
            ar = interpreter.call_stack.peek()
            self.assertEqual(ar["a"].value, result)

    def test_float_arithmetic_expressions(self):
        for expr, result in (
            ("3.14", 3.14),
            ("2.14 + 7 * 4", 30.14),
            ("7.14 - 8 / 4", 5.14),
        ):
            interpreter = self.makeInterpreter(
                """PROGRAM Test;
                   VAR
                       a : REAL;
                   BEGIN
                       a := %s
                   END.
                """
                % expr
            )
            interpreter.interpret()
            ar = interpreter.call_stack.peek()
            self.assertEqual(ar["a"].value, result)

    def test_boolean_expressions(self):
        for expr, result in (
            ("true", True),
            ("false", False),
            ("true and false", False),
            ("true and true", True),
            ("true or false", True),
            ("not true", False),
            ("not false", True),
            ("not true and not true", False),
            ("1 < 2", True),
            ("1 > 2", False),
            ("1 = 2", False),
            ("1 <> 2", True),
            ("1 <= 2", True),
            ("1 >= 2", False),
            ("1 < 2 and 2 < 3", True),
            ("1 < 2 and 2 > 3", False),
            ("1 < 2 or 2 > 3", True),
        ):
            interpreter = self.makeInterpreter(
                """PROGRAM Test;
                   VAR
                       flag : BOOLEAN;
                   BEGIN
                       flag := %s
                   END.
                """
                % expr
            )
            interpreter.interpret()
            ar = interpreter.call_stack.peek()
            self.assertEqual(ar["flag"].value, result)

    def test_default_value_for_type(self):
        text = """\
program DefaultValue;
var
  num: Integer;
  bool: Boolean;
begin {Main}
end. {Main}
"""
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["num"].value, 0)
        self.assertEqual(ar["bool"].value, False)
        self.assertEqual(ar.nesting_level, 1)

    def test_procedure_call(self):
        text = """\
program Main;

procedure Alpha(a : integer; b : integer);
var x : integer;
begin
   x := (a + b ) * 2;
end;

begin { Main }

   Alpha(3 + 5, 7);

end.  { Main }
"""
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["a"].value, 8)
        self.assertEqual(ar["b"].value, 7)
        self.assertEqual(ar["x"].value, 30)
        self.assertEqual(ar.nesting_level, 2)

    def test_comparison_calculus(self):
        text = """\
program ComparisonTest;
var 
  a , b : integer;
  f1, f2, f3, f4, f5, f6 : Boolean;
begin {Main}
  a := 1;
  b := 2;
  f1:= a < b; {TRUE}
  f2:= a > b; {FALSE}
  f3:= a = b; {FALSE}
  f4:= a <> b; {TRUE}
  f5:= a <= b; {TRUE}
  f6:= a >= b; {FALSE}
end. {Main}
"""
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["a"].value, 1)
        self.assertEqual(ar["b"].value, 2)
        self.assertEqual(ar["f1"].value, True)
        self.assertEqual(ar["f2"].value, False)
        self.assertEqual(ar["f3"].value, False)
        self.assertEqual(ar["f4"].value, True)
        self.assertEqual(ar["f5"].value, True)
        self.assertEqual(ar["f6"].value, False)
        self.assertEqual(ar.nesting_level, 1)

    def test_logic_calculus(self):
        text = """\
program LogicCalculus;
var 
  a , b, f1, f2, f3, f4: BOOLEAN;
begin {Main}
  a := TRUE;
  b := FALSE;
  f1 := a and b; {FALSE}
  f2 := a or b; {TRUE}
  f3 := not a; {FALSE}
  f4 := not b; {TRUE}
end. {Main}
"""
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["a"].value, True)
        self.assertEqual(ar["b"].value, False)
        self.assertEqual(ar["f1"].value, False)
        self.assertEqual(ar["f2"].value, True)
        self.assertEqual(ar["f3"].value, False)
        self.assertEqual(ar["f4"].value, True)
        self.assertEqual(ar.nesting_level, 1)

    def test_function_call(self):
        text = """\
program SimpleFunction;
var
  sum: Integer;
function Add(a, b: Integer): Integer;
begin {ADD}
  Add := a + b;
end; {ADD}

begin {Main}
  sum := Add(5, 3);
end. {Main}
"""
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["sum"].value, 8)
        self.assertEqual(ar.nesting_level, 2)

    def test_inner_ref_outer_var(self):
        text = """\
program Main;
var x : integer;
procedure Alpha();
   procedure Beta();
   begin
      x := x +  20;
   end;
begin
   x := 10;
   Beta();      { procedure call }
end;
begin { Main }
   Alpha();  { procedure call }
end.  { Main }
"""
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["x"].value, 30)
        self.assertEqual(ar.nesting_level, 3)

    def test_write_and_writeln(self):
        text = """\
program HackOutput;
var 
  a : integer;
  flag : Boolean;
begin {Main}

  flag := false;
  writeln(flag);
  flag := true;
  writeln(flag);

  a := 6;
  WRITELN(5);
  Writeln(5);
  writeln(5);
  Writeln(a);
  WRITE(5);
  
end. {Main}
"""
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["a"].value, 6)
        self.assertEqual(ar["flag"].value, True)
        self.assertEqual(ar.nesting_level, 2)

    def test_write_and_writeln_inside_function(self):
        text = """\
program SimpleFunction;
var 
  a : integer;

function sayHello(b: integer):Integer;
begin
  sayHello := a+b;
  Writeln(sayHello);
end;

begin {Main}
  a := 1;          { Initialize the variable 'a' within the main block }
  a := sayHello(2);     { Call the function 'sayHello' with argument 7 }
end. {Main}
"""
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["a"].value, 3)
        self.assertEqual(ar.nesting_level, 2)

    def test_set_length_builtin_procedure(self):
        text = """\
program ArraySetLength;
var
    arr: array of Integer;
    i, j, k: Integer;   
begin
    i := Length(arr);
    setLength(arr,10);
    j := Length(arr);
    for k := 1 to 10 do
        arr[k-1] := k;
    setLength(arr,5);
end.
    """
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["i"].value, 0)
        self.assertEqual(ar["j"].value, 10)
        self.assertEqual(ar["k"].value, 10)
        for i in range(0, 5):
            self.assertEqual(ar["arr"].elements[i].value, i + 1)
        self.assertEqual(ar.nesting_level, 2)

    def test_length_builtin_function(self):
        text = """\
program ArrayLength;
var
  arr: array[1..5] of Integer; 
  i: Integer;
begin
  i := Length(arr);
end.
    """
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["i"].value, 5)
        self.assertEqual(ar.nesting_level, 2)

    def test_if_then(self):
        for expr, result in ((10, -1), (21, 1)):
            text = (
                """\
            program IfThen;
            var
            a,b:integer;
            begin
            a:= %d;
            b:= 1;
            if a < 20 then
                b := -1;
            end.
        """
                % expr
            )
            interpreter = self.makeInterpreter(text)
            interpreter.interpret()
            ar = interpreter.call_stack.peek()

            self.assertEqual(ar["a"].value, expr)
            self.assertEqual(ar["b"].value, result)
            self.assertEqual(ar.nesting_level, 1)

    def test_if_else(self):
        for expr, result in ((10, -1), (21, 1)):
            text = (
                """\
    program ifelseChecking;
    var
    a , b : integer;
    begin
    a := %d;
    if a < 20  then
        b:= -1
    else
        b:= 1;
    end.
    """
                % expr
            )
            interpreter = self.makeInterpreter(text)
            interpreter.interpret()
            ar = interpreter.call_stack.peek()

            self.assertEqual(ar["a"].value, expr)
            self.assertEqual(ar["b"].value, result)
            self.assertEqual(ar.nesting_level, 1)

    def test_if_nest(self):
        text = """\
program NestedIfElseChecking;
var
   a, b, c : integer;
   flag : boolean;
begin
   a := 100;
   b := 200;
   c := 300;
   if a = 100 then
      if b = 200 then
        if c = 300 then
            flag := true
        else 
            flag := false
      else 
        flag := false
    else 
    flag := false;
end.
"""
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["a"].value, 100)
        self.assertEqual(ar["b"].value, 200)
        self.assertEqual(ar["c"].value, 300)
        self.assertEqual(ar["flag"].value, True)
        self.assertEqual(ar.nesting_level, 1)

    def test_if_block(self):
        for flag, a, b in ((True, 2, -1), (False, 1, -2)):
            text = (
                """\
    program IfBlockChecking;
    var
    a , b : integer;
    flag : boolean;
    begin
    flag := %s;
    a := 1;
    b := -1;
    if flag then
        begin 
            a := a+1
        end
    else
        begin 
            b := b-1;
        end;
    end.
    """
                % flag
            )
            interpreter = self.makeInterpreter(text)
            interpreter.interpret()
            ar = interpreter.call_stack.peek()

            self.assertEqual(ar["flag"].value, flag)
            self.assertEqual(ar["a"].value, a)
            self.assertEqual(ar["b"].value, b)
            self.assertEqual(ar.nesting_level, 1)

    def test_while_loop(self):
        text = """\
program whileLoop;
var
   a: integer;

begin
   a := 10;
   while  a < 20  do
   begin
      {writeln('value of a: ', a);}
      a := a + 1;
   end;
end.
"""
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["a"].value, 20)
        self.assertEqual(ar.nesting_level, 1)

    def test_for_loop(self):
        text = """\
program forLoop;
var
   a: integer;
   b: integer;
   sum : integer;

begin
   b := 10;
   for a := 1 to b do
   begin
      sum := sum + a;
   end;
   {writeln(a);}
   {writeln(b);}
   {writeln(sum);}
end.
"""
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["a"].value, 10)
        self.assertEqual(ar["sum"].value, 55)
        self.assertEqual(ar.nesting_level, 1)

    def test_array_low_high(self):
        text = """\
    program array_low_high;
    var
       n: array [-10..10] of integer;
       a, b : integer; 

    begin
        a := LOW(n);
        b := HIGH(n);
    end.
    """
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["a"].value, -10)
        self.assertEqual(ar["b"].value, 10)
        self.assertEqual(ar.nesting_level, 2)

    def test_array_initialized(self):
        text = """\
program exArrays;
var
    intArr: array [1..2] of integer;
    boolArr: array [1..2] of boolean;
    realArr: array [1..2] of real;
    negativeArr: array [-1..1] of integer;
    zeroArr : array of integer;
    nestArr : array [1..2] of array of integer;
begin
end.
    """
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        for i in range(1, 2):
            self.assertEqual(ar["intArr"].elements[i].value, 0)
            self.assertEqual(ar["boolArr"].elements[i].value, False)
            self.assertEqual(ar["realArr"].elements[i].value, 0.0)
        self.assertEqual(ar.nesting_level, 1)

    def test_array_range_invalid(self):
        from src.error import ArrayRangeInvalidError, InterpreterError

        text = """\
program ArranRange;
var
    validArr: array [1..2] of integer;
    invalidArr : array [2 .. -2] of integer;
begin
end.
    """
        with self.assertRaises(InterpreterError) as cm:
            interpreter = self.makeInterpreter(text)
            interpreter.interpret()

            ar = interpreter.call_stack.peek()
            self.assertEqual(ar.nesting_level, 1)
        self.assertIsInstance(cm.exception, ArrayRangeInvalidError)

    def test_static_array_modify_length(self):
        from src.error import InterpreterError, StaticArrayModifyLengthError

        text = """\
program ArranRange;
var
    arr: array [1..2] of integer;
begin
    setLength(arr,5);
end.
    """
        with self.assertRaises(InterpreterError) as cm:
            interpreter = self.makeInterpreter(text)
            interpreter.interpret()

            ar = interpreter.call_stack.peek()
            self.assertEqual(ar.nesting_level, 1)
        self.assertIsInstance(cm.exception, StaticArrayModifyLengthError)

    def test_array_out_of_range(self):
        text = """\
program ArranRange;
var
    intArr: array [1..2] of integer;
    boolArr: array [1..2] of boolean;
    realArr: array [1..2] of real;
    nestArr : array [1..2] of array of integer;
    a , b , c, d : integer;
begin
    a := intArr[100];
    b := boolArr[100];
    c := realArr[100];
    d := nestArr[100];
end.
    """
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["a"].value, 0)
        self.assertEqual(ar["b"].value, False)
        self.assertEqual(ar["c"].value, 0.0)
        self.assertEqual(len(ar["d"].elements), 0)  # Empty array
        self.assertEqual(ar.nesting_level, 1)

    def test_string(self):
        text = """\
program StringExample;
var
  str1: string[7]; {Declare a string wth a maximum size}
  str2 : string; {Declare a string without a maximum size}
  str3 : string; {will be used for setLength()}
  concat1 : string; {will be used for s1 + s2}
  concat2 : string[7]; {will be used for s1 + s2}
  a, b : string; {will use subscript to extra char from string}
  l1 , l2 , l3: integer;

begin
  str1 := 'abcdefghijklmnopqrstuvwxyz'; { will warn in sematic analyzer }
  str2 := 'abcdefghijklmnopqrstuvwxyz';
  str3 := str2;

  l1 := length(str1);
  l2 := length(str2);

  setLength(str3,14);
  l3 := length(str3);
  a := str3[1];
  b := str3[36];
  concat1 := str1 + '123' + '456' + str2[1];
  concat2 := str1 + '123' + '456' + str2[1]; { will warn in interpreter }
end.
    """
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["str1"].value, "abcdefg")
        self.assertEqual(ar["str2"].value, "abcdefghijklmnopqrstuvwxyz")
        self.assertEqual(ar["str3"].value, "abcdefghijklmn")
        self.assertEqual(ar["l1"].value, 7)
        self.assertEqual(ar["l2"].value, 26)
        self.assertEqual(ar["l3"].value, 14)
        self.assertEqual(ar["a"].value, "a")
        self.assertEqual(ar["b"].value, "")
        self.assertEqual(ar["concat1"].value, "abcdefg123456a")
        self.assertEqual(ar["concat2"].value, "abcdefg")
        self.assertEqual(ar.nesting_level, 2)

    def test_class(self):
        text = """\
program SimpleClass;

{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor

type
  TPerson = class
    private
      name: String;
      age: Integer;
    public
      constructor Create(aName: String; aAge: Integer);
      procedure SetData(aName: String; aAge: Integer);
      procedure PrintData;
      function getName:String;
      function getAge:Integer;
  end;

constructor TPerson.Create(aName: String; aAge: Integer);
begin
  name := aName;
  age := aAge;
end;

procedure TPerson.SetData(aName: String; aAge: Integer);
begin
  name := aName;
  age := aAge;
end;

procedure TPerson.PrintData;
begin
  WriteLn('Name: ', name);
  WriteLn('Age: ', age);
end;


function TPerson.getName:String;
begin
    getName := name;
end;

function TPerson.getAge:Integer;
begin
    getAge := age;
end;

var
  person : TPerson;
  person2 : TPerson;
  bName : String;
  bAge : INTEGER;
begin
  person := TPerson.Create('Alice', 30);
  bName := person.getName;
  bAge := person.getAge;
  person.PrintData;
  person2 := TPerson.Create('Alice', 30);
  person2.SetData('Tom',18);
end.
    """
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["person"].fields["name"].value, "Alice")
        self.assertEqual(ar["person"].fields["age"].value, 30)
        self.assertEqual(ar["bName"].value, "Alice")
        self.assertEqual(ar["bAge"].value, 30)
        self.assertEqual(ar["person2"].fields["name"].value, "Tom")
        self.assertEqual(ar["person2"].fields["age"].value, 18)
        self.assertEqual(ar.nesting_level, 3)

    def test_class_default_methods(self):
        text = """\
program ClassConstructorAndDestructor;

{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor

type
  TPerson = class
    private
      name: String;
      age: Integer;
    public
      procedure SetData(aName: String; aAge: Integer);
  end;

procedure TPerson.SetData(aName: String; aAge: Integer);
begin
  name := aName;
  age := aAge;
end;

var
  person: TPerson;
  person2: TPerson;
begin
  person := TPerson.Create;
  person.SetData('Tom', 18);

  person2 := TPerson.Create;
  person2.SetData('Tom', 18);

  person.Free;
end.
    """
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["person"].fields["name"].value, "")
        self.assertEqual(ar["person"].fields["age"].value, 0)
        self.assertEqual(ar["person2"].fields["name"].value, "Tom")
        self.assertEqual(ar["person2"].fields["age"].value, 18)
        self.assertEqual(ar.nesting_level, 3)

    def test_enum(self):
        text = """\
program EnumIndexExample;

{$mode objfpc}{$H+}

type
  TDayOfWeek = (Sun, Mon, Tue, Wed, Thu, Fri, Sat);

var
  d0, d1, d2, d3, d4, d5, d6 : TDayOfWeek;
  o0 ,o1,o2,o3,o4,o5,o6 : integer;
begin
  d0 := TDayOfWeek.Sun;
  d1 := TDayOfWeek.Mon;
  d2 := TDayOfWeek.Tue;
  d3 := TDayOfWeek.Wed;
  d4 := TDayOfWeek.Thu;
  d5 := TDayOfWeek.Fri;
  d6 := TDayOfWeek.Sat;
  o0 := Ord(d0);
  o1 := Ord(d1);
  o2 := Ord(d2);
  o3 := Ord(d3);
  o4 := Ord(d4);
  o5 := Ord(d5);
  o6 := Ord(d6);
end.
    """
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["d0"].name, "Sun")
        self.assertEqual(ar["d0"].index, 0)
        self.assertEqual(ar["d1"].name, "Mon")
        self.assertEqual(ar["d1"].index, 1)
        self.assertEqual(ar["d2"].name, "Tue")
        self.assertEqual(ar["d2"].index, 2)
        self.assertEqual(ar["d3"].name, "Wed")
        self.assertEqual(ar["d3"].index, 3)
        self.assertEqual(ar["d4"].name, "Thu")
        self.assertEqual(ar["d4"].index, 4)
        self.assertEqual(ar["d5"].name, "Fri")
        self.assertEqual(ar["d5"].index, 5)
        self.assertEqual(ar["d6"].name, "Sat")
        self.assertEqual(ar["d6"].index, 6)
        self.assertEqual(ar["o0"].value, 0)
        self.assertEqual(ar["o1"].value, 1)
        self.assertEqual(ar["o2"].value, 2)
        self.assertEqual(ar["o3"].value, 3)
        self.assertEqual(ar["o4"].value, 4)
        self.assertEqual(ar["o5"].value, 5)
        self.assertEqual(ar["o6"].value, 6)
        self.assertEqual(ar.nesting_level, 2)

    def test_record(self):
        text = """\
program RecordExample;

type
    User = record
        ID: Integer; {Integer field}
        Name: String[50]; {String field with a maximum length of 50 characters}
        Age: Integer;  {Integer field}
        Salary: Real; { Real number field}
        Active: Boolean;  {Boolean field}
        Scores: array[1..5] of Integer; { Array of integers with 5 elements}
    end;

var
    exampleUser: User;
    i , j: Integer;
    c : string;
begin
    exampleUser.ID := 1;
    exampleUser.Name := 'John Doe';
    exampleUser.Age := 30;
    exampleUser.Salary := 45000.50;
    exampleUser.Active := True;
    exampleUser.Scores[1] := 85;
    exampleUser.Scores[2] := 90;
    exampleUser.Scores[3] := 78;
    exampleUser.Scores[4] := 92;
    exampleUser.Scores[5] := 88;
    i := length(exampleUser.Scores);
    j := exampleUser.Scores[6];
    c := exampleUser.Name[1];
end.
    """
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["exampleUser"].fields["ID"].value, 1)
        self.assertEqual(ar["exampleUser"].fields["Name"].value, "John Doe")
        self.assertEqual(ar["exampleUser"].fields["Age"].value, 30)
        self.assertEqual(ar["exampleUser"].fields["Salary"].value, 45000.50)
        self.assertEqual(ar["exampleUser"].fields["Active"].value, True)
        self.assertEqual(ar["exampleUser"].fields["Scores"].elements[1].value, 85)
        self.assertEqual(ar["exampleUser"].fields["Scores"].elements[2].value, 90)
        self.assertEqual(ar["exampleUser"].fields["Scores"].elements[3].value, 78)
        self.assertEqual(ar["exampleUser"].fields["Scores"].elements[4].value, 92)
        self.assertEqual(ar["exampleUser"].fields["Scores"].elements[5].value, 88)
        self.assertEqual(ar["i"].value, 5)
        self.assertEqual(ar["j"].value, 0)
        self.assertEqual(ar["c"].value, "J")
        self.assertEqual(ar.nesting_level, 2)

    def test_const(self):
        text = """\
program const_example;

const
  PI = 3.141592654;
  FLAG = true;
  NUM = 100;
  STR = 'str';
  ARR: array[0..2] of Integer = (1, 2, 3);  {Declaring an array constant}

var
  r: REAL;
  b: BOOLEAN;
  i: INTEGER;
  s: String;
  a1 : integer;
  a2 : integer;
  a3 : integer;

begin
  r := PI;
  b := FLAG;
  i := NUM;
  s := STR;
  a1 := ARR[0];
  a2 := ARR[1];
  a3 := ARR[2];
end.
    """
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["PI"].value, 3.141592654)
        self.assertEqual(ar["FLAG"].value, True)
        self.assertEqual(ar["NUM"].value, 100)
        self.assertEqual(ar["STR"].value, "str")
        self.assertEqual(ar["ARR"].elements[0].value, 1)
        self.assertEqual(ar["ARR"].elements[1].value, 2)
        self.assertEqual(ar["ARR"].elements[2].value, 3)
        self.assertEqual(ar["r"].value, 3.141592654)
        self.assertEqual(ar["b"].value, True)
        self.assertEqual(ar["i"].value, 100)
        self.assertEqual(ar["s"].value, "str")
        self.assertEqual(ar["a1"].value, 1)
        self.assertEqual(ar["a2"].value, 2)
        self.assertEqual(ar["a3"].value, 3)
        self.assertEqual(ar.nesting_level, 1)

    def test_case_of_statement(self):
        text = """\
program CaseOfExample;

{$mode objfpc}{$H+}

type
  Day = (Sun, Mon, Tue, Wed, Thu, Fri, Sat);

var
  i1, i2: Integer;
  f1, f2: BOOLEAN;
  s1, s2, s3,s4: String;
  d1, d2 : Day;
  

begin
  i1 := 2;

  case i1 of
    1: i2 := 10;
    2: i2 := 20;
    3: i2 := 30;
  else
    Writeln('Invalid!');
  end;
  Writeln(i2);

  f1 := true;
  case f1 of
    true: f2 := true;
    false: f2 := false;
  else
    Writeln('Invalid!');
  end;
  Writeln(f2);

  s1 := 'a';
  case s1 of
    'a': s2 := 'a';
    'b': s2 := 'b';
    'c': s2 := 'c';
  else
    Writeln('Invalid!');
  end;
  Writeln(s2);

  s3 := 'a';
  case s1 of
    'a1': s4 := 'a';
    'b1': s4 := 'b';
    'c1': s4 := 'c';
  else
    s4 := 'd';
  end;
  Writeln(s4);

  d1 := Day.Sun;
  case d1 of
    Day.Sun: d2 := Day.Sun;
    Day.Mon: d2 := Day.Mon;
    Day.Tue: d2 := Day.Tue;
    Day.Wed: d2 := Day.Wed;
    Day.Thu: d2 := Day.Thu;
    Day.Fri: d2 := Day.Fri;
    Day.Sat: d2 := Day.Sat;
  else
    Writeln('Invalid!');
  end;
  Writeln(d2);
end.
    """
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["i1"].value, 2)
        self.assertEqual(ar["i2"].value, 20)
        self.assertEqual(ar["f1"].value, True)
        self.assertEqual(ar["f2"].value, True)
        self.assertEqual(ar["s1"].value, "a")
        self.assertEqual(ar["s2"].value, "a")
        self.assertEqual(ar["s3"].value, "a")
        self.assertEqual(ar["s4"].value, "d")
        self.assertEqual(ar["d1"].name, "Sun")
        self.assertEqual(ar["d1"].index, 0)
        self.assertEqual(ar["d2"].name, "Sun")
        self.assertEqual(ar["d2"].index, 0)
        self.assertEqual(ar.nesting_level, 2)

    def test_program(self):
        text = """\
PROGRAM Part12;
VAR
   number : INTEGER;
   a, b   : INTEGER;
   y      : REAL;

PROCEDURE P1;
VAR
   a : REAL;
   k : INTEGER;
   PROCEDURE P2;
   VAR
      a, z : INTEGER;
   BEGIN {P2}
      z := 777;
   END;  {P2}
BEGIN {P1}

END;  {P1}

BEGIN {Part12}
   number := 2;
   a := number ;
   b := 10 * a + 10 * number DIV 4;
   y := 20 / 7 + 3.14
END.  {Part12}
"""
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(len(ar.members.keys()), 4)
        self.assertEqual(ar["number"].value, 2)
        self.assertEqual(ar["a"].value, 2)
        self.assertEqual(ar["b"].value, 25)
        self.assertAlmostEqual(ar["y"].value, float(20) / 7 + 3.14)  # 5.9971...


class ModuleFileDiscoveryTestCase(unittest.TestCase):
    """Test cases for module file discovery system."""

    def setUp(self):
        """Set up test fixtures."""
        from src.module import ModuleRegistry

        self.registry = ModuleRegistry()

    def test_find_module_file_in_current_directory(self):
        """Test finding a module file in the current directory."""
        import os

        # Test finding CurrentDirModule.pas in current directory
        file_path = self.registry.find_module_file("CurrentDirModule")
        expected_path = os.path.join(".", "CurrentDirModule.pas")
        self.assertEqual(file_path, expected_path)

    def test_find_module_file_in_stdlib_directory(self):
        """Test finding a module file in the stdlib directory."""
        import os

        # Test finding TestModule.pas in stdlib directory
        file_path = self.registry.find_module_file("TestModule")
        expected_path = os.path.join("./stdlib", "TestModule.pas")
        self.assertEqual(file_path, expected_path)

    def test_find_module_file_current_dir_takes_precedence(self):
        """Test that current directory takes precedence over stdlib."""
        # Create a module with same name in both directories
        import os

        # Create a test file in current directory
        with open("TestPrecedence.pas", "w") as f:
            f.write("unit TestPrecedence;\ninterface\nend.")

        # Create a test file in stdlib directory
        with open("stdlib/TestPrecedence.pas", "w") as f:
            f.write("unit TestPrecedence;\ninterface\nend.")

        try:
            # Should find the one in current directory first
            file_path = self.registry.find_module_file("TestPrecedence")
            expected_path = os.path.join(".", "TestPrecedence.pas")
            self.assertEqual(file_path, expected_path)
        finally:
            # Clean up test files
            if os.path.exists("TestPrecedence.pas"):
                os.remove("TestPrecedence.pas")
            if os.path.exists("stdlib/TestPrecedence.pas"):
                os.remove("stdlib/TestPrecedence.pas")

    def test_find_module_file_not_found(self):
        """Test error when module file is not found."""
        from src.error import ModuleNotFoundError

        with self.assertRaises(ModuleNotFoundError) as cm:
            self.registry.find_module_file("NonExistentModule")

        exception = cm.exception
        self.assertEqual(exception.module_name, "NonExistentModule")
        self.assertEqual(exception.search_paths, [".", "./stdlib"])
        self.assertIn("NonExistentModule", str(exception))

    def test_module_not_found_error_message(self):
        """Test that ModuleNotFoundError provides clear error messages."""
        from src.error import ModuleNotFoundError

        error = ModuleNotFoundError("MissingModule", [".", "./stdlib", "./custom"])

        self.assertEqual(error.module_name, "MissingModule")
        self.assertEqual(error.search_paths, [".", "./stdlib", "./custom"])

        error_message = str(error)
        self.assertIn("MissingModule", error_message)
        self.assertIn(".", error_message)
        self.assertIn("./stdlib", error_message)
        self.assertIn("./custom", error_message)

    def test_load_module_with_file_discovery(self):
        """Test loading a module using file discovery."""
        # Load module from current directory
        module = self.registry.load_module("CurrentDirModule")

        self.assertEqual(module.name, "CurrentDirModule")
        self.assertEqual(module.file_path, ".\\CurrentDirModule.pas")
        self.assertFalse(module.is_loaded)

        # Verify it's stored in the registry
        self.assertIn("CurrentDirModule", self.registry.loaded_modules)
        self.assertEqual(self.registry.get_module("CurrentDirModule"), module)

    def test_load_module_from_stdlib(self):
        """Test loading a module from stdlib directory."""
        # Load module from stdlib directory
        module = self.registry.load_module("TestModule")

        self.assertEqual(module.name, "TestModule")
        self.assertEqual(module.file_path, "./stdlib\\TestModule.pas")
        self.assertFalse(module.is_loaded)

        # Verify it's stored in the registry
        self.assertIn("TestModule", self.registry.loaded_modules)
        self.assertEqual(self.registry.get_module("TestModule"), module)

    def test_load_module_with_explicit_path(self):
        """Test loading a module with an explicit file path."""
        # Load module with explicit path (should not use file discovery)
        module = self.registry.load_module("ExplicitModule", "stdlib/TestModule.pas")

        self.assertEqual(module.name, "ExplicitModule")
        self.assertEqual(module.file_path, "stdlib/TestModule.pas")
        self.assertFalse(module.is_loaded)

    def test_load_module_already_loaded(self):
        """Test that loading the same module twice returns the same instance."""
        # Load module first time
        module1 = self.registry.load_module("TestModule")

        # Load same module second time
        module2 = self.registry.load_module("TestModule")

        # Should return the same instance
        self.assertIs(module1, module2)

    def test_custom_search_paths(self):
        """Test module discovery with custom search paths."""
        import os
        from src.module import ModuleRegistry

        # Create a custom directory with a module
        os.makedirs("custom_lib", exist_ok=True)
        with open("custom_lib\\CustomModule.pas", "w") as f:
            f.write("unit CustomModule;\ninterface\nend.")

        try:
            # Create registry with custom search paths
            custom_registry = ModuleRegistry()
            custom_registry.search_paths = ["custom_lib", ".", "./stdlib"]

            # Should find module in custom directory
            file_path = custom_registry.find_module_file("CustomModule")
            self.assertEqual(file_path, "custom_lib\\CustomModule.pas")
        finally:
            # Clean up
            if os.path.exists("custom_lib/CustomModule.pas"):
                os.remove("custom_lib/CustomModule.pas")
            if os.path.exists("custom_lib"):
                os.rmdir("custom_lib")

    def test_search_paths_initialization(self):
        """Test that ModuleRegistry initializes with correct default search paths."""
        from src.module import ModuleRegistry
        registry = ModuleRegistry()

        self.assertEqual(registry.search_paths, [".", "./stdlib"])

    def test_module_registry_string_representation(self):
        """Test string representation of ModuleRegistry."""
        from src.module import ModuleRegistry
        registry = ModuleRegistry()

        # Load a module to test the loaded count
        registry.load_module("TestModule")

        str_repr = str(registry)
        self.assertIn("ModuleRegistry", str_repr)
        self.assertIn("loaded=1", str_repr)
        self.assertIn("paths=['.'", str_repr)
        self.assertIn("'./stdlib']", str_repr)


class ModuleSymbolTableTestCase(unittest.TestCase):
    def makeModuleSymbolTable(self, module_name="TestModule"):
        from src.module import ModuleSymbolTable
        return ModuleSymbolTable(
            scope_name=f"{module_name}_interface",
            scope_level=1,
            enclosing_scope=None,
            module_name=module_name
        )

    def test_module_symbol_table_creation(self):
        """Test basic ModuleSymbolTable creation and properties."""
        table = self.makeModuleSymbolTable("Math")
        
        self.assertEqual(table.module_name, "Math")
        self.assertEqual(table.scope_name, "Math_interface")
        self.assertEqual(table.scope_level, 1)
        self.assertEqual(len(table.imported_modules), 0)
        self.assertIsNone(table.enclosing_scope)

    def test_import_module_symbols(self):
        """Test importing symbols from another module."""
        from src.module import ModuleSymbolTable
        from src.symbol import BuiltinFunctionSymbol
        from src.spi_ast import Type
        from src.spi_token import Token, TokenType
        
        # Create main module symbol table
        main_table = self.makeModuleSymbolTable("Main")
        
        # Create a math module symbol table with some symbols
        math_table = ModuleSymbolTable(
            scope_name="Math_interface",
            scope_level=1,
            enclosing_scope=None,
            module_name="Math"
        )
        
        # Add a function symbol to math module
        add_func = BuiltinFunctionSymbol(
            name="ADD",
            return_type=Type(Token(TokenType.INTEGER, 0, -1, -1)),
            formal_params=[]
        )
        math_table.insert(add_func)
        
        # Import math module symbols into main
        main_table.import_module_symbols("Math", math_table)
        
        # Verify import
        self.assertTrue(main_table.has_imported_module("Math"))
        self.assertIn("Math", main_table.get_imported_modules())
        self.assertEqual(len(main_table.imported_modules), 1)

    def test_lookup_with_modules(self):
        """Test symbol lookup across modules."""
        from src.module import ModuleSymbolTable
        from src.symbol import BuiltinFunctionSymbol, VarSymbol, BuiltinTypeSymbol
        from src.spi_ast import Type
        from src.spi_token import Token, TokenType
        
        # Create main module symbol table
        main_table = self.makeModuleSymbolTable("Main")
        
        # Add a local symbol to main
        local_var = VarSymbol("x", BuiltinTypeSymbol("INTEGER"))
        main_table.insert(local_var)
        
        # Create math module with ADD function
        math_table = ModuleSymbolTable(
            scope_name="Math_interface",
            scope_level=1,
            enclosing_scope=None,
            module_name="Math"
        )
        add_func = BuiltinFunctionSymbol(
            name="ADD",
            return_type=Type(Token(TokenType.INTEGER, 0, -1, -1)),
            formal_params=[]
        )
        math_table.insert(add_func)
        
        # Import math module
        main_table.import_module_symbols("Math", math_table)
        
        # Test lookup - local symbol should be found first
        found_x = main_table.lookup_with_modules("x")
        self.assertIsNotNone(found_x)
        self.assertEqual(found_x.name, "x")
        
        # Test lookup - imported symbol should be found
        found_add = main_table.lookup_with_modules("ADD")
        self.assertIsNotNone(found_add)
        self.assertEqual(found_add.name, "ADD")
        
        # Test lookup - non-existent symbol
        found_none = main_table.lookup_with_modules("NONEXISTENT")
        self.assertIsNone(found_none)

    def test_resolve_qualified_name(self):
        """Test resolving module-qualified symbol names."""
        from src.module import ModuleSymbolTable
        from src.symbol import BuiltinFunctionSymbol
        from src.spi_ast import Type
        from src.spi_token import Token, TokenType
        
        # Create main module symbol table
        main_table = self.makeModuleSymbolTable("Main")
        
        # Create math module with ADD function
        math_table = ModuleSymbolTable(
            scope_name="Math_interface",
            scope_level=1,
            enclosing_scope=None,
            module_name="Math"
        )
        add_func = BuiltinFunctionSymbol(
            name="ADD",
            return_type=Type(Token(TokenType.INTEGER, 0, -1, -1)),
            formal_params=[]
        )
        math_table.insert(add_func)
        
        # Import math module
        main_table.import_module_symbols("Math", math_table)
        
        # Test qualified lookup
        found_add = main_table.resolve_qualified_name("Math", "ADD")
        self.assertIsNotNone(found_add)
        self.assertEqual(found_add.name, "ADD")
        
        # Test qualified lookup - non-existent module
        found_none = main_table.resolve_qualified_name("NonExistent", "ADD")
        self.assertIsNone(found_none)
        
        # Test qualified lookup - non-existent symbol
        found_none = main_table.resolve_qualified_name("Math", "NONEXISTENT")
        self.assertIsNone(found_none)

    def test_multiple_module_imports(self):
        """Test importing symbols from multiple modules."""
        from src.module import ModuleSymbolTable
        from src.symbol import BuiltinFunctionSymbol
        from src.spi_ast import Type
        from src.spi_token import Token, TokenType
        
        # Create main module symbol table
        main_table = self.makeModuleSymbolTable("Main")
        
        # Create math module
        math_table = ModuleSymbolTable(
            scope_name="Math_interface",
            scope_level=1,
            enclosing_scope=None,
            module_name="Math"
        )
        add_func = BuiltinFunctionSymbol(
            name="ADD",
            return_type=Type(Token(TokenType.INTEGER, 0, -1, -1)),
            formal_params=[]
        )
        math_table.insert(add_func)
        
        # Create array utils module
        array_table = ModuleSymbolTable(
            scope_name="ArrayUtils_interface",
            scope_level=1,
            enclosing_scope=None,
            module_name="ArrayUtils"
        )
        sort_func = BuiltinFunctionSymbol(
            name="SORT",
            return_type=Type(Token(TokenType.INTEGER, 0, -1, -1)),
            formal_params=[]
        )
        array_table.insert(sort_func)
        
        # Import both modules
        main_table.import_module_symbols("Math", math_table)
        main_table.import_module_symbols("ArrayUtils", array_table)
        
        # Verify both modules are imported
        imported_modules = main_table.get_imported_modules()
        self.assertEqual(len(imported_modules), 2)
        self.assertIn("Math", imported_modules)
        self.assertIn("ArrayUtils", imported_modules)
        
        # Test lookup from both modules
        found_add = main_table.lookup_with_modules("ADD")
        self.assertIsNotNone(found_add)
        self.assertEqual(found_add.name, "ADD")
        
        found_sort = main_table.lookup_with_modules("SORT")
        self.assertIsNotNone(found_sort)
        self.assertEqual(found_sort.name, "SORT")

    def test_symbol_precedence(self):
        """Test that local symbols take precedence over imported symbols."""
        from src.module import ModuleSymbolTable
        from src.symbol import BuiltinFunctionSymbol, VarSymbol, BuiltinTypeSymbol
        from src.spi_ast import Type
        from src.spi_token import Token, TokenType
        
        # Create main module symbol table
        main_table = self.makeModuleSymbolTable("Main")
        
        # Add a local symbol named "ADD"
        local_add = VarSymbol("ADD", BuiltinTypeSymbol("INTEGER"))
        main_table.insert(local_add)
        
        # Create math module with ADD function
        math_table = ModuleSymbolTable(
            scope_name="Math_interface",
            scope_level=1,
            enclosing_scope=None,
            module_name="Math"
        )
        imported_add = BuiltinFunctionSymbol(
            name="ADD",
            return_type=Type(Token(TokenType.INTEGER, 0, -1, -1)),
            formal_params=[]
        )
        math_table.insert(imported_add)
        
        # Import math module
        main_table.import_module_symbols("Math", math_table)
        
        # Test that local symbol takes precedence
        found_add = main_table.lookup_with_modules("ADD")
        self.assertIsNotNone(found_add)
        self.assertIsInstance(found_add, VarSymbol)  # Should be the local variable, not the function

    def test_string_representation(self):
        """Test string representation includes imported modules."""
        from src.module import ModuleSymbolTable
        
        # Create main module symbol table
        main_table = self.makeModuleSymbolTable("Main")
        
        # Create and import a math module
        math_table = ModuleSymbolTable(
            scope_name="Math_interface",
            scope_level=1,
            enclosing_scope=None,
            module_name="Math"
        )
        main_table.import_module_symbols("Math", math_table)
        
        # Test string representation
        str_repr = str(main_table)
        self.assertIn("Main_interface", str_repr)
        self.assertIn("Imported modules: ['Math']", str_repr)


if __name__ == "__main__":
    unittest.main()
