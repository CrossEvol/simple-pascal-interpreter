import unittest


class LexerTestCase(unittest.TestCase):
    def makeLexer(self, text):
        from spi import Lexer

        lexer = Lexer(text)
        return lexer

    def test_tokens(self):
        from spi import TokenType

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
        )
        for text, tok_type, tok_val in records:
            lexer = self.makeLexer(text)
            token = lexer.get_next_token()
            self.assertEqual(token.type, tok_type)
            self.assertEqual(token.value, tok_val)

    def test_lexer_exception(self):
        from spi import LexerError

        lexer = self.makeLexer("!")
        with self.assertRaises(LexerError):
            lexer.get_next_token()


class ParserTestCase(unittest.TestCase):
    def makeParser(self, text):
        from spi import Lexer, Parser

        lexer = Lexer(text)
        parser = Parser(lexer)
        return parser

    def test_expression_invalid_syntax_01(self):
        from spi import ParserError, ErrorCode

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
        from spi import ParserError, ErrorCode

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
        from spi import ParserError, VarDuplicateInScopeError

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
        from spi import Lexer, Parser, SemanticAnalyzer

        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()

        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.visit(tree)
        return semantic_analyzer

    def test_semantic_duplicate_id_error(self):
        from spi import SemanticError, ErrorCode

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
        from spi import SemanticError, ErrorCode

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
        from spi import SemanticError, ErrorCode

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
        from spi import Lexer, Parser, SemanticAnalyzer, Interpreter

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
            self.assertEqual(ar["a"], result)

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
            self.assertEqual(ar["a"], result)

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
            self.assertEqual(ar["flag"], result)

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

        self.assertEqual(ar["num"], 0)
        self.assertEqual(ar["bool"], False)
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

        self.assertEqual(ar["a"], 8)
        self.assertEqual(ar["b"], 7)
        self.assertEqual(ar["x"], 30)
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

        self.assertEqual(ar["a"], 1)
        self.assertEqual(ar["b"], 2)
        self.assertEqual(ar["f1"], True)
        self.assertEqual(ar["f2"], False)
        self.assertEqual(ar["f3"], False)
        self.assertEqual(ar["f4"], True)
        self.assertEqual(ar["f5"], True)
        self.assertEqual(ar["f6"], False)
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

        self.assertEqual(ar["a"], True)
        self.assertEqual(ar["b"], False)
        self.assertEqual(ar["f1"], False)
        self.assertEqual(ar["f2"], True)
        self.assertEqual(ar["f3"], False)
        self.assertEqual(ar["f4"], True)
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

        self.assertEqual(ar["sum"], 8)
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

        self.assertEqual(ar["x"], 30)
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

        self.assertEqual(ar["a"], 6)
        self.assertEqual(ar["flag"], True)
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

        self.assertEqual(ar["a"], 3)
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
        self.assertEqual(ar["i"], 0)
        self.assertEqual(ar["j"], 10)
        self.assertEqual(ar["k"], 10)
        for i in range(0, 5):
            self.assertEqual(ar["arr"][i], i + 1)
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
        self.assertEqual(ar["i"], 5)
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

            self.assertEqual(ar["a"], expr)
            self.assertEqual(ar["b"], result)
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

            self.assertEqual(ar["a"], expr)
            self.assertEqual(ar["b"], result)
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

        self.assertEqual(ar["a"], 100)
        self.assertEqual(ar["b"], 200)
        self.assertEqual(ar["c"], 300)
        self.assertEqual(ar["flag"], True)
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

            self.assertEqual(ar["flag"], flag)
            self.assertEqual(ar["a"], a)
            self.assertEqual(ar["b"], b)
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
        self.assertEqual(ar["a"], 20)
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
        self.assertEqual(ar["a"], 10)
        self.assertEqual(ar["sum"], 55)
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
        self.assertEqual(ar["a"], -10)
        self.assertEqual(ar["b"], 10)
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
            self.assertEqual(ar["intArr"][i], 0)
            self.assertEqual(ar["boolArr"][i], False)
            self.assertEqual(ar["realArr"][i], 0.0)
        self.assertEqual(ar.nesting_level, 1)

    def test_array_range_invalid(self):
        from spi import InterpreterError, ArrayRangeInvalidError

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
        from spi import InterpreterError, StaticArrayModifyLengthError

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
        self.assertEqual(ar["a"], 0)
        self.assertEqual(ar["b"], False)
        self.assertEqual(ar["c"], 0.0)
        self.assertEqual(ar["d"], {})
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
        self.assertEqual(ar["str1"], "abcdefg")
        self.assertEqual(ar["str2"], "abcdefghijklmnopqrstuvwxyz")
        self.assertEqual(ar["str3"], "abcdefghijklmn")
        self.assertEqual(ar["l1"], 7)
        self.assertEqual(ar["l2"], 26)
        self.assertEqual(ar["l3"], 14)
        self.assertEqual(ar["a"], "a")
        self.assertEqual(ar["b"], "")
        self.assertEqual(ar["concat1"], "abcdefg123456a")
        self.assertEqual(ar["concat2"], "abcdefg")
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
        self.assertEqual(ar["person"]["name"], "Alice")
        self.assertEqual(ar["person"]["age"], 30)
        self.assertEqual(ar["bName"], "Alice")
        self.assertEqual(ar["bAge"], 30)
        self.assertEqual(ar["person2"]["name"], "Tom")
        self.assertEqual(ar["person2"]["age"], 18)
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
        self.assertEqual(ar["person"]["name"], "")
        self.assertEqual(ar["person"]["age"], 0)
        self.assertEqual(ar["person2"]["name"], "Tom")
        self.assertEqual(ar["person2"]["age"], 18)
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
        self.assertEqual(ar["o0"], 0)
        self.assertEqual(ar["o1"], 1)
        self.assertEqual(ar["o2"], 2)
        self.assertEqual(ar["o3"], 3)
        self.assertEqual(ar["o4"], 4)
        self.assertEqual(ar["o5"], 5)
        self.assertEqual(ar["o6"], 6)
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
    i: Integer;

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
end.
    """
        interpreter = self.makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["exampleUser"]["ID"], 1)
        self.assertEqual(ar["exampleUser"]["Name"], "John Doe")
        self.assertEqual(ar["exampleUser"]["Age"], 30)
        self.assertEqual(ar["exampleUser"]["Salary"], 45000.50)
        self.assertEqual(ar["exampleUser"]["Active"], True)
        self.assertEqual(ar["exampleUser"]["Scores"][1], 85)
        self.assertEqual(ar["exampleUser"]["Scores"][2], 90)
        self.assertEqual(ar["exampleUser"]["Scores"][3], 78)
        self.assertEqual(ar["exampleUser"]["Scores"][4], 92)
        self.assertEqual(ar["exampleUser"]["Scores"][5], 88)
        self.assertEqual(ar["i"], 5)
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
        self.assertEqual(ar["number"], 2)
        self.assertEqual(ar["a"], 2)
        self.assertEqual(ar["b"], 25)
        self.assertAlmostEqual(ar["y"], float(20) / 7 + 3.14)  # 5.9971...


if __name__ == "__main__":
    unittest.main()
