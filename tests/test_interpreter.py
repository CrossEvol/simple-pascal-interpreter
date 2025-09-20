import unittest

from spi.ast_and_symbol import Var
from spi.interpreter import Interpreter
from spi.lexer import Lexer
from spi.object import (
    ArrayObject,
    BooleanObject,
    CharObject,
    EnumObject,
    IntegerObject,
    InterpreterError,
    NullObject,
    RealObject,
    RecordObject,
    StringObject,
)
from spi.parser import Parser
from spi.semantic_analyzer import SemanticAnalyzer
from spi.token import Token, TokenType


class MockCallStack:
    def __init__(self):
        self._records = []

    def push(self, ar):
        self._records.append(ar)

    def pop(self):
        # do nothing
        pass

    def peek(self):
        return self._records[-1]


def makeInterpreter(text):
    lexer = Lexer(text)
    parser = Parser(lexer)
    tree = parser.parse()

    semantic_analyzer = SemanticAnalyzer()
    semantic_analyzer.visit(tree)

    interpreter = Interpreter(tree)
    interpreter.call_stack = MockCallStack()
    return interpreter


class InterpreterCalculusTestCase(unittest.TestCase):
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
            ("17 MOD 5", 2),
            ("20 MOD 5", 0),
            ("10 MOD 3", 1),
        ):
            interpreter = makeInterpreter(
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

    def test_mod_operator_with_floats(self):
        """Test that MOD operator works with floats (converted to integers)"""
        interpreter = makeInterpreter(
            """PROGRAM Test;
               VAR
                   a : INTEGER;
               BEGIN
                   a := 17.5 MOD 5.2;  { Should be 2 (17 MOD 5) }
               END.
            """
        )
        interpreter.interpret()
        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["a"].value, 2)

    def test_float_arithmetic_expressions(self):
        for expr, result in (
            ("3.14", 3.14),
            ("2.14 + 7 * 4", 30.14),
            ("7.14 - 8 / 4", 5.14),
        ):
            interpreter = makeInterpreter(
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
        interpreter = makeInterpreter(text)
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
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["a"].value, True)
        self.assertEqual(ar["b"].value, False)
        self.assertEqual(ar["f1"].value, False)
        self.assertEqual(ar["f2"].value, True)
        self.assertEqual(ar["f3"].value, False)
        self.assertEqual(ar["f4"].value, True)
        self.assertEqual(ar.nesting_level, 1)


class InterpreterNativeMethodTestCase(unittest.TestCase):
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
        interpreter = makeInterpreter(text)
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
        interpreter = makeInterpreter(text)
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
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["i"].value, 0)
        self.assertEqual(ar["j"].value, 10)
        self.assertEqual(ar["k"].value, 10)
        for i in range(0, 5):
            self.assertEqual(ar["arr"].value[i].value, i + 1)
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
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["i"].value, 5)
        self.assertEqual(ar.nesting_level, 2)

    def test_inc_procedure(self):
        text = """\
PROGRAM TestInc;

VAR
    i: INTEGER;

BEGIN
    i := 1;
    Inc(i);
END.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["i"].value, 2)

    def test_dec_procedure(self):
        text = """\
PROGRAM TestDec;

VAR
    i: INTEGER;

BEGIN
    i := 10;
    Dec(i);
END.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["i"].value, 9)

    def test_ord_chr_functions(self):
        text = """\
PROGRAM TestOrdChr;

VAR
    c1, c2: CHAR;
    n1, n2: INTEGER;

BEGIN
    c1 := 'Z';
    n1 := Ord(c1);
    
    n2 := 66;
    c2 := Chr(n2);
END.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["c1"].value, "Z")
        self.assertEqual(ar["n1"].value, 90)  # ASCII code for 'Z'
        self.assertEqual(ar["n2"].value, 66)
        self.assertEqual(ar["c2"].value, "B")  # Chr(66) = 'B'


class InterpreterConditionalTestCase(unittest.TestCase):
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
            interpreter = makeInterpreter(text)
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
            interpreter = makeInterpreter(text)
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
        interpreter = makeInterpreter(text)
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
            interpreter = makeInterpreter(text)
            interpreter.interpret()
            ar = interpreter.call_stack.peek()

            self.assertEqual(ar["flag"].value, flag)
            self.assertEqual(ar["a"].value, a)
            self.assertEqual(ar["b"].value, b)
            self.assertEqual(ar.nesting_level, 1)

    def test_case_integer_statement(self):
        text = """\
program TestCaseInteger;
var
  i, result: integer;
begin
  i := 2;
  case i of
    1: result := 10;
    2: result := 20;
    3: result := 30;
  else
    result := 0;
  end;
end.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["i"].value, 2)
        self.assertEqual(ar["result"].value, 20)

    def test_case_char_statement(self):
        text = """\
program TestCaseChar;
var
  c: char;
  result: integer;
begin
  c := 'B';
  case c of
    'A': result := 1;
    'B': result := 2;
    'C': result := 3;
  else
    result := 0;
  end;
end.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["c"].value, "B")
        self.assertEqual(ar["result"].value, 2)

    def test_case_boolean_statement(self):
        text = """\
program TestCaseBoolean;
var
  b: boolean;
  result: integer;
begin
  b := false;
  case b of
    true: result := 1;
    false: result := 2;
  end;
end.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["b"].value, False)
        self.assertEqual(ar["result"].value, 2)


class InterpreterBasicTypeTestCase(unittest.TestCase):
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
            interpreter = makeInterpreter(
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
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["num"].value, 0)
        self.assertEqual(ar["bool"].value, False)
        self.assertEqual(ar.nesting_level, 1)

    def test_null_object(self):
        text = """\
program NullObjectTest;
var
  a: integer;
  b: boolean;
begin
  a := 10;
  b := true;
end.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        # Test that accessing undefined variables returns NullObject
        undefined_var = ar.get("undefined_var")
        self.assertIsNone(undefined_var)

        # Test that visiting undefined variables returns NullObject
        # We'll simulate this by directly calling the interpreter method

        undefined_node = Var(Token(TokenType.ID, "undefined_var"))
        result = interpreter.visit_Var(undefined_node)

        self.assertIsInstance(result, NullObject)

        # Test that NullObject converts to False in boolean context
        self.assertFalse(result.to_bool())

    def test_char_type_and_literals(self):
        text = """\
PROGRAM TestChar;

VAR
    c1, c2, c3, c4: CHAR;
    result1, result2: INTEGER;

BEGIN
    c1 := 'A';
    c2 := #65;  { ASCII code for 'A' }
    c3 := #97;  { ASCII code for 'a' }
    c4 := #32;  { ASCII code for space }
    
    result1 := Ord(c1);
    result2 := Ord(c3);
END.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["c1"].value, "A")
        self.assertEqual(ar["c2"].value, "A")
        self.assertEqual(ar["c3"].value, "a")
        self.assertEqual(ar["c4"].value, " ")
        self.assertEqual(ar["result1"].value, 65)
        self.assertEqual(ar["result2"].value, 97)

    def test_char_default_value(self):
        text = """\
PROGRAM TestCharDefault;

VAR
    c: CHAR;
    n: INTEGER;

BEGIN
    n := Ord(c);  { Should be 0 for empty char }
END.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["c"].value, "")
        self.assertEqual(ar["n"].value, 0)

    def test_char_comparison(self):
        text = """\
program CharacterComparison;

var
  char1, char2 , char3: CHAR;
  f0, f1, f2, f3, f4, f5, f6 : boolean;

begin
  { 初始化字符变量 }
  char1 := 'A';
  char2 := 'B';
  char3 := #65;
  
  { 演示等于运算 (=) }
  f0 := char1 = char3;
  f1 := char1 = char2;
  
  { 演示不等于运算 (<>) }
  f2 := char1 <> char2;
  
  { 演示小于运算 (<) }
  f3 := char1 < char2;
  
  { 演示大于运算 (>) }
  f4 := char1 > char2;
  
  { 演示小于等于运算 (<=) }
  f5 := char1 <= char2;
  
  { 演示大于等于运算 (>=) }
  f6 := char1 >= char2;
  
end.
"""
        interpreter = makeInterpreter(text)
        try:
            interpreter.interpret()
        except InterpreterError as e:
            print(f"Error Code: {e.error_code}")
            print(f"Token: {e.token}")
            print(f"Message: {e.message}")
            raise  # 重新抛出异常以便测试框架捕获

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["char1"].value, "A")
        self.assertEqual(ar["char2"].value, "B")
        self.assertEqual(ar["char3"].value, "A")
        self.assertEqual(ar["f0"].value, True)
        self.assertEqual(ar["f1"].value, False)
        self.assertEqual(ar["f2"].value, True)
        self.assertEqual(ar["f3"].value, True)
        self.assertEqual(ar["f4"].value, False)
        self.assertEqual(ar["f5"].value, True)
        self.assertEqual(ar["f6"].value, False)


class InterpreterComplexTypeTestCase(unittest.TestCase):
    def test_array(self):
        text = """\
    program exArrays;
    var
       n: array [1..10] of integer;
       i, j, sum: integer;

    begin
       for i := 1 to 10 do
           n[ i ] := i + 100;
       for j := 1 to 10 do
           sum := sum + n[j];
    end.
    """
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["i"].value, 10)
        self.assertEqual(ar["j"].value, 10)
        self.assertEqual(ar["sum"].value, 1055)
        for i in range(1, 10):
            self.assertEqual(ar["n"].value[i].value, 100 + i)
        self.assertEqual(ar.nesting_level, 1)

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
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        for i in range(1, 2):
            self.assertEqual(ar["intArr"].value[i].value, 0)
            self.assertEqual(ar["boolArr"].value[i].value, False)
            self.assertEqual(ar["realArr"].value[i].value, 0.0)
        self.assertEqual(ar.nesting_level, 1)

    def test_array_range_invalid(self):
        text = """\
program ArranRange;
var
    validArr: array [1..2] of integer;
    invalidArr : array [2 .. -2] of integer;
begin
end.
    """
        with self.assertRaises(InterpreterError) as cm:
            interpreter = makeInterpreter(text)
            interpreter.interpret()

            ar = interpreter.call_stack.peek()
            self.assertEqual(ar.nesting_level, 1)
        self.assertIsInstance(cm.exception, InterpreterError)

    def test_static_array_modify_length(self):
        text = """\
program ArranRange;
var
    arr: array [1..2] of integer;
begin
    setLength(arr,5);
end.
    """
        with self.assertRaises(InterpreterError) as cm:
            interpreter = makeInterpreter(text)
            interpreter.interpret()

            ar = interpreter.call_stack.peek()
            self.assertEqual(ar.nesting_level, 1)
        self.assertIsInstance(cm.exception, InterpreterError)

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
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["a"].value, 0)
        self.assertEqual(ar["b"].value, False)
        self.assertEqual(ar["c"].value, 0.0)
        self.assertEqual(ar["d"].value, {})
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
        interpreter = makeInterpreter(text)
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

    def test_char_array(self):
        text = """\
PROGRAM TestCharArray;

VAR
    chars: array[1..3] of CHAR;
    i: INTEGER;

BEGIN
    chars[1] := 'H';
    chars[2] := #101;  { 'e' }
    chars[3] := 'y';
    
    i := Ord(chars[2]);
END.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["chars"].value[1].value, "H")
        self.assertEqual(ar["chars"].value[2].value, "e")
        self.assertEqual(ar["chars"].value[3].value, "y")
        self.assertEqual(ar["i"].value, 101)

    def test_enum_types(self):
        """
        该测试中不要使用 Ord() 取值， 所有的函数调用都会压栈
        但是测试用的 TestCallStack 是不会出栈的， 也不会有复制值进去复制新值出来的操作
        """
        text = """\
program EnumDemo;

type
  {定义一个名为 TColor 的枚举类型}
  TColor = (Red, Green, Blue, Yellow, Purple);

var
  {声明 TColor 类型的变量}
  r, g , b , y , p: TColor;
  myColor: TColor;
  {声明一个整数变量用于存储枚举值的序号}
  c1,c2,c3, c4,c5: Integer;
  f1, f2 , f3, f4, f5, f6:Boolean;

begin
  {给枚举变量赋值}
  r := Red;
  g := Green;
  b := Blue;
  y := Yellow;
  p := Purple;

 

  {比较枚举值}
  f1 := r = Red;
  f2 := r <> Green;
  f3 := g < Blue;
  f4 := b > Yellow;
  f5 := y <= Purple;
  f6 := p >= Red;

  {遍历所有枚举值}
  for myColor := Red to Purple do
  begin
    case myColor of
      Red:    c1 := 0;
      Green:  c2 := 1;
      Blue:   c3 := 2;
      Yellow: c4 := 3;
      Purple: c5 := 4;
    end;
  end;

  {注意：不能直接 Write(myColor) 输出枚举名称（某些编译器支持，但非标准）}
  {标准做法是使用 Case 语句转换为字符串输出}
end.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        # 检查枚举值是否正确设置
        # 检查Ord函数是否返回正确的ordinal
        self.assertIsInstance(ar["r"], EnumObject)
        self.assertEqual(ar["r"].name, "Red")
        self.assertEqual(ar["r"].ordinal, 0)
        self.assertEqual(ar["r"].value, 0)
        self.assertEqual(ar["r"].type_name, "TColor")
        self.assertIsInstance(ar["g"], EnumObject)
        self.assertEqual(ar["g"].name, "Green")
        self.assertEqual(ar["g"].ordinal, 1)
        self.assertEqual(ar["g"].value, 1)
        self.assertEqual(ar["g"].type_name, "TColor")
        self.assertIsInstance(ar["b"], EnumObject)
        self.assertEqual(ar["b"].name, "Blue")
        self.assertEqual(ar["b"].ordinal, 2)
        self.assertEqual(ar["b"].value, 2)
        self.assertEqual(ar["b"].type_name, "TColor")
        self.assertIsInstance(ar["y"], EnumObject)
        self.assertEqual(ar["y"].name, "Yellow")
        self.assertEqual(ar["y"].ordinal, 3)
        self.assertEqual(ar["y"].value, 3)
        self.assertEqual(ar["y"].type_name, "TColor")
        self.assertIsInstance(ar["p"], EnumObject)
        self.assertEqual(ar["p"].name, "Purple")
        self.assertEqual(ar["p"].ordinal, 4)
        self.assertEqual(ar["p"].value, 4)
        self.assertEqual(ar["p"].type_name, "TColor")

        # 检查 c1~c5 是否赋值成功
        self.assertIsInstance(ar["c1"], IntegerObject)
        self.assertEqual(ar["c1"].value, 0)
        self.assertIsInstance(ar["c2"], IntegerObject)
        self.assertEqual(ar["c2"].value, 1)
        self.assertIsInstance(ar["c3"], IntegerObject)
        self.assertEqual(ar["c3"].value, 2)
        self.assertIsInstance(ar["c4"], IntegerObject)
        self.assertEqual(ar["c4"].value, 3)
        self.assertIsInstance(ar["c5"], IntegerObject)
        self.assertEqual(ar["c5"].value, 4)

        # 检查 f1~f6 是否赋值成功
        self.assertIsInstance(ar["f1"], BooleanObject)
        self.assertEqual(ar["f1"].value, True)
        self.assertIsInstance(ar["f2"], BooleanObject)
        self.assertEqual(ar["f2"].value, True)
        self.assertIsInstance(ar["f3"], BooleanObject)
        self.assertEqual(ar["f3"].value, True)
        self.assertIsInstance(ar["f4"], BooleanObject)
        self.assertEqual(ar["f4"].value, False)
        self.assertIsInstance(ar["f5"], BooleanObject)
        self.assertEqual(ar["f5"].value, True)
        self.assertIsInstance(ar["f6"], BooleanObject)
        self.assertEqual(ar["f6"].value, True)

        # 检查 myColor 是否赋值成功
        self.assertIsInstance(ar["myColor"], EnumObject)
        self.assertEqual(ar["myColor"].name, "Purple")
        self.assertEqual(ar["myColor"].ordinal, 4)
        self.assertEqual(ar["myColor"].value, 4)
        self.assertEqual(ar["myColor"].type_name, "TColor")

    def test_record_types_with_basic_type_as_field(self):
        """Test basic record type functionality"""
        text = """\
program RecordTest;
type
    TPerson = record
        name: String;
        age: Integer;
        height: Real;
        married: Boolean;
        mark : Char;
    end;

var
    person1: TPerson;
    name : String;
    age : Integer;
    height: Real;
    married : Boolean;
    mark : Char;
begin
    person1.name := 'Alice';
    person1.age := 30;
    person1.height := 1.65;
    person1.married := true;
    person1.mark := #65;
    name := person1.name;
    age := person1.age;
    height := person1.height;
    married := person1.married;
    mark := person1.mark;
end.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Check that the record was created
        self.assertIn("person1", ar.members)
        self.assertIsInstance(ar["person1"], RecordObject)
        self.assertIsInstance(ar["name"], StringObject)
        self.assertEqual(ar["name"].value, "Alice")
        self.assertIsInstance(ar["age"], IntegerObject)
        self.assertEqual(ar["age"].value, 30)
        self.assertIsInstance(ar["height"], RealObject)
        self.assertEqual(ar["height"].value, 1.65)
        self.assertIsInstance(ar["married"], BooleanObject)
        self.assertEqual(ar["married"].value, True)
        self.assertIsInstance(ar["mark"], CharObject)
        self.assertEqual(ar["mark"].value, "A")

        # For now, we're just checking that the program runs without error
        # In a more complete implementation, we would check the record fields

    def test_record_with_one_level_array(self):
        """Test record field access and assignment"""
        text = """\
program RecordExample;

type
    TPerson = record
        name: array of String;
        age: array of Integer;
        height: array of Real;
        married: array of Boolean;
        mark: array of Char;
    end;

var
    person1: TPerson;
    name : String;
    age : Integer;
    height : Real;
    married : Boolean;
    mark : Char;

begin
    {Allocate memory for the dynamic array 'name' and set its length to 1.}
    {Dynamic arrays are 0-indexed in Pascal, so the first element is at index 0.}
    {SetLength(person1.name, 1);}

    person1.name[0] := 'Alice'; {Change index to 0}
    person1.age[1] := 30;
    person1.height[2] := 1.65;
    person1.married[3] := true;
    person1.mark[4] := #65;
    
    name := person1.name[0];
    age := person1.age[1];
    height := person1.height[2];
    married := person1.married[3];
    mark := person1.mark[4];
end.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertIn("person1", ar.members)
        self.assertIsInstance(ar["person1"], RecordObject)
        self.assertIsInstance(ar["name"], StringObject)
        self.assertEqual(ar["name"].value, "Alice")
        self.assertIsInstance(ar["age"], IntegerObject)
        self.assertEqual(ar["age"].value, 30)
        self.assertIsInstance(ar["height"], RealObject)
        self.assertEqual(ar["height"].value, 1.65)
        self.assertIsInstance(ar["married"], BooleanObject)
        self.assertEqual(ar["married"].value, True)
        self.assertIsInstance(ar["mark"], CharObject)
        self.assertEqual(ar["mark"].value, "A")

    def test_nested_record_access(self):
        """Test nested record access"""
        text = """\
program NestedRecordAccess;
type
    TAddress = record
        street: String;
        city: String;
    end;
    
    TPerson = record
        name: String;
        address: TAddress;
    end;

var
    person: TPerson;
    name : String;
    street : String;
    city : String;
begin
    person.name := 'John Doe';
    person.address.street := '123 Main St';
    person.address.city := 'Anytown';
    name := person.name;
    street := person.address.street;
    city := person.address.city;
end.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Check that the nested record was created and accessed
        self.assertIn("person", ar.members)
        self.assertIsInstance(ar["person"], RecordObject)
        self.assertIsInstance(ar["name"], StringObject)
        self.assertEqual(ar["name"].value, "John Doe")
        self.assertIsInstance(ar["street"], StringObject)
        self.assertEqual(ar["street"].value, "123 Main St")
        self.assertIsInstance(ar["city"], StringObject)
        self.assertEqual(ar["city"].value, "Anytown")
        # In a more complete implementation, we would check the actual values

    def test_simple_variant_record(self):
        """Test nested record access"""
        text = """\
program SimpleVariantRecordExample;

type
    TShapeType = (Circle, Rectangle);
    
    TShape = record
        shapeType: TShapeType;
        case shapeType of
            Circle: (radius: Real);
            Rectangle: (width, height: Real);
    end;

var
    shape1: TShape;
    shapeType : TShapeType;
    radius : Real;
begin
    shape1.shapeType := Circle;
    shape1.shapeType := Rectangle;
    shape1.radius := 5.0;
    shape1.radius := shape1.radius + 5.0;
    shapeType := shape1.shapeType;
    radius := shape1.radius;
end.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Check that the nested record was created and accessed
        self.assertIn("shape1", ar.members)
        self.assertIn("shapeType", ar.members)
        self.assertIn("radius", ar.members)
        self.assertIsInstance(ar["shape1"], RecordObject)
        self.assertIsInstance(ar["shapeType"], EnumObject)
        self.assertEqual(ar["shapeType"].name, "Rectangle")
        self.assertIsInstance(ar["radius"], RealObject)
        self.assertEqual(ar["radius"].value, 10.0)
        # In a more complete implementation, we would check the actual values

    def test_chained_alias_for_basic_type(self):
        """Test nested record access"""
        text = """\
PROGRAM ChainedAliasTest;

TYPE
  T1 = INTEGER;
  T2 = T1;
  T3 = T2;
  T4 = T3;

VAR
  a: T1;
  b: T2;
  c: T3;
  d: T4;

BEGIN
  a := 10;
  b := 20;
  c := 30;
  d := 40;
END.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Check that the nested record was created and accessed
        self.assertIn("a", ar.members)
        self.assertIn("b", ar.members)
        self.assertIn("c", ar.members)
        self.assertIn("d", ar.members)
        self.assertIsInstance(ar["a"], IntegerObject)
        self.assertEqual(ar["a"].value, 10)
        self.assertIsInstance(ar["b"], IntegerObject)
        self.assertEqual(ar["b"].value, 20)
        self.assertIsInstance(ar["c"], IntegerObject)
        self.assertEqual(ar["c"].value, 30)
        self.assertIsInstance(ar["d"], IntegerObject)
        self.assertEqual(ar["d"].value, 40)
        # In a more complete implementation, we would check the actual values

    def test_chained_alias_for_record_type(self):
        """Test nested record access"""
        text = """\
PROGRAM RecordAliasTest;

TYPE
  Point = RECORD
    x, y: REAL;
  END;
  Vector = Point;
  P1 = Vector;
  P2 = Vector;
  P3 = Vector;

VAR
  p: P3;
  x: Real;
  y: Real;

BEGIN
  p.x := 1.5;
  p.y := 2.5;
  
  x := p.x;
  y := p.y;
  
END.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Check that the nested record was created and accessed
        self.assertIn("p", ar.members)
        self.assertIn("x", ar.members)
        self.assertIn("y", ar.members)
        self.assertIsInstance(ar["p"], RecordObject)
        self.assertIsInstance(ar["x"], RealObject)
        self.assertEqual(ar["x"].value, 1.5)
        self.assertIsInstance(ar["y"], RealObject)
        self.assertEqual(ar["y"].value, 2.5)
        # In a more complete implementation, we would check the actual values

    def test_chained_alias_for_array_type(self):
        """Test nested record access"""
        text = """\
PROGRAM ArrayAliasTest;

TYPE
  IntArray = array of Integer;
  A1 = IntArray;
  A2 = A1;
  A3 = A2;
var 
  arr : A3;
  i : Integer;
BEGIN
  {setLength(arr, 10);}
  arr[0] := 1;
  i := arr[0];
END.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Check that the nested record was created and accessed
        self.assertIn("arr", ar.members)
        self.assertIn("i", ar.members)
        self.assertIsInstance(ar["arr"], ArrayObject)
        self.assertIsInstance(ar["i"], IntegerObject)
        self.assertEqual(ar["i"].value, 1)
        # In a more complete implementation, we would check the actual values

    def test_chained_alias_for_string_type(self):
        """Test nested record access"""
        text = """\
PROGRAM ArrayAliasTest;

TYPE
    S0 = String;
    S1 = S0;
    S2 = S1;
    S3 = S2;
var 
  s : S3;
BEGIN
  s := 'ABC';
END.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Check that the nested record was created and accessed
        self.assertIn("s", ar.members)
        self.assertIsInstance(ar["s"], StringObject)
        self.assertEqual(ar["s"].value, "ABC")
        # In a more complete implementation, we would check the actual values

    def test_chained_alias_for_enum_type(self):
        """Test nested record access"""
        text = """\
PROGRAM EnumAliasTest;

TYPE
  Color = (Red, Green, Blue);
  MyColor = Color;
  PrimaryColor = MyColor;

VAR
  c1: Color;
  c2: MyColor;
  c3: PrimaryColor;

BEGIN
  c1 := Red;
  c2 := Green;
  c3 := Blue;
END.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Check that the nested record was created and accessed
        self.assertIn("c1", ar.members)
        self.assertIn("c2", ar.members)
        self.assertIn("c3", ar.members)
        self.assertIsInstance(ar["c1"], EnumObject)
        self.assertEqual(ar["c1"].name, "Red")
        self.assertIsInstance(ar["c2"], EnumObject)
        self.assertEqual(ar["c2"].name, "Green")
        self.assertIsInstance(ar["c3"], EnumObject)
        self.assertEqual(ar["c3"].name, "Blue")
        # In a more complete implementation, we would check the actual values

    def test_complex_nested_record(self):
        """Test nested record access"""
        text = """\
program ComplexNestedRecordExample;

{ Define the character array type }
type
  TStringArray = array[0..3] of string; { An array of 4 strings }

{ Define the innermost record 'C' which contains the string array }
  TRecordC = record
    d: TStringArray; { This field is an array of strings }
  end;

{ Define the middle record 'B' which contains record 'C' }
  TRecordB = record
    c: TRecordC;
  end;

{ Define the main record 'A' which contains record 'B' }
  TRecordA = record
    b: TRecordB;
  end;

  T1 = TRecordA;
  T2 = T1;

{ Define the array of records 'Arr' }
  TRecordArray = array[0..2] of T2; { An array of 3 'A' records }

{ Main program variables }
var
  arr: TRecordArray;
  charToAccess: Char;
  testString: string;
  testCharIndex: Integer;

begin
  { Set the string array within the innermost record for arr[1] }
  testCharIndex := 3;
  arr[1].b.c.d[0] := 'Alpha';
  arr[1].b.c.d[1] := 'Bravo';
  arr[1].b.c.d[2] := 'Charlie';
  arr[1].b.c.d[testCharIndex] := 'Delta';

  { Access the string at index 2 (which is 'Charlie') }
  testString := arr[1].b.c.d[2]; 
  
  { Access the character at index 3 within that string }
  { Note: Pascal string indexing is 1-based, not 0-based like arrays. }
  { The 'C' in 'Charlie' is at index 1, 'h' is at 2, 'a' is at 3. }
  charToAccess := arr[1].b.c.d[2][testCharIndex];
  
end.


"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Check that the nested record was created and accessed
        self.assertIn("arr", ar.members)
        self.assertIn("charToAccess", ar.members)
        self.assertIn("testString", ar.members)
        self.assertIn("testCharIndex", ar.members)
        self.assertIsInstance(ar["charToAccess"], CharObject)
        self.assertEqual(ar["charToAccess"].value, "a")
        self.assertIsInstance(ar["testString"], StringObject)
        self.assertEqual(ar["testString"].value, "Charlie")
        self.assertIsInstance(ar["testCharIndex"], IntegerObject)
        self.assertEqual(ar["testCharIndex"].value, 3)
        # In a more complete implementation, we would check the actual values


class InterpreterLoopTestCase(unittest.TestCase):
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
        interpreter = makeInterpreter(text)
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
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["a"].value, 10)
        self.assertEqual(ar["sum"].value, 55)
        self.assertEqual(ar.nesting_level, 1)


class InterpreterFunctionInvokeTestCase(unittest.TestCase):
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
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["a"].value, 8)
        self.assertEqual(ar["b"].value, 7)
        self.assertEqual(ar["x"].value, 30)
        self.assertEqual(ar.nesting_level, 2)

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
        interpreter = makeInterpreter(text)
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
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["x"].value, 30)
        self.assertEqual(ar.nesting_level, 3)


class InterpreterTestCase(unittest.TestCase):
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
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(len(ar.members.keys()), 4)
        self.assertEqual(ar["number"].value, 2)
        self.assertEqual(ar["a"].value, 2)
        self.assertEqual(ar["b"].value, 25)
        self.assertAlmostEqual(ar["y"].value, float(20) / 7 + 3.14)  # 5.9971...


if __name__ == "__main__":
    unittest.main()
