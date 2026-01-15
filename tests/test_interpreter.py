import unittest

from spi.ast import Var
from spi.error import ErrorCode
from spi.interpreter import ActivationRecord, Interpreter
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
    """
    MockCallStack will not pop ActivationRecord, its nesting_level will only incr
    """

    def __init__(self):
        self._records = []

    def push(self, ar: ActivationRecord):
        ar.nesting_level = self.nesting_level + 1
        self._records.append(ar)

    def pop(self):
        # do nothing
        pass

    def peek(self):
        if len(self._records) == 0:
            return None
        return self._records[-1]

    @property
    def nesting_level(self) -> int:
        return len(self._records)


class MockFunctionCallStack:
    def __init__(self) -> None:
        self._records: list[ActivationRecord] = []

    def push(self, ar: ActivationRecord) -> None:
        ar.nesting_level = self.nesting_level + 1
        self._records.append(ar)

    def pop(self) -> ActivationRecord:
        if len(self._records) >= 2:
            pre_ar = self._records[-1]
            self._records.pop()
            return pre_ar
        else:
            pass

    def peek(self) -> ActivationRecord:
        if len(self._records) == 0:
            return None
        return self._records[-1]

    @property
    def nesting_level(self) -> int:
        return len(self._records)

    def __str__(self) -> str:
        s = "\n".join(repr(ar) for ar in reversed(self._records))
        s = f"CALL STACK\n{s}\n\n"
        return s

    def __repr__(self) -> str:
        return self.__str__()


def makeInterpreter(text: str, mock_call_stack=None):
    lexer = Lexer(text)
    parser = Parser(lexer)
    tree = parser.parse()

    semantic_analyzer = SemanticAnalyzer()
    semantic_analyzer.visit(tree)

    interpreter = Interpreter(tree)
    interpreter.call_stack = (
        MockCallStack() if mock_call_stack is None else mock_call_stack
    )
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
            ("17 MOD 5", 2),
            ("20 MOD 5", 0),
            ("10 MOD 3", 1),
            ("5 - - - + - 3", 8),
            ("5 - - - + - (3 + 4) - +2", 10),
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

    def test_eq_calculus_for_char(self):
        text = """\
program NotEQ;

var 
  ch : Char;
  b1,b2 : Boolean;

begin 
  b1 := ch = ' ';
  b2 := ch <> ' ';
end.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["ch"].value, "")
        self.assertEqual(ar["b1"].value, False)
        self.assertEqual(ar["b2"].value, True)
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
        # MockCallStack will not pop ActivationRecord
        self.assertEqual(ar.nesting_level, 8)

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
        # MockCallStack will not pop ActivationRecord
        self.assertEqual(ar.nesting_level, 3)

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
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["i"].value, 0)
        self.assertEqual(ar["j"].value, 10)
        self.assertEqual(ar["k"].value, 10)
        for i in range(0, 5):
            self.assertEqual(ar["arr"].value[i].value, i + 1)
        self.assertEqual(ar.nesting_level, 1)

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
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["i"].value, 5)
        self.assertEqual(ar.nesting_level, 1)

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
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
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
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
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
        self.assertEqual(ar["n1"], 90)  # ASCII code for 'Z'
        self.assertEqual(ar["n2"], 66)
        self.assertEqual(ar["c2"].value, "B")  # Chr(66) = 'B'

    def test_get_tick_count_functions(self):
        text = """\
        program TimerTest;

        uses
            SysUtils;

        var
            StartTime : Integer;

        begin
            
            { 记录开始时间（单位：毫秒）}
            StartTime := GetTickCount();
        
        end.
"""
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertIn("StartTime", ar.members)

    def test_exit_in_simple_procedure(self):
        """Test Exit in a simple procedure without nested structures"""
        text = """\
        program SimpleExitTest;
        var
            executed: Boolean;
            
        procedure TestProc();
        begin
            executed := True;
            Exit();
            executed := False;  { This should not execute }
        end;
        
        begin
            executed := False;
            TestProc();
        end.
"""
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
        interpreter.interpret()
        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["executed"].value, True)
        self.assertEqual(ar.nesting_level, 1)

    def test_exit_in_simple_function(self):
        """Test Exit in a simple function without nested structures"""
        text = """\
        program SimpleFunctionExitTest;
        var
            result: Integer;
            
        function TestFunc(): Integer;
        begin
            TestFunc := 42;
            Exit();
            TestFunc := 99;  { This should not execute }
        end;
        
        begin
            result := TestFunc();
        end.
"""
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
        interpreter.interpret()
        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["result"].value, 42)
        self.assertEqual(ar.nesting_level, 1)

    def test_exit_for_nested_procedure_call(self):
        text = """\
        program ExitExample;

        var 
            count : Integer;

        procedure Proc4;
        begin
            count := count + 4;
        end;

        procedure Proc3;
        begin
            count := count + 3;
            {模拟某种条件，这里直接使用Exit提前退出}
            if true then  {可以替换为实际条件}
                Exit();  {提前退出Proc3，不会执行下面的代码}
            Proc4();
        end;

        procedure Proc2;
        begin
            count := count + 2;
            Proc3();    
        end;

        procedure Proc1;
        begin
            count := count + 1;
            Proc2();
        end;

        begin
            Proc1();
        end.
"""
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["count"].value, 6)
        self.assertEqual(ar.nesting_level, 1)

    def test_exit_for__nested_function_call(self):
        text = """\
        program SumExample;

        var 
            sum : Integer;

        function Sum4(): integer;
        begin
        Sum4 := 4;
        end;

        function Sum3(): integer;
        begin
        Sum3 := 3;
        {模拟条件，这里直接提前返回}
        if true then  {可以替换为实际条件}
            Exit();  {提前退出Sum3，不会执行下面的加法}
        Sum3 := Sum3 + Sum4();  {这行不会执行}
        end;

        function Sum2(): integer;
        begin
        Sum2 := 2 + Sum3();
        end;

        function Sum1(): integer;
        begin
        Sum1 := 1 + Sum2();
        end;

        begin
        sum := Sum1();
        end.
"""
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["sum"].value, 6)
        self.assertEqual(ar.nesting_level, 1)

    def test_exit_within_if_statement(self):
        """Test Exit within if statements in procedures"""
        text = """\
        program ExitInIfTest;
        var
            count: Integer;
            
        procedure ConditionalExit(condition: Boolean);
        begin
            count := count + 1;
            if condition then
            begin
                count := count + 10;
                Exit();
                count := count + 100;  { Should not execute }
            end;
            count := count + 1000;  { Should not execute if condition is true }
        end;
        
        begin
            count := 0;
            ConditionalExit(True);   { Should result in count = 11 }
        end.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["count"].value, 11)
        self.assertEqual(ar.nesting_level, 3)

    def test_exit_function_return_value_preservation(self):
        """Test that function return values are preserved when Exit is called"""
        text = """\
        program FunctionReturnPreservationTest;
        var
            result1, result2: Integer;
            
        function SetAndExit(value: Integer): Integer;
        begin
            SetAndExit := value;
            Exit();
            SetAndExit := 999;  { This should not change the return value }
        end;
        
        function SetBeforeAndAfterExit(value: Integer): Integer;
        begin
            SetBeforeAndAfterExit := value;
            if value > 0 then
            begin
                SetBeforeAndAfterExit := value * 2;
                Exit();
            end;
            SetBeforeAndAfterExit := 0;  { Should not execute }
        end;
        
        begin
            result1 := SetAndExit(42);
            result2 := SetBeforeAndAfterExit(5);
        end.
"""
        interpreter = makeInterpreter(text, MockFunctionCallStack())
        interpreter.interpret()
        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["result1"].value, 42)
        self.assertEqual(ar["result2"].value, 10)

    def test_exit_call_stack_behavior(self):
        """Test proper call stack behavior with Exit"""
        text = """\
        program CallStackExitTest;
        var
            depthBefore, depthAfter: Integer;
            
        procedure CheckDepthAndExit;
        begin
            { For this test, we just verify Exit doesn't break the stack }
            Exit();
        end;
        
        procedure WrapperProc;
        begin
            CheckDepthAndExit();
        end;
        
        begin
            WrapperProc();
        end.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()
        self.assertEqual(ar.nesting_level, 4)
        # Test passes if no exceptions are thrown and program completes

    def test_function_with_multiple_return_assignments(self):
        """Test Exit with multiple return value assignments in functions"""
        text = """\
        program MultipleReturnAssignmentTest;
        var
            result: Integer;
            
        function MultiAssign(x: Integer): Integer;
        begin
            MultiAssign := x;        { First assignment }
            MultiAssign := x * 2;    { Second assignment }
            if x > 0 then
            begin
                MultiAssign := x * 3;    { Third assignment }
                Exit();                  { Exit with x * 3 }
            end;
            MultiAssign := x * 4;    { Should not execute }
        end;
        
        begin
            result := MultiAssign(5);  { Should return 15 (5 * 3) }
        end.
"""
        interpreter = makeInterpreter(text, MockFunctionCallStack())
        interpreter.interpret()
        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["result"].value, 15)

    def test_program_with_simple_early_exit(self):
        """Test Exit with multiple return value assignments in functions"""
        text = """\
        program ProgramEarlyExit;

        var 
            i : Integer;

        begin
            i := 100;
            Exit();
            i := 999;
        end.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["i"].value, 100)

    def test_program_with_nested_early_exit(self):
        """Test Exit with multiple return value assignments in functions"""
        text = """\
        program ProgramEarlyExit;

        var 
            i : Integer;
            result : Integer;

        procedure TestProc();
        begin
            i := i + 200;  {  在过程内修改 i，测试是否影响主程序 }
            Exit();      {  提前退出 TestProc，不会执行下面的代码 }
            i := 999;  {  这行不会执行 }
        end;

        function TestFunc(): Integer;
        begin
            TestFunc := 50;  {  设置返回值 }
            if i > 150 then  {  模拟条件，使用 Exit 提前返回 }
                Exit();        {  提前退出 TestFunc，返回 50，不会执行下面的代码 }
            TestFunc := 75;  {  这行不会执行（因为 i=100 <150? 等下，i=100后调用Proc改成200，所以>150，Exit） }
        end;

        begin
            i := 100;
            TestProc();  {  调用过程，内部有 Exit，但只退出过程，主程序继续 }
            result := i +  TestFunc();  {  调用函数，内部有 Exit，但只退出函数，返回值正常 }
            Exit();  {  主程序的 Exit 提前终止整个程序，不会执行下面的代码 }
            i := 999 + i;
        end.
"""
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
        interpreter.interpret()
        ar = interpreter.call_stack.peek()
        self.assertEqual(ar["i"].value, 300)
        self.assertEqual(ar["result"].value, 350)


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

    def test_array_range_with_expression(self):
        """Test break and continue in nested loops"""
        text = """
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
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertIsInstance(ar["arr"], ArrayObject)
        self.assertEqual(ar["i"].value, 4)

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
        self.assertEqual(len(ar["intArr"].value), 0)
        self.assertEqual(len(ar["boolArr"].value), 0)
        self.assertEqual(len(ar["realArr"].value), 0)
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
    a : integer;
    b : boolean;
    c : real;
    d : integer;    
begin
    a := intArr[100];
    b := boolArr[100];
    c := realArr[100];
    d := nestArr[100][100];
end.
    """
        with self.assertRaises(InterpreterError) as cm:
            interpreter = makeInterpreter(text)
            interpreter.interpret()

            ar = interpreter.call_stack.peek()
            self.assertEqual(ar.nesting_level, 1)
        self.assertIsInstance(cm.exception, InterpreterError)
        self.assertEqual(
            cm.exception.error_code, ErrorCode.INTERPRETER_ARRAY_INDEX_OUT_OF_BOUNDS
        )

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
        self.assertEqual(ar["str1"], "abcdefg")
        self.assertEqual(ar["str2"], "abcdefghijklmnopqrstuvwxyz")
        self.assertEqual(ar["str3"], "abcdefghijklmn")
        self.assertEqual(ar["l1"].value, 7)
        self.assertEqual(ar["l2"].value, 26)
        self.assertEqual(ar["l3"].value, 14)
        self.assertEqual(ar["a"].value, "a")
        self.assertEqual(ar["b"].value, "")
        self.assertEqual(ar["concat1"], "abcdefg123456a")
        self.assertEqual(ar["concat2"], "abcdefg")
        # MockCallStack will not pop ActivationRecord
        self.assertEqual(ar.nesting_level, 5)

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
        self.assertEqual(ar["chars"].value[1], "H")
        self.assertEqual(ar["chars"].value[2].value, "e")
        self.assertEqual(ar["chars"].value[3], "y")
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
        self.assertEqual(ar["name"], "Alice")
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
        self.assertEqual(ar["name"], "Alice")
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
        self.assertEqual(ar["name"], "John Doe")
        self.assertIsInstance(ar["street"], StringObject)
        self.assertEqual(ar["street"], "123 Main St")
        self.assertIsInstance(ar["city"], StringObject)
        self.assertEqual(ar["city"], "Anytown")
        # In a more complete implementation, we would check the actual values

    def test_simple_variant_record(self):
        """Test nested record access"""
        text = """\
program SimpleVariantRecordExample;

type
    TShapeType = (Circle, Rectangle);
    
    TShape = record
        shapeType: TShapeType;
        case TShapeType of
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
        self.assertEqual(ar["s"], "ABC")
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
        self.assertEqual(ar["testString"], "Charlie")
        self.assertIsInstance(ar["testCharIndex"], IntegerObject)
        self.assertEqual(ar["testCharIndex"].value, 3)


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

    def test_procedure_registration(self):
        """Test that procedures are properly registered during declaration visits"""
        text = """\
program Main;

procedure Alpha(a : integer; b : integer);
var x : integer;
begin
   x := (a + b ) * 2;
end;

procedure Beta();
begin
   { empty procedure }
end;

begin { Main }
   { no procedure calls, just test registration }
end.  { Main }
"""
        interpreter = makeInterpreter(text)

        # Before interpretation, procedures should not be registered
        self.assertEqual(len(interpreter.user_procedures), 0)

        # Interpret the program (this should register procedures)
        interpreter.interpret()

        # After interpretation, procedures should be registered
        self.assertEqual(len(interpreter.user_procedures), 2)
        self.assertIn("ALPHA", interpreter.user_procedures)
        self.assertIn("BETA", interpreter.user_procedures)

        # Check Alpha procedure object
        alpha_proc = interpreter.user_procedures["ALPHA"]
        self.assertEqual(alpha_proc.name, "Alpha")
        self.assertEqual(alpha_proc.get_param_count(), 2)
        self.assertEqual(alpha_proc.get_param_names(), ["a", "b"])
        self.assertIsNotNone(alpha_proc.block_ast)

        # Check Beta procedure object
        beta_proc = interpreter.user_procedures["BETA"]
        self.assertEqual(beta_proc.name, "Beta")
        self.assertEqual(beta_proc.get_param_count(), 0)
        self.assertEqual(beta_proc.get_param_names(), [])
        self.assertIsNotNone(beta_proc.block_ast)

    def test_function_registration(self):
        """Test that functions are properly registered during declaration visits"""
        text = """\
program Main;

function Add(a : integer; b : integer) : integer;
var result : integer;
begin
   result := a + b;
   Add := result;
end;

function GetPi() : real;
begin
   GetPi := 3.14159;
end;

begin { Main }
   { no function calls, just test registration }
end.  { Main }
"""
        interpreter = makeInterpreter(text)

        # Before interpretation, functions should not be registered
        self.assertEqual(len(interpreter.user_functions), 0)

        # Interpret the program (this should register functions)
        interpreter.interpret()

        # After interpretation, functions should be registered
        self.assertEqual(len(interpreter.user_functions), 2)
        self.assertIn("ADD", interpreter.user_functions)
        self.assertIn("GETPI", interpreter.user_functions)

        # Check Add function object
        add_func = interpreter.user_functions["ADD"]
        self.assertEqual(add_func.name, "Add")
        self.assertEqual(add_func.get_param_count(), 2)
        self.assertEqual(add_func.get_param_names(), ["a", "b"])
        self.assertIsNotNone(add_func.block_ast)
        self.assertIsNotNone(add_func.return_type)

        # Check GetPi function object
        getpi_func = interpreter.user_functions["GETPI"]
        self.assertEqual(getpi_func.name, "GetPi")
        self.assertEqual(getpi_func.get_param_count(), 0)
        self.assertEqual(getpi_func.get_param_names(), [])
        self.assertIsNotNone(getpi_func.block_ast)
        self.assertIsNotNone(getpi_func.return_type)

    def test_forward_procedure_call(self):
        text = """\
        program ForwardSafeExample;

        var 
            count:integer;

        procedure Proc1(); forward;

        procedure Proc2();
        begin
            Proc1();
        end;

        procedure Proc1();
        begin   
            count := 10;
        end;

        { 主程序 }
        begin
            Proc2();
        end.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["count"].value, 10)
        self.assertEqual(ar.nesting_level, 3)

    def test_forward_function_call(self):
        text = """\
            program ForwardFunction;

            var 
                sum : Integer;

            function Add(a, b: Integer): Integer; forward;

            function DoubleAdd(a,b:Integer):Integer;
                begin
                DoubleAdd := Add(a,b) + Add(a,b);
                end;

            function Add(a, b: Integer): Integer;
                begin
                Add := a + b;
                end;

            begin
                sum := DoubleAdd(3,4);
            end.
"""
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["sum"].value, 14)
        self.assertEqual(ar.nesting_level, 1)

    def test_builtin_procedure_refer_variable(self):
        text = """\
        program UserProcedureReferVariable;

        type 
            TPerson = record
                age: Integer;
            end;

        var 
            person : TPerson;   
            arr : array of Integer;
            i , age : Integer;

        begin
            person.age := 0;
            Inc(person.age);
            age := person.age;
            arr[1] := 1;
            Inc(arr[1]);
            i := arr[1];
        end.
"""
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertIsInstance(ar["person"], RecordObject)
        self.assertIsInstance(ar["arr"], ArrayObject)
        self.assertEqual(ar["age"].value, 1)
        self.assertEqual(ar["i"].value, 2)
        self.assertEqual(ar.nesting_level, 1)

    def test_user_procedure_refer_variable(self):
        text = """\
        program UserProcedureReferVariable;

        type 
            TPerson = record
                age: Integer;
            end;

        var 
            person : TPerson;
            arr : array of Integer;
            age : Integer;
            i : Integer;

        procedure Incr(var x : Integer);
        begin
            x := x + 1;
        end;


        begin
            person.age := 0;
            Incr(person.age);
            age := person.age;

            arr[1] := 1;
            Incr(arr[1]);
            i := arr[1];
        end.
"""
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertIsInstance(ar["person"], RecordObject)
        self.assertIsInstance(ar["arr"], ArrayObject)
        self.assertEqual(ar["age"].value, 1)
        self.assertEqual(ar["i"].value, 2)
        self.assertEqual(ar.nesting_level, 1)

    def test_user_function_refer_variable(self):
        text = """\
        program UserFunctionReferVariable;

        type 
            TPerson = record
                age: Integer;
            end;

        FUNCTION ADD(var x , y : Integer): Integer;
            begin
                x := x * 2;
                y := y * 2;
                ADD := x + y;
            end;

        var 
            person : TPerson;
            arr : array of Integer;
            result : Integer;
            formerAge , afterAge : Integer;
            formerItem, afterItem : Integer;

        begin
            person.age := 3;
            arr[1] := 4;
            formerAge := person.age;
            formerItem := arr[1];
            result := ADD(person.age, arr[1]);
            afterAge := person.age;
            afterItem := arr[1];
        end.
"""
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertIsInstance(ar["person"], RecordObject)
        self.assertIsInstance(ar["arr"], ArrayObject)
        self.assertEqual(ar["result"].value, 14)
        self.assertEqual(ar["formerAge"].value, 3)
        self.assertEqual(ar["formerItem"].value, 4)
        self.assertEqual(ar["afterAge"].value, 6)
        self.assertEqual(ar["afterItem"].value, 8)
        self.assertEqual(ar.nesting_level, 1)

    def test_track_recursion_for_normal_param(self):
        text = """\
        program TrackRecursion;

        var 
            count : Integer;
            i : Integer;
            lastValueIndex : Integer;
            track1 : Integer;
            track2 : Integer;
            track3 : Integer;
            track4 : Integer;
            track5 : Integer;

        procedure CallIt(valueIndex:Integer);
        begin
            if track1 = 0 then
                track1 := valueIndex
            else if track2 = 0 then
                track2 := valueIndex
            else if track3 = 0 then
                track3 := valueIndex
            else if track4 = 0 then
                track4 := valueIndex
            else if track5 = 0 then
                track5 := valueIndex;
            if count < 5 then
            begin
                count := count + 1;
                valueIndex := valueIndex + 1;
                CallIt(valueIndex);
            end;
            lastValueIndex := valueIndex;
        end;

        begin
            i := 0;
            CallIt(i);
        end.
"""
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["track1"].value, 1)
        self.assertEqual(ar["track2"].value, 2)
        self.assertEqual(ar["track3"].value, 3)
        self.assertEqual(ar["track4"].value, 4)
        self.assertEqual(ar["track5"].value, 5)
        self.assertEqual(ar["lastValueIndex"].value, 1)
        self.assertEqual(ar.nesting_level, 1)

    def test_track_recursion_for_reference_param(self):
        text = """\
        program TrackRecursion;

        var 
            count : Integer;
            i : Integer;
            lastValueIndex : Integer;
            track1 : Integer;
            track2 : Integer;
            track3 : Integer;
            track4 : Integer;
            track5 : Integer;

        procedure CallIt(var valueIndex:Integer);
        begin
            if track1 = 0 then
                track1 := valueIndex
            else if track2 = 0 then
                track2 := valueIndex
            else if track3 = 0 then
                track3 := valueIndex
            else if track4 = 0 then
                track4 := valueIndex
            else if track5 = 0 then
                track5 := valueIndex;
            if count < 5 then
            begin
                count := count + 1;
                valueIndex := valueIndex + 1;
                CallIt(valueIndex);
            end;
            lastValueIndex := valueIndex;
        end;

        begin
            i := 0;
            CallIt(i);
        end.
"""
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["track1"].value, 1)
        self.assertEqual(ar["track2"].value, 2)
        self.assertEqual(ar["track3"].value, 3)
        self.assertEqual(ar["track4"].value, 4)
        self.assertEqual(ar["track5"].value, 5)
        self.assertEqual(ar["lastValueIndex"].value, 5)
        self.assertEqual(ar.nesting_level, 1)


class InterpreterTestCase(unittest.TestCase):
    def test_record_post_initialize_complex_type(self):
        text = """\
program JSONParserNoPointers;

const
  HASH_TABLE_SIZE = 2;
  MAX_VALUES = 2;

var 
  former : Integer;
  after : Integer;

type
  TJSONType = (jtNull, jtObject);

  TJSONObject = record
    HashTable: array[0..HASH_TABLE_SIZE-1] of Integer;
  end;

  TJSONValue = record
    JSONType: TJSONType;
    case TJSONType of
      jtNull: ();
      jtObject: (ObjectValue: TJSONObject);
  end;

var
  JSONValues: array[1..MAX_VALUES] of TJSONValue;

begin
  JSONValues[1].JSONType := jtNull;
  JSONValues[1].JSONType := jtObject;
  former := JSONValues[1].ObjectValue.HashTable[1];
  JSONValues[1].ObjectValue.HashTable[1] := 5;
  after := JSONValues[1].ObjectValue.HashTable[1];
end.
"""
        interpreter = makeInterpreter(text)
        interpreter.interpret()

        ar = interpreter.call_stack.peek()
        self.assertIsInstance(ar["JSONValues"], ArrayObject)
        self.assertEqual(ar["former"].value, 0)
        self.assertEqual(ar["after"].value, 5)

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


class InterpreterConstTestCase(unittest.TestCase):
    def test_const_declaration_initialization(self):
        """Test that const declarations are properly initialized"""
        text = """\
        PROGRAM TestConstInit;
        CONST
            PI = 3.14159;
            MAX_SIZE = 100;
            MESSAGE = 'Hello World';
            FLAG = TRUE;
        VAR
            pi_val : REAL;
            size_val : INTEGER;
            msg_val : STRING;
            flag_val : BOOLEAN;
        BEGIN
            pi_val := PI;
            size_val := MAX_SIZE;
            msg_val := MESSAGE;
            flag_val := FLAG;
        END.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["pi_val"].value, 3.14159)
        self.assertEqual(ar["size_val"].value, 100)
        self.assertEqual(ar["msg_val"], "Hello World")
        self.assertEqual(ar["flag_val"].value, True)

    def test_const_in_expressions(self):
        """Test that const values can be used in expressions"""
        text = """\
        PROGRAM TestConstExpr;
        CONST
            PI = 3.14159;
            RADIUS = 5.0;
        VAR
            area : REAL;
            circumference : REAL;
        BEGIN
            area := PI * RADIUS * RADIUS;
            circumference := 2.0 * PI * RADIUS;
        END.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        expected_area = 3.14159 * 5.0 * 5.0
        expected_circumference = 2.0 * 3.14159 * 5.0

        self.assertAlmostEqual(ar["area"].value, expected_area, places=5)
        self.assertAlmostEqual(
            ar["circumference"].value, expected_circumference, places=5
        )

    def test_const_parameters_in_procedures(self):
        """Test that const parameters work correctly in procedures"""
        text = """\
        PROGRAM TestConstProcParams;
        VAR
            x, y, z : INTEGER;
            result : INTEGER;
        
        PROCEDURE Calculate(CONST a : INTEGER; VAR b : INTEGER; c : INTEGER);
        BEGIN
            b := a + c;
            c := a * 2;  { This modifies the local copy, not the original }
        END;
        
        BEGIN
            x := 10;
            y := 0;
            z := 5;
            Calculate(x, y, z);
            result := y;  { Should be 15 (10 + 5) }
        END.
        """
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["x"].value, 10)  # const parameter, original unchanged
        self.assertEqual(ar["y"].value, 15)
        self.assertEqual(ar["z"].value, 5)  # value parameter, original unchanged
        self.assertEqual(ar["result"].value, 15)

    def test_const_parameters_in_functions(self):
        """Test that const parameters work correctly in functions"""
        text = """\
        PROGRAM TestConstFuncParams;
        VAR
            result : INTEGER;
        
        FUNCTION Multiply(CONST a : INTEGER; b : INTEGER) : INTEGER;
        BEGIN
            b := b * 2;  { Modify value parameter }
            Multiply := a * b;  { Use const parameter }
        END;
        
        BEGIN
            result := Multiply(5, 3);  { Should be 5 * (3 * 2) = 30 }
        END.
        """
        interpreter = makeInterpreter(text, MockFunctionCallStack())
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["result"].value, 30)

    def test_mixed_parameter_modes(self):
        """Test mixed parameter modes (const, var, value) in the same procedure"""
        text = """\
        PROGRAM TestMixedParams;
        VAR
            a, b, c : INTEGER;
            result1, result2 : INTEGER;
        
        PROCEDURE MixedProc(CONST x : INTEGER; VAR y : INTEGER; z : INTEGER);
        BEGIN
            y := x + z;  { const + value -> var }
            z := x * 2;  { modify value parameter (local copy) }
        END;
        
        FUNCTION MixedFunc(CONST p : INTEGER; VAR q : INTEGER) : INTEGER;
        BEGIN
            q := p * 3;
            MixedFunc := p + q;
        END;
        
        BEGIN
            a := 10;
            b := 0;
            c := 5;
            
            MixedProc(a, b, c);
            result1 := b;  { Should be 15 }
            
            result2 := MixedFunc(a, b);  { Should be 10 + (10 * 3) = 40 }
        END.
        """
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["a"].value, 10)  # const parameter, original unchanged
        self.assertEqual(ar["b"].value, 30)  # var parameter, modified by function
        self.assertEqual(ar["c"].value, 5)  # value parameter, original unchanged
        self.assertEqual(ar["result1"].value, 15)
        self.assertEqual(ar["result2"].value, 40)

    def test_const_with_complex_expressions(self):
        """Test const declarations with complex expressions"""
        text = """\
        PROGRAM TestConstComplexExpr;
        CONST
            BASE = 10;
            MULTIPLIER = 2;
            RESULT = BASE * MULTIPLIER + 5;  { Should be 25 }
        VAR
            value : INTEGER;
        BEGIN
            value := RESULT;
        END.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["value"].value, 25)

    def test_const_type_inference(self):
        """Test that const declarations properly infer types"""
        text = """\
        PROGRAM TestConstTypes;
        CONST
            INT_CONST = 42;
            REAL_CONST = 3.14;
            BOOL_CONST = TRUE;
            CHAR_CONST = 'A';
            STRING_CONST = 'Hello';
        VAR
            i : INTEGER;
            r : REAL;
            b : BOOLEAN;
            c : CHAR;
            s : STRING;
        BEGIN
            i := INT_CONST;
            r := REAL_CONST;
            b := BOOL_CONST;
            c := CHAR_CONST;
            s := STRING_CONST;
        END.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["i"].value, 42)
        self.assertEqual(ar["r"].value, 3.14)
        self.assertEqual(ar["b"].value, True)
        self.assertEqual(ar["c"].value, "A")
        self.assertEqual(ar["s"], "Hello")

    def test_const_in_nested_procedures(self):
        """Test const parameters in nested procedure calls"""
        text = """\
        PROGRAM TestNestedConstParams;
        VAR
            result : INTEGER;
            temp : INTEGER;
        
        PROCEDURE Outer(CONST a : INTEGER);
                PROCEDURE Inner(CONST b : INTEGER; VAR c : INTEGER);
                    BEGIN
                        c := a + b;  { Use const from outer scope and inner parameter }
                    END;
                
            BEGIN
                temp := 0;
                Inner(a, temp);
                result := temp;
            END;
        
        BEGIN
            Outer(10);  { Should result in result = 10 + 10 = 20 }
        END.
        """
        interpreter = makeInterpreter(
            text=text, mock_call_stack=MockFunctionCallStack()
        )
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["result"].value, 20)


class InterpreterLoopControlTestCase(unittest.TestCase):
    def test_break_in_while_loop(self):
        """Test break statement in while loop"""
        text = """\
        program TestBreakWhile;
        var i: integer;
        begin
            i := 1;
            while i <= 10 do
            begin
                if i = 5 then
                    break;
                i := i + 1;
            end;
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Loop should break when i = 5, so i should still be 5
        self.assertEqual(ar["i"].value, 5)

    def test_continue_in_while_loop(self):
        """Test continue statement in while loop"""
        text = """\
        program TestContinueWhile;
        var i, sum: integer;
        begin
            i := 0;
            sum := 0;
            while i < 10 do
            begin
                i := i + 1;
                if i mod 2 = 0 then
                    continue;
                sum := sum + i;
            end;
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Should sum only odd numbers: 1 + 3 + 5 + 7 + 9 = 25
        self.assertEqual(ar["sum"].value, 25)
        self.assertEqual(ar["i"].value, 10)

    def test_break_in_for_loop(self):
        """Test break statement in for loop"""
        text = """\
        program TestBreakFor;
        var i, sum: integer;
        begin
            sum := 0;
            for i := 1 to 10 do
            begin
                if i = 6 then
                    break;
                sum := sum + i;
            end;
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Should sum 1 + 2 + 3 + 4 + 5 = 15
        self.assertEqual(ar["sum"].value, 15)
        # i should be 6 when break occurred
        self.assertEqual(ar["i"].value, 6)

    def test_continue_in_for_loop(self):
        """Test continue statement in for loop"""
        text = """\
        program TestContinueFor;
        var i, sum: integer;
        begin
            sum := 0;
            for i := 1 to 10 do
            begin
                if i mod 2 = 0 then
                    continue;
                sum := sum + i;
            end;
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Should sum only odd numbers: 1 + 3 + 5 + 7 + 9 = 25
        self.assertEqual(ar["sum"].value, 25)
        # i should be 10 after loop completion
        self.assertEqual(ar["i"].value, 10)

    def test_nested_loops_with_break(self):
        """Test that break only affects the innermost loop"""
        text = """\
        program TestNestedBreak;
        var i, j, count: integer;
        begin
            count := 0;
            for i := 1 to 3 do
            begin
                for j := 1 to 3 do
                begin
                    if j = 2 then
                        break;
                    count := count + 1;
                end;
            end;
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Inner loop breaks at j=2, so only j=1 is counted for each i
        # Should count 3 times (once for each i)
        self.assertEqual(ar["count"].value, 3)
        self.assertEqual(ar["i"].value, 3)

    def test_nested_loops_with_continue(self):
        """Test that continue only affects the innermost loop"""
        text = """\
        program TestNestedContinue;
        var i, j, count: integer;
        begin
            count := 0;
            for i := 1 to 3 do
            begin
                for j := 1 to 3 do
                begin
                    if j = 2 then
                        continue;
                    count := count + 1;
                end;
            end;
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Inner loop skips j=2, so j=1 and j=3 are counted for each i
        # Should count 6 times (2 for each of 3 i values)
        self.assertEqual(ar["count"].value, 6)
        self.assertEqual(ar["i"].value, 3)

    def test_nested_for_while_break(self):
        """Test break in nested for-while loops - break should only affect innermost while loop"""
        text = """\
        program TestNestedForWhileBreak;
        var i, j, outer_count, inner_count: integer;
        begin
            outer_count := 0;
            inner_count := 0;
            for i := 1 to 3 do
            begin
                outer_count := outer_count + 1;
                j := 1;
                while j <= 5 do
                begin
                    inner_count := inner_count + 1;
                    if j = 3 then
                        break;  { Should only break the while loop }
                    j := j + 1;
                end;
            end;
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Outer for loop should complete all 3 iterations
        self.assertEqual(ar["outer_count"].value, 3)
        # Inner while loop breaks at j=3, so runs 3 times per outer iteration: 3 * 3 = 9
        self.assertEqual(ar["inner_count"].value, 9)
        self.assertEqual(ar["i"].value, 3)
        # j should be 3 from the last iteration where break occurred
        self.assertEqual(ar["j"].value, 3)

    def test_nested_while_for_continue(self):
        """Test continue in nested while-for loops - continue should only affect innermost for loop"""
        text = """\
        program TestNestedWhileForContinue;
        var i, j, outer_count, inner_count: integer;
        begin
            outer_count := 0;
            inner_count := 0;
            i := 1;
            while i <= 2 do
            begin
                outer_count := outer_count + 1;
                for j := 1 to 4 do
                begin
                    if j = 2 then
                        continue;  { Should only skip current for loop iteration }
                    inner_count := inner_count + 1;
                end;
                i := i + 1;
            end;
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Outer while loop should complete 2 iterations
        self.assertEqual(ar["outer_count"].value, 2)
        # Inner for loop skips j=2, so counts j=1,3,4 per outer iteration: 3 * 2 = 6
        self.assertEqual(ar["inner_count"].value, 6)
        self.assertEqual(ar["i"].value, 3)
        self.assertEqual(ar["j"].value, 4)

    def test_triple_nested_loops_break(self):
        """Test break in triple nested loops - should only affect innermost loop"""
        text = """\
        program TestTripleNestedBreak;
        var i, j, k, count: integer;
        begin
            count := 0;
            for i := 1 to 2 do
            begin
                for j := 1 to 2 do
                begin
                    for k := 1 to 5 do
                    begin
                        if k = 3 then
                            break;  { Should only break the innermost k loop }
                        count := count + 1;
                    end;
                end;
            end;
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Innermost loop breaks at k=3, so counts k=1,2 for each i,j combination
        # 2 counts per (i,j) pair * 2 i values * 2 j values = 8
        self.assertEqual(ar["count"].value, 8)
        self.assertEqual(ar["i"].value, 2)
        self.assertEqual(ar["j"].value, 2)
        # k should be 3 from the last break
        self.assertEqual(ar["k"].value, 3)

    def test_triple_nested_loops_continue(self):
        """Test continue in triple nested loops - should only affect innermost loop"""
        text = """\
        program TestTripleNestedContinue;
        var i, j, k, count: integer;
        begin
            count := 0;
            for i := 1 to 2 do
            begin
                for j := 1 to 2 do
                begin
                    for k := 1 to 4 do
                    begin
                        if k = 2 then
                            continue;  { Should only skip current k iteration }
                        count := count + 1;
                    end;
                end;
            end;
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Innermost loop skips k=2, so counts k=1,3,4 for each i,j combination
        # 3 counts per (i,j) pair * 2 i values * 2 j values = 12
        self.assertEqual(ar["count"].value, 12)
        self.assertEqual(ar["i"].value, 2)
        self.assertEqual(ar["j"].value, 2)
        self.assertEqual(ar["k"].value, 4)

    def test_nested_loops_outer_continues_after_inner_break(self):
        """Test that outer loop continues normally after inner loop breaks"""
        text = """\
        program TestOuterContinuesAfterInnerBreak;
        var i, j, outer_iterations, inner_breaks: integer;
        begin
            outer_iterations := 0;
            inner_breaks := 0;
            for i := 1 to 4 do
            begin
                outer_iterations := outer_iterations + 1;
                for j := 1 to 10 do
                begin
                    if j = 3 then
                    begin
                        inner_breaks := inner_breaks + 1;
                        break;  { Break inner loop }
                    end;
                end;
                { This code should execute after inner loop breaks }
            end;
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # Outer loop should complete all 4 iterations despite inner breaks
        self.assertEqual(ar["outer_iterations"].value, 4)
        # Inner loop should break 4 times (once per outer iteration)
        self.assertEqual(ar["inner_breaks"].value, 4)
        self.assertEqual(ar["i"].value, 4)
        # j should be 3 from the last break
        self.assertEqual(ar["j"].value, 3)

    def test_nested_loops_complex_control_flow(self):
        """Test complex nested loop scenario with both break and continue"""
        text = """\
        program TestComplexNestedControl;
        var i, j, processed, skipped, early_exits: integer;
        begin
            processed := 0;
            skipped := 0;
            early_exits := 0;
            for i := 1 to 3 do
            begin
                for j := 1 to 6 do
                begin
                    if j = 2 then
                    begin
                        skipped := skipped + 1;
                        continue;  { Skip j=2 }
                    end;
                    if j = 5 then
                    begin
                        early_exits := early_exits + 1;
                        break;  { Break at j=5 }
                    end;
                    processed := processed + 1;
                end;
            end;
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # For each outer iteration: j=1 (processed), j=2 (skipped), j=3,4 (processed), j=5 (break)
        # So 3 processed per outer iteration * 3 outer iterations = 9
        self.assertEqual(ar["processed"].value, 9)
        # j=2 skipped once per outer iteration * 3 = 3
        self.assertEqual(ar["skipped"].value, 3)
        # Break at j=5 once per outer iteration * 3 = 3
        self.assertEqual(ar["early_exits"].value, 3)
        self.assertEqual(ar["i"].value, 3)
        self.assertEqual(ar["j"].value, 5)


if __name__ == "__main__":
    unittest.main()


class InterpreterSubrangeSetTestCase(unittest.TestCase):
    """Test cases for subrange types, set literals, and in operator evaluation"""

    def test_subrange_evaluation_simple_integers(self):
        """Test subrange evaluation with simple integer bounds"""
        text = """\
        program TestSubrangeEvaluation;
        var
            result: boolean;
        begin
            result := 7 in 1..10;
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # 7 should be in range 1..10
        self.assertEqual(ar["result"].value, True)

    def test_subrange_evaluation_with_expressions(self):
        """Test subrange evaluation with expression bounds"""
        text = """\
        program TestSubrangeExpressions;
        var
            min, max: integer;
            result: boolean;
        begin
            min := 5;
            max := 15;
            result := 10 in min..max;
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # 10 should be in range 5..15
        self.assertEqual(ar["result"].value, True)

    def test_subrange_invalid_bounds_runtime_error(self):
        """Test that invalid subrange bounds (lower > upper) raise runtime error"""
        text = """\
        program TestInvalidSubrange;
        var
            lower, upper: integer;
            result: boolean;
        begin
            lower := 10;
            upper := 5;
            result := 7 in lower..upper;
        end.
        """
        interpreter = makeInterpreter(text)

        with self.assertRaises(InterpreterError) as cm:
            interpreter.interpret()

        the_exception = cm.exception
        self.assertEqual(the_exception.error_code.name, "INTERPRETER_SUBRANGE_INVALID")

    def test_set_literal_evaluation_individual_elements(self):
        """Test set literal evaluation with individual elements"""
        text = """\
        program TestSetLiteralIndividual;
        var
            result1, result2, result3: boolean;
        begin
            result1 := 1 in [1, 3, 5, 7];
            result2 := 2 in [1, 3, 5, 7];
            result3 := 5 in [1, 3, 5, 7];
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["result1"].value, True)  # 1 is in set
        self.assertEqual(ar["result2"].value, False)  # 2 is not in set
        self.assertEqual(ar["result3"].value, True)  # 5 is in set

    def test_set_literal_evaluation_with_ranges(self):
        """Test set literal evaluation with range elements"""
        text = """\
        program TestSetLiteralRanges;
        var
            result1, result2, result3, result4: boolean;
        begin
            result1 := 2 in [1..5, 8..10];
            result2 := 6 in [1..5, 8..10];
            result3 := 9 in [1..5, 8..10];
            result4 := 11 in [1..5, 8..10];
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["result1"].value, True)  # 2 is in range 1..5
        self.assertEqual(ar["result2"].value, False)  # 6 is not in either range
        self.assertEqual(ar["result3"].value, True)  # 9 is in range 8..10
        self.assertEqual(ar["result4"].value, False)  # 11 is not in either range

    def test_set_literal_evaluation_mixed_elements(self):
        """Test set literal evaluation with mixed individual and range elements"""
        text = """\
        program TestSetLiteralMixed;
        var
            result1, result2, result3, result4, result5: boolean;
        begin
            result1 := 1 in [1, 3..5, 8, 10..12];
            result2 := 2 in [1, 3..5, 8, 10..12];
            result3 := 4 in [1, 3..5, 8, 10..12];
            result4 := 8 in [1, 3..5, 8, 10..12];
            result5 := 11 in [1, 3..5, 8, 10..12];
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["result1"].value, True)  # 1 is individual element
        self.assertEqual(ar["result2"].value, False)  # 2 is not in set
        self.assertEqual(ar["result3"].value, True)  # 4 is in range 3..5
        self.assertEqual(ar["result4"].value, True)  # 8 is individual element
        self.assertEqual(ar["result5"].value, True)  # 11 is in range 10..12

    def test_set_literal_evaluation_empty_set(self):
        """Test set literal evaluation with empty set"""
        text = """\
        program TestEmptySet;
        var
            result: boolean;
        begin
            result := 5 in [];
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["result"].value, False)  # Nothing is in empty set

    def test_set_literal_evaluation_character_elements(self):
        """Test set literal evaluation with character elements (converted to ASCII)"""
        text = """\
        program TestCharacterSet;
        var
            result1, result2: boolean;
        begin
            result1 := 65 in [65, 67, 69];  { ASCII values for 'A', 'C', 'E' }
            result2 := 66 in [65, 67, 69];  { ASCII value for 'B' }
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["result1"].value, True)  # 65 ('A') is in set
        self.assertEqual(ar["result2"].value, False)  # 66 ('B') is not in set

    def test_in_operator_evaluation_with_subrange(self):
        """Test in operator evaluation with subrange operands"""
        text = """\
        program TestInOperatorSubrange;
        var
            result1, result2, result3: boolean;
        begin
            result1 := 5 in 1..10;
            result2 := 0 in 1..10;
            result3 := 15 in 1..10;
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["result1"].value, True)  # 5 is in range 1..10
        self.assertEqual(ar["result2"].value, False)  # 0 is not in range 1..10
        self.assertEqual(ar["result3"].value, False)  # 15 is not in range 1..10

    def test_in_operator_evaluation_with_char_subrange(self):
        """Test in operator evaluation with subrange operands"""
        text = """\
        program TestCharacterSet;
        var
            result1, result2: boolean;
        begin
            result1 := 'A' in [#65, #67, #69];  { ASCII values for 'A', 'C', 'E' }
            result2 := #66 in ['A', 'C', 'E'];  { ASCII value for 'B' }
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["result1"].value, True)  # 5 is in range 1..10
        self.assertEqual(ar["result2"].value, False)  # 0 is not in range 1..10

    def test_in_operator_evaluation_with_set_literal(self):
        """Test in operator evaluation with set literal operands"""
        text = """\
        program TestInOperatorSetLiteral;
        var
            result1, result2, result3: boolean;
        begin
            result1 := 3 in [1, 3, 5];
            result2 := 4 in [1, 3, 5];
            result3 := 7 in [1..5, 7..9];
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["result1"].value, True)  # 3 is in set [1, 3, 5]
        self.assertEqual(ar["result2"].value, False)  # 4 is not in set [1, 3, 5]
        self.assertEqual(ar["result3"].value, True)  # 7 is in range 7..9

    def test_in_operator_evaluation_complex_expressions(self):
        """Test in operator evaluation with complex expressions"""
        text = """\
        program TestInOperatorComplex;
        var
            x, y: integer;
            result1, result2: boolean;
        begin
            x := 3;
            y := 7;
            result1 := (x + y) in [5..15];
            result2 := (x * y) in [1, 5, 10, 21, 25];
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["result1"].value, True)  # (3 + 7) = 10 is in range 5..15
        self.assertEqual(ar["result2"].value, True)  # (3 * 7) = 21 is in set

    def test_subrange_bounds_validation_complex_expressions(self):
        """Test subrange bounds validation with complex expressions at runtime"""
        text = """\
        program TestSubrangeBoundsValidation;
        var
            a, b, c: integer;
            result: boolean;
        begin
            a := 5;
            b := 3;
            c := 2;
            result := 4 in (a - c)..(b + c * 2);
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        # (5 - 2)..(3 + 2 * 2) = 3..7, so 4 should be in range
        self.assertEqual(ar["result"].value, True)

    def test_nested_set_operations_in_conditionals(self):
        """Test using set operations in conditional statements"""
        text = """\
        program TestNestedSetOperations;
        var
            grade: integer;
            result: string;
        begin
            grade := 85;
            if grade in [90..100] then
                result := 'A'
            else if grade in [80..89] then
                result := 'B'
            else if grade in [70..79] then
                result := 'C'
            else
                result := 'F';
        end.
        """
        interpreter = makeInterpreter(text)
        interpreter.interpret()
        ar = interpreter.call_stack.peek()

        self.assertEqual(ar["result"], "B")  # 85 is in range 80..89
