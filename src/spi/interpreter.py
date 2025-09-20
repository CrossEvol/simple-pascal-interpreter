###############################################################################
#                                                                             #
#  INTERPRETER                                                                #
#                                                                             #
###############################################################################

from __future__ import annotations

from enum import Enum
from typing import Callable, cast

from spi.ast_and_symbol import (
    AccessExpression,
    ArrayType,
    Assign,
    BinOp,
    Block,
    Bool,
    BuiltinFunctionSymbol,
    BuiltinProcedureSymbol,
    CaseStatement,
    Char,
    Compound,
    EnumType,
    EnumTypeSymbol,
    ForStatement,
    FunctionCall,
    FunctionDecl,
    IfStatement,
    IndexSuffix,
    MemberSuffix,
    NoOp,
    Num,
    PrimitiveType,
    ProcedureCall,
    ProcedureDecl,
    Program,
    RecordType,
    RecordTypeSymbol,
    String,
    StringType,
    Type,
    TypeDeclaration,
    UnaryOp,
    Var,
    VarDecl,
    WhileStatement,
)
from spi.constants import ElementType
from spi.error import (
    ErrorCode,
    InterpreterError,
    SemanticError,
)
from spi.native import NativeMethod
from spi.object import (
    ArrayObject,
    BooleanObject,
    CharObject,
    EnumObject,
    IntegerObject,
    NullObject,
    NumberObject,
    Object,
    RealObject,
    RecordObject,
    StringObject,
)
from spi.token import TokenType
from spi.util import SpiUtil
from spi.visitor import NodeVisitor

RETURN_NUM_FOR_LENGTH = "RETURN_NUM_FOR_LENGTH"
_SHOULD_LOG_SCOPE = False  # see '--scope' command line option
_SHOULD_LOG_STACK = False  # see '--stack' command line option

# Built-in procedures and functions registry
BUILTIN_PROCEDURES: dict[str, Callable[..., None]] = {}
BUILTIN_FUNCTIONS: dict[str, Callable[..., Object]] = {}


def register_builtin_procedure(name, handler):
    """Register a built-in procedure handler"""
    BUILTIN_PROCEDURES[name.upper()] = handler


def register_builtin_function(name, handler):
    """Register a built-in function handler"""
    BUILTIN_FUNCTIONS[name.upper()] = handler


# Built-in procedure handlers
def handle_write(interpreter, node):
    """Handle WRITE built-in procedure"""
    proc_name = node.proc_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))

    # output actual params
    for argument_node in actual_params:
        obj = interpreter.visit(argument_node)
        # 对于枚举对象，打印名称而不是值
        if isinstance(obj, EnumObject):
            print(obj.name, end="")
        else:
            print(obj.value if hasattr(obj, "value") else obj, end="")

    interpreter.log(f"LEAVE: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))


def handle_writeln(interpreter, node):
    """Handle WRITELN built-in procedure"""
    proc_name = node.proc_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))

    # output actual params
    for argument_node in actual_params:
        obj = interpreter.visit(argument_node)
        # 对于枚举对象，打印名称而不是值
        if isinstance(obj, EnumObject):
            print(obj.name, end="")
        else:
            print(obj.value if hasattr(obj, "value") else obj, end="")
    print()
    interpreter.log(f"LEAVE: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))


def handle_setlength(interpreter, node):
    """Handle SETLENGTH built-in procedure"""
    proc_name = node.proc_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))

    # core
    arr_name = actual_params[0].value
    new_length_obj = interpreter.visit(actual_params[1])
    new_length = new_length_obj.value if isinstance(new_length_obj, NumberObject) else 0

    pre_ar = (
        interpreter.call_stack._records[-2]
        if len(interpreter.call_stack._records) >= 2
        else ar
    )
    var_obj = pre_ar.get(arr_name)

    if isinstance(var_obj, StringObject):
        var_obj.set_length(new_length)
    elif isinstance(var_obj, ArrayObject):
        var_obj.set_length(new_length)

    interpreter.log(f"LEAVE: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))


def handle_inc(interpreter, node):
    """Handle INC built-in procedure - increment a variable"""
    proc_name = node.proc_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))

    # Get the variable name and optional increment value
    var_name = actual_params[0].value
    increment = 1  # Default increment

    if len(actual_params) > 1:
        increment_obj = interpreter.visit(actual_params[1])
        if isinstance(increment_obj, NumberObject):
            increment = increment_obj.value

    # Get current value and increment it
    var_obj = ar.get(var_name)
    if isinstance(var_obj, NumberObject):
        new_value = var_obj.value + increment
        if isinstance(var_obj, IntegerObject):
            ar[var_name] = IntegerObject(new_value)
        else:
            ar[var_name] = RealObject(new_value)

    interpreter.log(f"LEAVE: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))


def handle_dec(interpreter, node):
    """Handle DEC built-in procedure - decrement a variable"""
    proc_name = node.proc_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))

    # Get the variable name and optional decrement value
    var_name = actual_params[0].value
    decrement = 1  # Default decrement

    if len(actual_params) > 1:
        decrement_obj = interpreter.visit(actual_params[1])
        if isinstance(decrement_obj, NumberObject):
            decrement = decrement_obj.value

    # Get current value and decrement it
    var_obj = ar.get(var_name)
    if isinstance(var_obj, NumberObject):
        new_value = var_obj.value - decrement
        if isinstance(var_obj, IntegerObject):
            ar[var_name] = IntegerObject(new_value)
        else:
            ar[var_name] = RealObject(new_value)

    interpreter.log(f"LEAVE: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))


# Built-in function handlers
def handle_length(interpreter, node):
    """Handle LENGTH built-in function"""
    func_name = node.func_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: FUNCTION {func_name}")
    interpreter.log(str(interpreter.call_stack))

    # Get the array/string object and return its length
    param_obj = interpreter.visit(actual_params[1])  # Skip the function name param
    if isinstance(param_obj, (ArrayObject, StringObject)):
        length_value = len(param_obj)
    else:
        length_value = 0

    result = IntegerObject(length_value)
    ar[RETURN_NUM_FOR_LENGTH] = result

    interpreter.log(f"LEAVE: FUNCTION {func_name}")
    interpreter.log(str(interpreter.call_stack))

    return result


def handle_ord(interpreter, node):
    """Handle ORD built-in function - returns ASCII code of a character or ordinal of an enum"""
    func_name = node.func_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: FUNCTION {func_name}")
    interpreter.log(str(interpreter.call_stack))

    # Get the character parameter and return its ASCII code
    param_obj = interpreter.visit(actual_params[1])  # Skip the function name param

    if isinstance(param_obj, CharObject):
        ascii_value = ord(param_obj.value) if param_obj.value else 0
    elif isinstance(param_obj, StringObject) and len(param_obj.value) > 0:
        ascii_value = ord(param_obj.value[0])
    elif isinstance(param_obj, EnumObject):
        # 对于枚举对象，返回其ordinal值
        ascii_value = param_obj.ordinal
    else:
        ascii_value = 0

    result = IntegerObject(ascii_value)
    ar[RETURN_NUM_FOR_LENGTH] = result

    interpreter.log(f"LEAVE: FUNCTION {func_name}")
    interpreter.log(str(interpreter.call_stack))

    return result


def handle_chr(interpreter, node):
    """Handle CHR built-in function - converts ASCII code to character"""
    func_name = node.func_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: FUNCTION {func_name}")
    interpreter.log(str(interpreter.call_stack))

    # Get the ASCII code parameter and convert it to a character
    param_obj = interpreter.visit(actual_params[1])  # Skip the function name param

    if isinstance(param_obj, NumberObject):
        ascii_code = param_obj.value
        try:
            char_value = chr(ascii_code)
        except (ValueError, OverflowError):
            char_value = ""
    else:
        char_value = ""

    result = CharObject(char_value)
    ar[RETURN_NUM_FOR_LENGTH] = result

    interpreter.log(f"LEAVE: FUNCTION {func_name}")
    interpreter.log(str(interpreter.call_stack))

    return result


class ARType(Enum):
    PROGRAM = "PROGRAM"
    PROCEDURE = "PROCEDURE"
    FUNCTION = "FUNCTION"


class CallStack:
    def __init__(self) -> None:
        self._records: list[ActivationRecord] = []

    def push(self, ar: ActivationRecord) -> None:
        self._records.append(ar)

    def pop(self) -> ActivationRecord:
        if len(self._records) >= 2:
            self._records[-2].copy_from(self._records[-1], True)
        return self._records.pop()

    def peek(self) -> ActivationRecord:
        return self._records[-1]

    def __str__(self) -> str:
        s = "\n".join(repr(ar) for ar in reversed(self._records))
        s = f"CALL STACK\n{s}\n\n"
        return s

    def __repr__(self) -> str:
        return self.__str__()


class ActivationRecord:
    def __init__(self, name: str, type: ARType, nesting_level: int) -> None:
        self.name = name
        self.type = type
        self.nesting_level = nesting_level
        self.members: dict[str | int, Object] = {}

    def __setitem__(self, key: str | int, value: Object) -> None:
        self.members[key] = value

    def __getitem__(self, key: str | int) -> Object:
        return self.members[key]

    def copy_from(self, other: "ActivationRecord", override: bool):
        for name, val in other.members.items():
            if override or name not in self.members:
                self.members[name] = val

    def get(self, key: str) -> Object:
        return cast(Object, self.members.get(key))

    def __str__(self) -> str:
        lines = [
            "{level}: {type} {name}".format(
                level=self.nesting_level,
                type=self.type.value,
                name=self.name,
            )
        ]
        for name, val in self.members.items():
            lines.append(f"   {name:<20}: {val}")

        s = "\n".join(lines)
        return s

    def __repr__(self) -> str:
        return self.__str__()


class Interpreter(NodeVisitor):
    def __init__(self, tree: Program) -> None:
        self.tree = tree
        self.call_stack = CallStack()

        # 枚举类型和值的注册表（模仿semantic analyzer的实现）
        self.enum_types: dict[
            str, dict
        ] = {}  # { type_name -> { 'values': [value_names...], 'size': int } }
        self.enum_values: dict[
            str, dict
        ] = {}  # { value_name -> { 'type': type_name, 'ordinal': int } }
        self.record_types: dict[str, RecordType] = {}

        # 类型别名映射表：alias_name -> actual_type_node
        self.type_aliases: dict[str, Type] = {}

        # Register built-in procedures and functions
        register_builtin_procedure(NativeMethod.WRITE.name, handle_write)
        register_builtin_procedure(NativeMethod.WRITELN.name, handle_writeln)
        register_builtin_procedure(NativeMethod.SETLENGTH.name, handle_setlength)
        register_builtin_procedure(NativeMethod.INC.name, handle_inc)
        register_builtin_procedure(NativeMethod.DEC.name, handle_dec)
        register_builtin_function(NativeMethod.LENGTH.name, handle_length)
        register_builtin_function(NativeMethod.ORD.name, handle_ord)
        register_builtin_function(NativeMethod.CHR.name, handle_chr)

    def log(self, msg) -> None:
        if _SHOULD_LOG_STACK:
            print(msg)

    def _resolve_type_alias(self, type_node: Type) -> Type:
        """解析类型别名，追随别名链直到找到实际类型"""
        if hasattr(type_node, "value"):
            type_name = type_node.value
            # 检查是否是类型别名
            if type_name in self.type_aliases:
                # 递归解析别名链
                return self._resolve_type_alias(self.type_aliases[type_name])

        # 如果不是别名或者已经是实际类型，直接返回
        return type_node

    def _enum_obj(self, type_name: str, ordinal: int) -> EnumObject:
        """根据ordinal反查名称创建枚举对象"""
        # 首先尝试从注册的枚举类型中查找名称
        if (
            type_name in self.enum_types
            and ordinal < self.enum_types[type_name]["size"]
        ):
            name = self.enum_types[type_name]["values"][ordinal]
            return EnumObject(type_name, name, ordinal)
        else:
            raise InterpreterError(error_code=ErrorCode.INTERPRETER_UNKNOWN_ENUM)

    def _initialize_record_complex_fields(self, record_obj: RecordObject) -> None:
        """使用Interpreter的属性信息初始化RecordObject中的复杂类型字段"""
        for field in record_obj.record_type.fields:
            field_name = field.name.value
            field_type = field.type_node

            # 获取已创建的默认对象
            current_obj = record_obj.fields.get(field_name, NullObject())

            # 如果是空对象，尝试使用interpreter信息初始化
            if isinstance(current_obj, NullObject):
                new_obj = self._create_complex_object_from_type_node(field_type)
                if not isinstance(new_obj, NullObject):
                    record_obj.fields[field_name] = new_obj

    def _create_complex_object_from_type_node(self, type_node) -> Object:
        """根据类型节点创建复杂类型对象，使用interpreter的属性信息"""
        # 直接检查是否是ArrayType实例
        if (
            hasattr(type_node, "__class__")
            and type_node.__class__.__name__ == "ArrayType"
        ):
            return self.__initArray(type_node)

        # 处理数组类型
        if hasattr(type_node, "token") and type_node.token.type == TokenType.ARRAY:
            return self.__initArray(type_node)

        # 处理基本类型
        if hasattr(type_node, "token"):
            token_type = type_node.token.type
            if token_type == TokenType.INTEGER:
                return IntegerObject(0)
            elif token_type == TokenType.REAL:
                return RealObject(0.0)
            elif token_type == TokenType.BOOLEAN:
                return BooleanObject(False)
            elif token_type == TokenType.STRING:
                return StringObject("")
            elif token_type == TokenType.CHAR:
                return CharObject("")

        # 处理自定义类型（枚举、记录等）
        if hasattr(type_node, "value"):
            type_name = type_node.value

            # 检查是否是枚举类型
            if type_name in self.enum_types:
                return self._enum_obj(type_name, 0)  # 使用第一个枚举值

            # 检查是否是记录类型
            if type_name in self.record_types:
                record_type = self.record_types[type_name]
                nested_record_obj = RecordObject(record_type)
                # 递归初始化嵌套记录的复杂字段
                self._initialize_record_complex_fields(nested_record_obj)
                return nested_record_obj

            # 检查是否是类型别名，然后递归解析
            if type_name in self.type_aliases:
                actual_type = self.type_aliases[type_name]
                return self._create_complex_object_from_type_node(actual_type)

        # 其他情况返回空对象
        return NullObject()

    def _post_initialize_array_elements(
        self, array_obj: ArrayObject, element_type_node: Type
    ) -> None:
        """为包含复杂类型元素的数组后初始化元素"""
        # 为静态数组的每个位置创建适当的对象，替换NullObject
        for index in range(array_obj.lower_bound, array_obj.upper_bound + 1):
            if array_obj.element_type == ElementType.RECORD:
                # 为记录类型创建对象
                if hasattr(element_type_node, "value"):
                    type_name = element_type_node.value
                    if type_name in self.record_types:
                        record_type = self.record_types[type_name]
                        record_obj = RecordObject(record_type)
                        self._initialize_record_complex_fields(record_obj)
                        array_obj.value[index] = record_obj
            elif array_obj.element_type == ElementType.CUSTOM:
                # 为其他自定义类型创建对象
                if hasattr(element_type_node, "value"):
                    type_name = element_type_node.value
                    if type_name in self.enum_types:
                        # 枚举类型，使用第一个枚举值
                        enum_obj = self._enum_obj(type_name, 0)
                        array_obj.value[index] = enum_obj
                    elif type_name in self.type_aliases:
                        # 处理类型别名
                        actual_type = self.type_aliases[type_name]
                        complex_obj = self._create_complex_object_from_type_node(
                            actual_type
                        )
                        array_obj.value[index] = complex_obj

    def visit_Program(self, node: Program) -> None:
        program_name = node.name
        self.log(f"ENTER: PROGRAM {program_name}")

        ar = ActivationRecord(
            name=program_name,
            type=ARType.PROGRAM,
            nesting_level=1,
        )
        self.call_stack.push(ar)

        self.log(str(self.call_stack))

        self.visit(node.block)

        self.log(f"LEAVE: PROGRAM {program_name}")
        self.log(str(self.call_stack))

        self.call_stack.pop()

    def visit_Block(self, node: Block) -> None:
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_TypeDeclaration(self, node: TypeDeclaration) -> None:
        # 在解释器中处理枚举类型声明，将枚举类型和枚举值注册到本地注册表中
        if isinstance(node.type_def, EnumType):
            type_name = node.type_name.value
            enum_values = node.type_def.enum_values

            # 注册枚举类型
            self.enum_types[type_name] = {
                "values": enum_values,
                "size": len(enum_values),
            }

            # 注册枚举值
            for i, enum_val in enumerate(enum_values):
                self.enum_values[enum_val] = {"type": type_name, "ordinal": i}
        elif isinstance(node.type_def, RecordType):
            type_name = node.type_name.value
            # Insert the record type into global record_types for interpreter use
            self.record_types[type_name] = node.type_def
        else:
            # 处理类型别名 (type aliases)
            # 对于非枚举和非记录类型，将其作为类型别名处理
            type_name = node.type_name.value
            self.type_aliases[type_name] = node.type_def

    def visit_VarDecl(self, node: VarDecl) -> None:
        ar = self.call_stack.peek()

        # 解析类型别名，获取实际类型
        resolved_type_node = self._resolve_type_alias(node.type_node)

        # 处理基本类型
        if hasattr(resolved_type_node, "token"):
            if resolved_type_node.token.type == TokenType.BOOLEAN:
                ar[node.var_node.value] = BooleanObject(False)
                return
            elif resolved_type_node.token.type == TokenType.INTEGER:
                ar[node.var_node.value] = IntegerObject(0)
                return
            elif resolved_type_node.token.type == TokenType.REAL:
                ar[node.var_node.value] = RealObject(0.0)
                return
            elif resolved_type_node.token.type == TokenType.CHAR:
                ar[node.var_node.value] = CharObject("")
                return
            elif resolved_type_node.token.type == TokenType.STRING:
                string_node = cast(StringType, resolved_type_node)
                limit: int = 255
                if string_node.limit is not None:
                    limit = self.visit(string_node.limit).value
                ar[node.var_node.value] = StringObject("", limit)
                return
            elif resolved_type_node.token.type == TokenType.ARRAY:
                ar[node.var_node.value] = self.__initArray(resolved_type_node)
                return

        # 处理复杂类型（记录、枚举等）
        var_name = node.var_node.value

        # 检查是否是记录类型
        if isinstance(resolved_type_node, RecordType):
            record_obj = RecordObject(resolved_type_node)
            # 初始化复杂类型字段
            self._initialize_record_complex_fields(record_obj)
            ar[var_name] = record_obj
            return

        # 检查是否是枚举类型（通过类型名称）
        if hasattr(resolved_type_node, "value"):
            type_name = resolved_type_node.value
            if type_name in self.enum_types:
                enum_obj = self._enum_obj(type_name, 0)
                ar[var_name] = enum_obj
                return
            elif type_name in self.record_types:
                record_type = self.record_types[type_name]
                record_obj = RecordObject(record_type)
                # 初始化复杂类型字段
                self._initialize_record_complex_fields(record_obj)
                ar[var_name] = record_obj
                return

        # 如果有type_symbol（从语义分析器传来），尝试使用它
        type_symbol = node.type_symbol
        if type_symbol is not None:
            # Handle based on the type symbol
            if isinstance(type_symbol, RecordTypeSymbol):
                # Handle record type - use global record_types lookup
                type_name = (
                    resolved_type_node.value
                    if hasattr(resolved_type_node, "value")
                    else ""
                )
                if type_name in self.record_types:
                    record_type = self.record_types[type_name]
                    record_obj = RecordObject(record_type)
                    # 初始化复杂类型字段
                    self._initialize_record_complex_fields(record_obj)
                    ar[var_name] = record_obj
                else:
                    ar[var_name] = NullObject()
            elif (
                isinstance(type_symbol, EnumTypeSymbol)
                or type_symbol.name in self.enum_types
            ):
                # Handle enum type
                enum_obj = self._enum_obj(type_symbol.name, 0)
                ar[var_name] = enum_obj
            else:
                # For other custom types, treat as null for now
                ar[var_name] = NullObject()
        else:
            # 如果没有type_symbol，默认创建Null对象
            ar[var_name] = NullObject()

    def __initArray(self, node: Type) -> ArrayObject:
        if isinstance(node, ArrayType):
            lower_bound: int = self.visit(node.lower).value
            upper_bound: int = self.visit(node.upper).value
            if lower_bound > upper_bound:
                raise InterpreterError(
                    error_code=ErrorCode.INTERPRETER_ARRAY_RANGE_INVALID,
                    token=node.token,
                    message=f"{ErrorCode.INTERPRETER_ARRAY_RANGE_INVALID.value} -> {node.token}",
                )

            # Determine element type
            element_type = ElementType.INTEGER  # default

            # Check if element_type has a token attribute for basic types
            if hasattr(node.element_type, "token"):
                if node.element_type.token.type == TokenType.BOOLEAN:
                    element_type = ElementType.BOOL
                elif node.element_type.token.type == TokenType.INTEGER:
                    element_type = ElementType.INTEGER
                elif node.element_type.token.type == TokenType.REAL:
                    element_type = ElementType.REAL
                elif node.element_type.token.type == TokenType.STRING:
                    element_type = ElementType.STRING
                elif node.element_type.token.type == TokenType.CHAR:
                    element_type = ElementType.CHAR
                elif node.element_type.token.type == TokenType.ARRAY:
                    element_type = ElementType.ARRAY
                elif node.element_type.token.type == TokenType.ID:
                    # Custom type (record, enum, etc.)
                    # Check if it's a record type first
                    type_name = node.element_type.value
                    if type_name in self.record_types:
                        element_type = ElementType.RECORD
                    else:
                        # Could be enum or other custom type
                        element_type = ElementType.CUSTOM
            else:
                # For complex types without simple token, treat as custom
                element_type = ElementType.CUSTOM

            # Create array with the determined element type
            array_obj = ArrayObject(
                element_type=element_type,
                lower_bound=lower_bound,
                upper_bound=upper_bound,
                dynamic=node.dynamic,
            )

            # For record and custom types, we need to post-initialize elements
            if (
                element_type in (ElementType.RECORD, ElementType.CUSTOM)
                and not node.dynamic
            ):
                self._post_initialize_array_elements(array_obj, node.element_type)

            return array_obj

        raise SemanticError(
            error_code=ErrorCode.SEMANTIC_UNKNOWN_TYPE,
            token=node.token,
            message=f"{ErrorCode.SEMANTIC_UNKNOWN_TYPE.value} -> {node.token}",
        )

    def visit_Type(self, node: Type) -> None:
        # Do nothing
        pass

    def visit_StringType(self, node: StringType) -> None:
        # Do nothing
        pass

    def visit_PrimitiveType(self, node: PrimitiveType) -> None:
        # Do nothing
        pass

    def visit_ArrayType(self, node: ArrayType) -> None:
        # Do nothing
        pass

    def visit_RecordType(self, node: RecordType) -> None:
        """访问记录类型定义节点"""
        # 在解释器中，记录类型定义已经在语义分析阶段处理
        pass

    def visit_BinOp(self, node: BinOp) -> Object:
        left_obj = self.visit(node.left)
        right_obj = self.visit(node.right)

        # logic operator
        if node.op.type == TokenType.AND:
            if isinstance(left_obj, BooleanObject) and isinstance(
                right_obj, BooleanObject
            ):
                return left_obj & right_obj
        elif node.op.type == TokenType.OR:
            if isinstance(left_obj, BooleanObject) and isinstance(
                right_obj, BooleanObject
            ):
                return left_obj | right_obj

        # arithmetic operator
        if node.op.type == TokenType.PLUS:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj + right_obj
            elif isinstance(left_obj, StringObject) and isinstance(
                right_obj, StringObject
            ):
                # For string concatenation, don't apply limits during intermediate operations
                return StringObject(left_obj.value + right_obj.value, -1)
            # Handle string + other types conversion
            elif isinstance(left_obj, StringObject):
                if hasattr(right_obj, "value"):
                    return StringObject(left_obj.value + str(right_obj.value), -1)
                else:
                    return StringObject(left_obj.value + str(right_obj), -1)
            elif isinstance(right_obj, StringObject):
                if hasattr(left_obj, "value"):
                    return StringObject(str(left_obj.value) + right_obj.value, -1)
                else:
                    return StringObject(str(left_obj) + right_obj.value, -1)
        elif node.op.type == TokenType.MINUS:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj - right_obj
        elif node.op.type == TokenType.MUL:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj * right_obj
        elif node.op.type == TokenType.INTEGER_DIV:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj // right_obj
        elif node.op.type == TokenType.FLOAT_DIV:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj / right_obj
        elif node.op.type == TokenType.MOD:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                # MOD 运算符应该返回整数余数
                # 在 Pascal 中，MOD 只对整数有效，因此我们需要转换为整数
                left_val = (
                    int(left_obj.value)
                    if isinstance(left_obj, RealObject)
                    else left_obj.value
                )
                right_val = (
                    int(right_obj.value)
                    if isinstance(right_obj, RealObject)
                    else right_obj.value
                )
                return IntegerObject(left_val % right_val)

        # comparison operator
        if node.op.type == TokenType.LT:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj < right_obj
            elif isinstance(left_obj, CharObject) and isinstance(right_obj, CharObject):
                return left_obj < right_obj
            elif isinstance(left_obj, EnumObject) and isinstance(right_obj, EnumObject):
                return left_obj < right_obj
        elif node.op.type == TokenType.GT:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj > right_obj
            elif isinstance(left_obj, CharObject) and isinstance(right_obj, CharObject):
                return left_obj > right_obj
            elif isinstance(left_obj, EnumObject) and isinstance(right_obj, EnumObject):
                return left_obj > right_obj
        elif node.op.type == TokenType.EQ:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return BooleanObject(value=left_obj == right_obj)
            elif isinstance(left_obj, BooleanObject) and isinstance(
                right_obj, BooleanObject
            ):
                return BooleanObject(value=left_obj == right_obj)
            elif isinstance(left_obj, CharObject) and isinstance(right_obj, CharObject):
                return BooleanObject(value=left_obj == right_obj)
            elif isinstance(left_obj, EnumObject) and isinstance(right_obj, EnumObject):
                return BooleanObject(value=left_obj == right_obj)
        elif node.op.type == TokenType.NE:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return BooleanObject(value=left_obj != right_obj)
            elif isinstance(left_obj, BooleanObject) and isinstance(
                right_obj, BooleanObject
            ):
                return BooleanObject(value=left_obj != right_obj)
            elif isinstance(left_obj, CharObject) and isinstance(right_obj, CharObject):
                return BooleanObject(value=left_obj != right_obj)
            elif isinstance(left_obj, EnumObject) and isinstance(right_obj, EnumObject):
                return BooleanObject(value=left_obj != right_obj)
        elif node.op.type == TokenType.LE:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj <= right_obj
            elif isinstance(left_obj, CharObject) and isinstance(right_obj, CharObject):
                return left_obj <= right_obj
            elif isinstance(left_obj, EnumObject) and isinstance(right_obj, EnumObject):
                return left_obj <= right_obj
        elif node.op.type == TokenType.GE:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj >= right_obj
            elif isinstance(left_obj, CharObject) and isinstance(right_obj, CharObject):
                return left_obj >= right_obj
            elif isinstance(left_obj, EnumObject) and isinstance(right_obj, EnumObject):
                return left_obj >= right_obj

        # !!
        raise InterpreterError(
            error_code=ErrorCode.INTERPRETER_UNKNOWN_OPERATOR,
            token=node.token,
            message=f"{ErrorCode.INTERPRETER_UNKNOWN_OPERATOR.value} -> {node.token}",
        )

    def visit_Num(self, node: Num) -> NumberObject:
        if isinstance(node.value, int):
            return IntegerObject(node.value)
        else:
            return RealObject(node.value)

    def visit_String(self, node: String) -> StringObject:
        return StringObject(node.value)

    def visit_Char(self, node: Char) -> CharObject:
        return CharObject(node.value)

    def visit_Bool(self, node: Bool) -> BooleanObject:
        if node.token.type == TokenType.TRUE:
            return BooleanObject(True)
        elif node.token.type == TokenType.FALSE:
            return BooleanObject(False)
        raise InterpreterError(
            error_code=ErrorCode.INTERPRETER_UNKNOWN_BOOLEAN,
            token=node.token,
            message=f"{ErrorCode.INTERPRETER_UNKNOWN_BOOLEAN.value} -> {node.token}",
        )

    def visit_UnaryOp(self, node: UnaryOp) -> Object:
        expr_obj = self.visit(node.expr)
        op = node.op.type

        # negative bang
        if op == TokenType.NOT:
            if isinstance(expr_obj, BooleanObject):
                return ~expr_obj
            else:
                return BooleanObject(not expr_obj.to_bool())

        # signal bang
        if op == TokenType.PLUS:
            if isinstance(expr_obj, NumberObject):
                return +expr_obj
        elif op == TokenType.MINUS:
            if isinstance(expr_obj, NumberObject):
                return -expr_obj

        raise InterpreterError(
            error_code=ErrorCode.INTERPRETER_UNKNOWN_OPERATOR,
            token=node.token,
            message=f"{ErrorCode.INTERPRETER_UNKNOWN_OPERATOR.value} -> {node.token}",
        )

    def visit_Compound(self, node: Compound) -> None:
        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node: Assign) -> None:
        var_value = self.visit(node.right)
        ar = self.call_stack.peek()

        if isinstance(node.left, AccessExpression):
            # Handle access expression assignments (e.g., arr[i] := value or record.field := value)
            # Get the base object
            base_obj = self.visit(node.left.base)
            current_obj = base_obj

            # Handle all suffixes except the last one
            for suffix in node.left.suffixes[:-1]:
                if isinstance(suffix, IndexSuffix):
                    # Handle array/string index access
                    index_obj = self.visit(suffix.index)
                    index = (
                        index_obj.value if isinstance(index_obj, NumberObject) else 0
                    )

                    if isinstance(current_obj, StringObject):
                        current_obj = current_obj[index]
                    elif isinstance(current_obj, ArrayObject):
                        current_obj = current_obj[index]
                    else:
                        # Unsupported indexing
                        return

                elif isinstance(suffix, MemberSuffix):
                    # Handle record member access
                    field_name = suffix.member.value

                    if isinstance(current_obj, RecordObject):
                        # Check if accessing variant field and initialize if needed
                        if (
                            current_obj.record_type.variant_part
                            and current_obj._is_variant_field(field_name)
                        ):
                            # Get current tag field value to ensure variant fields are initialized
                            tag_field_name = (
                                current_obj.record_type.variant_part.tag_field.value
                            )
                            tag_field_value = current_obj.fields.get(tag_field_name)
                            if tag_field_value and hasattr(tag_field_value, "value"):
                                current_obj._init_variant_fields(
                                    str(tag_field_value.value)
                                )

                        current_obj = current_obj[field_name]
                    else:
                        # Unsupported member access
                        return

            # Handle the last suffix for assignment
            last_suffix = node.left.suffixes[-1]
            if isinstance(last_suffix, IndexSuffix):
                # Handle array/string index assignment
                index_obj = self.visit(last_suffix.index)
                index = index_obj.value if isinstance(index_obj, NumberObject) else 0

                if isinstance(current_obj, StringObject) and isinstance(
                    var_value, (StringObject, CharObject)
                ):
                    # Handle string character assignment
                    if (
                        isinstance(var_value, StringObject)
                        and len(var_value.value) == 1
                    ):
                        current_obj[index] = CharObject(var_value.value)
                    elif isinstance(var_value, CharObject):
                        current_obj[index] = var_value
                elif isinstance(current_obj, ArrayObject):
                    # Handle array element assignment
                    current_obj[index] = var_value

            elif isinstance(last_suffix, MemberSuffix):
                # Handle record member assignment
                field_name = last_suffix.member.value

                if isinstance(current_obj, RecordObject):
                    # Check if assigning to variant field and initialize if needed
                    if (
                        current_obj.record_type.variant_part
                        and current_obj._is_variant_field(field_name)
                    ):
                        # Get current tag field value to ensure variant fields are initialized
                        tag_field_name = (
                            current_obj.record_type.variant_part.tag_field.value
                        )
                        tag_field_value = current_obj.fields.get(tag_field_name)
                        if tag_field_value and hasattr(tag_field_value, "value"):
                            if isinstance(tag_field_value, EnumObject):
                                current_obj._init_variant_fields(tag_field_value.name)
                            else:
                                current_obj._init_variant_fields(
                                    str(tag_field_value.value)
                                )

                    current_obj[field_name] = var_value

            return
        elif isinstance(node.left, Var):
            var_name = node.left.value
        else:
            # Fallback: treat as variable assignment if it has a value attribute
            var_name = getattr(node.left, "value", None)
            if var_name is None:
                return

        # Handle regular variable assignment
        existing_var = ar.get(var_name)
        if isinstance(existing_var, StringObject) and isinstance(
            var_value, StringObject
        ):
            # Handle string assignment with limit checking
            if existing_var.limit > 0 and len(var_value.value) > existing_var.limit:
                message = f"Warning: String literal has more characters[{len(var_value.value)}] than short string length[{existing_var.limit}]"
                SpiUtil.print_w(message=message)
                ar[var_name] = StringObject(
                    var_value.value[: existing_var.limit], existing_var.limit
                )
            else:
                ar[var_name] = StringObject(var_value.value, existing_var.limit)
        elif isinstance(existing_var, CharObject) and isinstance(
            var_value, StringObject
        ):
            ar[var_name] = CharObject(value=var_value.value)
        else:
            ar[var_name] = var_value

    def visit_Var(self, node: Var) -> Object:
        var_name = node.value
        ar = self.call_stack.peek()
        var_value = ar.get(var_name)

        # 如果在当前作用域中找不到该变量，检查它是否是枚举值
        if var_value is None:
            # 检查是否是已注册的枚举值
            if var_name in self.enum_values:
                enum_info = self.enum_values[var_name]
                return EnumObject(enum_info["type"], var_name, enum_info["ordinal"])
        return var_value if var_value is not None else NullObject()

    def visit_AccessExpression(self, node: AccessExpression) -> Object:
        # Start with the base value
        base_obj = self.visit(node.base)
        current_obj = base_obj

        # Apply each suffix in sequence
        for suffix in node.suffixes:
            if isinstance(suffix, IndexSuffix):
                # Array/string index access
                index_obj = self.visit(suffix.index)
                index = index_obj.value if isinstance(index_obj, NumberObject) else 0

                if isinstance(current_obj, StringObject):
                    current_obj = current_obj[index]
                elif isinstance(current_obj, ArrayObject):
                    current_obj = cast(Object, current_obj[index])
                else:
                    # Return null object for unsupported indexing
                    return NullObject()

            elif isinstance(suffix, MemberSuffix):
                # Member access for records/enums
                field_name = suffix.member.value

                if isinstance(current_obj, RecordObject):
                    # Direct field access - variant fields are already initialized
                    # when the tag field is set in RecordObject.__setitem__
                    current_obj = current_obj[field_name]
                else:
                    # Return null object for unsupported member access
                    return NullObject()

        return current_obj if current_obj is not None else NullObject()

    def visit_IndexSuffix(self, node: IndexSuffix) -> Object:
        result = self.visit(node.index)
        return result if isinstance(result, Object) else NullObject()

    def visit_MemberSuffix(self, node: MemberSuffix) -> Object:
        # Member access not implemented yet
        return NullObject()

    def visit_NoOp(self, node: NoOp) -> None:
        pass

    def visit_WhileStatement(self, node: WhileStatement) -> None:
        while self.visit(node.condition).to_bool():
            self.visit(node.block)

    def visit_ForStatement(self, node: ForStatement) -> None:
        ar = self.call_stack.peek()
        var_name = node.initialization.left.value
        self.visit(node.initialization)
        bound_obj = self.visit(node.bound)
        bound_value = bound_obj.value if isinstance(bound_obj, NumberObject) else 0

        var_obj = ar[var_name]
        var_value = var_obj.value if isinstance(var_obj, NumberObject) else 0

        # 检查是否是枚举类型
        if isinstance(var_obj, EnumObject) and isinstance(bound_obj, EnumObject):
            # 枚举类型的for循环
            if var_obj.type_name == bound_obj.type_name:
                start_ord = var_obj.ordinal
                end_ord = bound_obj.ordinal
                step = 1 if start_ord <= end_ord else -1

                # 获取枚举类型信息（需要从语义分析器获取）
                # 这里使用简化实现
                current_ord = start_ord
                while (step > 0 and current_ord <= end_ord) or (
                    step < 0 and current_ord >= end_ord
                ):
                    # 更新循环变量为当前枚举值
                    # 使用_enum_obj方法创建正确的枚举对象
                    ar[var_name] = self._enum_obj(var_obj.type_name, current_ord)
                    self.visit(node.block)
                    current_ord += step
        else:
            # 原来的数值类型for循环
            while var_value <= bound_value:
                self.visit(node.block)
                var_value += 1
                if var_value <= bound_value:
                    ar[var_name] = IntegerObject(var_value)

    def visit_ProcedureDecl(self, node: ProcedureDecl) -> None:
        pass

    def visit_ProcedureCall(self, node: ProcedureCall) -> None:
        proc_name = node.proc_name
        proc_symbol = node.proc_symbol

        if proc_symbol is None:
            raise InterpreterError(
                error_code=ErrorCode.NULL_POINTER,
                token=node.token,
                message=f"{ErrorCode.NULL_POINTER.value} -> {node.token}",
            )

        ar = ActivationRecord(
            name=proc_name,
            type=ARType.PROCEDURE,
            nesting_level=proc_symbol.scope_level + 1,
        )

        pre_ar = self.call_stack.peek()
        if pre_ar is not None:
            ar.copy_from(pre_ar, False)

        # deal with built-in procedure first
        if isinstance(proc_symbol, BuiltinProcedureSymbol):
            # Look up the built-in procedure in the registry
            handler = BUILTIN_PROCEDURES.get(proc_symbol.name.upper())
            if handler:
                # Prepare parameters
                actual_params = node.actual_params
                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                # Call the handler
                handler(self, node)

                self.call_stack.pop()
                return
            else:
                raise InterpreterError(
                    error_code=ErrorCode.INTERPRETER_UNKNOWN_BUILTIN_PROCEDURE,
                    token=node.token,
                    message=f"{ErrorCode.INTERPRETER_UNKNOWN_BUILTIN_PROCEDURE.value} -> {node.token}",
                )
        else:
            formal_params = proc_symbol.formal_params
            actual_params = node.actual_params

            for param_symbol, argument_node in zip(formal_params, actual_params):
                ar[param_symbol.name] = self.visit(argument_node)

            self.call_stack.push(ar)

            self.log(f"ENTER: PROCEDURE {proc_name}")
            self.log(str(self.call_stack))

            # evaluate procedure body
            if proc_symbol.block_ast is None:
                raise InterpreterError(
                    error_code=ErrorCode.NULL_POINTER,
                    token=None,
                    message=f"{ErrorCode.NULL_POINTER.value}",
                )
            self.visit(proc_symbol.block_ast)

            self.log(f"LEAVE: PROCEDURE {proc_name}")
            self.log(str(self.call_stack))

            self.call_stack.pop()
            pass

    def visit_FunctionDecl(self, node: FunctionDecl) -> None:
        pass

    def visit_FunctionCall(self, node: FunctionCall) -> Object:
        func_name = node.func_name
        func_symbol = node.func_symbol

        if func_symbol is None:
            raise InterpreterError(
                error_code=ErrorCode.NULL_POINTER,
                token=None,
                message=f"{ErrorCode.NULL_POINTER.value}",
            )

        ar = ActivationRecord(
            name=func_name,
            type=ARType.FUNCTION,
            nesting_level=func_symbol.scope_level + 1,
        )

        pre_ar = self.call_stack.peek()
        if pre_ar is not None:
            ar.copy_from(pre_ar, False)

        # deal with built-in function first
        if isinstance(func_symbol, BuiltinFunctionSymbol):
            # Look up the built-in function in the registry
            handler = BUILTIN_FUNCTIONS.get(func_symbol.name.upper())
            if handler:
                # Prepare parameters
                actual_params = node.actual_params
                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                # Call the handler and get the result
                result = handler(self, node)

                self.call_stack.pop()
                return result
            else:
                raise InterpreterError(
                    error_code=ErrorCode.INTERPRETER_UNKNOWN_BUILTIN_FUNCTION,
                    token=None,
                    message=f"{ErrorCode.INTERPRETER_UNKNOWN_BUILTIN_FUNCTION.value}",
                )
        else:
            formal_params = func_symbol.formal_params
            actual_params = node.actual_params

            for param_symbol, argument_node in zip(formal_params, actual_params):
                ar[param_symbol.name] = self.visit(argument_node)

            self.call_stack.push(ar)

            self.log(f"ENTER: FUNCTION {func_name}")
            self.log(str(self.call_stack))

            # evaluate procedure body
            if func_symbol.block_ast is None:
                raise InterpreterError(
                    error_code=ErrorCode.NULL_POINTER,
                    token=None,
                    message=f"{ErrorCode.NULL_POINTER.value}",
                )
            self.visit(func_symbol.block_ast)

            self.log(f"LEAVE: FUNCTION {func_name}")
            self.log(str(self.call_stack))

            result = ar.get(func_name)
            self.call_stack.pop()

            return result if result is not None else NullObject()
            
    def visit_IfStatement(self, node: IfStatement) -> None:
        flag = self.visit(node.condition).to_bool()

        if flag:
            self.visit(node.then_branch)
            return
        else:
            for branch in node.else_if_branches:
                sub_flag = self.visit(branch.condition).to_bool()
                if sub_flag:
                    self.visit(branch.then_branch)
                    return

        if node.else_branch is not None:
            self.visit(node.else_branch)

    def visit_CaseStatement(self, node: CaseStatement) -> None:
        # 计算case表达式的值
        case_value_obj = self.visit(node.case_expr)
        case_value = (
            case_value_obj.value if hasattr(case_value_obj, "value") else case_value_obj
        )

        # 查找匹配的case项
        matched = False
        for case_item in node.case_items:
            for label in case_item.labels:
                label_value = label.value
                # 检查是否匹配
                # 对于枚举值，我们需要比较名称而不是值
                if isinstance(case_value_obj, EnumObject):
                    # 如果case表达式是枚举对象，标签值应该是枚举值的名称
                    if label_value == case_value_obj.name:
                        # 执行匹配的语句
                        self.visit(case_item.statement)
                        matched = True
                        break
                elif case_value == label_value:
                    # 执行匹配的语句
                    self.visit(case_item.statement)
                    matched = True
                    break
            if matched:
                break

        # 如果没有匹配项且有else语句，则执行else语句
        if not matched and node.else_stmt:
            self.visit(node.else_stmt)


    def interpret(self):
        tree = self.tree
        if tree is None:
            return ""
        return self.visit(tree)
