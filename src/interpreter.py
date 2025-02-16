###############################################################################
#                                                                             #
#  INTERPRETER                                                                #
#                                                                             #
###############################################################################

"""SPI - Simple Pascal Interpreter. Part 19"""

from __future__ import annotations

from enum import Enum
from typing import Any, cast
from src.object import EnumObject
from src.globals import _SHOULD_LOG_STACK, RETURN_NUM
from src.sematic_analyzer import NativeMethod, SemanticAnalyzer
from src.visitor import NodeVisitor
from src.spi_ast import *
from src.symbol import *
from src.error import *
from src.util import SpiUtil
from src.spi_token import ElementType, TokenType


class ARType(Enum):
    PROGRAM = "PROGRAM"
    PROCEDURE = "PROCEDURE"
    FUNCTION = "FUNCTION"
    METHOD = "METHOD"


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


class MemberMeta:
    def __init__(self, type: ElementType, dynamic: bool = False, limit: int = -1):
        self.type = type
        self.dynamic = dynamic
        self.limit = limit
        self.is_class = False
        self.is_enum = False
        self.is_record = False
        self.is_instance = False
        self.is_record_instance = False
        self.ref_class_name = ""
        self.ref_record_name = ""


class ActivationRecord:
    def __init__(self, name: str, type: ARType, nesting_level: int) -> None:
        self.name = name
        self.type = type
        self.nesting_level = nesting_level
        self.members: dict[str, Any] = {}
        self.members_meta: dict[str, MemberMeta] = {}

    def __setitem__(self, key: str, value) -> None:
        self.members[key] = value

    def __getitem__(self, key: str):
        return self.members[key]

    def copy_from(self, other: "ActivationRecord", override: bool):
        for name, val in other.members.items():
            if override or name not in self.members:
                self.members[name] = val
        for name, val in other.members_meta.items():
            if override or name not in self.members_meta:
                self.members_meta[name] = val

    def get(self, key: str):
        return self.members.get(key)

    def set_meta(self, key: str, type: ElementType):
        self.members_meta[key] = MemberMeta(type=type)

    def set_dynamic(self, key: str, dynamic: bool):
        self.members_meta[key].dynamic = dynamic

    def set_limit(self, key: str, limit: int):
        self.members_meta[key].limit = limit

    def get_meta(self, key: str):
        meta = self.members_meta.get(key)
        if meta is None:
            raise InterpreterError()
        else:
            return meta

    def set_is_class(self, key: str, is_class: bool):
        self.members_meta[key].is_class = is_class

    def set_is_instance(self, key: str, is_instance: bool):
        self.members_meta[key].is_instance = is_instance

    def set_is_record_instance(
        self,
        key: str,
    ):
        self.members_meta[key].is_record_instance = True

    def set_is_enum(self, key: str):
        self.members_meta[key].is_enum = True

    def set_is_record(self, key: str):
        self.members_meta[key].is_record = True

    def set_ref_class_name(self, key: str, ref_class_name: str):
        self.members_meta[key].ref_class_name = ref_class_name

    def set_ref_record_name(self, key: str, ref_record_name: str):
        self.members_meta[key].ref_record_name = ref_record_name

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

    def log(self, msg) -> None:
        if _SHOULD_LOG_STACK:
            print(msg)

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

    def visit_VarDecl(self, node: VarDecl) -> None:
        ar = self.call_stack.peek()
        if node.type_node.token.type == TokenType.BOOLEAN:
            ar[node.var_node.value] = False
        elif node.type_node.token.type == TokenType.INTEGER:
            ar[node.var_node.value] = 0
        elif node.type_node.token.type == TokenType.REAL:
            ar[node.var_node.value] = 0.0
        elif node.type_node.token.type == TokenType.STRING:
            ar[node.var_node.value] = ""
            string_node = cast(StringType, node.type_node)
            limit: int = 255
            if string_node.limit is not None:
                limit = self.visit(string_node.limit)
            ar.set_meta(key=node.var_node.value, type=ElementType.STRING)
            ar.set_limit(key=node.var_node.value, limit=limit)
        elif node.type_node.token.type == TokenType.ARRAY:
            ar[node.var_node.value] = self.__initArray(node.type_node)
            self.__set_member_type(node, ar)
            if (cast(ArrayType, node.type_node)).dynamic is True:
                ar.set_dynamic(node.var_node.value, True)
        elif node.type_node.token.type == TokenType.RECORD:
            # TODO: should write this in recursion
            record_decl: RecordDecl = ar.get(node.type_node.token.value)
            instance: dict[str, Any] = {}
            for field in record_decl.fields:
                if field.type_node.token.type == TokenType.BOOLEAN:
                    instance[field.var_node.value] = False
                elif field.type_node.token.type == TokenType.INTEGER:
                    instance[field.var_node.value] = 0
                elif field.type_node.token.type == TokenType.REAL:
                    instance[field.var_node.value] = 0.0
                elif field.type_node.token.type == TokenType.STRING:
                    instance[field.var_node.value] = ""
                elif field.type_node.token.type == TokenType.ARRAY:
                    instance[field.var_node.value] = self.__initArray(field.type_node)
                elif field.type_node.token.type == TokenType.CLASS:
                    instance[field.var_node.value] = {}
            ar[node.var_node.value] = instance
            ar.set_meta(node.var_node.value, ElementType.INSTANCE)
            ar.set_is_record_instance(node.var_node.value)
            ar.set_ref_record_name(node.var_node.value, record_decl.record_name)
        elif node.type_node.token.type == TokenType.CLASS:
            # TODO: should write this in recursion
            class_decl: ClassDecl = ar.get(
                SpiUtil.toClassName(node.type_node.token.value)
            )
            instance = {}
            for field in class_decl.fields:
                if field.type_node.token.type == TokenType.BOOLEAN:
                    instance[field.var_node.value] = False
                elif field.type_node.token.type == TokenType.INTEGER:
                    instance[field.var_node.value] = 0
                elif field.type_node.token.type == TokenType.REAL:
                    instance[field.var_node.value] = 0.0
                elif field.type_node.token.type == TokenType.STRING:
                    instance[field.var_node.value] = ""
                elif field.type_node.token.type == TokenType.ARRAY:
                    instance[field.var_node.value] = self.__initArray(field.type_node)
                elif field.type_node.token.type == TokenType.CLASS:
                    instance[field.var_node.value] = {}
            ar[node.var_node.value] = instance
            ar.set_meta(node.var_node.value, ElementType.INSTANCE)
            ar.set_is_instance(node.var_node.value, True)
            ar.set_ref_class_name(node.var_node.value, class_decl.class_name)
        pass

    def __set_member_type(self, node: VarDecl, ar: ActivationRecord):
        if isinstance(node.type_node, ArrayType):
            if node.type_node.element_type.token.type == TokenType.BOOLEAN:
                ar.set_meta(node.var_node.value, ElementType.BOOL)
            elif node.type_node.element_type.token.type == TokenType.INTEGER:
                ar.set_meta(node.var_node.value, ElementType.INTEGER)
            elif node.type_node.element_type.token.type == TokenType.REAL:
                ar.set_meta(node.var_node.value, ElementType.REAL)
            elif node.type_node.element_type.token.type == TokenType.ARRAY:
                ar.set_meta(node.var_node.value, ElementType.ARRAY)
        else:
            pass

    def __initArray(
        self,
        node: Type,
    ) -> dict[Any, Any]:
        if isinstance(node, ArrayType):
            lower_bound: int = self.visit(node.lower)
            upper_bound: int = self.visit(node.upper)
            if lower_bound > upper_bound:
                raise ArrayRangeInvalidError()
            if node.element_type.token.type == TokenType.BOOLEAN:
                bool_arr: dict[int, bool] = {}
                if node.dynamic is False:
                    for i in range(lower_bound, upper_bound + 1):
                        bool_arr[i] = False
                return bool_arr
            elif node.element_type.token.type == TokenType.INTEGER:
                int_arr: dict[int, int] = {}
                if node.dynamic is False:
                    for i in range(lower_bound, upper_bound + 1):
                        int_arr[i] = 0
                return int_arr
            elif node.element_type.token.type == TokenType.REAL:
                real_arr: dict[int, float] = {}
                if node.dynamic is False:
                    for i in range(lower_bound, upper_bound + 1):
                        real_arr[i] = 0.0
                return real_arr
            elif node.element_type.token.type == TokenType.ARRAY:
                arr_arr: dict[int, dict] = {}
                if node.dynamic is False:
                    for i in range(lower_bound, upper_bound + 1):
                        arr_arr[i] = self.__initArray(node.element_type)
                return arr_arr
        raise UnknownTypeError()

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

    def visit_ClassType(self, node: ClassType) -> None:
        # Do nothing
        pass

    def visit_EnumType(self, node: EnumType) -> None:
        # Do nothing
        pass

    def visit_RecordType(self, node: RecordType) -> None:
        # Do nothing
        pass

    def visit_Decl(self, node: Decl) -> None:
        pass

    def visit_Member(self, node: Member) -> None:
        pass

    def visit_ClassDecl(self, node: ClassDecl) -> None:
        ar = self.call_stack.peek()
        for field in node.fields:
            self.visit(field)
        for method in node.methods:
            self.visit(method)
        ar[node.class_name] = node
        raw_class_name = SpiUtil.extraClassName(node.class_name)
        ar.set_meta(raw_class_name, ElementType.CLASS)
        ar.set_is_class(raw_class_name, True)
        pass

    def visit_EnumDecl(self, node: EnumDecl) -> None:
        ar = self.call_stack.peek()
        ar[node.enum_name] = node
        ar.set_meta(node.enum_name, ElementType.ENUM)
        ar.set_is_enum(node.enum_name)
        pass

    def visit_RecordDecl(self, node: RecordDecl) -> None:
        ar = self.call_stack.peek()
        for field in node.fields:
            self.visit(field)
        ar[node.record_name] = node
        ar.set_meta(node.record_name, ElementType.RECORD)
        ar.set_is_record(node.record_name)
        pass

    def visit_ConstructorDecl(self, node: ConstructorDecl) -> None:
        pass

    def visit_Def(self, node: Def) -> None:
        pass

    def visit_MethodDef(self, node: MethodDef):
        pass

    def visit_FieldDecl(self, node: FieldDecl):
        pass

    def visit_MethodDecl(self, node: MethodDecl):
        pass

    def visit_ProcedureDef(self, node: ProcedureDef) -> None:
        pass

    def visit_ConstructorDef(self, node: ConstructorDef) -> None:
        pass

    def visit_FunctionDef(self, node: FunctionDef) -> None:
        pass

    def visit_BinOp(self, node: BinOp) -> Number | bool | str:
        # logic operator
        if node.op.type == TokenType.AND:
            return bool(self.visit(node.left)) and bool(self.visit(node.right))
        elif node.op.type == TokenType.OR:
            return bool(self.visit(node.left)) or bool(self.visit(node.right))

        # arithmetic operator
        if node.op.type == TokenType.PLUS:
            return cast(Number | str, self.visit(node.left) + self.visit(node.right))
        elif node.op.type == TokenType.MINUS:
            return cast(Number, self.visit(node.left) - self.visit(node.right))
        elif node.op.type == TokenType.MUL:
            return cast(Number, self.visit(node.left) * self.visit(node.right))
        elif node.op.type == TokenType.INTEGER_DIV:
            return cast(Number, self.visit(node.left) // self.visit(node.right))
        elif node.op.type == TokenType.FLOAT_DIV:
            return float(self.visit(node.left)) / float(self.visit(node.right))

        # comparison operator
        if node.op.type == TokenType.LT:
            return float(self.visit(node.left)) < float(self.visit(node.right))
        elif node.op.type == TokenType.GT:
            return float(self.visit(node.left)) > float(self.visit(node.right))
        elif node.op.type == TokenType.EQ:
            return float(self.visit(node.left)) == float(self.visit(node.right))
        elif node.op.type == TokenType.NE:
            return float(self.visit(node.left)) != float(self.visit(node.right))
        elif node.op.type == TokenType.LE:
            return float(self.visit(node.left)) <= float(self.visit(node.right))
        elif node.op.type == TokenType.GE:
            return float(self.visit(node.left)) >= float(self.visit(node.right))

        # !!
        raise UnknownOperatorError(ErrorCode.UNKNOWN_BIN_OP, node.token)

    def visit_Num(self, node: Num):
        return node.value

    def visit_String(self, node: String):
        return node.value

    def visit_Bool(self, node: Bool):
        if node.token.type == TokenType.TRUE:
            return True
        elif node.token.type == TokenType.FALSE:
            return False
        raise UnknownBooleanError()

    def visit_UnaryOp(self, node: UnaryOp) -> Number | bool:
        op = node.op.type
        # negative bang
        if op == TokenType.NOT:
            return not self.visit(node.expr)

        # signal bang
        if op == TokenType.PLUS:
            return cast(Number, +self.visit(node.expr))
        elif op == TokenType.MINUS:
            return cast(Number, -self.visit(node.expr))
        raise UnknownOperatorError(ErrorCode.UNKNOWN_UNARY_OP, node.token)

    def visit_Compound(self, node: Compound) -> None:
        for child in node.children:
            self.visit(child)

    def visit_ConstAssign(self, node: ConstAssign):
        ar = self.call_stack.peek()
        if node.const_type == ConstType.NON_ARRAY:
            const_name = node.left.value
            value = self.visit(node.right)
            ar[const_name] = value
        elif node.const_type == ConstType.ARRAY:
            const_name = node.left.value
            elements: dict[int, Any] = {}
            array_type = cast(ArrayType, node.type)
            indices = list(
                range(self.visit(array_type.lower), self.visit(array_type.upper) + 1)
            )
            params = cast(Compound, node.right).children
            for k, v in zip(indices, params):
                elements[k] = self.visit(v)
            ar[const_name] = elements
            ar.set_meta(const_name, ElementType.ARRAY)

    def visit_Assign(self, node: Assign) -> None:
        var_value = self.visit(node.right)
        ar = self.call_stack.peek()

        if isinstance(node.left, IndexVar):
            # array [index] = value
            var_name = node.left.value
            index: int = self.visit(node.left.index)
            if var_name.find(".") != -1:
                record_name, record_key = var_name.split(".")
                ar[record_name][record_key][index] = var_value
            else:
                ar[var_name][index] = var_value
        elif isinstance(node.left, RecordVar):
            # user.Age = value
            record_name, record_key = node.left.name, node.left.key
            ar[record_name][record_key] = var_value
        else:
            # identifier = value
            var_name = node.left.value
            if var_name in ar.members_meta:
                limit = ar.get_meta(var_name).limit
                if limit > 0:
                    if len(var_value) > limit:
                        message = f"Warning: String literal has more characters[{len(var_value)}] than short string length[{limit}]"
                        SpiUtil.print_w(message=message)
                    ar[var_name] = cast(str, var_value)[0:limit]
                else:
                    ar[var_name] = var_value
            else:
                ar[var_name] = var_value

    def visit_Var(self, node: Var) -> Any:
        var_name = node.value

        ar = self.call_stack.peek()
        if var_name.find(".") != -1:
            type_name, type_key = var_name.split(".")
            meta = ar.get_meta(type_name)
            if meta.is_enum:
                enum_decl: EnumDecl | None = ar.get(type_name)
                if enum_decl is None:
                    raise NullPointerError()
                index = enum_decl.reversed_entries[type_key]
                return EnumObject(index=index, name=type_key)
            elif meta.is_record_instance:
                # TODO: it will not dispatch RecordVar to here
                raise InterpreterError()
            else:
                raise InterpreterError()
        else:
            var_value = ar.get(var_name)
            return var_value

    def visit_RecordVar(self, node: RecordVar) -> Any:
        ar = self.call_stack.peek()
        var_value = ar.get(node.name)
        return var_value[node.key]

    def visit_IndexVar(self, node: IndexVar) -> Any:
        var_name = node.value
        index: int = self.visit(node.index)
        ar = self.call_stack.peek()
        if isinstance(node.left, RecordVar):
            record_name, record_field = node.left.name, node.left.key
            meta = ar.get_meta(record_name)
            record_decl: RecordDecl = ar[meta.ref_record_name]
            field_decl: FieldDecl
            for f in record_decl.fields:
                if f.var_node.value == record_field:
                    field_decl = f
                    break
            if ElementType(field_decl.type_node.value) == ElementType.STRING:
                string_const = ar[record_name][record_field]
                if len(string_const) >= index:
                    return string_const[index - 1]
                else:
                    return ""
            else:
                array = ar[record_name][record_field]

                if index in array:
                    return array[index]
                else:
                    message = f"Warning: range check error while evaluating constants {var_name}[{index}]"
                    SpiUtil.print_w(message=message)
                    element_type = ElementType(
                        (cast(ArrayType, field_decl.type_node)).element_type.value
                    )
                    if element_type == ElementType.BOOL:
                        return False
                    if element_type == ElementType.INTEGER:
                        return 0
                    if element_type == ElementType.REAL:
                        return 0.0
                    if element_type == ElementType.ARRAY:
                        return {}
            return ar[record_name][record_field][index]
        else:
            if ar.get_meta(var_name).type == ElementType.STRING:
                string_const = ar.get(var_name)
                if len(string_const) >= index:
                    return string_const[index - 1]
                else:
                    return ""
            else:
                array = ar.get(var_name)

                if index in array:
                    return array[index]
                else:
                    message = f"Warning: range check error while evaluating constants {var_name}[{index}]"
                    SpiUtil.print_w(message=message)
                    element_type = ar.get_meta(var_name).type
                    if element_type == ElementType.BOOL:
                        return False
                    if element_type == ElementType.INTEGER:
                        return 0
                    if element_type == ElementType.REAL:
                        return 0.0
                    if element_type == ElementType.ARRAY:
                        return {}

    def visit_NoOp(self, node: NoOp) -> None:
        pass

    def visit_WhileStatement(self, node: WhileStatement) -> None:
        while self.visit(node.condition) is True:
            self.visit(node.block)

    def visit_ForStatement(self, node: ForStatement) -> None:
        ar = self.call_stack.peek()
        var_name = node.initialization.left.value
        self.visit(node.initialization)
        bound_value = cast(Number, self.visit(node.bound))
        var_value = ar[var_name]
        while var_value <= bound_value:
            self.visit(node.block)
            var_value += 1
            if var_value <= bound_value:
                ar[var_name] = var_value

    def visit_ProcedureDecl(self, node: ProcedureDecl) -> None:
        pass

    def visit_ProcedureCall(self, node: ProcedureCall) -> None:
        proc_name = node.proc_name
        proc_symbol = node.proc_symbol

        if proc_symbol is None:
            raise NullPointerError

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
            if proc_symbol.name.upper() == NativeMethod.WRITE.name:
                actual_params = node.actual_params

                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                # output actual params
                for argument_node in actual_params:
                    object = self.visit(argument_node)
                    if isinstance(object, EnumObject):
                        print(object.name, end="")
                    else:
                        print(object, end="")

                self.log(f"LEAVE: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return
            elif proc_symbol.name.upper() == NativeMethod.WRITELN.name:
                actual_params = node.actual_params

                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                # output actual params
                for argument_node in actual_params:
                    object = self.visit(argument_node)
                    if isinstance(object, EnumObject):
                        print(object.name, end="")
                    else:
                        print(object, end="")
                print()

                self.log(f"LEAVE: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return
            elif proc_symbol.name.upper() == NativeMethod.READ.name:
                actual_params = node.actual_params
                var_name = actual_params[0].value

                self.call_stack.push(ar)

                self.log(f"ENTER: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                # output actual params
                user_input = input("")
                ar[var_name] = user_input

                self.log(f"LEAVE: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return
            elif proc_symbol.name.upper() == NativeMethod.READLN.name:
                actual_params = node.actual_params
                var_name = actual_params[0].value

                self.call_stack.push(ar)

                self.log(f"ENTER: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                # output actual params
                user_input = input("")
                ar[var_name] = user_input

                self.log(f"LEAVE: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return
            elif proc_symbol.name.upper() == NativeMethod.SETLENGTH.name:
                actual_params = node.actual_params

                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                # core
                arr_name = actual_params[0].value
                new_length = actual_params[1].value
                element_type = ar.get_meta(arr_name).type
                if element_type == ElementType.STRING:
                    if len(ar[arr_name]) > new_length:
                        ar[arr_name] = ar[arr_name][0:new_length]
                else:
                    if ar.get_meta(arr_name).dynamic is False:
                        raise StaticArrayModifyLengthError()

                    for i in range(0, new_length):
                        if i in ar[arr_name]:
                            continue
                        if element_type == ElementType.BOOL:
                            ar[arr_name][i] = False
                        if element_type == ElementType.INTEGER:
                            ar[arr_name][i] = 0
                        if element_type == ElementType.REAL:
                            ar[arr_name][i] = 0.0
                        if element_type == ElementType.ARRAY:
                            ar[arr_name][i] = {}

                self.log(f"LEAVE: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return
            else:
                raise UnknownBuiltinProcedureError()
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
                raise NullPointerError
            self.visit(proc_symbol.block_ast)

            self.log(f"LEAVE: PROCEDURE {proc_name}")
            self.log(str(self.call_stack))

            self.call_stack.pop()
            pass

    def visit_FunctionDecl(self, node: FunctionDecl) -> None:
        pass

    def visit_IfStatement(self, node: IfStatement) -> None:
        flag: bool = self.visit(node.condition)

        if flag == True:
            self.visit(node.then_branch)
            return
        else:
            for branch in node.else_if_branches:
                sub_flag: bool = self.visit(branch)
                if sub_flag == True:
                    self.visit(branch.then_branch)
                    return

        if node.else_branch != None:
            self.visit(node.else_branch)

    def visit_CaseStatement(self, node: CaseStatement) -> None:
        matcher_value = self.visit(node.matcher)

        for condition, branch in node.branches:
            condition_value = self.visit(condition)
            if matcher_value == condition_value:
                self.visit(branch)
                return
            else:
                continue

        if node.else_branch != None:
            self.visit(node.else_branch)

    def visit_FunctionCall(self, node: FunctionCall) -> Any:
        func_name = node.func_name
        func_symbol = node.func_symbol

        if func_symbol is None:
            raise NullPointerError

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
            if func_symbol.name.upper() == NativeMethod.LENGTH.name:
                actual_params = node.actual_params

                # [0] = LENGTH, [1] = ARRAY_NAME
                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                ar[RETURN_NUM] = len(self.visit(actual_params[i]))

                self.log(f"LEAVE: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return ar[RETURN_NUM]
            elif func_symbol.name.upper() == NativeMethod.LOW.name:
                actual_params = node.actual_params

                # [0] = LENGTH, [1] = ARRAY_NAME
                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                ar[RETURN_NUM] = min(self.visit(actual_params[i]))

                self.log(f"LEAVE: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return ar[RETURN_NUM]
            elif func_symbol.name.upper() == NativeMethod.HIGH.name:
                actual_params = node.actual_params

                # [0] = LENGTH, [1] = ARRAY_NAME
                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                ar[RETURN_NUM] = max(self.visit(actual_params[i]))

                self.log(f"LEAVE: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return ar[RETURN_NUM]
            elif func_symbol.name.upper() == NativeMethod.ORD.name:
                actual_params = node.actual_params

                self.call_stack.push(ar)

                self.log(f"ENTER: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                ar[RETURN_NUM] = self.visit(actual_params[1]).index

                self.log(f"LEAVE: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return ar[RETURN_NUM]
            else:
                raise UnknownBuiltinFunctionError()
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
                raise NullPointerError
            self.visit(func_symbol.block_ast)

            self.log(f"LEAVE: FUNCTION {func_name}")
            self.log(str(self.call_stack))

            self.call_stack.pop()

            return ar[func_name]

    def visit_MethodCall(self, node: MethodCall) -> Any:
        method_full_name = node.method_full_name
        class_or_inst_name, method_name = method_full_name.split(".")
        method_symbol = node.method_symbol

        if method_symbol is None:
            raise UnknownMethodSymbolError

        ar = ActivationRecord(
            name=method_full_name,
            type=ARType.METHOD,
            nesting_level=method_symbol.scope_level + 1,
        )
        instance: dict[str, Any] = {}
        pre_ar = self.call_stack.peek()
        if pre_ar is not None:
            ar.copy_from(pre_ar, False)
            meta = pre_ar.get_meta(class_or_inst_name)
            if meta.is_instance:
                inst_name = class_or_inst_name
                instance = pre_ar.get(inst_name)
                for k, v in instance.items():
                    ar[k] = v
            elif meta.is_class:
                class_name = class_or_inst_name
                class_decl = pre_ar.get(SpiUtil.toClassName(class_name))
                for field in class_decl.fields:
                    if field.type_node.token.type == TokenType.BOOLEAN:
                        instance[field.var_node.value] = False
                    elif field.type_node.token.type == TokenType.INTEGER:
                        instance[field.var_node.value] = 0
                    elif field.type_node.token.type == TokenType.REAL:
                        instance[field.var_node.value] = 0.0
                    elif field.type_node.token.type == TokenType.STRING:
                        instance[field.var_node.value] = ""
                    elif field.type_node.token.type == TokenType.ARRAY:
                        instance[field.var_node.value] = []
                    elif field.type_node.token.type == TokenType.CLASS:
                        instance[field.var_node.value] = {}
            else:
                raise UnknownMethodSymbolError()

            pass

        formal_params = method_symbol.formal_params
        actual_params = node.actual_params

        method_type = method_symbol.method_type
        if method_type == MethodType.PROCEDURE:
            for param_symbol, argument_node in zip(formal_params, actual_params):
                ar[param_symbol.name] = self.visit(argument_node)
        else:
            for param_symbol, argument_node in zip(formal_params, actual_params[1:]):
                ar[param_symbol.name] = self.visit(argument_node)

        self.call_stack.push(ar)

        self.log(f"ENTER: METHOD {method_full_name}")
        self.log(str(self.call_stack))

        # evaluate procedure body
        if method_symbol.block_ast is None:
            raise NullPointerError
        self.visit(method_symbol.block_ast)

        self.log(f"LEAVE: METHOD {method_full_name}")
        self.log(str(self.call_stack))

        # before leave method scope, should assign the modified field value outside
        for k, v in instance.items():
            instance[k] = ar[k]

        self.call_stack.pop()

        method_type = method_symbol.method_type
        if method_type == MethodType.CONSTRUCTOR:
            # T.Create; BEGIN a:=?;b:=?; END; will not return any explicit value, but should generate an instance value for assignment
            return instance
        elif method_type == MethodType.FUNCTION:
            # T.m; BEGIN m := ? END; should assign to m as return value
            return ar[method_name]
        else:
            return None

    def interpret(self):
        tree = self.tree
        if tree is None:
            return ""
        return self.visit(tree)
