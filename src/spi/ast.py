"""SPI - Simple Pascal Interpreter. Part 19"""

from __future__ import annotations

from typing import Any

from spi.token import Token

###############################################################################
#                                                                             #
#  AST                                                                  #
#                                                                             #
###############################################################################


class AST:
    def __init__(self) -> None:
        self._num: int | None = None
        self.value: Any = None


class Statement(AST):
    def __init__(self):
        pass


class Declaration(Statement):
    def __init__(self):
        super().__init__()


class TypeDeclaration(Declaration):
    def __init__(self, type_name: Var, type_def: Type):
        self.type_name = type_name
        self.type_def = type_def


class Expression(AST):
    def __init__(self):
        super().__init__()


class Type(Expression):
    def __init__(self, token: Token) -> None:
        self.token = token
        self.value = token.value


class PrimitiveType(Type):
    def __init__(self, token):
        super().__init__(token)

    def __str__(self):
        return self.value


class StringType(Type):
    def __init__(self, token, limit: Expression | None = None):
        super().__init__(token)
        self.limit = limit

    def __str__(self):
        return "STRING"


class ArrayType(Type):
    def __init__(
        self,
        token,
        element_type: Type,
        lower: Expression,
        upper: Expression,
        dynamic: bool = False,
    ):
        super().__init__(token)
        self.element_type = element_type
        self.lower = lower
        self.upper = upper
        self.dynamic = dynamic

    def __str__(self):
        return "Array[{element_type_name}]".format(
            element_type_name=str(self.element_type)
        )


class EnumType(Type):
    def __init__(self, enum_values: list[str]):
        self.enum_values = enum_values


class BinOp(Expression):
    def __init__(self, left: AST, op: Token, right: AST) -> None:
        self.left = left
        self.token = self.op = op
        self.right = right


class Num(Expression):
    def __init__(self, token: Token) -> None:
        self.token = token
        self.value = token.value


class Bool(Expression):
    def __init__(self, token: Token) -> None:
        self.token = token
        self.value: bool = token.value


class String(Expression):
    def __init__(self, token: Token):
        self.token = token
        self.value: str = token.value


class Char(Expression):
    def __init__(self, token: Token):
        self.token = token
        self.value: str = token.value


class UnaryOp(Expression):
    def __init__(self, op: Token, expr: AST) -> None:
        self.token = self.op = op
        self.expr = expr


class Compound(Statement):
    """Represents a 'BEGIN ... END' block"""

    def __init__(self) -> None:
        self.children: list[AST] = []


class IfStatement(Statement):
    """Represents a 'if ... then... elseif ... then ... else' statement"""

    def __init__(
        self,
        condition: AST,
        then_branch: AST,
        else_if_branches: list[IfStatement],
        else_branch: AST | None,
    ) -> None:
        self.condition = condition
        self.then_branch = then_branch
        self.else_if_branches = else_if_branches
        self.else_branch = else_branch


class WhileStatement(Statement):
    """Represents a 'WHILE ... DO ... BEGIN ... END' block"""

    def __init__(self, condition: AST, block: Compound) -> None:
        self.condition = condition
        self.block = block


class ForStatement(Statement):
    """Represents a 'FOR left:= right TO ... DO (BEGIN ... END) or statement' block"""

    def __init__(self, initialization: Assign, bound: AST, block: AST) -> None:
        self.initialization = initialization
        self.bound = bound
        self.block = block


class CaseStatement(Statement):
    """Represents a 'CASE variable OF case_list (ELSE statement)? END' block"""

    def __init__(
        self, case_expr: AST, case_items: list[CaseItem], else_stmt: AST | None = None
    ) -> None:
        self.case_expr = case_expr  # 被判断的表达式
        self.case_items = case_items  # case项目列表
        self.else_stmt = else_stmt  # else语句（可选）


class CaseItem(AST):
    """Represents a case item with labels and a statement"""

    def __init__(self, labels: list[CaseLabel], statement: AST) -> None:
        self.labels = labels  # case标签列表
        self.statement = statement  # 对应的语句


class CaseLabel(AST):
    """Represents a case label value"""

    def __init__(self, value) -> None:
        self.value = value  # 标签值


class Assign(Statement):
    def __init__(self, left: Expression, op: Token, right) -> None:
        self.left = left
        self.token = self.op = op
        self.right = right


class Var(Expression):
    """The Var node is constructed out of ID token."""

    def __init__(self, token: Token) -> None:
        self.token = token
        self.value = token.value


class AccessExpression(Expression):
    """Represents variable access with optional suffixes: ID[expr] or ID.field"""

    def __init__(self, base: Expression, suffixes: list[AccessSuffix]):
        self.base = base
        self.suffixes = suffixes
        # Set token from the base expression
        self.token = getattr(base, "token", None)


class AccessSuffix(Expression):
    """Base class for access suffixes"""

    pass


class IndexSuffix(AccessSuffix):
    """Represents array index access [expr]"""

    def __init__(self, index: Expression):
        self.index = index


class MemberSuffix(AccessSuffix):
    """Represents member access .field"""

    def __init__(self, member: Token):
        self.member = member
        self.token = member


class NoOp(Statement):
    pass


class Program(Statement):
    def __init__(self, name: str, block: Block) -> None:
        self.name = name
        self.block = block


class Block(Statement):
    def __init__(
        self, declarations: list[Declaration], compound_statement: Compound
    ) -> None:
        self.declarations = declarations
        self.compound_statement = compound_statement


class VarDecl(Declaration):
    def __init__(self, var_node: Var, type_node: Type) -> None:
        self.var_node = var_node
        self.type_node = type_node


class Param(Expression):
    def __init__(self, var_node: Var, type_node: Type) -> None:
        self.var_node = var_node
        self.type_node = type_node


class ProcedureDecl(Declaration):
    def __init__(
        self, proc_name: str, formal_params: list[Param], block_node: Block
    ) -> None:
        self.proc_name = proc_name
        self.formal_params = formal_params  # a list of Param nodes
        self.block_node = block_node
        self.is_forward = False


class FunctionDecl(Declaration):
    def __init__(
        self,
        func_name: str,
        formal_params: list[Param],
        return_type: Type,
        block_node: Block,
    ) -> None:
        self.func_name = func_name
        self.formal_params = formal_params  # a list of Param nodes
        self.return_type = return_type
        self.block_node = block_node
        self.is_forward = False


class ProcedureCall(Statement):
    def __init__(
        self, proc_name: str, actual_params: list[Expression], token: Token
    ) -> None:
        self.proc_name = proc_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token


class FunctionCall(Expression):
    def __init__(
        self, func_name: str, actual_params: list[Expression], token: Token
    ) -> None:
        self.func_name = func_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token


class RecordType(Type):
    """表示记录类型定义，包含常规字段和可选的变体部分"""

    def __init__(
        self, fields: list[RecordField], variant_part: VariantPart | None = None
    ):
        self.fields = fields  # 常规字段列表
        self.variant_part = variant_part  # 可选的变体部分

    @property
    def field_names(self) -> list[str]:
        names: list[str] = []
        for field in self.fields:
            names.append(field.name.value)
        return names

    @property
    def field_entries(self) -> dict[str, Type]:
        entries: dict[str, Type] = {}
        for field in self.fields:
            entries[field.name.value] = field.type_node
        return entries


class RecordField(AST):
    """表示记录中的字段"""

    def __init__(self, name: Var, type_node: Type):
        self.name = name  # 字段名
        self.type_node = type_node  # 字段类型


class VariantPart(AST):
    """表示记录的变体部分"""

    def __init__(self, tag_field: Type, variant_cases: list[VariantCase]):
        self.tag_field = tag_field  # 标签字段（必须是枚举类型）
        self.variant_cases = variant_cases  # 变体情况列表


class VariantCase(AST):
    """表示变体记录中的一个变体情况"""

    def __init__(self, tag_values: list[str], fields: list[RecordField]):
        self.tag_values = tag_values  # 此变体情况对应的标签值列表
        self.fields = fields  # 该变体的字段列表


# ###############################################################################
# #                                                                             #
# #  SYMBOL                                                                    #
# #                                                                             #
# ###############################################################################


# class Symbol:
#     def __init__(self, name: str, type: Symbol | None = None) -> None:
#         self.name = name
#         self.type = type
#         self.scope_level: int = 0


# class VarSymbol(Symbol):
#     def __init__(self, name: str, type: Symbol | None, is_mutable: bool = True) -> None:
#         # Import here to avoid circular imports
#         from src.spi.symbol import NEVER_SYMBOL
#         super().__init__(name, type or NEVER_SYMBOL)
#         self.is_mutable = is_mutable
#         self.is_initialized = False  # Track if const has been initialized

#     def can_modify(self) -> bool:
#         """Check if this variable can be modified"""
#         if not self.is_mutable:
#             # Const variables can only be modified if not yet initialized
#             return not self.is_initialized
#         return True

#     def mark_initialized(self):
#         """Mark const variable as initialized"""
#         self.is_initialized = True

#     @property
#     def is_const(self) -> bool:
#         """Check if this is a const variable"""
#         return not self.is_mutable

#     def __str__(self) -> str:
#         return "<{class_name}(name='{name}', type='{type}', mutable={mutable})>".format(
#             class_name=self.__class__.__name__,
#             name=self.name,
#             type=self.type,
#             mutable=self.is_mutable,
#         )

#     __repr__ = __str__


# class StringTypeSymbol(Symbol):
#     def __init__(self, name: str, limit: int = 255) -> None:
#         super().__init__(name)
#         self.limit = limit

#     def __str__(self) -> str:
#         return self.name

#     def __repr__(self) -> str:
#         return "<{class_name}(name='{name}', limit='{limit}')>".format(
#             class_name=self.__class__.__name__, name=self.name, limit=self.limit
#         )


# class ArrayTypeSymbol(Symbol):
#     def __init__(self, name: str, element_type: Symbol) -> None:
#         super().__init__(name)
#         self.element_type = element_type

#     def __str__(self) -> str:
#         return "{name}[]".format(name=self.name)

#     def __repr__(self) -> str:
#         return "<{class_name}[{element_type_name}](name='{name}')>".format(
#             class_name=self.__class__.__name__,
#             name=self.name,
#             element_type_name=self.element_type.name,
#         )


# class EnumTypeSymbol(Symbol):
#     def __init__(self, name: str, values: list[str]) -> None:
#         super().__init__(name)
#         self.values = values
#         # Create a mapping from value name to ordinal
#         self.value_ordinals = {value: i for i, value in enumerate(values)}

#     def __str__(self) -> str:
#         return self.name

#     def __repr__(self) -> str:
#         return "<{class_name}(name='{name}', values={values})>".format(
#             class_name=self.__class__.__name__,
#             name=self.name,
#             values=self.values,
#         )


# class RecordTypeSymbol(Symbol):
#     """表示记录类型符号"""

#     def __init__(
#         self,
#         name: str,
#         fields: dict[str, Symbol],
#         variant_part: VariantPartSymbol | None = None,
#     ):
#         super().__init__(name)
#         self.fields = fields  # 字段名到字段符号的映射
#         self.variant_part = variant_part  # 可选的变体部分符号


# class RecordFieldSymbol(Symbol):
#     """表示记录字段符号"""

#     def __init__(self, name: str, type_symbol: Symbol):
#         super().__init__(name)
#         self.type = type_symbol


# class VariantPartSymbol:
#     """表示记录变体部分的符号"""

#     def __init__(
#         self,
#         tag_field: str,
#         tag_type: Symbol,
#         variant_cases: dict[str, dict[str, Symbol]],
#     ):
#         self.tag_field = tag_field  # 标签字段名
#         self.tag_type = tag_type  # 标签字段类型（必须是枚举类型）
#         self.variant_cases = variant_cases  # 标签值到变体字段符号的映射


# class ProcedureSymbol(Symbol):
#     def __init__(self, name: str, formal_params: list[Symbol] | None = None) -> None:
#         super().__init__(name)
#         # a list of VarSymbol objects
#         self.formal_params: list[Symbol] = (
#             [] if formal_params is None else formal_params
#         )
#         # a reference to procedure's body (AST sub-tree)
#         self.block_ast: Block | None = None

#     def __str__(self) -> str:
#         return "<{class_name}(name={name}, parameters={params})>".format(
#             class_name=self.__class__.__name__,
#             name=self.name,
#             params=self.formal_params,
#         )

#     __repr__ = __str__


# class BuiltinProcedureSymbol(Symbol):
#     def __init__(self, name: str, output_params: list[Symbol] | None = None) -> None:
#         super().__init__(name)
#         # a list of VarSymbol objects
#         self.output_params: list[Symbol] = (
#             [] if output_params is None else output_params
#         )

#     def __str__(self) -> str:
#         return "<{class_name}(name={name}, parameters={params})>".format(
#             class_name=self.__class__.__name__,
#             name=self.name,
#             params=self.output_params,
#         )

#     __repr__ = __str__


# class FunctionSymbol(Symbol):
#     def __init__(
#         self, name: str, return_type: Type, formal_params: list[Symbol] | None = None
#     ) -> None:
#         super().__init__(name)
#         # a list of VarSymbol objects
#         self.formal_params: list[Symbol] = (
#             [] if formal_params is None else formal_params
#         )
#         self.return_type = return_type
#         # a reference to procedure's body (AST sub-tree)
#         self.block_ast: Block | None = None

#     def __str__(self) -> str:
#         return "<{class_name}(name={name},return_type={return_type} parameters={params})>".format(
#             class_name=self.__class__.__name__,
#             name=self.name,
#             return_type=self.return_type,
#             params=self.formal_params,
#         )

#     __repr__ = __str__


# class BuiltinFunctionSymbol(Symbol):
#     def __init__(
#         self, name: str, return_type: Type, formal_params: list[Symbol] | None = None
#     ) -> None:
#         super().__init__(name)
#         # a list of VarSymbol objects
#         self.formal_params: list[Symbol] = (
#             [] if formal_params is None else formal_params
#         )
#         self.return_type = return_type
#         # a reference to procedure's body (AST sub-tree)
#         self.block_ast: Block | None = None

#     def __str__(self) -> str:
#         return "<{class_name}(name={name},return_type={return_type} parameters={params})>".format(
#             class_name=self.__class__.__name__,
#             name=self.name,
#             return_type=self.return_type,
#             params=self.formal_params,
#         )

#     __repr__ = __str__
