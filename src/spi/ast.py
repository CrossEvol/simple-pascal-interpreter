"""SPI - Simple Pascal Interpreter. Part 19"""

from __future__ import annotations

from enum import Enum
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

    def __str__(self):
        return f"{self.__class__.__name__}()"

    def __repr__(self):
        return self.__str__()


class Statement(AST):
    def __init__(self):
        super().__init__()

    def __str__(self):
        return f"{self.__class__.__name__}()"

    def __repr__(self):
        return self.__str__()


class Declaration(Statement):
    def __init__(self):
        super().__init__()

    def __str__(self):
        return f"{self.__class__.__name__}()"

    def __repr__(self):
        return self.__str__()


class TypeDeclaration(Declaration):
    def __init__(self, type_name: Var, type_def: Type):
        super().__init__()
        self.type_name = type_name
        self.type_def = type_def

    def __str__(self):
        if self.type_name and self.type_def:
            return f"{self.__class__.__name__}({self.type_name}, {self.type_def})"
        return f"{self.__class__.__name__}()"

    def __repr__(self):
        return self.__str__()


class Expression(AST):
    def __init__(self):
        super().__init__()

    def __str__(self):
        return f"{self.__class__.__name__}()"

    def __repr__(self):
        return self.__str__()


class Type(Expression):
    def __init__(self, token: Token) -> None:
        super().__init__()
        self.token = token
        self.value = token.value

    def __str__(self):
        if self.value:
            return f"{self.__class__.__name__}({self.value})"
        return f"{self.__class__.__name__}()"

    def __repr__(self):
        return self.__str__()


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
        bounds: SubrangeType | None = None,
        dynamic: bool = False,
    ):
        super().__init__(token)
        self.element_type = element_type
        self.bounds = bounds  # SubrangeType for array bounds
        self.dynamic = dynamic

        # Backward compatibility properties
        if bounds:
            self.lower = bounds.lower
            self.upper = bounds.upper
        else:
            # For dynamic arrays, create default zero bounds
            from spi.token import Token, TokenType

            zero_token = Token(
                TokenType.INTEGER_CONST,
                0,
                token.lineno if token else 1,
                token.column if token else 1,
            )
            self.lower = Num(zero_token)
            self.upper = Num(zero_token)

    def __str__(self):
        return "Array[{element_type_name}]".format(
            element_type_name=str(self.element_type)
        )


class SubrangeType(Type):
    """Represents a subrange type like 1..10"""

    def __init__(self, token: Token, lower: Expression, upper: Expression):
        super().__init__(token)
        self.lower = lower
        self.upper = upper

    def __str__(self):
        return f"{self.lower}..{self.upper}"


class EnumType(Type):
    def __init__(self, token: Token, enum_values: list[str]):
        super().__init__(token=token)
        self.token = token
        self.enum_values = enum_values

    def __str__(self):
        if self.enum_values:
            return f"{self.__class__.__name__}({self.enum_values})"
        return f"{self.__class__.__name__}()"

    def __repr__(self):
        return self.__str__()


class BinOp(Expression):
    def __init__(self, left: AST, op: Token, right: AST) -> None:
        super().__init__()
        self.left = left
        self.token = self.op = op
        self.right = right

    def __str__(self):
        return f"{self.__class__.__name__}({self.left}, {self.op}, {self.right})"

    def __repr__(self):
        return self.__str__()


class Num(Expression):
    def __init__(self, token: Token) -> None:
        super().__init__()
        self.token = token
        self.value = token.value

    def __str__(self):
        return f"{self.__class__.__name__}({self.value})"

    def __repr__(self):
        return self.__str__()


class Bool(Expression):
    def __init__(self, token: Token) -> None:
        super().__init__()
        self.token = token
        self.value: bool = token.value

    def __str__(self):
        return f"{self.__class__.__name__}({self.value})"

    def __repr__(self):
        return self.__str__()


class String(Expression):
    def __init__(self, token: Token):
        super().__init__()
        self.token = token
        self.value: str = token.value

    def __str__(self):
        return f"{self.__class__.__name__}('{self.value}')"

    def __repr__(self):
        return self.__str__()


class Char(Expression):
    def __init__(self, token: Token):
        super().__init__()
        self.token = token
        self.value: str = token.value

    def __str__(self):
        return f"{self.__class__.__name__}('{self.value}')"

    def __repr__(self):
        return self.__str__()


class UnaryOp(Expression):
    def __init__(self, op: Token, expr: AST) -> None:
        super().__init__()
        self.token = self.op = op
        self.expr = expr

    def __str__(self):
        return f"{self.__class__.__name__}({self.op}, {self.expr})"

    def __repr__(self):
        return self.__str__()


class SetLiteral(Expression):
    """Represents a set literal like [1, 3..5, 8]"""

    def __init__(self, token: Token, elements: list[Expression]):
        super().__init__()
        self.token = token
        self.elements = elements  # Mix of individual values and SubrangeType

    def __str__(self):
        return f"{self.__class__.__name__}({self.elements})"

    def __repr__(self):
        return self.__str__()


class InOperator(Expression):
    """Represents membership test like 'value in set'"""

    def __init__(self, value: Expression, set_expr: Expression, token: Token):
        super().__init__()
        self.value = value
        self.set_expr = set_expr
        self.token = token

    def __str__(self):
        return f"{self.__class__.__name__}({self.value}, {self.set_expr})"

    def __repr__(self):
        return self.__str__()


class Compound(Statement):
    """Represents a 'BEGIN ... END' block"""

    def __init__(self) -> None:
        super().__init__()
        self.children: list[AST] = []

    def __str__(self):
        return f"{self.__class__.__name__}(children={len(self.children)})"

    def __repr__(self):
        return self.__str__()


class IfStatement(Statement):
    """Represents a 'if ... then... elseif ... then ... else' statement"""

    def __init__(
        self,
        condition: AST,
        then_branch: AST,
        else_if_branches: list[IfStatement],
        else_branch: AST | None,
    ) -> None:
        super().__init__()
        self.condition = condition
        self.then_branch = then_branch
        self.else_if_branches = else_if_branches
        self.else_branch = else_branch

    def __str__(self):
        return f"{self.__class__.__name__}({self.condition}, then: {self.then_branch}, elifs: {len(self.else_if_branches)}, else: {self.else_branch})"

    def __repr__(self):
        return self.__str__()


class WhileStatement(Statement):
    """Represents a 'WHILE ... DO ... BEGIN ... END' block"""

    def __init__(self, condition: AST, block: Compound) -> None:
        super().__init__()
        self.condition = condition
        self.block = block

    def __str__(self):
        return f"{self.__class__.__name__}({self.condition}, {self.block})"

    def __repr__(self):
        return self.__str__()


class ForStatement(Statement):
    """Represents a 'FOR left:= right TO ... DO (BEGIN ... END) or statement' block"""

    def __init__(self, initialization: Assign, bound: AST, block: AST) -> None:
        super().__init__()
        self.initialization = initialization
        self.bound = bound
        self.block = block

    def __str__(self):
        return f"{self.__class__.__name__}({self.initialization}, {self.bound}, {self.block})"

    def __repr__(self):
        return self.__str__()


class CaseStatement(Statement):
    """Represents a 'CASE variable OF case_list (ELSE statement)? END' block"""

    def __init__(
        self, case_expr: AST, case_items: list[CaseItem], else_stmt: AST | None = None
    ) -> None:
        super().__init__()
        self.case_expr = case_expr  # 被判断的表达式
        self.case_items = case_items  # case项目列表
        self.else_stmt = else_stmt  # else语句（可选）

    def __str__(self):
        return f"{self.__class__.__name__}({self.case_expr}, {len(self.case_items)} cases, else: {self.else_stmt})"

    def __repr__(self):
        return self.__str__()


class CaseItem(AST):
    """Represents a case item with labels and a statement"""

    def __init__(self, labels: list[CaseLabel], statement: AST) -> None:
        super().__init__()
        self.labels = labels  # case标签列表
        self.statement = statement  # 对应的语句

    def __str__(self):
        return f"{self.__class__.__name__}({len(self.labels)} labels, {self.statement})"

    def __repr__(self):
        return self.__str__()


class CaseLabel(AST):
    """Represents a case label value"""

    def __init__(self, value) -> None:
        super().__init__()
        self.value = value  # 标签值

    def __str__(self):
        return f"{self.__class__.__name__}({self.value})"

    def __repr__(self):
        return self.__str__()


class Assign(Statement):
    def __init__(self, left: Expression, op: Token, right) -> None:
        super().__init__()
        self.left = left
        self.token = self.op = op
        self.right = right

    def __str__(self):
        return f"{self.__class__.__name__}({self.left}, {self.op}, {self.right})"

    def __repr__(self):
        return self.__str__()


class Var(Expression):
    """The Var node is constructed out of ID token."""

    def __init__(self, token: Token) -> None:
        super().__init__()
        self.token = token
        self.value = token.value

    def __str__(self):
        return f"{self.__class__.__name__}({self.value})"

    def __repr__(self):
        return self.__str__()


class AccessExpression(Expression):
    """Represents variable access with optional suffixes: ID[expr] or ID.field"""

    def __init__(self, base: Expression, suffixes: list[AccessSuffix]):
        super().__init__()
        self.base = base
        self.suffixes = suffixes
        # Set token from the base expression
        self.token = getattr(base, "token", None)

    def __str__(self):
        return f"{self.__class__.__name__}({self.base}, {len(self.suffixes)} suffixes)"

    def __repr__(self):
        return self.__str__()


class AccessSuffix(Expression):
    """Base class for access suffixes"""

    def __str__(self):
        return f"{self.__class__.__name__}()"

    def __repr__(self):
        return self.__str__()


class IndexSuffix(AccessSuffix):
    """Represents array index access [expr]"""

    def __init__(self, index: Expression):
        super().__init__()
        self.index = index

    def __str__(self):
        return f"{self.__class__.__name__}({self.index})"

    def __repr__(self):
        return self.__str__()


class MemberSuffix(AccessSuffix):
    """Represents member access .field"""

    def __init__(self, member: Token):
        super().__init__()
        self.member = member
        self.token = member

    def __str__(self):
        return f"{self.__class__.__name__}({self.member})"

    def __repr__(self):
        return self.__str__()


class BreakStatement(Statement):
    """Represents a break statement"""

    def __init__(self, token: Token) -> None:
        super().__init__()
        self.token = token

    def __str__(self):
        return f"{self.__class__.__name__}()"

    def __repr__(self):
        return self.__str__()


class ContinueStatement(Statement):
    """Represents a continue statement"""

    def __init__(self, token: Token) -> None:
        super().__init__()
        self.token = token

    def __str__(self):
        return f"{self.__class__.__name__}()"

    def __repr__(self):
        return self.__str__()


class NoOp(Statement):
    def __str__(self):
        return f"{self.__class__.__name__}()"

    def __repr__(self):
        return self.__str__()


class Program(Statement):
    def __init__(self, name: str, block: Block) -> None:
        super().__init__()
        self.name = name
        self.block = block

    def __str__(self):
        return f"{self.__class__.__name__}({self.name}, {self.block})"

    def __repr__(self):
        return self.__str__()


class Block(Statement):
    def __init__(
        self, declarations: list[Declaration], compound_statement: Compound
    ) -> None:
        super().__init__()
        self.declarations = declarations
        self.compound_statement = compound_statement

    def __str__(self):
        return f"{self.__class__.__name__}({len(self.declarations)} declarations, {self.compound_statement})"

    def __repr__(self):
        return self.__str__()


class VarDecl(Declaration):
    def __init__(self, var_node: Var, type_node: Type) -> None:
        super().__init__()
        self.var_node = var_node
        self.type_node = type_node

    def __str__(self):
        return f"{self.__class__.__name__}({self.var_node}, {self.type_node})"

    def __repr__(self):
        return self.__str__()


class ConstDecl(Declaration):
    def __init__(self, var_node: Var, value_expr: Expression) -> None:
        super().__init__()
        self.var_node = var_node
        self.value_expr = value_expr

    def __str__(self):
        return f"{self.__class__.__name__}({self.var_node}, {self.value_expr})"

    def __repr__(self):
        return self.__str__()


class ParamMode(Enum):
    REFER = "refer"
    CONST = "const"
    CLONE = "clone"


class Param(Expression):
    def __init__(
        self, var_node: Var, type_node: Type, param_mode: str = "value"
    ) -> None:
        super().__init__()
        self.var_node = var_node
        self.type_node = type_node
        self.param_mode = param_mode  # ParamMode

    def __str__(self):
        return f"{self.__class__.__name__}({self.var_node}, {self.type_node}, {self.param_mode})"

    def __repr__(self):
        return self.__str__()


class ProcedureDecl(Declaration):
    def __init__(
        self, proc_name: str, formal_params: list[Param], block_node: Block
    ) -> None:
        super().__init__()
        self.proc_name = proc_name
        self.formal_params = formal_params  # a list of Param nodes
        self.block_node = block_node
        self.is_forward = False

    def __str__(self):
        return f"{self.__class__.__name__}({self.proc_name}, {len(self.formal_params)} params, {self.block_node})"

    def __repr__(self):
        return self.__str__()


class FunctionDecl(Declaration):
    def __init__(
        self,
        func_name: str,
        formal_params: list[Param],
        return_type: Type,
        block_node: Block,
    ) -> None:
        super().__init__()
        self.func_name = func_name
        self.formal_params = formal_params  # a list of Param nodes
        self.return_type = return_type
        self.block_node = block_node
        self.is_forward = False

    def __str__(self):
        return f"{self.__class__.__name__}({self.func_name}, {len(self.formal_params)} params, {self.return_type}, {self.block_node})"

    def __repr__(self):
        return self.__str__()


class ProcedureCall(Statement):
    def __init__(
        self, proc_name: str, actual_params: list[Expression], token: Token
    ) -> None:
        super().__init__()
        self.proc_name = proc_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token

    def __str__(self):
        return f"{self.__class__.__name__}({self.proc_name}, {len(self.actual_params)} args)"

    def __repr__(self):
        return self.__str__()


class FunctionCall(Expression):
    def __init__(
        self, func_name: str, actual_params: list[Expression], token: Token
    ) -> None:
        super().__init__()
        self.func_name = func_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token

    def __str__(self):
        return f"{self.__class__.__name__}({self.func_name}, {len(self.actual_params)} args)"

    def __repr__(self):
        return self.__str__()


class RecordType(Type):
    """表示记录类型定义，包含常规字段和可选的变体部分"""

    def __init__(
        self,
        token: Token,
        fields: list[RecordField],
        variant_part: VariantPart | None = None,
    ):
        super().__init__(token=token)
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

    def __str__(self):
        return f"{self.__class__.__name__}({len(self.fields)} fields, variant_part: {self.variant_part is not None})"

    def __repr__(self):
        return self.__str__()


class RecordField(AST):
    """表示记录中的字段"""

    def __init__(self, name: Var, type_node: Type):
        super().__init__()
        self.name = name  # 字段名
        self.type_node = type_node  # 字段类型

    def __str__(self):
        return f"{self.__class__.__name__}({self.name}, {self.type_node})"

    def __repr__(self):
        return self.__str__()


class VariantPart(AST):
    """表示记录的变体部分"""

    def __init__(self, tag_field: Type, variant_cases: list[VariantCase]):
        super().__init__()
        self.tag_field = tag_field  # 标签字段（必须是枚举类型）
        self.variant_cases = variant_cases  # 变体情况列表

    def __str__(self):
        return f"{self.__class__.__name__}({self.tag_field}, {len(self.variant_cases)} cases)"

    def __repr__(self):
        return self.__str__()


class VariantCase(AST):
    """表示变体记录中的一个变体情况"""

    def __init__(self, tag_values: list[str], fields: list[RecordField]):
        super().__init__()
        self.tag_values = tag_values  # 此变体情况对应的标签值列表
        self.fields = fields  # 该变体的字段列表

    def __str__(self):
        return (
            f"{self.__class__.__name__}({self.tag_values}, {len(self.fields)} fields)"
        )

    def __repr__(self):
        return self.__str__()
