from __future__ import annotations
from enum import Enum

from src.symbol import FunctionSymbol, MethodSymbol, ProcedureSymbol
from src.spi_token import Token


class AST:
    def __init__(self) -> None:
        self._num: int | None = None


class Type(AST):
    def __init__(self, token: Token) -> None:
        self.token = token
        self.value = token.value


class Decl(AST):
    def __init__(self):
        super().__init__()


class Def(AST):
    def __init__(self):
        super().__init__()


class BinOp(AST):
    def __init__(self, left: AST, op: Token, right: AST) -> None:
        self.left = left
        self.token = self.op = op
        self.right = right


type Number = int | float


class Num(AST):
    def __init__(self, token: Token) -> None:
        self.token = token
        self.value: Number = token.value


class Bool(AST):
    def __init__(self, token: Token) -> None:
        self.token = token
        self.value: bool = token.value


class String(AST):
    def __init__(self, token: Token):
        self.token = token
        self.value: str = token.value


class UnaryOp(AST):
    def __init__(self, op: Token, expr: AST) -> None:
        self.token = self.op = op
        self.expr = expr


class Var(AST):
    """The Var node is constructed out of ID token."""

    def __init__(self, token: Token) -> None:
        self.token = token
        self.value = token.value


class Compound(AST):
    """Represents a 'BEGIN ... END' block"""

    def __init__(self) -> None:
        self.children: list[AST] = []

    @staticmethod
    def of(params: list[AST]) -> "Compound":
        compound = Compound()
        compound.children = params
        return compound


class IfStatement(AST):
    """Represents a 'if ... then... elseif ... then ... else' statement"""

    def __init__(
        self,
        condition: AST,
        then_branch: AST,
        else_if_branches: list["IfStatement"],
        else_branch: AST | None,
    ) -> None:
        self.condition = condition
        self.then_branch = then_branch
        self.else_if_branches = else_if_branches
        self.else_branch = else_branch


class CaseStatement(AST):
    """Represents a 'case ... of ... else ... end' statement"""

    def __init__(
        self,
        matcher: AST,
        branches: list[tuple[AST, AST]],
        else_branch: AST | None,
    ) -> None:
        self.matcher = matcher
        self.branches = branches
        self.else_branch = else_branch


class Assign(AST):
    def __init__(self, left: Var, op: Token, right: AST) -> None:
        self.left = left
        self.token = self.op = op
        self.right = right


class WhileStatement(AST):
    """Represents a 'WHILE ... DO ... BEGIN ... END' block"""

    def __init__(self, condition: AST, block: Compound) -> None:
        self.condition = condition
        self.block = block


class ForStatement(AST):
    """Represents a 'FOR left:= right TO ... DO (BEGIN ... END) or statement' block"""

    def __init__(self, initialization: Assign, bound: AST, block: AST) -> None:
        self.initialization = initialization
        self.bound = bound
        self.block = block


class ConstType(Enum):
    NON_ARRAY = "NON_ARRAY"
    ARRAY = "ARRAY"
    UNDEFINED = "UNDEFINED"


class ConstAssign(Assign, Decl):
    def __init__(
        self,
        left: Var,
        op: Token,
        right: AST,
        const_type: ConstType,
        type: Type | None = None,
    ):
        super().__init__(left, op, right)
        self.const_type = const_type
        self.type = type


class EnumVar(Var):
    """in the form like Color.Red"""

    def __init__(self, token: Token, name: str, key: str) -> None:
        self.token = token
        self.name = name
        self.key = key


class RecordVar(Var):
    """in the form like user.ID"""

    def __init__(self, token: Token, name: str, key: str) -> None:
        self.token = token
        self.name = name
        self.key = key


class IndexVar(Var):
    """The IndexVar is for ID[index]"""

    def __init__(self, token, left: Var, index: AST):
        super().__init__(token)
        self.left = left
        self.index = index


class NoOp(AST):
    pass


class Block(AST):
    def __init__(
        self,
        declarations: list[Decl],
        compound_statement: Compound,
        meta_classes: dict[int, list[str]] = {},
    ) -> None:
        self.declarations = declarations
        self.compound_statement = compound_statement
        self.meta_classes = meta_classes


class Program(AST):
    def __init__(self, name: str, block: Block) -> None:
        self.name = name
        self.block = block


class Member(Decl):
    def __init__(self):
        super().__init__()


class MethodType(Enum):
    CONSTRUCTOR = "CONSTRUCTOR"
    DESTRUCTOR = "DESTRUCTOR"
    PROCEDURE = "PROCEDURE"
    FUNCTION = "FUNCTION"
    UNDEFINED = "UNDEFINED"


class FieldDecl(Member):
    def __init__(self, var_node: Var, type_node: Type) -> None:
        self.var_node = var_node
        self.type_node = type_node

    def to_VarDecl(self) -> "VarDecl":
        return VarDecl(var_node=self.var_node, type_node=self.type_node)


class VarDecl(Decl):
    def __init__(self, var_node: Var, type_node: Type) -> None:
        self.var_node = var_node
        self.type_node = type_node

    def to_FieldDecl(self) -> FieldDecl:
        return FieldDecl(var_node=self.var_node, type_node=self.type_node)


class Param(AST):
    def __init__(self, var_node: Var, type_node: Type) -> None:
        self.var_node = var_node
        self.type_node = type_node


class MethodDef(Member):
    def __init__(
        self,
        method_name: str,
        params: list[Param],
        return_type: Type,
        method_type: MethodType,
    ) -> None:
        self.method_name = method_name
        self.params = params
        self.return_type = return_type
        self.method_type = method_type


class MethodDecl(Member):
    def __init__(
        self,
        method_full_name: str,
        params: list[Param],
        return_type: Type,
        method_type: MethodType,
        block: Block,
    ) -> None:
        self.method_full_name = method_full_name
        self.params = params
        self.return_type = return_type
        self.method_type = method_type
        self.block = block

    def to_def(self) -> MethodDef:
        return MethodDef(
            method_name=self.method_full_name.split(".")[1],
            params=self.params,
            return_type=self.return_type,
            method_type=self.method_type,
        )


class ClassDecl(Decl):
    def __init__(
        self,
        class_name: str,
        fields: list[FieldDecl],
        methods: list[MethodDef],
        constructor: MethodDecl | None = None,
        destructor: MethodDecl | None = None,
    ):
        super().__init__()
        self.class_name = class_name
        self.fields = fields
        self.methods = methods
        self.constructor = constructor
        self.destructor = destructor


class RecordDecl(Decl):
    def __init__(
        self,
        record_name: str,
        fields: list[FieldDecl],
    ):
        super().__init__()
        self.record_name = record_name
        self.fields = fields


class EnumDecl(Decl):
    def __init__(self, enum_name: str, entries: dict[int, str]):
        super().__init__()
        self.enum_name = enum_name
        self.entries = entries
        self.reversed_entries = {value: key for key, value in entries.items()}


class VoidType(Type):
    def __init__(self, token):
        super().__init__(token)

    def __str__(self):
        return "VOID"


class PrimitiveType(Type):
    def __init__(self, token):
        super().__init__(token)

    def __str__(self):
        return self.value


class StringType(Type):

    def __init__(self, token, limit: Factor | None = None):
        super().__init__(token)
        self.limit = limit

    def __str__(self):
        return super().__str__()


class ArrayType(Type):
    def __init__(
        self,
        token,
        element_type: Type,
        lower: Factor,
        upper: Factor,
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


class ClassType(Type):
    def __init__(self, token):
        super().__init__(token)

    def __str__(self):
        return "Class[{class_name}]".format(class_name=self.value)


class EnumType(Type):
    def __init__(self, token):
        super().__init__(token)

    def __str__(self):
        return "Enum[{enum_name}]".format(enum_name=self.value)


class RecordType(Type):
    def __init__(self, token):
        super().__init__(token)

    def __str__(self):
        return "Record[{record_name}]".format(record_name=self.value)


class ProcedureDecl(Decl):
    def __init__(
        self, proc_name: str, formal_params: list[Param], block_node: Block
    ) -> None:
        self.proc_name = proc_name
        self.formal_params = formal_params  # a list of Param nodes
        self.block_node = block_node


class ProcedureDef(Def):
    def __init__(self, proc_name: str, formal_params: list[Param]) -> None:
        self.proc_name = proc_name
        self.formal_params = formal_params


class ConstructorDecl(Decl):
    def __init__(
        self, constructor_name: str, formal_params: list[Param], block_node: Block
    ) -> None:
        self.constructor_name = constructor_name
        self.formal_params = formal_params  # a list of Param nodes
        self.block_node = block_node


class ConstructorDef(Def):
    def __init__(self, constructor_name: str, formal_params: list[Param]) -> None:
        self.constructor_name = constructor_name
        self.formal_params = formal_params


class FunctionDecl(Decl):
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


class FunctionDef(Def):
    def __init__(
        self,
        func_name: str,
        formal_params: list[Param],
        return_type: Type,
    ) -> None:
        self.func_name = func_name
        self.formal_params = formal_params
        self.return_type = return_type


class AbstractCall(AST):
    def __init__(self):
        super().__init__()


class ProcedureCall(AbstractCall):
    def __init__(
        self, proc_name: str, actual_params: list["Expr"], token: Token
    ) -> None:
        self.proc_name = proc_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        # a reference to procedure declaration symbol
        self.proc_symbol: ProcedureSymbol | None = None


class FunctionCall(AbstractCall):
    def __init__(
        self, func_name: str, actual_params: list["Expr"], token: Token
    ) -> None:
        self.func_name = func_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        # a reference to procedure declaration symbol
        self.func_symbol: FunctionSymbol | None = None


class MethodCall(AbstractCall):
    def __init__(
        self,
        method_full_name: str,
        actual_params: list["Expr"],
        token: Token,
    ) -> None:
        self.method_full_name = method_full_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        # a reference to procedure declaration symbol
        self.method_symbol: MethodSymbol | None = None
        self.method_type: MethodType = MethodType.UNDEFINED


class GetItem(AST):
    """Represents a property access or array index access"""
    
    def __init__(self, token: Token, key: str | AST) -> None:
        self.token = token
        self.key = key  # String for property access, AST for index access
        self.is_property = isinstance(key, str)


class ExprGet(AST):
    """Represents a complex property access like a.b.c[0].d[1]"""
    
    def __init__(self, object: AST, gets: list[GetItem]) -> None:
        self.object = object  # Base object being accessed
        self.gets = gets      # List of property/index accesses
        
    def add_get(self, get_item: GetItem) -> None:
        self.gets.append(get_item)


class ExprSet(AST):
    """Represents an assignment to a complex property like a.b.c[0].d[1] = 2"""
    
    def __init__(self, expr_get: ExprGet, value: AST, token: Token) -> None:
        self.expr_get = expr_get  # Left side complex property access
        self.value = value        # Right side value to assign
        self.token = token        # Assignment token


type Statement = Compound | ProcedureCall | MethodCall | Assign | NoOp | IfStatement | CaseStatement | WhileStatement | ForStatement | ExprSet
type Expr = Factor | ExprGet
type Term = Factor
type Factor = UnaryOp | BinOp | Num | Bool | Var | FunctionCall | MethodCall | String
