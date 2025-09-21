###############################################################################
#                                                                             #
#  PARSER                                                                     #
#                                                                             #
###############################################################################

from __future__ import annotations

from spi.ast import (
    AST,
    AccessExpression,
    AccessSuffix,
    ArrayType,
    Assign,
    BinOp,
    Block,
    Bool,
    CaseItem,
    CaseLabel,
    CaseStatement,
    Char,
    Compound,
    Declaration,
    EnumType,
    Expression,
    ForStatement,
    FunctionCall,
    FunctionDecl,
    IfStatement,
    IndexSuffix,
    MemberSuffix,
    NoOp,
    Num,
    Param,
    PrimitiveType,
    ProcedureCall,
    ProcedureDecl,
    Program,
    RecordField,
    RecordType,
    Statement,
    String,
    StringType,
    Type,
    TypeDeclaration,
    UnaryOp,
    Var,
    VarDecl,
    VariantCase,
    VariantPart,
    WhileStatement,
)
from spi.error import (
    ErrorCode,
    ParserError,
    SemanticError,
)
from spi.lexer import Lexer
from spi.token import Token, TokenType


class Parser:
    def __init__(self, lexer: Lexer) -> None:
        self.lexer = lexer
        # set current token to the first token taken from the input
        self.current_token = self.get_next_token()

    def newZeroNum(self, lineno: int = -1, column: int = -1) -> Num:
        return Num(
            token=Token(TokenType.INTEGER_CONST, value=0, lineno=lineno, column=column)
        )

    def get_next_token(self):
        return self.lexer.get_next_token()

    def peek_next_token(self):
        return self.lexer.peek_next_token()

    def error(self, error_code: ErrorCode, token: Token):
        raise ParserError(
            error_code=error_code,
            token=token,
            message=f"{error_code.value} -> {token}",
        )

    def eat(self, token_type: TokenType) -> None:
        # compare the current token type with the passed token
        # type and if they match then "eat" the current token
        # and assign the next token to the self.current_token,
        # otherwise raise an exception.
        if self.current_token.type == token_type:
            self.current_token = self.get_next_token()
        else:
            self.error(
                error_code=ErrorCode.UNEXPECTED_TOKEN,
                token=self.current_token,
            )

    def program(self) -> Program:
        """program : PROGRAM variable SEMI block DOT"""
        self.eat(TokenType.PROGRAM)
        var_node = self.variable()
        prog_name = var_node.value
        self.eat(TokenType.SEMI)
        block_node = self.block()
        program_node = Program(prog_name, block_node)
        self.eat(TokenType.DOT)
        return program_node

    def block(self) -> Block:
        """block : declarations compound_statement"""
        declaration_nodes = self.declarations()
        compound_statement_node = self.compound_statement()
        node = Block(declaration_nodes, compound_statement_node)
        return node

    def declarations(self) -> list[Declaration]:
        """
        declarations : type_declarations? (VAR (variable_declaration SEMI)+)? procedure_declaration* function_declaration*
        """
        declarations: list[Declaration] = []

        if self.current_token.type == TokenType.TYPE:
            type_decls = self.type_declarations()
            declarations.extend(type_decls)

        if self.current_token.type == TokenType.VAR:
            self.eat(TokenType.VAR)
            while self.current_token.type == TokenType.ID:
                var_decl = self.variable_declaration()
                declarations.extend(var_decl)
                self.eat(TokenType.SEMI)

        while self.current_token.type == TokenType.PROCEDURE:
            proc_decl = self.procedure_declaration()
            declarations.append(proc_decl)

        while self.current_token.type == TokenType.FUNCTION:
            func_decl = self.function_declaration()
            declarations.append(func_decl)

        return declarations

    def type_declarations(self) -> list[Declaration]:
        """
        type_declarations : TYPE (type_declaration SEMI)+
        """
        self.eat(TokenType.TYPE)
        type_decls = []

        while self.current_token.type == TokenType.ID:
            type_decl = self.type_declaration()
            type_decls.append(type_decl)
            self.eat(TokenType.SEMI)

        return type_decls

    def type_declaration(self) -> Declaration:
        """
        type_declaration : ID EQ (enum_type | type_spec)
        """
        type_name = self.current_token.value
        type_token = self.current_token
        self.eat(TokenType.ID)
        self.eat(TokenType.EQ)

        # Check if it's an enum type (starts with LPAREN)
        if self.current_token.type == TokenType.LPAREN:
            type_def = self.enum_type()
        else:
            # For other types, we might need a different approach
            # For now, let's assume it's an alias to another type
            type_def = self.type_spec()

        return TypeDeclaration(
            Var(Token(TokenType.ID, type_name, type_token.lineno, type_token.column)),
            type_def,
        )

    def formal_parameters(self) -> list[Param]:
        """formal_parameters : ID (COMMA ID)* COLON type_spec"""
        param_nodes: list[Param] = []

        param_tokens = [self.current_token]
        self.eat(TokenType.ID)
        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            param_tokens.append(self.current_token)
            self.eat(TokenType.ID)

        self.eat(TokenType.COLON)
        type_node = self.type_spec()

        for param_token in param_tokens:
            param_node = Param(Var(param_token), type_node)
            param_nodes.append(param_node)

        return param_nodes

    def formal_parameter_list(self) -> list[Param]:
        """formal_parameter_list : formal_parameters
        | formal_parameters SEMI formal_parameter_list
        """
        # procedure Foo();
        if not self.current_token.type == TokenType.ID:
            return []

        param_nodes = self.formal_parameters()

        while self.current_token.type == TokenType.SEMI:
            self.eat(TokenType.SEMI)
            param_nodes.extend(self.formal_parameters())

        return param_nodes

    def variable_declaration(self) -> list[VarDecl]:
        """variable_declaration : ID (COMMA ID)* COLON type_spec"""
        var_nodes = [Var(self.current_token)]  # first ID
        self.eat(TokenType.ID)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            var_nodes.append(Var(self.current_token))
            self.eat(TokenType.ID)

        self.eat(TokenType.COLON)

        type_node = self.type_spec()
        var_declarations = [VarDecl(var_node, type_node) for var_node in var_nodes]
        return var_declarations

    def procedure_declaration(self) -> ProcedureDecl:
        """procedure_declaration :
        PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? (FORWARD;?) (SEMI block SEMI)?
        """
        self.eat(TokenType.PROCEDURE)
        proc_name = self.current_token.value
        self.eat(TokenType.ID)
        formal_params = []

        if self.current_token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            formal_params = self.formal_parameter_list()
            self.eat(TokenType.RPAREN)
        self.eat(TokenType.SEMI)

        is_forward = False
        if self.current_token.type == TokenType.FORWARD:
            self.eat(TokenType.FORWARD)
            self.eat(TokenType.SEMI)
            is_forward = True

        if not is_forward:
            block_node = self.block()
            proc_decl = ProcedureDecl(proc_name, formal_params, block_node)
            proc_decl.is_forward = is_forward
            self.eat(TokenType.SEMI)
        else:
            if self.current_token.type == TokenType.BEGIN:
                raise ParserError(
                    error_code=ErrorCode.PARSER_UNEXPECTED_TOKEN,
                    token=self.current_token,
                    message=f"{ErrorCode.PARSER_UNEXPECTED_TOKEN.value} -> {self.current_token}",
                )
            proc_decl = ProcedureDecl(proc_name, formal_params, NoOp())
            proc_decl.is_forward = is_forward
        return proc_decl

    def function_declaration(self) -> FunctionDecl:
        """function_declaration :
        FUNCTION ID LPAREN (formal_parameter_list)? RPAREN COLON type_spec SEMI block SEMI
        """
        self.eat(TokenType.FUNCTION)
        func_name = self.current_token.value
        self.eat(TokenType.ID)

        formal_params = []
        self.eat(TokenType.LPAREN)
        formal_params = self.formal_parameter_list()
        self.eat(TokenType.RPAREN)
        self.eat(TokenType.COLON)

        return_type = self.type_spec()

        self.eat(TokenType.SEMI)
        block_node = self.block()
        func_decl = FunctionDecl(func_name, formal_params, return_type, block_node)
        self.eat(TokenType.SEMI)
        return func_decl

    def type_spec(self) -> Type:
        """
        type_spec : primitive_type_spec | string_type_spec | array_type_spec | record_type_spec | ID
        """
        if self.current_token.type in (
            TokenType.INTEGER,
            TokenType.REAL,
            TokenType.BOOLEAN,
            TokenType.CHAR,
        ):
            return self.primitive_type_spec()
        elif self.current_token.type == TokenType.STRING:
            return self.string_type_spec()
        elif self.current_token.type == TokenType.ARRAY:
            return self.array_type_spec()
        elif self.current_token.type == TokenType.RECORD:
            return self.record_type_spec()
        elif self.current_token.type == TokenType.ID:
            # 枚举类型或其他自定义类型
            token = self.current_token
            self.eat(TokenType.ID)
            return Type(token)
        else:
            raise SemanticError(
                error_code=ErrorCode.SEMANTIC_UNKNOWN_TYPE,
                token=self.current_token,
                message=f"{ErrorCode.SEMANTIC_UNKNOWN_TYPE.value} -> {self.current_token}",
            )

    def primitive_type_spec(self) -> Type:
        """
        primitive_type_spec : INTEGER | REAL | BOOLEAN | CHAR
        """
        token = self.current_token
        if self.current_token.type == TokenType.INTEGER:
            self.eat(TokenType.INTEGER)
        elif self.current_token.type == TokenType.REAL:
            self.eat(TokenType.REAL)
        elif self.current_token.type == TokenType.BOOLEAN:
            self.eat(TokenType.BOOLEAN)
        elif self.current_token.type == TokenType.CHAR:
            self.eat(TokenType.CHAR)
        node = PrimitiveType(token)
        return node

    def enum_type(self) -> Type:
        """
        enum_type : LPAREN identifier_list RPAREN
        """
        self.eat(TokenType.LPAREN)
        identifiers = self.identifier_list()
        self.eat(TokenType.RPAREN)
        return EnumType(identifiers)

    def identifier_list(self) -> list[str]:
        """
        identifier_list : ID (COMMA ID)*
        """
        identifiers = [self.current_token.value]
        self.eat(TokenType.ID)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            identifiers.append(self.current_token.value)
            self.eat(TokenType.ID)

        return identifiers

    def string_type_spec(self) -> StringType:
        """
        string_type_spec: STRING ( LBRACKET INTEGER_CONST RBRACKET )?
        """
        token = self.current_token
        if self.current_token.type == TokenType.STRING:
            self.eat(TokenType.STRING)
            if self.current_token.type == TokenType.LBRACKET:
                self.eat(TokenType.LBRACKET)
                limit = self.factor()
                self.eat(TokenType.RBRACKET)
                return StringType(token=token, limit=limit)
            else:
                return StringType(token=token)
        else:
            raise ParserError(
                error_code=ErrorCode.PARSER_UNEXPECTED_TOKEN,
                token=self.current_token,
                message=f"{ErrorCode.PARSER_UNEXPECTED_TOKEN.value} -> {self.current_token}",
            )

    def array_type_spec(self) -> ArrayType:
        """array_type_spec : ARRAY (LBRACKET INTEGER_CONST RANGE INTEGER_CONST RBRACKET)? of type_spec"""
        token = self.current_token
        self.eat(TokenType.ARRAY)
        lower: Expression = self.newZeroNum(
            lineno=self.current_token.lineno, column=self.current_token.column
        )
        upper: Expression = self.newZeroNum(
            lineno=self.current_token.lineno, column=self.current_token.column
        )
        dynamic: bool = True
        if self.current_token.type == TokenType.LBRACKET:
            self.eat(TokenType.LBRACKET)
            lower = self.factor()
            self.eat(TokenType.RANGE)
            upper = self.factor()
            self.eat(TokenType.RBRACKET)
            dynamic = False
        self.eat(TokenType.OF)
        element_type = self.type_spec()
        node = ArrayType(
            token=token,
            element_type=element_type,
            lower=lower,
            upper=upper,
            dynamic=dynamic,
        )
        return node

    def record_type_spec(self) -> RecordType:
        """解析记录类型定义，包括常规字段和可选的变体部分"""
        self.eat(TokenType.RECORD)
        fields = []

        # 解析固定部分字段
        while (
            self.current_token.type != TokenType.CASE
            and self.current_token.type != TokenType.END
        ):
            # 解析字段声明
            var_nodes = [Var(self.current_token)]
            self.eat(TokenType.ID)

            while self.current_token.type == TokenType.COMMA:
                self.eat(TokenType.COMMA)
                var_nodes.append(Var(self.current_token))
                self.eat(TokenType.ID)

            self.eat(TokenType.COLON)
            type_node = self.type_spec()

            # 为每个变量创建字段节点
            for var_node in var_nodes:
                fields.append(RecordField(var_node, type_node))

            if self.current_token.type == TokenType.SEMI:
                self.eat(TokenType.SEMI)

        # 解析变体部分（可选）
        variant_part = None
        if self.current_token.type == TokenType.CASE:
            variant_part = self._parse_variant_part()

        self.eat(TokenType.END)
        return RecordType(fields, variant_part)

    def _parse_variant_part(self) -> VariantPart:
        """解析记录的变体部分，使用 'case kind of' 语法格式"""
        self.eat(TokenType.CASE)

        # 解析标签字段（必须是枚举类型）
        tag_field = Type(token=self.current_token)
        self.eat(TokenType.ID)

        self.eat(TokenType.OF)

        # 解析变体情况列表
        variant_cases = []
        while self.current_token.type != TokenType.END:
            # 解析标签值列表
            tag_values = [self.current_token.value]
            self.eat(TokenType.ID)

            while self.current_token.type == TokenType.COMMA:
                self.eat(TokenType.COMMA)
                tag_values.append(self.current_token.value)
                self.eat(TokenType.ID)

            self.eat(TokenType.COLON)
            self.eat(TokenType.LPAREN)

            # 解析变体字段列表
            variant_fields = []
            while self.current_token.type != TokenType.RPAREN:
                # 解析字段声明（与 record_type_spec 相同）
                var_nodes = [Var(self.current_token)]
                self.eat(TokenType.ID)

                while self.current_token.type == TokenType.COMMA:
                    self.eat(TokenType.COMMA)
                    var_nodes.append(Var(self.current_token))
                    self.eat(TokenType.ID)

                self.eat(TokenType.COLON)
                type_node = self.type_spec()

                for var_node in var_nodes:
                    variant_fields.append(RecordField(var_node, type_node))

                if self.current_token.type == TokenType.SEMI:
                    self.eat(TokenType.SEMI)

            self.eat(TokenType.RPAREN)

            # 创建变体情况
            variant_case = VariantCase(tag_values, variant_fields)
            variant_cases.append(variant_case)

            if self.current_token.type == TokenType.SEMI:
                self.eat(TokenType.SEMI)

        return VariantPart(tag_field, variant_cases)

    def compound_statement(self) -> Compound:
        """
        compound_statement: BEGIN statement_list END
        """
        self.eat(TokenType.BEGIN)
        nodes = self.statement_list()
        self.eat(TokenType.END)

        root = Compound()
        for node in nodes:
            root.children.append(node)

        return root

    def statement_list(self) -> list[Statement]:
        """
        statement_list : statement
                       | statement SEMI statement_list
        """
        node = self.statement()

        results = [node]

        while self.current_token.type == TokenType.SEMI:
            self.eat(TokenType.SEMI)
            results.append(self.statement())

        return results

    # it should deal with func_call_expr
    def statement(self) -> Statement:
        """
        statement : compound_statement
                  | proccall_statement
                  | assignment_statement
                  | if_statement
                  | case_statement
                  | empty
        """
        node: Statement
        if self.current_token.type == TokenType.BEGIN:
            node = self.compound_statement()
        elif self.current_token.type == TokenType.IF:
            node = self.if_statement()
        elif self.current_token.type == TokenType.CASE:
            node = self.case_statement()
        elif self.current_token.type == TokenType.WHILE:
            node = self.while_statement()
        elif self.current_token.type == TokenType.FOR:
            node = self.for_statement()
        elif (
            self.current_token.type == TokenType.ID
            and self.lexer.peek_next_token().type == TokenType.LPAREN
        ):
            node = self.proccall_statement()
        elif self.current_token.type == TokenType.ID:
            node = self.assignment_statement()
        else:
            node = self.empty()
        return node

    def if_statement(self) -> IfStatement:
        """
        if_statement: IF logic_expr THEN (statement | compound_statement)
                    (ELSE IF logic_expr THEN (statement | compound_statement))*
                    (ELSE (statement | compound_statement))? SEMI
        """
        self.eat(TokenType.IF)
        condition = self.logic_expr()
        self.eat(TokenType.THEN)
        then_branch: AST
        if self.current_token.type == TokenType.BEGIN:
            then_branch = self.compound_statement()
        else:
            then_branch = self.statement()

        else_if_branches: list[IfStatement] = []
        else_branch: AST | None = None
        while self.current_token.type == TokenType.ELSE:
            self.eat(TokenType.ELSE)
            sub_then_branch: AST
            if self.current_token.type == TokenType.IF:
                self.eat(TokenType.IF)
                sub_condition = self.logic_expr()
                self.eat(TokenType.THEN)
                if self.current_token.type == TokenType.BEGIN:
                    sub_then_branch = self.compound_statement()
                else:
                    sub_then_branch = self.statement()
                sub_node = IfStatement(sub_condition, sub_then_branch, [], None)
                else_if_branches.append(sub_node)
            else:
                if self.current_token.type == TokenType.BEGIN:
                    else_branch = self.compound_statement()
                else:
                    else_branch = self.statement()
                break

        node = IfStatement(
            condition,
            then_branch,
            else_if_branches,
            else_branch,
        )
        return node

    def while_statement(self) -> WhileStatement:
        """while_statement:  WHILE logic_expr DO compound_statement SEMI"""
        self.eat(TokenType.WHILE)
        condition = self.logic_expr()
        self.eat(TokenType.DO)
        block = self.compound_statement()
        self.eat(TokenType.SEMI)
        node = WhileStatement(condition, block)
        return node

    def for_statement(self) -> ForStatement:
        """for_statement:  FOR assignment_statement TO summation_expr DO (statement | compound_statement) SEMI"""
        self.eat(TokenType.FOR)
        initialization = self.assignment_statement()
        self.eat(TokenType.TO)
        bound = self.summation_expr()
        self.eat(TokenType.DO)
        block: AST
        if self.current_token.type == TokenType.BEGIN:
            block = self.compound_statement()
        else:
            block = self.statement()
        node = ForStatement(initialization, bound, block)
        return node

    def case_statement(self) -> CaseStatement:
        """case_statement : CASE variable OF case_list (ELSE (statement | compound_statement) SEMI)? END SEMI"""
        self.eat(TokenType.CASE)
        case_expr = self.variable()
        self.eat(TokenType.OF)
        case_items = self.case_list()
        else_stmt: Statement | None = None
        if self.current_token.type == TokenType.ELSE:
            self.eat(TokenType.ELSE)
            if self.current_token.type == TokenType.BEGIN:
                else_stmt = self.compound_statement()
            else:
                else_stmt = self.statement()
            self.eat(TokenType.SEMI)
        self.eat(TokenType.END)
        node = CaseStatement(case_expr, case_items, else_stmt)
        return node

    def case_list(self) -> list[CaseItem]:
        """case_list : case_item SEMI (case_item SEMI)*"""
        case_items = [self.case_item()]
        self.eat(TokenType.SEMI)

        while self.current_token.type not in (TokenType.ELSE, TokenType.END):
            # Parse next case item if we have case labels
            if self.current_token.type in (
                TokenType.INTEGER_CONST,
                TokenType.CHAR_CONST,
                TokenType.STRING_CONST,
                TokenType.TRUE,
                TokenType.FALSE,
                TokenType.ID,  # 添加ID以支持枚举值
            ):
                case_items.append(self.case_item())
                self.eat(TokenType.SEMI)
            else:
                break

        return case_items

    def case_item(self) -> CaseItem:
        """case_item : case_label_list COLON (statement | compound_statement)"""
        labels = self.case_label_list()
        self.eat(TokenType.COLON)
        statement: Statement | None = None
        if self.current_token.type == TokenType.BEGIN:
            statement = self.compound_statement()
        else:
            statement = self.statement()
        node = CaseItem(labels, statement)
        return node

    def case_label_list(self) -> list[CaseLabel]:
        """case_label_list : case_label (COMMA case_label)*"""
        labels = [self.case_label()]
        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            labels.append(self.case_label())
        return labels

    def case_label(self) -> CaseLabel:
        """case_label : INTEGER | CHAR | boolean_literal | ID"""
        token = self.current_token
        if token.type == TokenType.INTEGER_CONST:
            self.eat(TokenType.INTEGER_CONST)
            return CaseLabel(token.value)
        elif token.type == TokenType.CHAR_CONST:
            self.eat(TokenType.CHAR_CONST)
            return CaseLabel(token.value)
        elif token.type == TokenType.STRING_CONST and len(token.value) == 1:
            # 处理单字符字符串常量，如'A'，在case标签中作为字符处理
            self.eat(TokenType.STRING_CONST)
            return CaseLabel(token.value)
        elif token.type in (TokenType.TRUE, TokenType.FALSE):
            if token.type == TokenType.TRUE:
                self.eat(TokenType.TRUE)
                return CaseLabel(True)
            else:
                self.eat(TokenType.FALSE)
                return CaseLabel(False)
        elif token.type == TokenType.ID:
            # 枚举值
            self.eat(TokenType.ID)
            return CaseLabel(token.value)
        else:
            self.error(
                error_code=ErrorCode.PARSER_UNEXPECTED_TOKEN,
                token=token,
            )
            raise

    def proccall_statement(self) -> ProcedureCall:
        """proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN"""
        token = self.current_token

        proc_name = self.current_token.value
        self.eat(TokenType.ID)
        self.eat(TokenType.LPAREN)
        actual_params: list[Expression] = []
        if self.current_token.type != TokenType.RPAREN:
            expr = self.expr()
            actual_params.append(expr)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            expr = self.expr()
            actual_params.append(expr)

        self.eat(TokenType.RPAREN)

        node = ProcedureCall(
            proc_name=proc_name,
            actual_params=actual_params,
            token=token,
        )
        return node

    def func_call_expr(self) -> FunctionCall:
        """func_call_expr : ID LPAREN (expr (COMMA expr)*)? RPAREN"""
        token = self.current_token

        fun_name = self.current_token.value
        self.eat(TokenType.ID)
        self.eat(TokenType.LPAREN)
        actual_params: list[Expression] = []

        actual_params.append(
            Var(Token(TokenType.ID, fun_name, token.lineno, token.column))
        )

        if self.current_token.type != TokenType.RPAREN:
            expr = self.summation_expr()
            actual_params.append(expr)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            expr = self.summation_expr()
            actual_params.append(expr)

        self.eat(TokenType.RPAREN)

        node = FunctionCall(
            func_name=fun_name,
            actual_params=actual_params,
            token=token,
        )
        return node

    def assignment_statement(self) -> Assign:
        """
        assignment_statement : variable ASSIGN expr
        """
        left = self.variable()
        token = self.current_token
        self.eat(TokenType.ASSIGN)
        right = self.expr()
        node = Assign(left, token, right)
        return node

    def variable(self) -> Expression:
        """
        variable : ID variable_suffix*
        variable_suffix : LBRACKET expr RBRACKET | DOT ID
        """
        # Parse the base identifier
        base = Var(self.current_token)
        self.eat(TokenType.ID)

        # Parse variable suffixes
        suffixes: list[AccessSuffix] = []

        while self.current_token.type in (TokenType.LBRACKET, TokenType.DOT):
            if self.current_token.type == TokenType.LBRACKET:
                # Index access: [expr]
                self.eat(TokenType.LBRACKET)
                index = self.expr()
                self.eat(TokenType.RBRACKET)
                suffixes.append(IndexSuffix(index))
            elif self.current_token.type == TokenType.DOT:
                # Member access: .field
                self.eat(TokenType.DOT)
                member_token = self.current_token
                self.eat(TokenType.ID)
                suffixes.append(MemberSuffix(member_token))

        # Return AccessExpression if there are suffixes, otherwise return base Var
        if suffixes:
            return AccessExpression(base, suffixes)
        else:
            return base

    def empty(self) -> NoOp:
        """An empty production"""
        return NoOp()

    def expr(self) -> Expression:
        """
        expr : logic_expr
        """
        return self.logic_expr()

    def logic_expr(self) -> Expression:
        """logic_expr : comparison_expr ((and | or ) comparison_expr)*"""
        node = self.comparison_expr()

        while self.current_token.type in (TokenType.AND, TokenType.OR):
            token = self.current_token
            if token.type == TokenType.AND:
                self.eat(TokenType.AND)
            elif token.type == TokenType.OR:
                self.eat(TokenType.OR)

            node = BinOp(left=node, op=token, right=self.comparison_expr())

        return node

    def comparison_expr(self) -> Expression:
        """comparison_expr : summation_expr ( (EQ | NE | GT | GE | LT | LE) summation_expr )*"""
        node = self.summation_expr()
        while self.current_token.type in (
            TokenType.EQ,
            TokenType.NE,
            TokenType.GT,
            TokenType.GE,
            TokenType.LT,
            TokenType.LE,
        ):
            token = self.current_token
            if token.type == TokenType.EQ:
                self.eat(TokenType.EQ)
            elif token.type == TokenType.NE:
                self.eat(TokenType.NE)
            elif token.type == TokenType.GT:
                self.eat(TokenType.GT)
            elif token.type == TokenType.GE:
                self.eat(TokenType.GE)
            elif token.type == TokenType.LT:
                self.eat(TokenType.LT)
            elif token.type == TokenType.LE:
                self.eat(TokenType.LE)

            node = BinOp(left=node, op=token, right=self.summation_expr())

        return node

    def summation_expr(self) -> Expression:
        """
        summation_expr : multiplication_expr ((PLUS | MINUS) multiplication_expr)*
        """
        node = self.multiplication_expr()

        while self.current_token.type in (TokenType.PLUS, TokenType.MINUS):
            token = self.current_token
            if token.type == TokenType.PLUS:
                self.eat(TokenType.PLUS)
            elif token.type == TokenType.MINUS:
                self.eat(TokenType.MINUS)

            node = BinOp(left=node, op=token, right=self.multiplication_expr())

        return node

    def multiplication_expr(self) -> Expression:
        """multiplication_expr : factor ((MUL | INTEGER_DIV | FLOAT_DIV | MOD) factor)*"""
        node = self.factor()

        while self.current_token.type in (
            TokenType.MUL,
            TokenType.INTEGER_DIV,
            TokenType.FLOAT_DIV,
            TokenType.MOD,
        ):
            token = self.current_token
            if token.type == TokenType.MUL:
                self.eat(TokenType.MUL)
            elif token.type == TokenType.INTEGER_DIV:
                self.eat(TokenType.INTEGER_DIV)
            elif token.type == TokenType.FLOAT_DIV:
                self.eat(TokenType.FLOAT_DIV)
            elif token.type == TokenType.MOD:
                self.eat(TokenType.MOD)

            node = BinOp(left=node, op=token, right=self.factor())

        return node

    def factor(self) -> Expression:
        """
        factor : not comparison_expr
               | PLUS factor
               | MINUS factor
               | INTEGER_CONST
               | STRING_CONST
               | REAL_CONST
               | TRUE_CONST
               | FALSE_CONST
               | LPAREN expr RPAREN
               | func_call_expr
               | variable
        """
        token = self.current_token
        node: Expression
        # logic OP
        if token.type == TokenType.NOT:
            self.eat(TokenType.NOT)
            node = UnaryOp(token, self.comparison_expr())
            return node

        # arithmetic OP
        if token.type == TokenType.PLUS:
            self.eat(TokenType.PLUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == TokenType.MINUS:
            self.eat(TokenType.MINUS)
            node = UnaryOp(token, self.factor())
            return node

        # value
        if token.type == TokenType.INTEGER_CONST:
            self.eat(TokenType.INTEGER_CONST)
            return Num(token)
        elif token.type == TokenType.REAL_CONST:
            self.eat(TokenType.REAL_CONST)
            return Num(token)
        elif token.type == TokenType.TRUE:
            self.eat(TokenType.TRUE)
            return Bool(token)
        elif token.type == TokenType.FALSE:
            self.eat(TokenType.FALSE)
            return Bool(token)

        # parent take precedence
        if token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            node = self.summation_expr()
            self.eat(TokenType.RPAREN)
            return node

        # parse string expr
        if self.current_token.type == TokenType.STRING_CONST:
            token = self.current_token
            self.eat(TokenType.STRING_CONST)
            return String(token=token)

        # parse char expr
        if self.current_token.type == TokenType.CHAR_CONST:
            token = self.current_token
            self.eat(TokenType.CHAR_CONST)
            return Char(token=token)

        # call
        if (
            token.type == TokenType.ID
            and self.peek_next_token().type == TokenType.LPAREN
        ):
            node = self.func_call_expr()
            return node
        else:
            node = self.variable()
            return node

    def parse(self):
        """
        program : PROGRAM variable SEMI block DOT

        block : declarations compound_statement

        declarations : type_declarations? (VAR (variable_declaration SEMI)+)? procedure_declaration* function_declaration*

        type_declarations : TYPE (type_declaration SEMI)+

        type_declaration : ID EQ (enum_type | type_spec)

        variable_declaration : ID (COMMA ID)* COLON type_spec

        procedure_declaration :
             PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI

        function_declaration :
             FUNCTION ID LPAREN (formal_parameter_list)? RPAREN COLON type_spec SEMI block SEMI

        formal_params_list : formal_parameters
                           | formal_parameters SEMI formal_parameter_list

        formal_parameters : ID (COMMA ID)* COLON type_spec

        type_spec : primitive_type_spec | string_type_spec | array_type_spec

        primitive_type_spec : INTEGER | REAL | BOOLEAN

        string_type_spec: STRING ( LBRACKET INTEGER_CONST RBRACKET )?

        array_type_spec : ARRAY ( LBRACKET INTEGER_CONST RANGE INTEGER_CONST RBRACKET )? of type_spec

        compound_statement : BEGIN statement_list END

        statement_list : statement
                       | statement SEMI statement_list

        statement : compound_statement
                  | proccall_statement
                  | assignment_statement
                  | if_statement
                  | case_statement
                  | for_statement
                  | while_statement
                  | empty

        if_statement: IF logic_expr THEN (statement | compound_statement)
                    (ELSE IF logic_expr THEN (statement | compound_statement))*
                    (ELSE (statement | compound_statement))? SEMI

        while_statement:  WHILE logic_expr DO compound_statement SEMI

        for_statement:  FOR assignment_statement TO summation_expr DO (statement | compound_statement) SEMI

        case_statement : CASE variable OF case_list (ELSE (statement | compound_statement) SEMI)? END SEMI

        case_list : case_item SEMI (case_item SEMI)*

        case_item : case_label_list COLON (statement | compound_statement)

        case_label_list : case_label (COMMA case_label)*

        case_label : INTEGER_CONST | CHAR_CONST | TRUE | FALSE

        proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN

        func_call_expr : ID LPAREN (expr (COMMA expr)*)? RPAREN

        assignment_statement : variable ASSIGN expr

        empty :

        expr : string_expr | logic_expr

        logic_expr : comparison_expr ((and | or ) comparison_expr)*

        comparison_expr : summation_expr ( (EQ | NE | GT | GE | LT | LE) summation_expr )*

        summation_expr : multiplication_expr ((PLUS | MINUS) multiplication_expr)*

        multiplication_expr : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*

        factor : not comparison_expr
               | PLUS factor
               | MINUS factor
               | INTEGER_CONST
               | STRING_CONST
               | CHAR_CONST
               | REAL_CONST
               | TRUE_CONST
               | FALSE_CONST
               | LPAREN expr RPAREN
               | func_call_expr
               | variable

        variable : ID variable_suffix*

        variable_suffix : LBRACKET expr RBRACKET | DOT ID
        """
        node = self.program()
        if self.current_token.type != TokenType.EOF:
            self.error(
                error_code=ErrorCode.PARSER_UNEXPECTED_TOKEN,
                token=self.current_token,
            )

        return node
