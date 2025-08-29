###############################################################################
#                                                                             #
#  PARSER                                                                     #
#                                                                             #
###############################################################################

from typing import cast

from src.error import *
from src.lexer import Lexer
from src.spi_ast import *
from src.spi_token import TokenType
from src.util import SpiUtil


class Parser:
    def __init__(self, lexer: Lexer) -> None:
        self.lexer = lexer
        # set current token to the first token taken from the input
        self.current_token = self.get_next_token()
        self.current_type_id = ""
        self.classes: list[str] = []
        self.enums: list[str] = []
        self.records: list[str] = []

    def newZeroNum(self, lineno: int = -1, column: int = -1) -> Num:
        return Num(
            token=Token(TokenType.INTEGER_CONST, value=0, lineno=lineno, column=column)
        )

    def newVoidType(self, lineno: int = -1, column: int = -1) -> VoidType:
        return VoidType(
            token=Token(
                type=TokenType.VOID,
                value=TokenType.VOID.value,
                lineno=lineno,
                column=column,
            )
        )

    def get_next_token(self):
        return self.lexer.get_next_token()

    def peek_next_token(self):
        return self.lexer.peek_next_token()

    def peek_next_two_token(self) -> tuple[Token, Token]:
        tokens = self.lexer.peek_next_token_list(2)
        return (tokens[0], tokens[1])

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
        """program : PROGRAM variable SEMI (uses_clause)? block DOT"""
        self.eat(TokenType.PROGRAM)
        var_node = self.variable()
        prog_name = var_node.value
        self.eat(TokenType.SEMI)
        
        # Parse optional uses clause
        uses_list = []
        if self.current_token.type == TokenType.USES:
            uses_list = self.uses_clause()
        
        block_node = self.block()
        program_node = Program(prog_name, block_node, uses_list)
        self.eat(TokenType.DOT)
        return program_node

    def uses_clause(self) -> list[str]:
        """uses_clause : USES ID (COMMA ID)* SEMI"""
        self.eat(TokenType.USES)
        
        # Parse first module name
        if self.current_token.type != TokenType.ID:
            self.error(
                error_code=ErrorCode.UNEXPECTED_TOKEN,
                token=self.current_token,
            )
        
        module_names = [self.current_token.value]
        self.eat(TokenType.ID)
        
        # Parse additional module names separated by commas
        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            if self.current_token.type != TokenType.ID:
                self.error(
                    error_code=ErrorCode.UNEXPECTED_TOKEN,
                    token=self.current_token,
                )
            module_names.append(self.current_token.value)
            self.eat(TokenType.ID)
        
        self.eat(TokenType.SEMI)
        return module_names

    def block(self) -> Block:
        """block : declarations compound_statement"""
        declaration_nodes = self.declarations()
        compound_statement_node = self.compound_statement()
        node = Block(declaration_nodes, compound_statement_node)
        return node

    def declarations(self) -> list[Decl]:
        """
        declarations :
            (const_declaration)?
            (type_declaration)?
            (VAR (variable_declaration SEMI)+)?
            procedure_declaration*
            function_declaration*
            (VAR (variable_declaration SEMI)+)?
        """
        """
        pascal does not emphasize the order of var/proc/func , the var before or after proc/func just for simplicity
        """
        hasVar: bool = False
        declarations: list[Decl] = []

        if self.current_token.type == TokenType.CONST:
            const_list = self.const_declaration()
            declarations.extend(const_list)

        if self.current_token.type == TokenType.TYPE:
            type_decl_list = self.type_declaration()
            declarations.extend(type_decl_list)

        if self.current_token.type == TokenType.VAR:
            hasVar = True
            self.eat(TokenType.VAR)
            while self.current_token.type == TokenType.ID:
                var_decl = self.variable_declaration()
                declarations.extend(var_decl)
                self.eat(TokenType.SEMI)
        while self.current_token.type in [
            TokenType.CONSTRUCTOR,
            TokenType.PROCEDURE,
            TokenType.FUNCTION,
        ]:
            first, second = self.peek_next_two_token()
            if first.type == TokenType.ID and second.type == TokenType.DOT:
                method_decl = self.method_declaration()
                declarations.append(method_decl)
            elif self.current_token.type == TokenType.CONSTRUCTOR:
                constructor_decl = self.constructor_declaration()
                declarations.append(constructor_decl)
            elif self.current_token.type == TokenType.PROCEDURE:
                proc_decl = self.procedure_declaration()
                declarations.append(proc_decl)
            elif self.current_token.type == TokenType.FUNCTION:
                func_decl = self.function_declaration()
                declarations.append(func_decl)

        if hasVar and self.current_token.type == TokenType.VAR:
            raise VarDuplicateInScopeError()
        if self.current_token.type == TokenType.VAR:
            hasVar = True
            self.eat(TokenType.VAR)
            while self.current_token.type == TokenType.ID:
                var_decl = self.variable_declaration()
                declarations.extend(var_decl)
                self.eat(TokenType.SEMI)

        return declarations

    def const_declaration(self) -> list[ConstAssign]:
        """
        const_declaration:
            CONST (const_assignment_statement)+
        """
        decl_list: list[ConstAssign] = []
        self.eat(TokenType.CONST)
        decl_list.append(self.const_assign_statement())
        while self.current_token.type == TokenType.ID:
            decl_list.append(self.const_assign_statement())
        return decl_list

    def const_assign_statement(self) -> ConstAssign:
        """
        const_assignment_statement:
            (ID EQ expr SEMI) | (ID COLON array_type_spec EQ LPAREN expr ( COMMA expr )* RPAREN SEMI)
        """
        if self.current_token.type == TokenType.ID:
            var = self.variable()
            if self.current_token.type == TokenType.EQ:
                token: Token = self.current_token
                self.eat(TokenType.EQ)
                right = self.expr()
                self.eat(TokenType.SEMI)
                return ConstAssign(
                    left=var, op=token, right=right, const_type=ConstType.NON_ARRAY
                )
            elif self.current_token.type == TokenType.COLON:
                token = self.current_token
                self.eat(TokenType.COLON)
                array_type = self.array_type_spec()
                self.eat(TokenType.EQ)
                self.eat(TokenType.LPAREN)
                array: list[AST] = []
                element = self.expr()
                array.append(element)
                while self.current_token.type == TokenType.COMMA:
                    self.eat(TokenType.COMMA)
                    element = self.expr()
                    array.append(element)
                self.eat(TokenType.RPAREN)
                self.eat(TokenType.SEMI)
                return ConstAssign(
                    left=var,
                    op=token,
                    right=Compound.of(array),
                    const_type=ConstType.ARRAY,
                    type=array_type,
                )
            else:
                raise InvalidConstAssignError()
        else:
            raise InvalidConstAssignError()

    def type_declaration(self) -> list[Decl]:
        """
        type_declaration:
            TYPE ( class_definition | enum_definition | record_definition )*
        """
        decl_list: list[Decl] = []
        self.eat(TokenType.TYPE)
        while self.current_token.type == TokenType.ID:
            # will be used in class_definition()
            token = self.current_token
            self.eat(TokenType.ID)
            self.eat(TokenType.EQ)
            if self.current_token.type == TokenType.CLASS:
                class_name = cast(str, token.value)
                self.current_type_id = SpiUtil.toClassName(class_name)
                self.classes.append(class_name)
                self.eat(TokenType.CLASS)
                class_def = self.class_definition()
                decl_list.append(class_def)
            elif self.current_token.type == TokenType.RECORD:
                record_name = cast(str, token.value)
                self.current_type_id = record_name
                self.records.append(record_name)
                self.eat(TokenType.RECORD)
                record_decl = self.record_definition()
                decl_list.append(record_decl)
            elif self.current_token.type == TokenType.LPAREN:
                enum_name = cast(str, token.value)
                self.current_type_id = enum_name
                self.enums.append(enum_name)
                enum_decl = self.enum_definition()
                decl_list.append(enum_decl)
        return decl_list

    def class_definition(self) -> ClassDecl:
        """
        class_definition:
            ID = CLASS (PRIVATE (field_definition SEMI)+)? (PUBLIC  (method_definition)+)? END SEMI
        """
        self.eat(TokenType.PRIVATE)
        fields: list[FieldDecl] = []
        while self.current_token.type == TokenType.ID:
            var_decl = self.variable_declaration()[0]
            self.eat(token_type=TokenType.SEMI)
            fields.append(
                FieldDecl(var_node=var_decl.var_node, type_node=var_decl.type_node)
            )

        methods: list[MethodDef] = []
        self.eat(TokenType.PUBLIC)
        while self.current_token.type in [
            TokenType.CONSTRUCTOR,
            TokenType.FUNCTION,
            TokenType.PROCEDURE,
        ]:
            method_def = self.method_definition()
            methods.append(method_def)
        self.eat(token_type=TokenType.END)
        self.eat(token_type=TokenType.SEMI)

        # create the default constructor and destructor
        constructor: MethodDecl | None = None
        destructor: MethodDecl | None = None
        has_constructor = False
        has_destructor = False
        for m in methods:
            if m.method_name.upper() == "CREATE":
                has_constructor = True
                continue
            if m.method_name.upper() == "FREE":
                has_destructor = True
                continue
        if not has_constructor:
            compound = Compound()
            for f in fields:
                var_name = f.var_node.value
                left = Var(token=Token(type=TokenType.ID, value=var_name))
                right: AST = NoOp()
                # TODO: only consider the primitive type and string type
                match f.type_node.token.type:
                    case TokenType.BOOLEAN:
                        right = Bool(token=Token(type=TokenType.FALSE, value=False))
                    case TokenType.INTEGER:
                        right = Num(token=Token(type=TokenType.INTEGER_CONST, value=0))
                    case TokenType.REAL:
                        right = Num(token=Token(type=TokenType.REAL_CONST, value=0.0))
                    case TokenType.STRING:
                        right = String(
                            token=Token(type=TokenType.STRING_CONST, value="")
                        )
                compound.children.append(
                    Assign(
                        left=left,
                        op=Token(type=TokenType.ASSIGN, value=TokenType.ASSIGN.value),
                        right=right,
                    )
                )

            constructor = MethodDecl(
                method_full_name=SpiUtil.extraClassName(self.current_type_id)
                + "."
                + "Create",
                params=[],
                return_type=self.newVoidType(),
                method_type=MethodType.CONSTRUCTOR,
                block=Block(declarations=[], compound_statement=compound),
            )
        if not has_destructor:
            compound = Compound()
            for f in fields:
                var_name = f.var_node.value
                left = Var(token=Token(type=TokenType.ID, value=var_name))
                right = NoOp()
                match f.type_node.token.type:
                    case TokenType.BOOLEAN:
                        right = Bool(token=Token(type=TokenType.FALSE, value=False))
                    case TokenType.INTEGER:
                        right = Num(token=Token(type=TokenType.INTEGER_CONST, value=0))
                    case TokenType.REAL:
                        right = Num(token=Token(type=TokenType.REAL_CONST, value=0.0))
                    case TokenType.STRING:
                        right = String(
                            token=Token(type=TokenType.STRING_CONST, value="")
                        )
                compound.children.append(
                    Assign(
                        left=left,
                        op=Token(type=TokenType.ASSIGN, value=TokenType.ASSIGN.value),
                        right=right,
                    )
                )
            destructor = MethodDecl(
                method_full_name=SpiUtil.extraClassName(self.current_type_id)
                + "."
                + "Free",
                params=[],
                return_type=self.newVoidType(),
                method_type=MethodType.DESTRUCTOR,
                block=Block(declarations=[], compound_statement=compound),
            )
        class_decl = ClassDecl(
            class_name=self.current_type_id,
            fields=fields,
            methods=methods,
            constructor=constructor,
            destructor=destructor,
        )
        self.current_type_id = ""  # reset here
        return class_decl

    def record_definition(self) -> RecordDecl:
        """
        record_definition:
            ID = RECORD ( field_definition SEMI )+ END SEMI
        """
        fields: list[FieldDecl] = []
        if self.current_token.type != TokenType.ID:
            raise InvalidRecordDeclError()
        var_decl = self.variable_declaration()[0]
        self.eat(token_type=TokenType.SEMI)
        fields.append(
            FieldDecl(var_node=var_decl.var_node, type_node=var_decl.type_node)
        )
        while self.current_token.type == TokenType.ID:
            var_decl = self.variable_declaration()[0]
            self.eat(token_type=TokenType.SEMI)
            fields.append(
                FieldDecl(var_node=var_decl.var_node, type_node=var_decl.type_node)
            )
        self.eat(token_type=TokenType.END)
        self.eat(token_type=TokenType.SEMI)
        record_decl = RecordDecl(record_name=self.current_type_id, fields=fields)
        self.current_type_id = ""
        return record_decl

    def enum_definition(self) -> EnumDecl:
        """
        enum_definition:
            ID = LPAREN ID ( COMMA ID )* RPAREN SEMI
        """
        self.eat(TokenType.LPAREN)
        if self.current_token.type == TokenType.RPAREN:
            raise InvalidEnumDeclError()
        entries: dict[int, str] = {}
        index = 0
        while self.current_token.type == TokenType.ID:
            token = self.current_token
            self.eat(TokenType.ID)
            entries[index] = token.value
            index += 1
            if self.current_token.type != TokenType.COMMA:
                break
            else:
                self.eat(TokenType.COMMA)
        self.eat(TokenType.RPAREN)
        self.eat(TokenType.SEMI)
        node = EnumDecl(enum_name=self.current_type_id, entries=entries)
        self.current_type_id = ""
        return node

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

    def field_definition(self) -> list[VarDecl]:
        """
        field_definition: variable_declaration
        """
        raise ParserError()

    def method_definition(self) -> MethodDef:
        """
        method_definition:
            ( CONSTRUCTOR | PROCEDURE | FUNCTION ) ID DOT ID (LPAREN (formal_parameter_list)? RPAREN)? ( COLON type_spec )? SEMI
        """
        while self.current_token.type in [
            TokenType.CONSTRUCTOR,
            TokenType.PROCEDURE,
            TokenType.FUNCTION,
        ]:
            if self.current_token.type == TokenType.CONSTRUCTOR:
                self.eat(TokenType.CONSTRUCTOR)
                method_type = MethodType.CONSTRUCTOR
            elif self.current_token.type == TokenType.PROCEDURE:
                self.eat(TokenType.PROCEDURE)
                method_type = MethodType.PROCEDURE
            elif self.current_token.type == TokenType.FUNCTION:
                self.eat(TokenType.FUNCTION)
                method_type = MethodType.FUNCTION

        method_name = self.id_expr()

        params = []
        if self.current_token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            params = self.formal_parameter_list()
            self.eat(TokenType.RPAREN)

        return_type: Type = self.newVoidType(
            lineno=self.current_token.lineno, column=self.current_token.column
        )
        if self.current_token.type == TokenType.COLON:
            self.eat(TokenType.COLON)
            return_type = self.type_spec()

        self.eat(TokenType.SEMI)

        node = MethodDef(
            method_name=method_name,
            params=params,
            return_type=return_type,
            method_type=method_type,
        )
        return node

    def method_declaration(self) -> MethodDecl:
        """
        method_declaration :
            method_definition block SEMI
        """
        method_def = self.method_definition()
        block_node = self.block()
        method_decl = MethodDecl(
            method_full_name=method_def.method_name,
            params=method_def.params,
            return_type=method_def.return_type,
            method_type=method_def.method_type,
            block=block_node,
        )
        self.eat(TokenType.SEMI)
        return method_decl

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
        if isinstance(type_node, ClassType):
            for i in range(0, len(var_nodes)):
                self.classes.append(var_nodes[i].value)
        elif isinstance(type_node, RecordType):
            for i in range(0, len(var_nodes)):
                self.records.append(var_nodes[i].value)
        var_declarations = [VarDecl(var_node, type_node) for var_node in var_nodes]
        return var_declarations

    def constructor_declaration(self) -> ConstructorDecl:
        """
        constructor_declaration :
            constructor_definition block SEMI
        """
        constructor_def = self.constructor_definition()
        block_node = self.block()

        proc_decl = ConstructorDecl(
            constructor_name=constructor_def.constructor_name,
            formal_params=constructor_def.formal_params,
            block_node=block_node,
        )
        self.eat(TokenType.SEMI)
        return proc_decl

    def constructor_definition(self) -> ConstructorDef:
        """
        constructor_definition:
            CONSTRUCTOR id_expr (LPAREN formal_parameter_list RPAREN)? SEMI
        """
        self.eat(TokenType.CONSTRUCTOR)
        constructor_name = self.id_expr()

        formal_params = []
        if self.current_token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            formal_params = self.formal_parameter_list()
            self.eat(TokenType.RPAREN)
        self.eat(TokenType.SEMI)

        node = ConstructorDef(
            constructor_name=constructor_name, formal_params=formal_params
        )
        return node

    def procedure_declaration(self) -> ProcedureDecl:
        """
        procedure_declaration :
            procedure_definition block SEMI
        """
        proc_def = self.procedure_definition()
        block_node = self.block()

        proc_decl = ProcedureDecl(
            proc_name=proc_def.proc_name,
            formal_params=proc_def.formal_params,
            block_node=block_node,
        )
        self.eat(TokenType.SEMI)
        return proc_decl

    def procedure_definition(self) -> ProcedureDef:
        """
        procedure_definition:
            PROCEDURE id_expr (LPAREN formal_parameter_list RPAREN)? SEMI
        """
        self.eat(TokenType.PROCEDURE)
        proc_name = self.id_expr()

        formal_params = []
        if self.current_token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            formal_params = self.formal_parameter_list()
            self.eat(TokenType.RPAREN)
        self.eat(TokenType.SEMI)

        node = ProcedureDef(proc_name=proc_name, formal_params=formal_params)
        return node

    def function_declaration(self) -> FunctionDecl:
        """
        function_declaration :
            function_definition block SEMI
        """
        function_def = self.function_definition()

        block_node = self.block()
        func_decl = FunctionDecl(
            func_name=function_def.func_name,
            formal_params=function_def.formal_params,
            return_type=function_def.return_type,
            block_node=block_node,
        )
        self.eat(TokenType.SEMI)
        return func_decl

    def function_definition(self) -> FunctionDef:
        """
        function_definition:
            FUNCTION id_expr LPAREN (formal_parameter_list)? RPAREN COLON type_spec SEMI
        """
        self.eat(TokenType.FUNCTION)
        func_name = self.id_expr()

        formal_params = []
        self.eat(TokenType.LPAREN)
        formal_params = self.formal_parameter_list()
        self.eat(TokenType.RPAREN)
        self.eat(TokenType.COLON)

        return_type = self.type_spec()

        self.eat(TokenType.SEMI)

        node = FunctionDef(
            func_name=func_name, formal_params=formal_params, return_type=return_type
        )
        return node

    def id_expr(self) -> str:
        name: str = self.current_token.value
        self.eat(TokenType.ID)
        while self.current_token.type == TokenType.DOT:
            name += self.current_token.value
            self.eat(TokenType.DOT)
            name += self.current_token.value
            self.eat(TokenType.ID)
        return name

    def type_spec(self) -> Type:
        """
        type_spec : primitive_type_spec
                    | string_type_spec
                    | array_type_spec
                    | class_type_spec
                    | enum_type_spec
                    | record_type_spec
        """
        if self.current_token.type in (
            TokenType.INTEGER,
            TokenType.REAL,
            TokenType.BOOLEAN,
        ):
            return self.primitive_type_spec()
        elif self.current_token.type == TokenType.STRING:
            return self.string_type_spec()
        elif self.current_token.type == TokenType.ARRAY:
            return self.array_type_spec()
        elif self.current_token.type == TokenType.ID:
            if self.current_token.value in self.classes:
                return self.class_type_spec()
            elif self.current_token.value in self.enums:
                return self.enum_type_spec()
            elif self.current_token.value in self.records:
                return self.record_type_spec()
            else:
                raise UnknownTypeError()
        else:
            raise UnknownTypeError()

    def primitive_type_spec(self) -> Type:
        """
        primitive_type_spec : INTEGER | REAL | BOOLEAN
        """
        token = self.current_token
        if self.current_token.type == TokenType.INTEGER:
            self.eat(TokenType.INTEGER)
        elif self.current_token.type == TokenType.REAL:
            self.eat(TokenType.REAL)
        elif self.current_token.type == TokenType.BOOLEAN:
            self.eat(TokenType.BOOLEAN)
        node = PrimitiveType(token)
        return node

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
            raise ParserError()

    def array_type_spec(self) -> ArrayType:
        """array_type_spec : ARRAY (LBRACKET INTEGER_CONST RANGE INTEGER_CONST RBRACKET)? of type_spec"""
        token = self.current_token
        self.eat(TokenType.ARRAY)
        lower: Factor = self.newZeroNum(
            lineno=self.current_token.lineno, column=self.current_token.column
        )
        upper: Factor = self.newZeroNum(
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

    def enum_type_spec(self) -> EnumType:
        token = self.current_token
        node = EnumType(
            token=Token(
                type=TokenType.ENUM,
                value=token.value,
                lineno=token.lineno,
                column=token.column,
            )
        )
        self.eat(TokenType.ID)
        return node

    def class_type_spec(self) -> ClassType:
        token = self.current_token
        node = ClassType(
            token=Token(
                type=TokenType.CLASS,
                value=token.value,
                lineno=token.lineno,
                column=token.column,
            )
        )
        self.eat(TokenType.ID)
        return node

    def record_type_spec(self) -> RecordType:
        token = self.current_token
        node = RecordType(
            token=Token(
                type=TokenType.RECORD,
                value=token.value,
                lineno=token.lineno,
                column=token.column,
            )
        )
        self.eat(TokenType.ID)
        return node

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
                  | method_call_statement
                  | assignment_statement
                  | if_statement
                  | case_statement
                  | for_statement
                  | while_statement
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
        elif self.current_token.type == TokenType.ID:
            token = self.current_token
            next_token_type = self.peek_next_token().type
            if next_token_type == TokenType.LPAREN:
                node = self.proccall_statement()
            elif next_token_type == TokenType.DOT:
                if token.value in self.records:
                    node = self.assignment_statement()
                else:
                    node = self.method_call_statement()
            else:
                node = self.assignment_statement()
        else:
            node = self.empty()
        return node

    def method_call_statement(self) -> MethodCall:
        """
        method_call_statement : ID DOT ID (LPAREN (expr (COMMA expr)*)? RPAREN)?
        """
        token = self.current_token

        inst_name = self.current_token.value
        self.eat(TokenType.ID)
        self.eat(TokenType.DOT)
        method_name = self.current_token.value
        self.eat(TokenType.ID)
        actual_params: list[Expr] = []
        if self.current_token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            if self.current_token.type != TokenType.RPAREN:
                expr = self.expr()
                actual_params.append(expr)
            while self.current_token.type == TokenType.COMMA:
                self.eat(TokenType.COMMA)
                expr = self.expr()
                actual_params.append(expr)
            self.eat(TokenType.RPAREN)

        node = MethodCall(
            method_full_name="{inst_name}.{method_name}".format(
                inst_name=inst_name, method_name=method_name
            ),
            actual_params=actual_params,
            token=token,
        )
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

    def case_statement(self) -> CaseStatement:
        """
        case_statement:
            CASE expr OF ( ( literal | variable ) COLON statement SEMI )+ ( ELSE statement SEMI )? END SEMI
        """
        self.eat(TokenType.CASE)
        matcher = self.expr()
        self.eat(TokenType.OF)

        branches: list[tuple[AST, AST]] = []
        else_branch: AST | None = None
        if self.current_token.type in (
            TokenType.INTEGER_CONST,
            TokenType.STRING_CONST,
            TokenType.TRUE,
            TokenType.FALSE,
            TokenType.ID,
        ):
            condition = self.expr()
            self.eat(TokenType.COLON)
            branch = self.statement()
            self.eat(TokenType.SEMI)
            branches.append((condition, branch))
        else:
            raise InvalidCaseStatementError()
        while self.current_token.type in (
            TokenType.INTEGER_CONST,
            TokenType.STRING_CONST,
            TokenType.TRUE,
            TokenType.FALSE,
            TokenType.ID,
        ):
            condition = self.expr()
            self.eat(TokenType.COLON)
            branch = self.statement()
            self.eat(TokenType.SEMI)
            branches.append((condition, branch))
        if self.current_token.type == TokenType.ELSE:
            self.eat(TokenType.ELSE)
            else_branch = self.statement()
            self.eat(TokenType.SEMI)
        self.eat(TokenType.END)
        node = CaseStatement(
            matcher,
            branches,
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

    def proccall_statement(self) -> ProcedureCall:
        """proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN"""
        token = self.current_token

        proc_name = self.current_token.value
        self.eat(TokenType.ID)
        self.eat(TokenType.LPAREN)
        actual_params: list[Expr] = []
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

    def method_call_expr(self) -> MethodCall:
        """
        method_call_expr : id_expr (LPAREN (expr (COMMA expr)*)? RPAREN)?
        """
        token = self.current_token
        method_name = self.id_expr()

        actual_params: list[Expr] = []
        actual_params.append(
            Var(Token(TokenType.ID, method_name, token.lineno, token.column))
        )
        if self.current_token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            if self.current_token.type != TokenType.RPAREN:
                expr = self.summation_expr()
                actual_params.append(expr)
            while self.current_token.type == TokenType.COMMA:
                self.eat(TokenType.COMMA)
                expr = self.summation_expr()
                actual_params.append(expr)
            self.eat(TokenType.RPAREN)

        node = MethodCall(
            method_full_name=method_name,
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
        actual_params: list[Expr] = []

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

    def assignment_statement(self) -> Assign | ExprSet:
        """
        assignment_statement : variable ASSIGN expr | expr_get ASSIGN expr
        """
        # Check if we have a complex property access
        if self.current_token.type == TokenType.ID and self.peek_next_token().type in (
            TokenType.DOT,
            TokenType.LBRACKET,
        ):
            left = self.expr_get()
            token = self.current_token
            self.eat(TokenType.ASSIGN)
            right = self.expr()
            node = ExprSet(left, right, token)
            return node
        else:
            # Original variable assignment
            left = self.variable()
            if not isinstance(left, Var):
                self.error(ErrorCode.UNEXPECTED_TOKEN, left.token)
            token = self.current_token
            self.eat(TokenType.ASSIGN)
            right = self.expr()
            assert isinstance(left, Var)
            node = Assign(left, token, right)
            return node

    def expr_get(self) -> ExprGet:
        """
        expr_get : variable (DOT ID | LBRACKET expr RBRACKET)+
        """
        base_var = self.variable()
        gets = []

        # If we got a RecordVar, convert it to a base Var and a GetItem
        if isinstance(base_var, RecordVar):
            # Create a base variable for the record
            base_object = Var(
                Token(
                    type=TokenType.ID,
                    value=base_var.name,
                    lineno=base_var.token.lineno,
                    column=base_var.token.column,
                )
            )

            # Add the property access as the first GetItem
            gets.append(
                GetItem(
                    token=Token(
                        type=TokenType.DOT,
                        value=".",
                        lineno=base_var.token.lineno,
                        column=base_var.token.column,
                    ),
                    key=base_var.key,
                )
            )
        else:
            base_object = base_var

        # Continue with any additional property or index accesses
        while self.current_token.type in (TokenType.DOT, TokenType.LBRACKET):
            if self.current_token.type == TokenType.DOT:
                token = self.current_token
                self.eat(TokenType.DOT)
                property_name = self.current_token.value
                self.eat(TokenType.ID)
                gets.append(GetItem(token, property_name))
            elif self.current_token.type == TokenType.LBRACKET:
                token = self.current_token
                self.eat(TokenType.LBRACKET)
                index = self.expr()
                self.eat(TokenType.RBRACKET)
                gets.append(GetItem(token, index))

        return ExprGet(base_object, gets)

    def variable(self) -> Var:
        """
        variable: ID
        """
        token = self.current_token
        var_name = self.id_expr()
        node = Var(
            Token(
                type=token.type,
                value=var_name,
                lineno=token.lineno,
                column=token.column,
            )
        )
        if var_name.find(".") != -1:
            type_name, type_key = var_name.split(".")
            if type_name in self.records:
                node = RecordVar(token=node.token, name=type_name, key=type_key)
            elif type_name in self.enums:
                # TODO: should use EnumVar instead of Var for enum type ?
                pass

        # We no longer handle indexing here as it's part of expr_get now
        return node

    def empty(self) -> NoOp:
        """An empty production"""
        return NoOp()

    def expr(self) -> Expr:
        """
        expr : logic_expr
        """
        return self.logic_expr()

    def logic_expr(self) -> Expr:
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

    def comparison_expr(self) -> Expr:
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

    def summation_expr(self) -> Expr:
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

    def multiplication_expr(self) -> Term:
        """multiplication_expr : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*"""
        node = self.factor()

        while self.current_token.type in (
            TokenType.MUL,
            TokenType.INTEGER_DIV,
            TokenType.FLOAT_DIV,
        ):
            token = self.current_token
            if token.type == TokenType.MUL:
                self.eat(TokenType.MUL)
            elif token.type == TokenType.INTEGER_DIV:
                self.eat(TokenType.INTEGER_DIV)
            elif token.type == TokenType.FLOAT_DIV:
                self.eat(TokenType.FLOAT_DIV)

            node = BinOp(left=node, op=token, right=self.factor())

        return node

    def factor(self) -> Factor:
        """
        factor : not comparison_expr
               | PLUS factor
               | MINUS factor
               | literal
               | LPAREN expr RPAREN
               | func_call_expr
               | method_call_expr
               | expr_get
               | variable
        """
        token = self.current_token
        node: Factor
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

        if token.type in (
            TokenType.INTEGER_CONST,
            TokenType.REAL_CONST,
            TokenType.TRUE,
            TokenType.FALSE,
            TokenType.STRING_CONST,
        ):
            return self.literal()

        # parent take precedence
        if token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            node = self.summation_expr()
            self.eat(TokenType.RPAREN)
            return node

        if token.type == TokenType.ID and self.peek_next_token().type == TokenType.DOT:
            if token.value in self.classes:
                # call method
                node = self.method_call_expr()
                return node
            elif token.value in self.enums:
                # call enum variable
                node = self.variable()
                return node
            elif token.value in self.records:
                # call enum variable
                node = self.expr_get()
                return node

        # call procedure
        if (
            token.type == TokenType.ID
            and self.peek_next_token().type == TokenType.LPAREN
        ):
            node = self.func_call_expr()
            return node
        else:
            # Check if this could be a complex property access
            if token.type == TokenType.ID:
                next_token = self.peek_next_token()
                if next_token.type in (TokenType.DOT, TokenType.LBRACKET):
                    node = self.expr_get()
                    return node

            node = self.variable()
            return node

    def literal(self) -> Factor:
        """
        literal :
            INTEGER_CONST | STRING_CONST | REAL_CONST | TRUE_CONST| FALSE_CONST
        """
        token = self.current_token
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
        elif self.current_token.type == TokenType.STRING_CONST:
            token = self.current_token
            self.eat(TokenType.STRING_CONST)
            return String(token=token)
        else:
            raise UnknownLiteralError()

    def parse(self):
        """
        program : PROGRAM variable SEMI block DOT

        block : declarations compound_statement

        declarations :
            (const_declaration)?
            (type_declaration)?
            (VAR (variable_declaration SEMI)+)?
            procedure_declaration*
            function_declaration*
            (VAR (variable_declaration SEMI)+)?

        const_declaration:
            CONST (const_assignment_statement)+

        const_assignment_statement:
            (ID EQ factor SEMI) | (ID COLON array_type_spec EQ LPAREN factor ( COMMA factor )* RPAREN SEMI)

        type_declaration:
            TYPE ( class_definition | enum_definition | record_definition )*

        class_definition:
            ID = CLASS (PRIVATE (field_definition SEMI)+)? (PUBLIC  (method_definition)+)? END SEMI

        record_definition:
            ID = RECORD ( field_definition SEMI )+ END SEMI

        enum_definition:
            ID = LPAREN ID ( COMMA ID )* RPAREN SEMI

        field_definition: variable_declaration

        method_definition:
            ( CONSTRUCTOR | PROCEDURE | FUNCTION ) ID DOT ID (LPAREN (formal_parameter_list)? RPAREN)? ( COLON type_spec )? SEMI

        method_declaration :
            method_definition block SEMI

        constructor_declaration :
            constructor_definition SEMI block SEMI

        constructor_definition:
            CONSTRUCTOR id_expr (LPAREN formal_parameter_list RPAREN)? SEMI

        procedure_declaration :
            procedure_definition SEMI block SEMI

        procedure_definition:
            PROCEDURE id_expr (LPAREN formal_parameter_list RPAREN)? SEMI

        function_declaration :
            function_definition block SEMI

        function_definition:
            FUNCTION id_expr LPAREN (formal_parameter_list)? RPAREN COLON type_spec SEMI

        variable_declaration : ID (COMMA ID)* COLON type_spec

        formal_params_list : formal_parameters
                           | formal_parameters SEMI formal_parameter_list

        formal_parameters : ID (COMMA ID)* COLON type_spec

        type_spec : primitive_type_spec
                    | string_type_spec
                    | array_type_spec
                    | class_type_spec
                    | enum_type_spec
                    | record_type_spec

        primitive_type_spec : INTEGER | REAL | BOOLEAN

        string_type_spec: STRING ( LBRACKET INTEGER_CONST RBRACKET )?

        array_type_spec : ARRAY ( LBRACKET INTEGER_CONST RANGE INTEGER_CONST RBRACKET )? of type_spec

        class_type_spec : ID

        compound_statement : BEGIN statement_list END

        statement_list : statement
                       | statement SEMI statement_list

        statement : compound_statement
                  | proccall_statement
                  | method_call_statement
                  | assignment_statement
                  | if_statement
                  | case_statement
                  | for_statement
                  | while_statement
                  | empty

        if_statement: IF logic_expr THEN (statement | compound_statement)
                    (ELSE IF logic_expr THEN (statement | compound_statement))*
                    (ELSE (statement | compound_statement))? SEMI

        case_statement:
            CASE expr OF ( ( literal | variable ) COLON statement SEMI )+ ( ELSE statement SEMI )? END SEMI

        while_statement:  WHILE logic_expr DO compound_statement SEMI

        for_statement:  FOR assignment_statement TO summation_expr DO (statement | compound_statement) SEMI

        proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN

        method_call_statement : ID DOT ID (LPAREN (expr (COMMA expr)*)? RPAREN)?

        func_call_expr : ID LPAREN (expr (COMMA expr)*)? RPAREN

        method_call_expr : id_expr (LPAREN (expr (COMMA expr)*)? RPAREN)?

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
               | literal
               | LPAREN expr RPAREN
               | func_call_expr
               | method_call_expr
               | expr_get
               | variable

        literal :
            INTEGER_CONST | STRING_CONST | REAL_CONST | TRUE_CONST| FALSE_CONST

        variable: id_expr (LBRACKET summation_expr RBRACKET)?

        id_expr : ID ( DOT  ID )*
        """
        node = self.program()
        if self.current_token.type != TokenType.EOF:
            self.error(
                error_code=ErrorCode.UNEXPECTED_TOKEN,
                token=self.current_token,
            )

        return node
