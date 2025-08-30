###############################################################################
#                                                                             #
#  SYMBOLS, TABLES, SEMANTIC ANALYSIS                                         #
#                                                                             #
###############################################################################

from __future__ import annotations

from enum import Enum
from typing import cast

from src.error import *
from src.globals import _SHOULD_LOG_SCOPE
from src.spi_ast import *
from src.spi_token import *
from src.symbol import *
from src.util import SpiUtil
from src.visitor import NodeVisitor


class NativeMethod(Enum):
    WRITE = "WRITE"
    WRITELN = "WRITELN"
    READ = "READ"
    READLN = "READLN"
    ORD = "ORD"
    LOW = "LOW"
    HIGH = "HIGH"
    LENGTH = "LENGTH"
    SETLENGTH = "SETLENGTH"


class ScopedSymbolTable:
    def __init__(
        self,
        scope_name: str,
        scope_level: int,
        enclosing_scope: ScopedSymbolTable | None,
    ) -> None:
        self._symbols: dict[str, Symbol] = {}
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.enclosing_scope = enclosing_scope

    def _init_builtins(self) -> None:
        self.insert(BuiltinTypeSymbol(name="INTEGER"))
        self.insert(BuiltinTypeSymbol(name="REAL"))
        self.insert(BuiltinTypeSymbol(name="BOOLEAN"))
        self.insert(StringTypeSymbol(name="STRING", limit=255))
        self.insert(
            BuiltinProcedureSymbol(name=NativeMethod.WRITE.name, output_params=[])
        )
        self.insert(
            BuiltinProcedureSymbol(name=NativeMethod.WRITELN.name, output_params=[])
        )
        self.insert(
            BuiltinProcedureSymbol(name=NativeMethod.READ.name, output_params=[])
        )
        self.insert(
            BuiltinProcedureSymbol(name=NativeMethod.READLN.name, output_params=[])
        )
        self.insert(
            BuiltinProcedureSymbol(name=NativeMethod.SETLENGTH.name, output_params=[])
        )
        self.insert(
            BuiltinFunctionSymbol(
                name=NativeMethod.LENGTH.name,
                return_type=Type(
                    token=Token(type=TokenType.INTEGER, value=0, lineno=-1, column=-1)
                ),
                formal_params=[],
            )
        )
        self.insert(
            BuiltinFunctionSymbol(
                name=NativeMethod.LOW.name,
                return_type=Type(
                    token=Token(type=TokenType.INTEGER, value=0, lineno=-1, column=-1)
                ),
                formal_params=[],
            )
        )
        self.insert(
            BuiltinFunctionSymbol(
                name=NativeMethod.HIGH.name,
                return_type=Type(
                    token=Token(type=TokenType.INTEGER, value=0, lineno=-1, column=-1)
                ),
                formal_params=[],
            )
        )
        self.insert(
            BuiltinFunctionSymbol(
                name=NativeMethod.ORD.name,
                return_type=Type(
                    token=Token(type=TokenType.INTEGER, value=0, lineno=-1, column=-1)
                ),
                formal_params=[],
            )
        )

    def __str__(self) -> str:
        h1 = "SCOPE (SCOPED SYMBOL TABLE)"
        lines = ["\n", h1, "=" * len(h1)]
        for header_name, header_value in (
            ("Scope name", self.scope_name),
            ("Scope level", self.scope_level),
            (
                "Enclosing scope",
                self.enclosing_scope.scope_name if self.enclosing_scope else None,
            ),
        ):
            lines.append(f"{header_name:<15}: {header_value}")
        h2 = "Scope (Scoped symbol table) contents"
        lines.extend([h2, "-" * len(h2)])
        lines.extend(f"{key:>7}: {value}" for key, value in self._symbols.items())
        lines.append("\n")
        s = "\n".join(lines)
        return s

    __repr__ = __str__

    def log(self, msg: str) -> None:
        if _SHOULD_LOG_SCOPE:
            print(msg)

    def insert(self, symbol: Symbol) -> None:
        self.log(f"Insert: {symbol.name}")
        symbol.scope_level = self.scope_level
        self._symbols[symbol.name] = symbol

    def insert_pair(self, name: str, symbol: Symbol) -> None:
        self.log(f"Insert: {name}")
        symbol.scope_level = self.scope_level
        self._symbols[name] = symbol

    def lookup(self, name: str, current_scope_only: bool = False) -> Symbol | None:
        self.log(f"Lookup: {name}. (Scope name: {self.scope_name})")
        # 'symbol' is either an instance of the Symbol class or None
        symbol = self._symbols.get(name)

        if symbol is not None:
            return symbol

        #  variables , identifiers,  function and procedure names in Pascal are not case-sensitive
        symbol = self._symbols.get(name.upper())
        if symbol is not None:
            return symbol

        if current_scope_only:
            return None

        # recursively go up the chain and lookup the name
        if self.enclosing_scope is not None:
            return self.enclosing_scope.lookup(name)
        return None


class SemanticAnalyzer(NodeVisitor):
    __string_type_limit: int = 255

    def __init__(self) -> None:
        self.current_scope: ScopedSymbolTable | None = None
        # used for for-loop
        self.loop_vars: list[str] = []
        self.constants: list[str] = []

    def log(self, msg) -> None:
        if _SHOULD_LOG_SCOPE:
            print(msg)

    @staticmethod
    def string_type_name(size: int):
        return "STRING[{size}]".format(size=size)

    def error(self, error_code: ErrorCode, token: Token):
        raise SemanticError(
            error_code=error_code,
            token=token,
            message=f"{error_code.value} -> {token}",
        )
    
    def lookup_symbol(self, name: str, current_scope_only: bool = False) -> Symbol | None:
        """
        Module-aware symbol lookup that uses cross-module resolution when available.
        
        Args:
            name: Symbol name to lookup
            current_scope_only: If True, only search current scope
            
        Returns:
            Symbol if found, None otherwise
        """
        if self.current_scope is None:
            return None
        
        # Import ModuleSymbolTable here to avoid circular imports
        try:
            from src.module import ModuleSymbolTable
            
            # If current scope is a ModuleSymbolTable, use module-aware lookup
            if isinstance(self.current_scope, ModuleSymbolTable) and not current_scope_only:
                return self.current_scope.lookup_with_modules(name)
        except ImportError:
            # If module system is not available, fall back to standard lookup
            pass
        
        # Fall back to standard lookup
        return self.current_scope.lookup(name, current_scope_only)

    def visit_Block(self, node: Block) -> None:
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_Program(self, node: Program) -> None:
        self.log("ENTER scope: global")
        
        # Check if we already have a ModuleSymbolTable set up (for module-aware analysis)
        created_new_scope = False
        try:
            from src.module import ModuleSymbolTable
            if isinstance(self.current_scope, ModuleSymbolTable):
                # Use the existing module-aware scope
                global_scope = self.current_scope
                self.log("Using existing ModuleSymbolTable as global scope")
            else:
                # Create new standard scope
                global_scope = ScopedSymbolTable(
                    scope_name="global",
                    scope_level=1,
                    enclosing_scope=self.current_scope,  # None
                )
                global_scope._init_builtins()
                self.current_scope = global_scope
                created_new_scope = True
        except ImportError:
            # Module system not available, use standard scope
            global_scope = ScopedSymbolTable(
                scope_name="global",
                scope_level=1,
                enclosing_scope=self.current_scope,  # None
            )
            global_scope._init_builtins()
            self.current_scope = global_scope
            created_new_scope = True

        # visit subtree
        self.visit(node.block)

        self.log(global_scope)

        # Only change scope if we created a new one
        if created_new_scope:
            self.current_scope = self.current_scope.enclosing_scope
        self.log("LEAVE scope: global")

    def visit_Compound(self, node: Compound) -> None:
        for child in node.children:
            self.visit(child)

    def visit_NoOp(self, node: NoOp) -> None:
        pass

    def visit_Decl(self, node: Decl) -> None:
        pass

    def visit_Member(self, node: Member) -> None:
        pass

    def visit_EnumDecl(self, node: EnumDecl) -> None:
        reversed_entries = {value: key for key, value in node.entries.items()}
        enum_symbol = EnumSymbol(name=node.enum_name, entries=reversed_entries)
        if self.current_scope is None:
            raise MissingCurrentScopeError()
        if self.current_scope.lookup(node.enum_name, current_scope_only=True):
            raise DuplicateClassError()
        self.current_scope.insert_pair(node.enum_name, enum_symbol)
        pass

    def visit_ClassDecl(self, node: ClassDecl) -> None:
        fields: dict[str, FieldSymbol] = {}
        for f in node.fields:
            field_symbol = self.visit_FieldDecl(f)
            fields[field_symbol.name] = field_symbol
        methods: dict[str, MethodSymbol] = {}
        for m in node.methods:
            method_symbol = self.visit_MethodDef(m)
            methods[method_symbol.name] = method_symbol
        class_symbol = ClassSymbol(name=node.class_name, fields=fields, methods=methods)

        if self.current_scope is None:
            raise MissingCurrentScopeError()
        if self.current_scope.lookup(node.class_name, current_scope_only=True):
            raise DuplicateClassError()
        self.current_scope.insert_pair(node.class_name, class_symbol)

        # bind constructor
        if node.constructor is not None:
            method_symbol = self.visit_MethodDef(node.constructor.to_def())
            methods[method_symbol.name] = method_symbol
            self.visit_MethodDecl(node.constructor)
        # bind destructor
        if node.destructor is not None:
            method_symbol = self.visit_MethodDef(node.destructor.to_def())
            methods[method_symbol.name] = method_symbol
            self.visit_MethodDecl(node.destructor)
        pass

    def visit_RecordDecl(self, node: RecordDecl) -> None:
        fields: dict[str, FieldSymbol] = {}
        for f in node.fields:
            field_symbol = self.visit_FieldDecl(f)
            fields[field_symbol.name] = field_symbol
        record_symbol = RecordSymbol(name=node.record_name, fields=fields)

        if self.current_scope is None:
            raise MissingCurrentScopeError()
        if self.current_scope.lookup(node.record_name, current_scope_only=True):
            raise DuplicateClassError()
        self.current_scope.insert_pair(node.record_name, record_symbol)
        pass

    def visit_ConstructorDecl(self, node: ConstructorDecl) -> None:
        pass

    def visit_Def(self, node: Def) -> None:
        pass

    def visit_MethodDef(self, node: MethodDef) -> MethodSymbol:
        method_name = node.method_name
        return_type = node.return_type
        method_symbol = MethodSymbol(
            name=method_name,
            return_type=return_type,
            formal_params=[],
            method_type=node.method_type,
        )
        if self.current_scope is None:
            raise MissingCurrentScopeError()
        # Insert parameters into the function scope
        for param in node.params:
            param_type = self.current_scope.lookup(param.type_node.value)
            param_name = param.var_node.value
            var_symbol = VarSymbol(param_name, param_type)
            method_symbol.formal_params.append(var_symbol)
        return method_symbol

    def visit_MethodDecl(self, node: MethodDecl) -> MethodSymbol:
        method_full_name = node.method_full_name
        class_name, method_name = method_full_name.split(".")
        return_type = node.return_type
        method_symbol = MethodSymbol(
            name=method_full_name,
            return_type=return_type,
            formal_params=[],
            method_type=node.method_type,
        )
        if self.current_scope is None:
            raise MissingCurrentScopeError()
        self.current_scope.insert(method_symbol)

        class_name = SpiUtil.toClassName(class_name=class_name)
        symbol = self.lookup_symbol(class_name)
        if symbol is None:
            raise UnknownSymbolError()
        class_symbol = cast(ClassSymbol, symbol)
        # bind method
        class_symbol.methods[method_name].block_ast = node.block

        self.log(f"ENTER method scope: {method_full_name}")
        # Scope for parameters and local variables
        function_scope = ScopedSymbolTable(
            scope_name=method_name,
            scope_level=self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope,
        )
        self.current_scope = function_scope
        for _, fs in class_symbol.fields.items():
            self.current_scope.insert_pair(fs.name, fs)
        for _, ms in class_symbol.methods.items():
            self.current_scope.insert_pair(ms.name, ms)

        # insert return value into the function scope
        # pascal support implicit return the value has the same name to the function name
        return_var_name = method_name
        return_var_symbol = VarSymbol(
            return_var_name, self.lookup_symbol(return_type.value)
        )
        self.current_scope.insert(return_var_symbol)
        method_symbol.formal_params.append(return_var_symbol)

        # Insert parameters into the function scope
        for param in node.params:
            param_type = self.lookup_symbol(param.type_node.value)
            param_name = param.var_node.value
            var_symbol = VarSymbol(param_name, param_type)
            self.current_scope.insert(var_symbol)
            method_symbol.formal_params.append(var_symbol)

        self.visit(node.block)

        self.log(function_scope)

        self.current_scope = self.current_scope.enclosing_scope
        self.log(f"LEAVE method scope: {method_name}")

        # accessed by the interpreter when executing procedure call
        method_symbol.block_ast = node.block
        return method_symbol

    def visit_MethodCall(self, node: MethodCall):
        if self.current_scope is None:
            raise MissingCurrentScopeError()

        class_or_instance_name, method_name = node.method_full_name.split(".")
        symbol = self.lookup_symbol(SpiUtil.toClassName(class_or_instance_name))
        if symbol is None:
            # use instance.method() to call
            symbol = self.lookup_symbol(class_or_instance_name)
            if symbol is None:
                raise UnknownSymbolError()
            else:
                instance_symbol = cast(VarSymbol, symbol)
                class_symbol = cast(ClassSymbol, instance_symbol.type)
                if method_name in class_symbol.methods:
                    method_symbol = class_symbol.methods[method_name]
                    method_type = method_symbol.method_type
                    node.method_symbol = cast(MethodSymbol, method_symbol)
        else:
            # use class.method() to call
            class_symbol = cast(ClassSymbol, symbol)
            if method_name in class_symbol.methods:
                method_symbol = class_symbol.methods[method_name]
                method_type = method_symbol.method_type
                node.method_symbol = cast(MethodSymbol, method_symbol)

        node.method_type = method_type

        actual_params_len = 0
        if method_type == MethodType.PROCEDURE:
            actual_params_len = len(node.actual_params)
        elif method_type == MethodType.FUNCTION:
            actual_params_len = len(node.actual_params) - 1
        elif method_type == MethodType.CONSTRUCTOR:
            actual_params_len = len(node.actual_params) - 1
        if len(method_symbol.formal_params) < actual_params_len:
            raise TooManyParametersError()
        elif len(method_symbol.formal_params) > actual_params_len:
            raise LackOfParametersError()
        for param_node in node.actual_params[1:]:
            self.visit(param_node)

    def visit_ProcedureDef(self, node: ProcedureDef) -> None:
        pass

    def visit_ConstructorDef(self, node: ConstructorDef) -> None:
        pass

    def visit_FunctionDef(self, node: FunctionDef) -> None:
        pass

    def visit_WhileStatement(self, node: WhileStatement) -> None:
        self.visit(node.condition)
        self.visit(node.block)

    def visit_ForStatement(self, node: ForStatement) -> None:
        self.visit(node.initialization)
        self.visit(node.bound)
        var_name = node.initialization.left.value
        self.loop_vars.append(var_name)
        self.visit(node.block)
        self.loop_vars.remove(var_name)

    def visit_IfStatement(self, node: IfStatement) -> None:
        self.visit(node.condition)
        self.visit(node.then_branch)
        for branch in node.else_if_branches:
            self.visit(branch)
        if node.else_branch is not None:
            self.visit(node.else_branch)

    def visit_CaseStatement(self, node: CaseStatement) -> None:
        self.visit(node.matcher)
        for condition, branch in node.branches:
            self.visit(condition)
            self.visit(branch)
        if node.else_branch is not None:
            self.visit(node.else_branch)

    def visit_BinOp(self, node: BinOp) -> None:
        self.visit(node.left)
        self.visit(node.right)

    def visit_Type(self, node: Type):
        pass

    def visit_PrimitiveType(self, node: PrimitiveType):
        pass

    def visit_StringType(self, node: StringType):
        if self.current_scope is None:
            raise MissingCurrentScopeError()
        if isinstance(node.limit, Num):
            limit = int(node.limit.value)
            self.__string_type_limit = limit
            self.current_scope.insert(
                StringTypeSymbol(
                    name=SemanticAnalyzer.string_type_name(limit), limit=int(limit)
                )
            )
        elif node.limit is None:
            self.current_scope.insert(
                StringTypeSymbol(name=SemanticAnalyzer.string_type_name(255), limit=255)
            )
        pass

    def visit_ArrayType(self, node: ArrayType) -> None:
        if isinstance(node.element_type, ArrayType):
            self.visit_ArrayType(node.element_type)
        if self.current_scope is None:
            raise MissingCurrentScopeError
        element_type_symbol = self.lookup_symbol(str(node.element_type))
        if element_type_symbol is None:
            raise UnknownArrayElementTypeError()
        type_name = str(node)
        type_symbol = self.lookup_symbol(type_name)
        if type_symbol is None:
            self.current_scope.insert(
                ArrayTypeSymbol(name=type_name, element_type=element_type_symbol)
            )

    def visit_ClassType(self, node: ClassType):
        if self.current_scope is None:
            raise MissingCurrentScopeError()
        symbol = self.lookup_symbol(str(node))
        if symbol is None:
            raise UnknownClassTypeError()

    def visit_EnumType(self, node: EnumType):
        if self.current_scope is None:
            raise MissingCurrentScopeError()
        symbol = self.lookup_symbol(node.value)
        if symbol is None:
            raise UnknownEnumTypeError()

    def visit_RecordType(self, node: RecordType):
        if self.current_scope is None:
            raise MissingCurrentScopeError()
        symbol = self.lookup_symbol(node.value)
        if symbol is None:
            raise UnknownRecordTypeError()

    def visit_FieldDecl(self, node: FieldDecl) -> FieldSymbol:
        var_symbol = self.visit_VarDecl(node.to_VarDecl())
        return var_symbol.to_FieldSymbol()

    def visit_ConstAssign(self, node: ConstAssign):
        if self.current_scope is None:
            raise MissingCurrentScopeError()
        if node.const_type == ConstType.ARRAY:
            assert node.type is not None
            self.visit(node.type)
            self.visit(node.right)
            const_name = node.left.value
            type_name = str(node.type)
            type_symbol = self.lookup_symbol(type_name)
            const_symbol = ConstSymbol(
                name=const_name, type=type_symbol, const_type=ConstType.ARRAY
            )
            self.current_scope.insert(const_symbol)
            self.constants.append(const_name)
        elif node.const_type == ConstType.NON_ARRAY:
            self.visit(node.right)
            const_name = node.left.value
            const_symbol = ConstSymbol(
                name=const_name, type=None, const_type=ConstType.NON_ARRAY
            )
            self.current_scope.insert(const_symbol)
            self.constants.append(const_name)
        else:
            raise SemanticError()

    def visit_VarDecl(self, node: VarDecl) -> VarSymbol:
        if self.current_scope is None:
            raise MissingCurrentScopeError()

        type_name = node.type_node.value
        if isinstance(node.type_node, ArrayType):
            self.visit(node.type_node)
            type_name = str(node.type_node)
        elif isinstance(node.type_node, StringType):
            self.visit(node.type_node)
            type_name = SemanticAnalyzer.string_type_name(size=self.__string_type_limit)
        elif isinstance(node.type_node, RecordType):
            self.visit(node.type_node)
            type_name = node.type_node.value
        elif isinstance(node.type_node, ClassType):
            self.visit(node.type_node)
            type_name = SpiUtil.toClassName(type_name)
        type_symbol = self.lookup_symbol(type_name)

        # We have all the information we need to create a variable symbol.
        # Create the symbol and insert it into the symbol table.
        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name, type_symbol)

        # Signal an error if the table already has a symbol
        # with the same name
        if self.current_scope.lookup(var_name, current_scope_only=True):
            self.error(
                error_code=ErrorCode.DUPLICATE_ID,
                token=node.var_node.token,
            )

        self.current_scope.insert(var_symbol)
        return var_symbol

    def visit_Assign(self, node: Assign) -> None:
        # right-hand side
        self.visit(node.right)
        if self.current_scope is None:
            raise MissingCurrentScopeError()
        # left-hand side
        if isinstance(node.left, RecordVar):
            self.visit(node.left)
            record_name, record_field = node.left.name, node.left.key
            symbol = self.lookup_symbol(record_name)
            if symbol is None:
                raise UnknownSymbolError()
            record_symbol = cast(RecordSymbol, cast(VarSymbol, symbol).type)
            if record_field not in record_symbol.fields.keys():
                raise UnknownRecordFieldError()
        else:
            if node.left.value in self.constants:
                self.error(ErrorCode.MODIFY_CONST_NOT_ALLOW, token=node.left.token)
            if node.left.value in self.loop_vars:
                self.error(ErrorCode.MODIFY_LOOP_VAR_NOT_ALLOW, token=node.left.token)
            self.visit(node.left)
            var_name = node.left.value
            if var_name.find(".") == -1:
                var_symbol = self.lookup_symbol(var_name)
                if var_symbol is None:
                    raise UnknownSymbolError()
                if var_symbol.type is None:
                    raise UnknownSymbolError()
                if isinstance(var_symbol.type, StringTypeSymbol):
                    # string_size = var_symbol.type.limit
                    # if isinstance(node.right, String):
                    #     string_value = node.right.value
                    #     if len(string_value) > string_size:
                    #         message = f"Warning: String literal has more characters[{len(string_value)}] than short string length[{string_size}]"
                    #         SpiUtil.print_w(message=message)
                    pass

    def visit_Var(self, node: Var) -> None:
        var_name = cast(str, node.value)
        if self.current_scope is None:
            raise MissingCurrentScopeError()

        if var_name.find(".") != -1:
            type_name, type_key = var_name.split(".")
            symbol = self.lookup_symbol(type_name)
            if symbol is None:
                self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)
                return
            if isinstance(symbol, EnumSymbol):
                enum_symbol = cast(EnumSymbol, symbol)
                if type_key not in enum_symbol.entries:
                    raise UnknownEnumTypeError()
            elif isinstance(symbol, RecordSymbol):
                record_symbol = cast(RecordSymbol, symbol)
                if type_key not in record_symbol.fields:
                    raise UnknownRecordFieldError()
        else:
            var_symbol = self.lookup_symbol(var_name)
            if var_symbol is None:
                self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)
                return

    def visit_RecordVar(self, node: RecordVar) -> None:
        if self.current_scope is None:
            raise MissingCurrentScopeError()
        var_symbol = self.lookup_symbol(node.name)
        if var_symbol is None:
            self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)
            return
        record_symbol = cast(RecordSymbol, var_symbol.type)
        if node.key not in record_symbol.fields.keys():
            raise UnknownRecordFieldError()

    def visit_IndexVar(self, node: IndexVar) -> None:
        var_name = node.value
        if self.current_scope is None:
            self.error(error_code=ErrorCode.NULL_POINTER, token=node.token)
            return

        if var_name.find(".") != -1:
            self.visit(node.left)
        else:
            var_symbol = self.lookup_symbol(var_name)
            if var_symbol is None:
                self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)
                return
        self.visit(node.index)

    def visit_Num(self, node: Num) -> None:
        pass

    def visit_Bool(self, node: Bool) -> None:
        pass

    def visit_String(self, node: String) -> None:
        pass

    def visit_UnaryOp(self, node: UnaryOp) -> None:
        pass

    def visit_ProcedureDecl(self, node: ProcedureDecl) -> None:
        proc_name = node.proc_name
        proc_symbol = ProcedureSymbol(proc_name)
        if self.current_scope is None:
            raise MissingCurrentScopeError()
        self.current_scope.insert(proc_symbol)

        self.log(f"ENTER scope: {proc_name}")
        # Scope for parameters and local variables
        procedure_scope = ScopedSymbolTable(
            scope_name=proc_name,
            scope_level=self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope,
        )
        self.current_scope = procedure_scope

        # Insert parameters into the procedure scope
        for param in node.formal_params:
            param_type = self.lookup_symbol(param.type_node.value)
            param_name = param.var_node.value
            var_symbol = VarSymbol(param_name, param_type)
            self.current_scope.insert(var_symbol)
            proc_symbol.formal_params.append(var_symbol)

        self.visit(node.block_node)

        self.log(procedure_scope)

        self.current_scope = self.current_scope.enclosing_scope
        self.log(f"LEAVE scope: {proc_name}")

        # accessed by the interpreter when executing procedure call
        proc_symbol.block_ast = node.block_node

    def visit_ProcedureCall(self, node: ProcedureCall) -> None:
        for param_node in node.actual_params:
            self.visit(param_node)

        if self.current_scope is None:
            self.error(error_code=ErrorCode.CURRENT_SCOPE_NOT_FOUND, token=node.token)
            return
        proc_symbol = self.lookup_symbol(node.proc_name)
        # accessed by the interpreter when executing procedure call
        node.proc_symbol = cast(ProcedureSymbol, proc_symbol)

    def visit_FunctionDecl(self, node: FunctionDecl) -> None:
        func_name = node.func_name
        return_type = node.return_type
        func_symbol = FunctionSymbol(func_name, return_type)
        if self.current_scope is None:
            raise MissingCurrentScopeError()
        self.current_scope.insert(func_symbol)

        self.log(f"ENTER scope: {func_name}")
        # Scope for parameters and local variables
        function_scope = ScopedSymbolTable(
            scope_name=func_name,
            scope_level=self.current_scope.scope_level + 1,
            enclosing_scope=self.current_scope,
        )
        self.current_scope = function_scope

        # insert return value into the function scope
        # pascal support implicit return the value has the same name to the function name
        return_var_symbol = VarSymbol(
            func_name, self.lookup_symbol(return_type.value)
        )
        self.current_scope.insert(return_var_symbol)
        func_symbol.formal_params.append(return_var_symbol)

        # Insert parameters into the function scope
        for param in node.formal_params:
            param_type = self.lookup_symbol(param.type_node.value)
            param_name = param.var_node.value
            var_symbol = VarSymbol(param_name, param_type)
            self.current_scope.insert(var_symbol)
            func_symbol.formal_params.append(var_symbol)

        self.visit(node.block_node)

        self.log(function_scope)

        self.current_scope = self.current_scope.enclosing_scope
        self.log(f"LEAVE scope: {func_name}")

        # accessed by the interpreter when executing procedure call
        func_symbol.block_ast = node.block_node

    def visit_FunctionCall(self, node: FunctionCall) -> None:
        for param_node in node.actual_params:
            self.visit(param_node)

        if self.current_scope is None:
            self.error(error_code=ErrorCode.CURRENT_SCOPE_NOT_FOUND, token=node.token)
            return
        func_symbol = self.lookup_symbol(node.func_name)
        # accessed by the interpreter when executing procedure call
        node.func_symbol = cast(FunctionSymbol, func_symbol)

    def visit_GetItem(self, node: GetItem) -> None:
        if node.is_property:
            # It's a property access, nothing to visit
            pass
        else:
            # It's an index access, visit the index expression
            self.visit(node.key)

    def visit_ExprGet(self, node: ExprGet) -> None:
        # Visit the base object
        self.visit(node.object)
        
        # Visit all the property/index accesses
        for get_item in node.gets:
            self.visit(get_item)

    def visit_ExprSet(self, node: ExprSet) -> None:
        # Visit the left side (ExprGet)
        self.visit(node.expr_get)
        
        # Visit the right side (value being assigned)
        self.visit(node.value)
        
        # We could add additional checks here, like ensuring the types match
        # or checking if the target is a constant or loop variable that
        # shouldn't be modified, similar to visit_Assign
