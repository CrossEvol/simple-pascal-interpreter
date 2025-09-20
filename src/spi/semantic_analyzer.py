###############################################################################
#                                                                             #
#  TABLES, SEMANTIC ANALYSIS                                         #
#                                                                             #
###############################################################################

from __future__ import annotations

from typing import cast

from spi.ast_and_symbol import (
    AccessExpression,
    ArrayType,
    ArrayTypeSymbol,
    Assign,
    BinOp,
    Block,
    Bool,
    BuiltinFunctionSymbol,
    BuiltinProcedureSymbol,
    BuiltinTypeSymbol,
    CaseStatement,
    Char,
    Compound,
    EnumType,
    EnumTypeSymbol,
    ForStatement,
    FunctionCall,
    FunctionDecl,
    FunctionSymbol,
    IfStatement,
    IndexSuffix,
    MemberSuffix,
    NoOp,
    Num,
    PrimitiveType,
    ProcedureCall,
    ProcedureDecl,
    ProcedureSymbol,
    Program,
    RecordFieldSymbol,
    RecordType,
    RecordTypeSymbol,
    String,
    StringType,
    StringTypeSymbol,
    Symbol,
    Type,
    TypeDeclaration,
    UnaryOp,
    Var,
    VarDecl,
    VariantPart,
    VariantPartSymbol,
    VarSymbol,
    WhileStatement,
)
from spi.error import (
    ErrorCode,
    SemanticError,
)
from spi.native import NativeMethod
from spi.token import Token, TokenType
from spi.visitor import NodeVisitor

_SHOULD_LOG_SCOPE = False  # see '--scope' command line option
_SHOULD_LOG_STACK = False  # see '--stack' command line option


class ScopedSymbolTable:
    def __init__(
        self,
        scope_name: str,
        scope_level: int,
        enclosing_scope: "ScopedSymbolTable" | None,
    ) -> None:
        self._symbols: dict[str, Symbol] = {}
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.enclosing_scope = enclosing_scope

    def _init_builtins(self) -> None:
        self.insert(BuiltinTypeSymbol("INTEGER"))
        self.insert(BuiltinTypeSymbol("REAL"))
        self.insert(BuiltinTypeSymbol("BOOLEAN"))
        self.insert(BuiltinTypeSymbol("STRING"))
        self.insert(BuiltinTypeSymbol("CHAR"))
        self.insert(
            BuiltinProcedureSymbol(name=NativeMethod.WRITE.name, output_params=[])
        )
        self.insert(
            BuiltinProcedureSymbol(name=NativeMethod.WRITELN.name, output_params=[])
        )
        self.insert(
            BuiltinProcedureSymbol(name=NativeMethod.SETLENGTH.name, output_params=[])
        )
        self.insert(
            BuiltinProcedureSymbol(name=NativeMethod.INC.name, output_params=[])
        )
        self.insert(
            BuiltinProcedureSymbol(name=NativeMethod.DEC.name, output_params=[])
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
                name=NativeMethod.ORD.name,
                return_type=Type(
                    token=Token(type=TokenType.INTEGER, value=0, lineno=-1, column=-1)
                ),
                formal_params=[],
            )
        )
        self.insert(
            BuiltinFunctionSymbol(
                name=NativeMethod.CHR.name,
                return_type=Type(
                    token=Token(type=TokenType.CHAR, value="", lineno=-1, column=-1)
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
        self.current_type: Symbol | None = None
        self.unmodified_vars: list[str] = []
        # 枚举类型和值的注册表
        self.enum_types: dict[
            str, dict
        ] = {}  # { type_name -> { 'values': [value_names...], 'size': int } }
        self.enum_values: dict[
            str, dict
        ] = {}  # { value_name -> { 'type': type_name, 'ordinal': int } }
        # 类型名称和符号的映射关系
        self.type_mappings: dict[str, Symbol] = {}  # { type_name -> type_symbol }

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

    def visit_Block(self, node: Block) -> None:
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_Program(self, node: Program) -> None:
        self.log("ENTER scope: global")
        global_scope = ScopedSymbolTable(
            scope_name="global",
            scope_level=1,
            enclosing_scope=self.current_scope,  # None
        )
        global_scope._init_builtins()
        self.current_scope = global_scope

        # visit subtree
        self.visit(node.block)

        self.log(global_scope)

        self.current_scope = self.current_scope.enclosing_scope
        self.log("LEAVE scope: global")

    def visit_Compound(self, node: Compound) -> None:
        for child in node.children:
            self.visit(child)

    def visit_TypeDeclaration(self, node: TypeDeclaration) -> None:
        type_name = node.type_name.value
        # 跨命名空间查重（变量/过程/函数/类型/枚举值统一冲突检测）
        if self.current_scope is None:
            raise SemanticError(
                error_code=ErrorCode.MISSING_CURRENT_SCOPE,
                token=node.type_name.token,
                message=f"{ErrorCode.MISSING_CURRENT_SCOPE.value} -> {node.type_name.token}",
            )

        if self.current_scope.lookup(type_name, current_scope_only=True):
            self.error(
                error_code=ErrorCode.DUPLICATE_ID,
                token=node.type_name.token,
            )

        if isinstance(node.type_def, EnumType):
            enum_values = node.type_def.enum_values
            # 为每个枚举值做同名检测
            for i, enum_val in enumerate(enum_values):
                if self.current_scope.lookup(enum_val, current_scope_only=True):
                    self.error(
                        error_code=ErrorCode.DUPLICATE_ID,
                        token=Token(
                            TokenType.ID,
                            enum_val,
                            node.type_name.token.lineno,
                            node.type_name.token.column,
                        ),
                    )
                self.enum_values[enum_val] = {"type": type_name, "ordinal": i}
            self.enum_types[type_name] = {
                "values": enum_values,
                "size": len(enum_values),
            }

            # 将枚举类型插入符号表
            enum_symbol = EnumTypeSymbol(type_name, enum_values)
            self.current_scope.insert(enum_symbol)
            # 将类型映射添加到映射字典
            self.type_mappings[type_name] = enum_symbol

            # 将枚举值也插入符号表，使它们可以被解析为变量
            for i, enum_val in enumerate(enum_values):
                # 枚举值作为特殊的变量符号插入
                var_symbol = VarSymbol(enum_val, enum_symbol)
                self.current_scope.insert(var_symbol)
        elif isinstance(node.type_def, RecordType):
            # Handle record type definition
            # Process the record type to create a RecordTypeSymbol
            self.visit(node.type_def)
            record_type_symbol = cast(Symbol, self.current_type)

            # Update the record type symbol name to match the type declaration name
            record_type_symbol.name = type_name

            # Insert the record type symbol into the scope
            self.current_scope.insert(record_type_symbol)
            # 将类型映射添加到映射字典
            self.type_mappings[type_name] = record_type_symbol

            # Store the record type information for the interpreter
            # This is a simplified approach - in a real implementation, we would need more detailed info
        else:
            # Handle type aliases and other type definitions
            # First, visit the type definition to ensure any nested types are processed
            self.visit(node.type_def)

            # Check if this is a simple type alias (Type with value attribute)
            if hasattr(node.type_def, "value") and hasattr(node.type_def, "token"):
                original_type_name = node.type_def.value
                original_type_symbol = self.current_scope.lookup(original_type_name)

                if original_type_symbol is None:
                    original_type_symbol = self.current_scope.lookup(str(node.type_def))
                    if original_type_symbol is None:
                        self.error(
                            error_code=ErrorCode.ID_NOT_FOUND, token=node.type_def.token
                        )

                # Create a new type alias symbol
                # For simplicity, we'll create a BuiltinTypeSymbol that points to the original type
                alias_symbol = BuiltinTypeSymbol(type_name)
                alias_symbol.type = (
                    original_type_symbol  # Make it "point to" the actual type
                )

                self.current_scope.insert(alias_symbol)
                # 将类型映射添加到映射字典
                self.type_mappings[type_name] = alias_symbol
            else:
                # For complex types (arrays, strings, etc.), register them with their string representation
                type_name_str = str(node.type_def)
                type_symbol = self.current_scope.lookup(type_name_str)

                if type_symbol:
                    alias_symbol = BuiltinTypeSymbol(type_name)
                    alias_symbol.type = type_symbol
                    self.current_scope.insert(alias_symbol)
                    # 将类型映射添加到映射字典
                    self.type_mappings[type_name] = alias_symbol

    def visit_NoOp(self, node: NoOp) -> None:
        pass

    def visit_WhileStatement(self, node: WhileStatement) -> None:
        self.visit(node.condition)
        self.visit(node.block)

    def visit_ForStatement(self, node: ForStatement) -> None:
        self.visit(node.initialization)
        self.visit(node.bound)
        var_name = node.initialization.left.value
        self.unmodified_vars.append(var_name)
        self.visit(node.block)
        self.unmodified_vars.remove(var_name)

    def _is_enum_type(self, type_symbol):
        """Check if a type symbol is an enum type"""
        return isinstance(type_symbol, EnumTypeSymbol)

    def visit_IfStatement(self, node: IfStatement) -> None:
        self.visit(node.condition)
        self.visit(node.then_branch)
        for branch in node.else_if_branches:
            self.visit(branch)
        if node.else_branch is not None:
            self.visit(node.else_branch)

    def visit_CaseStatement(self, node: CaseStatement) -> None:
        # 访问case表达式
        self.visit(node.case_expr)

        # 获取case表达式的符号和类型
        case_type_name = None
        if isinstance(node.case_expr, Var):
            var_name = node.case_expr.value
            if self.current_scope is None:
                raise SemanticError(
                    error_code=ErrorCode.MISSING_CURRENT_SCOPE,
                    token=node.case_expr.token,
                    message=f"{ErrorCode.MISSING_CURRENT_SCOPE.value} -> {node.case_expr.token}",
                )
            var_symbol = self.current_scope.lookup(var_name)
            if var_symbol is None:
                raise SemanticError(
                    error_code=ErrorCode.SEMANTIC_UNKNOWN_SYMBOL,
                    token=node.case_expr.token,
                    message=f"{ErrorCode.SEMANTIC_UNKNOWN_SYMBOL.value} -> {node.case_expr.token}",
                )
            if var_symbol.type is None:
                raise SemanticError(
                    error_code=ErrorCode.SEMANTIC_UNKNOWN_SYMBOL,
                    token=node.case_expr.token,
                    message=f"{ErrorCode.SEMANTIC_UNKNOWN_SYMBOL.value} -> {node.case_expr.token}",
                )

            # 检查case表达式类型是否支持
            case_type_name = var_symbol.type.name
            if case_type_name not in ["INTEGER", "CHAR", "BOOLEAN"]:
                # 检查是否是枚举类型
                if not isinstance(var_symbol.type, EnumTypeSymbol):
                    self.error(
                        error_code=ErrorCode.SEMANTIC_UNSUPPORTED_TYPE,
                        token=node.case_expr.token,
                    )

        # 收集所有case标签，检查重复
        used_labels = set()

        for case_item in node.case_items:
            for label in case_item.labels:
                label_value = label.value

                # 检查标签类型是否与case表达式匹配（仅当我们有case_type_name时）
                if case_type_name:
                    label_type = self._get_literal_type(label_value)
                    # 如果是枚举类型变量，检查标签是否是该枚举类型的值
                    if isinstance(var_symbol.type, EnumTypeSymbol):
                        # 对于枚举类型，标签必须是该枚举的值之一
                        if label_value not in var_symbol.type.values:
                            self.error(
                                error_code=ErrorCode.SEMANTIC_UNKNOWN_ENUM,
                                token=node.case_expr.token,  # Use the case expression token for error reporting
                            )
                    elif label_type and not self._types_compatible(
                        case_type_name, label_type
                    ):
                        self.error(
                            error_code=ErrorCode.SEMANTIC_INCOMPATIBLE_TYPE,
                            token=node.case_expr.token,  # Use the case expression token for error reporting
                        )

                # 检查重复标签
                if label_value in used_labels:
                    self.error(
                        error_code=ErrorCode.SEMANTIC_DUPLICATE_CASE_LABEL,
                        token=node.case_expr.token,  # Use the case expression token for error reporting
                    )
                used_labels.add(label_value)

            # 访问语句
            self.visit(case_item.statement)

        # 访问else语句（如果存在）
        if node.else_stmt:
            self.visit(node.else_stmt)

    def _get_literal_type(self, value):
        """Get the type name for a literal value"""
        # Check bool first since bool is a subclass of int in Python
        if isinstance(value, bool):
            return "BOOLEAN"
        elif isinstance(value, int):
            return "INTEGER"
        elif isinstance(value, str) and len(value) == 1:
            return "CHAR"
        elif self.enum_values[value]:
            return self.enum_values[value]["type"]
        else:
            return None

    def _types_compatible(self, case_type, label_type):
        """Check if case expression type and label type are compatible"""
        if case_type is None or label_type is None:
            return False
        return case_type == label_type

    def _resolve_type_alias(self, symbol: Symbol) -> Symbol:
        """Follow alias chain until finding the base type symbol."""
        if symbol is None:
            return symbol

        visited = {symbol.name}
        current_symbol = symbol

        while (
            hasattr(current_symbol, "type")
            and current_symbol.type is not None
            and current_symbol.type != current_symbol
        ):
            current_symbol = current_symbol.type

            # Detect alias cycles
            if current_symbol.name in visited:
                # For simplicity, return the symbol as-is if we detect a cycle
                # In a real implementation, we should throw an error
                return symbol

            visited.add(current_symbol.name)

        return current_symbol

    def visit_BinOp(self, node: BinOp) -> None:
        self.visit(node.left)
        self.visit(node.right)

    def visit_Type(self, node: Type):
        return self.current_scope.lookup(node.value)

    def visit_PrimitiveType(self, node: PrimitiveType):
        pass

    def visit_StringType(self, node: StringType):
        if isinstance(node.limit, Num):
            limit = int(node.limit.value)
            self.__string_type_limit = limit
            if self.current_scope is None:
                raise SemanticError(
                    error_code=ErrorCode.MISSING_CURRENT_SCOPE,
                    token=node.token,
                    message=f"{ErrorCode.MISSING_CURRENT_SCOPE.value} -> {node.token}",
                )
            self.current_scope.insert(
                StringTypeSymbol(
                    name=SemanticAnalyzer.string_type_name(limit), limit=int(limit)
                )
            )
        pass

    def visit_ArrayType(self, node: ArrayType) -> None:
        if isinstance(node.element_type, ArrayType):
            self.visit_ArrayType(node.element_type)
        if self.current_scope is None:
            raise SemanticError(
                error_code=ErrorCode.MISSING_CURRENT_SCOPE,
                token=node.token,
                message=f"{ErrorCode.MISSING_CURRENT_SCOPE.value} -> {node.token}",
            )

        # 首先尝试从类型映射中查找
        # 正确处理元素类型名称
        if hasattr(node.element_type, "value"):
            element_type_str = node.element_type.value
        else:
            element_type_str = str(node.element_type)
        element_type_symbol = self.type_mappings.get(element_type_str)

        # 如果映射中没找到，尝试从符号表中查找
        if element_type_symbol is None:
            element_type_symbol = self.current_scope.lookup(element_type_str)

        if element_type_symbol is None:
            element_type_symbol = self.current_scope.lookup(str(node.element_type))

        if element_type_symbol is None:
            raise SemanticError(
                error_code=ErrorCode.SEMANTIC_UNKNOWN_ARRAY_ELEMENT_TYPE,
                token=node.token,
                message=f"{ErrorCode.SEMANTIC_UNKNOWN_ARRAY_ELEMENT_TYPE.value} -> {node.token}",
            )
        type_name = str(node)
        type_symbol = self.current_scope.lookup(type_name)
        if type_symbol is None:
            array_type_symbol = ArrayTypeSymbol(
                name=type_name, element_type=element_type_symbol
            )
            self.current_scope.insert(array_type_symbol)
            # 将类型映射添加到映射字典
            self.type_mappings[type_name] = array_type_symbol

    def visit_RecordType(self, node: RecordType) -> None:
        """访问记录类型定义节点"""
        # 创建记录类型符号
        type_name = f"record_{self._get_record_type_counter()}"
        fields = {}

        # 处理常规字段
        for field in node.fields:
            field_name = field.name.value
            field_type = cast(Symbol, self.visit(field.type_node))

            # 创建字段符号并添加到记录中
            field_symbol = RecordFieldSymbol(field_name, field_type)
            fields[field_name] = field_symbol

        # 处理变体部分（如果存在）
        variant_part_symbol = None
        if node.variant_part:
            variant_part_symbol = self._process_variant_part(node.variant_part, fields)

        # 创建记录类型符号
        record_type_symbol = RecordTypeSymbol(type_name, fields, variant_part_symbol)
        self.current_type = record_type_symbol

    def _process_variant_part(
        self, variant_part: VariantPart, existing_fields: dict[str, Symbol]
    ) -> VariantPartSymbol:
        """处理记录的变体部分"""
        tag_field_name = variant_part.tag_field.value

        # 检查标签字段是否是已定义的枚举类型
        if tag_field_name not in existing_fields:
            self.error(
                error_code=ErrorCode.SEMANTIC_ERROR, token=variant_part.tag_field.token
            )

        tag_field_symbol = cast(Symbol, existing_fields[tag_field_name])
        if not isinstance(tag_field_symbol.type, EnumTypeSymbol):
            self.error(
                error_code=ErrorCode.SEMANTIC_ERROR, token=variant_part.tag_field.token
            )

        # 处理变体情况
        variant_cases = {}
        for variant_case in variant_part.variant_cases:
            # 检查标签值是否有效
            for tag_value in variant_case.tag_values:
                if tag_value not in tag_field_symbol.type.values:
                    self.error(
                        error_code=ErrorCode.SEMANTIC_ERROR,
                        token=variant_part.tag_field.token,
                    )

            # 处理变体字段
            variant_fields = {}
            for field in variant_case.fields:
                field_name = field.name.value
                self.visit(field.type_node)
                field_type = cast(Symbol, self.current_type)

                # 创建字段符号并添加到变体字段中
                field_symbol = RecordFieldSymbol(field_name, field_type)
                variant_fields[field_name] = field_symbol

            # 将变体字段添加到对应的标签值
            for tag_value in variant_case.tag_values:
                variant_cases[tag_value] = variant_fields

        return VariantPartSymbol(tag_field_name, tag_field_symbol.type, variant_cases)

    def _get_record_type_counter(self) -> int:
        """获取记录类型计数器，用于生成唯一的记录类型名称"""
        if not hasattr(self, "_record_type_counter"):
            self._record_type_counter = 0
        self._record_type_counter += 1
        return self._record_type_counter

    def visit_VarDecl(self, node: VarDecl) -> None:
        type_name = node.type_node.value
        if isinstance(node.type_node, ArrayType):
            self.visit(node.type_node)
            type_name = str(node.type_node)
        elif isinstance(node.type_node, StringType):
            self.visit(node.type_node)
            # type_name = SemanticAnalyzer.string_type_name(size=self.__string_type_limit)
        elif isinstance(node.type_node, RecordType):
            self.visit(node.type_node)
            # For record types, we'll use the type symbol directly
            type_symbol = self.current_type
        if self.current_scope is None:
            raise SemanticError(
                error_code=ErrorCode.MISSING_CURRENT_SCOPE,
                token=node.var_node.token,
                message=f"{ErrorCode.MISSING_CURRENT_SCOPE.value} -> {node.var_node.token}",
            )
        if not isinstance(node.type_node, RecordType):
            type_symbol = self.current_scope.lookup(type_name)

        # Resolve type aliases to get the base type
        if type_symbol is not None:
            resolved_type_symbol = self._resolve_type_alias(type_symbol)
        else:
            resolved_type_symbol = type_symbol

        # We have all the information we need to create a variable symbol.
        # Create the symbol and insert it into the symbol table.
        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name, resolved_type_symbol)

        # Signal an error if the table already has a symbol
        # with the same name
        if self.current_scope.lookup(var_name, current_scope_only=True):
            self.error(
                error_code=ErrorCode.DUPLICATE_ID,
                token=node.var_node.token,
            )

        self.current_scope.insert(var_symbol)
        # Set the resolved type symbol in the node for the interpreter
        node.type_symbol = resolved_type_symbol

    def visit_Assign(self, node: Assign) -> None:
        # right-hand side
        self.visit(node.right)
        # left-hand side
        if isinstance(node.left, Var):
            if node.left.value in self.unmodified_vars:
                self.error(ErrorCode.MODIFY_LOOP_VAR_NOT_ALLOW, token=node.left.token)
            self.visit(node.left)
            if self.current_scope is None:
                raise SemanticError(
                    error_code=ErrorCode.MISSING_CURRENT_SCOPE,
                    token=node.left.token,
                    message=f"{ErrorCode.MISSING_CURRENT_SCOPE.value} -> {node.left.token}",
                )
            var_symbol = self.current_scope.lookup(node.left.value)
            if var_symbol is None:
                raise SemanticError(
                    error_code=ErrorCode.SEMANTIC_UNKNOWN_SYMBOL,
                    token=node.left.token,
                    message=f"{ErrorCode.SEMANTIC_UNKNOWN_SYMBOL.value} -> {node.left.token}",
                )
            if var_symbol.type is None:
                raise SemanticError(
                    error_code=ErrorCode.SEMANTIC_UNKNOWN_SYMBOL,
                    token=node.left.token,
                    message=f"{ErrorCode.SEMANTIC_UNKNOWN_SYMBOL.value} -> {node.left.token}",
                )
        elif isinstance(node.left, AccessExpression):
            # Handle access expressions (array/member access)
            self.visit(node.left)
        else:
            self.visit(node.left)

        # Additional validation for specific cases
        if isinstance(node.left, Var):
            if self.current_scope is None:
                raise SemanticError(
                    error_code=ErrorCode.MISSING_CURRENT_SCOPE,
                    token=node.left.token,
                    message=f"{ErrorCode.MISSING_CURRENT_SCOPE.value} -> {node.left.token}",
                )
            var_symbol = self.current_scope.lookup(node.left.value)
            if var_symbol and var_symbol.type:
                if isinstance(var_symbol.type, StringTypeSymbol):
                    # string_size = var_symbol.type.limit
                    # if isinstance(node.right, String):
                    #     string_value = node.right.value
                    #     if len(string_value) > string_size:
                    #         message = f"Warning: String literal has more characters[{len(string_value)}] than short string length[{string_size}]"
                    #         SpiUtil.print_w(message=message)
                    pass
                elif var_symbol.type.name == "CHAR":
                    # Validate character assignment
                    if isinstance(node.right, String):
                        string_value = node.right.value
                        if len(string_value) > 1:
                            raise SemanticError(
                                error_code=ErrorCode.SEMANTIC_CHAR_TOO_MANY_CHARS,
                                token=node.right.token,
                                message=f"String literal has too many characters for CHAR variable: '{string_value}'",
                            )

    def visit_Var(self, node: Var) -> None:
        var_name = node.value
        if self.current_scope is None:
            self.error(error_code=ErrorCode.NULL_POINTER, token=node.token)
            return

        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)
            return

    def visit_AccessExpression(self, node: AccessExpression) -> None:
        # Visit the base expression
        self.visit(node.base)

        # Validate each suffix
        for suffix in node.suffixes:
            if isinstance(suffix, IndexSuffix):
                # Check if base supports indexing (arrays, strings)
                if isinstance(node.base, Var):
                    var_name = node.base.value
                    if self.current_scope is None:
                        self.error(
                            error_code=ErrorCode.NULL_POINTER, token=node.base.token
                        )
                        return

                    var_symbol = self.current_scope.lookup(var_name)
                    if var_symbol is None:
                        self.error(
                            error_code=ErrorCode.ID_NOT_FOUND, token=node.base.token
                        )
                        return

                    # TODO: Add type checking to ensure variable supports indexing

                # Visit the index expression
                self.visit(suffix.index)

            elif isinstance(suffix, MemberSuffix):
                # Member access - check if base supports member access (records, enums)
                # For records, we need to check if the field exists
                # For now, we'll allow member access without strict validation
                pass  # Allow member access for records

    def visit_IndexSuffix(self, node: IndexSuffix) -> None:
        self.visit(node.index)

    def visit_MemberSuffix(self, node: MemberSuffix) -> None:
        # Nothing to validate for the member token itself
        pass

    def visit_Num(self, node: Num) -> None:
        pass

    def visit_Bool(self, node: Bool) -> None:
        pass

    def visit_String(self, node: String) -> None:
        pass

    def visit_Char(self, node: Char) -> None:
        # Validate character value
        char_value = node.value
        if len(char_value) > 1:
            raise SemanticError(
                error_code=ErrorCode.SEMANTIC_CHAR_TOO_MANY_CHARS,
                token=node.token,
                message=f"Character literal has too many characters: '{char_value}'",
            )

        # Validate ASCII range for character constants parsed from #\d\d format
        if node.token.type == TokenType.CHAR_CONST:
            ascii_value = ord(char_value) if char_value else 0
            if ascii_value < 0 or ascii_value > 255:
                raise SemanticError(
                    error_code=ErrorCode.SEMANTIC_CHAR_INVALID_ASCII,
                    token=node.token,
                    message=f"Character ASCII value {ascii_value} is out of range (0-255)",
                )

    def visit_UnaryOp(self, node: UnaryOp) -> None:
        pass

    def visit_ProcedureDecl(self, node: ProcedureDecl) -> None:
        proc_name = node.proc_name
        proc_symbol = ProcedureSymbol(proc_name)
        if self.current_scope is None:
            raise SemanticError(
                error_code=ErrorCode.MISSING_CURRENT_SCOPE,
                token=None,
                message=f"{ErrorCode.MISSING_CURRENT_SCOPE.value}",
            )
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
            param_type = self.current_scope.lookup(param.type_node.value)
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
        proc_symbol = self.current_scope.lookup(node.proc_name)
        # Note: proc_symbol field removed from AST node for interpreter decoupling

    def visit_FunctionDecl(self, node: FunctionDecl) -> None:
        func_name = node.func_name
        return_type = node.return_type
        func_symbol = FunctionSymbol(func_name, return_type)
        if self.current_scope is None:
            raise SemanticError(
                error_code=ErrorCode.MISSING_CURRENT_SCOPE,
                token=None,
                message=f"{ErrorCode.MISSING_CURRENT_SCOPE.value}",
            )
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
            func_name, self.current_scope.lookup(return_type.value)
        )
        self.current_scope.insert(return_var_symbol)
        func_symbol.formal_params.append(return_var_symbol)

        # Insert parameters into the function scope
        for param in node.formal_params:
            param_type = self.current_scope.lookup(param.type_node.value)
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
        func_symbol = self.current_scope.lookup(node.func_name)
        # Note: func_symbol field removed from AST node for interpreter decoupling
