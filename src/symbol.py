from __future__ import annotations
from src.spi_ast import *
from src.visibility import VisibilityLevel


class Symbol:

    def __init__(self, name: str, type: Symbol | None = None, visibility: VisibilityLevel = VisibilityLevel.PRIVATE) -> None:
        self.name = name
        self.type = type
        self.scope_level: int = 0
        self.visibility = visibility


class VarSymbol(Symbol):
    def __init__(self, name: str, type: Symbol | None, visibility: VisibilityLevel = VisibilityLevel.PRIVATE) -> None:
        super().__init__(name, type, visibility)

    def __str__(self) -> str:
        return "<{class_name}(name='{name}', type='{type}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            type=self.type,
        )

    def to_FieldSymbol(self) -> "FieldSymbol":
        field_symbol = FieldSymbol(name=self.name, type=self.type, visibility=self.visibility)
        return field_symbol

    __repr__ = __str__


class ConstSymbol(Symbol):
    def __init__(self, name: str, type: Symbol | None, const_type: ConstType, visibility: VisibilityLevel = VisibilityLevel.PRIVATE) -> None:
        super().__init__(name, type, visibility)
        self.const_type = const_type

    def __str__(self) -> str:
        return "<{class_name}(name='{name}', const_type='{const_type}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            const_type=self.const_type,
        )

    __repr__ = __str__


class BuiltinTypeSymbol(Symbol):
    def __init__(self, name: str, visibility: VisibilityLevel = VisibilityLevel.INTERFACE) -> None:
        super().__init__(name, visibility=visibility)

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return "<{class_name}(name='{name}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
        )


class StringTypeSymbol(Symbol):
    def __init__(self, name: str, limit: int = 255, visibility: VisibilityLevel = VisibilityLevel.INTERFACE) -> None:
        super().__init__(name, visibility=visibility)
        self.limit = limit

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return "<{class_name}(name='{name}', limit='{limit}')>".format(
            class_name=self.__class__.__name__, name=self.name, limit=self.limit
        )


class ArrayTypeSymbol(Symbol):
    def __init__(self, name: str, element_type: Symbol, visibility: VisibilityLevel = VisibilityLevel.PRIVATE) -> None:
        super().__init__(name, visibility=visibility)
        self.element_type = element_type

    def __str__(self) -> str:
        return "{name}[]".format(name=self.name)

    def __repr__(self) -> str:
        return "<{class_name}[{element_type_name}](name='{name}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            element_type_name=self.element_type.name,
        )


class ProcedureSymbol(Symbol):
    def __init__(self, name: str, formal_params: list[Symbol] | None = None, visibility: VisibilityLevel = VisibilityLevel.PRIVATE) -> None:
        super().__init__(name, visibility=visibility)
        # a list of VarSymbol objects
        self.formal_params: list[Symbol] = (
            [] if formal_params is None else formal_params
        )
        # a reference to procedure's body (AST sub-tree)
        self.block_ast: Block | None = None

    def __str__(self) -> str:
        return "<{class_name}(name={name}, parameters={params})>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            params=self.formal_params,
        )

    __repr__ = __str__


class BuiltinProcedureSymbol(Symbol):
    def __init__(self, name: str, output_params: list[Symbol] | None = None, visibility: VisibilityLevel = VisibilityLevel.INTERFACE) -> None:
        super().__init__(name, visibility=visibility)
        # a list of VarSymbol objects
        self.output_params: list[Symbol] = (
            [] if output_params is None else output_params
        )

    def __str__(self) -> str:
        return "<{class_name}(name={name}, parameters={params})>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            params=self.output_params,
        )

    __repr__ = __str__


class FunctionSymbol(Symbol):
    def __init__(
        self, name: str, return_type: Type, formal_params: list[Symbol] | None = None, visibility: VisibilityLevel = VisibilityLevel.PRIVATE
    ) -> None:
        super().__init__(name, visibility=visibility)
        # a list of VarSymbol objects
        self.formal_params: list[Symbol] = (
            [] if formal_params is None else formal_params
        )
        self.return_type = return_type
        # a reference to procedure's body (AST sub-tree)
        self.block_ast: Block | None = None

    def __str__(self) -> str:
        return "<{class_name}(name={name},return_type={return_type} parameters={params})>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            return_type=self.return_type,
            params=self.formal_params,
        )

    __repr__ = __str__


class EnumSymbol(Symbol):
    def __init__(
        self,
        name: str,
        entries: dict[str, int] = {},
        visibility: VisibilityLevel = VisibilityLevel.PRIVATE
    ) -> None:
        super().__init__(name, None, visibility)
        self.entries = entries

    def __str__(self) -> str:
        return "<{class_name}(name='{name}', entries = {entries}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            entries=self.entries,
        )

    __repr__ = __str__


class FieldSymbol(Symbol):
    def __init__(self, name: str, type: Symbol | None, visibility: VisibilityLevel = VisibilityLevel.PRIVATE) -> None:
        super().__init__(name, type, visibility)

    def __str__(self) -> str:
        return "<{class_name}(name='{name}', type='{type}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            type=self.type,
        )

    def to_VarSymbol(self) -> VarSymbol:
        var_symbol = VarSymbol(name=self.name, type=self.type, visibility=self.visibility)
        return var_symbol

    __repr__ = __str__


class MethodSymbol(Symbol):
    def __init__(
        self,
        name: str,
        return_type: Type | None,
        formal_params: list[Symbol],
        method_type: MethodType,
        visibility: VisibilityLevel = VisibilityLevel.PRIVATE
    ) -> None:
        super().__init__(name, visibility=visibility)
        # a list of VarSymbol objects
        self.formal_params: list[Symbol] = formal_params
        self.return_type = return_type
        self.method_type = method_type
        # a reference to method's body (AST sub-tree)
        self.block_ast: Block | None = None

    def __str__(self) -> str:
        return "<{class_name}(name={name},return_type={return_type} parameters={params})>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            return_type=self.return_type,
            params=self.formal_params,
        )

    __repr__ = __str__


class RecordSymbol(Symbol):
    def __init__(
        self,
        name: str,
        fields: dict[str, FieldSymbol] = {},
        visibility: VisibilityLevel = VisibilityLevel.PRIVATE
    ) -> None:
        super().__init__(name, None, visibility)
        self.fields = fields

    def __str__(self) -> str:
        return "<{class_name}(name='{name}', fields='{fields}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            fields=self.fields,
        )

    __repr__ = __str__


class ClassSymbol(Symbol):
    def __init__(
        self,
        name: str,
        fields: dict[str, FieldSymbol] = {},
        methods: dict[str, MethodSymbol] = {},
        visibility: VisibilityLevel = VisibilityLevel.PRIVATE
    ) -> None:
        super().__init__(name, None, visibility)
        self.fields = fields
        self.methods = methods

    def __str__(self) -> str:
        return "<{class_name}(name='{name}', fields='{fields}',  methods='{methods}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            fields=self.fields,
            methods=self.methods,
        )

    __repr__ = __str__


class BuiltinFunctionSymbol(Symbol):
    def __init__(
        self, name: str, return_type: Type, formal_params: list[Symbol] | None = None, visibility: VisibilityLevel = VisibilityLevel.INTERFACE
    ) -> None:
        super().__init__(name, visibility=visibility)
        # a list of VarSymbol objects
        self.formal_params: list[Symbol] = (
            [] if formal_params is None else formal_params
        )
        self.return_type = return_type
        # a reference to procedure's body (AST sub-tree)
        self.block_ast: Block | None = None

    def __str__(self) -> str:
        return "<{class_name}(name={name},return_type={return_type} parameters={params})>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            return_type=self.return_type,
            params=self.formal_params,
        )

    __repr__ = __str__
