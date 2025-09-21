"""SPI - Simple Pascal Interpreter. Part 19"""

from __future__ import annotations

from abc import ABC, abstractmethod
from spi.ast import Block, Type

###############################################################################
#                                                                             #
#  SYMBOL                                                                    #
#                                                                             #
###############################################################################


class Symbol:
    def __init__(self, name: str, type: Symbol | None = None) -> None:
        self.name = name
        self.type = type
        self.scope_level: int = 0


###############################################################################
#                                                                             #
#  TYPE SYMBOL INFRASTRUCTURE                                                #
#                                                                             #
###############################################################################


class TypeSymbol(Symbol, ABC):
    """Abstract base class for all type symbols"""
    
    def __init__(self, name: str):
        super().__init__(name)
    
    @abstractmethod
    def is_compatible_with(self, other: 'TypeSymbol') -> bool:
        """Check if this type is compatible with another type"""
        pass
    
    @abstractmethod
    def can_assign_from(self, other: 'TypeSymbol') -> bool:
        """Check if a value of other type can be assigned to this type"""
        pass
    
    @abstractmethod
    def get_result_type(self, operation: str, other: 'TypeSymbol') -> 'TypeSymbol':
        """Get the result type of an operation with another type"""
        pass
    
    def resolve_final_type(self) -> 'TypeSymbol':
        """Resolve through type alias chain to get the final concrete type"""
        return self


class NeverSymbol(TypeSymbol):
    """Special symbol representing 'never' type (replacement for None)"""
    
    _instance = None
    
    def __new__(cls):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance
    
    def __init__(self):
        if not hasattr(self, '_initialized'):
            super().__init__("NEVER")
            self._initialized = True
    
    def is_compatible_with(self, other: TypeSymbol) -> bool:
        """Never type is compatible with nothing"""
        return False
    
    def can_assign_from(self, other: TypeSymbol) -> bool:
        """Never type cannot be assigned from anything"""
        return False
    
    def get_result_type(self, operation: str, other: TypeSymbol) -> TypeSymbol:
        """Never type operations always result in Never type"""
        return self
    
    def __str__(self) -> str:
        return "NEVER"
    
    def __repr__(self) -> str:
        return "<NeverSymbol(name='NEVER')>"


# Singleton instance
NEVER_SYMBOL = NeverSymbol()


class PrimitiveTypeSymbol(TypeSymbol, ABC):
    """Abstract base class for primitive types (INTEGER, REAL, BOOLEAN, CHAR)"""
    
    def __init__(self, name: str):
        super().__init__(name)
    
    def __str__(self) -> str:
        return self.name
    
    def __repr__(self) -> str:
        return "<{class_name}(name='{name}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
        )


class VarSymbol(Symbol):
    def __init__(self, name: str, type: Symbol | None, is_mutable: bool = True) -> None:
        super().__init__(name, type or NEVER_SYMBOL)
        self.is_mutable = is_mutable
        self.is_initialized = False  # Track if const has been initialized
    
    def can_modify(self) -> bool:
        """Check if this variable can be modified"""
        if not self.is_mutable:
            # Const variables can only be modified if not yet initialized
            return not self.is_initialized
        return True
    
    def mark_initialized(self):
        """Mark const variable as initialized"""
        self.is_initialized = True
    
    @property
    def is_const(self) -> bool:
        """Check if this is a const variable"""
        return not self.is_mutable

    def __str__(self) -> str:
        return "<{class_name}(name='{name}', type='{type}', mutable={mutable})>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            type=self.type,
            mutable=self.is_mutable,
        )

    __repr__ = __str__


class BuiltinTypeSymbol(Symbol):
    def __init__(self, name: str) -> None:
        super().__init__(name)

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return "<{class_name}(name='{name}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
        )


class StringTypeSymbol(Symbol):
    def __init__(self, name: str, limit: int = 255) -> None:
        super().__init__(name)
        self.limit = limit

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return "<{class_name}(name='{name}', limit='{limit}')>".format(
            class_name=self.__class__.__name__, name=self.name, limit=self.limit
        )


class ArrayTypeSymbol(Symbol):
    def __init__(self, name: str, element_type: Symbol) -> None:
        super().__init__(name)
        self.element_type = element_type

    def __str__(self) -> str:
        return "{name}[]".format(name=self.name)

    def __repr__(self) -> str:
        return "<{class_name}[{element_type_name}](name='{name}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            element_type_name=self.element_type.name,
        )


class EnumTypeSymbol(Symbol):
    def __init__(self, name: str, values: list[str]) -> None:
        super().__init__(name)
        self.values = values
        # Create a mapping from value name to ordinal
        self.value_ordinals = {value: i for i, value in enumerate(values)}

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return "<{class_name}(name='{name}', values={values})>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            values=self.values,
        )


class RecordTypeSymbol(Symbol):
    """表示记录类型符号"""

    def __init__(
        self,
        name: str,
        fields: dict[str, Symbol],
        variant_part: VariantPartSymbol | None = None,
    ):
        super().__init__(name)
        self.fields = fields  # 字段名到字段符号的映射
        self.variant_part = variant_part  # 可选的变体部分符号


class RecordFieldSymbol(Symbol):
    """表示记录字段符号"""

    def __init__(self, name: str, type_symbol: Symbol):
        super().__init__(name)
        self.type = type_symbol


class VariantPartSymbol:
    """表示记录变体部分的符号"""

    def __init__(
        self,
        tag_field: str,
        tag_type: Symbol,
        variant_cases: dict[str, dict[str, Symbol]],
    ):
        self.tag_field = tag_field  # 标签字段名
        self.tag_type = tag_type  # 标签字段类型（必须是枚举类型）
        self.variant_cases = variant_cases  # 标签值到变体字段符号的映射


class ProcedureSymbol(Symbol):
    def __init__(self, name: str, formal_params: list[Symbol] | None = None) -> None:
        super().__init__(name)
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
    def __init__(self, name: str, output_params: list[Symbol] | None = None) -> None:
        super().__init__(name)
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
        self, name: str, return_type: Type, formal_params: list[Symbol] | None = None
    ) -> None:
        super().__init__(name)
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


class BuiltinFunctionSymbol(Symbol):
    def __init__(
        self, name: str, return_type: Type, formal_params: list[Symbol] | None = None
    ) -> None:
        super().__init__(name)
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
