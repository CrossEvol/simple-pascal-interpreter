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
    def is_compatible_with(self, other: "TypeSymbol") -> bool:
        """Check if this type is compatible with another type"""
        pass

    @abstractmethod
    def can_assign_from(self, other: "TypeSymbol") -> bool:
        """Check if a value of other type can be assigned to this type"""
        pass

    @abstractmethod
    def get_result_type(self, operation: str, other: "TypeSymbol") -> "TypeSymbol":
        """Get the result type of an operation with another type"""
        pass

    def resolve_final_type(self) -> "TypeSymbol":
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
        if not hasattr(self, "_initialized"):
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

# Forward declarations for type symbols (will be defined after all classes)
INTEGER_TYPE_SYMBOL = None
REAL_TYPE_SYMBOL = None
BOOLEAN_TYPE_SYMBOL = None
CHAR_TYPE_SYMBOL = None
STRING_TYPE_SYMBOL = None


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


class IntegerTypeSymbol(PrimitiveTypeSymbol):
    """Type symbol for INTEGER type with arithmetic operations"""

    def __init__(self):
        super().__init__("INTEGER")

    def is_compatible_with(self, other: "TypeSymbol") -> bool:
        """INTEGER is compatible with INTEGER and REAL"""
        if isinstance(other, NeverSymbol):
            return False
        return isinstance(other, (IntegerTypeSymbol, RealTypeSymbol))

    def can_assign_from(self, other: "TypeSymbol") -> bool:
        """INTEGER can be assigned from INTEGER only"""
        if isinstance(other, NeverSymbol):
            return False
        return isinstance(other, IntegerTypeSymbol)

    def get_result_type(self, operation: str, other: "TypeSymbol") -> "TypeSymbol":
        """Get result type for operations with INTEGER"""
        if isinstance(other, NeverSymbol):
            return NEVER_SYMBOL

        if operation in ["+", "-", "*"]:
            if isinstance(other, IntegerTypeSymbol):
                return INTEGER_TYPE_SYMBOL
            elif isinstance(other, RealTypeSymbol):
                return REAL_TYPE_SYMBOL
        elif operation == "/":
            # Division always returns REAL in Pascal
            if isinstance(other, (IntegerTypeSymbol, RealTypeSymbol)):
                return REAL_TYPE_SYMBOL
        elif operation in ["=", "<>", "<", ">", "<=", ">="]:
            if isinstance(other, (IntegerTypeSymbol, RealTypeSymbol)):
                return BOOLEAN_TYPE_SYMBOL

        return NEVER_SYMBOL

    def __add__(self, other: "TypeSymbol") -> "TypeSymbol":
        """INTEGER + INTEGER → INTEGER, INTEGER + REAL → REAL"""
        return self.get_result_type("+", other)

    def __sub__(self, other: "TypeSymbol") -> "TypeSymbol":
        """INTEGER - INTEGER → INTEGER, INTEGER - REAL → REAL"""
        return self.get_result_type("-", other)

    def __mul__(self, other: "TypeSymbol") -> "TypeSymbol":
        """INTEGER * INTEGER → INTEGER, INTEGER * REAL → REAL"""
        return self.get_result_type("*", other)

    def __truediv__(self, other: "TypeSymbol") -> "TypeSymbol":
        """INTEGER / INTEGER → REAL, INTEGER / REAL → REAL"""
        return self.get_result_type("/", other)


class RealTypeSymbol(PrimitiveTypeSymbol):
    """Type symbol for REAL type with arithmetic operations"""

    def __init__(self):
        super().__init__("REAL")

    def is_compatible_with(self, other: "TypeSymbol") -> bool:
        """REAL is compatible with INTEGER and REAL"""
        if isinstance(other, NeverSymbol):
            return False
        return isinstance(other, (IntegerTypeSymbol, RealTypeSymbol))

    def can_assign_from(self, other: "TypeSymbol") -> bool:
        """REAL can be assigned from INTEGER and REAL"""
        if isinstance(other, NeverSymbol):
            return False
        return isinstance(other, (IntegerTypeSymbol, RealTypeSymbol))

    def get_result_type(self, operation: str, other: "TypeSymbol") -> "TypeSymbol":
        """Get result type for operations with REAL"""
        if isinstance(other, NeverSymbol):
            return NEVER_SYMBOL

        if operation in ["+", "-", "*", "/"]:
            if isinstance(other, (IntegerTypeSymbol, RealTypeSymbol)):
                return REAL_TYPE_SYMBOL
        elif operation in ["=", "<>", "<", ">", "<=", ">="]:
            if isinstance(other, (IntegerTypeSymbol, RealTypeSymbol)):
                return BOOLEAN_TYPE_SYMBOL

        return NEVER_SYMBOL

    def __add__(self, other: "TypeSymbol") -> "TypeSymbol":
        """REAL + INTEGER → REAL, REAL + REAL → REAL"""
        return self.get_result_type("+", other)

    def __sub__(self, other: "TypeSymbol") -> "TypeSymbol":
        """REAL - INTEGER → REAL, REAL - REAL → REAL"""
        return self.get_result_type("-", other)

    def __mul__(self, other: "TypeSymbol") -> "TypeSymbol":
        """REAL * INTEGER → REAL, REAL * REAL → REAL"""
        return self.get_result_type("*", other)

    def __truediv__(self, other: "TypeSymbol") -> "TypeSymbol":
        """REAL / INTEGER → REAL, REAL / REAL → REAL"""
        return self.get_result_type("/", other)


class BooleanTypeSymbol(PrimitiveTypeSymbol):
    """Type symbol for BOOLEAN type with logical operations"""

    def __init__(self):
        super().__init__("BOOLEAN")

    def is_compatible_with(self, other: "TypeSymbol") -> bool:
        """BOOLEAN is only compatible with BOOLEAN"""
        if isinstance(other, NeverSymbol):
            return False
        return isinstance(other, BooleanTypeSymbol)

    def can_assign_from(self, other: "TypeSymbol") -> bool:
        """BOOLEAN can only be assigned from BOOLEAN"""
        if isinstance(other, NeverSymbol):
            return False
        return isinstance(other, BooleanTypeSymbol)

    def get_result_type(self, operation: str, other: "TypeSymbol") -> "TypeSymbol":
        """Get result type for operations with BOOLEAN"""
        if isinstance(other, NeverSymbol):
            return NEVER_SYMBOL

        if operation in ["AND", "OR"]:
            if isinstance(other, BooleanTypeSymbol):
                return BOOLEAN_TYPE_SYMBOL
        elif operation == "NOT":
            # NOT is unary operation, other should be None or ignored
            return BOOLEAN_TYPE_SYMBOL
        elif operation in ["=", "<>"]:
            if isinstance(other, BooleanTypeSymbol):
                return BOOLEAN_TYPE_SYMBOL

        return NEVER_SYMBOL

    def __and__(self, other: "TypeSymbol") -> "TypeSymbol":
        """BOOLEAN AND BOOLEAN → BOOLEAN"""
        return self.get_result_type("AND", other)

    def __or__(self, other: "TypeSymbol") -> "TypeSymbol":
        """BOOLEAN OR BOOLEAN → BOOLEAN"""
        return self.get_result_type("OR", other)

    def logical_not(self) -> "TypeSymbol":
        """NOT BOOLEAN → BOOLEAN"""
        return self.get_result_type("NOT", self)


class CharTypeSymbol(PrimitiveTypeSymbol):
    """Type symbol for CHAR type with comparison operations"""

    def __init__(self):
        super().__init__("CHAR")

    def is_compatible_with(self, other: "TypeSymbol") -> bool:
        """CHAR is compatible with CHAR and STRING for some operations"""
        if isinstance(other, NeverSymbol):
            return False
        return isinstance(other, (CharTypeSymbol, StringTypeSymbol))

    def can_assign_from(self, other: "TypeSymbol") -> bool:
        """CHAR can only be assigned from CHAR"""
        if isinstance(other, NeverSymbol):
            return False
        return isinstance(other, CharTypeSymbol)

    def get_result_type(self, operation: str, other: "TypeSymbol") -> "TypeSymbol":
        """Get result type for operations with CHAR"""
        if isinstance(other, NeverSymbol):
            return NEVER_SYMBOL

        if operation in ["=", "<>", "<", ">", "<=", ">="]:
            if isinstance(other, CharTypeSymbol):
                return BOOLEAN_TYPE_SYMBOL
        elif operation == "+":
            # CHAR + CHAR could be string concatenation in some contexts
            if isinstance(other, (CharTypeSymbol, StringTypeSymbol)):
                return STRING_TYPE_SYMBOL

        return NEVER_SYMBOL

    def __eq__(self, other: "TypeSymbol") -> "TypeSymbol":
        """CHAR = CHAR → BOOLEAN"""
        return self.get_result_type("=", other)

    def __ne__(self, other: "TypeSymbol") -> "TypeSymbol":
        """CHAR <> CHAR → BOOLEAN"""
        return self.get_result_type("<>", other)

    def __lt__(self, other: "TypeSymbol") -> "TypeSymbol":
        """CHAR < CHAR → BOOLEAN"""
        return self.get_result_type("<", other)

    def __le__(self, other: "TypeSymbol") -> "TypeSymbol":
        """CHAR <= CHAR → BOOLEAN"""
        return self.get_result_type("<=", other)

    def __gt__(self, other: "TypeSymbol") -> "TypeSymbol":
        """CHAR > CHAR → BOOLEAN"""
        return self.get_result_type(">", other)

    def __ge__(self, other: "TypeSymbol") -> "TypeSymbol":
        """CHAR >= CHAR → BOOLEAN"""
        return self.get_result_type(">=", other)

    def __add__(self, other: "TypeSymbol") -> "TypeSymbol":
        """CHAR + CHAR/STRING → STRING (concatenation)"""
        return self.get_result_type("+", other)


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


class StringTypeSymbol(TypeSymbol):
    def __init__(self, name: str, limit: int = 255) -> None:
        super().__init__(name)
        self.limit = limit

    def is_compatible_with(self, other: "TypeSymbol") -> bool:
        """STRING is compatible with STRING and CHAR for some operations"""
        if isinstance(other, NeverSymbol):
            return False
        return isinstance(other, (StringTypeSymbol, CharTypeSymbol))

    def can_assign_from(self, other: "TypeSymbol") -> bool:
        """STRING can be assigned from STRING and CHAR"""
        if isinstance(other, NeverSymbol):
            return False
        return isinstance(other, (StringTypeSymbol, CharTypeSymbol))

    def get_result_type(self, operation: str, other: "TypeSymbol") -> "TypeSymbol":
        """Get result type for operations with STRING"""
        if isinstance(other, NeverSymbol):
            return NEVER_SYMBOL

        if operation == "+":
            # String concatenation with STRING or CHAR
            if isinstance(other, (StringTypeSymbol, CharTypeSymbol)):
                return STRING_TYPE_SYMBOL
        elif operation in ["=", "<>", "<", ">", "<=", ">="]:
            # String comparison operations
            if isinstance(other, StringTypeSymbol):
                return BOOLEAN_TYPE_SYMBOL

        return NEVER_SYMBOL

    def __add__(self, other: "TypeSymbol") -> "TypeSymbol":
        """STRING + STRING/CHAR → STRING (concatenation)"""
        return self.get_result_type("+", other)

    def __eq__(self, other: "TypeSymbol") -> "TypeSymbol":
        """STRING = STRING → BOOLEAN"""
        return self.get_result_type("=", other)

    def __ne__(self, other: "TypeSymbol") -> "TypeSymbol":
        """STRING <> STRING → BOOLEAN"""
        return self.get_result_type("<>", other)

    def __lt__(self, other: "TypeSymbol") -> "TypeSymbol":
        """STRING < STRING → BOOLEAN"""
        return self.get_result_type("<", other)

    def __le__(self, other: "TypeSymbol") -> "TypeSymbol":
        """STRING <= STRING → BOOLEAN"""
        return self.get_result_type("<=", other)

    def __gt__(self, other: "TypeSymbol") -> "TypeSymbol":
        """STRING > STRING → BOOLEAN"""
        return self.get_result_type(">", other)

    def __ge__(self, other: "TypeSymbol") -> "TypeSymbol":
        """STRING >= STRING → BOOLEAN"""
        return self.get_result_type(">=", other)

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return "<{class_name}(name='{name}', limit='{limit}')>".format(
            class_name=self.__class__.__name__, name=self.name, limit=self.limit
        )


class ArrayTypeSymbol(TypeSymbol):
    def __init__(self, name: str, element_type: Symbol) -> None:
        super().__init__(name)
        self.element_type = element_type

    def is_compatible_with(self, other: "TypeSymbol") -> bool:
        """Arrays are compatible if they have the same element type"""
        if isinstance(other, NeverSymbol):
            return False
        if not isinstance(other, ArrayTypeSymbol):
            return False

        # Check element type compatibility
        if isinstance(self.element_type, TypeSymbol) and isinstance(
            other.element_type, TypeSymbol
        ):
            return self.element_type.is_compatible_with(other.element_type)

        # Fallback to name comparison for non-TypeSymbol element types
        return self.element_type.name == other.element_type.name

    def can_assign_from(self, other: "TypeSymbol") -> bool:
        """Arrays can be assigned from arrays with compatible element types"""
        if isinstance(other, NeverSymbol):
            return False
        if not isinstance(other, ArrayTypeSymbol):
            return False

        # Check element type assignment compatibility
        if isinstance(self.element_type, TypeSymbol) and isinstance(
            other.element_type, TypeSymbol
        ):
            return self.element_type.can_assign_from(other.element_type)

        # Fallback to name comparison for non-TypeSymbol element types
        return self.element_type.name == other.element_type.name

    def get_result_type(self, operation: str, other: "TypeSymbol") -> "TypeSymbol":
        """Get result type for operations with arrays"""
        if isinstance(other, NeverSymbol):
            return NEVER_SYMBOL

        if operation in ["=", "<>"]:
            # Array comparison operations
            if isinstance(other, ArrayTypeSymbol) and self.is_compatible_with(other):
                return BOOLEAN_TYPE_SYMBOL

        # Arrays don't support arithmetic operations
        return NEVER_SYMBOL

    def get_element_type_compatibility(self, element_type: "TypeSymbol") -> bool:
        """Check if the given type is compatible with this array's element type"""
        if isinstance(self.element_type, TypeSymbol):
            return self.element_type.is_compatible_with(element_type)

        # Fallback for non-TypeSymbol element types
        return (
            self.element_type.name == element_type.name
            if hasattr(element_type, "name")
            else False
        )

    def can_assign_element_from(self, element_type: "TypeSymbol") -> bool:
        """Check if a value of the given type can be assigned to this array's elements"""
        if isinstance(self.element_type, TypeSymbol):
            return self.element_type.can_assign_from(element_type)

        # Fallback for non-TypeSymbol element types
        return (
            self.element_type.name == element_type.name
            if hasattr(element_type, "name")
            else False
        )

    def __eq__(self, other: "TypeSymbol") -> "TypeSymbol":
        """Array = Array → BOOLEAN (if compatible)"""
        return self.get_result_type("=", other)

    def __ne__(self, other: "TypeSymbol") -> "TypeSymbol":
        """Array <> Array → BOOLEAN (if compatible)"""
        return self.get_result_type("<>", other)

    def __str__(self) -> str:
        return "{name}[]".format(name=self.name)

    def __repr__(self) -> str:
        return "<{class_name}[{element_type_name}](name='{name}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            element_type_name=self.element_type.name,
        )


class EnumTypeSymbol(TypeSymbol):
    def __init__(self, name: str, values: list[str]) -> None:
        super().__init__(name)
        self.values = values
        # Create a mapping from value name to ordinal
        self.value_ordinals = {value: i for i, value in enumerate(values)}

    def is_compatible_with(self, other: "TypeSymbol") -> bool:
        """Enums are only compatible with the same enum type"""
        if isinstance(other, NeverSymbol):
            return False
        if not isinstance(other, EnumTypeSymbol):
            return False

        # Enums are compatible only if they are the same type (same name and values)
        return self.name == other.name and self.values == other.values

    def can_assign_from(self, other: "TypeSymbol") -> bool:
        """Enums can only be assigned from the same enum type"""
        if isinstance(other, NeverSymbol):
            return False
        if not isinstance(other, EnumTypeSymbol):
            return False

        # Enums can be assigned only from the same type
        return self.name == other.name and self.values == other.values

    def get_result_type(self, operation: str, other: "TypeSymbol") -> "TypeSymbol":
        """Get result type for operations with enums"""
        if isinstance(other, NeverSymbol):
            return NEVER_SYMBOL

        if operation in ["=", "<>", "<", ">", "<=", ">="]:
            # Enum comparison operations
            if isinstance(other, EnumTypeSymbol) and self.is_compatible_with(other):
                return BOOLEAN_TYPE_SYMBOL
        elif operation in ["SUCC", "PRED"]:
            # Successor and predecessor operations return the same enum type
            return self
        elif operation == "ORD":
            # Ordinal operation returns INTEGER
            return INTEGER_TYPE_SYMBOL

        return NEVER_SYMBOL

    def get_ordinal(self, value: str) -> int:
        """Get the ordinal value of an enum constant"""
        return self.value_ordinals.get(value, -1)

    def get_value_at_ordinal(self, ordinal: int) -> str | None:
        """Get the enum value at the given ordinal position"""
        if 0 <= ordinal < len(self.values):
            return self.values[ordinal]
        return None

    def get_successor(self, value: str) -> str | None:
        """Get the successor of an enum value"""
        ordinal = self.get_ordinal(value)
        if ordinal >= 0 and ordinal < len(self.values) - 1:
            return self.values[ordinal + 1]
        return None

    def get_predecessor(self, value: str) -> str | None:
        """Get the predecessor of an enum value"""
        ordinal = self.get_ordinal(value)
        if ordinal > 0:
            return self.values[ordinal - 1]
        return None

    def __eq__(self, other: "TypeSymbol") -> "TypeSymbol":
        """Enum = Enum → BOOLEAN (if same enum type)"""
        return self.get_result_type("=", other)

    def __ne__(self, other: "TypeSymbol") -> "TypeSymbol":
        """Enum <> Enum → BOOLEAN (if same enum type)"""
        return self.get_result_type("<>", other)

    def __lt__(self, other: "TypeSymbol") -> "TypeSymbol":
        """Enum < Enum → BOOLEAN (ordinal comparison)"""
        return self.get_result_type("<", other)

    def __le__(self, other: "TypeSymbol") -> "TypeSymbol":
        """Enum <= Enum → BOOLEAN (ordinal comparison)"""
        return self.get_result_type("<=", other)

    def __gt__(self, other: "TypeSymbol") -> "TypeSymbol":
        """Enum > Enum → BOOLEAN (ordinal comparison)"""
        return self.get_result_type(">", other)

    def __ge__(self, other: "TypeSymbol") -> "TypeSymbol":
        """Enum >= Enum → BOOLEAN (ordinal comparison)"""
        return self.get_result_type(">=", other)

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        return "<{class_name}(name='{name}', values={values})>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            values=self.values,
        )


class RecordTypeSymbol(TypeSymbol):
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

    def is_compatible_with(self, other: "TypeSymbol") -> bool:
        """Records are compatible if they have the same structure"""
        if isinstance(other, NeverSymbol):
            return False
        if not isinstance(other, RecordTypeSymbol):
            return False

        # Records are compatible if they have the same name and field structure
        if self.name != other.name:
            return False

        # Check if field names match
        if set(self.fields.keys()) != set(other.fields.keys()):
            return False

        # Check if field types are compatible
        for field_name in self.fields:
            self_field_type = self.fields[field_name]
            other_field_type = other.fields[field_name]

            if isinstance(self_field_type, TypeSymbol) and isinstance(
                other_field_type, TypeSymbol
            ):
                if not self_field_type.is_compatible_with(other_field_type):
                    return False
            else:
                # Fallback to name comparison for non-TypeSymbol field types
                if self_field_type.name != other_field_type.name:
                    return False

        return True

    def can_assign_from(self, other: "TypeSymbol") -> bool:
        """Records can be assigned from records with compatible field types"""
        if isinstance(other, NeverSymbol):
            return False
        if not isinstance(other, RecordTypeSymbol):
            return False

        # Records can be assigned if they have the same structure and compatible field types
        if self.name != other.name:
            return False

        # Check if field names match
        if set(self.fields.keys()) != set(other.fields.keys()):
            return False

        # Check if field types can be assigned
        for field_name in self.fields:
            self_field_type = self.fields[field_name]
            other_field_type = other.fields[field_name]

            if isinstance(self_field_type, TypeSymbol) and isinstance(
                other_field_type, TypeSymbol
            ):
                if not self_field_type.can_assign_from(other_field_type):
                    return False
            else:
                # Fallback to name comparison for non-TypeSymbol field types
                if self_field_type.name != other_field_type.name:
                    return False

        return True

    def get_result_type(self, operation: str, other: "TypeSymbol") -> "TypeSymbol":
        """Get result type for operations with records"""
        if isinstance(other, NeverSymbol):
            return NEVER_SYMBOL

        if operation in ["=", "<>"]:
            # Record comparison operations
            if isinstance(other, RecordTypeSymbol) and self.is_compatible_with(other):
                return BOOLEAN_TYPE_SYMBOL

        # Records don't support arithmetic operations
        return NEVER_SYMBOL

    def get_field_type(self, field_name: str) -> Symbol | None:
        """Get the type of a specific field"""
        return self.fields.get(field_name)

    def has_field(self, field_name: str) -> bool:
        """Check if the record has a specific field"""
        return field_name in self.fields

    def validate_field_assignment(
        self, field_name: str, value_type: "TypeSymbol"
    ) -> bool:
        """Validate if a value of the given type can be assigned to the specified field"""
        field_type = self.get_field_type(field_name)
        if field_type is None:
            return False

        if isinstance(field_type, TypeSymbol):
            return field_type.can_assign_from(value_type)

        # Fallback for non-TypeSymbol field types
        return (
            field_type.name == value_type.name if hasattr(value_type, "name") else False
        )

    def __eq__(self, other: "TypeSymbol") -> "TypeSymbol":
        """Record = Record → BOOLEAN (if compatible)"""
        return self.get_result_type("=", other)

    def __ne__(self, other: "TypeSymbol") -> "TypeSymbol":
        """Record <> Record → BOOLEAN (if compatible)"""
        return self.get_result_type("<>", other)


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


# Initialize singleton type symbol instances
INTEGER_TYPE_SYMBOL = IntegerTypeSymbol()
REAL_TYPE_SYMBOL = RealTypeSymbol()
BOOLEAN_TYPE_SYMBOL = BooleanTypeSymbol()
CHAR_TYPE_SYMBOL = CharTypeSymbol()
STRING_TYPE_SYMBOL = StringTypeSymbol("STRING")
