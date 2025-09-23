"""SPI - Simple Pascal Interpreter. Part 19"""

from __future__ import annotations

from abc import ABC, abstractmethod

from spi.ast import Block, ParamMode, Type

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

        if operation in ["+", "-", "*", "DIV", "MOD"]:
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
    def __init__(
        self,
        name: str,
        type: Symbol | None,
        param_mode: ParamMode = ParamMode.CLONE,
    ) -> None:
        super().__init__(name, type or NEVER_SYMBOL)
        self.is_initialized = False  # Track if const has been initialized
        self.param_mode = param_mode

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
        return self.param_mode == ParamMode.CONST

    @property
    def is_mutable(self) -> bool:
        """Check if this is a non-const variable"""
        return self.param_mode != ParamMode.CONST

    def validate_assignment(
        self, is_initialization: bool = False
    ) -> tuple[bool, str | None]:
        """
        Validate if assignment to this variable is allowed.

        Args:
            is_initialization: True if this is the initial assignment to a const variable

        Returns:
            Tuple of (is_valid, error_message)
        """
        if self.is_mutable:
            # Mutable variables can always be assigned
            return True, None

        # For const variables
        if is_initialization and not self.is_initialized:
            # Allow initialization-time assignment to const variables
            return True, None
        elif self.is_initialized:
            # Const variable already initialized, cannot be reassigned
            return (
                False,
                f"Cannot assign to const variable '{self.name}' - already initialized",
            )
        else:
            # Const variable not yet initialized, but this is not an initialization
            return (
                False,
                f"Cannot assign to const variable '{self.name}' - must be initialized at declaration",
            )

    def validate_modification_permission(self) -> tuple[bool, str | None]:
        """
        Check if this variable has permission to be modified.

        Returns:
            Tuple of (can_modify, error_message)
        """
        if self.is_mutable:
            return True, None

        if not self.is_initialized:
            return True, None  # Can modify during initialization

        return False, f"Cannot modify const variable '{self.name}'"

    def require_initialization_check(self) -> bool:
        """
        Check if this variable requires initialization validation.

        Returns:
            True if this is a const variable that must be initialized
        """
        return self.is_const

    def is_initialization_required(self) -> bool:
        """
        Check if this const variable still needs to be initialized.

        Returns:
            True if this is a const variable that hasn't been initialized yet
        """
        return self.is_const and not self.is_initialized

    def __str__(self) -> str:
        return "<{class_name}(name='{name}', type='{type}', mutable={mutable})>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            type=self.type,
            mutable=self.is_mutable,
        )

    __repr__ = __str__


class MutabilityValidator:
    """Helper class for validating variable mutability and const assignment rules"""

    @staticmethod
    def validate_const_assignment(
        var_symbol: VarSymbol, is_initialization: bool = False
    ) -> tuple[bool, str | None]:
        """
        Validate assignment to a const variable.

        Args:
            var_symbol: The variable symbol being assigned to
            is_initialization: True if this is the initial assignment during declaration

        Returns:
            Tuple of (is_valid, error_message)
        """
        if not isinstance(var_symbol, VarSymbol):
            return True, None  # Not a variable, no validation needed

        return var_symbol.validate_assignment(is_initialization)

    @staticmethod
    def validate_variable_modification(
        var_symbol: VarSymbol,
    ) -> tuple[bool, str | None]:
        """
        Validate if a variable can be modified.

        Args:
            var_symbol: The variable symbol being modified

        Returns:
            Tuple of (is_valid, error_message)
        """
        if not isinstance(var_symbol, VarSymbol):
            return True, None  # Not a variable, no validation needed

        return var_symbol.validate_modification_permission()

    @staticmethod
    def check_initialization_requirements(
        var_symbol: VarSymbol,
    ) -> tuple[bool, str | None]:
        """
        Check if a const variable meets initialization requirements.

        Args:
            var_symbol: The variable symbol to check

        Returns:
            Tuple of (is_satisfied, error_message)
        """
        if not isinstance(var_symbol, VarSymbol):
            return True, None  # Not a variable, no requirements

        if var_symbol.is_initialization_required():
            return False, f"Const variable '{var_symbol.name}' must be initialized"

        return True, None

    @staticmethod
    def mark_variable_initialized(var_symbol: VarSymbol) -> None:
        """
        Mark a variable as initialized (for const variables).

        Args:
            var_symbol: The variable symbol to mark as initialized
        """
        if isinstance(var_symbol, VarSymbol):
            var_symbol.mark_initialized()

    @staticmethod
    def is_const_variable(var_symbol: VarSymbol) -> bool:
        """
        Check if a variable is a const variable.

        Args:
            var_symbol: The variable symbol to check

        Returns:
            True if the variable is const, False otherwise
        """
        if not isinstance(var_symbol, VarSymbol):
            return False

        return var_symbol.is_const

    @staticmethod
    def get_mutability_info(var_symbol: VarSymbol) -> dict[str, bool]:
        """
        Get comprehensive mutability information for a variable.

        Args:
            var_symbol: The variable symbol to analyze

        Returns:
            Dictionary with mutability information
        """
        if not isinstance(var_symbol, VarSymbol):
            return {
                "is_mutable": True,
                "is_const": False,
                "is_initialized": True,
                "can_modify": True,
                "requires_initialization": False,
            }

        return {
            "is_mutable": var_symbol.is_mutable,
            "is_const": var_symbol.is_const,
            "is_initialized": var_symbol.is_initialized,
            "can_modify": var_symbol.can_modify(),
            "requires_initialization": var_symbol.require_initialization_check(),
        }


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

    @property
    def variant_fields(self) -> dict[str, Symbol]:
        return self.variant_part.variant_fields


class RecordFieldSymbol(TypeSymbol):
    """表示记录字段符号"""

    def __init__(self, name: str, type_symbol: Symbol):
        super().__init__(name)
        self.type = type_symbol

    def is_compatible_with(self, other: "TypeSymbol") -> bool:
        """Check if this field type is compatible with another type"""
        if isinstance(other, NeverSymbol):
            return False

        # Delegate to the actual field type
        if isinstance(self.type, TypeSymbol):
            return self.type.is_compatible_with(other)

        # Fallback for non-TypeSymbol field types
        return self.type.name == other.name if hasattr(other, "name") else False

    def can_assign_from(self, other: "TypeSymbol") -> bool:
        """Check if a value of other type can be assigned to this field"""
        if isinstance(other, NeverSymbol):
            return False

        # Delegate to the actual field type
        if isinstance(self.type, TypeSymbol):
            return self.type.can_assign_from(other)

        # Fallback for non-TypeSymbol field types
        return self.type.name == other.name if hasattr(other, "name") else False

    def get_result_type(self, operation: str, other: "TypeSymbol") -> "TypeSymbol":
        """Get the result type of an operation with another type"""
        if isinstance(other, NeverSymbol):
            return NEVER_SYMBOL

        # Delegate to the actual field type
        if isinstance(self.type, TypeSymbol):
            return self.type.get_result_type(operation, other)

        # Fallback for non-TypeSymbol field types - no operations supported
        return NEVER_SYMBOL


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

    @property
    def variant_fields(self) -> dict[str, Symbol]:
        variant_fields: dict[str, Symbol] = {}
        for _, fields_dict in self.variant_cases.items():
            for field_name, symbol in fields_dict.items():
                variant_fields[field_name] = symbol
        return variant_fields


class TypeAliasSymbol(TypeSymbol):
    """Symbol for type aliases that can chain to other types"""

    def __init__(self, name: str, target_type: TypeSymbol):
        super().__init__(name)
        self.target_type = target_type

    def resolve_final_type(self) -> TypeSymbol:
        """Resolve through alias chain to get final concrete type"""
        visited = {self.name}
        current = self.target_type

        while isinstance(current, TypeAliasSymbol):
            if current.name in visited:
                # Import here to avoid circular imports
                from src.spi.error import ErrorCode, SemanticError

                chain_str = " -> ".join(visited) + f" -> {current.name}"
                raise SemanticError(
                    error_code=ErrorCode.SEMANTIC_CIRCULAR_TYPE_ALIAS,
                    token=None,
                    message=f"Circular type alias detected in chain: {chain_str}",
                )
            visited.add(current.name)
            current = current.target_type

        return current

    def is_compatible_with(self, other: TypeSymbol) -> bool:
        """Delegate to final resolved type"""
        if isinstance(other, NeverSymbol):
            return False

        final_type = self.resolve_final_type()

        # If other is also a type alias, resolve it too
        if isinstance(other, TypeAliasSymbol):
            other_final_type = other.resolve_final_type()
            return final_type.is_compatible_with(other_final_type)

        return final_type.is_compatible_with(other)

    def can_assign_from(self, other: TypeSymbol) -> bool:
        """Delegate to final resolved type"""
        if isinstance(other, NeverSymbol):
            return False

        final_type = self.resolve_final_type()

        # If other is also a type alias, resolve it too
        if isinstance(other, TypeAliasSymbol):
            other_final_type = other.resolve_final_type()
            return final_type.can_assign_from(other_final_type)

        return final_type.can_assign_from(other)

    def get_result_type(self, operation: str, other: TypeSymbol) -> TypeSymbol:
        """Delegate to final resolved type"""
        if isinstance(other, NeverSymbol):
            return NEVER_SYMBOL

        final_type = self.resolve_final_type()

        # If other is also a type alias, resolve it too
        if isinstance(other, TypeAliasSymbol):
            other_final_type = other.resolve_final_type()
            return final_type.get_result_type(operation, other_final_type)

        return final_type.get_result_type(operation, other)

    def __str__(self) -> str:
        return f"{self.name} -> {self.target_type.name}"

    def __repr__(self) -> str:
        return (
            f"<TypeAliasSymbol(name='{self.name}', target='{self.target_type.name}')>"
        )


class ProcedureSymbol(Symbol):
    def __init__(self, name: str) -> None:
        super().__init__(name)
        # a list of VarSymbol objects
        self.formal_params: list[VarSymbol] = []
        # a reference to procedure's body (AST sub-tree)
        self.block_ast: Block | None = None
        self.is_forward = False

    def __str__(self) -> str:
        return "<{class_name}(name={name}, parameters={params})>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            params=self.formal_params,
        )

    __repr__ = __str__


class BuiltinProcedureSymbol(Symbol):
    def __init__(self, name: str) -> None:
        super().__init__(name)
        # a list of VarSymbol objects
        self.formal_params: list[Symbol] = []

    def __str__(self) -> str:
        return "<{class_name}(name={name}, parameters={params})>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            params=self.formal_params,
        )

    __repr__ = __str__


class FunctionSymbol(Symbol):
    def __init__(self, name: str, return_type: Type) -> None:
        super().__init__(name)
        # a list of VarSymbol objects
        self.formal_params: list[VarSymbol] = []
        self.return_type = return_type
        # a reference to procedure's body (AST sub-tree)
        self.block_ast: Block | None = None
        self.is_forward = False

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


class ProcedureTypeSymbol(TypeSymbol):
    """Type symbol for procedure types (for type checking procedure parameters)"""

    def __init__(self, name: str, param_types: list[TypeSymbol]):
        super().__init__(name)
        self.param_types = param_types

    def is_compatible_with(self, other: "TypeSymbol") -> bool:
        """Check if procedure signatures are compatible"""
        if isinstance(other, NeverSymbol):
            return False
        if not isinstance(other, ProcedureTypeSymbol):
            return False

        if len(self.param_types) != len(other.param_types):
            return False

        return all(
            p1.is_compatible_with(p2)
            for p1, p2 in zip(self.param_types, other.param_types)
        )

    def can_assign_from(self, other: "TypeSymbol") -> bool:
        """Check if a procedure of other type can be assigned to this procedure type"""
        if isinstance(other, NeverSymbol):
            return False
        if not isinstance(other, ProcedureTypeSymbol):
            return False

        if len(self.param_types) != len(other.param_types):
            return False

        # For procedure assignment, parameter types must be exactly compatible
        # (contravariant for input parameters)
        return all(
            p1.can_assign_from(p2)
            for p1, p2 in zip(self.param_types, other.param_types)
        )

    def get_result_type(self, operation: str, other: "TypeSymbol") -> "TypeSymbol":
        """Get result type for operations with procedure types"""
        if isinstance(other, NeverSymbol):
            return NEVER_SYMBOL

        if operation in ["=", "<>"]:
            # Procedure comparison operations
            if isinstance(other, ProcedureTypeSymbol) and self.is_compatible_with(
                other
            ):
                return BOOLEAN_TYPE_SYMBOL

        # Procedures don't support arithmetic operations
        return NEVER_SYMBOL

    def validate_call_signature(self, arg_types: list[TypeSymbol]) -> bool:
        """Validate if the given argument types match this procedure's signature"""
        if len(arg_types) != len(self.param_types):
            return False

        return all(
            param_type.can_assign_from(arg_type)
            for param_type, arg_type in zip(self.param_types, arg_types)
        )

    def get_parameter_count(self) -> int:
        """Get the number of parameters this procedure expects"""
        return len(self.param_types)

    def get_parameter_type(self, index: int) -> TypeSymbol:
        """Get the type of the parameter at the given index"""
        if 0 <= index < len(self.param_types):
            return self.param_types[index]
        return NEVER_SYMBOL

    def __eq__(self, other: "TypeSymbol") -> "TypeSymbol":
        """Procedure = Procedure → BOOLEAN (if compatible)"""
        return self.get_result_type("=", other)

    def __ne__(self, other: "TypeSymbol") -> "TypeSymbol":
        """Procedure <> Procedure → BOOLEAN (if compatible)"""
        return self.get_result_type("<>", other)

    def __str__(self) -> str:
        param_names = [param.name for param in self.param_types]
        return f"PROCEDURE({', '.join(param_names)})"

    def __repr__(self) -> str:
        return "<{class_name}(name='{name}', params={params})>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            params=[param.name for param in self.param_types],
        )


class FunctionTypeSymbol(TypeSymbol):
    """Type symbol for function types (for type checking function parameters and return type)"""

    def __init__(
        self, name: str, param_types: list[TypeSymbol], return_type: TypeSymbol
    ):
        super().__init__(name)
        self.param_types = param_types
        self.return_type = return_type

    def is_compatible_with(self, other: "TypeSymbol") -> bool:
        """Check if function signatures are compatible"""
        if isinstance(other, NeverSymbol):
            return False
        if not isinstance(other, FunctionTypeSymbol):
            return False

        if len(self.param_types) != len(other.param_types):
            return False

        if not self.return_type.is_compatible_with(other.return_type):
            return False

        return all(
            p1.is_compatible_with(p2)
            for p1, p2 in zip(self.param_types, other.param_types)
        )

    def can_assign_from(self, other: "TypeSymbol") -> bool:
        """Check if a function of other type can be assigned to this function type"""
        if isinstance(other, NeverSymbol):
            return False
        if not isinstance(other, FunctionTypeSymbol):
            return False

        if len(self.param_types) != len(other.param_types):
            return False

        # Return type must be covariant (can assign from more specific to more general)
        if not self.return_type.can_assign_from(other.return_type):
            return False

        # Parameter types must be contravariant (can assign from more general to more specific)
        return all(
            p1.can_assign_from(p2)
            for p1, p2 in zip(self.param_types, other.param_types)
        )

    def get_result_type(self, operation: str, other: "TypeSymbol") -> "TypeSymbol":
        """Get result type for operations with function types"""
        if isinstance(other, NeverSymbol):
            return NEVER_SYMBOL

        if operation in ["=", "<>"]:
            # Function comparison operations
            if isinstance(other, FunctionTypeSymbol) and self.is_compatible_with(other):
                return BOOLEAN_TYPE_SYMBOL
        elif operation == "CALL":
            # Function call operation returns the function's return type
            return self.return_type

        # Functions don't support arithmetic operations
        return NEVER_SYMBOL

    def validate_call_signature(self, arg_types: list[TypeSymbol]) -> bool:
        """Validate if the given argument types match this function's signature"""
        if len(arg_types) != len(self.param_types):
            return False

        return all(
            param_type.can_assign_from(arg_type)
            for param_type, arg_type in zip(self.param_types, arg_types)
        )

    def get_parameter_count(self) -> int:
        """Get the number of parameters this function expects"""
        return len(self.param_types)

    def get_parameter_type(self, index: int) -> TypeSymbol:
        """Get the type of the parameter at the given index"""
        if 0 <= index < len(self.param_types):
            return self.param_types[index]
        return NEVER_SYMBOL

    def get_return_type(self) -> TypeSymbol:
        """Get the return type of this function"""
        return self.return_type

    def __eq__(self, other: "TypeSymbol") -> "TypeSymbol":
        """Function = Function → BOOLEAN (if compatible)"""
        return self.get_result_type("=", other)

    def __ne__(self, other: "TypeSymbol") -> "TypeSymbol":
        """Function <> Function → BOOLEAN (if compatible)"""
        return self.get_result_type("<>", other)

    def __str__(self) -> str:
        param_names = [param.name for param in self.param_types]
        return f"FUNCTION({', '.join(param_names)}) : {self.return_type.name}"

    def __repr__(self) -> str:
        return "<{class_name}(name='{name}', params={params}, return_type='{return_type}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            params=[param.name for param in self.param_types],
            return_type=self.return_type.name,
        )


# Initialize singleton type symbol instances
INTEGER_TYPE_SYMBOL = IntegerTypeSymbol()
REAL_TYPE_SYMBOL = RealTypeSymbol()
BOOLEAN_TYPE_SYMBOL = BooleanTypeSymbol()
CHAR_TYPE_SYMBOL = CharTypeSymbol()
STRING_TYPE_SYMBOL = StringTypeSymbol("STRING")


class BuiltinTypeSymbol(TypeSymbol):
    """Builtin type symbol that delegates to appropriate TypeSymbol instances

    This class serves as a bridge between the old type system and the new enhanced
    type system. It can delegate to primitive types or type aliases, providing
    backward compatibility while supporting the new type operations.
    """

    def __init__(self, name: str, delegate_type: TypeSymbol = None) -> None:
        super().__init__(name)
        if delegate_type is not None:
            self._delegate_type = delegate_type
        else:
            self._delegate_type = self._get_delegate_type(name)

    def _get_delegate_type(self, name: str) -> TypeSymbol:
        """Get the appropriate TypeSymbol delegate for the builtin type"""
        name_upper = name.upper()
        if name_upper == "INTEGER":
            return INTEGER_TYPE_SYMBOL
        elif name_upper == "REAL":
            return REAL_TYPE_SYMBOL
        elif name_upper == "BOOLEAN":
            return BOOLEAN_TYPE_SYMBOL
        elif name_upper == "CHAR":
            return CHAR_TYPE_SYMBOL
        elif name_upper == "STRING":
            return STRING_TYPE_SYMBOL
        else:
            # For unknown types, return NEVER_SYMBOL
            return NEVER_SYMBOL

    def set_delegate_type(self, delegate_type: TypeSymbol) -> None:
        """Set the delegate type for this builtin type symbol

        This method allows dynamic assignment of the delegate type,
        which is useful for type aliases and complex type scenarios.
        """
        self._delegate_type = delegate_type

    @property
    def delegate_type(self) -> TypeSymbol:
        """Get the current delegate type"""
        return self._delegate_type

    def resolve_final_type(self) -> TypeSymbol:
        """Resolve through type alias chain to get the final concrete type

        This method handles complex type alias chains and ensures that
        we always get to the final concrete type, even through multiple
        levels of BuiltinTypeSymbol and TypeAliasSymbol indirection.
        """
        current = self._delegate_type
        visited = {self.name}  # Track visited types to detect cycles

        while True:
            if isinstance(current, TypeAliasSymbol):
                # Resolve through type alias
                current = current.resolve_final_type()
                break
            elif isinstance(current, BuiltinTypeSymbol):
                # Handle nested BuiltinTypeSymbol delegation
                if current.name in visited:
                    # Circular reference detected
                    return NEVER_SYMBOL
                visited.add(current.name)
                current = current._delegate_type
            else:
                # We've reached a concrete type
                break

        return current

    def is_compatible_with(self, other: TypeSymbol) -> bool:
        """Delegate compatibility checking to the underlying type"""
        if isinstance(other, BuiltinTypeSymbol):
            return self._delegate_type.is_compatible_with(other._delegate_type)
        return self._delegate_type.is_compatible_with(other)

    def can_assign_from(self, other: TypeSymbol) -> bool:
        """Delegate assignment compatibility to the underlying type"""
        if isinstance(other, BuiltinTypeSymbol):
            return self._delegate_type.can_assign_from(other._delegate_type)
        return self._delegate_type.can_assign_from(other)

    def get_result_type(self, operation: str, other: TypeSymbol) -> TypeSymbol:
        """Delegate operation result type to the underlying type"""
        if isinstance(other, BuiltinTypeSymbol):
            return self._delegate_type.get_result_type(operation, other._delegate_type)
        return self._delegate_type.get_result_type(operation, other)

    def is_builtin_primitive(self) -> bool:
        """Check if this builtin type represents a primitive type"""
        resolved = self.resolve_final_type()
        return isinstance(
            resolved,
            (IntegerTypeSymbol, RealTypeSymbol, BooleanTypeSymbol, CharTypeSymbol),
        )

    def is_alias(self) -> bool:
        """Check if this builtin type is actually a type alias"""
        return isinstance(self._delegate_type, (TypeAliasSymbol, BuiltinTypeSymbol))

    def get_primitive_name(self) -> str:
        """Get the name of the underlying primitive type, if any"""
        resolved = self.resolve_final_type()
        if resolved == NEVER_SYMBOL:
            return "NEVER"
        return resolved.name

    def __str__(self) -> str:
        return self.name

    def __repr__(self) -> str:
        delegate_name = getattr(self._delegate_type, "name", str(self._delegate_type))
        return "<{class_name}(name='{name}', delegate='{delegate}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            delegate=delegate_name,
        )
