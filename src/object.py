import copy
from typing import Any, Dict, Union

from src.spi_token import ElementType


class InterpreterError(Exception):
    """Base class for interpreter errors."""

    pass


class OperationNotSupportedError(InterpreterError):
    """Raised when an operation is not supported for a specific object type."""

    def __init__(self, operation: str, type_name: str):
        self.message = f"Operation '{operation}' not supported for type '{type_name}'"
        super().__init__(self.message)


Number = Union[int, float]


class Object:
    def __init__(self, type: ElementType):
        self.type = type

    def __repr__(self):
        """Default string representation for all objects."""
        return f"{self.__class__.__name__}(type={self.type})"

    def __str__(self):
        """String representation for display purposes."""
        return self.__repr__()

    def __eq__(self, other) -> bool:
        """Default equality comparison that subclasses should override."""
        raise OperationNotSupportedError("equality comparison", self.type.value)

    def __lt__(self, other) -> bool:
        """Less than comparison that subclasses should override."""
        raise OperationNotSupportedError("less than comparison", self.type.value)

    def __gt__(self, other) -> bool:
        """Greater than comparison that subclasses should override."""
        raise OperationNotSupportedError("greater than comparison", self.type.value)

    def __le__(self, other) -> bool:
        """Less than or equal comparison that subclasses should override."""
        raise OperationNotSupportedError(
            "less than or equal comparison", self.type.value
        )

    def __ge__(self, other) -> bool:
        """Greater than or equal comparison that subclasses should override."""
        raise OperationNotSupportedError(
            "greater than or equal comparison", self.type.value
        )

    def plus(self, other):
        """Addition operation that number-related subclasses should implement."""
        raise OperationNotSupportedError("addition", self.type.value)

    def minus(self, other):
        """Subtraction operation that number-related subclasses should implement."""
        raise OperationNotSupportedError("subtraction", self.type.value)

    def multiply(self, other):
        """Multiplication operation that number-related subclasses should implement."""
        raise OperationNotSupportedError("multiplication", self.type.value)

    def divide(self, other):
        """Division operation that number-related subclasses should implement."""
        raise OperationNotSupportedError("division", self.type.value)

    def __getitem__(self, key):
        """Item access that container-like subclasses should implement."""
        raise OperationNotSupportedError("item access", self.type.value)

    def __setitem__(self, key, value):
        """Item assignment that container-like subclasses should implement."""
        raise OperationNotSupportedError("item assignment", self.type.value)

    def __copy__(self):
        """Create a shallow copy of the object."""
        return copy.copy(self)

    def __deepcopy__(self, memo):
        """Create a deep copy of the object."""
        raise OperationNotSupportedError("deep copy", self.type.value)


class EnumObject(Object):
    def __init__(self, index: int, name: str):
        super().__init__(type=ElementType.ENUM)
        self.index = index
        self.name = name

    def __eq__(self, other) -> bool:
        if isinstance(other, EnumObject):
            return self.index == other.index and self.name == other.name
        return False

    def __repr__(self):
        return f"{self.__class__.__name__}(index={self.index}, name='{self.name}')"

    def __str__(self):
        return self.name

    def __copy__(self):
        return EnumObject(self.index, self.name)

    def __deepcopy__(self, memo):
        return EnumObject(self.index, self.name)


class IntegerObject(Object):
    def __init__(self, value: int):
        super().__init__(type=ElementType.INTEGER)
        self.value = value

    def __repr__(self):
        return f"{self.__class__.__name__}(value={self.value})"

    def __str__(self):
        return str(self.value)

    def plus(self, other):
        if isinstance(other, IntegerObject):
            return IntegerObject(self.value + other.value)
        elif isinstance(other, RealObject):
            return RealObject(self.value + other.value)
        raise OperationNotSupportedError(
            "addition", f"{self.type.value} with {type(other)}"
        )

    def minus(self, other):
        if isinstance(other, IntegerObject):
            return IntegerObject(self.value - other.value)
        elif isinstance(other, RealObject):
            return RealObject(self.value - other.value)
        raise OperationNotSupportedError(
            "subtraction", f"{self.type.value} with {type(other)}"
        )

    def multiply(self, other):
        if isinstance(other, IntegerObject):
            return IntegerObject(self.value * other.value)
        elif isinstance(other, RealObject):
            return RealObject(self.value * other.value)
        raise OperationNotSupportedError(
            "multiplication", f"{self.type.value} with {type(other)}"
        )

    def divide(self, other):
        if isinstance(other, (IntegerObject, RealObject)):
            if other.value == 0:
                raise ZeroDivisionError("Division by zero")
            # Always return RealObject for division
            return RealObject(self.value / other.value)
        raise OperationNotSupportedError(
            "division", f"{self.type.value} with {type(other)}"
        )

    def __eq__(self, other) -> bool:
        if isinstance(other, IntegerObject):
            return self.value == other.value
        elif isinstance(other, RealObject):
            return self.value == other.value
        return False

    def __lt__(self, other) -> bool:
        if isinstance(other, (IntegerObject, RealObject)):
            return self.value < other.value
        raise OperationNotSupportedError(
            "less than comparison", f"{self.type.value} with {type(other)}"
        )

    def __gt__(self, other) -> bool:
        if isinstance(other, (IntegerObject, RealObject)):
            return self.value > other.value
        raise OperationNotSupportedError(
            "greater than comparison", f"{self.type.value} with {type(other)}"
        )

    def __le__(self, other) -> bool:
        if isinstance(other, (IntegerObject, RealObject)):
            return self.value <= other.value
        raise OperationNotSupportedError(
            "less than or equal comparison", f"{self.type.value} with {type(other)}"
        )

    def __ge__(self, other) -> bool:
        if isinstance(other, (IntegerObject, RealObject)):
            return self.value >= other.value
        raise OperationNotSupportedError(
            "greater than or equal comparison", f"{self.type.value} with {type(other)}"
        )

    def __copy__(self):
        return IntegerObject(self.value)

    def __deepcopy__(self, memo):
        return IntegerObject(self.value)


class RealObject(Object):
    def __init__(self, value: float):
        super().__init__(type=ElementType.REAL)
        self.value = value

    def __repr__(self):
        return f"{self.__class__.__name__}(value={self.value})"

    def __str__(self):
        return str(self.value)

    def plus(self, other):
        if isinstance(other, (IntegerObject, RealObject)):
            return RealObject(self.value + other.value)
        raise OperationNotSupportedError(
            "addition", f"{self.type.value} with {type(other)}"
        )

    def minus(self, other):
        if isinstance(other, (IntegerObject, RealObject)):
            return RealObject(self.value - other.value)
        raise OperationNotSupportedError(
            "subtraction", f"{self.type.value} with {type(other)}"
        )

    def multiply(self, other):
        if isinstance(other, (IntegerObject, RealObject)):
            return RealObject(self.value * other.value)
        raise OperationNotSupportedError(
            "multiplication", f"{self.type.value} with {type(other)}"
        )

    def divide(self, other):
        if isinstance(other, (IntegerObject, RealObject)):
            if other.value == 0:
                raise ZeroDivisionError("Division by zero")
            return RealObject(self.value / other.value)
        raise OperationNotSupportedError(
            "division", f"{self.type.value} with {type(other)}"
        )

    def __eq__(self, other) -> bool:
        if isinstance(other, RealObject):
            return self.value == other.value
        elif isinstance(other, IntegerObject):
            return self.value == other.value
        return False

    def __lt__(self, other) -> bool:
        if isinstance(other, (IntegerObject, RealObject)):
            return self.value < other.value
        raise OperationNotSupportedError(
            "less than comparison", f"{self.type.value} with {type(other)}"
        )

    def __gt__(self, other) -> bool:
        if isinstance(other, (IntegerObject, RealObject)):
            return self.value > other.value
        raise OperationNotSupportedError(
            "greater than comparison", f"{self.type.value} with {type(other)}"
        )

    def __le__(self, other) -> bool:
        if isinstance(other, (IntegerObject, RealObject)):
            return self.value <= other.value
        raise OperationNotSupportedError(
            "less than or equal comparison", f"{self.type.value} with {type(other)}"
        )

    def __ge__(self, other) -> bool:
        if isinstance(other, (IntegerObject, RealObject)):
            return self.value >= other.value
        raise OperationNotSupportedError(
            "greater than or equal comparison", f"{self.type.value} with {type(other)}"
        )

    def __copy__(self):
        return RealObject(self.value)

    def __deepcopy__(self, memo):
        return RealObject(self.value)


class BooleanObject(Object):
    def __init__(self, value: bool):
        super().__init__(type=ElementType.BOOL)
        self.value = value

    def __repr__(self):
        return f"{self.__class__.__name__}(value={self.value})"

    def __str__(self):
        return str(self.value).lower()

    def __eq__(self, other) -> bool:
        if isinstance(other, BooleanObject):
            return self.value == other.value
        return False

    def __copy__(self):
        return BooleanObject(self.value)

    def __deepcopy__(self, memo):
        return BooleanObject(self.value)


class StringObject(Object):
    def __init__(self, value: str, limit: int = -1):
        super().__init__(type=ElementType.STRING)
        self.value = value
        self.limit = limit

        # Truncate string if it exceeds the limit
        if limit > 0 and len(value) > limit:
            self.value = value[:limit]

    def __repr__(self):
        return f"{self.__class__.__name__}(value='{self.value}', limit={self.limit})"

    def __str__(self):
        return self.value

    def plus(self, other):
        if isinstance(other, StringObject):
            result = self.value + other.value
            # Apply limit if it exists
            # if self.limit > 0 and len(result) > self.limit:
            #     result = result[: self.limit]
            return StringObject(result, -1)
            # if self.limit > 0 and len(result) > self.limit:
            #     result = result[:self.limit]
            # return StringObject(result, self.limit)
        raise OperationNotSupportedError(
            "addition", f"{self.type.value} with {type(other)}"
        )

    def __getitem__(self, index):
        # Pascal strings are 1-indexed
        if isinstance(index, IntegerObject):
            index = index.value

        if isinstance(index, int):
            if index < 1 or index > len(self.value):
                return StringObject("")
            return StringObject(self.value[index - 1])
        elif isinstance(index, slice):
            # Handle Python slice objects
            start = index.start if index.start is not None else 0
            stop = (
                index.stop.value
                if isinstance(index.stop, IntegerObject)
                else len(self.value)
            )
            step = index.step

            # Adjust for 1-indexed system if input is from the language
            # But use Python's 0-indexing for the actual slicing
            # For simplicity, we assume Python-style 0-indexed slices are being passed
            result = self.value[start:stop:step]
            return StringObject(result, self.limit)

        raise TypeError("String index must be an integer, IntegerObject, or slice")

    def __eq__(self, other) -> bool:
        if isinstance(other, StringObject):
            return self.value == other.value
        return False

    def __len__(self):
        return len(self.value)

    def __copy__(self):
        return StringObject(self.value, self.limit)

    def __deepcopy__(self, memo):
        return StringObject(self.value, self.limit)


class ArrayObject(Object):
    def __init__(
        self, elements: Dict[int, Any], element_type: ElementType, dynamic: bool = False
    ):
        super().__init__(type=ElementType.ARRAY)
        self.elements = elements
        self.element_type = element_type
        self.dynamic = dynamic

    def __repr__(self):
        elements_str = ", ".join([f"{k}: {v}" for k, v in self.elements.items()])
        return f"{self.__class__.__name__}({{{elements_str}}}, element_type={self.element_type}, dynamic={self.dynamic})"

    def __str__(self):
        # Sort elements by key to ensure consistent output
        sorted_elements = sorted(self.elements.items())
        elements_str = ", ".join([str(v) for _, v in sorted_elements])
        return f"[{elements_str}]"

    def __len__(self):
        return len(self.elements)

    def __getitem__(self, index):
        # Convert IntegerObject to int for dictionary access
        if isinstance(index, IntegerObject):
            index = index.value

        if index in self.elements:
            return self.elements[index]
        # Return default values for missing indices based on element type
        if self.element_type == ElementType.BOOL:
            return BooleanObject(False)
        elif self.element_type == ElementType.INTEGER:
            return IntegerObject(0)
        elif self.element_type == ElementType.REAL:
            return RealObject(0.0)
        elif self.element_type == ElementType.ARRAY:
            return ArrayObject({}, self.element_type)
        elif self.element_type == ElementType.STRING:
            return StringObject("")
        return None

    def __setitem__(self, index, value):
        # Convert IntegerObject to int for dictionary access
        if isinstance(index, IntegerObject):
            index = index.value

        self.elements[index] = value

    def __eq__(self, other) -> bool:
        if isinstance(other, ArrayObject):
            return (
                self.element_type == other.element_type
                and self.elements == other.elements
            )
        return False

    def __contains__(self, index):
        """Check if an index exists in the array."""
        # Convert IntegerObject to int for dictionary access
        if isinstance(index, IntegerObject):
            index = index.value

        return index in self.elements

    def __copy__(self):
        return ArrayObject(self.elements.copy(), self.element_type, self.dynamic)

    def __deepcopy__(self, memo):
        if id(self) in memo:
            return memo[id(self)]

        # Create a new instance with empty elements
        result = ArrayObject({}, self.element_type, self.dynamic)
        memo[id(self)] = result

        # Deep copy the elements
        for key, value in self.elements.items():
            result.elements[key] = copy.deepcopy(value, memo)

        return result


class RecordClassObject(Object):
    def __init__(self, record_name: str, fields: Dict[str, Any]):
        super().__init__(type=ElementType.RECORD_CLASS)
        self.record_name = record_name
        self.fields = fields

    def __repr__(self):
        fields_str = ", ".join([f"{k}: {v}" for k, v in self.fields.items()])
        return f"{self.__class__.__name__}(record_name='{self.record_name}', fields={{{fields_str}}})"

    def __str__(self):
        fields_str = ", ".join([f"{k}: {str(v)}" for k, v in self.fields.items()])
        return f"{{{fields_str}}}"

    def __getitem__(self, field_name):
        if field_name in self.fields:
            return self.fields[field_name]
        raise KeyError(f"Field '{field_name}' not found in record")

    def __setitem__(self, field_name, value):
        if field_name in self.fields:
            self.fields[field_name] = value
        else:
            raise KeyError(f"Field '{field_name}' not found in record")

    def __eq__(self, other) -> bool:
        if isinstance(other, RecordClassObject):
            return self.record_name == other.record_name and self.fields == other.fields
        return False

    def __copy__(self):
        return RecordClassObject(self.record_name, self.fields.copy())

    def __deepcopy__(self, memo):
        if id(self) in memo:
            return memo[id(self)]

        # Create a new instance with empty fields
        result = RecordClassObject(self.record_name, {})
        memo[id(self)] = result

        # Deep copy the fields
        for key, value in self.fields.items():
            result.fields[key] = copy.deepcopy(value, memo)

        return result


class RecordInstanceObject(Object):
    def __init__(self, record_name: str, fields: Dict[str, Any]):
        super().__init__(type=ElementType.RECORD_INSTANCE)
        self.record_name = record_name
        self.fields = fields

    def __repr__(self):
        fields_str = ", ".join([f"{k}: {v}" for k, v in self.fields.items()])
        return f"{self.__class__.__name__}(record_name='{self.record_name}', fields={{{fields_str}}})"

    def __str__(self):
        fields_str = ", ".join([f"{k}: {str(v)}" for k, v in self.fields.items()])
        return f"<record {self.record_name} {{{fields_str}}}>"

    def __getitem__(self, field_name):
        if field_name in self.fields:
            return self.fields[field_name]
        raise KeyError(f"Field '{field_name}' not found in record instance")

    def __setitem__(self, field_name, value):
        if field_name in self.fields:
            self.fields[field_name] = value
        else:
            raise KeyError(f"Field '{field_name}' not found in record instance")

    def __eq__(self, other) -> bool:
        if isinstance(other, RecordInstanceObject):
            return self.record_name == other.record_name and self.fields == other.fields
        return False

    def __copy__(self):
        return RecordInstanceObject(self.record_name, self.fields.copy())

    def __deepcopy__(self, memo):
        if id(self) in memo:
            return memo[id(self)]

        # Create a new instance with empty fields
        result = RecordInstanceObject(self.record_name, {})
        memo[id(self)] = result

        # Deep copy the fields
        for key, value in self.fields.items():
            result.fields[key] = copy.deepcopy(value, memo)

        return result


class ClassObject(Object):
    def __init__(
        self, class_name: str, fields: Dict[str, Any], methods: Dict[str, Any]
    ):
        super().__init__(type=ElementType.CLASS)
        self.class_name = class_name
        self.fields = fields
        self.methods = methods

    def __repr__(self):
        return f"{self.__class__.__name__}(class_name='{self.class_name}', fields={len(self.fields)}, methods={len(self.methods)})"

    def __str__(self):
        return f"<class {self.class_name}>"

    def __eq__(self, other) -> bool:
        if isinstance(other, ClassObject):
            return self.class_name == other.class_name and self.fields == other.fields
        return False

    def __copy__(self):
        return ClassObject(self.class_name, self.fields.copy(), self.methods.copy())

    def __deepcopy__(self, memo):
        if id(self) in memo:
            return memo[id(self)]

        # Create a new instance
        result = ClassObject(self.class_name, {}, {})
        memo[id(self)] = result

        # Deep copy fields and methods
        for key, value in self.fields.items():
            result.fields[key] = copy.deepcopy(value, memo)

        for key, value in self.methods.items():
            result.methods[key] = copy.deepcopy(value, memo)

        return result


class InstanceObject(Object):
    def __init__(self, class_name: str, fields: Dict[str, Any]):
        super().__init__(type=ElementType.INSTANCE)
        self.class_name = class_name
        self.fields = fields

    def __repr__(self):
        fields_str = ", ".join([f"{k}: {v}" for k, v in self.fields.items()])
        return f"{self.__class__.__name__}(class_name='{self.class_name}', fields={{{fields_str}}})"

    def __str__(self):
        return f"<instance of {self.class_name}>"

    def __getitem__(self, field_name):
        if field_name in self.fields:
            return self.fields[field_name]
        raise KeyError(
            f"Field '{field_name}' not found in instance of {self.class_name}"
        )

    def __setitem__(self, field_name, value):
        if field_name in self.fields:
            self.fields[field_name] = value
        else:
            raise KeyError(
                f"Field '{field_name}' not found in instance of {self.class_name}"
            )

    def __eq__(self, other) -> bool:
        if isinstance(other, InstanceObject):
            return self.class_name == other.class_name and self.fields == other.fields
        return False

    def __copy__(self):
        return InstanceObject(self.class_name, self.fields.copy())

    def __deepcopy__(self, memo):
        if id(self) in memo:
            return memo[id(self)]

        # Create a new instance with empty fields
        result = InstanceObject(self.class_name, {})
        memo[id(self)] = result

        # Deep copy the fields
        for key, value in self.fields.items():
            result.fields[key] = copy.deepcopy(value, memo)

        return result
