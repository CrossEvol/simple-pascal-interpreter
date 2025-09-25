"""SPI - Simple Pascal Interpreter. Part 19"""

from __future__ import annotations

from typing import cast

from spi.ast import (
    Param,
    RecordType,
    Type,
)
from spi.constants import ElementType
from spi.error import (
    ErrorCode,
    InterpreterError,
)
from spi.token import TokenType

###############################################################################
#                                                                             #
#  OBJECT SYSTEM                                                              #
#                                                                             #
###############################################################################


class Object:
    """Base class for all Pascal values in the interpreter"""

    def __init__(self, value=None):
        self.value = value

    def __str__(self):
        return str(self.value)

    def __repr__(self):
        return f"{self.__class__.__name__}({self.value})"

    def to_bool(self):
        """Convert to boolean for conditional expressions"""
        return bool(self.value)


class NullObject(Object):
    """Default null/empty type object"""

    def __init__(self):
        super().__init__(None)

    def __str__(self):
        return "None"

    def __repr__(self):
        return "NullObject()"

    def to_bool(self):
        """Convert to boolean for conditional expressions"""
        return False


class NumberObject(Object):
    """Base class for numeric types"""

    def __add__(self, other) -> Object:
        if isinstance(other, NumberObject):
            return self._create_result(self.value + other.value)
        return NotImplemented

    def __sub__(self, other) -> Object:
        if isinstance(other, NumberObject):
            return self._create_result(self.value - other.value)
        return NotImplemented

    def __mul__(self, other) -> Object:
        if isinstance(other, NumberObject):
            return self._create_result(self.value * other.value)
        return NotImplemented

    def __truediv__(self, other) -> Object:
        if isinstance(other, NumberObject):
            return RealObject(float(self.value) / float(other.value))
        return NotImplemented

    def __floordiv__(self, other) -> Object:
        if isinstance(other, NumberObject):
            return self._create_result(self.value // other.value)
        return NotImplemented

    def __pos__(self) -> Object:
        return self._create_result(+self.value)

    def __neg__(self) -> Object:
        return self._create_result(-self.value)

    def __lt__(self, other) -> Object:
        if isinstance(other, NumberObject):
            return BooleanObject(self.value < other.value)
        return NotImplemented

    def __le__(self, other) -> Object:
        if isinstance(other, NumberObject):
            return BooleanObject(self.value <= other.value)
        return NotImplemented

    def __gt__(self, other) -> Object:
        if isinstance(other, NumberObject):
            return BooleanObject(self.value > other.value)
        return NotImplemented

    def __ge__(self, other) -> Object:
        if isinstance(other, NumberObject):
            return BooleanObject(self.value >= other.value)
        return NotImplemented

    def __eq__(self, other):
        if isinstance(other, NumberObject):
            return self.value == other.value
        return NotImplemented

    def __ne__(self, other):
        if isinstance(other, NumberObject):
            return self.value != other.value
        return NotImplemented

    def _create_result(self, value) -> Object:
        """Create appropriate result type based on value"""
        if isinstance(value, int):
            return IntegerObject(value)
        else:
            return RealObject(value)


class IntegerObject(NumberObject):
    """Integer value object"""

    def __init__(self, value: int = 0):
        super().__init__(int(value))

    def _create_result(self, value) -> Object:
        if isinstance(value, int):
            return IntegerObject(value)
        else:
            return RealObject(value)


class RealObject(NumberObject):
    """Real/Float value object"""

    def __init__(self, value: float = 0.0):
        super().__init__(float(value))

    def _create_result(self, value) -> Object:
        return RealObject(float(value))


class BooleanObject(Object):
    """Boolean value object"""

    def __init__(self, value: bool = False):
        super().__init__(bool(value))

    def __and__(self, other) -> Object:
        if isinstance(other, BooleanObject):
            return BooleanObject(self.value and other.value)
        return NotImplemented

    def __or__(self, other) -> Object:
        if isinstance(other, BooleanObject):
            return BooleanObject(self.value or other.value)
        return NotImplemented

    def __invert__(self) -> Object:
        return BooleanObject(not self.value)

    def __eq__(self, other):
        if isinstance(other, BooleanObject):
            return BooleanObject(self.value == other.value)
        return NotImplemented

    def __ne__(self, other):
        if isinstance(other, BooleanObject):
            return BooleanObject(self.value != other.value)
        return NotImplemented


class StringObject(Object):
    """String value object"""

    def __init__(self, value: str = "", limit: int = -1):
        if limit > 0 and len(value) > limit:
            value = value[:limit]
        super().__init__(str(value))
        self.limit = limit

    def __add__(self, other) -> Object:
        if isinstance(other, StringObject):
            result_value = self.value + other.value
            # Don't apply limits during concatenation operations
            return StringObject(result_value, -1)
        return NotImplemented

    def __getitem__(self, index) -> Object:
        """Get character at index (1-based indexing for Pascal)"""
        if 1 <= index <= len(self.value):
            return CharObject(self.value[index - 1])
        return CharObject("")

    def __setitem__(self, index: int, value: Object) -> None:
        """Set character at index (1-based indexing for Pascal)"""
        if 1 <= index <= len(self.value):
            # Convert value to string character
            if isinstance(value, CharObject):
                char_value = value.value
            elif isinstance(value, StringObject):
                char_value = value.value[:1] if value.value else ""
            else:
                char_value = str(value)[:1] if hasattr(value, "value") else ""

            # Modify the string at the specified index
            value_list = list(self.value)
            value_list[index - 1] = char_value
            self.value = "".join(value_list)

    def __len__(self):
        return len(self.value)

    def set_length(self, new_length: int):
        """Set new length for string"""
        if new_length < len(self.value):
            self.value = self.value[:new_length]
        # Note: Pascal strings don't automatically extend with spaces


class CharObject(Object):
    """Character value object"""

    def __init__(self, value: str = ""):
        super().__init__(str(value)[:1] if value else "")

    def __lt__(self, other) -> Object:
        """Less than comparison based on ASCII value"""
        if isinstance(other, CharObject):
            return BooleanObject(ord(self.value) < ord(other.value))
        return NotImplemented

    def __gt__(self, other) -> Object:
        """Greater than comparison based on ASCII value"""
        if isinstance(other, CharObject):
            return BooleanObject(ord(self.value) > ord(other.value))
        return NotImplemented

    def __eq__(self, other) -> bool:
        """Equal comparison based on ASCII value"""
        if isinstance(other, CharObject):
            return ord(self.value) == ord(other.value)
        return NotImplemented

    def __ne__(self, other) -> bool:
        """Not equal comparison based on ASCII value"""
        if isinstance(other, CharObject):
            return ord(self.value) != ord(other.value)
        return NotImplemented

    def __le__(self, other) -> Object:
        """Less than or equal comparison based on ASCII value"""
        if isinstance(other, CharObject):
            return BooleanObject(ord(self.value) <= ord(other.value))
        return NotImplemented

    def __ge__(self, other) -> Object:
        """Greater than or equal comparison based on ASCII value"""
        if isinstance(other, CharObject):
            return BooleanObject(ord(self.value) >= ord(other.value))
        return NotImplemented


class SubrangeObject(Object):
    """Runtime object representing a subrange type like 1..10"""

    def __init__(self, lower: int, upper: int):
        super().__init__((lower, upper))
        self.lower = lower
        self.upper = upper

    def contains(self, value: int) -> bool:
        """Check if value is within the subrange bounds"""
        return self.lower <= value <= self.upper

    def to_set(self) -> set[int]:
        """Convert subrange to a set of all values in the range"""
        return set(range(self.lower, self.upper + 1))

    def __str__(self):
        return f"{self.lower}..{self.upper}"

    def __repr__(self):
        return f"SubrangeObject({self.lower}, {self.upper})"


class ArrayObject(Object):
    """Array value object"""

    def __init__(
        self,
        element_type: ElementType,
        dynamic: bool = False,
        bounds_subrange: SubrangeObject = SubrangeObject(lower=0, upper=0),
    ):
        super().__init__({})
        self.element_type = element_type
        self.dynamic = dynamic
        self.bounds_subrange = (
            bounds_subrange  # Optional SubrangeObject for bounds checking
        )

        lower_bound = bounds_subrange.lower
        upper_bound = bounds_subrange.upper
        # Initialize static arrays
        if not dynamic and lower_bound <= upper_bound:
            for i in range(lower_bound, upper_bound + 1):
                self.value[i] = self._create_default_element()

    def _create_default_element(self):
        """Create default element based on element type"""
        if self.element_type == ElementType.INTEGER:
            return IntegerObject(0)
        elif self.element_type == ElementType.REAL:
            return RealObject(0.0)
        elif self.element_type == ElementType.BOOL:
            return BooleanObject(False)
        elif self.element_type == ElementType.STRING:
            return StringObject("")
        elif self.element_type == ElementType.CHAR:
            return CharObject("")
        elif self.element_type == ElementType.ARRAY:
            return ArrayObject(
                element_type=ElementType.INTEGER,
                dynamic=True,
                bounds_subrange=SubrangeObject(lower=0, upper=0),
            )  # Default nested array
        elif self.element_type == ElementType.RECORD:
            # For record elements, return NullObject - will be initialized by interpreter
            return NullObject()
        elif self.element_type == ElementType.CUSTOM:
            # For custom types, return NullObject - will be initialized by interpreter
            return NullObject()
        else:
            return Object()

    @property
    def lower_bound(self) -> int:
        return self.bounds_subrange.lower

    @property
    def upper_bound(self) -> int:
        return self.bounds_subrange.upper

    def __getitem__(self, index):
        """Get element at index"""
        # For static arrays, check bounds using SubrangeObject if available
        if not self.dynamic and self.bounds_subrange:
            if not self.bounds_subrange.contains(index):
                raise InterpreterError(
                    error_code=ErrorCode.INTERPRETER_ARRAY_INDEX_OUT_OF_BOUNDS,
                    token=None,
                    message=f"Array index {index} out of bounds {self.bounds_subrange}",
                )

        if index in self.value:
            return self.value[index]
        else:
            # Return default value for out-of-bounds access
            return self._create_default_element()

    def __setitem__(self, index, value):
        """Set element at index"""
        # For static arrays, check bounds using SubrangeObject if available
        if not self.dynamic and self.bounds_subrange:
            if not self.bounds_subrange.contains(index):
                raise InterpreterError(
                    error_code=ErrorCode.INTERPRETER_ARRAY_INDEX_OUT_OF_BOUNDS,
                    token=None,
                    message=f"Array index {index} out of bounds {self.bounds_subrange}",
                )

        self.value[index] = value

    def __len__(self):
        """Return length of array"""
        if self.dynamic:
            return len(self.value)
        else:
            return self.upper_bound - self.lower_bound + 1

    def set_length(self, new_length: int):
        """Set new length for dynamic array"""
        if not self.dynamic:
            raise InterpreterError(
                error_code=ErrorCode.INTERPRETER_STATIC_ARRAY_MODIFY_LENGTH,
                token=None,
                message="Cannot modify length of static array",
            )

        # Add new elements if extending
        for i in range(len(self.value), new_length):
            self.value[i] = self._create_default_element()

        # Remove elements if shrinking
        if new_length < len(self.value):
            keys_to_remove = [k for k in self.value.keys() if k >= new_length]
            for k in keys_to_remove:
                del self.value[k]


class EnumObject(Object):
    def __init__(self, type_name: str, name: str, ordinal: int):
        super().__init__(ordinal)  # self.value 存放 ordinal
        self.type_name = type_name  # 枚举类型名
        self.name = name  # 枚举值名称
        self.ordinal = ordinal

    def __str__(self):
        # 打印时输出"名称"，而非序号
        return self.name

    # 比较基于 ordinal
    def __eq__(self, other) -> bool:
        return (
            isinstance(other, EnumObject)
            and self.type_name == other.type_name
            and self.ordinal == other.ordinal
        )

    def __ne__(self, other) -> bool:
        return not self.__eq__(other)

    def __lt__(self, other) -> Object:
        if isinstance(other, EnumObject) and self.type_name == other.type_name:
            return BooleanObject(self.ordinal < other.ordinal)
        return NotImplemented

    def __le__(self, other) -> Object:
        if isinstance(other, EnumObject) and self.type_name == other.type_name:
            return BooleanObject(self.ordinal <= other.ordinal)
        return NotImplemented

    def __gt__(self, other) -> Object:
        if isinstance(other, EnumObject) and self.type_name == other.type_name:
            return BooleanObject(self.ordinal > other.ordinal)
        return NotImplemented

    def __ge__(self, other) -> Object:
        if isinstance(other, EnumObject) and self.type_name == other.type_name:
            return BooleanObject(self.ordinal >= other.ordinal)
        return NotImplemented

    # 不支持算术运算（保持语义清晰）
    def __add__(self, other):
        return NotImplemented

    def __sub__(self, other):
        return NotImplemented

    def __mul__(self, other):
        return NotImplemented

    def __truediv__(self, other):
        return NotImplemented


class ProcedureObject(Object):
    """Runtime object representing a user-defined procedure"""

    def __init__(self, name: str, formal_params: list[Param], block_ast):
        super().__init__()
        self.name = name
        self.formal_params = formal_params  # List of Param AST nodes
        self.block_ast = block_ast

    def get_param_names(self) -> list[str]:
        """Get list of formal parameter names"""
        return [param.var_node.value for param in self.formal_params]

    def get_param_count(self) -> int:
        """Get number of formal parameters"""
        return len(self.formal_params)


class FunctionObject(Object):
    """Runtime object representing a user-defined function"""

    def __init__(self, name: str, formal_params: list[Param], return_type, block_ast):
        super().__init__()
        self.name = name
        self.formal_params = formal_params  # List of Param AST nodes (includes return variable as first param)
        self.return_type = return_type
        self.block_ast = block_ast

    def get_param_names(self) -> list[str]:
        """Get list of formal parameter names (excluding return variable)"""
        # Skip the first parameter which is the return variable
        return [param.var_node.value for param in self.formal_params[1:]]

    def get_param_count(self) -> int:
        """Get number of formal parameters (excluding return variable)"""
        # Subtract 1 to exclude the return variable parameter
        return len(self.formal_params) - 1 if len(self.formal_params) > 0 else 0


class SetObject(Object):
    """Runtime object representing a set of values"""

    def __init__(self, elements: set[int] = None):
        if elements is None:
            elements = set()
        super().__init__(elements)
        self.elements = elements

    def contains(self, value: int) -> bool:
        """Check if value is in the set"""
        return value in self.elements

    def add(self, value: int) -> None:
        """Add a value to the set"""
        self.elements.add(value)

    def remove(self, value: int) -> None:
        """Remove a value from the set"""
        self.elements.discard(value)

    def union(self, other: "SetObject") -> "SetObject":
        """Return union of this set with another set"""
        return SetObject(self.elements | other.elements)

    def intersection(self, other: "SetObject") -> "SetObject":
        """Return intersection of this set with another set"""
        return SetObject(self.elements & other.elements)

    def difference(self, other: "SetObject") -> "SetObject":
        """Return difference of this set with another set"""
        return SetObject(self.elements - other.elements)

    def is_empty(self) -> bool:
        """Check if the set is empty"""
        return len(self.elements) == 0

    def size(self) -> int:
        """Return the number of elements in the set"""
        return len(self.elements)

    def __str__(self):
        if self.is_empty():
            return "[]"
        sorted_elements = sorted(self.elements)
        return f"[{', '.join(map(str, sorted_elements))}]"

    def __repr__(self):
        return f"SetObject({self.elements})"


class RecordObject(Object):
    """表示记录对象，只需要record_type参数初始化"""

    def __init__(self, record_type: RecordType):
        super().__init__()
        self.fields: dict[str, Object] = {}  # 字段名到字段对象的映射
        self.record_type = record_type  # 记录类型模板，相当于元信息
        self.pending_fields: dict[str, Type] = {}

        # 初始化常规字段
        self._init_regular_fields()

    @property
    def pending_field_name(self) -> list[str]:
        return self.pending_fields.keys()

    def _init_regular_fields(self):
        """初始化常规字段"""
        for field in self.record_type.fields:
            field_name = field.name.value
            field_type = field.type_node
            self.fields[field_name] = self._create_default_object_from_type_node(
                field_type
            )

    def _create_default_object_from_type_node(self, type_node: Type) -> Object:
        """根据类型节点创建默认对象（仅处理基本类型）"""
        if hasattr(type_node, "token"):
            token_type = type_node.token.type
            if token_type == TokenType.INTEGER:
                return IntegerObject(0)
            elif token_type == TokenType.REAL:
                return RealObject(0.0)
            elif token_type == TokenType.BOOLEAN:
                return BooleanObject(False)
            elif token_type == TokenType.STRING:
                return StringObject("")
            elif token_type == TokenType.CHAR:
                return CharObject("")

        # 对于其他类型，返回空对象，等待后续通过interpreter初始化
        return NullObject()

    def _init_variant_fields(self, tag_value: str):
        """根据标签值初始化变体字段"""
        if not self.record_type.variant_part:
            return

        # 清除现有的变体字段
        variant_fields = set()
        for variant_case in self.record_type.variant_part.variant_cases:
            for field in variant_case.fields:
                variant_fields.add(field.name.value)

        for field_name in variant_fields:
            if field_name in self.fields:
                del self.fields[field_name]

        # 添加新的变体字段
        for variant_case in self.record_type.variant_part.variant_cases:
            if tag_value in variant_case.tag_values:
                for field in variant_case.fields:
                    field_name = field.name.value
                    default_object = self._create_default_object_from_type_node(
                        field.type_node
                    )
                    self.fields[field_name] = default_object
                    if isinstance(default_object, NullObject):
                        self.pending_fields[field_name] = field.type_node

    def __str__(self):
        fields_str = ", ".join(
            [f"{name}={value}" for name, value in self.fields.items()]
        )
        return f"Record({fields_str})"

    __repr__ = __str__

    def __getitem__(self, field_name: str):
        """获取字段值"""
        return self.fields.get(field_name, NullObject())

    def __setitem__(self, field_name: str, value: Object):
        """设置字段值"""
        # 检查字段是否在常规字段中
        is_valid_field = self._is_regular_field(field_name)

        # 检查是否是变体字段
        if not is_valid_field and self.record_type.variant_part:
            is_valid_field = self._is_variant_field(field_name)

        if is_valid_field:
            self.fields[field_name] = value
            if isinstance(value, EnumObject):
                enum_obj = cast(EnumObject, value)
                # 如果设置的是标签字段，需要重新初始化变体字段
                if (
                    self.record_type.variant_part
                    and enum_obj.type_name
                    == self.record_type.variant_part.tag_field.value
                ):
                    if hasattr(value, "value"):
                        # For enum objects, use the enum name, not the ordinal
                        if isinstance(value, EnumObject):
                            self._init_variant_fields(value.name)
                        else:
                            self._init_variant_fields(str(value.value))
            elif (
                self.record_type.variant_part
                and field_name == self.record_type.variant_part.tag_field.value
            ):
                if hasattr(value, "value"):
                    # For enum objects, use the enum name, not the ordinal
                    if isinstance(value, EnumObject):
                        self._init_variant_fields(value.name)
                    else:
                        self._init_variant_fields(str(value.value))
        else:
            raise KeyError(f"Field '{field_name}' not found in record")

    def _is_regular_field(self, field_name: str) -> bool:
        """检查是否是常规字段"""
        for field in self.record_type.fields:
            if field.name.value == field_name:
                return True
        return False

    def _is_variant_field(self, field_name: str) -> bool:
        """检查是否是变体字段"""
        if not self.record_type.variant_part:
            return False

        for variant_case in self.record_type.variant_part.variant_cases:
            for field in variant_case.fields:
                if field.name.value == field_name:
                    return True
        return False
