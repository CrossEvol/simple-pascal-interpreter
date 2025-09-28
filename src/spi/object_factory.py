from __future__ import annotations

from typing import TYPE_CHECKING

from spi.ast import ArrayType, Type
from spi.constants import ElementType
from spi.error import ErrorCode, InterpreterError
from spi.object import (
    ArrayObject,
    BooleanObject,
    CharObject,
    EnumObject,
    IntegerObject,
    NullObject,
    Object,
    RealObject,
    RecordObject,
    StringObject,
    SubrangeObject,
)
from spi.token import TokenType

if TYPE_CHECKING:
    from spi.interpreter import Interpreter


class ObjectFactory:
    def __init__(self, interpreter: "Interpreter"):
        self.interpreter = interpreter

    def resolve_type_alias(self, type_node: Type) -> Type:
        """解析类型别名，追随别名链直到找到实际类型"""
        if hasattr(type_node, "value"):
            type_name = type_node.value
            # 检查是否是类型别名
            if type_name in self.interpreter.type_aliases:
                # 递归解析别名链
                return self.resolve_type_alias(self.interpreter.type_aliases[type_name])

        # 如果不是别名或者已经是实际类型，直接返回
        return type_node

    def enum_obj(self, type_name: str, ordinal: int) -> EnumObject:
        """根据ordinal反查名称创建枚举对象"""
        # 首先尝试从注册的枚举类型中查找名称
        if (
            type_name in self.interpreter.enum_types
            and ordinal < self.interpreter.enum_types[type_name]["size"]
        ):
            name = self.interpreter.enum_types[type_name]["values"][ordinal]
            return EnumObject(type_name, name, ordinal)
        else:
            raise InterpreterError(error_code=ErrorCode.INTERPRETER_UNKNOWN_ENUM)

    def initialize_record_complex_fields(self, record_obj: RecordObject) -> None:
        """使用Interpreter的属性信息初始化RecordObject中的复杂类型字段"""
        for field in record_obj.record_type.fields:
            field_name = field.name.value
            field_type = field.type_node

            # 获取已创建的默认对象
            current_obj = record_obj.fields.get(field_name, NullObject())

            # 如果是空对象，尝试使用interpreter信息初始化
            if isinstance(current_obj, NullObject):
                new_obj = self.create_complex_object_from_type_node(field_type)
                if not isinstance(new_obj, NullObject):
                    record_obj[field_name] = new_obj

    def create_complex_object_from_type_node(self, type_node) -> Object:
        """根据类型节点创建复杂类型对象，使用interpreter的属性信息"""
        # 直接检查是否是ArrayType实例
        if (
            hasattr(type_node, "__class__")
            and type_node.__class__.__name__ == "ArrayType"
        ):
            return self.initArray(type_node)

        # 处理数组类型
        if hasattr(type_node, "token") and type_node.token.type == "ARRAY":
            return self.initArray(type_node)

        # 处理基本类型
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

        # 处理自定义类型（枚举、记录等）
        if hasattr(type_node, "value"):
            type_name = type_node.value

            # 检查是否是枚举类型
            if type_name in self.interpreter.enum_types:
                return self.enum_obj(type_name, 0)  # 使用第一个枚举值

            # 检查是否是记录类型
            if type_name in self.interpreter.record_types:
                record_type = self.interpreter.record_types[type_name]
                nested_record_obj = RecordObject(record_type)
                # 递归初始化嵌套记录的复杂字段
                self.initialize_record_complex_fields(nested_record_obj)
                return nested_record_obj

            # 检查是否是类型别名，然后递归解析
            if type_name in self.interpreter.type_aliases:
                actual_type = self.interpreter.type_aliases[type_name]
                return self.create_complex_object_from_type_node(actual_type)

        # 其他情况返回空对象
        return NullObject()

    def post_initialize_array_elements(
        self, array_obj: ArrayObject, element_type_node: Type
    ) -> None:
        """为包含复杂类型元素的数组后初始化元素"""
        # 为静态数组的每个位置创建适当的对象，替换NullObject
        for index in range(array_obj.lower_bound, array_obj.upper_bound + 1):
            if array_obj.element_type == ElementType.RECORD:
                # 为记录类型创建对象
                if hasattr(element_type_node, "value"):
                    type_name = element_type_node.value
                    if type_name in self.interpreter.record_types:
                        record_type = self.interpreter.record_types[type_name]
                        record_obj = RecordObject(record_type)
                        self.initialize_record_complex_fields(record_obj)
                        array_obj.value[index] = record_obj
            elif array_obj.element_type == ElementType.CUSTOM:
                # 为其他自定义类型创建对象
                if hasattr(element_type_node, "value"):
                    type_name = element_type_node.value
                    if type_name in self.interpreter.enum_types:
                        # 枚举类型，使用第一个枚举值
                        enum_obj = self.enum_obj(type_name, 0)
                        array_obj.value[index] = enum_obj
                    elif type_name in self.interpreter.type_aliases:
                        # 处理类型别名
                        actual_type = self.interpreter.type_aliases[type_name]
                        complex_obj = self.create_complex_object_from_type_node(
                            actual_type
                        )
                        array_obj.value[index] = complex_obj

    def initArray(self, node: ArrayType) -> ArrayObject:
        # Get bounds from SubrangeType if available, otherwise use backward compatibility
        if node.bounds:
            lower_bound: int = self.interpreter.visit(node.bounds.lower).value
            upper_bound: int = self.interpreter.visit(node.bounds.upper).value
        else:
            # For dynamic arrays, use default bounds
            lower_bound: int = 0
            upper_bound: int = 0

        if not node.dynamic and lower_bound > upper_bound:
            raise InterpreterError(
                error_code=ErrorCode.INTERPRETER_ARRAY_RANGE_INVALID,
                token=node.token,
                message=f"{ErrorCode.INTERPRETER_ARRAY_RANGE_INVALID.value} -> {node.token}",
            )

        # Determine element type
        element_type = ElementType.INTEGER  # default

        # Check if element_type has a token attribute for basic types
        if hasattr(node.element_type, "token"):
            if node.element_type.token.type == TokenType.BOOLEAN:
                element_type = ElementType.BOOL
            elif node.element_type.token.type == TokenType.INTEGER:
                element_type = ElementType.INTEGER
            elif node.element_type.token.type == TokenType.REAL:
                element_type = ElementType.REAL
            elif node.element_type.token.type == TokenType.STRING:
                element_type = ElementType.STRING
            elif node.element_type.token.type == TokenType.CHAR:
                element_type = ElementType.CHAR
            elif node.element_type.token.type == TokenType.ARRAY:
                element_type = ElementType.ARRAY
            elif node.element_type.token.type == TokenType.ID:
                # Custom type (record, enum, etc.)
                # Check if it's a record type first
                type_name = node.element_type.value
                if type_name in self.interpreter.record_types:
                    element_type = ElementType.RECORD
                else:
                    # Could be enum or other custom type
                    element_type = ElementType.CUSTOM
        else:
            # For complex types without simple token, treat as custom
            element_type = ElementType.CUSTOM

        # Create SubrangeObject for bounds checking if we have bounds
        bounds_subrange = (
            SubrangeObject(lower_bound, upper_bound)
            if node.bounds and not node.dynamic
            else SubrangeObject(0, 0)
        )

        # Create array with the determined element type
        array_obj = ArrayObject(
            element_type=element_type,
            dynamic=node.dynamic,
            bounds_subrange=bounds_subrange,
        )

        # For record and custom types, we need to post-initialize elements
        if (
            element_type in (ElementType.RECORD, ElementType.CUSTOM)
            and not node.dynamic
        ):
            self.post_initialize_array_elements(array_obj, node.element_type)

        return array_obj
