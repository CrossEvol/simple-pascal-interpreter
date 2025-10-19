from __future__ import annotations

from typing import TYPE_CHECKING

from spi.ast import ArrayType, PrimitiveType, RecordType, Type
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

from typing import List, Optional


class UnionSet:
    def __init__(self, canonicals: Optional[List[str]] = None) -> None:
        """
        初始化 UnionSet，用于类型别名解析。
        :param canonicals: 可选，最终类型列表（如 ["Integer", "String"]），每个为独立根。
        """
        self.parent: dict[str, str] = {}  # 父节点映射

        # 添加最终类型
        if canonicals:
            for canon in canonicals:
                self.add_canonical(canon)

    def add_canonical(self, canonical: str) -> None:
        """
        添加最终类型：如果不存在，则作为新独立根添加。
        :param canonical: 最终类型字符串
        """
        if not isinstance(canonical, str):
            raise ValueError("最终类型必须是字符串")
        if canonical in self.parent:
            return  # 已存在，忽略

        self.parent[canonical] = canonical

    def find(self, key: str) -> str:
        """
        查找 key 的根节点（最终类型，路径压缩）。
        :param key: 类型名（别名或最终类型）
        :return: 最终类型字符串
        """
        if key not in self.parent:
            # 如果 key 不存在，视作新最终类型
            self.add_canonical(key)
            return key

        if self.parent[key] != key:
            self.parent[key] = self.find(self.parent[key])
        return self.parent[key]

    def union(self, key1: str, key2: str) -> None:
        """
        合并 key1 和 key2 所属的集合（按秩合并）。
        :param key1: 第一个类型名
        :param key2: 第二个类型名
        """
        root1 = self.find(key1)
        root2 = self.find(key2)
        if root1 == root2:
            return

        self.parent[root1] = root2

    def add_alias(self, alias: str, canonical: str) -> None:
        """
        添加别名：将 alias 合并到最终类型 canonical。
        :param alias: 别名字符串
        :param canonical: 最终类型字符串
        """
        self.union(alias, canonical)

    def resolve_type(self, key: str) -> str:
        """
        解析类型别名，返回最终类型。
        :param key: 类型名
        :return: 最终类型字符串
        """
        return self.find(key)

    def is_connected(self, key1: str, key2: str) -> bool:
        """
        检查两个类型是否解析到同一最终类型。
        :param key1: 第一个类型名
        :param key2: 第二个类型名
        :return: bool
        """
        return self.find(key1) == self.find(key2)


class ObjectFactory:
    def __init__(self, interpreter: "Interpreter"):
        self.interpreter = interpreter
        # 类型别名映射表：alias_name -> actual_type_node
        self.type_aliases: dict[str, Type] = {}
        # UnionSet for efficient type alias resolution
        self.type_union_set = UnionSet()
        # 枚举类型和值的注册表（模仿semantic analyzer的实现）
        self.enum_types: dict[
            str, dict
        ] = {}  # { type_name -> { 'values': [value_names...], 'size': int } }
        self.enum_values: dict[
            str, dict
        ] = {}  # { value_name -> { 'type': type_name, 'ordinal': int } }
        self.record_types: dict[str, RecordType] = {}

    def add_type_alias(self, alias_name: str, actual_type: Type) -> None:
        """添加类型别名，将别名映射到实际类型，并在UnionSet中建立关系"""
        # 存储别名到实际类型节点的映射
        self.type_aliases[alias_name] = actual_type

        # 如果实际类型有value属性（即有类型名），则在UnionSet中建立关系
        if actual_type.value:
            # Use the type name from the actual_type as the canonical type in UnionSet
            self.type_union_set.add_alias(alias_name, actual_type.value)

    def resolve_type_alias(self, type_node: Type) -> Type:
        """解析类型别名，使用UnionSet来高效地追随别名链直到找到实际类型"""
        type_name = type_node.value

        # 使用UnionSet来查找类型的当前解析结果
        current_resolved = self.type_union_set.resolve_type(type_name)

        # 如果当前解析结果在别名表中，继续递归解析
        if current_resolved in self.type_aliases:
            actual_type = self.type_aliases[current_resolved]

            # 递归解析，获取最终的实际类型
            final_resolved_type = self.resolve_type_alias(actual_type)

            # 实现路径压缩：将原始类型名直接连接到最终类型名
            # 这样下次查询时会更快
            if final_resolved_type.value in self.type_aliases:
                self.type_union_set.add_alias(type_name, final_resolved_type.value)

            return final_resolved_type

        # 如果不是别名或者已经是实际类型，直接返回
        return type_node

    def enum_obj(self, type_name: str, ordinal: int) -> EnumObject:
        """根据ordinal反查名称创建枚举对象"""
        # 首先尝试从注册的枚举类型中查找名称
        if (
            type_name in self.enum_types
            and ordinal < self.enum_types[type_name]["size"]
        ):
            name = self.enum_types[type_name]["values"][ordinal]
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
        """根据类型节点创建复杂类型对象，使用interpreter的属性信息和UnionSet处理类型别名"""
        # # 处理数组类型
        if isinstance(type_node, ArrayType):
            return self.initArray(type_node)

        # 处理基本类型
        if isinstance(type_node, PrimitiveType):
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
        type_name = type_node.value

        # 使用UnionSet解析类型别名，找到最终类型名
        resolved_type_name = self.type_union_set.resolve_type(type_name)

        # 检查是否是枚举类型 (使用解析后的类型名)
        if resolved_type_name in self.enum_types:
            return self.enum_obj(resolved_type_name, 0)  # 使用第一个枚举值

        # 检查是否是记录类型 (使用解析后的类型名)
        if resolved_type_name in self.record_types:
            record_type = self.record_types[resolved_type_name]
            nested_record_obj = RecordObject(record_type)
            # 递归初始化嵌套记录的复杂字段
            self.initialize_record_complex_fields(nested_record_obj)
            return nested_record_obj

        # 检查是否是类型别名，然后递归解析
        if type_name in self.type_aliases:
            actual_type = self.type_aliases[type_name]
            return self.create_complex_object_from_type_node(actual_type)
        elif (
            resolved_type_name != type_name and resolved_type_name in self.type_aliases
        ):
            # 如果类型名经过解析后发生变化，且解析后的类型名在别名表中
            actual_type = self.type_aliases[resolved_type_name]
            return self.create_complex_object_from_type_node(actual_type)

    def post_initialize_array_elements(
        self, array_obj: ArrayObject, element_type_node: Type
    ) -> None:
        """为包含复杂类型元素的数组后初始化元素"""
        # 为静态数组的每个位置创建适当的对象，替换NullObject
        for index in range(array_obj.lower_bound, array_obj.upper_bound + 1):
            if array_obj.element_type == ElementType.RECORD:
                # 为记录类型创建对象
                type_name = element_type_node.value
                # 使用UnionSet解析类型别名
                resolved_type_name = self.type_union_set.resolve_type(type_name)
                if resolved_type_name in self.record_types:
                    record_type = self.record_types[resolved_type_name]
                    record_obj = RecordObject(record_type)
                    self.initialize_record_complex_fields(record_obj)
                    array_obj.value[index] = record_obj
            elif array_obj.element_type == ElementType.CUSTOM:
                # 为其他自定义类型创建对象
                type_name = element_type_node.value
                # 使用UnionSet解析类型别名
                resolved_type_name = self.type_union_set.resolve_type(type_name)

                if resolved_type_name in self.enum_types:
                    # 枚举类型，使用第一个枚举值
                    enum_obj = self.enum_obj(resolved_type_name, 0)
                    array_obj.value[index] = enum_obj
                elif type_name in self.type_aliases:
                    # 处理类型别名
                    actual_type = self.type_aliases[type_name]
                    complex_obj = self.create_complex_object_from_type_node(actual_type)
                    array_obj.value[index] = complex_obj
                elif (
                    resolved_type_name != type_name
                    and resolved_type_name in self.type_aliases
                ):
                    # 如果类型名经过解析后发生变化，且解析后的类型名在别名表中
                    actual_type = self.type_aliases[resolved_type_name]
                    complex_obj = self.create_complex_object_from_type_node(actual_type)
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
            if type_name in self.record_types:
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
