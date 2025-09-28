from __future__ import annotations

from typing import TYPE_CHECKING, cast

from spi.ast import PrimitiveType, VariantPart
from spi.error import ErrorCode
from spi.symbol import (
    BOOLEAN_TYPE_SYMBOL,
    CHAR_TYPE_SYMBOL,
    INTEGER_TYPE_SYMBOL,
    NEVER_SYMBOL,
    REAL_TYPE_SYMBOL,
    STRING_TYPE_SYMBOL,
    EnumTypeSymbol,
    RecordFieldSymbol,
    Symbol,
    TypeSymbol,
    VariantPartSymbol,
)

if TYPE_CHECKING:
    from spi.semantic_analyzer import SemanticAnalyzer


class SemanticAnalyzerHelper:
    def __init__(self, semantic_analyzer: "SemanticAnalyzer"):
        self.semantic_analyzer = semantic_analyzer

    def get_literal_type(self, value) -> str:
        """Get the type name for a literal value"""
        # Check bool first since bool is a subclass of int in Python
        if isinstance(value, bool):
            return "BOOLEAN"
        elif isinstance(value, int):
            return "INTEGER"
        elif isinstance(value, str) and len(value) == 1:
            return "CHAR"
        elif self.semantic_analyzer.enum_values[value]:
            return self.semantic_analyzer.enum_values[value]["type"]
        else:
            return None

    def check_types_compatible(self, case_type, label_type) -> bool:
        """Check if case expression type and label type are compatible"""
        if case_type is None or label_type is None:
            return False
        return case_type == label_type

    def resolve_type_alias(self, symbol: Symbol) -> Symbol:
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

    def to_type_symbol(self, symbol: Symbol) -> TypeSymbol:
        """Convert a Symbol to TypeSymbol"""
        if isinstance(symbol, TypeSymbol):
            return symbol
        elif isinstance(symbol, PrimitiveType):
            return self.semantic_analyzer.visit_PrimitiveType(symbol)
        elif hasattr(symbol, "name"):
            name = symbol.name.upper()
            if name == "INTEGER":
                return INTEGER_TYPE_SYMBOL
            elif name == "REAL":
                return REAL_TYPE_SYMBOL
            elif name == "BOOLEAN":
                return BOOLEAN_TYPE_SYMBOL
            elif name == "CHAR":
                return CHAR_TYPE_SYMBOL
            elif name == "STRING" or name.startswith("STRING["):
                return STRING_TYPE_SYMBOL
            else:
                return NEVER_SYMBOL
        else:
            return NEVER_SYMBOL

    def get_variable_type(self, var_symbol: Symbol) -> TypeSymbol:
        """Get the TypeSymbol for a variable symbol"""
        if var_symbol is None or var_symbol.type is None:
            return NEVER_SYMBOL

        # Convert to TypeSymbol if needed
        if isinstance(var_symbol.type, TypeSymbol):
            return var_symbol.type.resolve_final_type()
        else:
            return self.to_type_symbol(var_symbol.type)

    def process_variant_part(
        self, variant_part: VariantPart, existing_fields: dict[str, Symbol]
    ) -> VariantPartSymbol:
        """处理记录的变体部分"""
        tag_field_name = variant_part.tag_field.value

        tag_field_symbol: TypeSymbol = NEVER_SYMBOL
        for field_type_symbol in existing_fields.values():
            if tag_field_name == field_type_symbol.type.name:
                tag_field_symbol = field_type_symbol
                break

        # 检查标签字段是否是已定义的枚举类型
        if tag_field_symbol is NEVER_SYMBOL:
            self.semantic_analyzer.error(
                error_code=ErrorCode.SEMANTIC_RECORD_VARIANT_INVALID_TAG_ERROR,
                token=variant_part.tag_field.token,
            )

        if not isinstance(tag_field_symbol.type, EnumTypeSymbol):
            self.semantic_analyzer.error(
                error_code=ErrorCode.SEMANTIC_ERROR, token=variant_part.tag_field.token
            )

        # 处理变体情况
        variant_cases = {}
        for variant_case in variant_part.variant_cases:
            # 检查标签值是否有效
            for tag_value in variant_case.tag_values:
                if tag_value not in tag_field_symbol.type.values:
                    self.semantic_analyzer.error(
                        error_code=ErrorCode.SEMANTIC_ERROR,
                        token=variant_part.tag_field.token,
                    )

            # 处理变体字段
            variant_fields = {}
            for field in variant_case.fields:
                field_name = field.name.value
                field_type = cast(Symbol, self.semantic_analyzer.visit(field.type_node))

                # 创建字段符号并添加到变体字段中
                field_symbol = RecordFieldSymbol(field_name, field_type)
                variant_fields[field_name] = field_symbol

            # 将变体字段添加到对应的标签值
            for tag_value in variant_case.tag_values:
                variant_cases[tag_value] = variant_fields

        return VariantPartSymbol(tag_field_name, tag_field_symbol.type, variant_cases)

    def get_record_type_counter(self) -> int:
        """获取记录类型计数器，用于生成唯一的记录类型名称"""
        if not hasattr(self, "_record_type_counter"):
            self.semantic_analyzer._record_type_counter = 0
        self.semantic_analyzer._record_type_counter += 1
        return self.semantic_analyzer._record_type_counter

    def check_set_element_types_compatible(
        self, type1: TypeSymbol, type2: TypeSymbol
    ) -> bool:
        """Check if two types are compatible for set elements"""
        if type1 is None or type2 is None:
            return False

        # Same type is always compatible (use 'is' for identity comparison)
        if type1 is type2:
            return True

        # Check by name for basic types
        type1_name = getattr(type1, "name", str(type1))
        type2_name = getattr(type2, "name", str(type2))

        if type1_name == type2_name:
            return True

        # Integer and real are compatible in sets
        if (type1_name == "INTEGER" and type2_name == "REAL") or (
            type1_name == "REAL" and type2_name == "INTEGER"
        ):
            return True

        # Different basic types are not compatible
        return False
