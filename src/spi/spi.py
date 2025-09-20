"""SPI - Simple Pascal Interpreter. Part 19"""

from __future__ import annotations

import argparse
import sys
from dataclasses import dataclass
from enum import Enum
from typing import Any, Callable, cast

_SHOULD_LOG_SCOPE = False  # see '--scope' command line option
_SHOULD_LOG_STACK = False  # see '--stack' command line option

RETURN_NUM_FOR_LENGTH = "RETURN_NUM_FOR_LENGTH"


class SpiUtil:
    @staticmethod
    def print_w(message: Any):
        print(f"\033[91m{message}\033[0m", file=sys.stderr)


class ElementType(Enum):
    INTEGER = "INTEGER"
    REAL = "REAL"
    BOOL = "BOOL"
    STRING = "STRING"
    CHAR = "CHAR"
    ARRAY = "ARRAY"
    RECORD = "RECORD"
    CUSTOM = "CUSTOM"


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


class ArrayObject(Object):
    """Array value object"""

    def __init__(
        self,
        element_type: ElementType,
        lower_bound: int = 0,
        upper_bound: int = 0,
        dynamic: bool = False,
    ):
        super().__init__({})
        self.element_type = element_type
        self.lower_bound = lower_bound
        self.upper_bound = upper_bound
        self.dynamic = dynamic

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
            return ArrayObject(ElementType.INTEGER, 0, 0, True)  # Default nested array
        elif self.element_type == ElementType.RECORD:
            # For record elements, return NullObject - will be initialized by interpreter
            return NullObject()
        elif self.element_type == ElementType.CUSTOM:
            # For custom types, return NullObject - will be initialized by interpreter
            return NullObject()
        else:
            return Object()

    def __getitem__(self, index):
        """Get element at index"""
        if index in self.value:
            return self.value[index]
        else:
            # Return default value for out-of-bounds access
            return self._create_default_element()

    def __setitem__(self, index, value):
        """Set element at index"""
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


class RecordObject(Object):
    """表示记录对象，只需要record_type参数初始化"""

    def __init__(self, record_type: RecordType):
        super().__init__()
        self.fields: dict[str, Object] = {}  # 字段名到字段对象的映射
        self.record_type = record_type  # 记录类型模板，相当于元信息

        # 初始化常规字段
        self._init_regular_fields()

    def _init_regular_fields(self):
        """初始化常规字段"""
        for field in self.record_type.fields:
            field_name = field.name.value
            field_type = field.type_node
            self.fields[field_name] = self._create_default_object_from_type_node(
                field_type
            )

    def _create_default_object_from_type_node(self, type_node) -> Object:
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
                    self.fields[field_name] = (
                        self._create_default_object_from_type_node(field.type_node)
                    )

    def __str__(self):
        fields_str = ", ".join(
            [f"{name}={value}" for name, value in self.fields.items()]
        )
        return f"Record({fields_str})"

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

            # 如果设置的是标签字段，需要重新初始化变体字段
            if (
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


class ErrorCode(Enum):
    # Common errors
    UNEXPECTED_TOKEN = "Unexpected token"
    ID_NOT_FOUND = "Identifier not found"
    DUPLICATE_ID = "Duplicate id found"
    NULL_POINTER = "Null pointer exception"
    CURRENT_SCOPE_NOT_FOUND = "Current scope not found"
    UNKNOWN_BIN_OP = "Unknown binary operator"
    UNKNOWN_UNARY_OP = "Unknown unary operator"
    MODIFY_LOOP_VAR_NOT_ALLOW = (
        "modify loop control variable is not allowed inside for-statement"
    )
    MISSING_CURRENT_SCOPE = "Missing current scope"
    SEMANTIC_ERROR = "Semantic error"

    # Lexer errors
    LEXER_INVALID_CHARACTER = "Lexer invalid character"
    LEXER_STRING_ERROR = "Lexer string error"

    # Parser errors
    PARSER_UNEXPECTED_TOKEN = "Parser unexpected token"

    # Semantic errors
    SEMANTIC_UNKNOWN_TYPE = "Semantic unknown type"
    SEMANTIC_UNKNOWN_ENUM = "Semantic unknown enum"
    SEMANTIC_UNKNOWN_ARRAY_ELEMENT_TYPE = "Semantic unknown array element type"
    SEMANTIC_UNKNOWN_SYMBOL = "Semantic unknown symbol"
    SEMANTIC_UNKNOWN_BOOLEAN = "Semantic unknown boolean"
    SEMANTIC_UNSUPPORTED_TYPE = "Semantic unsupported type"
    SEMANTIC_INCOMPATIBLE_TYPE = "Semantic incompatible type"
    SEMANTIC_CHAR_TOO_MANY_CHARS = "Semantic char too many characters"
    SEMANTIC_CHAR_INVALID_ASCII = "Semantic char invalid ASCII value"
    SEMANTIC_DUPLICATE_CASE_LABEL = "Semantic duplicate case label"

    # Interpreter errors
    INTERPRETER_STATIC_ARRAY_MODIFY_LENGTH = "Interpreter static array modify length"
    INTERPRETER_UNKNOWN_BUILTIN_FUNCTION = "Interpreter unknown builtin function"
    INTERPRETER_UNKNOWN_BUILTIN_PROCEDURE = "Interpreter unknown builtin procedure"
    INTERPRETER_ARRAY_RANGE_INVALID = "Interpreter array range invalid"
    INTERPRETER_UNKNOWN_OPERATOR = "Interpreter unknown operator"
    INTERPRETER_UNKNOWN_ENUM = "Interpreter unknown enum"
    INTERPRETER_UNKNOWN_BOOLEAN = "Interpreter unknown boolean"


class Error(Exception):
    def __init__(self, error_code=None, token=None, message=None) -> None:
        self.error_code = error_code
        self.token = token
        # add exception class name before the message
        self.message = f"{self.__class__.__name__}: {message}"


###############################################################################
#                                                                             #
#  LexerError                                                                 #
#                                                                             #
###############################################################################


class LexerError(Error):
    pass


###############################################################################
#                                                                             #
#  ParserError                                                                #
#                                                                             #
###############################################################################


class ParserError(Error):
    pass


###############################################################################
#                                                                             #
#  SemanticError                                                              #
#                                                                             #
###############################################################################


class SemanticError(Error):
    pass


###############################################################################
#                                                                             #
#  InterpreterError                                                           #
#                                                                             #
###############################################################################


class InterpreterError(Error):
    pass


###############################################################################
#                                                                             #
#  LEXER                                                                      #
#                                                                             #
###############################################################################


class TokenType(Enum):
    # single-character token types
    PLUS = "+"
    MINUS = "-"
    MUL = "*"
    FLOAT_DIV = "/"
    LPAREN = "("
    RPAREN = ")"
    LBRACKET = "["
    RBRACKET = "]"
    SEMI = ";"
    DOT = "."
    RANGE = ".."
    COLON = ":"
    COMMA = ","
    EQ = "="
    NE = "<>"
    GT = ">"
    LT = "<"
    GE = ">="
    LE = "<="
    HASH = "#"
    # block of reserved words
    PROGRAM = "PROGRAM"  # marks the beginning of the block
    RECORD = "RECORD"
    FUNCTION = "FUNCTION"
    INTEGER = "INTEGER"
    REAL = "REAL"
    BOOLEAN = "BOOLEAN"
    STRING = "STRING"
    CHAR = "CHAR"
    INTEGER_DIV = "DIV"
    MOD = "MOD"
    TRUE = "TRUE"
    FALSE = "FALSE"
    AND = "AND"
    OR = "OR"
    NOT = "NOT"
    VAR = "VAR"
    IF = "IF"
    THEN = "THEN"
    ELSE = "ELSE"
    WHILE = "WHILE"
    DO = "DO"
    FOR = "FOR"
    TO = "TO"
    ARRAY = "ARRAY"
    OF = "OF"
    CASE = "CASE"
    PROCEDURE = "PROCEDURE"
    TYPE = "TYPE"
    BEGIN = "BEGIN"
    END = "END"  # marks the end of the block
    # misc
    ID = "ID"
    INTEGER_CONST = "INTEGER_CONST"
    REAL_CONST = "REAL_CONST"
    STRING_CONST = "STRING_CONST"
    CHAR_CONST = "CHAR_CONST"
    ASSIGN = ":="
    EOF = "EOF"


class Token:
    def __init__(
        self,
        type: TokenType | None,
        value,
        lineno: int | None = None,
        column: int | None = None,
    ) -> None:
        self.type = type
        self.value = value
        self.lineno = lineno
        self.column = column

    def __str__(self) -> str:
        """String representation of the class instance.

        Example:
            >>> Token(TokenType.INTEGER, 7, lineno=5, column=10)
            Token(TokenType.INTEGER, 7, position=5:10)
        """
        return "Token({type}, {value}, position={lineno}:{column})".format(
            type=self.type,
            value=repr(self.value),
            lineno=self.lineno,
            column=self.column,
        )

    def __repr__(self) -> str:
        return self.__str__()


def _build_reserved_keywords():
    """Build a dictionary of reserved keywords.

    The function relies on the fact that in the TokenType
    enumeration the beginning of the block of reserved keywords is
    marked with PROGRAM and the end of the block is marked with
    the END keyword.

    Result:
        {'PROGRAM': <TokenType.PROGRAM: 'PROGRAM'>,
         'INTEGER': <TokenType.INTEGER: 'INTEGER'>,
         'REAL': <TokenType.REAL: 'REAL'>,
         'DIV': <TokenType.INTEGER_DIV: 'DIV'>,
         'MOD': <TokenType.MOD: 'MOD'>,
         'VAR': <TokenType.VAR: 'VAR'>,
         'PROCEDURE': <TokenType.PROCEDURE: 'PROCEDURE'>,
         'BEGIN': <TokenType.BEGIN: 'BEGIN'>,
         'END': <TokenType.END: 'END'>},
         'FUNCTION':<TokenType.FUNCTION: 'FUNCTION'>,
    """
    # enumerations support iteration, in definition order
    tt_list = list(TokenType)
    start_index = tt_list.index(TokenType.PROGRAM)
    end_index = tt_list.index(TokenType.END)
    reserved_keywords = {
        token_type.value: token_type
        for token_type in tt_list[start_index : end_index + 1]
    }
    return reserved_keywords


RESERVED_KEYWORDS: dict[str, TokenType] = _build_reserved_keywords()


@dataclass
class LexerStatus:
    pos: int
    current_char: str | None
    lineno: int
    column: int


class Lexer:
    def __init__(self, text: str) -> None:
        # client string input, e.g. "4 + 2 * 3 - 6 / 2"
        self.text = text
        # self.pos is an index into self.text
        self.pos = 0
        self.current_char: str | None = self.text[self.pos]
        # token line number and column number
        self.lineno = 1
        self.column = 1

    def error(self):
        s = "Lexer error on '{lexeme}' line: {lineno} column: {column}".format(
            lexeme=self.current_char,
            lineno=self.lineno,
            column=self.column,
        )
        raise LexerError(message=s)

    def status(self) -> LexerStatus:
        """return the `pos` pointer`"""
        return LexerStatus(self.pos, self.current_char, self.lineno, self.column)

    def advance(self) -> None:
        """Advance the `pos` pointer and set the `current_char` variable."""
        if self.current_char == "\n":
            self.lineno += 1
            self.column = 0

        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None  # Indicates end of input
        else:
            self.current_char = self.text[self.pos]
            self.column += 1

    def peek(self):
        peek_pos = self.pos + 1
        if peek_pos > len(self.text) - 1:
            return None
        else:
            return self.text[peek_pos]

    def skip_whitespace(self) -> None:
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def skip_comment(self) -> None:
        while self.current_char != "}":
            self.advance()
        self.advance()  # the closing curly brace

    def number(self) -> Token:
        """Return a (multidigit) integer or float consumed from the input."""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

        result = ""
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()

        if self.current_char == "." and self.peek() == ".":
            token.type = TokenType.INTEGER_CONST
            token.value = int(result)
        elif self.current_char == ".":
            result += self.current_char
            self.advance()

            while self.current_char is not None and self.current_char.isdigit():
                result += self.current_char
                self.advance()

            token.type = TokenType.REAL_CONST
            token.value = float(result)
        else:
            token.type = TokenType.INTEGER_CONST
            token.value = int(result)

        return token

    def __string(self) -> Token:
        """Handle string const"""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

        if self.current_char == "'":
            self.advance()
        else:
            raise LexerError(
                error_code=ErrorCode.LEXER_STRING_ERROR,
                token=token,
                message="Unterminated string",
            )
        value = ""
        while self.current_char is not None and self.current_char != "'":
            value += self.current_char
            self.advance()

        if self.current_char == "'":
            self.advance()
        else:
            raise LexerError(
                error_code=ErrorCode.LEXER_STRING_ERROR,
                token=token,
                message="Unterminated string",
            )

        token.type = TokenType.STRING_CONST
        token.value = value
        return token

    def __char_const(self) -> Token:
        """Handle character const in format #digits"""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

        if self.current_char == "#":
            self.advance()
        else:
            raise LexerError(
                error_code=ErrorCode.LEXER_INVALID_CHARACTER,
                token=token,
                message="Expected # for character constant",
            )

        # Parse the numeric part
        value = ""
        while self.current_char is not None and self.current_char.isdigit():
            value += self.current_char
            self.advance()

        if not value:
            raise LexerError(
                error_code=ErrorCode.LEXER_INVALID_CHARACTER,
                token=token,
                message="Expected digits after # for character constant",
            )

        # Convert to character
        ascii_value = int(value)
        char_value = chr(ascii_value) if 0 <= ascii_value <= 255 else "\0"

        token.type = TokenType.CHAR_CONST
        token.value = char_value
        return token

    def __id(self) -> Token:
        """Handle identifiers and reserved keywords"""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

        value = ""
        while self.current_char is not None and self.current_char.isalnum():
            value += self.current_char
            self.advance()

        token_type = RESERVED_KEYWORDS.get(value.upper())
        if token_type is None:
            token.type = TokenType.ID
            token.value = value
        else:
            # reserved keyword
            token.type = token_type
            token.value = value.upper()

        return token

    def comparison(self) -> Token:
        """handle six comparison operators, [ '<', '>', '<>', '=', '<=', '>=']"""
        if self.current_char == "=":
            token = Token(
                type=TokenType.EQ,
                value=TokenType.EQ.value,  # '='
                lineno=self.lineno,
                column=self.column,
            )
            self.advance()
            return token

        if self.current_char == ">":
            if self.peek() == "=":
                token = Token(
                    type=TokenType.GE,
                    value=TokenType.GE.value,  # '>='
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token
            else:
                token = Token(
                    type=TokenType.GT,
                    value=TokenType.GT.value,  # '>'
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                return token

        if self.current_char == "<":
            if self.peek() == "=":
                token = Token(
                    type=TokenType.LE,
                    value=TokenType.LE.value,  # '<='
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token
            elif self.peek() == ">":
                token = Token(
                    type=TokenType.NE,
                    value=TokenType.NE.value,  # '<>'
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token
            else:
                token = Token(
                    type=TokenType.LT,
                    value=TokenType.LT.value,  # '<'
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                return token
        raise LexerError(
            error_code=ErrorCode.LEXER_INVALID_CHARACTER,
            token=Token(
                type=None,
                value=self.current_char,
                lineno=self.lineno,
                column=self.column,
            ),
            message=f"{ErrorCode.LEXER_INVALID_CHARACTER.value} -> '{self.current_char}'",
        )

    def get_next_token(self) -> Token:
        """Lexical analyzer (also known as scanner or tokenizer)

        This method is responsible for breaking a sentence
        apart into tokens. One token at a time.
        """
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue

            if self.current_char in ["<", ">", "="]:
                return self.comparison()

            if self.current_char == "{":
                self.advance()
                self.skip_comment()
                continue

            if self.current_char == "'":
                return self.__string()

            if self.current_char.isalpha():
                return self.__id()

            if self.current_char == "#":
                return self.__char_const()

            if self.current_char == "." and self.peek() == ".":
                token = Token(
                    type=TokenType.RANGE,
                    value=TokenType.RANGE.value,  # ':='
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token

            if self.current_char.isdigit():
                return self.number()

            if self.current_char == ":" and self.peek() == "=":
                token = Token(
                    type=TokenType.ASSIGN,
                    value=TokenType.ASSIGN.value,  # ':='
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token

            # single-character token
            try:
                # get enum member by value, e.g.
                # TokenType(';') --> TokenType.SEMI
                token_type = TokenType(self.current_char)
            except ValueError:
                # no enum member with value equal to self.current_char
                self.error()
            else:
                # create a token with a single-character lexeme as its value
                token = Token(
                    type=token_type,
                    value=token_type.value,  # e.g. ';', '.', etc
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                return token

        # EOF (end-of-file) token indicates that there is no more
        # input left for lexical analysis
        return Token(type=TokenType.EOF, value=None)

    def peek_next_token(self) -> Token:
        prev_status = self.status()
        token = self.get_next_token()
        self.revert(prev_status)
        return token

    def revert(self, status: LexerStatus) -> None:
        """revert the current lexer status before call self.get_next_token()"""
        self.pos = status.pos
        self.current_char = status.current_char
        self.lineno = status.lineno
        self.column = status.column


###############################################################################
#                                                                             #
#  PARSER                                                                     #
#                                                                             #
###############################################################################
class AST:
    def __init__(self) -> None:
        self._num: int | None = None
        self.value: Any = None


class Statement(AST):
    def __init__(self):
        pass


class Declaration(Statement):
    def __init__(self):
        super().__init__()


class TypeDeclaration(Declaration):
    def __init__(self, type_name: Var, type_def: Type):
        self.type_name = type_name
        self.type_def = type_def


class Expression(AST):
    def __init__(self):
        super().__init__()


class Type(Expression):
    def __init__(self, token: Token) -> None:
        self.token = token
        self.value = token.value


class PrimitiveType(Type):
    def __init__(self, token):
        super().__init__(token)

    def __str__(self):
        return self.value


class StringType(Type):
    def __init__(self, token, limit: Expression | None = None):
        super().__init__(token)
        self.limit = limit

    def __str__(self):
        return "STRING"


class ArrayType(Type):
    def __init__(
        self,
        token,
        element_type: Type,
        lower: Expression,
        upper: Expression,
        dynamic: bool = False,
    ):
        super().__init__(token)
        self.element_type = element_type
        self.lower = lower
        self.upper = upper
        self.dynamic = dynamic

    def __str__(self):
        return "Array[{element_type_name}]".format(
            element_type_name=str(self.element_type)
        )


class EnumType(Type):
    def __init__(self, enum_values: list[str]):
        self.enum_values = enum_values


class BinOp(Expression):
    def __init__(self, left: AST, op: Token, right: AST) -> None:
        self.left = left
        self.token = self.op = op
        self.right = right


type Number = int | float


class Num(Expression):
    def __init__(self, token: Token) -> None:
        self.token = token
        self.value: Number = token.value


class Bool(Expression):
    def __init__(self, token: Token) -> None:
        self.token = token
        self.value: bool = token.value


class String(Expression):
    def __init__(self, token: Token):
        self.token = token
        self.value: str = token.value


class Char(Expression):
    def __init__(self, token: Token):
        self.token = token
        self.value: str = token.value


class UnaryOp(Expression):
    def __init__(self, op: Token, expr: AST) -> None:
        self.token = self.op = op
        self.expr = expr


class Compound(Statement):
    """Represents a 'BEGIN ... END' block"""

    def __init__(self) -> None:
        self.children: list[AST] = []


class IfStatement(Statement):
    """Represents a 'if ... then... elseif ... then ... else' statement"""

    def __init__(
        self,
        condition: AST,
        then_branch: AST,
        else_if_branches: list[IfStatement],
        else_branch: AST | None,
    ) -> None:
        self.condition = condition
        self.then_branch = then_branch
        self.else_if_branches = else_if_branches
        self.else_branch = else_branch


class WhileStatement(Statement):
    """Represents a 'WHILE ... DO ... BEGIN ... END' block"""

    def __init__(self, condition: AST, block: Compound) -> None:
        self.condition = condition
        self.block = block


class ForStatement(Statement):
    """Represents a 'FOR left:= right TO ... DO (BEGIN ... END) or statement' block"""

    def __init__(self, initialization: Assign, bound: AST, block: AST) -> None:
        self.initialization = initialization
        self.bound = bound
        self.block = block


class CaseStatement(Statement):
    """Represents a 'CASE variable OF case_list (ELSE statement)? END' block"""

    def __init__(
        self, case_expr: AST, case_items: list[CaseItem], else_stmt: AST | None = None
    ) -> None:
        self.case_expr = case_expr  # 被判断的表达式
        self.case_items = case_items  # case项目列表
        self.else_stmt = else_stmt  # else语句（可选）


class CaseItem(AST):
    """Represents a case item with labels and a statement"""

    def __init__(self, labels: list[CaseLabel], statement: AST) -> None:
        self.labels = labels  # case标签列表
        self.statement = statement  # 对应的语句


class CaseLabel(AST):
    """Represents a case label value"""

    def __init__(self, value) -> None:
        self.value = value  # 标签值


class Assign(Statement):
    def __init__(self, left: Expression, op: Token, right) -> None:
        self.left = left
        self.token = self.op = op
        self.right = right


class Var(Expression):
    """The Var node is constructed out of ID token."""

    def __init__(self, token: Token) -> None:
        self.token = token
        self.value = token.value


class AccessExpression(Expression):
    """Represents variable access with optional suffixes: ID[expr] or ID.field"""

    def __init__(self, base: Expression, suffixes: list[AccessSuffix]):
        self.base = base
        self.suffixes = suffixes
        # Set token from the base expression
        self.token = getattr(base, "token", None)


class AccessSuffix(Expression):
    """Base class for access suffixes"""

    pass


class IndexSuffix(AccessSuffix):
    """Represents array index access [expr]"""

    def __init__(self, index: Expression):
        self.index = index


class MemberSuffix(AccessSuffix):
    """Represents member access .field"""

    def __init__(self, member: Token):
        self.member = member
        self.token = member


class NoOp(Statement):
    pass


class Program(Statement):
    def __init__(self, name: str, block: Block) -> None:
        self.name = name
        self.block = block


class Block(Statement):
    def __init__(
        self, declarations: list[Declaration], compound_statement: Compound
    ) -> None:
        self.declarations = declarations
        self.compound_statement = compound_statement


class VarDecl(Declaration):
    def __init__(self, var_node: Var, type_node: Type) -> None:
        self.var_node = var_node
        self.type_node = type_node
        self.type_symbol: Symbol | None = None  # Set by semantic analyzer


class Param(Expression):
    def __init__(self, var_node: Var, type_node: Type) -> None:
        self.var_node = var_node
        self.type_node = type_node


class ProcedureDecl(Declaration):
    def __init__(
        self, proc_name: str, formal_params: list[Param], block_node: Block
    ) -> None:
        self.proc_name = proc_name
        self.formal_params = formal_params  # a list of Param nodes
        self.block_node = block_node


class FunctionDecl(Declaration):
    def __init__(
        self,
        func_name: str,
        formal_params: list[Param],
        return_type: Type,
        block_node: Block,
    ) -> None:
        self.func_name = func_name
        self.formal_params = formal_params  # a list of Param nodes
        self.return_type = return_type
        self.block_node = block_node


class ProcedureCall(Statement):
    def __init__(
        self, proc_name: str, actual_params: list[Expression], token: Token
    ) -> None:
        self.proc_name = proc_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        # a reference to procedure declaration symbol
        self.proc_symbol: ProcedureSymbol | None = None


class FunctionCall(Expression):
    def __init__(
        self, func_name: str, actual_params: list[Expression], token: Token
    ) -> None:
        self.func_name = func_name
        self.actual_params = actual_params  # a list of AST nodes
        self.token = token
        # a reference to procedure declaration symbol
        self.func_symbol: FunctionSymbol | None = None


class RecordType(Type):
    """表示记录类型定义，包含常规字段和可选的变体部分"""

    def __init__(
        self, fields: list[RecordField], variant_part: VariantPart | None = None
    ):
        self.fields = fields  # 常规字段列表
        self.variant_part = variant_part  # 可选的变体部分

    @property
    def field_names(self) -> list[str]:
        names: list[str] = []
        for field in self.fields:
            names.append(field.name.value)
        return names

    @property
    def field_entries(self) -> dict[str, Type]:
        entries: dict[str, Type] = {}
        for field in self.fields:
            entries[field.name.value] = field.type_node
        return entries


class RecordField(AST):
    """表示记录中的字段"""

    def __init__(self, name: Var, type_node: Type):
        self.name = name  # 字段名
        self.type_node = type_node  # 字段类型


class VariantPart(AST):
    """表示记录的变体部分"""

    def __init__(self, tag_field: Var, variant_cases: list[VariantCase]):
        self.tag_field = tag_field  # 标签字段（必须是枚举类型）
        self.variant_cases = variant_cases  # 变体情况列表


class VariantCase(AST):
    """表示变体记录中的一个变体情况"""

    def __init__(self, tag_values: list[str], fields: list[RecordField]):
        self.tag_values = tag_values  # 此变体情况对应的标签值列表
        self.fields = fields  # 该变体的字段列表


class Parser:
    def __init__(self, lexer: Lexer) -> None:
        self.lexer = lexer
        # set current token to the first token taken from the input
        self.current_token = self.get_next_token()

    def newZeroNum(self, lineno: int = -1, column: int = -1) -> Num:
        return Num(
            token=Token(TokenType.INTEGER_CONST, value=0, lineno=lineno, column=column)
        )

    def get_next_token(self):
        return self.lexer.get_next_token()

    def peek_next_token(self):
        return self.lexer.peek_next_token()

    def error(self, error_code: ErrorCode, token: Token):
        raise ParserError(
            error_code=error_code,
            token=token,
            message=f"{error_code.value} -> {token}",
        )

    def eat(self, token_type: TokenType) -> None:
        # compare the current token type with the passed token
        # type and if they match then "eat" the current token
        # and assign the next token to the self.current_token,
        # otherwise raise an exception.
        if self.current_token.type == token_type:
            self.current_token = self.get_next_token()
        else:
            self.error(
                error_code=ErrorCode.UNEXPECTED_TOKEN,
                token=self.current_token,
            )

    def program(self) -> Program:
        """program : PROGRAM variable SEMI block DOT"""
        self.eat(TokenType.PROGRAM)
        var_node = self.variable()
        prog_name = var_node.value
        self.eat(TokenType.SEMI)
        block_node = self.block()
        program_node = Program(prog_name, block_node)
        self.eat(TokenType.DOT)
        return program_node

    def block(self) -> Block:
        """block : declarations compound_statement"""
        declaration_nodes = self.declarations()
        compound_statement_node = self.compound_statement()
        node = Block(declaration_nodes, compound_statement_node)
        return node

    def declarations(self) -> list[Declaration]:
        """
        declarations : type_declarations? (VAR (variable_declaration SEMI)+)? procedure_declaration* function_declaration*
        """
        declarations: list[Declaration] = []

        if self.current_token.type == TokenType.TYPE:
            type_decls = self.type_declarations()
            declarations.extend(type_decls)

        if self.current_token.type == TokenType.VAR:
            self.eat(TokenType.VAR)
            while self.current_token.type == TokenType.ID:
                var_decl = self.variable_declaration()
                declarations.extend(var_decl)
                self.eat(TokenType.SEMI)

        while self.current_token.type == TokenType.PROCEDURE:
            proc_decl = self.procedure_declaration()
            declarations.append(proc_decl)

        while self.current_token.type == TokenType.FUNCTION:
            func_decl = self.function_declaration()
            declarations.append(func_decl)

        return declarations

    def type_declarations(self) -> list[Declaration]:
        """
        type_declarations : TYPE (type_declaration SEMI)+
        """
        self.eat(TokenType.TYPE)
        type_decls = []

        while self.current_token.type == TokenType.ID:
            type_decl = self.type_declaration()
            type_decls.append(type_decl)
            self.eat(TokenType.SEMI)

        return type_decls

    def type_declaration(self) -> Declaration:
        """
        type_declaration : ID EQ (enum_type | type_spec)
        """
        type_name = self.current_token.value
        type_token = self.current_token
        self.eat(TokenType.ID)
        self.eat(TokenType.EQ)

        # Check if it's an enum type (starts with LPAREN)
        if self.current_token.type == TokenType.LPAREN:
            type_def = self.enum_type()
        else:
            # For other types, we might need a different approach
            # For now, let's assume it's an alias to another type
            type_def = self.type_spec()

        return TypeDeclaration(
            Var(Token(TokenType.ID, type_name, type_token.lineno, type_token.column)),
            type_def,
        )

    def formal_parameters(self) -> list[Param]:
        """formal_parameters : ID (COMMA ID)* COLON type_spec"""
        param_nodes: list[Param] = []

        param_tokens = [self.current_token]
        self.eat(TokenType.ID)
        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            param_tokens.append(self.current_token)
            self.eat(TokenType.ID)

        self.eat(TokenType.COLON)
        type_node = self.type_spec()

        for param_token in param_tokens:
            param_node = Param(Var(param_token), type_node)
            param_nodes.append(param_node)

        return param_nodes

    def formal_parameter_list(self) -> list[Param]:
        """formal_parameter_list : formal_parameters
        | formal_parameters SEMI formal_parameter_list
        """
        # procedure Foo();
        if not self.current_token.type == TokenType.ID:
            return []

        param_nodes = self.formal_parameters()

        while self.current_token.type == TokenType.SEMI:
            self.eat(TokenType.SEMI)
            param_nodes.extend(self.formal_parameters())

        return param_nodes

    def variable_declaration(self) -> list[VarDecl]:
        """variable_declaration : ID (COMMA ID)* COLON type_spec"""
        var_nodes = [Var(self.current_token)]  # first ID
        self.eat(TokenType.ID)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            var_nodes.append(Var(self.current_token))
            self.eat(TokenType.ID)

        self.eat(TokenType.COLON)

        type_node = self.type_spec()
        var_declarations = [VarDecl(var_node, type_node) for var_node in var_nodes]
        return var_declarations

    def procedure_declaration(self) -> ProcedureDecl:
        """procedure_declaration :
        PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI
        """
        self.eat(TokenType.PROCEDURE)
        proc_name = self.current_token.value
        self.eat(TokenType.ID)
        formal_params = []

        if self.current_token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            formal_params = self.formal_parameter_list()
            self.eat(TokenType.RPAREN)

        self.eat(TokenType.SEMI)
        block_node = self.block()
        proc_decl = ProcedureDecl(proc_name, formal_params, block_node)
        self.eat(TokenType.SEMI)
        return proc_decl

    def function_declaration(self) -> FunctionDecl:
        """function_declaration :
        FUNCTION ID LPAREN (formal_parameter_list)? RPAREN COLON type_spec SEMI block SEMI
        """
        self.eat(TokenType.FUNCTION)
        func_name = self.current_token.value
        self.eat(TokenType.ID)

        formal_params = []
        self.eat(TokenType.LPAREN)
        formal_params = self.formal_parameter_list()
        self.eat(TokenType.RPAREN)
        self.eat(TokenType.COLON)

        return_type = self.type_spec()

        self.eat(TokenType.SEMI)
        block_node = self.block()
        func_decl = FunctionDecl(func_name, formal_params, return_type, block_node)
        self.eat(TokenType.SEMI)
        return func_decl

    def type_spec(self) -> Type:
        """
        type_spec : primitive_type_spec | string_type_spec | array_type_spec | record_type_spec | ID
        """
        if self.current_token.type in (
            TokenType.INTEGER,
            TokenType.REAL,
            TokenType.BOOLEAN,
            TokenType.CHAR,
        ):
            return self.primitive_type_spec()
        elif self.current_token.type == TokenType.STRING:
            return self.string_type_spec()
        elif self.current_token.type == TokenType.ARRAY:
            return self.array_type_spec()
        elif self.current_token.type == TokenType.RECORD:
            return self.record_type_spec()
        elif self.current_token.type == TokenType.ID:
            # 枚举类型或其他自定义类型
            token = self.current_token
            self.eat(TokenType.ID)
            return Type(token)
        else:
            raise SemanticError(
                error_code=ErrorCode.SEMANTIC_UNKNOWN_TYPE,
                token=self.current_token,
                message=f"{ErrorCode.SEMANTIC_UNKNOWN_TYPE.value} -> {self.current_token}",
            )

    def primitive_type_spec(self) -> Type:
        """
        primitive_type_spec : INTEGER | REAL | BOOLEAN | CHAR
        """
        token = self.current_token
        if self.current_token.type == TokenType.INTEGER:
            self.eat(TokenType.INTEGER)
        elif self.current_token.type == TokenType.REAL:
            self.eat(TokenType.REAL)
        elif self.current_token.type == TokenType.BOOLEAN:
            self.eat(TokenType.BOOLEAN)
        elif self.current_token.type == TokenType.CHAR:
            self.eat(TokenType.CHAR)
        node = PrimitiveType(token)
        return node

    def enum_type(self) -> Type:
        """
        enum_type : LPAREN identifier_list RPAREN
        """
        self.eat(TokenType.LPAREN)
        identifiers = self.identifier_list()
        self.eat(TokenType.RPAREN)
        return EnumType(identifiers)

    def identifier_list(self) -> list[str]:
        """
        identifier_list : ID (COMMA ID)*
        """
        identifiers = [self.current_token.value]
        self.eat(TokenType.ID)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            identifiers.append(self.current_token.value)
            self.eat(TokenType.ID)

        return identifiers

    def string_type_spec(self) -> StringType:
        """
        string_type_spec: STRING ( LBRACKET INTEGER_CONST RBRACKET )?
        """
        token = self.current_token
        if self.current_token.type == TokenType.STRING:
            self.eat(TokenType.STRING)
            if self.current_token.type == TokenType.LBRACKET:
                self.eat(TokenType.LBRACKET)
                limit = self.factor()
                self.eat(TokenType.RBRACKET)
                return StringType(token=token, limit=limit)
            else:
                return StringType(token=token)
        else:
            raise ParserError(
                error_code=ErrorCode.PARSER_UNEXPECTED_TOKEN,
                token=self.current_token,
                message=f"{ErrorCode.PARSER_UNEXPECTED_TOKEN.value} -> {self.current_token}",
            )

    def array_type_spec(self) -> ArrayType:
        """array_type_spec : ARRAY (LBRACKET INTEGER_CONST RANGE INTEGER_CONST RBRACKET)? of type_spec"""
        token = self.current_token
        self.eat(TokenType.ARRAY)
        lower: Expression = self.newZeroNum(
            lineno=self.current_token.lineno, column=self.current_token.column
        )
        upper: Expression = self.newZeroNum(
            lineno=self.current_token.lineno, column=self.current_token.column
        )
        dynamic: bool = True
        if self.current_token.type == TokenType.LBRACKET:
            self.eat(TokenType.LBRACKET)
            lower = self.factor()
            self.eat(TokenType.RANGE)
            upper = self.factor()
            self.eat(TokenType.RBRACKET)
            dynamic = False
        self.eat(TokenType.OF)
        element_type = self.type_spec()
        node = ArrayType(
            token=token,
            element_type=element_type,
            lower=lower,
            upper=upper,
            dynamic=dynamic,
        )
        return node

    def record_type_spec(self) -> RecordType:
        """解析记录类型定义，包括常规字段和可选的变体部分"""
        self.eat(TokenType.RECORD)
        fields = []

        # 解析固定部分字段
        while (
            self.current_token.type != TokenType.CASE
            and self.current_token.type != TokenType.END
        ):
            # 解析字段声明
            var_nodes = [Var(self.current_token)]
            self.eat(TokenType.ID)

            while self.current_token.type == TokenType.COMMA:
                self.eat(TokenType.COMMA)
                var_nodes.append(Var(self.current_token))
                self.eat(TokenType.ID)

            self.eat(TokenType.COLON)
            type_node = self.type_spec()

            # 为每个变量创建字段节点
            for var_node in var_nodes:
                fields.append(RecordField(var_node, type_node))

            if self.current_token.type == TokenType.SEMI:
                self.eat(TokenType.SEMI)

        # 解析变体部分（可选）
        variant_part = None
        if self.current_token.type == TokenType.CASE:
            variant_part = self._parse_variant_part()

        self.eat(TokenType.END)
        return RecordType(fields, variant_part)

    def _parse_variant_part(self) -> VariantPart:
        """解析记录的变体部分，使用 'case kind of' 语法格式"""
        self.eat(TokenType.CASE)

        # 解析标签字段（必须是枚举类型）
        tag_field = Var(self.current_token)
        self.eat(TokenType.ID)

        self.eat(TokenType.OF)

        # 解析变体情况列表
        variant_cases = []
        while self.current_token.type != TokenType.END:
            # 解析标签值列表
            tag_values = [self.current_token.value]
            self.eat(TokenType.ID)

            while self.current_token.type == TokenType.COMMA:
                self.eat(TokenType.COMMA)
                tag_values.append(self.current_token.value)
                self.eat(TokenType.ID)

            self.eat(TokenType.COLON)
            self.eat(TokenType.LPAREN)

            # 解析变体字段列表
            variant_fields = []
            while self.current_token.type != TokenType.RPAREN:
                # 解析字段声明（与 record_type_spec 相同）
                var_nodes = [Var(self.current_token)]
                self.eat(TokenType.ID)

                while self.current_token.type == TokenType.COMMA:
                    self.eat(TokenType.COMMA)
                    var_nodes.append(Var(self.current_token))
                    self.eat(TokenType.ID)

                self.eat(TokenType.COLON)
                type_node = self.type_spec()

                for var_node in var_nodes:
                    variant_fields.append(RecordField(var_node, type_node))

                if self.current_token.type == TokenType.SEMI:
                    self.eat(TokenType.SEMI)

            self.eat(TokenType.RPAREN)

            # 创建变体情况
            variant_case = VariantCase(tag_values, variant_fields)
            variant_cases.append(variant_case)

            if self.current_token.type == TokenType.SEMI:
                self.eat(TokenType.SEMI)

        return VariantPart(tag_field, variant_cases)

    def compound_statement(self) -> Compound:
        """
        compound_statement: BEGIN statement_list END
        """
        self.eat(TokenType.BEGIN)
        nodes = self.statement_list()
        self.eat(TokenType.END)

        root = Compound()
        for node in nodes:
            root.children.append(node)

        return root

    def statement_list(self) -> list[Statement]:
        """
        statement_list : statement
                       | statement SEMI statement_list
        """
        node = self.statement()

        results = [node]

        while self.current_token.type == TokenType.SEMI:
            self.eat(TokenType.SEMI)
            results.append(self.statement())

        return results

    # it should deal with func_call_expr
    def statement(self) -> Statement:
        """
        statement : compound_statement
                  | proccall_statement
                  | assignment_statement
                  | if_statement
                  | case_statement
                  | empty
        """
        node: Statement
        if self.current_token.type == TokenType.BEGIN:
            node = self.compound_statement()
        elif self.current_token.type == TokenType.IF:
            node = self.if_statement()
        elif self.current_token.type == TokenType.CASE:
            node = self.case_statement()
        elif self.current_token.type == TokenType.WHILE:
            node = self.while_statement()
        elif self.current_token.type == TokenType.FOR:
            node = self.for_statement()
        elif (
            self.current_token.type == TokenType.ID
            and self.lexer.peek_next_token().type == TokenType.LPAREN
        ):
            node = self.proccall_statement()
        elif self.current_token.type == TokenType.ID:
            node = self.assignment_statement()
        else:
            node = self.empty()
        return node

    def if_statement(self) -> IfStatement:
        """
        if_statement: IF logic_expr THEN (statement | compound_statement)
                    (ELSE IF logic_expr THEN (statement | compound_statement))*
                    (ELSE (statement | compound_statement))? SEMI
        """
        self.eat(TokenType.IF)
        condition = self.logic_expr()
        self.eat(TokenType.THEN)
        then_branch: AST
        if self.current_token.type == TokenType.BEGIN:
            then_branch = self.compound_statement()
        else:
            then_branch = self.statement()

        else_if_branches: list[IfStatement] = []
        else_branch: AST | None = None
        while self.current_token.type == TokenType.ELSE:
            self.eat(TokenType.ELSE)
            sub_then_branch: AST
            if self.current_token.type == TokenType.IF:
                self.eat(TokenType.IF)
                sub_condition = self.logic_expr()
                self.eat(TokenType.THEN)
                if self.current_token.type == TokenType.BEGIN:
                    sub_then_branch = self.compound_statement()
                else:
                    sub_then_branch = self.statement()
                sub_node = IfStatement(sub_condition, sub_then_branch, [], None)
                else_if_branches.append(sub_node)
            else:
                if self.current_token.type == TokenType.BEGIN:
                    else_branch = self.compound_statement()
                else:
                    else_branch = self.statement()
                break

        node = IfStatement(
            condition,
            then_branch,
            else_if_branches,
            else_branch,
        )
        return node

    def while_statement(self) -> WhileStatement:
        """while_statement:  WHILE logic_expr DO compound_statement SEMI"""
        self.eat(TokenType.WHILE)
        condition = self.logic_expr()
        self.eat(TokenType.DO)
        block = self.compound_statement()
        self.eat(TokenType.SEMI)
        node = WhileStatement(condition, block)
        return node

    def for_statement(self) -> ForStatement:
        """for_statement:  FOR assignment_statement TO summation_expr DO (statement | compound_statement) SEMI"""
        self.eat(TokenType.FOR)
        initialization = self.assignment_statement()
        self.eat(TokenType.TO)
        bound = self.summation_expr()
        self.eat(TokenType.DO)
        block: AST
        if self.current_token.type == TokenType.BEGIN:
            block = self.compound_statement()
        else:
            block = self.statement()
        node = ForStatement(initialization, bound, block)
        return node

    def case_statement(self) -> CaseStatement:
        """case_statement : CASE variable OF case_list (ELSE (statement | compound_statement) SEMI)? END SEMI"""
        self.eat(TokenType.CASE)
        case_expr = self.variable()
        self.eat(TokenType.OF)
        case_items = self.case_list()
        else_stmt: Statement | None = None
        if self.current_token.type == TokenType.ELSE:
            self.eat(TokenType.ELSE)
            if self.current_token.type == TokenType.BEGIN:
                else_stmt = self.compound_statement()
            else:
                else_stmt = self.statement()
            self.eat(TokenType.SEMI)
        self.eat(TokenType.END)
        node = CaseStatement(case_expr, case_items, else_stmt)
        return node

    def case_list(self) -> list[CaseItem]:
        """case_list : case_item SEMI (case_item SEMI)*"""
        case_items = [self.case_item()]
        self.eat(TokenType.SEMI)

        while self.current_token.type not in (TokenType.ELSE, TokenType.END):
            # Parse next case item if we have case labels
            if self.current_token.type in (
                TokenType.INTEGER_CONST,
                TokenType.CHAR_CONST,
                TokenType.STRING_CONST,
                TokenType.TRUE,
                TokenType.FALSE,
                TokenType.ID,  # 添加ID以支持枚举值
            ):
                case_items.append(self.case_item())
                self.eat(TokenType.SEMI)
            else:
                break

        return case_items

    def case_item(self) -> CaseItem:
        """case_item : case_label_list COLON (statement | compound_statement)"""
        labels = self.case_label_list()
        self.eat(TokenType.COLON)
        statement: Statement | None = None
        if self.current_token.type == TokenType.BEGIN:
            statement = self.compound_statement()
        else:
            statement = self.statement()
        node = CaseItem(labels, statement)
        return node

    def case_label_list(self) -> list[CaseLabel]:
        """case_label_list : case_label (COMMA case_label)*"""
        labels = [self.case_label()]
        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            labels.append(self.case_label())
        return labels

    def case_label(self) -> CaseLabel:
        """case_label : INTEGER | CHAR | boolean_literal | ID"""
        token = self.current_token
        if token.type == TokenType.INTEGER_CONST:
            self.eat(TokenType.INTEGER_CONST)
            return CaseLabel(token.value)
        elif token.type == TokenType.CHAR_CONST:
            self.eat(TokenType.CHAR_CONST)
            return CaseLabel(token.value)
        elif token.type == TokenType.STRING_CONST and len(token.value) == 1:
            # 处理单字符字符串常量，如'A'，在case标签中作为字符处理
            self.eat(TokenType.STRING_CONST)
            return CaseLabel(token.value)
        elif token.type in (TokenType.TRUE, TokenType.FALSE):
            if token.type == TokenType.TRUE:
                self.eat(TokenType.TRUE)
                return CaseLabel(True)
            else:
                self.eat(TokenType.FALSE)
                return CaseLabel(False)
        elif token.type == TokenType.ID:
            # 枚举值
            self.eat(TokenType.ID)
            return CaseLabel(token.value)
        else:
            self.error(
                error_code=ErrorCode.UNEXPECTED_TOKEN,
                token=token,
            )
            raise

    def proccall_statement(self) -> ProcedureCall:
        """proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN"""
        token = self.current_token

        proc_name = self.current_token.value
        self.eat(TokenType.ID)
        self.eat(TokenType.LPAREN)
        actual_params: list[Expression] = []
        if self.current_token.type != TokenType.RPAREN:
            expr = self.expr()
            actual_params.append(expr)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            expr = self.expr()
            actual_params.append(expr)

        self.eat(TokenType.RPAREN)

        node = ProcedureCall(
            proc_name=proc_name,
            actual_params=actual_params,
            token=token,
        )
        return node

    def func_call_expr(self) -> FunctionCall:
        """func_call_expr : ID LPAREN (expr (COMMA expr)*)? RPAREN"""
        token = self.current_token

        fun_name = self.current_token.value
        self.eat(TokenType.ID)
        self.eat(TokenType.LPAREN)
        actual_params: list[Expression] = []

        actual_params.append(
            Var(Token(TokenType.ID, fun_name, token.lineno, token.column))
        )

        if self.current_token.type != TokenType.RPAREN:
            expr = self.summation_expr()
            actual_params.append(expr)

        while self.current_token.type == TokenType.COMMA:
            self.eat(TokenType.COMMA)
            expr = self.summation_expr()
            actual_params.append(expr)

        self.eat(TokenType.RPAREN)

        node = FunctionCall(
            func_name=fun_name,
            actual_params=actual_params,
            token=token,
        )
        return node

    def assignment_statement(self) -> Assign:
        """
        assignment_statement : variable ASSIGN expr
        """
        left = self.variable()
        token = self.current_token
        self.eat(TokenType.ASSIGN)
        right = self.expr()
        node = Assign(left, token, right)
        return node

    def variable(self) -> Expression:
        """
        variable : ID variable_suffix*
        variable_suffix : LBRACKET expr RBRACKET | DOT ID
        """
        # Parse the base identifier
        base = Var(self.current_token)
        self.eat(TokenType.ID)

        # Parse variable suffixes
        suffixes: list[AccessSuffix] = []

        while self.current_token.type in (TokenType.LBRACKET, TokenType.DOT):
            if self.current_token.type == TokenType.LBRACKET:
                # Index access: [expr]
                self.eat(TokenType.LBRACKET)
                index = self.expr()
                self.eat(TokenType.RBRACKET)
                suffixes.append(IndexSuffix(index))
            elif self.current_token.type == TokenType.DOT:
                # Member access: .field
                self.eat(TokenType.DOT)
                member_token = self.current_token
                self.eat(TokenType.ID)
                suffixes.append(MemberSuffix(member_token))

        # Return AccessExpression if there are suffixes, otherwise return base Var
        if suffixes:
            return AccessExpression(base, suffixes)
        else:
            return base

    def empty(self) -> NoOp:
        """An empty production"""
        return NoOp()

    def expr(self) -> Expression:
        """
        expr : logic_expr
        """
        return self.logic_expr()

    def logic_expr(self) -> Expression:
        """logic_expr : comparison_expr ((and | or ) comparison_expr)*"""
        node = self.comparison_expr()

        while self.current_token.type in (TokenType.AND, TokenType.OR):
            token = self.current_token
            if token.type == TokenType.AND:
                self.eat(TokenType.AND)
            elif token.type == TokenType.OR:
                self.eat(TokenType.OR)

            node = BinOp(left=node, op=token, right=self.comparison_expr())

        return node

    def comparison_expr(self) -> Expression:
        """comparison_expr : summation_expr ( (EQ | NE | GT | GE | LT | LE) summation_expr )*"""
        node = self.summation_expr()
        while self.current_token.type in (
            TokenType.EQ,
            TokenType.NE,
            TokenType.GT,
            TokenType.GE,
            TokenType.LT,
            TokenType.LE,
        ):
            token = self.current_token
            if token.type == TokenType.EQ:
                self.eat(TokenType.EQ)
            elif token.type == TokenType.NE:
                self.eat(TokenType.NE)
            elif token.type == TokenType.GT:
                self.eat(TokenType.GT)
            elif token.type == TokenType.GE:
                self.eat(TokenType.GE)
            elif token.type == TokenType.LT:
                self.eat(TokenType.LT)
            elif token.type == TokenType.LE:
                self.eat(TokenType.LE)

            node = BinOp(left=node, op=token, right=self.summation_expr())

        return node

    def summation_expr(self) -> Expression:
        """
        summation_expr : multiplication_expr ((PLUS | MINUS) multiplication_expr)*
        """
        node = self.multiplication_expr()

        while self.current_token.type in (TokenType.PLUS, TokenType.MINUS):
            token = self.current_token
            if token.type == TokenType.PLUS:
                self.eat(TokenType.PLUS)
            elif token.type == TokenType.MINUS:
                self.eat(TokenType.MINUS)

            node = BinOp(left=node, op=token, right=self.multiplication_expr())

        return node

    def multiplication_expr(self) -> Expression:
        """multiplication_expr : factor ((MUL | INTEGER_DIV | FLOAT_DIV | MOD) factor)*"""
        node = self.factor()

        while self.current_token.type in (
            TokenType.MUL,
            TokenType.INTEGER_DIV,
            TokenType.FLOAT_DIV,
            TokenType.MOD,
        ):
            token = self.current_token
            if token.type == TokenType.MUL:
                self.eat(TokenType.MUL)
            elif token.type == TokenType.INTEGER_DIV:
                self.eat(TokenType.INTEGER_DIV)
            elif token.type == TokenType.FLOAT_DIV:
                self.eat(TokenType.FLOAT_DIV)
            elif token.type == TokenType.MOD:
                self.eat(TokenType.MOD)

            node = BinOp(left=node, op=token, right=self.factor())

        return node

    def factor(self) -> Expression:
        """
        factor : not comparison_expr
               | PLUS factor
               | MINUS factor
               | INTEGER_CONST
               | STRING_CONST
               | REAL_CONST
               | TRUE_CONST
               | FALSE_CONST
               | LPAREN expr RPAREN
               | func_call_expr
               | variable
        """
        token = self.current_token
        node: Expression
        # logic OP
        if token.type == TokenType.NOT:
            self.eat(TokenType.NOT)
            node = UnaryOp(token, self.comparison_expr())
            return node

        # arithmetic OP
        if token.type == TokenType.PLUS:
            self.eat(TokenType.PLUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == TokenType.MINUS:
            self.eat(TokenType.MINUS)
            node = UnaryOp(token, self.factor())
            return node

        # value
        if token.type == TokenType.INTEGER_CONST:
            self.eat(TokenType.INTEGER_CONST)
            return Num(token)
        elif token.type == TokenType.REAL_CONST:
            self.eat(TokenType.REAL_CONST)
            return Num(token)
        elif token.type == TokenType.TRUE:
            self.eat(TokenType.TRUE)
            return Bool(token)
        elif token.type == TokenType.FALSE:
            self.eat(TokenType.FALSE)
            return Bool(token)

        # parent take precedence
        if token.type == TokenType.LPAREN:
            self.eat(TokenType.LPAREN)
            node = self.summation_expr()
            self.eat(TokenType.RPAREN)
            return node

        # parse string expr
        if self.current_token.type == TokenType.STRING_CONST:
            token = self.current_token
            self.eat(TokenType.STRING_CONST)
            return String(token=token)

        # parse char expr
        if self.current_token.type == TokenType.CHAR_CONST:
            token = self.current_token
            self.eat(TokenType.CHAR_CONST)
            return Char(token=token)

        # call
        if (
            token.type == TokenType.ID
            and self.peek_next_token().type == TokenType.LPAREN
        ):
            node = self.func_call_expr()
            return node
        else:
            node = self.variable()
            return node

    def parse(self):
        """
        program : PROGRAM variable SEMI block DOT

        block : declarations compound_statement

        declarations : type_declarations? (VAR (variable_declaration SEMI)+)? procedure_declaration* function_declaration*

        type_declarations : TYPE (type_declaration SEMI)+

        type_declaration : ID EQ (enum_type | type_spec)

        variable_declaration : ID (COMMA ID)* COLON type_spec

        procedure_declaration :
             PROCEDURE ID (LPAREN formal_parameter_list RPAREN)? SEMI block SEMI

        function_declaration :
             FUNCTION ID LPAREN (formal_parameter_list)? RPAREN COLON type_spec SEMI block SEMI

        formal_params_list : formal_parameters
                           | formal_parameters SEMI formal_parameter_list

        formal_parameters : ID (COMMA ID)* COLON type_spec

        type_spec : primitive_type_spec | string_type_spec | array_type_spec

        primitive_type_spec : INTEGER | REAL | BOOLEAN

        string_type_spec: STRING ( LBRACKET INTEGER_CONST RBRACKET )?

        array_type_spec : ARRAY ( LBRACKET INTEGER_CONST RANGE INTEGER_CONST RBRACKET )? of type_spec

        compound_statement : BEGIN statement_list END

        statement_list : statement
                       | statement SEMI statement_list

        statement : compound_statement
                  | proccall_statement
                  | assignment_statement
                  | if_statement
                  | case_statement
                  | for_statement
                  | while_statement
                  | empty

        if_statement: IF logic_expr THEN (statement | compound_statement)
                    (ELSE IF logic_expr THEN (statement | compound_statement))*
                    (ELSE (statement | compound_statement))? SEMI

        while_statement:  WHILE logic_expr DO compound_statement SEMI

        for_statement:  FOR assignment_statement TO summation_expr DO (statement | compound_statement) SEMI

        case_statement : CASE variable OF case_list (ELSE (statement | compound_statement) SEMI)? END SEMI

        case_list : case_item SEMI (case_item SEMI)*

        case_item : case_label_list COLON (statement | compound_statement)

        case_label_list : case_label (COMMA case_label)*

        case_label : INTEGER_CONST | CHAR_CONST | TRUE | FALSE

        proccall_statement : ID LPAREN (expr (COMMA expr)*)? RPAREN

        func_call_expr : ID LPAREN (expr (COMMA expr)*)? RPAREN

        assignment_statement : variable ASSIGN expr

        empty :

        expr : string_expr | logic_expr

        logic_expr : comparison_expr ((and | or ) comparison_expr)*

        comparison_expr : summation_expr ( (EQ | NE | GT | GE | LT | LE) summation_expr )*

        summation_expr : multiplication_expr ((PLUS | MINUS) multiplication_expr)*

        multiplication_expr : factor ((MUL | INTEGER_DIV | FLOAT_DIV) factor)*

        factor : not comparison_expr
               | PLUS factor
               | MINUS factor
               | INTEGER_CONST
               | STRING_CONST
               | CHAR_CONST
               | REAL_CONST
               | TRUE_CONST
               | FALSE_CONST
               | LPAREN expr RPAREN
               | func_call_expr
               | variable

        variable : ID variable_suffix*

        variable_suffix : LBRACKET expr RBRACKET | DOT ID
        """
        node = self.program()
        if self.current_token.type != TokenType.EOF:
            self.error(
                error_code=ErrorCode.UNEXPECTED_TOKEN,
                token=self.current_token,
            )

        return node


###############################################################################
#                                                                             #
#  AST visitors (walkers)                                                     #
#                                                                             #
###############################################################################


class NodeVisitor:
    def visit(self, node: AST):
        method_name = "visit_" + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node: AST):
        raise Exception("No visit_{} method".format(type(node).__name__))


###############################################################################
#                                                                             #
#  SYMBOLS, TABLES, SEMANTIC ANALYSIS                                         #
#                                                                             #
###############################################################################


class NativeMethod(Enum):
    WRITE = "WRITE"
    WRITELN = "WRITELN"
    LENGTH = "LENGTH"
    SETLENGTH = "SETLENGTH"
    ORD = "ORD"
    CHR = "CHR"
    INC = "INC"
    DEC = "DEC"


class Symbol:
    def __init__(self, name: str, type: Symbol | None = None) -> None:
        self.name = name
        self.type = type
        self.scope_level: int = 0


class VarSymbol(Symbol):
    def __init__(self, name: str, type: Symbol | None) -> None:
        super().__init__(name, type)

    def __str__(self) -> str:
        return "<{class_name}(name='{name}', type='{type}')>".format(
            class_name=self.__class__.__name__,
            name=self.name,
            type=self.type,
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
        # accessed by the interpreter when executing procedure call
        node.proc_symbol = cast(ProcedureSymbol, proc_symbol)

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
        # accessed by the interpreter when executing procedure call
        node.func_symbol = cast(FunctionSymbol, func_symbol)


###############################################################################
#                                                                             #
#  INTERPRETER                                                                #
#                                                                             #
###############################################################################

# Built-in procedures and functions registry
BUILTIN_PROCEDURES: dict[str, Callable[..., None]] = {}
BUILTIN_FUNCTIONS: dict[str, Callable[..., Object]] = {}


def register_builtin_procedure(name, handler):
    """Register a built-in procedure handler"""
    BUILTIN_PROCEDURES[name.upper()] = handler


def register_builtin_function(name, handler):
    """Register a built-in function handler"""
    BUILTIN_FUNCTIONS[name.upper()] = handler


# Built-in procedure handlers
def handle_write(interpreter, node):
    """Handle WRITE built-in procedure"""
    proc_name = node.proc_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))

    # output actual params
    for argument_node in actual_params:
        obj = interpreter.visit(argument_node)
        # 对于枚举对象，打印名称而不是值
        if isinstance(obj, EnumObject):
            print(obj.name, end="")
        else:
            print(obj.value if hasattr(obj, "value") else obj, end="")

    interpreter.log(f"LEAVE: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))


def handle_writeln(interpreter, node):
    """Handle WRITELN built-in procedure"""
    proc_name = node.proc_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))

    # output actual params
    for argument_node in actual_params:
        obj = interpreter.visit(argument_node)
        # 对于枚举对象，打印名称而不是值
        if isinstance(obj, EnumObject):
            print(obj.name, end="")
        else:
            print(obj.value if hasattr(obj, "value") else obj, end="")
    print()
    interpreter.log(f"LEAVE: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))


def handle_setlength(interpreter, node):
    """Handle SETLENGTH built-in procedure"""
    proc_name = node.proc_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))

    # core
    arr_name = actual_params[0].value
    new_length_obj = interpreter.visit(actual_params[1])
    new_length = new_length_obj.value if isinstance(new_length_obj, NumberObject) else 0

    pre_ar = (
        interpreter.call_stack._records[-2]
        if len(interpreter.call_stack._records) >= 2
        else ar
    )
    var_obj = pre_ar.get(arr_name)

    if isinstance(var_obj, StringObject):
        var_obj.set_length(new_length)
    elif isinstance(var_obj, ArrayObject):
        var_obj.set_length(new_length)

    interpreter.log(f"LEAVE: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))


def handle_inc(interpreter, node):
    """Handle INC built-in procedure - increment a variable"""
    proc_name = node.proc_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))

    # Get the variable name and optional increment value
    var_name = actual_params[0].value
    increment = 1  # Default increment

    if len(actual_params) > 1:
        increment_obj = interpreter.visit(actual_params[1])
        if isinstance(increment_obj, NumberObject):
            increment = increment_obj.value

    # Get current value and increment it
    var_obj = ar.get(var_name)
    if isinstance(var_obj, NumberObject):
        new_value = var_obj.value + increment
        if isinstance(var_obj, IntegerObject):
            ar[var_name] = IntegerObject(new_value)
        else:
            ar[var_name] = RealObject(new_value)

    interpreter.log(f"LEAVE: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))


def handle_dec(interpreter, node):
    """Handle DEC built-in procedure - decrement a variable"""
    proc_name = node.proc_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))

    # Get the variable name and optional decrement value
    var_name = actual_params[0].value
    decrement = 1  # Default decrement

    if len(actual_params) > 1:
        decrement_obj = interpreter.visit(actual_params[1])
        if isinstance(decrement_obj, NumberObject):
            decrement = decrement_obj.value

    # Get current value and decrement it
    var_obj = ar.get(var_name)
    if isinstance(var_obj, NumberObject):
        new_value = var_obj.value - decrement
        if isinstance(var_obj, IntegerObject):
            ar[var_name] = IntegerObject(new_value)
        else:
            ar[var_name] = RealObject(new_value)

    interpreter.log(f"LEAVE: PROCEDURE {proc_name}")
    interpreter.log(str(interpreter.call_stack))


# Built-in function handlers
def handle_length(interpreter, node):
    """Handle LENGTH built-in function"""
    func_name = node.func_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: FUNCTION {func_name}")
    interpreter.log(str(interpreter.call_stack))

    # Get the array/string object and return its length
    param_obj = interpreter.visit(actual_params[1])  # Skip the function name param
    if isinstance(param_obj, (ArrayObject, StringObject)):
        length_value = len(param_obj)
    else:
        length_value = 0

    result = IntegerObject(length_value)
    ar[RETURN_NUM_FOR_LENGTH] = result

    interpreter.log(f"LEAVE: FUNCTION {func_name}")
    interpreter.log(str(interpreter.call_stack))

    return result


def handle_ord(interpreter, node):
    """Handle ORD built-in function - returns ASCII code of a character or ordinal of an enum"""
    func_name = node.func_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: FUNCTION {func_name}")
    interpreter.log(str(interpreter.call_stack))

    # Get the character parameter and return its ASCII code
    param_obj = interpreter.visit(actual_params[1])  # Skip the function name param

    if isinstance(param_obj, CharObject):
        ascii_value = ord(param_obj.value) if param_obj.value else 0
    elif isinstance(param_obj, StringObject) and len(param_obj.value) > 0:
        ascii_value = ord(param_obj.value[0])
    elif isinstance(param_obj, EnumObject):
        # 对于枚举对象，返回其ordinal值
        ascii_value = param_obj.ordinal
    else:
        ascii_value = 0

    result = IntegerObject(ascii_value)
    ar[RETURN_NUM_FOR_LENGTH] = result

    interpreter.log(f"LEAVE: FUNCTION {func_name}")
    interpreter.log(str(interpreter.call_stack))

    return result


def handle_chr(interpreter, node):
    """Handle CHR built-in function - converts ASCII code to character"""
    func_name = node.func_name
    actual_params = node.actual_params

    ar = interpreter.call_stack.peek()

    interpreter.log(f"ENTER: FUNCTION {func_name}")
    interpreter.log(str(interpreter.call_stack))

    # Get the ASCII code parameter and convert it to a character
    param_obj = interpreter.visit(actual_params[1])  # Skip the function name param

    if isinstance(param_obj, NumberObject):
        ascii_code = param_obj.value
        try:
            char_value = chr(ascii_code)
        except (ValueError, OverflowError):
            char_value = ""
    else:
        char_value = ""

    result = CharObject(char_value)
    ar[RETURN_NUM_FOR_LENGTH] = result

    interpreter.log(f"LEAVE: FUNCTION {func_name}")
    interpreter.log(str(interpreter.call_stack))

    return result


class ARType(Enum):
    PROGRAM = "PROGRAM"
    PROCEDURE = "PROCEDURE"
    FUNCTION = "FUNCTION"


class CallStack:
    def __init__(self) -> None:
        self._records: list[ActivationRecord] = []

    def push(self, ar: ActivationRecord) -> None:
        self._records.append(ar)

    def pop(self) -> ActivationRecord:
        if len(self._records) >= 2:
            self._records[-2].copy_from(self._records[-1], True)
        return self._records.pop()

    def peek(self) -> ActivationRecord:
        return self._records[-1]

    def __str__(self) -> str:
        s = "\n".join(repr(ar) for ar in reversed(self._records))
        s = f"CALL STACK\n{s}\n\n"
        return s

    def __repr__(self) -> str:
        return self.__str__()


class ActivationRecord:
    def __init__(self, name: str, type: ARType, nesting_level: int) -> None:
        self.name = name
        self.type = type
        self.nesting_level = nesting_level
        self.members: dict[str | int, Object] = {}

    def __setitem__(self, key: str | int, value: Object) -> None:
        self.members[key] = value

    def __getitem__(self, key: str | int) -> Object:
        return self.members[key]

    def copy_from(self, other: "ActivationRecord", override: bool):
        for name, val in other.members.items():
            if override or name not in self.members:
                self.members[name] = val

    def get(self, key: str) -> Object:
        return cast(Object, self.members.get(key))

    def __str__(self) -> str:
        lines = [
            "{level}: {type} {name}".format(
                level=self.nesting_level,
                type=self.type.value,
                name=self.name,
            )
        ]
        for name, val in self.members.items():
            lines.append(f"   {name:<20}: {val}")

        s = "\n".join(lines)
        return s

    def __repr__(self) -> str:
        return self.__str__()


class Interpreter(NodeVisitor):
    def __init__(self, tree: Program) -> None:
        self.tree = tree
        self.call_stack = CallStack()

        # 枚举类型和值的注册表（模仿semantic analyzer的实现）
        self.enum_types: dict[
            str, dict
        ] = {}  # { type_name -> { 'values': [value_names...], 'size': int } }
        self.enum_values: dict[
            str, dict
        ] = {}  # { value_name -> { 'type': type_name, 'ordinal': int } }
        self.record_types: dict[str, RecordType] = {}

        # 类型别名映射表：alias_name -> actual_type_node
        self.type_aliases: dict[str, Type] = {}

        # Register built-in procedures and functions
        register_builtin_procedure(NativeMethod.WRITE.name, handle_write)
        register_builtin_procedure(NativeMethod.WRITELN.name, handle_writeln)
        register_builtin_procedure(NativeMethod.SETLENGTH.name, handle_setlength)
        register_builtin_procedure(NativeMethod.INC.name, handle_inc)
        register_builtin_procedure(NativeMethod.DEC.name, handle_dec)
        register_builtin_function(NativeMethod.LENGTH.name, handle_length)
        register_builtin_function(NativeMethod.ORD.name, handle_ord)
        register_builtin_function(NativeMethod.CHR.name, handle_chr)

    def log(self, msg) -> None:
        if _SHOULD_LOG_STACK:
            print(msg)

    def _resolve_type_alias(self, type_node: Type) -> Type:
        """解析类型别名，追随别名链直到找到实际类型"""
        if hasattr(type_node, "value"):
            type_name = type_node.value
            # 检查是否是类型别名
            if type_name in self.type_aliases:
                # 递归解析别名链
                return self._resolve_type_alias(self.type_aliases[type_name])

        # 如果不是别名或者已经是实际类型，直接返回
        return type_node

    def _enum_obj(self, type_name: str, ordinal: int) -> EnumObject:
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

    def _initialize_record_complex_fields(self, record_obj: RecordObject) -> None:
        """使用Interpreter的属性信息初始化RecordObject中的复杂类型字段"""
        for field in record_obj.record_type.fields:
            field_name = field.name.value
            field_type = field.type_node

            # 获取已创建的默认对象
            current_obj = record_obj.fields.get(field_name, NullObject())

            # 如果是空对象，尝试使用interpreter信息初始化
            if isinstance(current_obj, NullObject):
                new_obj = self._create_complex_object_from_type_node(field_type)
                if not isinstance(new_obj, NullObject):
                    record_obj.fields[field_name] = new_obj

    def _create_complex_object_from_type_node(self, type_node) -> Object:
        """根据类型节点创建复杂类型对象，使用interpreter的属性信息"""
        # 直接检查是否是ArrayType实例
        if (
            hasattr(type_node, "__class__")
            and type_node.__class__.__name__ == "ArrayType"
        ):
            return self.__initArray(type_node)

        # 处理数组类型
        if hasattr(type_node, "token") and type_node.token.type == TokenType.ARRAY:
            return self.__initArray(type_node)

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
            if type_name in self.enum_types:
                return self._enum_obj(type_name, 0)  # 使用第一个枚举值

            # 检查是否是记录类型
            if type_name in self.record_types:
                record_type = self.record_types[type_name]
                nested_record_obj = RecordObject(record_type)
                # 递归初始化嵌套记录的复杂字段
                self._initialize_record_complex_fields(nested_record_obj)
                return nested_record_obj

            # 检查是否是类型别名，然后递归解析
            if type_name in self.type_aliases:
                actual_type = self.type_aliases[type_name]
                return self._create_complex_object_from_type_node(actual_type)

        # 其他情况返回空对象
        return NullObject()

    def _post_initialize_array_elements(
        self, array_obj: ArrayObject, element_type_node: Type
    ) -> None:
        """为包含复杂类型元素的数组后初始化元素"""
        # 为静态数组的每个位置创建适当的对象，替换NullObject
        for index in range(array_obj.lower_bound, array_obj.upper_bound + 1):
            if array_obj.element_type == ElementType.RECORD:
                # 为记录类型创建对象
                if hasattr(element_type_node, "value"):
                    type_name = element_type_node.value
                    if type_name in self.record_types:
                        record_type = self.record_types[type_name]
                        record_obj = RecordObject(record_type)
                        self._initialize_record_complex_fields(record_obj)
                        array_obj.value[index] = record_obj
            elif array_obj.element_type == ElementType.CUSTOM:
                # 为其他自定义类型创建对象
                if hasattr(element_type_node, "value"):
                    type_name = element_type_node.value
                    if type_name in self.enum_types:
                        # 枚举类型，使用第一个枚举值
                        enum_obj = self._enum_obj(type_name, 0)
                        array_obj.value[index] = enum_obj
                    elif type_name in self.type_aliases:
                        # 处理类型别名
                        actual_type = self.type_aliases[type_name]
                        complex_obj = self._create_complex_object_from_type_node(
                            actual_type
                        )
                        array_obj.value[index] = complex_obj

    def visit_Program(self, node: Program) -> None:
        program_name = node.name
        self.log(f"ENTER: PROGRAM {program_name}")

        ar = ActivationRecord(
            name=program_name,
            type=ARType.PROGRAM,
            nesting_level=1,
        )
        self.call_stack.push(ar)

        self.log(str(self.call_stack))

        self.visit(node.block)

        self.log(f"LEAVE: PROGRAM {program_name}")
        self.log(str(self.call_stack))

        self.call_stack.pop()

    def visit_Block(self, node: Block) -> None:
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_TypeDeclaration(self, node: TypeDeclaration) -> None:
        # 在解释器中处理枚举类型声明，将枚举类型和枚举值注册到本地注册表中
        if isinstance(node.type_def, EnumType):
            type_name = node.type_name.value
            enum_values = node.type_def.enum_values

            # 注册枚举类型
            self.enum_types[type_name] = {
                "values": enum_values,
                "size": len(enum_values),
            }

            # 注册枚举值
            for i, enum_val in enumerate(enum_values):
                self.enum_values[enum_val] = {"type": type_name, "ordinal": i}
        elif isinstance(node.type_def, RecordType):
            type_name = node.type_name.value
            # Insert the record type into global record_types for interpreter use
            self.record_types[type_name] = node.type_def
        else:
            # 处理类型别名 (type aliases)
            # 对于非枚举和非记录类型，将其作为类型别名处理
            type_name = node.type_name.value
            self.type_aliases[type_name] = node.type_def

    def visit_VarDecl(self, node: VarDecl) -> None:
        ar = self.call_stack.peek()

        # 解析类型别名，获取实际类型
        resolved_type_node = self._resolve_type_alias(node.type_node)

        # 处理基本类型
        if hasattr(resolved_type_node, "token"):
            if resolved_type_node.token.type == TokenType.BOOLEAN:
                ar[node.var_node.value] = BooleanObject(False)
                return
            elif resolved_type_node.token.type == TokenType.INTEGER:
                ar[node.var_node.value] = IntegerObject(0)
                return
            elif resolved_type_node.token.type == TokenType.REAL:
                ar[node.var_node.value] = RealObject(0.0)
                return
            elif resolved_type_node.token.type == TokenType.CHAR:
                ar[node.var_node.value] = CharObject("")
                return
            elif resolved_type_node.token.type == TokenType.STRING:
                string_node = cast(StringType, resolved_type_node)
                limit: int = 255
                if string_node.limit is not None:
                    limit = self.visit(string_node.limit).value
                ar[node.var_node.value] = StringObject("", limit)
                return
            elif resolved_type_node.token.type == TokenType.ARRAY:
                ar[node.var_node.value] = self.__initArray(resolved_type_node)
                return

        # 处理复杂类型（记录、枚举等）
        var_name = node.var_node.value

        # 检查是否是记录类型
        if isinstance(resolved_type_node, RecordType):
            record_obj = RecordObject(resolved_type_node)
            # 初始化复杂类型字段
            self._initialize_record_complex_fields(record_obj)
            ar[var_name] = record_obj
            return

        # 检查是否是枚举类型（通过类型名称）
        if hasattr(resolved_type_node, "value"):
            type_name = resolved_type_node.value
            if type_name in self.enum_types:
                enum_obj = self._enum_obj(type_name, 0)
                ar[var_name] = enum_obj
                return
            elif type_name in self.record_types:
                record_type = self.record_types[type_name]
                record_obj = RecordObject(record_type)
                # 初始化复杂类型字段
                self._initialize_record_complex_fields(record_obj)
                ar[var_name] = record_obj
                return

        # 如果有type_symbol（从语义分析器传来），尝试使用它
        type_symbol = node.type_symbol
        if type_symbol is not None:
            # Handle based on the type symbol
            if isinstance(type_symbol, RecordTypeSymbol):
                # Handle record type - use global record_types lookup
                type_name = (
                    resolved_type_node.value
                    if hasattr(resolved_type_node, "value")
                    else ""
                )
                if type_name in self.record_types:
                    record_type = self.record_types[type_name]
                    record_obj = RecordObject(record_type)
                    # 初始化复杂类型字段
                    self._initialize_record_complex_fields(record_obj)
                    ar[var_name] = record_obj
                else:
                    ar[var_name] = NullObject()
            elif (
                isinstance(type_symbol, EnumTypeSymbol)
                or type_symbol.name in self.enum_types
            ):
                # Handle enum type
                enum_obj = self._enum_obj(type_symbol.name, 0)
                ar[var_name] = enum_obj
            else:
                # For other custom types, treat as null for now
                ar[var_name] = NullObject()
        else:
            # 如果没有type_symbol，默认创建Null对象
            ar[var_name] = NullObject()

    def __initArray(self, node: Type) -> ArrayObject:
        if isinstance(node, ArrayType):
            lower_bound: int = self.visit(node.lower).value
            upper_bound: int = self.visit(node.upper).value
            if lower_bound > upper_bound:
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
                    if type_name in self.record_types:
                        element_type = ElementType.RECORD
                    else:
                        # Could be enum or other custom type
                        element_type = ElementType.CUSTOM
            else:
                # For complex types without simple token, treat as custom
                element_type = ElementType.CUSTOM

            # Create array with the determined element type
            array_obj = ArrayObject(
                element_type=element_type,
                lower_bound=lower_bound,
                upper_bound=upper_bound,
                dynamic=node.dynamic,
            )

            # For record and custom types, we need to post-initialize elements
            if (
                element_type in (ElementType.RECORD, ElementType.CUSTOM)
                and not node.dynamic
            ):
                self._post_initialize_array_elements(array_obj, node.element_type)

            return array_obj

        raise SemanticError(
            error_code=ErrorCode.SEMANTIC_UNKNOWN_TYPE,
            token=node.token,
            message=f"{ErrorCode.SEMANTIC_UNKNOWN_TYPE.value} -> {node.token}",
        )

    def visit_Type(self, node: Type) -> None:
        # Do nothing
        pass

    def visit_StringType(self, node: StringType) -> None:
        # Do nothing
        pass

    def visit_PrimitiveType(self, node: PrimitiveType) -> None:
        # Do nothing
        pass

    def visit_ArrayType(self, node: ArrayType) -> None:
        # Do nothing
        pass

    def visit_RecordType(self, node: RecordType) -> None:
        """访问记录类型定义节点"""
        # 在解释器中，记录类型定义已经在语义分析阶段处理
        pass

    def visit_BinOp(self, node: BinOp) -> Object:
        left_obj = self.visit(node.left)
        right_obj = self.visit(node.right)

        # logic operator
        if node.op.type == TokenType.AND:
            if isinstance(left_obj, BooleanObject) and isinstance(
                right_obj, BooleanObject
            ):
                return left_obj & right_obj
        elif node.op.type == TokenType.OR:
            if isinstance(left_obj, BooleanObject) and isinstance(
                right_obj, BooleanObject
            ):
                return left_obj | right_obj

        # arithmetic operator
        if node.op.type == TokenType.PLUS:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj + right_obj
            elif isinstance(left_obj, StringObject) and isinstance(
                right_obj, StringObject
            ):
                # For string concatenation, don't apply limits during intermediate operations
                return StringObject(left_obj.value + right_obj.value, -1)
            # Handle string + other types conversion
            elif isinstance(left_obj, StringObject):
                if hasattr(right_obj, "value"):
                    return StringObject(left_obj.value + str(right_obj.value), -1)
                else:
                    return StringObject(left_obj.value + str(right_obj), -1)
            elif isinstance(right_obj, StringObject):
                if hasattr(left_obj, "value"):
                    return StringObject(str(left_obj.value) + right_obj.value, -1)
                else:
                    return StringObject(str(left_obj) + right_obj.value, -1)
        elif node.op.type == TokenType.MINUS:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj - right_obj
        elif node.op.type == TokenType.MUL:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj * right_obj
        elif node.op.type == TokenType.INTEGER_DIV:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj // right_obj
        elif node.op.type == TokenType.FLOAT_DIV:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj / right_obj
        elif node.op.type == TokenType.MOD:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                # MOD 运算符应该返回整数余数
                # 在 Pascal 中，MOD 只对整数有效，因此我们需要转换为整数
                left_val = (
                    int(left_obj.value)
                    if isinstance(left_obj, RealObject)
                    else left_obj.value
                )
                right_val = (
                    int(right_obj.value)
                    if isinstance(right_obj, RealObject)
                    else right_obj.value
                )
                return IntegerObject(left_val % right_val)

        # comparison operator
        if node.op.type == TokenType.LT:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj < right_obj
            elif isinstance(left_obj, CharObject) and isinstance(right_obj, CharObject):
                return left_obj < right_obj
            elif isinstance(left_obj, EnumObject) and isinstance(right_obj, EnumObject):
                return left_obj < right_obj
        elif node.op.type == TokenType.GT:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj > right_obj
            elif isinstance(left_obj, CharObject) and isinstance(right_obj, CharObject):
                return left_obj > right_obj
            elif isinstance(left_obj, EnumObject) and isinstance(right_obj, EnumObject):
                return left_obj > right_obj
        elif node.op.type == TokenType.EQ:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return BooleanObject(value=left_obj == right_obj)
            elif isinstance(left_obj, BooleanObject) and isinstance(
                right_obj, BooleanObject
            ):
                return BooleanObject(value=left_obj == right_obj)
            elif isinstance(left_obj, CharObject) and isinstance(right_obj, CharObject):
                return BooleanObject(value=left_obj == right_obj)
            elif isinstance(left_obj, EnumObject) and isinstance(right_obj, EnumObject):
                return BooleanObject(value=left_obj == right_obj)
        elif node.op.type == TokenType.NE:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return BooleanObject(value=left_obj != right_obj)
            elif isinstance(left_obj, BooleanObject) and isinstance(
                right_obj, BooleanObject
            ):
                return BooleanObject(value=left_obj != right_obj)
            elif isinstance(left_obj, CharObject) and isinstance(right_obj, CharObject):
                return BooleanObject(value=left_obj != right_obj)
            elif isinstance(left_obj, EnumObject) and isinstance(right_obj, EnumObject):
                return BooleanObject(value=left_obj != right_obj)
        elif node.op.type == TokenType.LE:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj <= right_obj
            elif isinstance(left_obj, CharObject) and isinstance(right_obj, CharObject):
                return left_obj <= right_obj
            elif isinstance(left_obj, EnumObject) and isinstance(right_obj, EnumObject):
                return left_obj <= right_obj
        elif node.op.type == TokenType.GE:
            if isinstance(left_obj, NumberObject) and isinstance(
                right_obj, NumberObject
            ):
                return left_obj >= right_obj
            elif isinstance(left_obj, CharObject) and isinstance(right_obj, CharObject):
                return left_obj >= right_obj
            elif isinstance(left_obj, EnumObject) and isinstance(right_obj, EnumObject):
                return left_obj >= right_obj

        # !!
        raise InterpreterError(
            error_code=ErrorCode.INTERPRETER_UNKNOWN_OPERATOR,
            token=node.token,
            message=f"{ErrorCode.INTERPRETER_UNKNOWN_OPERATOR.value} -> {node.token}",
        )

    def visit_Num(self, node: Num) -> NumberObject:
        if isinstance(node.value, int):
            return IntegerObject(node.value)
        else:
            return RealObject(node.value)

    def visit_String(self, node: String) -> StringObject:
        return StringObject(node.value)

    def visit_Char(self, node: Char) -> CharObject:
        return CharObject(node.value)

    def visit_Bool(self, node: Bool) -> BooleanObject:
        if node.token.type == TokenType.TRUE:
            return BooleanObject(True)
        elif node.token.type == TokenType.FALSE:
            return BooleanObject(False)
        raise InterpreterError(
            error_code=ErrorCode.INTERPRETER_UNKNOWN_BOOLEAN,
            token=node.token,
            message=f"{ErrorCode.INTERPRETER_UNKNOWN_BOOLEAN.value} -> {node.token}",
        )

    def visit_UnaryOp(self, node: UnaryOp) -> Object:
        expr_obj = self.visit(node.expr)
        op = node.op.type

        # negative bang
        if op == TokenType.NOT:
            if isinstance(expr_obj, BooleanObject):
                return ~expr_obj
            else:
                return BooleanObject(not expr_obj.to_bool())

        # signal bang
        if op == TokenType.PLUS:
            if isinstance(expr_obj, NumberObject):
                return +expr_obj
        elif op == TokenType.MINUS:
            if isinstance(expr_obj, NumberObject):
                return -expr_obj

        raise InterpreterError(
            error_code=ErrorCode.INTERPRETER_UNKNOWN_OPERATOR,
            token=node.token,
            message=f"{ErrorCode.INTERPRETER_UNKNOWN_OPERATOR.value} -> {node.token}",
        )

    def visit_Compound(self, node: Compound) -> None:
        for child in node.children:
            self.visit(child)

    def visit_Assign(self, node: Assign) -> None:
        var_value = self.visit(node.right)
        ar = self.call_stack.peek()

        if isinstance(node.left, AccessExpression):
            # Handle access expression assignments (e.g., arr[i] := value or record.field := value)
            # Get the base object
            base_obj = self.visit(node.left.base)
            current_obj = base_obj

            # Handle all suffixes except the last one
            for suffix in node.left.suffixes[:-1]:
                if isinstance(suffix, IndexSuffix):
                    # Handle array/string index access
                    index_obj = self.visit(suffix.index)
                    index = (
                        index_obj.value if isinstance(index_obj, NumberObject) else 0
                    )

                    if isinstance(current_obj, StringObject):
                        current_obj = current_obj[index]
                    elif isinstance(current_obj, ArrayObject):
                        current_obj = current_obj[index]
                    else:
                        # Unsupported indexing
                        return

                elif isinstance(suffix, MemberSuffix):
                    # Handle record member access
                    field_name = suffix.member.value

                    if isinstance(current_obj, RecordObject):
                        # Check if accessing variant field and initialize if needed
                        if (
                            current_obj.record_type.variant_part
                            and current_obj._is_variant_field(field_name)
                        ):
                            # Get current tag field value to ensure variant fields are initialized
                            tag_field_name = (
                                current_obj.record_type.variant_part.tag_field.value
                            )
                            tag_field_value = current_obj.fields.get(tag_field_name)
                            if tag_field_value and hasattr(tag_field_value, "value"):
                                current_obj._init_variant_fields(
                                    str(tag_field_value.value)
                                )

                        current_obj = current_obj[field_name]
                    else:
                        # Unsupported member access
                        return

            # Handle the last suffix for assignment
            last_suffix = node.left.suffixes[-1]
            if isinstance(last_suffix, IndexSuffix):
                # Handle array/string index assignment
                index_obj = self.visit(last_suffix.index)
                index = index_obj.value if isinstance(index_obj, NumberObject) else 0

                if isinstance(current_obj, StringObject) and isinstance(
                    var_value, (StringObject, CharObject)
                ):
                    # Handle string character assignment
                    if (
                        isinstance(var_value, StringObject)
                        and len(var_value.value) == 1
                    ):
                        current_obj[index] = CharObject(var_value.value)
                    elif isinstance(var_value, CharObject):
                        current_obj[index] = var_value
                elif isinstance(current_obj, ArrayObject):
                    # Handle array element assignment
                    current_obj[index] = var_value

            elif isinstance(last_suffix, MemberSuffix):
                # Handle record member assignment
                field_name = last_suffix.member.value

                if isinstance(current_obj, RecordObject):
                    # Check if assigning to variant field and initialize if needed
                    if (
                        current_obj.record_type.variant_part
                        and current_obj._is_variant_field(field_name)
                    ):
                        # Get current tag field value to ensure variant fields are initialized
                        tag_field_name = (
                            current_obj.record_type.variant_part.tag_field.value
                        )
                        tag_field_value = current_obj.fields.get(tag_field_name)
                        if tag_field_value and hasattr(tag_field_value, "value"):
                            if isinstance(tag_field_value, EnumObject):
                                current_obj._init_variant_fields(tag_field_value.name)
                            else:
                                current_obj._init_variant_fields(
                                    str(tag_field_value.value)
                                )

                    current_obj[field_name] = var_value

            return
        elif isinstance(node.left, Var):
            var_name = node.left.value
        else:
            # Fallback: treat as variable assignment if it has a value attribute
            var_name = getattr(node.left, "value", None)
            if var_name is None:
                return

        # Handle regular variable assignment
        existing_var = ar.get(var_name)
        if isinstance(existing_var, StringObject) and isinstance(
            var_value, StringObject
        ):
            # Handle string assignment with limit checking
            if existing_var.limit > 0 and len(var_value.value) > existing_var.limit:
                message = f"Warning: String literal has more characters[{len(var_value.value)}] than short string length[{existing_var.limit}]"
                SpiUtil.print_w(message=message)
                ar[var_name] = StringObject(
                    var_value.value[: existing_var.limit], existing_var.limit
                )
            else:
                ar[var_name] = StringObject(var_value.value, existing_var.limit)
        elif isinstance(existing_var, CharObject) and isinstance(
            var_value, StringObject
        ):
            ar[var_name] = CharObject(value=var_value.value)
        else:
            ar[var_name] = var_value

    def visit_Var(self, node: Var) -> Object:
        var_name = node.value
        ar = self.call_stack.peek()
        var_value = ar.get(var_name)

        # 如果在当前作用域中找不到该变量，检查它是否是枚举值
        if var_value is None:
            # 检查是否是已注册的枚举值
            if var_name in self.enum_values:
                enum_info = self.enum_values[var_name]
                return EnumObject(enum_info["type"], var_name, enum_info["ordinal"])
        return var_value if var_value is not None else NullObject()

    def visit_AccessExpression(self, node: AccessExpression) -> Object:
        # Start with the base value
        base_obj = self.visit(node.base)
        current_obj = base_obj

        # Apply each suffix in sequence
        for suffix in node.suffixes:
            if isinstance(suffix, IndexSuffix):
                # Array/string index access
                index_obj = self.visit(suffix.index)
                index = index_obj.value if isinstance(index_obj, NumberObject) else 0

                if isinstance(current_obj, StringObject):
                    current_obj = current_obj[index]
                elif isinstance(current_obj, ArrayObject):
                    current_obj = cast(Object, current_obj[index])
                else:
                    # Return null object for unsupported indexing
                    return NullObject()

            elif isinstance(suffix, MemberSuffix):
                # Member access for records/enums
                field_name = suffix.member.value

                if isinstance(current_obj, RecordObject):
                    # Direct field access - variant fields are already initialized
                    # when the tag field is set in RecordObject.__setitem__
                    current_obj = current_obj[field_name]
                else:
                    # Return null object for unsupported member access
                    return NullObject()

        return current_obj if current_obj is not None else NullObject()

    def visit_IndexSuffix(self, node: IndexSuffix) -> Object:
        result = self.visit(node.index)
        return result if isinstance(result, Object) else NullObject()

    def visit_MemberSuffix(self, node: MemberSuffix) -> Object:
        # Member access not implemented yet
        return NullObject()

    def visit_NoOp(self, node: NoOp) -> None:
        pass

    def visit_WhileStatement(self, node: WhileStatement) -> None:
        while self.visit(node.condition).to_bool():
            self.visit(node.block)

    def visit_ForStatement(self, node: ForStatement) -> None:
        ar = self.call_stack.peek()
        var_name = node.initialization.left.value
        self.visit(node.initialization)
        bound_obj = self.visit(node.bound)
        bound_value = bound_obj.value if isinstance(bound_obj, NumberObject) else 0

        var_obj = ar[var_name]
        var_value = var_obj.value if isinstance(var_obj, NumberObject) else 0

        # 检查是否是枚举类型
        if isinstance(var_obj, EnumObject) and isinstance(bound_obj, EnumObject):
            # 枚举类型的for循环
            if var_obj.type_name == bound_obj.type_name:
                start_ord = var_obj.ordinal
                end_ord = bound_obj.ordinal
                step = 1 if start_ord <= end_ord else -1

                # 获取枚举类型信息（需要从语义分析器获取）
                # 这里使用简化实现
                current_ord = start_ord
                while (step > 0 and current_ord <= end_ord) or (
                    step < 0 and current_ord >= end_ord
                ):
                    # 更新循环变量为当前枚举值
                    # 使用_enum_obj方法创建正确的枚举对象
                    ar[var_name] = self._enum_obj(var_obj.type_name, current_ord)
                    self.visit(node.block)
                    current_ord += step
        else:
            # 原来的数值类型for循环
            while var_value <= bound_value:
                self.visit(node.block)
                var_value += 1
                if var_value <= bound_value:
                    ar[var_name] = IntegerObject(var_value)

    def visit_ProcedureDecl(self, node: ProcedureDecl) -> None:
        pass

    def visit_ProcedureCall(self, node: ProcedureCall) -> None:
        proc_name = node.proc_name
        proc_symbol = node.proc_symbol

        if proc_symbol is None:
            raise InterpreterError(
                error_code=ErrorCode.NULL_POINTER,
                token=node.token,
                message=f"{ErrorCode.NULL_POINTER.value} -> {node.token}",
            )

        ar = ActivationRecord(
            name=proc_name,
            type=ARType.PROCEDURE,
            nesting_level=proc_symbol.scope_level + 1,
        )

        pre_ar = self.call_stack.peek()
        if pre_ar is not None:
            ar.copy_from(pre_ar, False)

        # deal with built-in procedure first
        if isinstance(proc_symbol, BuiltinProcedureSymbol):
            # Look up the built-in procedure in the registry
            handler = BUILTIN_PROCEDURES.get(proc_symbol.name.upper())
            if handler:
                # Prepare parameters
                actual_params = node.actual_params
                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                # Call the handler
                handler(self, node)

                self.call_stack.pop()
                return
            else:
                raise InterpreterError(
                    error_code=ErrorCode.INTERPRETER_UNKNOWN_BUILTIN_PROCEDURE,
                    token=node.token,
                    message=f"{ErrorCode.INTERPRETER_UNKNOWN_BUILTIN_PROCEDURE.value} -> {node.token}",
                )
        else:
            formal_params = proc_symbol.formal_params
            actual_params = node.actual_params

            for param_symbol, argument_node in zip(formal_params, actual_params):
                ar[param_symbol.name] = self.visit(argument_node)

            self.call_stack.push(ar)

            self.log(f"ENTER: PROCEDURE {proc_name}")
            self.log(str(self.call_stack))

            # evaluate procedure body
            if proc_symbol.block_ast is None:
                raise InterpreterError(
                    error_code=ErrorCode.NULL_POINTER,
                    token=None,
                    message=f"{ErrorCode.NULL_POINTER.value}",
                )
            self.visit(proc_symbol.block_ast)

            self.log(f"LEAVE: PROCEDURE {proc_name}")
            self.log(str(self.call_stack))

            self.call_stack.pop()
            pass

    def visit_FunctionDecl(self, node: FunctionDecl) -> None:
        pass

    def visit_IfStatement(self, node: IfStatement) -> None:
        flag = self.visit(node.condition).to_bool()

        if flag:
            self.visit(node.then_branch)
            return
        else:
            for branch in node.else_if_branches:
                sub_flag = self.visit(branch.condition).to_bool()
                if sub_flag:
                    self.visit(branch.then_branch)
                    return

        if node.else_branch is not None:
            self.visit(node.else_branch)

    def visit_CaseStatement(self, node: CaseStatement) -> None:
        # 计算case表达式的值
        case_value_obj = self.visit(node.case_expr)
        case_value = (
            case_value_obj.value if hasattr(case_value_obj, "value") else case_value_obj
        )

        # 查找匹配的case项
        matched = False
        for case_item in node.case_items:
            for label in case_item.labels:
                label_value = label.value
                # 检查是否匹配
                # 对于枚举值，我们需要比较名称而不是值
                if isinstance(case_value_obj, EnumObject):
                    # 如果case表达式是枚举对象，标签值应该是枚举值的名称
                    if label_value == case_value_obj.name:
                        # 执行匹配的语句
                        self.visit(case_item.statement)
                        matched = True
                        break
                elif case_value == label_value:
                    # 执行匹配的语句
                    self.visit(case_item.statement)
                    matched = True
                    break
            if matched:
                break

        # 如果没有匹配项且有else语句，则执行else语句
        if not matched and node.else_stmt:
            self.visit(node.else_stmt)

    def visit_FunctionCall(self, node: FunctionCall) -> Object:
        func_name = node.func_name
        func_symbol = node.func_symbol

        if func_symbol is None:
            raise InterpreterError(
                error_code=ErrorCode.NULL_POINTER,
                token=None,
                message=f"{ErrorCode.NULL_POINTER.value}",
            )

        ar = ActivationRecord(
            name=func_name,
            type=ARType.FUNCTION,
            nesting_level=func_symbol.scope_level + 1,
        )

        pre_ar = self.call_stack.peek()
        if pre_ar is not None:
            ar.copy_from(pre_ar, False)

        # deal with built-in function first
        if isinstance(func_symbol, BuiltinFunctionSymbol):
            # Look up the built-in function in the registry
            handler = BUILTIN_FUNCTIONS.get(func_symbol.name.upper())
            if handler:
                # Prepare parameters
                actual_params = node.actual_params
                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                # Call the handler and get the result
                result = handler(self, node)

                self.call_stack.pop()
                return result
            else:
                raise InterpreterError(
                    error_code=ErrorCode.INTERPRETER_UNKNOWN_BUILTIN_FUNCTION,
                    token=None,
                    message=f"{ErrorCode.INTERPRETER_UNKNOWN_BUILTIN_FUNCTION.value}",
                )
        else:
            formal_params = func_symbol.formal_params
            actual_params = node.actual_params

            for param_symbol, argument_node in zip(formal_params, actual_params):
                ar[param_symbol.name] = self.visit(argument_node)

            self.call_stack.push(ar)

            self.log(f"ENTER: FUNCTION {func_name}")
            self.log(str(self.call_stack))

            # evaluate procedure body
            if func_symbol.block_ast is None:
                raise InterpreterError(
                    error_code=ErrorCode.NULL_POINTER,
                    token=None,
                    message=f"{ErrorCode.NULL_POINTER.value}",
                )
            self.visit(func_symbol.block_ast)

            self.log(f"LEAVE: FUNCTION {func_name}")
            self.log(str(self.call_stack))

            result = ar.get(func_name)
            self.call_stack.pop()

            return result if result is not None else NullObject()

    def interpret(self):
        tree = self.tree
        if tree is None:
            return ""
        return self.visit(tree)


def runSpi() -> None:
    arg_parser = argparse.ArgumentParser(description="SPI - Simple Pascal Interpreter")
    arg_parser.add_argument("inputfile", help="Pascal source file")
    arg_parser.add_argument(
        "--scope",
        help="Print scope information",
        action="store_true",
    )
    arg_parser.add_argument(
        "--stack",
        help="Print call stack",
        action="store_true",
    )
    args = arg_parser.parse_args()

    global _SHOULD_LOG_SCOPE, _SHOULD_LOG_STACK
    _SHOULD_LOG_SCOPE = args.scope
    _SHOULD_LOG_STACK = args.stack

    text = open(args.inputfile, "r", encoding="utf-8").read()

    lexer = Lexer(text)
    try:
        parser = Parser(lexer)
        tree = parser.parse()
    except (LexerError, ParserError) as e:
        print(e.message)
        sys.exit(1)

    semantic_analyzer = SemanticAnalyzer()
    try:
        semantic_analyzer.visit(tree)
    except SemanticError as e:
        print(e.message)
        sys.exit(1)

    interpreter = Interpreter(tree)
    interpreter.interpret()
