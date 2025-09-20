"""SPI - Simple Pascal Interpreter. Part 19"""

from __future__ import annotations

from enum import Enum

_SHOULD_LOG_SCOPE = False  # see '--scope' command line option
_SHOULD_LOG_STACK = False  # see '--stack' command line option


class ElementType(Enum):
    INTEGER = "INTEGER"
    REAL = "REAL"
    BOOL = "BOOL"
    STRING = "STRING"
    CHAR = "CHAR"
    ARRAY = "ARRAY"
    RECORD = "RECORD"
    CUSTOM = "CUSTOM"
