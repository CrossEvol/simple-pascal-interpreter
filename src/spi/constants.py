"""SPI - Simple Pascal Interpreter. Part 19"""

from __future__ import annotations

from enum import Enum


class SpiConfig:
    """Configuration class to manage SPI interpreter settings."""

    def __init__(self) -> None:
        self.should_log_scope: bool = False  # see '--scope' command line option
        self.should_log_stack: bool = False  # see '--stack' command line option

    def set_log_scope(self, value: bool) -> None:
        """Set the scope logging flag."""
        self.should_log_scope = value

    def set_log_stack(self, value: bool) -> None:
        """Set the stack logging flag."""
        self.should_log_stack = value


# Global configuration instance
CONFIG = SpiConfig()


class ElementType(Enum):
    INTEGER = "INTEGER"
    REAL = "REAL"
    BOOL = "BOOL"
    STRING = "STRING"
    CHAR = "CHAR"
    ARRAY = "ARRAY"
    RECORD = "RECORD"
    CUSTOM = "CUSTOM"
