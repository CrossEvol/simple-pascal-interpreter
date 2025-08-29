"""
Visibility levels for symbols in the module system.
"""

from enum import Enum


class VisibilityLevel(Enum):
    """Enumeration for symbol visibility levels in modules."""
    INTERFACE = "interface"
    IMPLEMENTATION = "implementation"
    PRIVATE = "private"