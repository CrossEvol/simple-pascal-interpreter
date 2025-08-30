"""
Type resolution service for Pascal interpreter.

This module provides the TypeResolver class that handles type resolution
during semantic analysis and interpretation phases, separating type validation
from parsing responsibilities.
"""

from __future__ import annotations
from typing import Dict, List
from dataclasses import dataclass
import difflib

from src.spi_ast import (
    Type,
    UnresolvedType,
    ClassType,
    EnumType,
    RecordType,
    PrimitiveType,
    StringType,
)
from src.spi_token import Token, TokenType
from src.error import TypeResolutionError
from src.module import ModuleRegistry


@dataclass
class TypeResolutionContext:
    """Context information for type resolution."""

    current_module: str | None = None
    imported_modules: List[str] = None
    local_classes: List[str] = None
    local_enums: List[str] = None
    local_records: List[str] = None
    module_registry: ModuleRegistry | None = None

    def __post_init__(self):
        if self.imported_modules is None:
            self.imported_modules = []
        if self.local_classes is None:
            self.local_classes = []
        if self.local_enums is None:
            self.local_enums = []
        if self.local_records is None:
            self.local_records = []


@dataclass
class TypeResolutionResult:
    """Result of type resolution attempt."""

    success: bool
    resolved_type: Type | None = None
    error_message: str | None = None
    suggestions: List[str] = None

    def __post_init__(self):
        if self.suggestions is None:
            self.suggestions = []


class TypeResolver:
    """
    Handles type resolution during semantic analysis and interpretation.

    This service resolves UnresolvedType nodes to concrete type nodes,
    providing clear error messages and suggestions when resolution fails.
    """

    def __init__(self, module_registry: ModuleRegistry = None):
        """
        Initialize the TypeResolver.

        Args:
            module_registry: Registry for accessing loaded modules and their types
        """
        self.module_registry = module_registry
        self._type_cache: Dict[str, Type] = {}

    def resolve_type(
        self, unresolved_type: UnresolvedType, context: TypeResolutionContext
    ) -> Type:
        """
        Resolve an unresolved type to a concrete type.

        Args:
            unresolved_type: The unresolved type node from parsing
            context: Current resolution context (imported modules, local types, etc.)

        Returns:
            Concrete type node (ClassType, EnumType, RecordType, etc.)

        Raises:
            TypeResolutionError: If type cannot be resolved
        """
        if unresolved_type.resolved_type is not None:
            # Already resolved, return cached result
            return unresolved_type.resolved_type

        type_name = unresolved_type.type_name

        # Create cache key for this resolution context
        cache_key = self._create_cache_key(type_name, context)

        # Check cache first
        if cache_key in self._type_cache:
            resolved_type = self._type_cache[cache_key]
            unresolved_type.resolved_type = resolved_type
            return resolved_type

        # Try to resolve the type
        result = self._attempt_type_resolution(
            type_name, context, unresolved_type.token
        )

        if result.success and result.resolved_type is not None:
            # Cache the successful resolution
            self._type_cache[cache_key] = result.resolved_type
            unresolved_type.resolved_type = result.resolved_type
            return result.resolved_type
        else:
            # Resolution failed, raise error with suggestions
            raise TypeResolutionError(
                message=result.error_message or f"Unknown type '{type_name}'",
                suggestions=result.suggestions,
                token=unresolved_type.token,
            )

    def _attempt_type_resolution(
        self, type_name: str, context: TypeResolutionContext, token: Token
    ) -> TypeResolutionResult:
        """
        Attempt to resolve a type name to a concrete type.

        Args:
            type_name: Name of the type to resolve
            context: Resolution context
            token: Token for error reporting

        Returns:
            TypeResolutionResult with success status and resolved type or error info
        """
        # Resolution order:
        # 1. Local types (classes, enums, records)
        # 2. Imported module types
        # 3. Built-in primitive types (fallback)

        # Check local classes
        if type_name in context.local_classes:
            return TypeResolutionResult(success=True, resolved_type=ClassType(token))

        # Check local enums
        if type_name in context.local_enums:
            return TypeResolutionResult(success=True, resolved_type=EnumType(token))

        # Check local records
        if type_name in context.local_records:
            return TypeResolutionResult(success=True, resolved_type=RecordType(token))

        # Check imported modules
        if self.module_registry and context.imported_modules:
            module_result = self._resolve_from_modules(
                type_name, context.imported_modules, token
            )
            if module_result.success:
                return module_result

        # Check if it's a primitive type that wasn't caught by parser
        primitive_result = self._resolve_primitive_type(type_name, token)
        if primitive_result.success:
            return primitive_result

        # Type not found - generate suggestions and error message
        suggestions = self._get_type_suggestions(type_name, context)
        error_message = self._format_type_resolution_error(
            type_name, token, context, suggestions
        )

        return TypeResolutionResult(
            success=False, error_message=error_message, suggestions=suggestions
        )

    def _resolve_from_modules(
        self, type_name: str, imported_modules: List[str], token: Token
    ) -> TypeResolutionResult:
        """
        Resolve type from imported modules.

        Args:
            type_name: Name of the type to resolve
            imported_modules: List of imported module names
            token: Token for creating resolved type

        Returns:
            TypeResolutionResult with resolution status
        """
        if not self.module_registry:
            return TypeResolutionResult(success=False)

        for module_name in imported_modules:
            module = self.module_registry.get_module(module_name)
            if not module or not module.is_loaded:
                continue

            # Check if the type exists in the module's interface symbols
            interface_symbols = module.interface_symbols.get_interface_symbols()

            for symbol_name, symbol in interface_symbols.items():
                if symbol_name == type_name:
                    # Determine the type based on symbol type
                    if hasattr(symbol, "symbol_type"):
                        if "Class" in str(symbol.symbol_type):
                            return TypeResolutionResult(
                                success=True, resolved_type=ClassType(token)
                            )
                        elif "Enum" in str(symbol.symbol_type):
                            return TypeResolutionResult(
                                success=True, resolved_type=EnumType(token)
                            )
                        elif "Record" in str(symbol.symbol_type):
                            return TypeResolutionResult(
                                success=True, resolved_type=RecordType(token)
                            )

        return TypeResolutionResult(success=False)

    def _resolve_primitive_type(
        self, type_name: str, token: Token
    ) -> TypeResolutionResult:
        """
        Resolve primitive types that might have been missed by parser.

        Args:
            type_name: Name of the type to resolve
            token: Token for creating resolved type

        Returns:
            TypeResolutionResult with resolution status
        """
        primitive_types = {
            "Integer": TokenType.INTEGER,
            "Real": TokenType.REAL,
            "Boolean": TokenType.BOOLEAN,
            "String": TokenType.STRING,
        }

        if type_name in primitive_types:
            # Create a new token with the correct type
            primitive_token = Token(
                type=primitive_types[type_name],
                value=type_name,
                lineno=token.lineno,
                column=token.column,
            )

            if type_name == "String":
                return TypeResolutionResult(
                    success=True, resolved_type=StringType(primitive_token)
                )
            else:
                return TypeResolutionResult(
                    success=True, resolved_type=PrimitiveType(primitive_token)
                )

        return TypeResolutionResult(success=False)

    def _get_type_suggestions(
        self, type_name: str, context: TypeResolutionContext
    ) -> List[str]:
        """
        Provide suggestions for similar type names using fuzzy matching.

        Args:
            type_name: The unknown type name
            context: Resolution context with available types

        Returns:
            List of suggested type names
        """
        available_types = []

        # Collect all available types
        available_types.extend(context.local_classes)
        available_types.extend(context.local_enums)
        available_types.extend(context.local_records)

        # Add primitive types
        available_types.extend(["Integer", "Real", "Boolean", "String"])

        # Add types from imported modules
        if self.module_registry and context.imported_modules:
            for module_name in context.imported_modules:
                module = self.module_registry.get_module(module_name)
                if module and module.is_loaded:
                    interface_symbols = module.interface_symbols.get_interface_symbols()
                    for symbol_name in interface_symbols.keys():
                        # Add both qualified and unqualified names for better matching
                        available_types.append(symbol_name)
                        available_types.append(f"{module_name}.{symbol_name}")

        # Remove duplicates and sort
        available_types = list(set(available_types))

        # Use difflib to find close matches
        close_matches = difflib.get_close_matches(
            type_name,
            available_types,
            n=3,  # Return up to 3 suggestions
            cutoff=0.6,  # Minimum similarity ratio
        )

        return close_matches

    def _format_type_resolution_error(
        self,
        type_name: str,
        token: Token,
        context: TypeResolutionContext,
        suggestions: List[str],
    ) -> str:
        """
        Format a comprehensive error message for type resolution failure.

        Args:
            type_name: The unknown type name
            token: Token for location information
            context: Resolution context
            suggestions: List of suggested type names

        Returns:
            Formatted error message string
        """
        message = f"Unknown type '{type_name}'"

        if token and hasattr(token, "lineno"):
            message += f" at line {token.lineno}"
            if hasattr(token, "column"):
                message += f", column {token.column}"

        # Add available types information
        available_types = []
        if context.local_classes:
            available_types.extend([f"Class: {cls}" for cls in context.local_classes])
        if context.local_enums:
            available_types.extend([f"Enum: {enum}" for enum in context.local_enums])
        if context.local_records:
            available_types.extend([f"Record: {rec}" for rec in context.local_records])

        if available_types:
            message += f"\n  Available local types: {', '.join(available_types)}"

        # Add imported module types
        if self.module_registry and context.imported_modules:
            imported_types = []
            for module_name in context.imported_modules:
                module = self.module_registry.get_module(module_name)
                if module and module.is_loaded:
                    interface_symbols = module.interface_symbols.get_interface_symbols()
                    for symbol_name in interface_symbols.keys():
                        imported_types.append(f"{module_name}.{symbol_name}")

            if imported_types:
                message += f"\n  Available imported types: {', '.join(imported_types)}"

        return message

    def _create_cache_key(self, type_name: str, context: TypeResolutionContext) -> str:
        """
        Create a cache key for type resolution results.

        Args:
            type_name: Name of the type
            context: Resolution context

        Returns:
            Cache key string
        """
        # Create a key that includes the type name and context
        key_parts = [type_name]

        if context.current_module:
            key_parts.append(f"module:{context.current_module}")

        if context.imported_modules:
            key_parts.append(f"imports:{','.join(sorted(context.imported_modules))}")

        if context.local_classes:
            key_parts.append(f"classes:{','.join(sorted(context.local_classes))}")

        if context.local_enums:
            key_parts.append(f"enums:{','.join(sorted(context.local_enums))}")

        if context.local_records:
            key_parts.append(f"records:{','.join(sorted(context.local_records))}")

        return "|".join(key_parts)

    def clear_cache(self) -> None:
        """Clear the type resolution cache."""
        self._type_cache.clear()

    def get_cache_stats(self) -> Dict[str, int]:
        """
        Get cache statistics for debugging and optimization.

        Returns:
            Dictionary with cache statistics
        """
        return {
            "cache_size": len(self._type_cache),
            "cached_types": list(self._type_cache.keys()),
        }

    def resolve_qualified_type(
        self, module_name: str, type_name: str, token: Token
    ) -> Type:
        """
        Resolve a module-qualified type name (e.g., Math.Number).

        Args:
            module_name: Name of the module
            type_name: Name of the type within the module
            token: Token for error reporting

        Returns:
            Resolved Type node

        Raises:
            TypeResolutionError: If type cannot be resolved
        """
        if not self.module_registry:
            raise TypeResolutionError(
                message=f"Cannot resolve qualified type '{module_name}.{type_name}' - no module registry available",
                token=token,
            )

        module = self.module_registry.get_module(module_name)
        if not module or not module.is_loaded:
            raise TypeResolutionError(
                message=f"Module '{module_name}' not found or not loaded",
                suggestions=[f"Ensure '{module_name}' is imported and available"],
                token=token,
            )

        # Check if the type exists in the module's interface symbols
        interface_symbols = module.interface_symbols.get_interface_symbols()

        if type_name not in interface_symbols:
            available_symbols = list(interface_symbols.keys())
            suggestions = difflib.get_close_matches(
                type_name, available_symbols, n=3, cutoff=0.6
            )

            raise TypeResolutionError(
                message=f"Type '{type_name}' not found in module '{module_name}'",
                suggestions=suggestions
                if suggestions
                else [
                    f"Available types in {module_name}: {', '.join(available_symbols)}"
                ],
                token=token,
            )

        symbol = interface_symbols[type_name]

        # Determine the type based on symbol type
        if hasattr(symbol, "symbol_type"):
            if "Class" in str(symbol.symbol_type):
                return ClassType(token)
            elif "Enum" in str(symbol.symbol_type):
                return EnumType(token)
            elif "Record" in str(symbol.symbol_type):
                return RecordType(token)

        # Default to treating it as a class type if we can't determine the specific type
        return ClassType(token)
