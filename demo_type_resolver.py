#!/usr/bin/env python3
"""
Demonstration script for TypeResolver functionality.

This script shows how the TypeResolver service works with various
type resolution scenarios.
"""

from src.type_resolver import TypeResolver, TypeResolutionContext
from src.spi_ast import UnresolvedType, ClassType, EnumType, RecordType
from src.spi_token import Token, TokenType
from src.error import TypeResolutionError
from src.module import ModuleRegistry


def create_test_token(type_name: str, line: int = 1, col: int = 1) -> Token:
    """Create a test token for demonstration."""
    return Token(
        type=TokenType.ID,
        value=type_name,
        lineno=line,
        column=col
    )


def demo_basic_type_resolution():
    """Demonstrate basic type resolution scenarios."""
    print("=== Basic Type Resolution Demo ===")
    
    # Create TypeResolver
    resolver = TypeResolver()
    
    # Test 1: Resolve local class type
    print("\n1. Resolving local class type 'User':")
    unresolved = UnresolvedType(create_test_token("User"), "User")
    context = TypeResolutionContext(
        local_classes=["User", "Product", "Order"]
    )
    
    try:
        resolved = resolver.resolve_type(unresolved, context)
        print(f"   ✓ Resolved to: {type(resolved).__name__}")
        print(f"   ✓ Token value: {resolved.token.value}")
    except TypeResolutionError as e:
        print(f"   ✗ Error: {e}")
    
    # Test 2: Resolve local enum type
    print("\n2. Resolving local enum type 'Status':")
    unresolved = UnresolvedType(create_test_token("Status"), "Status")
    context = TypeResolutionContext(
        local_enums=["Status", "Priority", "Color"]
    )
    
    try:
        resolved = resolver.resolve_type(unresolved, context)
        print(f"   ✓ Resolved to: {type(resolved).__name__}")
    except TypeResolutionError as e:
        print(f"   ✗ Error: {e}")
    
    # Test 3: Resolve primitive type
    print("\n3. Resolving primitive type 'Integer':")
    unresolved = UnresolvedType(create_test_token("Integer"), "Integer")
    context = TypeResolutionContext()
    
    try:
        resolved = resolver.resolve_type(unresolved, context)
        print(f"   ✓ Resolved to: {type(resolved).__name__}")
        print(f"   ✓ Token type: {resolved.token.type}")
    except TypeResolutionError as e:
        print(f"   ✗ Error: {e}")


def demo_error_handling_and_suggestions():
    """Demonstrate error handling and suggestion system."""
    print("\n=== Error Handling and Suggestions Demo ===")
    
    resolver = TypeResolver()
    
    # Test 1: Unknown type with suggestions
    print("\n1. Unknown type 'Usr' (typo in 'User'):")
    unresolved = UnresolvedType(create_test_token("Usr", 10, 5), "Usr")
    context = TypeResolutionContext(
        local_classes=["User", "UserProfile"],
        local_enums=["UserStatus"],
        local_records=["UserData"]
    )
    
    try:
        resolved = resolver.resolve_type(unresolved, context)
        print(f"   ✓ Resolved to: {type(resolved).__name__}")
    except TypeResolutionError as e:
        print(f"   ✗ Error occurred (as expected)")
        print(f"   ✓ Error message: {str(e).split(chr(10))[0]}")  # First line only
        print(f"   ✓ Suggestions: {e.suggestions}")
    
    # Test 2: Completely unknown type
    print("\n2. Completely unknown type 'NonExistent':")
    unresolved = UnresolvedType(create_test_token("NonExistent"), "NonExistent")
    context = TypeResolutionContext(
        local_classes=["Apple", "Banana"],
        local_enums=["Color"]
    )
    
    try:
        resolved = resolver.resolve_type(unresolved, context)
        print(f"   ✓ Resolved to: {type(resolved).__name__}")
    except TypeResolutionError as e:
        print(f"   ✗ Error occurred (as expected)")
        print(f"   ✓ No close suggestions found: {len(e.suggestions) == 0}")


def demo_caching_behavior():
    """Demonstrate caching behavior."""
    print("\n=== Caching Behavior Demo ===")
    
    resolver = TypeResolver()
    
    # Create context
    context = TypeResolutionContext(
        local_classes=["CachedType"]
    )
    
    print("\n1. Initial cache state:")
    stats = resolver.get_cache_stats()
    print(f"   Cache size: {stats['cache_size']}")
    
    print("\n2. First resolution of 'CachedType':")
    unresolved1 = UnresolvedType(create_test_token("CachedType"), "CachedType")
    resolved1 = resolver.resolve_type(unresolved1, context)
    print(f"   ✓ Resolved to: {type(resolved1).__name__}")
    
    stats = resolver.get_cache_stats()
    print(f"   Cache size after first resolution: {stats['cache_size']}")
    
    print("\n3. Second resolution of 'CachedType' (should use cache):")
    unresolved2 = UnresolvedType(create_test_token("CachedType"), "CachedType")
    resolved2 = resolver.resolve_type(unresolved2, context)
    print(f"   ✓ Resolved to: {type(resolved2).__name__}")
    
    stats = resolver.get_cache_stats()
    print(f"   Cache size after second resolution: {stats['cache_size']}")
    print(f"   Cached types: {stats['cached_types']}")
    
    print("\n4. Clearing cache:")
    resolver.clear_cache()
    stats = resolver.get_cache_stats()
    print(f"   Cache size after clearing: {stats['cache_size']}")


def demo_priority_resolution():
    """Demonstrate resolution priority order."""
    print("\n=== Resolution Priority Demo ===")
    
    resolver = TypeResolver()
    
    # Create context where same name exists in multiple scopes
    print("\n1. Type 'TestType' exists as both class and enum:")
    context = TypeResolutionContext(
        local_classes=["TestType"],  # Should have priority
        local_enums=["TestType"]
    )
    
    unresolved = UnresolvedType(create_test_token("TestType"), "TestType")
    resolved = resolver.resolve_type(unresolved, context)
    
    print(f"   ✓ Resolved to: {type(resolved).__name__} (classes have priority over enums)")
    
    print("\n2. Type 'AnotherType' exists only as enum:")
    context = TypeResolutionContext(
        local_enums=["AnotherType"]
    )
    
    unresolved = UnresolvedType(create_test_token("AnotherType"), "AnotherType")
    resolved = resolver.resolve_type(unresolved, context)
    
    print(f"   ✓ Resolved to: {type(resolved).__name__}")


def demo_context_variations():
    """Demonstrate different context scenarios."""
    print("\n=== Context Variations Demo ===")
    
    resolver = TypeResolver()
    
    # Test different context configurations
    contexts = [
        ("Empty context", TypeResolutionContext()),
        ("Only classes", TypeResolutionContext(local_classes=["MyClass"])),
        ("Only enums", TypeResolutionContext(local_enums=["MyEnum"])),
        ("Only records", TypeResolutionContext(local_records=["MyRecord"])),
        ("Mixed types", TypeResolutionContext(
            local_classes=["MyClass"],
            local_enums=["MyEnum"],
            local_records=["MyRecord"]
        ))
    ]
    
    for desc, context in contexts:
        print(f"\n{desc}:")
        
        # Try to resolve a primitive type (should always work)
        unresolved = UnresolvedType(create_test_token("String"), "String")
        try:
            resolved = resolver.resolve_type(unresolved, context)
            print(f"   ✓ 'String' resolved to: {type(resolved).__name__}")
        except TypeResolutionError:
            print(f"   ✗ 'String' failed to resolve")
        
        # Try to resolve a local type if available
        if context.local_classes:
            unresolved = UnresolvedType(create_test_token("MyClass"), "MyClass")
            try:
                resolved = resolver.resolve_type(unresolved, context)
                print(f"   ✓ 'MyClass' resolved to: {type(resolved).__name__}")
            except TypeResolutionError:
                print(f"   ✗ 'MyClass' failed to resolve")


def main():
    """Run all demonstrations."""
    print("TypeResolver Service Demonstration")
    print("=" * 50)
    
    try:
        demo_basic_type_resolution()
        demo_error_handling_and_suggestions()
        demo_caching_behavior()
        demo_priority_resolution()
        demo_context_variations()
        
        print("\n" + "=" * 50)
        print("✓ All demonstrations completed successfully!")
        
    except Exception as e:
        print(f"\n✗ Demonstration failed with error: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    main()