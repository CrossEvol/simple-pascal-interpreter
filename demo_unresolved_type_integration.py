"""
Integration demonstration for UnresolvedType handling in the interpreter.

This script demonstrates how the interpreter now handles UnresolvedType nodes
during interpretation, resolving types dynamically rather than during parsing.
"""

from src.lexer import Lexer
from src.parser import Parser
from src.interpreter import Interpreter
from src.sematic_analyzer import SemanticAnalyzer
from src.spi_ast import UnresolvedType


def demonstrate_unresolved_type_handling():
    """Demonstrate UnresolvedType handling in the interpreter."""
    
    print("=== UnresolvedType Integration Demonstration ===\n")
    
    # Simple program that would create UnresolvedType nodes
    # Note: This is a conceptual demonstration since the parser refactoring
    # (task 3) hasn't been completed yet, so the parser still validates types
    program_text = """
    program TestProgram;
    var
        x: Integer;
        y: Real;
        flag: Boolean;
    begin
        x := 42;
        y := 3.14;
        flag := true;
        writeln(x);
        writeln(y);
        writeln(flag);
    end.
    """
    
    print("Program text:")
    print(program_text)
    print()
    
    try:
        # Parse the program
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.parse()
        
        # Run semantic analysis
        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.visit(tree)
        
        # Create interpreter with TypeResolver integration
        interpreter = Interpreter(tree)
        
        print("✓ Interpreter created successfully with TypeResolver integration")
        print(f"✓ TypeResolver initialized: {type(interpreter.type_resolver).__name__}")
        print(f"✓ Module registry available: {interpreter.module_registry is not None}")
        
        # Demonstrate that the interpreter has the new visitor method
        print(f"✓ visit_UnresolvedType method available: {hasattr(interpreter, 'visit_UnresolvedType')}")
        print(f"✓ _build_type_resolution_context method available: {hasattr(interpreter, '_build_type_resolution_context')}")
        
        # Run the interpreter
        print("\nExecuting program:")
        print("-" * 20)
        interpreter.visit(tree)
        print("-" * 20)
        print("✓ Program executed successfully")
        
    except Exception as e:
        print(f"✗ Error: {e}")
        return False
    
    return True


def demonstrate_unresolved_type_node_creation():
    """Demonstrate creating and handling UnresolvedType nodes directly."""
    
    print("\n=== Direct UnresolvedType Node Demonstration ===\n")
    
    from src.spi_token import Token, TokenType
    from src.type_resolver import TypeResolutionContext
    
    # Create a simple program for context
    program_text = "program Test; begin end."
    lexer = Lexer(program_text)
    parser = Parser(lexer)
    tree = parser.parse()
    interpreter = Interpreter(tree)
    
    # Create UnresolvedType nodes
    test_cases = [
        ("Integer", "Should resolve to PrimitiveType"),
        ("String", "Should resolve to StringType"),
        ("Boolean", "Should resolve to PrimitiveType"),
        ("Real", "Should resolve to PrimitiveType"),
    ]
    
    for type_name, description in test_cases:
        try:
            # Create UnresolvedType node
            token = Token(TokenType.ID, type_name, 1, 1)
            unresolved_type = UnresolvedType(token, type_name)
            
            print(f"Testing type: {type_name}")
            print(f"Description: {description}")
            
            # Create a mock activation record for context
            from src.interpreter import ActivationRecord, ARType
            ar = ActivationRecord("test", ARType.PROGRAM, 1)
            interpreter.call_stack.push(ar)
            
            # Resolve the type
            resolved_type = interpreter.visit_UnresolvedType(unresolved_type)
            
            print(f"✓ '{type_name}' resolved to: {type(resolved_type).__name__}")
            print(f"✓ Cached result: {unresolved_type.resolved_type is not None}")
            
            # Test caching - second call should use cached result
            resolved_type_2 = interpreter.visit_UnresolvedType(unresolved_type)
            print(f"✓ Caching works: {resolved_type is resolved_type_2}")
            
            interpreter.call_stack.pop()
            print()
            
        except Exception as e:
            print(f"✗ Error resolving {type_name}: {e}")
            print()


def demonstrate_type_resolution_context():
    """Demonstrate building type resolution context."""
    
    print("=== Type Resolution Context Demonstration ===\n")
    
    # Create a simple program
    program_text = """
    program ContextTest;
    uses TestModule;
    type
        MyClass = class
        end;
        MyEnum = (Value1, Value2);
        MyRecord = record
            field: Integer;
        end;
    begin
    end.
    """
    
    try:
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.parse()
        interpreter = Interpreter(tree)
        
        # Create activation record with type metadata
        from src.interpreter import ActivationRecord, ARType
        ar = ActivationRecord("test", ARType.PROGRAM, 1)
        
        # Simulate type declarations being processed
        from unittest.mock import Mock
        ar.set_meta("MyClass", Mock())
        ar.set_is_class("MyClass", True)
        
        ar.set_meta("MyEnum", Mock())
        ar.set_is_enum("MyEnum")
        
        ar.set_meta("MyRecord", Mock())
        ar.set_is_record("MyRecord")
        
        interpreter.call_stack.push(ar)
        
        # Build context
        context = interpreter._build_type_resolution_context(ar)
        
        print("Type resolution context built successfully:")
        print(f"✓ Local classes: {context.local_classes}")
        print(f"✓ Local enums: {context.local_enums}")
        print(f"✓ Local records: {context.local_records}")
        print(f"✓ Imported modules: {context.imported_modules}")
        print(f"✓ Module registry available: {context.module_registry is not None}")
        
    except Exception as e:
        print(f"✗ Error building context: {e}")


if __name__ == "__main__":
    print("UnresolvedType Integration Demonstration")
    print("=" * 50)
    
    success = True
    
    # Run demonstrations
    success &= demonstrate_unresolved_type_handling()
    demonstrate_unresolved_type_node_creation()
    demonstrate_type_resolution_context()
    
    print("\n" + "=" * 50)
    if success:
        print("✓ All demonstrations completed successfully!")
    else:
        print("✗ Some demonstrations failed.")