#!/usr/bin/env python3
"""
Demonstration script for enhanced error messages in type resolution failures.

This script shows the improved error messages that distinguish between syntax
and semantic errors, provide contextual information, and include helpful suggestions.
"""

from src.lexer import Lexer
from src.parser import Parser
from src.interpreter import Interpreter
from src.error import TypeResolutionError, ParserError


def demo_syntax_error():
    """Demonstrate syntax error with clear location information."""
    print("=" * 60)
    print("DEMO 1: Syntax Error with Location Information")
    print("=" * 60)
    
    code = """program SyntaxErrorDemo;
var
    x: [invalid syntax here];
begin
end."""
    
    print("Code:")
    print(code)
    print("\nResult:")
    
    try:
        lexer = Lexer(code)
        parser = Parser(lexer)
        ast = parser.parse()
        print("No error occurred (unexpected)")
    except Exception as e:
        print(f"Error Type: {type(e).__name__}")
        print(f"Message: {str(e)}")


def demo_type_resolution_error():
    """Demonstrate semantic error with comprehensive context."""
    print("\n" + "=" * 60)
    print("DEMO 2: Type Resolution Error with Context")
    print("=" * 60)
    
    code = """program TypeErrorDemo;
var
    x: UnknownType;
begin
end."""
    
    print("Code:")
    print(code)
    print("\nResult:")
    
    try:
        lexer = Lexer(code)
        parser = Parser(lexer)
        ast = parser.parse()
        interpreter = Interpreter(ast)
        interpreter.interpret()
        print("No error occurred (unexpected)")
    except TypeResolutionError as e:
        print(f"Error Type: {type(e).__name__}")
        print(f"Message:\n{str(e)}")


def demo_helpful_suggestions():
    """Demonstrate helpful suggestions for typos."""
    print("\n" + "=" * 60)
    print("DEMO 3: Helpful Suggestions for Typos")
    print("=" * 60)
    
    code = """program SuggestionDemo;
var
    x: Integr;
    y: Rea;
    z: Boolea;
begin
end."""
    
    print("Code:")
    print(code)
    print("\nResult:")
    
    try:
        lexer = Lexer(code)
        parser = Parser(lexer)
        ast = parser.parse()
        interpreter = Interpreter(ast)
        interpreter.interpret()
        print("No error occurred (unexpected)")
    except TypeResolutionError as e:
        print(f"Error Type: {type(e).__name__}")
        print(f"Message:\n{str(e)}")
        print("\nNote: Only the first error is shown, but similar suggestions")
        print("would be provided for 'Rea' -> 'Real' and 'Boolea' -> 'Boolean'")


def demo_context_with_local_types():
    """Demonstrate error context with local type definitions."""
    print("\n" + "=" * 60)
    print("DEMO 4: Context with Local Type Definitions")
    print("=" * 60)
    
    code = """program ContextDemo;
type
    Color = (Red, Green, Blue);
    Status = (Active, Inactive, Pending);
var
    x: MissingType;
begin
end."""
    
    print("Code:")
    print(code)
    print("\nResult:")
    
    try:
        lexer = Lexer(code)
        parser = Parser(lexer)
        ast = parser.parse()
        interpreter = Interpreter(ast)
        interpreter.interpret()
        print("No error occurred (unexpected)")
    except TypeResolutionError as e:
        print(f"Error Type: {type(e).__name__}")
        print(f"Message:\n{str(e)}")


def demo_line_and_column_info():
    """Demonstrate accurate line and column information."""
    print("\n" + "=" * 60)
    print("DEMO 5: Accurate Line and Column Information")
    print("=" * 60)
    
    code = """program LocationDemo;
var
    a: Integer;
    b: Real;
    c: Boolean;
    d: String;
    e: UnknownType;
begin
end."""
    
    print("Code:")
    for i, line in enumerate(code.split('\n'), 1):
        print(f"{i:2}: {line}")
    print("\nResult:")
    
    try:
        lexer = Lexer(code)
        parser = Parser(lexer)
        ast = parser.parse()
        interpreter = Interpreter(ast)
        interpreter.interpret()
        print("No error occurred (unexpected)")
    except TypeResolutionError as e:
        print(f"Error Type: {type(e).__name__}")
        print(f"Message:\n{str(e)}")
        print("\nNote: The error correctly identifies line 7 where 'UnknownType' is used")


if __name__ == "__main__":
    print("Enhanced Error Messages Demonstration")
    print("=====================================")
    print("This demo shows the improved error handling in the Pascal interpreter.")
    print("Key improvements:")
    print("- Clear distinction between syntax and semantic errors")
    print("- Comprehensive contextual information")
    print("- Accurate line and column reporting")
    print("- Helpful suggestions for similar type names")
    print("- Available types listing")
    
    demo_syntax_error()
    demo_type_resolution_error()
    demo_helpful_suggestions()
    demo_context_with_local_types()
    demo_line_and_column_info()
    
    print("\n" + "=" * 60)
    print("SUMMARY")
    print("=" * 60)
    print("The enhanced error messages provide:")
    print("✓ Clear error categorization (Syntax vs Semantic)")
    print("✓ Precise location information (line and column)")
    print("✓ Contextual information about available types")
    print("✓ Intelligent suggestions for typos")
    print("✓ Comprehensive error phase identification")
    print("✓ Available types listing for better debugging")