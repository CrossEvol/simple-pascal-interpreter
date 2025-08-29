"""Integration tests for Simple Pascal Interpreter"""

import os
import sys
import argparse
from pathlib import Path
from unittest.mock import patch

# Add src to path so we can import the modules
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'src'))

from src.lexer import Lexer
from src.parser import Parser
from src.sematic_analyzer import SemanticAnalyzer
from src.interpreter import Interpreter
from src.error import LexerError, ParserError, SemanticError


def test_pascal_file(file_path):
    """Test a single Pascal file"""
    print(f"Testing {file_path}...")
    
    try:
        # Read the file
        with open(file_path, 'r') as f:
            text = f.read()
        
        # Lexical analysis
        lexer = Lexer(text)
        
        # Parsing
        parser = Parser(lexer)
        tree = parser.parse()
        
        # Semantic analysis
        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.visit(tree)
        
        # Interpretation
        interpreter = Interpreter(tree)
        
        # Handle input files that require user input
        if "input.pas" in str(file_path) or "io.pas" in str(file_path):
            # Mock input for files that require user input
            with patch('builtins.input', side_effect=get_mock_input(file_path)):
                interpreter.interpret()
        else:
            interpreter.interpret()
        
        print(f"  PASSED: {file_path}")
        return True
        
    except (LexerError, ParserError, SemanticError) as e:
        print(f"  FAILED: {file_path}")
        print(f"    Error: {e.message}")
        return False
    except Exception as e:
        print(f"  FAILED: {file_path}")
        print(f"    Unexpected error: {str(e)}")
        return False


def get_mock_input(file_path):
    """Get mock input values for specific files"""
    if "input.pas" in str(file_path):
        # Mock inputs for input.pas: Integer, Real, String
        return ["42", "3.14", "Hello World"]
    elif "io.pas" in str(file_path):
        # Mock inputs for io.pas: String, Integer
        return ["John Doe", "25"]
    else:
        # Default empty input
        return [""]


def find_pascal_files(root_dir):
    """Find all .pas files in the directory tree"""
    pas_files = []
    for path in Path(root_dir).rglob("*.pas"):
        pas_files.append(path)
    return pas_files


def main():
    """Main test function"""
    # Get the project root directory
    project_root = os.path.dirname(__file__)
    pas_dir = os.path.join(project_root, "pas")
    
    # Check if pas directory exists
    if not os.path.exists(pas_dir):
        print(f"Error: Directory {pas_dir} does not exist")
        return 1
    
    # Find all Pascal files
    pas_files = find_pascal_files(pas_dir)
    
    if not pas_files:
        print(f"No .pas files found in {pas_dir}")
        return 1
    
    print(f"Found {len(pas_files)} Pascal files to test\n")
    
    # Test each file
    passed = 0
    failed = 0
    
    for pas_file in pas_files:
        if test_pascal_file(pas_file):
            passed += 1
        else:
            failed += 1
        print()  # Empty line for readability
    
    # Print summary
    print("=" * 50)
    print("TEST SUMMARY")
    print("=" * 50)
    print(f"Total files tested: {len(pas_files)}")
    print(f"Passed: {passed}")
    print(f"Failed: {failed}")
    
    if failed == 0:
        print("\nAll tests PASSED!")
        return 0
    else:
        print(f"\n{failed} test(s) FAILED!")
        return 1


if __name__ == "__main__":
    sys.exit(main())