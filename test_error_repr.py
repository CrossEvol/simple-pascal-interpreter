"""Test script to verify __str__ and __repr__ implementations for Error classes."""

from src.spi.error import Error, LexerError, ParserError, SemanticError, InterpreterError, ErrorCode

def test_error_repr():
    print("Testing Error class implementations...")
    
    # Test base Error class
    error = Error(error_code=ErrorCode.UNEXPECTED_TOKEN, message="Test message")
    print(f"Error str: {str(error)}")
    print(f"Error repr: {repr(error)}")
    print()
    
    # Test LexerError
    lexer_error = LexerError(error_code=ErrorCode.LEXER_INVALID_CHARACTER, message="Invalid character '!'")
    print(f"LexerError str: {str(lexer_error)}")
    print(f"LexerError repr: {repr(lexer_error)}")
    print()
    
    # Test ParserError
    parser_error = ParserError(error_code=ErrorCode.PARSER_UNEXPECTED_TOKEN, message="Expected identifier")
    print(f"ParserError str: {str(parser_error)}")
    print(f"ParserError repr: {repr(parser_error)}")
    print()
    
    # Test SemanticError
    semantic_error = SemanticError(error_code=ErrorCode.SEMANTIC_ERROR, message="Variable not declared")
    print(f"SemanticError str: {str(semantic_error)}")
    print(f"SemanticError repr: {repr(semantic_error)}")
    print()
    
    # Test InterpreterError
    interpreter_error = InterpreterError(error_code=ErrorCode.INTERPRETER_UNKNOWN_FUNCTION, message="Function 'foo' not found")
    print(f"InterpreterError str: {str(interpreter_error)}")
    print(f"InterpreterError repr: {repr(interpreter_error)}")
    print()
    
    # Test with None values
    empty_error = Error()
    print(f"Empty Error str: {str(empty_error)}")
    print(f"Empty Error repr: {repr(empty_error)}")

if __name__ == "__main__":
    test_error_repr()