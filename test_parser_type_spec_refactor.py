#!/usr/bin/env python3

"""
Test parser type_spec method refactor.

This test verifies that the parser creates UnresolvedType nodes for ID tokens
and no longer performs module checking during parsing.
"""

import unittest
from src.lexer import Lexer
from src.parser import Parser
from src.spi_ast import UnresolvedType, PrimitiveType, StringType, ArrayType
from src.spi_token import TokenType


class TestParserTypeSpecRefactor(unittest.TestCase):
    """Test cases for the refactored parser type_spec method."""

    def makeParser(self, text: str) -> Parser:
        """Helper method to create a parser from text."""
        lexer = Lexer(text)
        parser = Parser(lexer)
        return parser

    def test_primitive_types_still_work(self):
        """Test that primitive types are still parsed correctly."""
        # Test INTEGER type
        parser = self.makeParser("program Test; var x: INTEGER; begin end.")
        program = parser.program()
        var_decl = program.block.declarations[0]
        self.assertIsInstance(var_decl.type_node, PrimitiveType)
        self.assertEqual(var_decl.type_node.token.type, TokenType.INTEGER)

        # Test REAL type
        parser = self.makeParser("program Test; var x: REAL; begin end.")
        program = parser.program()
        var_decl = program.block.declarations[0]
        self.assertIsInstance(var_decl.type_node, PrimitiveType)
        self.assertEqual(var_decl.type_node.token.type, TokenType.REAL)

        # Test BOOLEAN type
        parser = self.makeParser("program Test; var x: BOOLEAN; begin end.")
        program = parser.program()
        var_decl = program.block.declarations[0]
        self.assertIsInstance(var_decl.type_node, PrimitiveType)
        self.assertEqual(var_decl.type_node.token.type, TokenType.BOOLEAN)

    def test_string_type_still_works(self):
        """Test that STRING type is still parsed correctly."""
        parser = self.makeParser("program Test; var x: STRING; begin end.")
        program = parser.program()
        var_decl = program.block.declarations[0]
        self.assertIsInstance(var_decl.type_node, StringType)

    def test_array_type_still_works(self):
        """Test that ARRAY type is still parsed correctly."""
        parser = self.makeParser("program Test; var x: ARRAY[1..10] OF INTEGER; begin end.")
        program = parser.program()
        var_decl = program.block.declarations[0]
        self.assertIsInstance(var_decl.type_node, ArrayType)

    def test_unknown_type_creates_unresolved_type(self):
        """Test that unknown type IDs create UnresolvedType nodes."""
        parser = self.makeParser("program Test; var x: MyCustomType; begin end.")
        program = parser.program()
        var_decl = program.block.declarations[0]
        
        # Should create UnresolvedType instead of raising an error
        self.assertIsInstance(var_decl.type_node, UnresolvedType)
        self.assertEqual(var_decl.type_node.type_name, "MyCustomType")
        self.assertIsNone(var_decl.type_node.resolved_type)

    def test_imported_module_type_creates_unresolved_type(self):
        """Test that types from imported modules create UnresolvedType nodes."""
        parser = self.makeParser("""
            program Test;
            uses SomeModule;
            var x: SomeModuleType;
            begin 
            end.
        """)
        program = parser.program()
        var_decl = program.block.declarations[0]
        
        # Should create UnresolvedType instead of trying to resolve from modules
        self.assertIsInstance(var_decl.type_node, UnresolvedType)
        self.assertEqual(var_decl.type_node.type_name, "SomeModuleType")

    def test_multiple_unknown_types(self):
        """Test parsing multiple unknown types in the same program."""
        parser = self.makeParser("""
            program Test;
            var 
                x: TypeA;
                y: TypeB;
                z: TypeC;
            begin 
            end.
        """)
        program = parser.program()
        
        # All should be UnresolvedType
        for i, expected_type in enumerate(["TypeA", "TypeB", "TypeC"]):
            var_decl = program.block.declarations[i]
            self.assertIsInstance(var_decl.type_node, UnresolvedType)
            self.assertEqual(var_decl.type_node.type_name, expected_type)

    def test_array_with_unknown_element_type(self):
        """Test that arrays with unknown element types work correctly."""
        parser = self.makeParser("program Test; var x: ARRAY[1..10] OF MyType; begin end.")
        program = parser.program()
        var_decl = program.block.declarations[0]
        
        # Should be ArrayType with UnresolvedType element
        self.assertIsInstance(var_decl.type_node, ArrayType)
        self.assertIsInstance(var_decl.type_node.element_type, UnresolvedType)
        self.assertEqual(var_decl.type_node.element_type.type_name, "MyType")

    def test_parser_no_longer_requires_module_registry(self):
        """Test that parser can be created without module_registry parameter."""
        lexer = Lexer("program Test; begin end.")
        
        # Should work without module_registry
        parser = Parser(lexer)
        self.assertIsNotNone(parser)
        
        # Should be able to parse successfully
        program = parser.program()
        self.assertIsNotNone(program)

    def test_uses_clause_still_parsed_but_no_loading(self):
        """Test that uses clause is parsed but modules are not loaded."""
        parser = self.makeParser("""
            program Test;
            uses Module1, Module2;
            begin 
            end.
        """)
        program = parser.program()
        
        # Uses clause should be parsed
        self.assertEqual(program.uses_clause, ["Module1", "Module2"])
        
        # Parser should track imported modules
        self.assertEqual(parser.imported_modules, ["Module1", "Module2"])

    def test_local_types_still_work_in_type_declarations(self):
        """Test that locally declared types still work correctly."""
        parser = self.makeParser("""
            program Test;
            type
                MyRecord = record
                    id: integer;
                end;
            var
                x: MyRecord;
            begin 
            end.
        """)
        program = parser.program()
        
        # The type declaration should work
        type_decl = program.block.declarations[0]
        self.assertEqual(type_decl.record_name, "MyRecord")
        
        # The variable using the local type should create UnresolvedType
        # (since we simplified type_spec to always create UnresolvedType for IDs)
        var_decl = program.block.declarations[1]
        self.assertIsInstance(var_decl.type_node, UnresolvedType)
        self.assertEqual(var_decl.type_node.type_name, "MyRecord")

    def test_syntax_errors_still_reported(self):
        """Test that syntax errors in type specifications are still reported."""
        from src.error import ParserError, ErrorCode
        
        # Invalid type syntax should still raise syntax error
        parser = self.makeParser("program Test; var x: ; begin end.")
        
        with self.assertRaises(ParserError) as cm:
            parser.program()
        
        # Should be a syntax error, not a type resolution error
        self.assertEqual(cm.exception.error_code, ErrorCode.UNEXPECTED_TOKEN)


if __name__ == "__main__":
    unittest.main()