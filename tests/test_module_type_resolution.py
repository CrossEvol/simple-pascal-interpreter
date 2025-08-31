#!/usr/bin/env python3

"""
Test module type resolution in parser.

This test verifies that the parser can resolve types from imported modules.
"""

import unittest
import tempfile
import os
from src.lexer import Lexer
from src.parser import Parser
from src.module import ModuleRegistry


class TestModuleTypeResolution(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures."""
        self.temp_dir = tempfile.mkdtemp()
        self.module_registry = ModuleRegistry()
        # Add temp directory to search paths
        self.module_registry.search_paths.insert(0, self.temp_dir)

    def tearDown(self):
        """Clean up test fixtures."""
        # Clean up temp files
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_type_resolution_from_imported_module(self):
        """Test that parser creates UnresolvedType nodes for imported module types."""

        # Create a program that uses a module
        program_content = """
program TestProgram;
uses TestTypes;
var
  myRecord: TCustomRecord;
  myClass: TCustomClass;
begin
end.
"""

        # Parse the program without module registry (new behavior)
        lexer = Lexer(program_content)
        parser = Parser(lexer)

        # This should not raise an error and should create UnresolvedType nodes
        try:
            tree = parser.parse()
            self.assertIsNotNone(tree)

            # Verify that the uses clause was parsed
            self.assertEqual(tree.uses_clause, ["TestTypes"])

            # Verify that type references create UnresolvedType nodes
            from src.spi_ast import UnresolvedType

            # Check the variable declarations
            var_decls = [
                decl for decl in tree.block.declarations if hasattr(decl, "type_node")
            ]
            self.assertEqual(len(var_decls), 2)

            # First variable should have UnresolvedType for TCustomRecord
            self.assertIsInstance(var_decls[0].type_node, UnresolvedType)
            self.assertEqual(var_decls[0].type_node.type_name, "TCustomRecord")

            # Second variable should have UnresolvedType for TCustomClass
            self.assertIsInstance(var_decls[1].type_node, UnresolvedType)
            self.assertEqual(var_decls[1].type_node.type_name, "TCustomClass")

        except Exception as e:
            self.fail(
                f"Parser should create UnresolvedType nodes for imported module types, but got: {e}"
            )


if __name__ == "__main__":
    unittest.main()
