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
        """Test that parser can resolve types from imported modules."""

        # Create a test module with a custom type
        test_module_content = """
unit TestTypes;

interface
  type
    TCustomRecord = record
      id: integer;
      name: string;
    end;
    
    TCustomClass = class
    private
      value: integer;
    public
      procedure SetValue(v: integer);
    end;

implementation
  procedure TCustomClass.SetValue(v: integer);
  begin
    value := v;
  end;
end.
"""

        # Write the test module to a file
        module_file_path = os.path.join(self.temp_dir, "TestTypes.pas")
        with open(module_file_path, "w") as f:
            f.write(test_module_content)

        # Load the module into the registry
        self.module_registry.load_module("TestTypes", module_file_path)

        # Create a program that uses the module
        program_content = """
program TestProgram;
uses TestTypes;
var
  myRecord: TCustomRecord;
  myClass: TCustomClass;
begin
  myRecord.id := 1;
  myRecord.name := 'test';
end.
"""

        # Parse the program with module registry
        lexer = Lexer(program_content)
        parser = Parser(lexer, self.module_registry)

        # This should not raise an UnknownTypeError
        try:
            tree = parser.parse()
            self.assertIsNotNone(tree)
            # Verify that the uses clause was parsed
            self.assertEqual(tree.uses_clause, ["TestTypes"])
        except Exception as e:
            self.fail(
                f"Parser should be able to resolve types from imported modules, but got: {e}"
            )


if __name__ == "__main__":
    unittest.main()
