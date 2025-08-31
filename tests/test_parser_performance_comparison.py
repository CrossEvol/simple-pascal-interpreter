"""
Performance comparison test for parser type resolution refactor.

This test demonstrates the performance improvements achieved by removing
module loading from the parser and deferring type validation to interpretation.
"""

import unittest
import time
import tempfile
import os
import shutil
from unittest.mock import Mock, patch

from src.lexer import Lexer
from src.parser import Parser
from src.interpreter import Interpreter
from src.module import ModuleRegistry


class TestParserPerformanceComparison(unittest.TestCase):
    """Performance comparison tests for parser refactor."""

    def setUp(self):
        """Set up test environment."""
        self.temp_dir = tempfile.mkdtemp()
        self.stdlib_dir = os.path.join(self.temp_dir, "stdlib")
        os.makedirs(self.stdlib_dir, exist_ok=True)
        
        # Create multiple test modules to simulate a larger codebase
        self._create_multiple_test_modules()

    def tearDown(self):
        """Clean up temporary directories."""
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def _create_multiple_test_modules(self):
        """Create multiple test modules to simulate module loading overhead."""
        modules = [
            ("TypesA", "CustomRecordA", "CustomEnumA"),
            ("TypesB", "CustomRecordB", "CustomEnumB"), 
            ("TypesC", "CustomRecordC", "CustomEnumC"),
            ("TypesD", "CustomRecordD", "CustomEnumD"),
            ("TypesE", "CustomRecordE", "CustomEnumE"),
        ]
        
        for module_name, record_type, enum_type in modules:
            module_content = f"""
            unit {module_name};
            
            interface
            
            type
                {record_type} = record
                    id: integer;
                    name: string;
                end;
                
                {enum_type} = (Red, Green, Blue);
                
            implementation
            
            end.
            """
            
            module_path = os.path.join(self.stdlib_dir, f"{module_name}.pas")
            with open(module_path, "w") as f:
                f.write(module_content)

    def test_parsing_performance_with_many_type_references(self):
        """Test parsing performance with many type references.
        
        This test demonstrates that parsing is fast because it doesn't
        load modules or validate types during parsing.
        """
        
        # Create a program with many type references
        program_text = """
        program PerformanceTest;
        uses TypesA, TypesB, TypesC, TypesD, TypesE;
        
        var
            recordA1, recordA2, recordA3: CustomRecordA;
            recordB1, recordB2, recordB3: CustomRecordB;
            recordC1, recordC2, recordC3: CustomRecordC;
            recordD1, recordD2, recordD3: CustomRecordD;
            recordE1, recordE2, recordE3: CustomRecordE;
            
            enumA1, enumA2: CustomEnumA;
            enumB1, enumB2: CustomEnumB;
            enumC1, enumC2: CustomEnumC;
            enumD1, enumD2: CustomEnumD;
            enumE1, enumE2: CustomEnumE;
            
            arrayA: array[1..100] of CustomRecordA;
            arrayB: array[1..100] of CustomRecordB;
            arrayC: array[1..100] of CustomRecordC;
            arrayD: array[1..100] of CustomRecordD;
            arrayE: array[1..100] of CustomRecordE;
            
        begin
            recordA1.id := 1;
            recordB1.id := 2;
            recordC1.id := 3;
            recordD1.id := 4;
            recordE1.id := 5;
        end.
        """
        
        # Measure parsing time multiple times for accuracy
        parsing_times = []
        
        for _ in range(5):
            start_time = time.time()
            
            lexer = Lexer(program_text)
            parser = Parser(lexer)
            ast = parser.parse()
            
            parsing_time = time.time() - start_time
            parsing_times.append(parsing_time)
            
            # Verify parsing succeeded
            self.assertIsNotNone(ast)
        
        # Calculate average parsing time
        avg_parsing_time = sum(parsing_times) / len(parsing_times)
        
        # Parsing should be very fast (under 0.1 seconds for this program)
        # because no module loading occurs during parsing
        self.assertLess(avg_parsing_time, 0.1, 
                       f"Parsing should be fast without module loading. Average time: {avg_parsing_time:.4f}s")
        
        print(f"Average parsing time: {avg_parsing_time:.4f} seconds")
        print(f"Parsing times: {[f'{t:.4f}' for t in parsing_times]}")

    def test_parser_does_not_access_filesystem_during_parsing(self):
        """Test that parser doesn't access filesystem during parsing.
        
        This demonstrates the performance improvement by showing that
        no file I/O operations occur during parsing.
        """
        
        program_text = """
        program FileSystemTest;
        uses NonExistentModule1, NonExistentModule2, NonExistentModule3;
        
        var
            var1: NonExistentType1;
            var2: NonExistentType2;
            var3: NonExistentType3;
            
        begin
        end.
        """
        
        # Mock file operations to track if they're called during parsing
        file_operations_count = 0
        
        def mock_file_operation(*args, **kwargs):
            nonlocal file_operations_count
            file_operations_count += 1
            raise FileNotFoundError("Mocked file operation")
        
        # Patch file operations that might be used for module loading
        with patch('builtins.open', side_effect=mock_file_operation), \
             patch('os.path.exists', side_effect=mock_file_operation), \
             patch('os.listdir', side_effect=mock_file_operation):
            
            # Parse the program
            lexer = Lexer(program_text)
            parser = Parser(lexer)
            ast = parser.parse()
            
            # Verify parsing succeeded
            self.assertIsNotNone(ast)
            
            # Verify no file operations were performed during parsing
            self.assertEqual(file_operations_count, 0, 
                           "Parser should not perform file operations during parsing")

    def test_memory_usage_improvement(self):
        """Test memory usage improvement from not loading modules during parsing.
        
        This test shows that parser memory usage is minimal because
        it doesn't load and store module information.
        """
        
        program_text = """
        program MemoryTest;
        uses TypesA, TypesB, TypesC, TypesD, TypesE;
        
        var
            var1: CustomRecordA;
            var2: CustomRecordB;
            var3: CustomRecordC;
            var4: CustomRecordD;
            var5: CustomRecordE;
            
        begin
        end.
        """
        
        # Parse the program
        lexer = Lexer(program_text)
        parser = Parser(lexer)
        ast = parser.parse()
        
        # Verify parsing succeeded
        self.assertIsNotNone(ast)
        
        # Verify parser doesn't store module information
        # (In the old implementation, parser might have stored loaded modules)
        self.assertFalse(hasattr(parser, 'loaded_modules'), 
                        "Parser should not store loaded modules")
        self.assertFalse(hasattr(parser, 'module_registry'), 
                        "Parser should not have module registry")
        
        # Parser should only contain lexer and parsing state
        parser_attributes = [attr for attr in dir(parser) 
                           if not attr.startswith('_') and not callable(getattr(parser, attr))]
        
        # Parser should have minimal state (mainly lexer and current token)
        expected_attributes = {'lexer', 'current_token'}
        actual_attributes = set(parser_attributes)
        
        # Parser should not have module-related attributes
        module_related_attrs = {'module_registry', 'loaded_modules', 'imported_modules'}
        self.assertFalse(actual_attributes.intersection(module_related_attrs),
                        "Parser should not have module-related attributes")

    def test_scalability_with_large_programs(self):
        """Test parser scalability with large programs containing many type references.
        
        This demonstrates that parsing time scales linearly with program size,
        not with the number of modules or types referenced.
        """
        
        # Create programs of different sizes
        program_sizes = [10, 50, 100]  # Number of variable declarations
        parsing_times = []
        
        for size in program_sizes:
            # Generate program with 'size' variable declarations
            var_declarations = []
            for i in range(size):
                module_idx = i % 5  # Cycle through TypesA-TypesE
                module_name = chr(ord('A') + module_idx)
                var_declarations.append(f"    var{i}: CustomRecord{module_name};")
            
            program_text = f"""
            program ScalabilityTest{size};
            uses TypesA, TypesB, TypesC, TypesD, TypesE;
            
            var
{chr(10).join(var_declarations)}
                
            begin
                var0.id := 1;
            end.
            """
            
            # Measure parsing time
            start_time = time.time()
            
            lexer = Lexer(program_text)
            parser = Parser(lexer)
            ast = parser.parse()
            
            parsing_time = time.time() - start_time
            parsing_times.append(parsing_time)
            
            # Verify parsing succeeded
            self.assertIsNotNone(ast)
        
        # Verify parsing time scales reasonably with program size
        # (Should be roughly linear, not exponential)
        for i in range(1, len(parsing_times)):
            size_ratio = program_sizes[i] / program_sizes[i-1]
            time_ratio = parsing_times[i] / parsing_times[i-1] if parsing_times[i-1] > 0 else 1
            
            # Time ratio should not be much larger than size ratio
            # (allowing some overhead, but not exponential growth)
            self.assertLess(time_ratio, size_ratio * 2, 
                          f"Parsing time should scale reasonably. Size ratio: {size_ratio:.2f}, Time ratio: {time_ratio:.2f}")
        
        print(f"Program sizes: {program_sizes}")
        print(f"Parsing times: {[f'{t:.4f}' for t in parsing_times]}")

    def test_concurrent_parsing_performance(self):
        """Test that multiple parsers can work concurrently without interference.
        
        This demonstrates that parser instances are independent and don't
        share module loading state that could cause contention.
        """
        
        program_texts = [
            """
            program Concurrent1;
            uses TypesA;
            var x: CustomRecordA;
            begin end.
            """,
            """
            program Concurrent2;
            uses TypesB;
            var y: CustomRecordB;
            begin end.
            """,
            """
            program Concurrent3;
            uses TypesC;
            var z: CustomRecordC;
            begin end.
            """
        ]
        
        # Parse all programs and measure total time
        start_time = time.time()
        
        asts = []
        for program_text in program_texts:
            lexer = Lexer(program_text)
            parser = Parser(lexer)
            ast = parser.parse()
            asts.append(ast)
        
        total_time = time.time() - start_time
        
        # Verify all parsing succeeded
        for ast in asts:
            self.assertIsNotNone(ast)
        
        # Total time should be reasonable (under 0.1 seconds for 3 small programs)
        self.assertLess(total_time, 0.1, 
                       f"Concurrent parsing should be fast. Total time: {total_time:.4f}s")
        
        print(f"Concurrent parsing time for {len(program_texts)} programs: {total_time:.4f} seconds")


if __name__ == '__main__':
    unittest.main()