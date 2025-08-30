"""
Real-world end-to-end integration tests for the Pascal module system.

This test suite demonstrates realistic usage scenarios that a developer
would encounter when using the module system in practice.

Requirements covered: 2.4, 3.4, 5.1
"""

import unittest
import tempfile
import os
import shutil
import time
from src.lexer import Lexer
from src.parser import Parser
from src.interpreter import Interpreter


class TestRealWorldModuleScenarios(unittest.TestCase):
    """Real-world usage scenarios for the module system."""

    def setUp(self):
        """Set up test environment."""
        self.temp_dir = tempfile.mkdtemp()

    def tearDown(self):
        """Clean up temporary directories."""
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def create_unit_file(self, name: str, content: str) -> str:
        """Create a unit file for testing."""
        file_path = os.path.join(self.temp_dir, f"{name}.pas")
        with open(file_path, "w") as f:
            f.write(content)
        return file_path

    def test_calculator_application_with_math_library(self):
        """Test a calculator application that uses the Math standard library."""
        program_text = """
        program Calculator;
        var
          num1, num2, result: integer;
          operation: integer;
        begin
          { Simple calculator simulation }
          num1 := 10;
          num2 := 5;
          operation := 1; { 1=add, 2=sub, 3=mul, 4=div }
          
          if operation = 1 then
            result := num1 + num2
          else if operation = 2 then
            result := num1 - num2
          else if operation = 3 then
            result := num1 * num2
          else if operation = 4 then
            result := num1 div num2;
        end.
        """

        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Use Math library
        tree.uses_clause = ["Math"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, "./stdlib"]

        # Execute calculator program
        interpreter.visit_Program(tree)

        # Verify Math library was loaded
        self.assertIn("Math", interpreter.module_registry.loaded_modules)
        math_module = interpreter.module_registry.get_module("Math")
        self.assertIsNotNone(math_module)
        self.assertTrue(math_module.is_loaded)

        print("✓ Calculator application successfully used Math library")

    def test_data_processing_with_array_utils(self):
        """Test a data processing application using ArrayUtils."""
        program_text = """
        program DataProcessor;
        var
          data: array[0..9] of integer;
          i, size: integer;
        begin
          { Initialize data array }
          data[0] := 64;
          data[1] := 34;
          data[2] := 25;
          data[3] := 12;
          data[4] := 22;
          data[5] := 11;
          data[6] := 90;
          data[7] := 88;
          data[8] := 76;
          data[9] := 50;
          size := 10;
          
          { Process the data }
          i := 0;
          while i < size do
          begin
            if data[i] > 50 then
              data[i] := data[i] - 10;
            i := i + 1;
          end;
        end.
        """

        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Use ArrayUtils library
        tree.uses_clause = ["ArrayUtils"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, "./stdlib"]

        # Execute data processing program
        interpreter.visit_Program(tree)

        # Verify ArrayUtils library was loaded
        self.assertIn("ArrayUtils", interpreter.module_registry.loaded_modules)
        array_utils_module = interpreter.module_registry.get_module("ArrayUtils")
        self.assertIsNotNone(array_utils_module)
        self.assertTrue(array_utils_module.is_loaded)

        print("✓ Data processing application successfully used ArrayUtils library")

    def test_key_value_store_with_map_library(self):
        """Test a key-value store application using Map library."""
        program_text = """
        program KeyValueStore;
        var
          store_size: integer;
          key_count: integer;
        begin
          { Simulate key-value store operations }
          store_size := 0;
          key_count := 0;
          
          { Add some entries }
          store_size := store_size + 1;
          key_count := key_count + 1;
          
          { Simulate lookup }
          if key_count > 0 then
            store_size := store_size - 1;
        end.
        """

        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Use Map library
        tree.uses_clause = ["Map"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, "./stdlib"]

        # Execute key-value store program
        interpreter.visit_Program(tree)

        # Verify Map library was loaded
        self.assertIn("Map", interpreter.module_registry.loaded_modules)
        map_module = interpreter.module_registry.get_module("Map")
        self.assertIsNotNone(map_module)
        self.assertTrue(map_module.is_loaded)

        print("✓ Key-value store application successfully used Map library")

    def test_comprehensive_application_using_all_libraries(self):
        """Test a comprehensive application using all standard libraries."""
        program_text = """
        program ComprehensiveApp;
        var
          numbers: array[0..4] of integer;
          total, average: integer;
          i, size: integer;
          processing_complete: boolean;
        begin
          { Initialize data }
          numbers[0] := 15;
          numbers[1] := 8;
          numbers[2] := 23;
          numbers[3] := 4;
          numbers[4] := 16;
          size := 5;
          
          { Calculate total }
          total := 0;
          for i := 0 to size - 1 do
          begin
            total := total + numbers[i];
          end;
          
          { Calculate average }
          if size > 0 then
            average := total div size
          else
            average := 0;
          
          processing_complete := true;
        end.
        """

        lexer = Lexer(program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Use all standard libraries
        tree.uses_clause = ["Math", "ArrayUtils", "Map", "TestModule"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, "./stdlib"]

        # Execute comprehensive application
        interpreter.visit_Program(tree)

        # Verify all libraries were loaded
        expected_libraries = ["Math", "ArrayUtils", "Map", "TestModule"]
        for lib_name in expected_libraries:
            self.assertIn(lib_name, interpreter.module_registry.loaded_modules)
            lib_module = interpreter.module_registry.get_module(lib_name)
            self.assertIsNotNone(lib_module)
            self.assertTrue(lib_module.is_loaded)

        print("✓ Comprehensive application successfully used all standard libraries")

    def test_modular_game_engine_simulation(self):
        """Test a modular game engine simulation with custom modules."""
        # Create game engine modules
        graphics_module_content = """
        program Graphics;
        var
          screen_width, screen_height: integer;
        begin
          screen_width := 800;
          screen_height := 600;
        end.
        """

        physics_module_content = """
        program Physics;
        var
          gravity: integer;
          time_step: integer;
        begin
          gravity := 9;
          time_step := 16; { 60 FPS = ~16ms per frame }
        end.
        """

        audio_module_content = """
        program Audio;
        var
          volume: integer;
          sound_enabled: boolean;
        begin
          volume := 75;
          sound_enabled := true;
        end.
        """

        input_module_content = """
        program Input;
        var
          mouse_x, mouse_y: integer;
          key_pressed: boolean;
        begin
          mouse_x := 0;
          mouse_y := 0;
          key_pressed := false;
        end.
        """

        # Create the module files
        self.create_unit_file("Graphics", graphics_module_content)
        self.create_unit_file("Physics", physics_module_content)
        self.create_unit_file("Audio", audio_module_content)
        self.create_unit_file("Input", input_module_content)

        # Create main game program
        game_program_text = """
        program GameEngine;
        var
          game_running: boolean;
          frame_count: integer;
          player_x, player_y: integer;
        begin
          game_running := true;
          frame_count := 0;
          player_x := 100;
          player_y := 100;
          
          player_y := player_y + 1;
          
          if player_x > 800 then
            player_x := 0;
          
          player_x := player_x + 5;
          frame_count := frame_count + 1;
          
          game_running := false;
        end.
        """

        lexer = Lexer(game_program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Use all game engine modules plus standard libraries
        tree.uses_clause = ["Graphics", "Physics", "Audio", "Input", "Math", "ArrayUtils"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, "./stdlib"]

        # Execute game engine
        interpreter.visit_Program(tree)

        # Verify all modules were loaded
        expected_modules = ["Graphics", "Physics", "Audio", "Input", "Math", "ArrayUtils"]
        for module_name in expected_modules:
            self.assertIn(module_name, interpreter.module_registry.loaded_modules)
            module = interpreter.module_registry.get_module(module_name)
            self.assertIsNotNone(module)
            self.assertTrue(module.is_loaded)

        print("✓ Modular game engine successfully loaded all modules")

    def test_scientific_computing_application(self):
        """Test a scientific computing application using multiple libraries."""
        # Create scientific computing modules
        statistics_module_content = """
        program Statistics;
        var
          sample_count: integer;
          mean, variance: integer;
        begin
          sample_count := 100;
          mean := 0;
          variance := 0;
        end.
        """

        matrix_module_content = """
        program Matrix;
        var
          rows, cols: integer;
          determinant: integer;
        begin
          rows := 3;
          cols := 3;
          determinant := 1;
        end.
        """

        self.create_unit_file("Statistics", statistics_module_content)
        self.create_unit_file("Matrix", matrix_module_content)

        # Create scientific application
        science_program_text = """
        program ScientificComputing;
        var
          data: array[0..9] of integer;
          results: array[0..2] of integer;
          i, sum, count: integer;
        begin
          { Initialize experimental data }
          data[0] := 12;
          data[1] := 15;
          data[2] := 18;
          data[3] := 14;
          data[4] := 16;
          data[5] := 13;
          data[6] := 17;
          data[7] := 19;
          data[8] := 11;
          data[9] := 20;
          
          { Calculate statistics }
          sum := 0;
          count := 10;
          for i := 0 to 9 do
          begin
            sum := sum + data[i];
          end;
          
          { Store results }
          results[0] := sum;
          results[1] := sum div count; { mean }
          results[2] := count;
        end.
        """

        lexer = Lexer(science_program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Use scientific modules plus standard libraries
        tree.uses_clause = ["Statistics", "Matrix", "Math", "ArrayUtils"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, "./stdlib"]

        # Execute scientific application
        interpreter.visit_Program(tree)

        # Verify all modules were loaded
        expected_modules = ["Statistics", "Matrix", "Math", "ArrayUtils"]
        for module_name in expected_modules:
            self.assertIn(module_name, interpreter.module_registry.loaded_modules)
            module = interpreter.module_registry.get_module(module_name)
            self.assertIsNotNone(module)
            self.assertTrue(module.is_loaded)

        print("✓ Scientific computing application successfully used all modules")

    def test_web_server_simulation_with_modules(self):
        """Test a web server simulation using modular architecture."""
        # Create web server modules
        http_module_content = """
        program HTTP;
        var
          port: integer;
          max_connections: integer;
        begin
          port := 8080;
          max_connections := 100;
        end.
        """

        routing_module_content = """
        program Routing;
        var
          route_count: integer;
          default_route: integer;
        begin
          route_count := 5;
          default_route := 1;
        end.
        """

        database_module_content = """
        program Database;
        var
          connection_pool_size: integer;
          query_timeout: integer;
        begin
          connection_pool_size := 10;
          query_timeout := 30;
        end.
        """

        self.create_unit_file("HTTP", http_module_content)
        self.create_unit_file("Routing", routing_module_content)
        self.create_unit_file("Database", database_module_content)

        # Create web server program
        server_program_text = """
        program WebServer;
        var
          server_running: boolean;
          request_count: integer;
          response_time: integer;
        begin
          server_running := true;
          request_count := 0;
          response_time := 0;
          
          response_time := response_time + 50;
          request_count := request_count + 1;
          
          server_running := false;
        end.
        """

        lexer = Lexer(server_program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Use web server modules plus utilities
        tree.uses_clause = ["HTTP", "Routing", "Database", "Map", "ArrayUtils"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, "./stdlib"]

        # Execute web server
        interpreter.visit_Program(tree)

        # Verify all modules were loaded
        expected_modules = ["HTTP", "Routing", "Database", "Map", "ArrayUtils"]
        for module_name in expected_modules:
            self.assertIn(module_name, interpreter.module_registry.loaded_modules)
            module = interpreter.module_registry.get_module(module_name)
            self.assertIsNotNone(module)
            self.assertTrue(module.is_loaded)

        print("✓ Web server simulation successfully used modular architecture")

    def test_module_system_stress_test(self):
        """Stress test the module system with many modules and complex dependencies."""
        # Create a large number of interconnected modules
        num_modules = 15
        
        for i in range(num_modules):
            module_content = f"""
            program StressModule{i:02d};
            var
              module_id: integer;
              processing_time: integer;
            begin
              module_id := {i};
              processing_time := {i * 10};
            end.
            """
            self.create_unit_file(f"StressModule{i:02d}", module_content)

        # Create main stress test program
        stress_program_text = """
        program StressTest;
        var
          total_modules: integer;
          processing_complete: boolean;
          i: integer;
        begin
          total_modules := 15;
          processing_complete := false;
          
          for i := 1 to 5 do
          begin
            total_modules := total_modules + 1;
          end;
          
          processing_complete := true;
        end.
        """

        lexer = Lexer(stress_program_text)
        parser = Parser(lexer)
        tree = parser.program()

        # Use all stress test modules plus standard libraries
        stress_modules = [f"StressModule{i:02d}" for i in range(num_modules)]
        tree.uses_clause = stress_modules + ["Math", "ArrayUtils", "Map"]

        interpreter = Interpreter(tree)
        interpreter.module_registry.search_paths = [self.temp_dir, "./stdlib"]

        # Execute stress test
        start_time = time.time()
        interpreter.visit_Program(tree)
        execution_time = time.time() - start_time

        # Verify all modules were loaded
        all_expected_modules = stress_modules + ["Math", "ArrayUtils", "Map"]
        for module_name in all_expected_modules:
            self.assertIn(module_name, interpreter.module_registry.loaded_modules)
            module = interpreter.module_registry.get_module(module_name)
            self.assertIsNotNone(module)
            self.assertTrue(module.is_loaded)

        # Performance should be reasonable
        self.assertLess(execution_time, 3.0)

        print(f"✓ Stress test successfully loaded {len(all_expected_modules)} modules in {execution_time:.3f}s")


if __name__ == "__main__":
    unittest.main()