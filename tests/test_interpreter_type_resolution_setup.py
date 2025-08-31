"""
Unit tests for interpreter initialization with type resolution capabilities.

Tests cover proper TypeResolver initialization, module registry setup,
and type resolution context initialization.
"""

import unittest
from unittest.mock import Mock, MagicMock, patch

from src.interpreter import Interpreter
from src.lexer import Lexer
from src.parser import Parser
from src.module import ModuleRegistry
from src.type_resolver import TypeResolver, TypeResolutionContext
from src.spi_ast import Program, Block, Compound
from src.error import ModuleNotFoundError


class TestInterpreterTypeResolutionSetup(unittest.TestCase):
    """Test cases for interpreter initialization with type resolution."""

    def setUp(self):
        """Set up test fixtures before each test method."""
        # Create a simple program AST for testing
        self.simple_program_text = """
        program TestProgram;
        begin
        end.
        """
        
        # Create a program with uses clause
        self.program_with_uses_text = """
        program TestProgram;
        uses Math, Map;
        begin
        end.
        """
        
        # Parse the simple program
        lexer = Lexer(self.simple_program_text)
        parser = Parser(lexer)
        self.simple_program_ast = parser.program()
        
        # Parse the program with uses clause
        lexer_uses = Lexer(self.program_with_uses_text)
        parser_uses = Parser(lexer_uses)
        self.program_with_uses_ast = parser_uses.program()

    def test_interpreter_initialization_without_module_registry(self):
        """Test interpreter initialization without providing module registry."""
        interpreter = Interpreter(self.simple_program_ast)
        
        # Check that interpreter is properly initialized
        self.assertIsNotNone(interpreter.tree)
        self.assertEqual(interpreter.tree, self.simple_program_ast)
        
        # Check that module registry is created
        self.assertIsNotNone(interpreter.module_registry)
        self.assertIsInstance(interpreter.module_registry, ModuleRegistry)
        
        # Check that type resolver is initialized with module registry
        self.assertIsNotNone(interpreter.type_resolver)
        self.assertIsInstance(interpreter.type_resolver, TypeResolver)
        self.assertEqual(interpreter.type_resolver.module_registry, interpreter.module_registry)
        
        # Check that call stack is initialized
        self.assertIsNotNone(interpreter.call_stack)

    def test_interpreter_initialization_with_module_registry(self):
        """Test interpreter initialization with provided module registry."""
        # Create a mock module registry
        mock_module_registry = Mock(spec=ModuleRegistry)
        
        interpreter = Interpreter(self.simple_program_ast, mock_module_registry)
        
        # Check that provided module registry is used
        self.assertEqual(interpreter.module_registry, mock_module_registry)
        
        # Check that type resolver uses the provided module registry
        self.assertEqual(interpreter.type_resolver.module_registry, mock_module_registry)

    def test_type_resolver_initialization_with_context(self):
        """Test that type resolver is properly initialized with context information."""
        interpreter = Interpreter(self.simple_program_ast)
        
        # Check that type resolver cache is cleared during initialization
        # (This tests that _initialize_type_resolution_context was called)
        self.assertEqual(len(interpreter.type_resolver._type_cache), 0)
        
        # Check that type resolver has access to module registry
        self.assertIsNotNone(interpreter.type_resolver.module_registry)

    @patch('src.interpreter.Interpreter.log')
    def test_initialization_with_uses_clause_valid_modules(self, mock_log):
        """Test initialization when uses clause contains valid modules."""
        # Mock the module registry to simulate finding modules
        mock_module_registry = Mock(spec=ModuleRegistry)
        mock_module_registry.find_module_file.return_value = "/path/to/module.pas"
        
        interpreter = Interpreter(self.program_with_uses_ast, mock_module_registry)
        
        # Check that module files were checked during initialization
        expected_calls = [
            unittest.mock.call('Math'),
            unittest.mock.call('Map')
        ]
        mock_module_registry.find_module_file.assert_has_calls(expected_calls, any_order=True)
        
        # Check that no warning was logged
        mock_log.assert_not_called()

    @patch('src.interpreter.Interpreter.log')
    def test_initialization_with_uses_clause_missing_modules(self, mock_log):
        """Test initialization when uses clause contains missing modules."""
        # Mock the module registry to simulate module not found
        mock_module_registry = Mock(spec=ModuleRegistry)
        
        def side_effect(module_name):
            raise ModuleNotFoundError(module_name, [])
        
        mock_module_registry.find_module_file.side_effect = side_effect
        
        interpreter = Interpreter(self.program_with_uses_ast, mock_module_registry)
        
        # Check that warning was logged for missing modules
        mock_log.assert_called()
        # Check that at least one of the expected modules was logged
        logged_calls = [call[0][0] for call in mock_log.call_args_list]
        found_math_warning = any("Warning: Module 'Math' not found during initialization" in msg for msg in logged_calls)
        found_map_warning = any("Warning: Module 'Map' not found during initialization" in msg for msg in logged_calls)
        
        self.assertTrue(found_math_warning or found_map_warning, 
                       f"Expected warning for missing modules, got: {logged_calls}")

    def test_build_type_resolution_context_simple_program(self):
        """Test building type resolution context for simple program."""
        interpreter = Interpreter(self.simple_program_ast)
        
        # Create a mock activation record
        mock_ar = Mock()
        mock_ar.members_meta = {}
        
        context = interpreter._build_type_resolution_context(mock_ar)
        
        # Check context properties
        self.assertIsInstance(context, TypeResolutionContext)
        self.assertIsNone(context.current_module)
        self.assertEqual(context.imported_modules, [])
        self.assertEqual(context.local_classes, [])
        self.assertEqual(context.local_enums, [])
        self.assertEqual(context.local_records, [])
        self.assertEqual(context.module_registry, interpreter.module_registry)

    def test_build_type_resolution_context_with_uses_clause(self):
        """Test building type resolution context for program with uses clause."""
        interpreter = Interpreter(self.program_with_uses_ast)
        
        # Create a mock activation record
        mock_ar = Mock()
        mock_ar.members_meta = {}
        
        context = interpreter._build_type_resolution_context(mock_ar)
        
        # Check that uses clause modules are included
        self.assertEqual(set(context.imported_modules), {'Math', 'Map'})
        self.assertEqual(context.module_registry, interpreter.module_registry)

    def test_build_type_resolution_context_with_local_types(self):
        """Test building type resolution context with local types in activation record."""
        interpreter = Interpreter(self.simple_program_ast)
        
        # Create a mock activation record with type metadata
        mock_ar = Mock()
        
        # Create mock member metadata
        class MockMemberMeta:
            def __init__(self, is_class=False, is_enum=False, is_record=False):
                self.is_class = is_class
                self.is_enum = is_enum
                self.is_record = is_record
        
        mock_ar.members_meta = {
            'MyClass': MockMemberMeta(is_class=True),
            'MyEnum': MockMemberMeta(is_enum=True),
            'MyRecord': MockMemberMeta(is_record=True),
            'SomeVar': MockMemberMeta()  # Regular variable
        }
        
        context = interpreter._build_type_resolution_context(mock_ar)
        
        # Check that local types are properly categorized
        self.assertEqual(context.local_classes, ['MyClass'])
        self.assertEqual(context.local_enums, ['MyEnum'])
        self.assertEqual(context.local_records, ['MyRecord'])

    def test_type_resolver_cache_cleared_on_initialization(self):
        """Test that type resolver cache is cleared during initialization."""
        # Create interpreter and add something to cache
        interpreter = Interpreter(self.simple_program_ast)
        
        # Manually add something to cache to test clearing
        interpreter.type_resolver._type_cache['test_key'] = Mock()
        self.assertEqual(len(interpreter.type_resolver._type_cache), 1)
        
        # Re-initialize (simulate calling _initialize_type_resolution_context again)
        interpreter._initialize_type_resolution_context()
        
        # Check that cache was cleared
        self.assertEqual(len(interpreter.type_resolver._type_cache), 0)

    def test_interpreter_constructor_parameters(self):
        """Test that interpreter constructor accepts correct parameters."""
        # Test with just tree
        interpreter1 = Interpreter(self.simple_program_ast)
        self.assertIsNotNone(interpreter1.module_registry)
        
        # Test with tree and module registry
        mock_registry = Mock(spec=ModuleRegistry)
        interpreter2 = Interpreter(self.simple_program_ast, mock_registry)
        self.assertEqual(interpreter2.module_registry, mock_registry)
        
        # Test with None module registry (should create new one)
        interpreter3 = Interpreter(self.simple_program_ast, None)
        self.assertIsNotNone(interpreter3.module_registry)
        self.assertIsInstance(interpreter3.module_registry, ModuleRegistry)

    def test_type_resolver_integration_setup(self):
        """Test that type resolver is properly integrated with interpreter."""
        mock_module_registry = Mock(spec=ModuleRegistry)
        interpreter = Interpreter(self.simple_program_ast, mock_module_registry)
        
        # Check that type resolver has correct module registry reference
        self.assertEqual(interpreter.type_resolver.module_registry, mock_module_registry)
        
        # Check that type resolver methods are accessible
        self.assertTrue(hasattr(interpreter.type_resolver, 'resolve_type'))
        self.assertTrue(hasattr(interpreter.type_resolver, 'clear_cache'))
        self.assertTrue(hasattr(interpreter.type_resolver, 'get_cache_stats'))

    def test_initialization_error_handling(self):
        """Test error handling during initialization."""
        # Test with invalid tree (should not crash)
        try:
            interpreter = Interpreter(None)
            # Should not raise exception during initialization
            self.assertIsNotNone(interpreter.type_resolver)
        except Exception as e:
            self.fail(f"Interpreter initialization should not fail with None tree: {e}")

    def test_module_registry_default_configuration(self):
        """Test that default module registry is properly configured."""
        interpreter = Interpreter(self.simple_program_ast)
        
        # Check that module registry has default search paths
        self.assertIsNotNone(interpreter.module_registry.search_paths)
        self.assertIn('.', interpreter.module_registry.search_paths)
        self.assertIn('./stdlib', interpreter.module_registry.search_paths)
        
        # Check that loaded_modules dict exists
        self.assertTrue(hasattr(interpreter.module_registry, 'loaded_modules'))
        self.assertIsInstance(interpreter.module_registry.loaded_modules, dict)


if __name__ == '__main__':
    unittest.main()