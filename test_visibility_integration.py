"""
Integration tests for symbol visibility enforcement across the module system.

This test demonstrates the complete visibility enforcement system working
with multiple modules, different symbol types, and cross-module access.
"""

import unittest
from src.symbol import VarSymbol, ProcedureSymbol, FunctionSymbol, BuiltinTypeSymbol
from src.visibility import VisibilityLevel
from src.module import Unit, ModuleRegistry
from src.spi_ast import Type
from src.spi_token import Token, TokenType


class TestVisibilityIntegration(unittest.TestCase):
    """Integration tests for complete visibility enforcement system."""

    def setUp(self):
        """Set up test fixtures with multiple modules."""
        self.registry = ModuleRegistry()

        # Create sample type for testing
        self.integer_type = Type(
            token=Token(type=TokenType.INTEGER, value=0, lineno=1, column=1)
        )

        # Create three modules for testing
        self.math_unit = Unit("Math", "./stdlib/Math.pas")
        self.utils_unit = Unit("Utils", "./stdlib/Utils.pas")
        self.main_unit = Unit("Main", "./Main.pas")

        # Register modules
        self.registry.loaded_modules["Math"] = self.math_unit
        self.registry.loaded_modules["Utils"] = self.utils_unit
        self.registry.loaded_modules["Main"] = self.main_unit

    def test_complete_module_visibility_scenario(self):
        """Test a complete scenario with multiple modules and visibility levels."""

        # === Set up Math module ===
        # Interface section - public symbols
        self.math_unit.interface_symbols.set_current_section_visibility(
            VisibilityLevel.INTERFACE
        )

        add_func = FunctionSymbol("ADD", self.integer_type, [])
        mul_func = FunctionSymbol("MUL", self.integer_type, [])
        pi_const = VarSymbol("PI", BuiltinTypeSymbol("REAL"))

        self.math_unit.interface_symbols.insert(add_func)
        self.math_unit.interface_symbols.insert(mul_func)
        self.math_unit.interface_symbols.insert(pi_const)

        # Implementation section - private symbols
        self.math_unit.implementation_symbols.set_current_section_visibility(
            VisibilityLevel.IMPLEMENTATION
        )

        internal_helper = ProcedureSymbol("InternalHelper", [])
        debug_var = VarSymbol("DebugMode", BuiltinTypeSymbol("BOOLEAN"))

        self.math_unit.implementation_symbols.insert(internal_helper)
        self.math_unit.implementation_symbols.insert(debug_var)

        # === Set up Utils module ===
        # Interface section
        self.utils_unit.interface_symbols.set_current_section_visibility(
            VisibilityLevel.INTERFACE
        )

        sort_proc = ProcedureSymbol("Sort", [])
        max_size_const = VarSymbol("MAX_SIZE", BuiltinTypeSymbol("INTEGER"))

        self.utils_unit.interface_symbols.insert(sort_proc)
        self.utils_unit.interface_symbols.insert(max_size_const)

        # Implementation section
        self.utils_unit.implementation_symbols.set_current_section_visibility(
            VisibilityLevel.IMPLEMENTATION
        )

        swap_proc = ProcedureSymbol("Swap", [])
        temp_buffer = VarSymbol("TempBuffer", BuiltinTypeSymbol("STRING"))

        self.utils_unit.implementation_symbols.insert(swap_proc)
        self.utils_unit.implementation_symbols.insert(temp_buffer)

        # === Set up Main module that imports from both ===
        self.main_unit.interface_symbols.import_module_symbols(
            "Math", self.math_unit.interface_symbols
        )
        self.main_unit.interface_symbols.import_module_symbols(
            "Utils", self.utils_unit.interface_symbols
        )

        # === Test visibility enforcement ===

        # 1. Main should be able to access interface symbols from Math
        found_add = self.main_unit.interface_symbols.lookup_with_modules("ADD")
        self.assertIsNotNone(found_add)
        self.assertEqual(found_add.name, "ADD")

        found_pi = self.main_unit.interface_symbols.lookup_with_modules("PI")
        self.assertIsNotNone(found_pi)
        self.assertEqual(found_pi.name, "PI")

        # 2. Main should be able to access interface symbols from Utils
        found_sort = self.main_unit.interface_symbols.lookup_with_modules("Sort")
        self.assertIsNotNone(found_sort)
        self.assertEqual(found_sort.name, "Sort")

        found_max_size = self.main_unit.interface_symbols.lookup_with_modules(
            "MAX_SIZE"
        )
        self.assertIsNotNone(found_max_size)
        self.assertEqual(found_max_size.name, "MAX_SIZE")

        # 3. Main should NOT be able to access implementation symbols
        found_helper = self.main_unit.interface_symbols.lookup_with_modules(
            "InternalHelper"
        )
        self.assertIsNone(found_helper)

        found_debug = self.main_unit.interface_symbols.lookup_with_modules("DebugMode")
        self.assertIsNone(found_debug)

        found_swap = self.main_unit.interface_symbols.lookup_with_modules("Swap")
        self.assertIsNone(found_swap)

        found_temp = self.main_unit.interface_symbols.lookup_with_modules("TempBuffer")
        self.assertIsNone(found_temp)

        # 4. Math module should be able to access its own implementation symbols
        found_internal = self.math_unit.implementation_symbols.lookup_with_visibility(
            "InternalHelper", "Math"
        )
        self.assertIsNotNone(found_internal)

        found_debug_internal = (
            self.math_unit.implementation_symbols.lookup_with_visibility(
                "DebugMode", "Math"
            )
        )
        self.assertIsNotNone(found_debug_internal)

        # 5. Math module should NOT be able to access Utils implementation symbols
        # (even if it tried to import them, which it shouldn't be able to)
        found_swap_from_math = self.math_unit.interface_symbols.lookup_with_visibility(
            "Swap", "Math"
        )
        self.assertIsNone(found_swap_from_math)

    def test_interface_symbol_filtering(self):
        """Test that only interface symbols are exported during module import."""
        # Create a module with mixed visibility symbols
        test_unit = Unit("TestUnit", "./TestUnit.pas")

        # Add symbols with different visibility levels
        interface_var = VarSymbol(
            "InterfaceVar", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.INTERFACE
        )
        impl_var = VarSymbol(
            "ImplVar", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.IMPLEMENTATION
        )
        private_var = VarSymbol(
            "PrivateVar", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.PRIVATE
        )

        test_unit.interface_symbols.insert(interface_var)
        test_unit.interface_symbols.insert(impl_var)
        test_unit.interface_symbols.insert(private_var)

        # Import into main module
        self.main_unit.interface_symbols.import_module_symbols(
            "TestUnit", test_unit.interface_symbols
        )

        # Check that only interface symbols are accessible
        interface_symbols = self.main_unit.interface_symbols.imported_modules[
            "TestUnit"
        ]._symbols

        # Should only contain interface symbols
        self.assertIn("InterfaceVar", interface_symbols)
        self.assertNotIn("ImplVar", interface_symbols)
        self.assertNotIn("PrivateVar", interface_symbols)

        # Verify the symbol has correct visibility
        imported_var = interface_symbols["InterfaceVar"]
        self.assertEqual(imported_var.visibility, VisibilityLevel.INTERFACE)

    def test_cross_module_symbol_resolution_order(self):
        """Test symbol resolution order: local scope -> imported modules."""
        # Add a symbol to Math module
        math_var = VarSymbol(
            "CommonName", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.INTERFACE
        )
        self.math_unit.interface_symbols.insert(math_var)

        # Add a symbol to Utils module with same name
        utils_var = VarSymbol(
            "CommonName", BuiltinTypeSymbol("REAL"), VisibilityLevel.INTERFACE
        )
        self.utils_unit.interface_symbols.insert(utils_var)

        # Import both modules into Main (Utils imported last)
        self.main_unit.interface_symbols.import_module_symbols(
            "Math", self.math_unit.interface_symbols
        )
        self.main_unit.interface_symbols.import_module_symbols(
            "Utils", self.utils_unit.interface_symbols
        )

        # Should find the symbol from Math (first imported)
        found_var = self.main_unit.interface_symbols.lookup_with_modules("CommonName")
        self.assertIsNotNone(found_var)
        self.assertEqual(found_var.type.name, "INTEGER")  # From Math module

        # Now add a local symbol with same name
        local_var = VarSymbol(
            "CommonName", BuiltinTypeSymbol("BOOLEAN"), VisibilityLevel.PRIVATE
        )
        self.main_unit.interface_symbols.insert(local_var)

        # Should now find the local symbol (takes precedence)
        found_local = self.main_unit.interface_symbols.lookup_with_modules("CommonName")
        self.assertIsNotNone(found_local)
        self.assertEqual(found_local.type.name, "BOOLEAN")  # Local symbol

    def test_module_encapsulation(self):
        """Test that modules properly encapsulate their implementation details."""
        # Create a module with both interface and implementation symbols
        encap_unit = Unit("Encapsulation", "./Encapsulation.pas")

        # Interface symbols (public API)
        public_func = FunctionSymbol(
            "PublicFunction", self.integer_type, [], VisibilityLevel.INTERFACE
        )
        public_const = VarSymbol(
            "PUBLIC_CONST", BuiltinTypeSymbol("INTEGER"), VisibilityLevel.INTERFACE
        )

        # Implementation symbols (private internals)
        private_helper = ProcedureSymbol(
            "PrivateHelper", [], VisibilityLevel.IMPLEMENTATION
        )
        internal_state = VarSymbol(
            "InternalState",
            BuiltinTypeSymbol("BOOLEAN"),
            VisibilityLevel.IMPLEMENTATION,
        )

        encap_unit.interface_symbols.insert(public_func)
        encap_unit.interface_symbols.insert(public_const)
        encap_unit.implementation_symbols.insert(private_helper)
        encap_unit.implementation_symbols.insert(internal_state)

        # Another module tries to import
        client_unit = Unit("Client", "./Client.pas")
        client_unit.interface_symbols.import_module_symbols(
            "Encapsulation", encap_unit.interface_symbols
        )

        # Client should only see public interface
        self.assertIsNotNone(
            client_unit.interface_symbols.lookup_with_modules("PublicFunction")
        )
        self.assertIsNotNone(
            client_unit.interface_symbols.lookup_with_modules("PUBLIC_CONST")
        )

        # Client should NOT see implementation details
        self.assertIsNone(
            client_unit.interface_symbols.lookup_with_modules("PrivateHelper")
        )
        self.assertIsNone(
            client_unit.interface_symbols.lookup_with_modules("InternalState")
        )

        # But the original module should see everything
        self.assertIsNotNone(
            encap_unit.interface_symbols.lookup_with_visibility(
                "PublicFunction", "Encapsulation"
            )
        )
        self.assertIsNotNone(
            encap_unit.interface_symbols.lookup_with_visibility(
                "PUBLIC_CONST", "Encapsulation"
            )
        )
        self.assertIsNotNone(
            encap_unit.implementation_symbols.lookup_with_visibility(
                "PrivateHelper", "Encapsulation"
            )
        )
        self.assertIsNotNone(
            encap_unit.implementation_symbols.lookup_with_visibility(
                "InternalState", "Encapsulation"
            )
        )


if __name__ == "__main__":
    unittest.main()
