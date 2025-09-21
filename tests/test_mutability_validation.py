"""Tests for variable mutability validation functionality"""

import unittest
from spi.symbol import (
    VarSymbol,
    MutabilityValidator,
    NEVER_SYMBOL,
    IntegerTypeSymbol,
)


class TestVarSymbolMutability(unittest.TestCase):
    """Test VarSymbol mutability tracking and validation"""

    def setUp(self):
        """Set up test fixtures"""
        self.integer_type = IntegerTypeSymbol()

    def test_mutable_variable_creation(self):
        """Test creating a mutable variable"""
        var = VarSymbol("x", self.integer_type, is_mutable=True)

        self.assertTrue(var.is_mutable)
        self.assertFalse(var.is_const)
        self.assertFalse(var.is_initialized)
        self.assertTrue(var.can_modify())

    def test_const_variable_creation(self):
        """Test creating a const variable"""
        var = VarSymbol("x", self.integer_type, is_mutable=False)

        self.assertFalse(var.is_mutable)
        self.assertTrue(var.is_const)
        self.assertFalse(var.is_initialized)
        self.assertTrue(var.can_modify())  # Can modify before initialization

    def test_const_variable_initialization(self):
        """Test const variable initialization"""
        var = VarSymbol("x", self.integer_type, is_mutable=False)

        # Before initialization
        self.assertTrue(var.can_modify())
        self.assertTrue(var.is_initialization_required())

        # Mark as initialized
        var.mark_initialized()

        # After initialization
        self.assertFalse(var.can_modify())
        self.assertTrue(var.is_initialized)
        self.assertFalse(var.is_initialization_required())

    def test_mutable_variable_default_behavior(self):
        """Test that variables are mutable by default"""
        var = VarSymbol("x", self.integer_type)

        self.assertTrue(var.is_mutable)
        self.assertFalse(var.is_const)
        self.assertTrue(var.can_modify())

    def test_never_symbol_type_handling(self):
        """Test that None type is replaced with NEVER_SYMBOL"""
        var = VarSymbol("x", None)

        self.assertEqual(var.type, NEVER_SYMBOL)

    def test_validate_assignment_mutable_variable(self):
        """Test assignment validation for mutable variables"""
        var = VarSymbol("x", self.integer_type, is_mutable=True)

        # Mutable variables can always be assigned
        is_valid, error = var.validate_assignment(is_initialization=False)
        self.assertTrue(is_valid)
        self.assertIsNone(error)

        is_valid, error = var.validate_assignment(is_initialization=True)
        self.assertTrue(is_valid)
        self.assertIsNone(error)

    def test_validate_assignment_const_variable_initialization(self):
        """Test assignment validation for const variable during initialization"""
        var = VarSymbol("x", self.integer_type, is_mutable=False)

        # Should allow initialization
        is_valid, error = var.validate_assignment(is_initialization=True)
        self.assertTrue(is_valid)
        self.assertIsNone(error)

    def test_validate_assignment_const_variable_after_initialization(self):
        """Test assignment validation for const variable after initialization"""
        var = VarSymbol("x", self.integer_type, is_mutable=False)
        var.mark_initialized()

        # Should not allow reassignment
        is_valid, error = var.validate_assignment(is_initialization=False)
        self.assertFalse(is_valid)
        self.assertIn("Cannot assign to const variable", error)
        self.assertIn("already initialized", error)

    def test_validate_assignment_const_variable_non_initialization(self):
        """Test assignment validation for const variable without initialization flag"""
        var = VarSymbol("x", self.integer_type, is_mutable=False)

        # Should not allow assignment without initialization flag
        is_valid, error = var.validate_assignment(is_initialization=False)
        self.assertFalse(is_valid)
        self.assertIn("Cannot assign to const variable", error)
        self.assertIn("must be initialized at declaration", error)

    def test_validate_modification_permission_mutable(self):
        """Test modification permission for mutable variables"""
        var = VarSymbol("x", self.integer_type, is_mutable=True)

        can_modify, error = var.validate_modification_permission()
        self.assertTrue(can_modify)
        self.assertIsNone(error)

    def test_validate_modification_permission_const_before_init(self):
        """Test modification permission for const variable before initialization"""
        var = VarSymbol("x", self.integer_type, is_mutable=False)

        can_modify, error = var.validate_modification_permission()
        self.assertTrue(can_modify)
        self.assertIsNone(error)

    def test_validate_modification_permission_const_after_init(self):
        """Test modification permission for const variable after initialization"""
        var = VarSymbol("x", self.integer_type, is_mutable=False)
        var.mark_initialized()

        can_modify, error = var.validate_modification_permission()
        self.assertFalse(can_modify)
        self.assertIn("Cannot modify const variable", error)

    def test_require_initialization_check(self):
        """Test initialization requirement checking"""
        mutable_var = VarSymbol("x", self.integer_type, is_mutable=True)
        const_var = VarSymbol("y", self.integer_type, is_mutable=False)

        self.assertFalse(mutable_var.require_initialization_check())
        self.assertTrue(const_var.require_initialization_check())


class TestMutabilityValidator(unittest.TestCase):
    """Test MutabilityValidator helper class"""

    def setUp(self):
        """Set up test fixtures"""
        self.integer_type = IntegerTypeSymbol()

    def test_validate_const_assignment_mutable_variable(self):
        """Test const assignment validation for mutable variables"""
        var = VarSymbol("x", self.integer_type, is_mutable=True)

        is_valid, error = MutabilityValidator.validate_const_assignment(
            var, is_initialization=False
        )
        self.assertTrue(is_valid)
        self.assertIsNone(error)

    def test_validate_const_assignment_const_initialization(self):
        """Test const assignment validation during initialization"""
        var = VarSymbol("x", self.integer_type, is_mutable=False)

        is_valid, error = MutabilityValidator.validate_const_assignment(
            var, is_initialization=True
        )
        self.assertTrue(is_valid)
        self.assertIsNone(error)

    def test_validate_const_assignment_const_reassignment(self):
        """Test const assignment validation for reassignment"""
        var = VarSymbol("x", self.integer_type, is_mutable=False)
        var.mark_initialized()

        is_valid, error = MutabilityValidator.validate_const_assignment(
            var, is_initialization=False
        )
        self.assertFalse(is_valid)
        self.assertIn("Cannot assign to const variable", error)

    def test_validate_variable_modification_mutable(self):
        """Test variable modification validation for mutable variables"""
        var = VarSymbol("x", self.integer_type, is_mutable=True)

        is_valid, error = MutabilityValidator.validate_variable_modification(var)
        self.assertTrue(is_valid)
        self.assertIsNone(error)

    def test_validate_variable_modification_const_initialized(self):
        """Test variable modification validation for initialized const variables"""
        var = VarSymbol("x", self.integer_type, is_mutable=False)
        var.mark_initialized()

        is_valid, error = MutabilityValidator.validate_variable_modification(var)
        self.assertFalse(is_valid)
        self.assertIn("Cannot modify const variable", error)

    def test_check_initialization_requirements_mutable(self):
        """Test initialization requirements for mutable variables"""
        var = VarSymbol("x", self.integer_type, is_mutable=True)

        is_satisfied, error = MutabilityValidator.check_initialization_requirements(var)
        self.assertTrue(is_satisfied)
        self.assertIsNone(error)

    def test_check_initialization_requirements_const_uninitialized(self):
        """Test initialization requirements for uninitialized const variables"""
        var = VarSymbol("x", self.integer_type, is_mutable=False)

        is_satisfied, error = MutabilityValidator.check_initialization_requirements(var)
        self.assertFalse(is_satisfied)
        self.assertIn("Const variable 'x' must be initialized", error)

    def test_check_initialization_requirements_const_initialized(self):
        """Test initialization requirements for initialized const variables"""
        var = VarSymbol("x", self.integer_type, is_mutable=False)
        var.mark_initialized()

        is_satisfied, error = MutabilityValidator.check_initialization_requirements(var)
        self.assertTrue(is_satisfied)
        self.assertIsNone(error)

    def test_mark_variable_initialized(self):
        """Test marking a variable as initialized"""
        var = VarSymbol("x", self.integer_type, is_mutable=False)
        self.assertFalse(var.is_initialized)

        MutabilityValidator.mark_variable_initialized(var)
        self.assertTrue(var.is_initialized)

    def test_is_const_variable(self):
        """Test const variable detection"""
        mutable_var = VarSymbol("x", self.integer_type, is_mutable=True)
        const_var = VarSymbol("y", self.integer_type, is_mutable=False)

        self.assertFalse(MutabilityValidator.is_const_variable(mutable_var))
        self.assertTrue(MutabilityValidator.is_const_variable(const_var))

    def test_get_mutability_info_mutable(self):
        """Test getting mutability info for mutable variables"""
        var = VarSymbol("x", self.integer_type, is_mutable=True)

        info = MutabilityValidator.get_mutability_info(var)

        self.assertTrue(info["is_mutable"])
        self.assertFalse(info["is_const"])
        self.assertFalse(info["is_initialized"])
        self.assertTrue(info["can_modify"])
        self.assertFalse(info["requires_initialization"])

    def test_get_mutability_info_const_uninitialized(self):
        """Test getting mutability info for uninitialized const variables"""
        var = VarSymbol("x", self.integer_type, is_mutable=False)

        info = MutabilityValidator.get_mutability_info(var)

        self.assertFalse(info["is_mutable"])
        self.assertTrue(info["is_const"])
        self.assertFalse(info["is_initialized"])
        self.assertTrue(info["can_modify"])  # Can modify before initialization
        self.assertTrue(info["requires_initialization"])

    def test_get_mutability_info_const_initialized(self):
        """Test getting mutability info for initialized const variables"""
        var = VarSymbol("x", self.integer_type, is_mutable=False)
        var.mark_initialized()

        info = MutabilityValidator.get_mutability_info(var)

        self.assertFalse(info["is_mutable"])
        self.assertTrue(info["is_const"])
        self.assertTrue(info["is_initialized"])
        self.assertFalse(info["can_modify"])  # Cannot modify after initialization
        self.assertTrue(info["requires_initialization"])

    def test_validate_non_var_symbol(self):
        """Test validation methods with non-VarSymbol objects"""
        non_var = self.integer_type  # Use a TypeSymbol instead

        # Should handle gracefully and return valid/no-error
        is_valid, error = MutabilityValidator.validate_const_assignment(non_var)
        self.assertTrue(is_valid)
        self.assertIsNone(error)

        is_valid, error = MutabilityValidator.validate_variable_modification(non_var)
        self.assertTrue(is_valid)
        self.assertIsNone(error)

        is_satisfied, error = MutabilityValidator.check_initialization_requirements(
            non_var
        )
        self.assertTrue(is_satisfied)
        self.assertIsNone(error)

        self.assertFalse(MutabilityValidator.is_const_variable(non_var))

        info = MutabilityValidator.get_mutability_info(non_var)
        self.assertTrue(info["is_mutable"])
        self.assertFalse(info["is_const"])


if __name__ == "__main__":
    unittest.main()
