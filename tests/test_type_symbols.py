"""Unit tests for TypeSymbol classes and operations"""

import unittest
from spi.symbol import (
    IntegerTypeSymbol,
    RealTypeSymbol,
    BooleanTypeSymbol,
    CharTypeSymbol,
    NeverSymbol,
    NEVER_SYMBOL,
    TypeAliasSymbol,
    BuiltinTypeSymbol,
    ProcedureTypeSymbol,
    FunctionTypeSymbol,
    INTEGER_TYPE_SYMBOL,
    REAL_TYPE_SYMBOL,
    BOOLEAN_TYPE_SYMBOL,
    CHAR_TYPE_SYMBOL,
)
from src.spi.error import SemanticError, ErrorCode


class TestIntegerTypeSymbol(unittest.TestCase):
    """Test IntegerTypeSymbol arithmetic operations and compatibility"""

    def setUp(self):
        self.integer_type = INTEGER_TYPE_SYMBOL
        self.real_type = REAL_TYPE_SYMBOL
        self.boolean_type = BOOLEAN_TYPE_SYMBOL
        self.char_type = CHAR_TYPE_SYMBOL
        self.never_type = NEVER_SYMBOL

    def test_integer_creation(self):
        """Test IntegerTypeSymbol creation"""
        self.assertEqual(self.integer_type.name, "INTEGER")
        self.assertIsInstance(self.integer_type, IntegerTypeSymbol)

    def test_integer_compatibility(self):
        """Test INTEGER type compatibility"""
        # INTEGER is compatible with INTEGER and REAL
        self.assertTrue(self.integer_type.is_compatible_with(self.integer_type))
        self.assertTrue(self.integer_type.is_compatible_with(self.real_type))

        # INTEGER is not compatible with BOOLEAN, CHAR, or NEVER
        self.assertFalse(self.integer_type.is_compatible_with(self.boolean_type))
        self.assertFalse(self.integer_type.is_compatible_with(self.char_type))
        self.assertFalse(self.integer_type.is_compatible_with(self.never_type))

    def test_integer_assignment(self):
        """Test INTEGER assignment compatibility"""
        # INTEGER can only be assigned from INTEGER
        self.assertTrue(self.integer_type.can_assign_from(self.integer_type))

        # INTEGER cannot be assigned from REAL, BOOLEAN, CHAR, or NEVER
        self.assertFalse(self.integer_type.can_assign_from(self.real_type))
        self.assertFalse(self.integer_type.can_assign_from(self.boolean_type))
        self.assertFalse(self.integer_type.can_assign_from(self.char_type))
        self.assertFalse(self.integer_type.can_assign_from(self.never_type))

    def test_integer_addition(self):
        """Test INTEGER addition operations"""
        # INTEGER + INTEGER → INTEGER
        result = self.integer_type + self.integer_type
        self.assertIs(result, INTEGER_TYPE_SYMBOL)

        # INTEGER + REAL → REAL
        result = self.integer_type + self.real_type
        self.assertIs(result, REAL_TYPE_SYMBOL)

        # INTEGER + BOOLEAN → NEVER (invalid)
        result = self.integer_type + self.boolean_type
        self.assertIs(result, NEVER_SYMBOL)

        # INTEGER + NEVER → NEVER
        result = self.integer_type + self.never_type
        self.assertIs(result, NEVER_SYMBOL)

    def test_integer_subtraction(self):
        """Test INTEGER subtraction operations"""
        # INTEGER - INTEGER → INTEGER
        result = self.integer_type - self.integer_type
        self.assertIs(result, INTEGER_TYPE_SYMBOL)

        # INTEGER - REAL → REAL
        result = self.integer_type - self.real_type
        self.assertIs(result, REAL_TYPE_SYMBOL)

        # INTEGER - BOOLEAN → NEVER (invalid)
        result = self.integer_type - self.boolean_type
        self.assertIs(result, NEVER_SYMBOL)

    def test_integer_multiplication(self):
        """Test INTEGER multiplication operations"""
        # INTEGER * INTEGER → INTEGER
        result = self.integer_type * self.integer_type
        self.assertIs(result, INTEGER_TYPE_SYMBOL)

        # INTEGER * REAL → REAL
        result = self.integer_type * self.real_type
        self.assertIs(result, REAL_TYPE_SYMBOL)

        # INTEGER * BOOLEAN → NEVER (invalid)
        result = self.integer_type * self.boolean_type
        self.assertIs(result, NEVER_SYMBOL)

    def test_integer_division(self):
        """Test INTEGER division operations"""
        # INTEGER / INTEGER → REAL (division always returns REAL in Pascal)
        result = self.integer_type / self.integer_type
        self.assertIs(result, REAL_TYPE_SYMBOL)

        # INTEGER / REAL → REAL
        result = self.integer_type / self.real_type
        self.assertIs(result, REAL_TYPE_SYMBOL)

        # INTEGER / BOOLEAN → NEVER (invalid)
        result = self.integer_type / self.boolean_type
        self.assertIs(result, NEVER_SYMBOL)

    def test_integer_comparison_result_types(self):
        """Test INTEGER comparison result types"""
        # INTEGER compared with INTEGER → BOOLEAN
        result = self.integer_type.get_result_type("=", self.integer_type)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)

        result = self.integer_type.get_result_type("<", self.integer_type)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)

        # INTEGER compared with REAL → BOOLEAN
        result = self.integer_type.get_result_type("=", self.real_type)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)

        # INTEGER compared with BOOLEAN → NEVER (invalid)
        result = self.integer_type.get_result_type("=", self.boolean_type)
        self.assertIs(result, NEVER_SYMBOL)


class TestRealTypeSymbol(unittest.TestCase):
    """Test RealTypeSymbol arithmetic operations and compatibility"""

    def setUp(self):
        self.integer_type = INTEGER_TYPE_SYMBOL
        self.real_type = REAL_TYPE_SYMBOL
        self.boolean_type = BOOLEAN_TYPE_SYMBOL
        self.never_type = NEVER_SYMBOL

    def test_real_creation(self):
        """Test RealTypeSymbol creation"""
        self.assertEqual(self.real_type.name, "REAL")
        self.assertIsInstance(self.real_type, RealTypeSymbol)

    def test_real_compatibility(self):
        """Test REAL type compatibility"""
        # REAL is compatible with INTEGER and REAL
        self.assertTrue(self.real_type.is_compatible_with(self.integer_type))
        self.assertTrue(self.real_type.is_compatible_with(self.real_type))

        # REAL is not compatible with BOOLEAN or NEVER
        self.assertFalse(self.real_type.is_compatible_with(self.boolean_type))
        self.assertFalse(self.real_type.is_compatible_with(self.never_type))

    def test_real_assignment(self):
        """Test REAL assignment compatibility"""
        # REAL can be assigned from INTEGER and REAL
        self.assertTrue(self.real_type.can_assign_from(self.integer_type))
        self.assertTrue(self.real_type.can_assign_from(self.real_type))

        # REAL cannot be assigned from BOOLEAN or NEVER
        self.assertFalse(self.real_type.can_assign_from(self.boolean_type))
        self.assertFalse(self.real_type.can_assign_from(self.never_type))

    def test_real_arithmetic_operations(self):
        """Test REAL arithmetic operations"""
        # REAL + INTEGER → REAL
        result = self.real_type + self.integer_type
        self.assertIs(result, REAL_TYPE_SYMBOL)

        # REAL + REAL → REAL
        result = self.real_type + self.real_type
        self.assertIs(result, REAL_TYPE_SYMBOL)

        # REAL - INTEGER → REAL
        result = self.real_type - self.integer_type
        self.assertIs(result, REAL_TYPE_SYMBOL)

        # REAL * REAL → REAL
        result = self.real_type * self.real_type
        self.assertIs(result, REAL_TYPE_SYMBOL)

        # REAL / INTEGER → REAL
        result = self.real_type / self.integer_type
        self.assertIs(result, REAL_TYPE_SYMBOL)


class TestBooleanTypeSymbol(unittest.TestCase):
    """Test BooleanTypeSymbol logical operations and compatibility"""

    def setUp(self):
        self.integer_type = INTEGER_TYPE_SYMBOL
        self.boolean_type = BOOLEAN_TYPE_SYMBOL
        self.never_type = NEVER_SYMBOL

    def test_boolean_creation(self):
        """Test BooleanTypeSymbol creation"""
        self.assertEqual(self.boolean_type.name, "BOOLEAN")
        self.assertIsInstance(self.boolean_type, BooleanTypeSymbol)

    def test_boolean_compatibility(self):
        """Test BOOLEAN type compatibility"""
        # BOOLEAN is only compatible with BOOLEAN
        self.assertTrue(self.boolean_type.is_compatible_with(self.boolean_type))

        # BOOLEAN is not compatible with INTEGER or NEVER
        self.assertFalse(self.boolean_type.is_compatible_with(self.integer_type))
        self.assertFalse(self.boolean_type.is_compatible_with(self.never_type))

    def test_boolean_assignment(self):
        """Test BOOLEAN assignment compatibility"""
        # BOOLEAN can only be assigned from BOOLEAN
        self.assertTrue(self.boolean_type.can_assign_from(self.boolean_type))

        # BOOLEAN cannot be assigned from INTEGER or NEVER
        self.assertFalse(self.boolean_type.can_assign_from(self.integer_type))
        self.assertFalse(self.boolean_type.can_assign_from(self.never_type))

    def test_boolean_logical_operations(self):
        """Test BOOLEAN logical operations"""
        # BOOLEAN AND BOOLEAN → BOOLEAN
        result = self.boolean_type & self.boolean_type
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)

        # BOOLEAN OR BOOLEAN → BOOLEAN
        result = self.boolean_type | self.boolean_type
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)

        # NOT BOOLEAN → BOOLEAN
        result = self.boolean_type.logical_not()
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)

        # BOOLEAN AND INTEGER → NEVER (invalid)
        result = self.boolean_type & self.integer_type
        self.assertIs(result, NEVER_SYMBOL)

    def test_boolean_comparison_operations(self):
        """Test BOOLEAN comparison operations"""
        # BOOLEAN = BOOLEAN → BOOLEAN
        result = self.boolean_type.get_result_type("=", self.boolean_type)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)

        # BOOLEAN <> BOOLEAN → BOOLEAN
        result = self.boolean_type.get_result_type("<>", self.boolean_type)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)


class TestCharTypeSymbol(unittest.TestCase):
    """Test CharTypeSymbol comparison operations and compatibility"""

    def setUp(self):
        self.char_type = CHAR_TYPE_SYMBOL
        self.boolean_type = BOOLEAN_TYPE_SYMBOL
        self.never_type = NEVER_SYMBOL

    def test_char_creation(self):
        """Test CharTypeSymbol creation"""
        self.assertEqual(self.char_type.name, "CHAR")
        self.assertIsInstance(self.char_type, CharTypeSymbol)

    def test_char_compatibility(self):
        """Test CHAR type compatibility"""
        # CHAR is compatible with CHAR
        self.assertTrue(self.char_type.is_compatible_with(self.char_type))

        # CHAR is not compatible with BOOLEAN or NEVER
        self.assertFalse(self.char_type.is_compatible_with(self.boolean_type))
        self.assertFalse(self.char_type.is_compatible_with(self.never_type))

    def test_char_assignment(self):
        """Test CHAR assignment compatibility"""
        # CHAR can only be assigned from CHAR
        self.assertTrue(self.char_type.can_assign_from(self.char_type))

        # CHAR cannot be assigned from BOOLEAN or NEVER
        self.assertFalse(self.char_type.can_assign_from(self.boolean_type))
        self.assertFalse(self.char_type.can_assign_from(self.never_type))

    def test_char_comparison_operations(self):
        """Test CHAR comparison operations"""
        # CHAR = CHAR → BOOLEAN
        result = self.char_type.get_result_type("=", self.char_type)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)

        # CHAR < CHAR → BOOLEAN
        result = self.char_type.get_result_type("<", self.char_type)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)

        # CHAR >= CHAR → BOOLEAN
        result = self.char_type.get_result_type(">=", self.char_type)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)


class TestNeverSymbol(unittest.TestCase):
    """Test NeverSymbol behavior"""

    def setUp(self):
        self.never_type = NEVER_SYMBOL
        self.integer_type = INTEGER_TYPE_SYMBOL

    def test_never_singleton(self):
        """Test NeverSymbol is a singleton"""
        never1 = NeverSymbol()
        never2 = NeverSymbol()
        self.assertIs(never1, never2)
        self.assertIs(never1, NEVER_SYMBOL)

    def test_never_compatibility(self):
        """Test NEVER type compatibility"""
        # NEVER is compatible with nothing
        self.assertFalse(self.never_type.is_compatible_with(self.integer_type))
        self.assertFalse(self.never_type.is_compatible_with(self.never_type))

    def test_never_assignment(self):
        """Test NEVER assignment compatibility"""
        # NEVER cannot be assigned from anything
        self.assertFalse(self.never_type.can_assign_from(self.integer_type))
        self.assertFalse(self.never_type.can_assign_from(self.never_type))

    def test_never_operations(self):
        """Test NEVER type operations"""
        # All operations with NEVER return NEVER
        result = self.never_type.get_result_type("+", self.integer_type)
        self.assertIs(result, NEVER_SYMBOL)


class TestTypeAliasSymbol(unittest.TestCase):
    """Test TypeAliasSymbol type alias resolution and operations"""

    def setUp(self):
        self.integer_type = INTEGER_TYPE_SYMBOL
        self.real_type = REAL_TYPE_SYMBOL
        self.boolean_type = BOOLEAN_TYPE_SYMBOL
        self.never_type = NEVER_SYMBOL

    def test_type_alias_creation(self):
        """Test TypeAliasSymbol creation"""
        alias = TypeAliasSymbol("MyInt", self.integer_type)
        self.assertEqual(alias.name, "MyInt")
        self.assertIs(alias.target_type, self.integer_type)

    def test_simple_type_alias_resolution(self):
        """Test simple type alias resolution"""
        alias = TypeAliasSymbol("MyInt", self.integer_type)
        resolved = alias.resolve_final_type()
        self.assertIs(resolved, self.integer_type)

    def test_chained_type_alias_resolution(self):
        """Test chained type alias resolution"""
        # Create chain: MyInt -> YourInt -> INTEGER
        alias1 = TypeAliasSymbol("MyInt", self.integer_type)
        alias2 = TypeAliasSymbol("YourInt", alias1)

        resolved = alias2.resolve_final_type()
        self.assertIs(resolved, self.integer_type)

    def test_circular_type_alias_detection(self):
        """Test circular type alias detection"""
        # Create circular reference: A -> B -> A
        alias_a = TypeAliasSymbol("AliasA", self.integer_type)  # Temporary target
        alias_b = TypeAliasSymbol("AliasB", alias_a)
        alias_a.target_type = alias_b  # Create circular reference

        with self.assertRaises(SemanticError):
            alias_a.resolve_final_type()

    def test_complex_circular_type_alias_detection(self):
        """Test complex circular type alias detection"""
        # Create circular reference: A -> B -> C -> A
        alias_a = TypeAliasSymbol("AliasA", self.integer_type)  # Temporary target
        alias_b = TypeAliasSymbol("AliasB", alias_a)
        alias_c = TypeAliasSymbol("AliasC", alias_b)
        alias_a.target_type = alias_c  # Create circular reference

        with self.assertRaises(SemanticError):
            alias_b.resolve_final_type()

    def test_type_alias_compatibility(self):
        """Test type alias compatibility checking"""
        int_alias = TypeAliasSymbol("MyInt", self.integer_type)
        real_alias = TypeAliasSymbol("MyReal", self.real_type)

        # MyInt should be compatible with INTEGER and REAL
        self.assertTrue(int_alias.is_compatible_with(self.integer_type))
        self.assertTrue(int_alias.is_compatible_with(self.real_type))

        # MyInt should be compatible with MyReal (through resolved types)
        self.assertTrue(int_alias.is_compatible_with(real_alias))

        # MyInt should not be compatible with BOOLEAN
        self.assertFalse(int_alias.is_compatible_with(self.boolean_type))

        # MyInt should not be compatible with NEVER
        self.assertFalse(int_alias.is_compatible_with(self.never_type))

    def test_type_alias_assignment_compatibility(self):
        """Test type alias assignment compatibility"""
        int_alias = TypeAliasSymbol("MyInt", self.integer_type)
        real_alias = TypeAliasSymbol("MyReal", self.real_type)

        # MyInt can be assigned from INTEGER
        self.assertTrue(int_alias.can_assign_from(self.integer_type))

        # MyReal can be assigned from INTEGER and REAL
        self.assertTrue(real_alias.can_assign_from(self.integer_type))
        self.assertTrue(real_alias.can_assign_from(self.real_type))

        # MyInt cannot be assigned from REAL
        self.assertFalse(int_alias.can_assign_from(self.real_type))

        # Type aliases can be assigned from compatible aliases
        self.assertTrue(int_alias.can_assign_from(int_alias))
        self.assertTrue(real_alias.can_assign_from(int_alias))
        self.assertFalse(int_alias.can_assign_from(real_alias))

    def test_type_alias_operations(self):
        """Test type alias arithmetic operations"""
        int_alias = TypeAliasSymbol("MyInt", self.integer_type)
        real_alias = TypeAliasSymbol("MyReal", self.real_type)

        # MyInt + MyInt → INTEGER
        result = int_alias.get_result_type("+", int_alias)
        self.assertIs(result, INTEGER_TYPE_SYMBOL)

        # MyInt + MyReal → REAL
        result = int_alias.get_result_type("+", real_alias)
        self.assertIs(result, REAL_TYPE_SYMBOL)

        # MyInt + INTEGER → INTEGER
        result = int_alias.get_result_type("+", self.integer_type)
        self.assertIs(result, INTEGER_TYPE_SYMBOL)

        # MyReal / MyInt → REAL
        result = real_alias.get_result_type("/", int_alias)
        self.assertIs(result, REAL_TYPE_SYMBOL)

        # MyInt compared with MyInt → BOOLEAN
        result = int_alias.get_result_type("=", int_alias)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)

    def test_type_alias_with_never_type(self):
        """Test type alias operations with NEVER type"""
        int_alias = TypeAliasSymbol("MyInt", self.integer_type)

        # Operations with NEVER should return NEVER
        result = int_alias.get_result_type("+", self.never_type)
        self.assertIs(result, NEVER_SYMBOL)

        # Compatibility with NEVER should be False
        self.assertFalse(int_alias.is_compatible_with(self.never_type))
        self.assertFalse(int_alias.can_assign_from(self.never_type))

    def test_type_alias_string_representation(self):
        """Test TypeAliasSymbol string representation"""
        alias = TypeAliasSymbol("MyInt", self.integer_type)

        self.assertEqual(str(alias), "MyInt -> INTEGER")
        self.assertEqual(
            repr(alias), "<TypeAliasSymbol(name='MyInt', target='INTEGER')>"
        )

    def test_chained_alias_with_different_types(self):
        """Test chained aliases resolving to different types"""
        # Create chain: StringAlias -> CharAlias -> CHAR
        char_alias = TypeAliasSymbol("CharAlias", CHAR_TYPE_SYMBOL)
        string_alias = TypeAliasSymbol("StringAlias", char_alias)

        resolved = string_alias.resolve_final_type()
        self.assertIs(resolved, CHAR_TYPE_SYMBOL)

        # Test operations through the chain
        result = string_alias.get_result_type("=", CHAR_TYPE_SYMBOL)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)


class TestBuiltinTypeSymbol(unittest.TestCase):
    """Test BuiltinTypeSymbol delegation and compatibility"""

    def setUp(self):
        self.integer_type = INTEGER_TYPE_SYMBOL
        self.real_type = REAL_TYPE_SYMBOL
        self.boolean_type = BOOLEAN_TYPE_SYMBOL
        self.never_type = NEVER_SYMBOL

    def test_builtin_type_creation(self):
        """Test BuiltinTypeSymbol creation and delegation"""
        builtin_int = BuiltinTypeSymbol("INTEGER")
        self.assertEqual(builtin_int.name, "INTEGER")
        self.assertIs(builtin_int._delegate_type, INTEGER_TYPE_SYMBOL)

        builtin_real = BuiltinTypeSymbol("REAL")
        self.assertEqual(builtin_real.name, "REAL")
        self.assertIs(builtin_real._delegate_type, REAL_TYPE_SYMBOL)

        builtin_bool = BuiltinTypeSymbol("BOOLEAN")
        self.assertEqual(builtin_bool.name, "BOOLEAN")
        self.assertIs(builtin_bool._delegate_type, BOOLEAN_TYPE_SYMBOL)

    def test_builtin_type_case_insensitive(self):
        """Test BuiltinTypeSymbol is case insensitive"""
        builtin_int_lower = BuiltinTypeSymbol("integer")
        builtin_int_upper = BuiltinTypeSymbol("INTEGER")
        builtin_int_mixed = BuiltinTypeSymbol("Integer")

        self.assertIs(builtin_int_lower._delegate_type, INTEGER_TYPE_SYMBOL)
        self.assertIs(builtin_int_upper._delegate_type, INTEGER_TYPE_SYMBOL)
        self.assertIs(builtin_int_mixed._delegate_type, INTEGER_TYPE_SYMBOL)

    def test_builtin_type_unknown_type(self):
        """Test BuiltinTypeSymbol with unknown type"""
        builtin_unknown = BuiltinTypeSymbol("UNKNOWN")
        self.assertIs(builtin_unknown._delegate_type, NEVER_SYMBOL)

    def test_builtin_type_compatibility(self):
        """Test BuiltinTypeSymbol compatibility checking"""
        builtin_int = BuiltinTypeSymbol("INTEGER")
        builtin_real = BuiltinTypeSymbol("REAL")
        builtin_bool = BuiltinTypeSymbol("BOOLEAN")

        # INTEGER builtin should be compatible with INTEGER and REAL
        self.assertTrue(builtin_int.is_compatible_with(builtin_int))
        self.assertTrue(builtin_int.is_compatible_with(builtin_real))
        self.assertFalse(builtin_int.is_compatible_with(builtin_bool))

        # Should also be compatible with direct TypeSymbol instances
        self.assertTrue(builtin_int.is_compatible_with(self.integer_type))
        self.assertTrue(builtin_int.is_compatible_with(self.real_type))
        self.assertFalse(builtin_int.is_compatible_with(self.boolean_type))

    def test_builtin_type_assignment_compatibility(self):
        """Test BuiltinTypeSymbol assignment compatibility"""
        builtin_int = BuiltinTypeSymbol("INTEGER")
        builtin_real = BuiltinTypeSymbol("REAL")

        # INTEGER builtin can only be assigned from INTEGER
        self.assertTrue(builtin_int.can_assign_from(builtin_int))
        self.assertFalse(builtin_int.can_assign_from(builtin_real))

        # REAL builtin can be assigned from INTEGER and REAL
        self.assertTrue(builtin_real.can_assign_from(builtin_int))
        self.assertTrue(builtin_real.can_assign_from(builtin_real))

        # Should work with direct TypeSymbol instances too
        self.assertTrue(builtin_int.can_assign_from(self.integer_type))
        self.assertTrue(builtin_real.can_assign_from(self.integer_type))

    def test_builtin_type_operations(self):
        """Test BuiltinTypeSymbol arithmetic operations"""
        builtin_int = BuiltinTypeSymbol("INTEGER")
        builtin_real = BuiltinTypeSymbol("REAL")

        # INTEGER + INTEGER → INTEGER
        result = builtin_int.get_result_type("+", builtin_int)
        self.assertIs(result, INTEGER_TYPE_SYMBOL)

        # INTEGER + REAL → REAL
        result = builtin_int.get_result_type("+", builtin_real)
        self.assertIs(result, REAL_TYPE_SYMBOL)

        # Should work with direct TypeSymbol instances too
        result = builtin_int.get_result_type("+", self.real_type)
        self.assertIs(result, REAL_TYPE_SYMBOL)

        # Division always returns REAL
        result = builtin_int.get_result_type("/", builtin_int)
        self.assertIs(result, REAL_TYPE_SYMBOL)

    def test_builtin_type_resolve_final_type(self):
        """Test BuiltinTypeSymbol final type resolution"""
        builtin_int = BuiltinTypeSymbol("INTEGER")

        # Should resolve to the delegate type
        resolved = builtin_int.resolve_final_type()
        self.assertIs(resolved, INTEGER_TYPE_SYMBOL)

    def test_builtin_type_with_type_alias_delegate(self):
        """Test BuiltinTypeSymbol when delegate is a type alias"""
        # Create a type alias
        int_alias = TypeAliasSymbol("MyInt", self.integer_type)

        # Manually set delegate to test alias resolution
        builtin_int = BuiltinTypeSymbol("INTEGER")
        builtin_int._delegate_type = int_alias

        # Should resolve through the alias
        resolved = builtin_int.resolve_final_type()
        self.assertIs(resolved, INTEGER_TYPE_SYMBOL)

    def test_builtin_type_string_representation(self):
        """Test BuiltinTypeSymbol string representation"""
        builtin_int = BuiltinTypeSymbol("INTEGER")

        self.assertEqual(str(builtin_int), "INTEGER")
        self.assertIn("BuiltinTypeSymbol", repr(builtin_int))
        self.assertIn("INTEGER", repr(builtin_int))
        self.assertIn("delegate", repr(builtin_int))

    def test_builtin_type_with_custom_delegate(self):
        """Test BuiltinTypeSymbol with custom delegate type"""
        # Create a custom delegate
        custom_alias = TypeAliasSymbol("CustomInt", self.integer_type)
        builtin_custom = BuiltinTypeSymbol("CUSTOM", custom_alias)

        self.assertEqual(builtin_custom.name, "CUSTOM")
        self.assertIs(builtin_custom._delegate_type, custom_alias)

        # Should resolve through the alias
        resolved = builtin_custom.resolve_final_type()
        self.assertIs(resolved, INTEGER_TYPE_SYMBOL)

    def test_builtin_type_set_delegate_type(self):
        """Test BuiltinTypeSymbol set_delegate_type method"""
        builtin_type = BuiltinTypeSymbol("TEST")

        # Initially should delegate to NEVER (unknown type)
        self.assertIs(builtin_type._delegate_type, NEVER_SYMBOL)

        # Set a new delegate type
        builtin_type.set_delegate_type(self.integer_type)
        self.assertIs(builtin_type._delegate_type, INTEGER_TYPE_SYMBOL)

        # Should now behave like INTEGER
        self.assertTrue(builtin_type.is_compatible_with(self.integer_type))
        self.assertTrue(builtin_type.can_assign_from(self.integer_type))

    def test_builtin_type_delegate_type_property(self):
        """Test BuiltinTypeSymbol delegate_type property"""
        builtin_int = BuiltinTypeSymbol("INTEGER")

        delegate = builtin_int.delegate_type
        self.assertIs(delegate, INTEGER_TYPE_SYMBOL)

    def test_builtin_type_is_builtin_primitive(self):
        """Test BuiltinTypeSymbol is_builtin_primitive method"""
        builtin_int = BuiltinTypeSymbol("INTEGER")
        builtin_real = BuiltinTypeSymbol("REAL")
        builtin_bool = BuiltinTypeSymbol("BOOLEAN")
        builtin_char = BuiltinTypeSymbol("CHAR")
        builtin_unknown = BuiltinTypeSymbol("UNKNOWN")

        # Primitive types should return True
        self.assertTrue(builtin_int.is_builtin_primitive())
        self.assertTrue(builtin_real.is_builtin_primitive())
        self.assertTrue(builtin_bool.is_builtin_primitive())
        self.assertTrue(builtin_char.is_builtin_primitive())

        # Unknown type should return False
        self.assertFalse(builtin_unknown.is_builtin_primitive())

    def test_builtin_type_is_alias(self):
        """Test BuiltinTypeSymbol is_alias method"""
        builtin_int = BuiltinTypeSymbol("INTEGER")

        # Standard builtin should not be considered an alias
        self.assertFalse(builtin_int.is_alias())

        # Create with type alias delegate
        alias = TypeAliasSymbol("MyInt", self.integer_type)
        builtin_alias = BuiltinTypeSymbol("ALIAS", alias)
        self.assertTrue(builtin_alias.is_alias())

        # Create with nested BuiltinTypeSymbol delegate
        nested_builtin = BuiltinTypeSymbol("NESTED", builtin_int)
        self.assertTrue(nested_builtin.is_alias())

    def test_builtin_type_get_primitive_name(self):
        """Test BuiltinTypeSymbol get_primitive_name method"""
        builtin_int = BuiltinTypeSymbol("INTEGER")
        builtin_unknown = BuiltinTypeSymbol("UNKNOWN")

        self.assertEqual(builtin_int.get_primitive_name(), "INTEGER")
        self.assertEqual(builtin_unknown.get_primitive_name(), "NEVER")

        # Test with alias
        alias = TypeAliasSymbol("MyInt", self.integer_type)
        builtin_alias = BuiltinTypeSymbol("ALIAS", alias)
        self.assertEqual(builtin_alias.get_primitive_name(), "INTEGER")

    def test_builtin_type_complex_alias_resolution(self):
        """Test BuiltinTypeSymbol with complex alias chains"""
        # Create chain: BuiltinTypeSymbol -> TypeAliasSymbol -> INTEGER
        alias = TypeAliasSymbol("MyInt", self.integer_type)
        builtin_alias = BuiltinTypeSymbol("COMPLEX", alias)

        # Should resolve to INTEGER
        resolved = builtin_alias.resolve_final_type()
        self.assertIs(resolved, INTEGER_TYPE_SYMBOL)

        # Should work with operations
        result = builtin_alias.get_result_type("+", self.integer_type)
        self.assertIs(result, INTEGER_TYPE_SYMBOL)

        # Should be compatible with INTEGER types
        self.assertTrue(builtin_alias.is_compatible_with(self.integer_type))
        self.assertTrue(builtin_alias.can_assign_from(self.integer_type))

    def test_builtin_type_nested_builtin_resolution(self):
        """Test BuiltinTypeSymbol with nested BuiltinTypeSymbol delegation"""
        # Create nested chain: BuiltinTypeSymbol -> BuiltinTypeSymbol -> INTEGER
        inner_builtin = BuiltinTypeSymbol("INNER", self.integer_type)
        outer_builtin = BuiltinTypeSymbol("OUTER", inner_builtin)

        # Should resolve to INTEGER
        resolved = outer_builtin.resolve_final_type()
        self.assertIs(resolved, INTEGER_TYPE_SYMBOL)

        # Should work with operations
        result = outer_builtin.get_result_type("+", self.integer_type)
        self.assertIs(result, INTEGER_TYPE_SYMBOL)

    def test_builtin_type_circular_reference_detection(self):
        """Test BuiltinTypeSymbol circular reference detection"""
        # Create circular reference: A -> B -> A
        builtin_a = BuiltinTypeSymbol("A")
        builtin_b = BuiltinTypeSymbol("B", builtin_a)
        builtin_a.set_delegate_type(builtin_b)

        # Should detect circular reference and return NEVER
        resolved = builtin_a.resolve_final_type()
        self.assertIs(resolved, NEVER_SYMBOL)

        resolved = builtin_b.resolve_final_type()
        self.assertIs(resolved, NEVER_SYMBOL)

    def test_builtin_type_enhanced_string_representation(self):
        """Test BuiltinTypeSymbol enhanced string representation"""
        # Test with type alias delegate
        alias = TypeAliasSymbol("MyInt", self.integer_type)
        builtin_alias = BuiltinTypeSymbol("ALIAS", alias)

        repr_str = repr(builtin_alias)
        self.assertIn("BuiltinTypeSymbol", repr_str)
        self.assertIn("ALIAS", repr_str)
        self.assertIn("MyInt", repr_str)

        # Test with object that doesn't have name attribute
        class MockType:
            def __str__(self):
                return "MockType"

        mock_type = MockType()
        builtin_mock = BuiltinTypeSymbol("MOCK", mock_type)
        repr_str = repr(builtin_mock)
        self.assertIn("MockType", repr_str)


if __name__ == "__main__":
    unittest.main()


class TestProcedureTypeSymbol(unittest.TestCase):
    """Test ProcedureTypeSymbol signature compatibility and validation"""

    def setUp(self):
        self.integer_type = INTEGER_TYPE_SYMBOL
        self.real_type = REAL_TYPE_SYMBOL
        self.boolean_type = BOOLEAN_TYPE_SYMBOL
        self.char_type = CHAR_TYPE_SYMBOL
        self.never_type = NEVER_SYMBOL

    def test_procedure_type_creation(self):
        """Test ProcedureTypeSymbol creation"""
        proc_type = ProcedureTypeSymbol("TestProc", [self.integer_type, self.real_type])
        self.assertEqual(proc_type.name, "TestProc")
        self.assertEqual(len(proc_type.param_types), 2)
        self.assertIs(proc_type.param_types[0], self.integer_type)
        self.assertIs(proc_type.param_types[1], self.real_type)

    def test_procedure_type_empty_params(self):
        """Test ProcedureTypeSymbol with no parameters"""
        proc_type = ProcedureTypeSymbol("EmptyProc", [])
        self.assertEqual(proc_type.name, "EmptyProc")
        self.assertEqual(len(proc_type.param_types), 0)

    def test_procedure_type_compatibility_same_signature(self):
        """Test procedure type compatibility with same signature"""
        proc1 = ProcedureTypeSymbol("Proc1", [self.integer_type, self.real_type])
        proc2 = ProcedureTypeSymbol("Proc2", [self.integer_type, self.real_type])

        self.assertTrue(proc1.is_compatible_with(proc2))
        self.assertTrue(proc2.is_compatible_with(proc1))

    def test_procedure_type_compatibility_different_param_count(self):
        """Test procedure type compatibility with different parameter count"""
        proc1 = ProcedureTypeSymbol("Proc1", [self.integer_type])
        proc2 = ProcedureTypeSymbol("Proc2", [self.integer_type, self.real_type])

        self.assertFalse(proc1.is_compatible_with(proc2))
        self.assertFalse(proc2.is_compatible_with(proc1))

    def test_procedure_type_compatibility_different_param_types(self):
        """Test procedure type compatibility with different parameter types"""
        proc1 = ProcedureTypeSymbol("Proc1", [self.integer_type, self.boolean_type])
        proc2 = ProcedureTypeSymbol("Proc2", [self.integer_type, self.real_type])

        self.assertFalse(proc1.is_compatible_with(proc2))
        self.assertFalse(proc2.is_compatible_with(proc1))

    def test_procedure_type_compatibility_compatible_param_types(self):
        """Test procedure type compatibility with compatible parameter types"""
        # INTEGER is compatible with REAL
        proc1 = ProcedureTypeSymbol("Proc1", [self.integer_type])
        proc2 = ProcedureTypeSymbol("Proc2", [self.real_type])

        self.assertTrue(proc1.is_compatible_with(proc2))
        self.assertTrue(proc2.is_compatible_with(proc1))

    def test_procedure_type_compatibility_with_never_type(self):
        """Test procedure type compatibility with NEVER type"""
        proc_type = ProcedureTypeSymbol("Proc", [self.integer_type])

        self.assertFalse(proc_type.is_compatible_with(self.never_type))
        self.assertFalse(self.never_type.is_compatible_with(proc_type))

    def test_procedure_type_compatibility_with_non_procedure(self):
        """Test procedure type compatibility with non-procedure types"""
        proc_type = ProcedureTypeSymbol("Proc", [self.integer_type])

        self.assertFalse(proc_type.is_compatible_with(self.integer_type))
        self.assertFalse(proc_type.is_compatible_with(self.boolean_type))

    def test_procedure_type_assignment_compatibility(self):
        """Test procedure type assignment compatibility"""
        proc1 = ProcedureTypeSymbol("Proc1", [self.integer_type, self.real_type])
        proc2 = ProcedureTypeSymbol("Proc2", [self.integer_type, self.real_type])

        self.assertTrue(proc1.can_assign_from(proc2))
        self.assertTrue(proc2.can_assign_from(proc1))

    def test_procedure_type_assignment_different_signatures(self):
        """Test procedure type assignment with different signatures"""
        proc1 = ProcedureTypeSymbol("Proc1", [self.integer_type])
        proc2 = ProcedureTypeSymbol("Proc2", [self.real_type])

        # For procedure assignment, parameter types must be exactly assignable
        # INTEGER param can accept INTEGER, REAL param can accept INTEGER and REAL
        self.assertFalse(proc1.can_assign_from(proc2))  # INTEGER param cannot accept REAL arg
        self.assertTrue(proc2.can_assign_from(proc1))   # REAL param can accept INTEGER arg

    def test_procedure_type_assignment_incompatible_params(self):
        """Test procedure type assignment with incompatible parameters"""
        proc1 = ProcedureTypeSymbol("Proc1", [self.integer_type])
        proc2 = ProcedureTypeSymbol("Proc2", [self.boolean_type])

        self.assertFalse(proc1.can_assign_from(proc2))
        self.assertFalse(proc2.can_assign_from(proc1))

    def test_procedure_type_comparison_operations(self):
        """Test procedure type comparison operations"""
        proc1 = ProcedureTypeSymbol("Proc1", [self.integer_type])
        proc2 = ProcedureTypeSymbol("Proc2", [self.integer_type])

        # Procedure = Procedure → BOOLEAN (if compatible)
        result = proc1.get_result_type("=", proc2)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)

        # Procedure <> Procedure → BOOLEAN (if compatible)
        result = proc1.get_result_type("<>", proc2)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)

    def test_procedure_type_comparison_incompatible(self):
        """Test procedure type comparison with incompatible procedures"""
        proc1 = ProcedureTypeSymbol("Proc1", [self.integer_type])
        proc2 = ProcedureTypeSymbol("Proc2", [self.boolean_type])

        # Incompatible procedures should return NEVER for comparison
        result = proc1.get_result_type("=", proc2)
        self.assertIs(result, NEVER_SYMBOL)

    def test_procedure_type_arithmetic_operations(self):
        """Test procedure type arithmetic operations (should be invalid)"""
        proc_type = ProcedureTypeSymbol("Proc", [self.integer_type])

        # Procedures don't support arithmetic operations
        result = proc_type.get_result_type("+", proc_type)
        self.assertIs(result, NEVER_SYMBOL)

        result = proc_type.get_result_type("-", self.integer_type)
        self.assertIs(result, NEVER_SYMBOL)

        result = proc_type.get_result_type("*", proc_type)
        self.assertIs(result, NEVER_SYMBOL)

    def test_procedure_type_validate_call_signature(self):
        """Test procedure type call signature validation"""
        proc_type = ProcedureTypeSymbol("Proc", [self.integer_type, self.real_type])

        # Valid call with exact types
        self.assertTrue(proc_type.validate_call_signature([self.integer_type, self.real_type]))

        # Valid call with compatible types (INTEGER can be passed to REAL param)
        self.assertTrue(proc_type.validate_call_signature([self.integer_type, self.integer_type]))

        # Invalid call with wrong number of arguments
        self.assertFalse(proc_type.validate_call_signature([self.integer_type]))
        self.assertFalse(proc_type.validate_call_signature([self.integer_type, self.real_type, self.boolean_type]))

        # Invalid call with incompatible types
        self.assertFalse(proc_type.validate_call_signature([self.boolean_type, self.real_type]))

    def test_procedure_type_parameter_access(self):
        """Test procedure type parameter access methods"""
        proc_type = ProcedureTypeSymbol("Proc", [self.integer_type, self.real_type, self.boolean_type])

        # Test parameter count
        self.assertEqual(proc_type.get_parameter_count(), 3)

        # Test parameter type access
        self.assertIs(proc_type.get_parameter_type(0), self.integer_type)
        self.assertIs(proc_type.get_parameter_type(1), self.real_type)
        self.assertIs(proc_type.get_parameter_type(2), self.boolean_type)

        # Test out-of-bounds access
        self.assertIs(proc_type.get_parameter_type(-1), NEVER_SYMBOL)
        self.assertIs(proc_type.get_parameter_type(3), NEVER_SYMBOL)

    def test_procedure_type_string_representation(self):
        """Test ProcedureTypeSymbol string representation"""
        proc_type = ProcedureTypeSymbol("TestProc", [self.integer_type, self.real_type])

        self.assertEqual(str(proc_type), "PROCEDURE(INTEGER, REAL)")
        self.assertIn("ProcedureTypeSymbol", repr(proc_type))
        self.assertIn("TestProc", repr(proc_type))
        self.assertIn("INTEGER", repr(proc_type))
        self.assertIn("REAL", repr(proc_type))

    def test_procedure_type_empty_params_string(self):
        """Test ProcedureTypeSymbol string representation with no parameters"""
        proc_type = ProcedureTypeSymbol("EmptyProc", [])

        self.assertEqual(str(proc_type), "PROCEDURE()")
        self.assertIn("ProcedureTypeSymbol", repr(proc_type))
        self.assertIn("EmptyProc", repr(proc_type))


class TestFunctionTypeSymbol(unittest.TestCase):
    """Test FunctionTypeSymbol signature compatibility and validation"""

    def setUp(self):
        self.integer_type = INTEGER_TYPE_SYMBOL
        self.real_type = REAL_TYPE_SYMBOL
        self.boolean_type = BOOLEAN_TYPE_SYMBOL
        self.char_type = CHAR_TYPE_SYMBOL
        self.never_type = NEVER_SYMBOL

    def test_function_type_creation(self):
        """Test FunctionTypeSymbol creation"""
        func_type = FunctionTypeSymbol("TestFunc", [self.integer_type, self.real_type], self.boolean_type)
        self.assertEqual(func_type.name, "TestFunc")
        self.assertEqual(len(func_type.param_types), 2)
        self.assertIs(func_type.param_types[0], self.integer_type)
        self.assertIs(func_type.param_types[1], self.real_type)
        self.assertIs(func_type.return_type, self.boolean_type)

    def test_function_type_empty_params(self):
        """Test FunctionTypeSymbol with no parameters"""
        func_type = FunctionTypeSymbol("EmptyFunc", [], self.integer_type)
        self.assertEqual(func_type.name, "EmptyFunc")
        self.assertEqual(len(func_type.param_types), 0)
        self.assertIs(func_type.return_type, self.integer_type)

    def test_function_type_compatibility_same_signature(self):
        """Test function type compatibility with same signature"""
        func1 = FunctionTypeSymbol("Func1", [self.integer_type], self.real_type)
        func2 = FunctionTypeSymbol("Func2", [self.integer_type], self.real_type)

        self.assertTrue(func1.is_compatible_with(func2))
        self.assertTrue(func2.is_compatible_with(func1))

    def test_function_type_compatibility_different_param_count(self):
        """Test function type compatibility with different parameter count"""
        func1 = FunctionTypeSymbol("Func1", [self.integer_type], self.real_type)
        func2 = FunctionTypeSymbol("Func2", [self.integer_type, self.real_type], self.real_type)

        self.assertFalse(func1.is_compatible_with(func2))
        self.assertFalse(func2.is_compatible_with(func1))

    def test_function_type_compatibility_different_return_type(self):
        """Test function type compatibility with different return type"""
        func1 = FunctionTypeSymbol("Func1", [self.integer_type], self.real_type)
        func2 = FunctionTypeSymbol("Func2", [self.integer_type], self.boolean_type)

        self.assertFalse(func1.is_compatible_with(func2))
        self.assertFalse(func2.is_compatible_with(func1))

    def test_function_type_compatibility_compatible_return_type(self):
        """Test function type compatibility with compatible return type"""
        # INTEGER return is compatible with REAL return
        func1 = FunctionTypeSymbol("Func1", [self.integer_type], self.integer_type)
        func2 = FunctionTypeSymbol("Func2", [self.integer_type], self.real_type)

        self.assertTrue(func1.is_compatible_with(func2))
        self.assertTrue(func2.is_compatible_with(func1))

    def test_function_type_compatibility_with_never_type(self):
        """Test function type compatibility with NEVER type"""
        func_type = FunctionTypeSymbol("Func", [self.integer_type], self.real_type)

        self.assertFalse(func_type.is_compatible_with(self.never_type))
        self.assertFalse(self.never_type.is_compatible_with(func_type))

    def test_function_type_compatibility_with_non_function(self):
        """Test function type compatibility with non-function types"""
        func_type = FunctionTypeSymbol("Func", [self.integer_type], self.real_type)

        self.assertFalse(func_type.is_compatible_with(self.integer_type))
        self.assertFalse(func_type.is_compatible_with(self.boolean_type))

    def test_function_type_assignment_compatibility(self):
        """Test function type assignment compatibility"""
        func1 = FunctionTypeSymbol("Func1", [self.integer_type], self.real_type)
        func2 = FunctionTypeSymbol("Func2", [self.integer_type], self.real_type)

        self.assertTrue(func1.can_assign_from(func2))
        self.assertTrue(func2.can_assign_from(func1))

    def test_function_type_assignment_covariant_return(self):
        """Test function type assignment with covariant return types"""
        # Function returning INTEGER can be assigned to function returning REAL
        func_int_ret = FunctionTypeSymbol("FuncInt", [self.integer_type], self.integer_type)
        func_real_ret = FunctionTypeSymbol("FuncReal", [self.integer_type], self.real_type)

        # REAL can accept INTEGER (covariant)
        self.assertTrue(func_real_ret.can_assign_from(func_int_ret))
        # But INTEGER cannot accept REAL
        self.assertFalse(func_int_ret.can_assign_from(func_real_ret))

    def test_function_type_assignment_contravariant_params(self):
        """Test function type assignment with contravariant parameters"""
        # Function taking REAL param can be assigned to function taking INTEGER param
        func_int_param = FunctionTypeSymbol("FuncInt", [self.integer_type], self.boolean_type)
        func_real_param = FunctionTypeSymbol("FuncReal", [self.real_type], self.boolean_type)

        # For function assignment, parameter types must be exactly assignable
        # INTEGER param can accept INTEGER, REAL param can accept INTEGER and REAL
        self.assertFalse(func_int_param.can_assign_from(func_real_param))  # INTEGER param cannot accept REAL arg
        self.assertTrue(func_real_param.can_assign_from(func_int_param))   # REAL param can accept INTEGER arg

    def test_function_type_comparison_operations(self):
        """Test function type comparison operations"""
        func1 = FunctionTypeSymbol("Func1", [self.integer_type], self.real_type)
        func2 = FunctionTypeSymbol("Func2", [self.integer_type], self.real_type)

        # Function = Function → BOOLEAN (if compatible)
        result = func1.get_result_type("=", func2)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)

        # Function <> Function → BOOLEAN (if compatible)
        result = func1.get_result_type("<>", func2)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)

    def test_function_type_call_operation(self):
        """Test function type call operation"""
        func_type = FunctionTypeSymbol("Func", [self.integer_type], self.real_type)

        # Function call should return the function's return type
        result = func_type.get_result_type("CALL", func_type)
        self.assertIs(result, self.real_type)

    def test_function_type_arithmetic_operations(self):
        """Test function type arithmetic operations (should be invalid)"""
        func_type = FunctionTypeSymbol("Func", [self.integer_type], self.real_type)

        # Functions don't support arithmetic operations
        result = func_type.get_result_type("+", func_type)
        self.assertIs(result, NEVER_SYMBOL)

        result = func_type.get_result_type("-", self.integer_type)
        self.assertIs(result, NEVER_SYMBOL)

    def test_function_type_validate_call_signature(self):
        """Test function type call signature validation"""
        func_type = FunctionTypeSymbol("Func", [self.integer_type, self.real_type], self.boolean_type)

        # Valid call with exact types
        self.assertTrue(func_type.validate_call_signature([self.integer_type, self.real_type]))

        # Valid call with compatible types
        self.assertTrue(func_type.validate_call_signature([self.integer_type, self.integer_type]))

        # Invalid call with wrong number of arguments
        self.assertFalse(func_type.validate_call_signature([self.integer_type]))
        self.assertFalse(func_type.validate_call_signature([self.integer_type, self.real_type, self.boolean_type]))

        # Invalid call with incompatible types
        self.assertFalse(func_type.validate_call_signature([self.boolean_type, self.real_type]))

    def test_function_type_parameter_access(self):
        """Test function type parameter access methods"""
        func_type = FunctionTypeSymbol("Func", [self.integer_type, self.real_type], self.boolean_type)

        # Test parameter count
        self.assertEqual(func_type.get_parameter_count(), 2)

        # Test parameter type access
        self.assertIs(func_type.get_parameter_type(0), self.integer_type)
        self.assertIs(func_type.get_parameter_type(1), self.real_type)

        # Test out-of-bounds access
        self.assertIs(func_type.get_parameter_type(-1), NEVER_SYMBOL)
        self.assertIs(func_type.get_parameter_type(2), NEVER_SYMBOL)

        # Test return type access
        self.assertIs(func_type.get_return_type(), self.boolean_type)

    def test_function_type_string_representation(self):
        """Test FunctionTypeSymbol string representation"""
        func_type = FunctionTypeSymbol("TestFunc", [self.integer_type, self.real_type], self.boolean_type)

        self.assertEqual(str(func_type), "FUNCTION(INTEGER, REAL) : BOOLEAN")
        self.assertIn("FunctionTypeSymbol", repr(func_type))
        self.assertIn("TestFunc", repr(func_type))
        self.assertIn("INTEGER", repr(func_type))
        self.assertIn("REAL", repr(func_type))
        self.assertIn("BOOLEAN", repr(func_type))

    def test_function_type_empty_params_string(self):
        """Test FunctionTypeSymbol string representation with no parameters"""
        func_type = FunctionTypeSymbol("EmptyFunc", [], self.integer_type)

        self.assertEqual(str(func_type), "FUNCTION() : INTEGER")
        self.assertIn("FunctionTypeSymbol", repr(func_type))
        self.assertIn("EmptyFunc", repr(func_type))


if __name__ == "__main__":
    unittest.main()