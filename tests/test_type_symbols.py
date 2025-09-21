"""Unit tests for TypeSymbol classes and operations"""

import unittest
from src.spi.symbol import (
    IntegerTypeSymbol, RealTypeSymbol, BooleanTypeSymbol, CharTypeSymbol,
    NeverSymbol, NEVER_SYMBOL,
    INTEGER_TYPE_SYMBOL, REAL_TYPE_SYMBOL, BOOLEAN_TYPE_SYMBOL, CHAR_TYPE_SYMBOL
)


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
        result = self.integer_type.get_result_type('=', self.integer_type)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)
        
        result = self.integer_type.get_result_type('<', self.integer_type)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)
        
        # INTEGER compared with REAL → BOOLEAN
        result = self.integer_type.get_result_type('=', self.real_type)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)
        
        # INTEGER compared with BOOLEAN → NEVER (invalid)
        result = self.integer_type.get_result_type('=', self.boolean_type)
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
        result = self.boolean_type.get_result_type('=', self.boolean_type)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)
        
        # BOOLEAN <> BOOLEAN → BOOLEAN
        result = self.boolean_type.get_result_type('<>', self.boolean_type)
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
        result = self.char_type.get_result_type('=', self.char_type)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)
        
        # CHAR < CHAR → BOOLEAN
        result = self.char_type.get_result_type('<', self.char_type)
        self.assertIs(result, BOOLEAN_TYPE_SYMBOL)
        
        # CHAR >= CHAR → BOOLEAN
        result = self.char_type.get_result_type('>=', self.char_type)
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
        result = self.never_type.get_result_type('+', self.integer_type)
        self.assertIs(result, NEVER_SYMBOL)


if __name__ == '__main__':
    unittest.main()