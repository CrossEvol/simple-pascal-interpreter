import unittest

from spi.ast import Block, Compound, Param, Type, Var
from spi.object import FunctionObject, ProcedureObject, SubrangeObject, SetObject
from spi.token import Token, TokenType


class TestProcedureObject(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures with mock AST nodes"""
        # Create mock tokens
        self.token = Token(TokenType.ID, "test", 1, 1)

        # Create mock parameter nodes
        self.param1 = Param(
            var_node=Var(self.token),
            type_node=Type(Token(TokenType.INTEGER, "INTEGER", 1, 1)),
        )
        self.param1.var_node.value = "x"

        self.param2 = Param(
            var_node=Var(self.token),
            type_node=Type(Token(TokenType.REAL, "REAL", 1, 1)),
        )
        self.param2.var_node.value = "y"

        # Create mock block AST
        self.block_ast = Block(declarations=[], compound_statement=Compound())

    def test_procedure_object_creation(self):
        """Test ProcedureObject creation with basic attributes"""
        proc_obj = ProcedureObject(
            name="TestProc",
            formal_params=[self.param1, self.param2],
            block_ast=self.block_ast,
        )

        self.assertEqual(proc_obj.name, "TestProc")
        self.assertEqual(len(proc_obj.formal_params), 2)
        self.assertEqual(proc_obj.block_ast, self.block_ast)

    def test_procedure_object_empty_params(self):
        """Test ProcedureObject creation with no parameters"""
        proc_obj = ProcedureObject(
            name="EmptyProc", formal_params=[], block_ast=self.block_ast
        )

        self.assertEqual(proc_obj.name, "EmptyProc")
        self.assertEqual(len(proc_obj.formal_params), 0)

    def test_get_param_names(self):
        """Test get_param_names method returns correct parameter names"""
        proc_obj = ProcedureObject(
            name="TestProc",
            formal_params=[self.param1, self.param2],
            block_ast=self.block_ast,
        )

        param_names = proc_obj.get_param_names()
        self.assertEqual(param_names, ["x", "y"])

    def test_get_param_names_empty(self):
        """Test get_param_names method with no parameters"""
        proc_obj = ProcedureObject(
            name="EmptyProc", formal_params=[], block_ast=self.block_ast
        )

        param_names = proc_obj.get_param_names()
        self.assertEqual(param_names, [])

    def test_get_param_count(self):
        """Test get_param_count method returns correct count"""
        proc_obj = ProcedureObject(
            name="TestProc",
            formal_params=[self.param1, self.param2],
            block_ast=self.block_ast,
        )

        param_count = proc_obj.get_param_count()
        self.assertEqual(param_count, 2)

    def test_get_param_count_empty(self):
        """Test get_param_count method with no parameters"""
        proc_obj = ProcedureObject(
            name="EmptyProc", formal_params=[], block_ast=self.block_ast
        )

        param_count = proc_obj.get_param_count()
        self.assertEqual(param_count, 0)


class TestFunctionObject(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures with mock AST nodes"""
        # Create mock tokens
        self.token = Token(TokenType.ID, "test", 1, 1)

        # Create mock parameter nodes
        self.param1 = Param(
            var_node=Var(self.token),
            type_node=Type(Token(TokenType.INTEGER, "INTEGER", 1, 1)),
        )
        self.param1.var_node.value = "a"

        self.param2 = Param(
            var_node=Var(self.token),
            type_node=Type(Token(TokenType.REAL, "REAL", 1, 1)),
        )
        self.param2.var_node.value = "b"

        # Create mock return type
        self.return_type = Type(Token(TokenType.INTEGER, "INTEGER", 1, 1))

        # Create mock block AST
        self.block_ast = Block(declarations=[], compound_statement=Compound())

    def test_function_object_creation(self):
        """Test FunctionObject creation with basic attributes"""
        func_obj = FunctionObject(
            name="TestFunc",
            formal_params=[self.param1, self.param2],
            return_type=self.return_type,
            block_ast=self.block_ast,
        )

        self.assertEqual(func_obj.name, "TestFunc")
        self.assertEqual(len(func_obj.formal_params), 2)
        self.assertEqual(func_obj.return_type, self.return_type)
        self.assertEqual(func_obj.block_ast, self.block_ast)

    def test_function_object_empty_params(self):
        """Test FunctionObject creation with no parameters"""
        func_obj = FunctionObject(
            name="EmptyFunc",
            formal_params=[],
            return_type=self.return_type,
            block_ast=self.block_ast,
        )

        self.assertEqual(func_obj.name, "EmptyFunc")
        self.assertEqual(len(func_obj.formal_params), 0)

    def test_get_param_names(self):
        """Test get_param_names method returns correct parameter names"""
        func_obj = FunctionObject(
            name="TestFunc",
            formal_params=[self.return_type, self.param1, self.param2],
            return_type=self.return_type,
            block_ast=self.block_ast,
        )

        param_names = func_obj.get_param_names()
        self.assertEqual(param_names, ["a", "b"])

    def test_get_param_names_empty(self):
        """Test get_param_names method with no parameters"""
        func_obj = FunctionObject(
            name="EmptyFunc",
            formal_params=[],
            return_type=self.return_type,
            block_ast=self.block_ast,
        )

        param_names = func_obj.get_param_names()
        self.assertEqual(param_names, [])

    def test_get_param_count(self):
        """Test get_param_count method returns correct count"""
        func_obj = FunctionObject(
            name="TestFunc",
            formal_params=[self.return_type, self.param1, self.param2],
            return_type=self.return_type,
            block_ast=self.block_ast,
        )

        param_count = func_obj.get_param_count()
        self.assertEqual(param_count, 2)

    def test_get_param_count_empty(self):
        """Test get_param_count method with no parameters"""
        func_obj = FunctionObject(
            name="EmptyFunc",
            formal_params=[],
            return_type=self.return_type,
            block_ast=self.block_ast,
        )

        param_count = func_obj.get_param_count()
        self.assertEqual(param_count, 0)


class TestSubrangeObject(unittest.TestCase):
    def test_subrange_object_creation(self):
        """Test SubrangeObject creation with basic attributes"""
        subrange = SubrangeObject(1, 10)
        
        self.assertEqual(subrange.lower, 1)
        self.assertEqual(subrange.upper, 10)
        self.assertEqual(subrange.value, (1, 10))

    def test_subrange_contains_valid_values(self):
        """Test contains method with values within range"""
        subrange = SubrangeObject(5, 15)
        
        self.assertTrue(subrange.contains(5))   # Lower bound
        self.assertTrue(subrange.contains(10))  # Middle value
        self.assertTrue(subrange.contains(15))  # Upper bound

    def test_subrange_contains_invalid_values(self):
        """Test contains method with values outside range"""
        subrange = SubrangeObject(5, 15)
        
        self.assertFalse(subrange.contains(4))   # Below lower bound
        self.assertFalse(subrange.contains(16))  # Above upper bound
        self.assertFalse(subrange.contains(0))   # Far below
        self.assertFalse(subrange.contains(100)) # Far above

    def test_subrange_single_value(self):
        """Test subrange with single value (lower == upper)"""
        subrange = SubrangeObject(7, 7)
        
        self.assertTrue(subrange.contains(7))
        self.assertFalse(subrange.contains(6))
        self.assertFalse(subrange.contains(8))

    def test_subrange_to_set(self):
        """Test to_set method converts subrange to set"""
        subrange = SubrangeObject(3, 6)
        result_set = subrange.to_set()
        
        expected_set = {3, 4, 5, 6}
        self.assertEqual(result_set, expected_set)

    def test_subrange_to_set_single_value(self):
        """Test to_set method with single value subrange"""
        subrange = SubrangeObject(5, 5)
        result_set = subrange.to_set()
        
        expected_set = {5}
        self.assertEqual(result_set, expected_set)

    def test_subrange_to_set_large_range(self):
        """Test to_set method with larger range"""
        subrange = SubrangeObject(1, 5)
        result_set = subrange.to_set()
        
        expected_set = {1, 2, 3, 4, 5}
        self.assertEqual(result_set, expected_set)

    def test_subrange_str_representation(self):
        """Test string representation of subrange"""
        subrange = SubrangeObject(1, 10)
        self.assertEqual(str(subrange), "1..10")

    def test_subrange_repr_representation(self):
        """Test repr representation of subrange"""
        subrange = SubrangeObject(5, 15)
        self.assertEqual(repr(subrange), "SubrangeObject(5, 15)")

    def test_subrange_negative_bounds(self):
        """Test subrange with negative bounds"""
        subrange = SubrangeObject(-5, 5)
        
        self.assertTrue(subrange.contains(-3))
        self.assertTrue(subrange.contains(0))
        self.assertTrue(subrange.contains(3))
        self.assertFalse(subrange.contains(-6))
        self.assertFalse(subrange.contains(6))
        
        expected_set = {-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5}
        self.assertEqual(subrange.to_set(), expected_set)


class TestSetObject(unittest.TestCase):
    def test_set_object_creation_empty(self):
        """Test SetObject creation with no elements"""
        set_obj = SetObject()
        
        self.assertEqual(set_obj.elements, set())
        self.assertEqual(set_obj.value, set())
        self.assertTrue(set_obj.is_empty())

    def test_set_object_creation_with_elements(self):
        """Test SetObject creation with initial elements"""
        elements = {1, 3, 5}
        set_obj = SetObject(elements)
        
        self.assertEqual(set_obj.elements, elements)
        self.assertEqual(set_obj.value, elements)
        self.assertFalse(set_obj.is_empty())

    def test_set_contains_valid_elements(self):
        """Test contains method with elements in set"""
        set_obj = SetObject({1, 3, 5, 7})
        
        self.assertTrue(set_obj.contains(1))
        self.assertTrue(set_obj.contains(3))
        self.assertTrue(set_obj.contains(5))
        self.assertTrue(set_obj.contains(7))

    def test_set_contains_invalid_elements(self):
        """Test contains method with elements not in set"""
        set_obj = SetObject({1, 3, 5, 7})
        
        self.assertFalse(set_obj.contains(2))
        self.assertFalse(set_obj.contains(4))
        self.assertFalse(set_obj.contains(6))
        self.assertFalse(set_obj.contains(8))

    def test_set_add_element(self):
        """Test adding elements to set"""
        set_obj = SetObject({1, 3})
        
        set_obj.add(5)
        self.assertTrue(set_obj.contains(5))
        self.assertEqual(set_obj.elements, {1, 3, 5})

    def test_set_add_duplicate_element(self):
        """Test adding duplicate element to set"""
        set_obj = SetObject({1, 3, 5})
        
        set_obj.add(3)  # Add duplicate
        self.assertEqual(set_obj.elements, {1, 3, 5})  # Should remain unchanged

    def test_set_remove_element(self):
        """Test removing elements from set"""
        set_obj = SetObject({1, 3, 5, 7})
        
        set_obj.remove(3)
        self.assertFalse(set_obj.contains(3))
        self.assertEqual(set_obj.elements, {1, 5, 7})

    def test_set_remove_nonexistent_element(self):
        """Test removing element not in set (should not raise error)"""
        set_obj = SetObject({1, 3, 5})
        
        set_obj.remove(7)  # Remove non-existent element
        self.assertEqual(set_obj.elements, {1, 3, 5})  # Should remain unchanged

    def test_set_union(self):
        """Test union operation between sets"""
        set1 = SetObject({1, 3, 5})
        set2 = SetObject({3, 5, 7, 9})
        
        result = set1.union(set2)
        expected = {1, 3, 5, 7, 9}
        self.assertEqual(result.elements, expected)

    def test_set_intersection(self):
        """Test intersection operation between sets"""
        set1 = SetObject({1, 3, 5, 7})
        set2 = SetObject({3, 5, 7, 9})
        
        result = set1.intersection(set2)
        expected = {3, 5, 7}
        self.assertEqual(result.elements, expected)

    def test_set_difference(self):
        """Test difference operation between sets"""
        set1 = SetObject({1, 3, 5, 7})
        set2 = SetObject({3, 5, 9})
        
        result = set1.difference(set2)
        expected = {1, 7}
        self.assertEqual(result.elements, expected)

    def test_set_size(self):
        """Test size method returns correct count"""
        set_obj = SetObject({1, 3, 5, 7, 9})
        self.assertEqual(set_obj.size(), 5)
        
        empty_set = SetObject()
        self.assertEqual(empty_set.size(), 0)

    def test_set_is_empty(self):
        """Test is_empty method"""
        empty_set = SetObject()
        self.assertTrue(empty_set.is_empty())
        
        non_empty_set = SetObject({1})
        self.assertFalse(non_empty_set.is_empty())

    def test_set_str_representation_empty(self):
        """Test string representation of empty set"""
        set_obj = SetObject()
        self.assertEqual(str(set_obj), "[]")

    def test_set_str_representation_with_elements(self):
        """Test string representation of set with elements"""
        set_obj = SetObject({3, 1, 5})  # Unordered input
        result = str(set_obj)
        self.assertEqual(result, "[1, 3, 5]")  # Should be sorted

    def test_set_repr_representation(self):
        """Test repr representation of set"""
        elements = {1, 3, 5}
        set_obj = SetObject(elements)
        self.assertEqual(repr(set_obj), f"SetObject({elements})")


if __name__ == "__main__":
    unittest.main()
