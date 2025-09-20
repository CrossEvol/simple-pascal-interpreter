import unittest

from spi.ast_and_symbol import Block, Compound, Param, Type, Var
from spi.object import FunctionObject, ProcedureObject
from spi.token import Token, TokenType


class TestProcedureObject(unittest.TestCase):
    def setUp(self):
        """Set up test fixtures with mock AST nodes"""
        # Create mock tokens
        self.token = Token(TokenType.ID, "test", 1, 1)
        
        # Create mock parameter nodes
        self.param1 = Param(
            var_node=Var(self.token),
            type_node=Type(Token(TokenType.INTEGER, "INTEGER", 1, 1))
        )
        self.param1.var_node.value = "x"
        
        self.param2 = Param(
            var_node=Var(self.token),
            type_node=Type(Token(TokenType.REAL, "REAL", 1, 1))
        )
        self.param2.var_node.value = "y"
        
        # Create mock block AST
        self.block_ast = Block(
            declarations=[],
            compound_statement=Compound()
        )

    def test_procedure_object_creation(self):
        """Test ProcedureObject creation with basic attributes"""
        proc_obj = ProcedureObject(
            name="TestProc",
            formal_params=[self.param1, self.param2],
            block_ast=self.block_ast
        )
        
        self.assertEqual(proc_obj.name, "TestProc")
        self.assertEqual(len(proc_obj.formal_params), 2)
        self.assertEqual(proc_obj.block_ast, self.block_ast)

    def test_procedure_object_empty_params(self):
        """Test ProcedureObject creation with no parameters"""
        proc_obj = ProcedureObject(
            name="EmptyProc",
            formal_params=[],
            block_ast=self.block_ast
        )
        
        self.assertEqual(proc_obj.name, "EmptyProc")
        self.assertEqual(len(proc_obj.formal_params), 0)

    def test_get_param_names(self):
        """Test get_param_names method returns correct parameter names"""
        proc_obj = ProcedureObject(
            name="TestProc",
            formal_params=[self.param1, self.param2],
            block_ast=self.block_ast
        )
        
        param_names = proc_obj.get_param_names()
        self.assertEqual(param_names, ["x", "y"])

    def test_get_param_names_empty(self):
        """Test get_param_names method with no parameters"""
        proc_obj = ProcedureObject(
            name="EmptyProc",
            formal_params=[],
            block_ast=self.block_ast
        )
        
        param_names = proc_obj.get_param_names()
        self.assertEqual(param_names, [])

    def test_get_param_count(self):
        """Test get_param_count method returns correct count"""
        proc_obj = ProcedureObject(
            name="TestProc",
            formal_params=[self.param1, self.param2],
            block_ast=self.block_ast
        )
        
        param_count = proc_obj.get_param_count()
        self.assertEqual(param_count, 2)

    def test_get_param_count_empty(self):
        """Test get_param_count method with no parameters"""
        proc_obj = ProcedureObject(
            name="EmptyProc",
            formal_params=[],
            block_ast=self.block_ast
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
            type_node=Type(Token(TokenType.INTEGER, "INTEGER", 1, 1))
        )
        self.param1.var_node.value = "a"
        
        self.param2 = Param(
            var_node=Var(self.token),
            type_node=Type(Token(TokenType.REAL, "REAL", 1, 1))
        )
        self.param2.var_node.value = "b"
        
        # Create mock return type
        self.return_type = Type(Token(TokenType.INTEGER, "INTEGER", 1, 1))
        
        # Create mock block AST
        self.block_ast = Block(
            declarations=[],
            compound_statement=Compound()
        )

    def test_function_object_creation(self):
        """Test FunctionObject creation with basic attributes"""
        func_obj = FunctionObject(
            name="TestFunc",
            formal_params=[self.param1, self.param2],
            return_type=self.return_type,
            block_ast=self.block_ast
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
            block_ast=self.block_ast
        )
        
        self.assertEqual(func_obj.name, "EmptyFunc")
        self.assertEqual(len(func_obj.formal_params), 0)

    def test_get_param_names(self):
        """Test get_param_names method returns correct parameter names"""
        func_obj = FunctionObject(
            name="TestFunc",
            formal_params=[self.param1, self.param2],
            return_type=self.return_type,
            block_ast=self.block_ast
        )
        
        param_names = func_obj.get_param_names()
        self.assertEqual(param_names, ["a", "b"])

    def test_get_param_names_empty(self):
        """Test get_param_names method with no parameters"""
        func_obj = FunctionObject(
            name="EmptyFunc",
            formal_params=[],
            return_type=self.return_type,
            block_ast=self.block_ast
        )
        
        param_names = func_obj.get_param_names()
        self.assertEqual(param_names, [])

    def test_get_param_count(self):
        """Test get_param_count method returns correct count"""
        func_obj = FunctionObject(
            name="TestFunc",
            formal_params=[self.param1, self.param2],
            return_type=self.return_type,
            block_ast=self.block_ast
        )
        
        param_count = func_obj.get_param_count()
        self.assertEqual(param_count, 2)

    def test_get_param_count_empty(self):
        """Test get_param_count method with no parameters"""
        func_obj = FunctionObject(
            name="EmptyFunc",
            formal_params=[],
            return_type=self.return_type,
            block_ast=self.block_ast
        )
        
        param_count = func_obj.get_param_count()
        self.assertEqual(param_count, 0)


if __name__ == "__main__":
    unittest.main()