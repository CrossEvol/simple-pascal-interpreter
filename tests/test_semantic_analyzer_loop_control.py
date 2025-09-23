"""
Unit tests for semantic analyzer loop control statement validation.
Tests the loop scope tracking mechanism and error detection for break/continue statements.
"""

import pytest
from spi.ast import BreakStatement, ContinueStatement
from spi.error import ErrorCode, SemanticError
from spi.lexer import Lexer
from spi.parser import Parser
from spi.semantic_analyzer import SemanticAnalyzer
from spi.token import Token, TokenType


class TestSemanticAnalyzerLoopControl:
    """Test cases for loop control statement semantic analysis"""

    def test_break_in_while_loop_valid(self):
        """Test that break statement inside while loop is valid"""
        text = """
        program TestBreak;
        var i: integer;
        begin
            i := 1;
            while i <= 10 do
            begin
                if i = 5 then
                    break;
                i := i + 1;
            end;
        end.
        """
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()
        semantic_analyzer = SemanticAnalyzer()

        # Should not raise any exception
        semantic_analyzer.visit(tree)

    def test_break_in_for_loop_valid(self):
        """Test that break statement inside for loop is valid"""
        text = """
        program TestBreak;
        var i: integer;
        begin
            for i := 1 to 10 do
            begin
                if i = 5 then
                    break;
            end;
        end.
        """
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()
        semantic_analyzer = SemanticAnalyzer()

        # Should not raise any exception
        semantic_analyzer.visit(tree)

    def test_continue_in_while_loop_valid(self):
        """Test that continue statement inside while loop is valid"""
        text = """
        program TestContinue;
        var i: integer;
        begin
            i := 1;
            while i <= 10 do
            begin
                if i mod 2 = 0 then
                    continue;
                i := i + 1;
            end;
        end.
        """
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()
        semantic_analyzer = SemanticAnalyzer()

        # Should not raise any exception
        semantic_analyzer.visit(tree)

    def test_continue_in_for_loop_valid(self):
        """Test that continue statement inside for loop is valid"""
        text = """
        program TestContinue;
        var i: integer;
        begin
            for i := 1 to 10 do
            begin
                if i mod 2 = 0 then
                    continue;
            end;
        end.
        """
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()
        semantic_analyzer = SemanticAnalyzer()

        # Should not raise any exception
        semantic_analyzer.visit(tree)

    def test_break_outside_loop_error(self):
        """Test that break statement outside loop raises error"""
        text = """
        program TestBreak;
        begin
            break;
        end.
        """
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()
        semantic_analyzer = SemanticAnalyzer()

        with pytest.raises(SemanticError) as exc_info:
            semantic_analyzer.visit(tree)

        assert exc_info.value.error_code == ErrorCode.BREAK_OUTSIDE_LOOP

    def test_continue_outside_loop_error(self):
        """Test that continue statement outside loop raises error"""
        text = """
        program TestContinue;
        begin
            continue;
        end.
        """
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()
        semantic_analyzer = SemanticAnalyzer()

        with pytest.raises(SemanticError) as exc_info:
            semantic_analyzer.visit(tree)

        assert exc_info.value.error_code == ErrorCode.CONTINUE_OUTSIDE_LOOP

    def test_nested_loops_break_inner_only(self):
        """Test that break in nested loop only affects inner loop"""
        text = """
        program TestNestedBreak;
        var i, j: integer;
        begin
            for i := 1 to 3 do
            begin
                for j := 1 to 3 do
                begin
                    if j = 2 then
                        break;
                end;
            end;
        end.
        """
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()
        semantic_analyzer = SemanticAnalyzer()

        # Should not raise any exception
        semantic_analyzer.visit(tree)

    def test_nested_loops_continue_inner_only(self):
        """Test that continue in nested loop only affects inner loop"""
        text = """
        program TestNestedContinue;
        var i, j: integer;
        begin
            for i := 1 to 3 do
            begin
                for j := 1 to 3 do
                begin
                    if j = 2 then
                        continue;
                end;
            end;
        end.
        """
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()
        semantic_analyzer = SemanticAnalyzer()

        # Should not raise any exception
        semantic_analyzer.visit(tree)

    def test_break_after_nested_loop_error(self):
        """Test that break after nested loop (but outside any loop) raises error"""
        text = """
        program TestBreakAfterNested;
        var i, j, k: integer;
        begin
            for i := 1 to 3 do
            begin
                for j := 1 to 3 do
                begin
                    k := k + 1;
                end;
            end;
            break;
        end.
        """
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()
        semantic_analyzer = SemanticAnalyzer()

        with pytest.raises(SemanticError) as exc_info:
            semantic_analyzer.visit(tree)

        assert exc_info.value.error_code == ErrorCode.BREAK_OUTSIDE_LOOP

    def test_continue_after_nested_loop_error(self):
        """Test that continue after nested loop (but outside any loop) raises error"""
        text = """
        program TestContinueAfterNested;
        var i, j, k: integer;
        begin
            for i := 1 to 3 do
            begin
                for j := 1 to 3 do
                begin
                    k := k + 1;
                end;
            end;
            continue;
        end.
        """
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()
        semantic_analyzer = SemanticAnalyzer()

        with pytest.raises(SemanticError) as exc_info:
            semantic_analyzer.visit(tree)

        assert exc_info.value.error_code == ErrorCode.CONTINUE_OUTSIDE_LOOP

    def test_mixed_loop_types_break_continue(self):
        """Test break and continue in mixed loop types (for inside while)"""
        text = """
        program TestMixedLoops;
        var i, j: integer;
        begin
            i := 1;
            while i <= 3 do
            begin
                for j := 1 to 3 do
                begin
                    if j = 2 then
                        break;
                    if j = 1 then
                        continue;
                end;
                i := i + 1;
            end;
        end.
        """
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()
        semantic_analyzer = SemanticAnalyzer()

        # Should not raise any exception
        semantic_analyzer.visit(tree)

    def test_loop_scope_stack_management(self):
        """Test that loop scope stack is properly managed"""
        semantic_analyzer = SemanticAnalyzer()

        # Initially empty
        assert len(semantic_analyzer.loop_stack) == 0

        # Simulate entering a while loop
        semantic_analyzer.loop_stack.append("while")
        assert len(semantic_analyzer.loop_stack) == 1
        assert semantic_analyzer.loop_stack[-1] == "while"

        # Simulate entering a nested for loop
        semantic_analyzer.loop_stack.append("for")
        assert len(semantic_analyzer.loop_stack) == 2
        assert semantic_analyzer.loop_stack[-1] == "for"

        # Simulate leaving the for loop
        semantic_analyzer.loop_stack.pop()
        assert len(semantic_analyzer.loop_stack) == 1
        assert semantic_analyzer.loop_stack[-1] == "while"

        # Simulate leaving the while loop
        semantic_analyzer.loop_stack.pop()
        assert len(semantic_analyzer.loop_stack) == 0

    def test_break_statement_direct_validation(self):
        """Test break statement validation directly"""
        semantic_analyzer = SemanticAnalyzer()
        break_token = Token(TokenType.BREAK, "break", 1, 1)
        break_stmt = BreakStatement(break_token)

        # Should raise error when not in loop
        with pytest.raises(SemanticError) as exc_info:
            semantic_analyzer.visit_BreakStatement(break_stmt)
        assert exc_info.value.error_code == ErrorCode.BREAK_OUTSIDE_LOOP

        # Should not raise error when in loop
        semantic_analyzer.loop_stack.append("while")
        semantic_analyzer.visit_BreakStatement(break_stmt)  # Should not raise

    def test_continue_statement_direct_validation(self):
        """Test continue statement validation directly"""
        semantic_analyzer = SemanticAnalyzer()
        continue_token = Token(TokenType.CONTINUE, "continue", 1, 1)
        continue_stmt = ContinueStatement(continue_token)

        # Should raise error when not in loop
        with pytest.raises(SemanticError) as exc_info:
            semantic_analyzer.visit_ContinueStatement(continue_stmt)
        assert exc_info.value.error_code == ErrorCode.CONTINUE_OUTSIDE_LOOP

        # Should not raise error when in loop
        semantic_analyzer.loop_stack.append("for")
        semantic_analyzer.visit_ContinueStatement(continue_stmt)  # Should not raise
