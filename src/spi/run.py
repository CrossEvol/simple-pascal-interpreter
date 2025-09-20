"""SPI - Simple Pascal Interpreter. Part 19"""

from __future__ import annotations

import argparse
import sys

from spi.error import (
    LexerError,
    ParserError,
    SemanticError,
)
from spi.interpreter import Interpreter
from spi.lexer import Lexer
from spi.parser import Parser
from spi.semantic_analyzer import SemanticAnalyzer

_SHOULD_LOG_SCOPE = False  # see '--scope' command line option
_SHOULD_LOG_STACK = False  # see '--stack' command line option

RETURN_NUM_FOR_LENGTH = "RETURN_NUM_FOR_LENGTH"


def runSpi() -> None:
    arg_parser = argparse.ArgumentParser(description="SPI - Simple Pascal Interpreter")
    arg_parser.add_argument("inputfile", help="Pascal source file")
    arg_parser.add_argument(
        "--scope",
        help="Print scope information",
        action="store_true",
    )
    arg_parser.add_argument(
        "--stack",
        help="Print call stack",
        action="store_true",
    )
    args = arg_parser.parse_args()

    global _SHOULD_LOG_SCOPE, _SHOULD_LOG_STACK
    _SHOULD_LOG_SCOPE = args.scope
    _SHOULD_LOG_STACK = args.stack

    text = open(args.inputfile, "r", encoding="utf-8").read()

    lexer = Lexer(text)
    try:
        parser = Parser(lexer)
        tree = parser.parse()
    except (LexerError, ParserError) as e:
        print(e.message)
        sys.exit(1)

    semantic_analyzer = SemanticAnalyzer()
    try:
        semantic_analyzer.visit(tree)
    except SemanticError as e:
        print(e.message)
        sys.exit(1)

    interpreter = Interpreter(tree)
    interpreter.interpret()
