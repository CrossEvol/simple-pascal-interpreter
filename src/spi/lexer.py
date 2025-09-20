"""SPI - Simple Pascal Interpreter. Part 19"""

from __future__ import annotations

from dataclasses import dataclass

from spi.error import (
    ErrorCode,
    LexerError,
)
from spi.token import Token, TokenType, _build_reserved_keywords

###############################################################################
#                                                                             #
#  LEXER                                                                      #
#                                                                             #
###############################################################################


RESERVED_KEYWORDS: dict[str, TokenType] = _build_reserved_keywords()


@dataclass
class LexerStatus:
    pos: int
    current_char: str | None
    lineno: int
    column: int


class Lexer:
    def __init__(self, text: str) -> None:
        # client string input, e.g. "4 + 2 * 3 - 6 / 2"
        self.text = text
        # self.pos is an index into self.text
        self.pos = 0
        self.current_char: str | None = self.text[self.pos]
        # token line number and column number
        self.lineno = 1
        self.column = 1

    def error(self):
        s = "Lexer error on '{lexeme}' line: {lineno} column: {column}".format(
            lexeme=self.current_char,
            lineno=self.lineno,
            column=self.column,
        )
        raise LexerError(message=s)

    def status(self) -> LexerStatus:
        """return the `pos` pointer`"""
        return LexerStatus(self.pos, self.current_char, self.lineno, self.column)

    def advance(self) -> None:
        """Advance the `pos` pointer and set the `current_char` variable."""
        if self.current_char == "\n":
            self.lineno += 1
            self.column = 0

        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_char = None  # Indicates end of input
        else:
            self.current_char = self.text[self.pos]
            self.column += 1

    def peek(self):
        peek_pos = self.pos + 1
        if peek_pos > len(self.text) - 1:
            return None
        else:
            return self.text[peek_pos]

    def skip_whitespace(self) -> None:
        while self.current_char is not None and self.current_char.isspace():
            self.advance()

    def skip_comment(self) -> None:
        while self.current_char != "}":
            self.advance()
        self.advance()  # the closing curly brace

    def number(self) -> Token:
        """Return a (multidigit) integer or float consumed from the input."""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

        result = ""
        while self.current_char is not None and self.current_char.isdigit():
            result += self.current_char
            self.advance()

        if self.current_char == "." and self.peek() == ".":
            token.type = TokenType.INTEGER_CONST
            token.value = int(result)
        elif self.current_char == ".":
            result += self.current_char
            self.advance()

            while self.current_char is not None and self.current_char.isdigit():
                result += self.current_char
                self.advance()

            token.type = TokenType.REAL_CONST
            token.value = float(result)
        else:
            token.type = TokenType.INTEGER_CONST
            token.value = int(result)

        return token

    def __string(self) -> Token:
        """Handle string const"""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

        if self.current_char == "'":
            self.advance()
        else:
            raise LexerError(
                error_code=ErrorCode.LEXER_STRING_ERROR,
                token=token,
                message="Unterminated string",
            )
        value = ""
        while self.current_char is not None and self.current_char != "'":
            value += self.current_char
            self.advance()

        if self.current_char == "'":
            self.advance()
        else:
            raise LexerError(
                error_code=ErrorCode.LEXER_STRING_ERROR,
                token=token,
                message="Unterminated string",
            )

        token.type = TokenType.STRING_CONST
        token.value = value
        return token

    def __char_const(self) -> Token:
        """Handle character const in format #digits"""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

        if self.current_char == "#":
            self.advance()
        else:
            raise LexerError(
                error_code=ErrorCode.LEXER_INVALID_CHARACTER,
                token=token,
                message="Expected # for character constant",
            )

        # Parse the numeric part
        value = ""
        while self.current_char is not None and self.current_char.isdigit():
            value += self.current_char
            self.advance()

        if not value:
            raise LexerError(
                error_code=ErrorCode.LEXER_INVALID_CHARACTER,
                token=token,
                message="Expected digits after # for character constant",
            )

        # Convert to character
        ascii_value = int(value)
        char_value = chr(ascii_value) if 0 <= ascii_value <= 255 else "\0"

        token.type = TokenType.CHAR_CONST
        token.value = char_value
        return token

    def __id(self) -> Token:
        """Handle identifiers and reserved keywords"""

        # Create a new token with current line and column number
        token = Token(type=None, value=None, lineno=self.lineno, column=self.column)

        value = ""
        while self.current_char is not None and self.current_char.isalnum():
            value += self.current_char
            self.advance()

        token_type = RESERVED_KEYWORDS.get(value.upper())
        if token_type is None:
            token.type = TokenType.ID
            token.value = value
        else:
            # reserved keyword
            token.type = token_type
            token.value = value.upper()

        return token

    def comparison(self) -> Token:
        """handle six comparison operators, [ '<', '>', '<>', '=', '<=', '>=']"""
        if self.current_char == "=":
            token = Token(
                type=TokenType.EQ,
                value=TokenType.EQ.value,  # '='
                lineno=self.lineno,
                column=self.column,
            )
            self.advance()
            return token

        if self.current_char == ">":
            if self.peek() == "=":
                token = Token(
                    type=TokenType.GE,
                    value=TokenType.GE.value,  # '>='
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token
            else:
                token = Token(
                    type=TokenType.GT,
                    value=TokenType.GT.value,  # '>'
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                return token

        if self.current_char == "<":
            if self.peek() == "=":
                token = Token(
                    type=TokenType.LE,
                    value=TokenType.LE.value,  # '<='
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token
            elif self.peek() == ">":
                token = Token(
                    type=TokenType.NE,
                    value=TokenType.NE.value,  # '<>'
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token
            else:
                token = Token(
                    type=TokenType.LT,
                    value=TokenType.LT.value,  # '<'
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                return token
        raise LexerError(
            error_code=ErrorCode.LEXER_INVALID_CHARACTER,
            token=Token(
                type=None,
                value=self.current_char,
                lineno=self.lineno,
                column=self.column,
            ),
            message=f"{ErrorCode.LEXER_INVALID_CHARACTER.value} -> '{self.current_char}'",
        )

    def get_next_token(self) -> Token:
        """Lexical analyzer (also known as scanner or tokenizer)

        This method is responsible for breaking a sentence
        apart into tokens. One token at a time.
        """
        while self.current_char is not None:
            if self.current_char.isspace():
                self.skip_whitespace()
                continue

            if self.current_char in ["<", ">", "="]:
                return self.comparison()

            if self.current_char == "{":
                self.advance()
                self.skip_comment()
                continue

            if self.current_char == "'":
                return self.__string()

            if self.current_char.isalpha():
                return self.__id()

            if self.current_char == "#":
                return self.__char_const()

            if self.current_char == "." and self.peek() == ".":
                token = Token(
                    type=TokenType.RANGE,
                    value=TokenType.RANGE.value,  # ':='
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token

            if self.current_char.isdigit():
                return self.number()

            if self.current_char == ":" and self.peek() == "=":
                token = Token(
                    type=TokenType.ASSIGN,
                    value=TokenType.ASSIGN.value,  # ':='
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token

            # single-character token
            try:
                # get enum member by value, e.g.
                # TokenType(';') --> TokenType.SEMI
                token_type = TokenType(self.current_char)
            except ValueError:
                # no enum member with value equal to self.current_char
                self.error()
            else:
                # create a token with a single-character lexeme as its value
                token = Token(
                    type=token_type,
                    value=token_type.value,  # e.g. ';', '.', etc
                    lineno=self.lineno,
                    column=self.column,
                )
                self.advance()
                return token

        # EOF (end-of-file) token indicates that there is no more
        # input left for lexical analysis
        return Token(type=TokenType.EOF, value=None)

    def peek_next_token(self) -> Token:
        prev_status = self.status()
        token = self.get_next_token()
        self.revert(prev_status)
        return token

    def revert(self, status: LexerStatus) -> None:
        """revert the current lexer status before call self.get_next_token()"""
        self.pos = status.pos
        self.current_char = status.current_char
        self.lineno = status.lineno
        self.column = status.column
