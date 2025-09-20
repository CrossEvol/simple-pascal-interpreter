"""SPI - Simple Pascal Interpreter. Part 19"""

from __future__ import annotations

import sys
from typing import Any


class SpiUtil:
    @staticmethod
    def print_w(message: Any):
        print(f"\033[91m{message}\033[0m", file=sys.stderr)
