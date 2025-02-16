import re
import sys
from typing import Any


class SpiUtil:
    @staticmethod
    def print_w(message: Any):
        print(f"\033[91m{message}\033[0m", file=sys.stderr)

    @staticmethod
    def getField(s: str, n):
        list = s.split(".")
        v: Any = n[list[0]]
        for item in list[1:]:
            v = v[item]
        return v

    @staticmethod
    def toClassName(class_name: str):
        return "Class[{class_name}]".format(class_name=class_name)

    @staticmethod
    def extraClassName(class_name: str):
        pattern = r"Class\[(\w+)\]"
        match = re.search(pattern, class_name)
        if match:
            raw_class_name = match.group(1)
            return raw_class_name
        else:
            SpiUtil.print_w(
                "Could not find pattern {pattern} in {str}".format(
                    pattern=pattern, str=class_name
                )
            )
            return class_name
