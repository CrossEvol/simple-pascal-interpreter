from enum import Enum


class NativeMethod(Enum):
    # procedure
    WRITE = "WRITE"
    WRITELN = "WRITELN"
    SETLENGTH = "SETLENGTH"
    EXIT = "EXIT"
    INC = "INC"
    DEC = "DEC"
    # function
    ORD = "ORD"
    LENGTH = "LENGTH"
    CHR = "CHR"
