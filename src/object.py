from src.spi_token import ElementType


class Object:
    def __init__(self, type: ElementType):
        self.type = type


class EnumObject(Object):
    def __init__(self, index: int, name: str):
        super().__init__(type=ElementType.ENUM)
        self.index = index
        self.name = name

    def __eq__(self, other):
        if isinstance(other, EnumObject):
            return self.index == other.index and self.name == other.name
        return False
