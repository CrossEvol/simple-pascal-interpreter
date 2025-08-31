###############################################################################
#                                                                             #
#  INTERPRETER                                                                #
#                                                                             #
###############################################################################

"""SPI - Simple Pascal Interpreter. Part 19"""

from __future__ import annotations

import copy
from enum import Enum
from typing import Any, Dict, cast

from src.error import *
from src.globals import _SHOULD_LOG_STACK, RETURN_NUM
from src.object import (
    ArrayObject,
    BooleanObject,
    ClassObject,
    EnumObject,
    InstanceObject,
    IntegerObject,
    Object,
    OperationNotSupportedError,
    RealObject,
    RecordClassObject,
    RecordInstanceObject,
    StringObject,
)
from src.sematic_analyzer import NativeMethod
from src.spi_ast import *
from src.spi_token import ElementType, TokenType
from src.symbol import *
from src.util import SpiUtil
from src.visitor import NodeVisitor
from src.module import ModuleRegistry, Unit
from src.lexer import Lexer
from src.parser import Parser
from src.sematic_analyzer import SemanticAnalyzer
from src.visibility import VisibilityLevel
from src.type_resolver import TypeResolver, TypeResolutionContext


class ARType(Enum):
    PROGRAM = "PROGRAM"
    PROCEDURE = "PROCEDURE"
    FUNCTION = "FUNCTION"
    METHOD = "METHOD"


class CallStack:
    def __init__(self) -> None:
        self._records: list[ActivationRecord] = []

    def push(self, ar: ActivationRecord) -> None:
        self._records.append(ar)

    def pop(self) -> ActivationRecord:
        if len(self._records) >= 2:
            self._records[-2].copy_from(self._records[-1], True)
        return self._records.pop()

    def peek(self) -> ActivationRecord:
        return self._records[-1]

    def __str__(self) -> str:
        s = "\n".join(repr(ar) for ar in reversed(self._records))
        s = f"CALL STACK\n{s}\n\n"
        return s

    def __repr__(self) -> str:
        return self.__str__()


class MemberMeta:
    def __init__(self, type: ElementType, dynamic: bool = False, limit: int = -1):
        self.type = type
        self.dynamic = dynamic
        self.limit = limit
        self.is_class = False
        self.is_enum = False
        self.is_record = False
        self.is_instance = False
        self.is_record_instance = False
        self.ref_class_name = ""
        self.ref_record_name = ""


class ActivationRecord:
    def __init__(self, name: str, type: ARType, nesting_level: int) -> None:
        self.name = name
        self.type = type
        self.nesting_level = nesting_level
        self.members: dict[str, Any] = {}
        self.members_meta: dict[str, MemberMeta] = {}

    def __setitem__(self, key: str, value) -> None:
        self.members[key] = value

    def __getitem__(self, key: str):
        return self.members[key]

    def copy_from(self, other: "ActivationRecord", override: bool):
        for name, val in other.members.items():
            if override or name not in self.members:
                self.members[name] = val
        for name, val in other.members_meta.items():
            if override or name not in self.members_meta:
                self.members_meta[name] = val

    def get(self, key: str):
        return self.members.get(key)

    def set_meta(self, key: str, type: ElementType):
        self.members_meta[key] = MemberMeta(type=type)

    def set_dynamic(self, key: str, dynamic: bool):
        self.members_meta[key].dynamic = dynamic

    def set_limit(self, key: str, limit: int):
        self.members_meta[key].limit = limit

    def get_meta(self, key: str):
        meta = self.members_meta.get(key)
        if meta is None:
            raise InterpreterError()
        else:
            return meta

    def set_is_class(self, key: str, is_class: bool):
        self.members_meta[key].is_class = is_class

    def set_is_instance(self, key: str, is_instance: bool):
        self.members_meta[key].is_instance = is_instance

    def set_is_record_instance(
        self,
        key: str,
    ):
        self.members_meta[key].is_record_instance = True

    def set_is_enum(self, key: str):
        self.members_meta[key].is_enum = True

    def set_is_record(self, key: str):
        self.members_meta[key].is_record = True

    def set_ref_class_name(self, key: str, ref_class_name: str):
        self.members_meta[key].ref_class_name = ref_class_name

    def set_ref_record_name(self, key: str, ref_record_name: str):
        self.members_meta[key].ref_record_name = ref_record_name

    def __str__(self) -> str:
        lines = [
            "{level}: {type} {name}".format(
                level=self.nesting_level,
                type=self.type.value,
                name=self.name,
            )
        ]
        for name, val in self.members.items():
            lines.append(f"   {name:<20}: {val}")

        s = "\n".join(lines)
        return s

    def __repr__(self) -> str:
        return self.__str__()


class Interpreter(NodeVisitor):
    def __init__(self, tree: Program) -> None:
        self.tree = tree
        self.call_stack = CallStack()
        self.module_registry = ModuleRegistry()
        self.type_resolver = TypeResolver(self.module_registry)

    def log(self, msg) -> None:
        if _SHOULD_LOG_STACK:
            print(msg)

    def _load_and_process_modules(
        self, module_names: list[str], ar: ActivationRecord
    ) -> None:
        """
        Load and process modules specified in the uses clause.

        Args:
            module_names: List of module names to load
            ar: Current activation record to populate with module symbols
        """
        for module_name in module_names:
            try:
                self.log(f"Loading module: {module_name}")

                # Load the module using the registry
                module = self._load_module(module_name)

                # Import interface symbols into the current activation record
                self._import_module_symbols(module, ar)

                self.log(f"Successfully loaded module: {module_name}")

            except ModuleError as e:
                # Re-raise module errors with context
                raise e
            except Exception as e:
                # Wrap unexpected errors in a module error
                raise ModuleNotFoundError(
                    module_name, self.module_registry.search_paths
                ) from e

    def _load_module(self, module_name: str) -> Unit:
        """
        Load a module by name, parsing it if not already loaded.

        Args:
            module_name: Name of the module to load

        Returns:
            The loaded Unit instance

        Raises:
            ModuleNotFoundError: If the module file cannot be found
            Various parsing/semantic errors: If the module has syntax or semantic errors
        """
        # Check if module is already loaded
        existing_module = self.module_registry.get_module(module_name)
        if existing_module and existing_module.is_loaded:
            return existing_module

        # Find the module file
        try:
            file_path = self.module_registry.find_module_file(module_name)
        except ModuleNotFoundError:
            raise

        # Load and parse the module
        try:
            with open(file_path, "r") as file:
                text = file.read()

            # Create lexer and parser for the module
            lexer = Lexer(text)
            parser = Parser(lexer, self.module_registry)

            # Parse the unit (assuming it's a unit file)
            unit_ast = self._parse_unit_file(parser)

            # Create or get the unit from registry
            unit = self.module_registry.load_module(module_name, file_path)
            if not isinstance(unit, Unit):
                # Convert to Unit if it's a basic Module
                unit = Unit(unit.name, unit.file_path)
                self.module_registry.loaded_modules[module_name] = unit

            # Store the parsed AST
            unit.interface_ast = unit_ast.interface_block
            unit.implementation_ast = unit_ast.implementation_block

            # Add dependencies to the registry
            if unit_ast.uses_clause:
                for dep in unit_ast.uses_clause:
                    self.module_registry.add_dependency(module_name, dep)
                    # Recursively load dependencies
                    self._load_module(dep)

            # Run semantic analysis on the module
            try:
                self._analyze_module(unit)
            except Exception as analysis_error:
                # Log analysis error but continue - some modules might have issues
                # that don't prevent basic loading
                self.log(
                    f"Warning: Semantic analysis failed for module '{module_name}': {analysis_error}"
                )
                # For now, continue without full analysis

            # Mark as loaded
            unit.is_loaded = True

            return unit

        except FileNotFoundError:
            raise ModuleNotFoundError(module_name, self.module_registry.search_paths)
        except Exception as e:
            # Wrap parsing/analysis errors with module context
            raise ModuleError(f"Error loading module '{module_name}': {str(e)}") from e

    def _analyze_module(self, unit: Unit) -> None:
        """
        Run semantic analysis on a loaded module.

        Args:
            unit: The unit to analyze
        """
        # Create a semantic analyzer for the module
        analyzer = SemanticAnalyzer()

        # Set up module-aware symbol table
        unit.interface_symbols.set_current_section_visibility(VisibilityLevel.INTERFACE)
        analyzer.current_scope = unit.interface_symbols

        # Analyze interface section
        if unit.interface_ast:
            analyzer.visit(unit.interface_ast)

        # Switch to implementation section
        unit.implementation_symbols.set_current_section_visibility(
            VisibilityLevel.IMPLEMENTATION
        )
        analyzer.current_scope = unit.implementation_symbols

        # Analyze implementation section
        if unit.implementation_ast:
            analyzer.visit(unit.implementation_ast)

    def _import_module_symbols(self, module: Unit, ar: ActivationRecord) -> None:
        """
        Import interface symbols from a module into the current activation record.

        Args:
            module: The module to import symbols from
            ar: The activation record to populate with symbols
        """
        # Get interface symbols from the module
        interface_symbols = module.interface_symbols.get_interface_symbols()

        for symbol_name, symbol in interface_symbols.items():
            # Convert symbols to appropriate runtime objects and store in activation record
            if hasattr(symbol, "symbol_type"):
                if symbol.symbol_type.name == "INTEGER":
                    ar[symbol_name] = IntegerObject(0)
                    ar.set_meta(symbol_name, ElementType.INTEGER)
                elif symbol.symbol_type.name == "REAL":
                    ar[symbol_name] = RealObject(0.0)
                    ar.set_meta(symbol_name, ElementType.REAL)
                elif symbol.symbol_type.name == "BOOLEAN":
                    ar[symbol_name] = BooleanObject(False)
                    ar.set_meta(symbol_name, ElementType.BOOL)
                elif symbol.symbol_type.name == "STRING":
                    ar[symbol_name] = StringObject("")
                    ar.set_meta(symbol_name, ElementType.STRING)
                # For procedures and functions, store the symbol directly
                # They will be resolved during function calls
                else:
                    ar[symbol_name] = symbol

    def _parse_unit_file(self, parser: Parser) -> Unit:
        """
        Parse a unit file using the proper unit parsing functionality.

        Args:
            parser: The parser instance

        Returns:
            A Unit AST node with interface and implementation sections
        """
        try:
            # Use the new unit parsing functionality
            unit_ast = parser.unit_file()
            return unit_ast
        except Exception as e:
            # Handle parsing errors with more specific error messages
            raise ParserError(
                error_code=ErrorCode.UNEXPECTED_TOKEN,
                token=parser.current_token,
                message=f"Failed to parse unit file: {str(e)}",
            )

    def visit_Program(self, node: Program) -> None:
        program_name = node.name
        self.log(f"ENTER: PROGRAM {program_name}")

        # Create program activation record with module-aware symbol table
        ar = ActivationRecord(
            name=program_name,
            type=ARType.PROGRAM,
            nesting_level=1,
        )
        self.call_stack.push(ar)

        # Process uses clause if present
        if node.uses_clause:
            self.log(f"Processing uses clause: {node.uses_clause}")
            self._load_and_process_modules(node.uses_clause, ar)

        self.log(str(self.call_stack))

        self.visit(node.block)

        self.log(f"LEAVE: PROGRAM {program_name}")
        self.log(str(self.call_stack))

        self.call_stack.pop()

    def visit_Block(self, node: Block) -> None:
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node: VarDecl) -> None:
        ar = self.call_stack.peek()
        if node.type_node.token.type == TokenType.BOOLEAN:
            ar[node.var_node.value] = BooleanObject(False)
            ar.set_meta(key=node.var_node.value, type=ElementType.BOOL)
        elif node.type_node.token.type == TokenType.INTEGER:
            ar[node.var_node.value] = IntegerObject(0)
            ar.set_meta(key=node.var_node.value, type=ElementType.INTEGER)
        elif node.type_node.token.type == TokenType.REAL:
            ar[node.var_node.value] = RealObject(0.0)
            ar.set_meta(key=node.var_node.value, type=ElementType.REAL)
        elif node.type_node.token.type == TokenType.STRING:
            ar[node.var_node.value] = StringObject("")
            string_node = cast(StringType, node.type_node)
            limit: int = 255
            if string_node.limit is not None:
                limit = self.visit(string_node.limit)
            ar.set_meta(key=node.var_node.value, type=ElementType.STRING)
            ar.set_limit(key=node.var_node.value, limit=limit)
        elif node.type_node.token.type == TokenType.ARRAY:
            elements, element_type = self.__initArray(node.type_node)
            ar[node.var_node.value] = ArrayObject(elements, element_type)
            ar.set_meta(key=node.var_node.value, type=ElementType.ARRAY)
            if (cast(ArrayType, node.type_node)).dynamic is True:
                ar.set_dynamic(node.var_node.value, True)
        elif node.type_node.token.type == TokenType.RECORD:
            # TODO: should write this in recursion
            record_class: RecordClassObject = ar.get(node.type_node.token.value)
            fields: dict[str, Any] = record_class.fields.copy()
            ar[node.var_node.value] = RecordInstanceObject(
                record_name=record_class.record_name, fields=fields
            )
            ar.set_meta(node.var_node.value, ElementType.RECORD_CLASS)
            ar.set_is_record_instance(node.var_node.value)
            ar.set_ref_record_name(node.var_node.value, record_class.record_name)
        elif node.type_node.token.type == TokenType.CLASS:
            # TODO: should write this in recursion
            class_decl: ClassDecl = ar.get(
                SpiUtil.toClassName(node.type_node.token.value)
            )
            fields = class_decl.fields.copy()
            ar[node.var_node.value] = InstanceObject(class_decl.class_name, fields)
            ar.set_meta(node.var_node.value, ElementType.INSTANCE)
            ar.set_is_instance(node.var_node.value, True)
            ar.set_ref_class_name(node.var_node.value, class_decl.class_name)
        elif isinstance(node.type_node, UnresolvedType):
            # Resolve the type during interpretation
            resolved_type = self.visit_UnresolvedType(node.type_node)

            # Handle the resolved type appropriately
            if isinstance(resolved_type, PrimitiveType):
                if resolved_type.token.type == TokenType.BOOLEAN:
                    ar[node.var_node.value] = BooleanObject(False)
                    ar.set_meta(key=node.var_node.value, type=ElementType.BOOL)
                elif resolved_type.token.type == TokenType.INTEGER:
                    ar[node.var_node.value] = IntegerObject(0)
                    ar.set_meta(key=node.var_node.value, type=ElementType.INTEGER)
                elif resolved_type.token.type == TokenType.REAL:
                    ar[node.var_node.value] = RealObject(0.0)
                    ar.set_meta(key=node.var_node.value, type=ElementType.REAL)
            elif isinstance(resolved_type, StringType):
                ar[node.var_node.value] = StringObject("")
                limit: int = 255
                if resolved_type.limit is not None:
                    limit = self.visit(resolved_type.limit)
                ar.set_meta(key=node.var_node.value, type=ElementType.STRING)
                ar.set_limit(key=node.var_node.value, limit=limit)
            elif isinstance(resolved_type, ClassType):
                class_decl: ClassDecl = ar.get(
                    SpiUtil.toClassName(resolved_type.token.value)
                )
                fields = class_decl.fields.copy()
                ar[node.var_node.value] = InstanceObject(class_decl.class_name, fields)
                ar.set_meta(node.var_node.value, ElementType.INSTANCE)
                ar.set_is_instance(node.var_node.value, True)
                ar.set_ref_class_name(node.var_node.value, class_decl.class_name)
            elif isinstance(resolved_type, RecordType):
                record_class: RecordClassObject = ar.get(resolved_type.token.value)
                fields: dict[str, Any] = record_class.fields.copy()
                ar[node.var_node.value] = RecordInstanceObject(
                    record_name=record_class.record_name, fields=fields
                )
                ar.set_meta(node.var_node.value, ElementType.RECORD_CLASS)
                ar.set_is_record_instance(node.var_node.value)
                ar.set_ref_record_name(node.var_node.value, record_class.record_name)
            elif isinstance(resolved_type, EnumType):
                # Handle enum type variables
                enum_decl: EnumDecl = ar.get(resolved_type.token.value)
                ar[node.var_node.value] = enum_decl
                ar.set_meta(node.var_node.value, ElementType.ENUM)
                ar.set_is_enum(node.var_node.value)
        pass

    def __set_member_type(self, node: VarDecl, ar: ActivationRecord):
        if isinstance(node.type_node, ArrayType):
            if node.type_node.element_type.token.type == TokenType.BOOLEAN:
                ar.set_meta(node.var_node.value, ElementType.BOOL)
            elif node.type_node.element_type.token.type == TokenType.INTEGER:
                ar.set_meta(node.var_node.value, ElementType.INTEGER)
            elif node.type_node.element_type.token.type == TokenType.REAL:
                ar.set_meta(node.var_node.value, ElementType.REAL)
            elif node.type_node.element_type.token.type == TokenType.ARRAY:
                ar.set_meta(node.var_node.value, ElementType.ARRAY)
        else:
            pass

    def __initArray(
        self,
        node: Type,
    ) -> tuple[dict[Any, Any], ElementType]:
        if isinstance(node, ArrayType):
            lower_bound: IntegerObject = self.visit(node.lower)
            upper_bound: IntegerObject = self.visit(node.upper)
            if lower_bound > upper_bound:
                raise ArrayRangeInvalidError()

            elements: dict[int, Object] = {}
            element_type = ElementType.INTEGER  # Default type

            if node.element_type.token.type == TokenType.BOOLEAN:
                element_type = ElementType.BOOL
                if node.dynamic is False:
                    for i in range(lower_bound.value, upper_bound.value + 1):
                        elements[i] = BooleanObject(False)
            elif node.element_type.token.type == TokenType.INTEGER:
                element_type = ElementType.INTEGER
                if node.dynamic is False:
                    for i in range(lower_bound.value, upper_bound.value + 1):
                        elements[i] = IntegerObject(0)
            elif node.element_type.token.type == TokenType.REAL:
                element_type = ElementType.REAL
                if node.dynamic is False:
                    for i in range(lower_bound.value, upper_bound.value + 1):
                        elements[i] = RealObject(0.0)
            elif node.element_type.token.type == TokenType.ARRAY:
                element_type = ElementType.ARRAY
                if node.dynamic is False:
                    for i in range(lower_bound.value, upper_bound.value + 1):
                        sub_elements, sub_element_type = self.__initArray(
                            node.element_type
                        )
                        elements[i] = ArrayObject(sub_elements, sub_element_type)
            elif isinstance(node.element_type, UnresolvedType):
                # Resolve the element type during array initialization
                resolved_element_type = self.visit_UnresolvedType(node.element_type)

                # Handle the resolved element type
                if isinstance(resolved_element_type, PrimitiveType):
                    if resolved_element_type.token.type == TokenType.BOOLEAN:
                        element_type = ElementType.BOOL
                        if node.dynamic is False:
                            for i in range(lower_bound.value, upper_bound.value + 1):
                                elements[i] = BooleanObject(False)
                    elif resolved_element_type.token.type == TokenType.INTEGER:
                        element_type = ElementType.INTEGER
                        if node.dynamic is False:
                            for i in range(lower_bound.value, upper_bound.value + 1):
                                elements[i] = IntegerObject(0)
                    elif resolved_element_type.token.type == TokenType.REAL:
                        element_type = ElementType.REAL
                        if node.dynamic is False:
                            for i in range(lower_bound.value, upper_bound.value + 1):
                                elements[i] = RealObject(0.0)
                elif isinstance(resolved_element_type, StringType):
                    element_type = ElementType.STRING
                    if node.dynamic is False:
                        for i in range(lower_bound.value, upper_bound.value + 1):
                            elements[i] = StringObject("")
                elif isinstance(resolved_element_type, ArrayType):
                    element_type = ElementType.ARRAY
                    if node.dynamic is False:
                        for i in range(lower_bound.value, upper_bound.value + 1):
                            sub_elements, sub_element_type = self.__initArray(
                                resolved_element_type
                            )
                            elements[i] = ArrayObject(sub_elements, sub_element_type)
                else:
                    # For complex types (classes, records, enums), use default element type
                    element_type = ElementType.INTEGER  # Default fallback
            return elements, element_type
        raise UnknownTypeError()

    def visit_Type(self, node: Type) -> None:
        # Do nothing
        pass

    def visit_StringType(self, node: StringType) -> None:
        # Do nothing
        pass

    def visit_PrimitiveType(self, node: PrimitiveType) -> None:
        # Do nothing
        pass

    def visit_ArrayType(self, node: ArrayType) -> None:
        # Do nothing
        pass

    def visit_ClassType(self, node: ClassType) -> None:
        # Do nothing
        pass

    def visit_EnumType(self, node: EnumType) -> None:
        # Do nothing
        pass

    def visit_RecordType(self, node: RecordType) -> None:
        # Do nothing
        pass

    def visit_UnresolvedType(self, node: UnresolvedType) -> Type:
        """
        Resolve unresolved types during interpretation.

        Args:
            node: The UnresolvedType node to resolve

        Returns:
            The resolved Type node

        Raises:
            TypeResolutionError: If type cannot be resolved
        """
        # Check if already resolved (caching mechanism)
        if node.resolved_type is not None:
            return node.resolved_type

        # Build resolution context from current activation record
        ar = self.call_stack.peek()
        context = self._build_type_resolution_context(ar)

        # Resolve the type using TypeResolver
        resolved_type = self.type_resolver.resolve_type(node, context)

        # Cache the resolved type to avoid repeated resolution
        node.resolved_type = resolved_type

        return resolved_type

    def _build_type_resolution_context(
        self, ar: ActivationRecord
    ) -> TypeResolutionContext:
        """
        Build type resolution context from current activation record.

        Args:
            ar: Current activation record

        Returns:
            TypeResolutionContext with available types and modules
        """
        local_classes = []
        local_enums = []
        local_records = []
        imported_modules = []

        # Extract type information from activation record
        for name, meta in ar.members_meta.items():
            if meta.is_class:
                local_classes.append(name)
            elif meta.is_enum:
                local_enums.append(name)
            elif meta.is_record:
                local_records.append(name)

        # Get imported modules from the program's uses clause
        if hasattr(self.tree, "uses_clause") and self.tree.uses_clause:
            imported_modules = self.tree.uses_clause.copy()

        return TypeResolutionContext(
            current_module=None,  # We're in the main program
            imported_modules=imported_modules,
            local_classes=local_classes,
            local_enums=local_enums,
            local_records=local_records,
            module_registry=self.module_registry,
        )

    def visit_Decl(self, node: Decl) -> None:
        pass

    def visit_Member(self, node: Member) -> None:
        pass

    def __convert_class_decl_to_object(self, node: ClassDecl) -> "ClassObject":
        """Converts a ClassDecl AST node to a ClassObject instance."""

        # Initialize empty dictionaries for fields and methods
        fields: Dict[str, Any] = {}
        methods: Dict[str, Any] = {}

        # Process fields
        for field in node.fields:
            field_name = field.var_node.value

            # Initialize field with default value based on type
            if field.type_node.token.type == TokenType.BOOLEAN:
                fields[field_name] = BooleanObject(False)
            elif field.type_node.token.type == TokenType.INTEGER:
                fields[field_name] = IntegerObject(0)
            elif field.type_node.token.type == TokenType.REAL:
                fields[field_name] = RealObject(0.0)
            elif field.type_node.token.type == TokenType.STRING:
                fields[field_name] = StringObject("")
            elif field.type_node.token.type == TokenType.ARRAY:
                elements, element_type = self.__initArray(field.type_node)
                fields[field_name] = ArrayObject(elements, element_type)
            elif field.type_node.token.type == TokenType.CLASS:
                fields[field_name] = {}

        # Process methods
        for method in node.methods:
            methods[method.method_name] = method

        # Add constructor if present
        if node.constructor:
            methods["constructor"] = node.constructor

        # Add destructor if present
        if node.destructor:
            methods["destructor"] = node.destructor

        # Create and return ClassObject
        return ClassObject(node.class_name, fields, methods)

    def visit_ClassDecl(self, node: ClassDecl) -> None:
        ar = self.call_stack.peek()
        for field in node.fields:
            self.visit(field)
        for method in node.methods:
            self.visit(method)

        # Convert ClassDecl to ClassObject and store it
        class_object = self.__convert_class_decl_to_object(node)
        ar[node.class_name] = class_object

        raw_class_name = SpiUtil.extraClassName(node.class_name)
        ar.set_meta(raw_class_name, ElementType.CLASS)
        ar.set_is_class(raw_class_name, True)
        pass

    def visit_EnumDecl(self, node: EnumDecl) -> None:
        ar = self.call_stack.peek()
        ar[node.enum_name] = node
        ar.set_meta(node.enum_name, ElementType.ENUM)
        ar.set_is_enum(node.enum_name)
        pass

    def __convert_record_decl_to_object(self, node: RecordDecl) -> "RecordClassObject":
        """Converts a RecordDecl AST node to a RecordObject instance."""

        # Initialize empty dictionary for fields
        fields: Dict[str, Any] = {}

        # Process fields
        for field in node.fields:
            field_name = field.var_node.value

            # Initialize field with default value based on type
            if field.type_node.token.type == TokenType.BOOLEAN:
                fields[field_name] = BooleanObject(False)
            elif field.type_node.token.type == TokenType.INTEGER:
                fields[field_name] = IntegerObject(0)
            elif field.type_node.token.type == TokenType.REAL:
                fields[field_name] = RealObject(0.0)
            elif field.type_node.token.type == TokenType.STRING:
                fields[field_name] = StringObject("")
            elif field.type_node.token.type == TokenType.ARRAY:
                elements, element_type = self.__initArray(field.type_node)
                fields[field_name] = ArrayObject(elements, element_type)
            elif field.type_node.token.type == TokenType.CLASS:
                fields[field_name] = {}
            elif field.type_node.token.type == TokenType.RECORD:
                # Handle nested records
                record_decl: RecordDecl = self.call_stack.peek().get(
                    field.type_node.token.value
                )
                if record_decl:
                    fields[field_name] = self.__convert_record_decl_to_object(
                        record_decl
                    )
                else:
                    fields[field_name] = {}

        # Create and return RecordObject
        return RecordClassObject(record_name=node.record_name, fields=fields)

    def visit_RecordDecl(self, node: RecordDecl) -> None:
        ar = self.call_stack.peek()
        for field in node.fields:
            self.visit(field)
        ar[node.record_name] = self.__convert_record_decl_to_object(node)
        ar.set_meta(node.record_name, ElementType.RECORD_CLASS)
        ar.set_is_record(node.record_name)
        pass

    def visit_ConstructorDecl(self, node: ConstructorDecl) -> None:
        pass

    def visit_Def(self, node: Def) -> None:
        pass

    def visit_MethodDef(self, node: MethodDef):
        pass

    def visit_FieldDecl(self, node: FieldDecl):
        pass

    def visit_MethodDecl(self, node: MethodDecl):
        pass

    def visit_ProcedureDef(self, node: ProcedureDef) -> None:
        pass

    def visit_ConstructorDef(self, node: ConstructorDef) -> None:
        pass

    def visit_FunctionDef(self, node: FunctionDef) -> None:
        pass

    def visit_BinOp(self, node: BinOp) -> Object:
        left = self.visit(node.left)
        right = self.visit(node.right)

        # Ensure we're working with Object instances
        if not isinstance(left, Object):
            if isinstance(left, bool):
                left = BooleanObject(left)
            elif isinstance(left, int):
                left = IntegerObject(left)
            elif isinstance(left, float):
                left = RealObject(left)
            elif isinstance(left, str):
                left = StringObject(left)

        if not isinstance(right, Object):
            if isinstance(right, bool):
                right = BooleanObject(right)
            elif isinstance(right, int):
                right = IntegerObject(right)
            elif isinstance(right, float):
                right = RealObject(right)
            elif isinstance(right, str):
                right = StringObject(right)

        # Logic operators
        if node.op.type == TokenType.AND:
            if isinstance(left, BooleanObject) and isinstance(right, BooleanObject):
                return BooleanObject(left.value and right.value)
            raise OperationNotSupportedError(
                "logical AND", f"{left.type.value} with {right.type.value}"
            )
        elif node.op.type == TokenType.OR:
            if isinstance(left, BooleanObject) and isinstance(right, BooleanObject):
                return BooleanObject(left.value or right.value)
            raise OperationNotSupportedError(
                "logical OR", f"{left.type.value} with {right.type.value}"
            )

        # Arithmetic operators
        if node.op.type == TokenType.PLUS:
            return left.plus(right)
        elif node.op.type == TokenType.MINUS:
            return left.minus(right)
        elif node.op.type == TokenType.MUL:
            return left.multiply(right)
        elif node.op.type == TokenType.INTEGER_DIV:
            if isinstance(left, IntegerObject) and isinstance(right, IntegerObject):
                return IntegerObject(left.value // right.value)
            raise OperationNotSupportedError(
                "integer division", f"{left.type.value} with {right.type.value}"
            )
        elif node.op.type == TokenType.FLOAT_DIV:
            return left.divide(right)

        # Comparison operators
        if node.op.type == TokenType.LT:
            if isinstance(left, (IntegerObject, RealObject)) and isinstance(
                right, (IntegerObject, RealObject)
            ):
                return BooleanObject(left.value < right.value)
            raise OperationNotSupportedError(
                "less than", f"{left.type.value} with {right.type.value}"
            )
        elif node.op.type == TokenType.GT:
            if isinstance(left, (IntegerObject, RealObject)) and isinstance(
                right, (IntegerObject, RealObject)
            ):
                return BooleanObject(left.value > right.value)
            raise OperationNotSupportedError(
                "greater than", f"{left.type.value} with {right.type.value}"
            )
        elif node.op.type == TokenType.EQ:
            return BooleanObject(left == right)
        elif node.op.type == TokenType.NE:
            return BooleanObject(left != right)
        elif node.op.type == TokenType.LE:
            if isinstance(left, (IntegerObject, RealObject)) and isinstance(
                right, (IntegerObject, RealObject)
            ):
                return BooleanObject(left.value <= right.value)
            raise OperationNotSupportedError(
                "less than or equal", f"{left.type.value} with {right.type.value}"
            )
        elif node.op.type == TokenType.GE:
            if isinstance(left, (IntegerObject, RealObject)) and isinstance(
                right, (IntegerObject, RealObject)
            ):
                return BooleanObject(left.value >= right.value)
            raise OperationNotSupportedError(
                "greater than or equal", f"{left.type.value} with {right.type.value}"
            )

        # Unknown operator
        raise UnknownOperatorError(ErrorCode.UNKNOWN_BIN_OP, node.token)

    def visit_Num(self, node: Num) -> Object:
        if isinstance(node.value, int):
            return IntegerObject(node.value)
        elif isinstance(node.value, float):
            return RealObject(node.value)
        else:
            raise TypeError(f"Unexpected number type: {type(node.value)}")

    def visit_String(self, node: String) -> Object:
        return StringObject(node.value)

    def visit_Bool(self, node: Bool) -> Object:
        if node.token.type == TokenType.TRUE:
            return BooleanObject(True)
        elif node.token.type == TokenType.FALSE:
            return BooleanObject(False)
        raise UnknownBooleanError()

    def visit_UnaryOp(self, node: UnaryOp) -> Object:
        operand = self.visit(node.expr)

        # Convert primitive values to Object instances if needed
        if not isinstance(operand, Object):
            if isinstance(operand, bool):
                operand = BooleanObject(operand)
            elif isinstance(operand, int):
                operand = IntegerObject(operand)
            elif isinstance(operand, float):
                operand = RealObject(operand)

        op = node.op.type
        if op == TokenType.NOT:
            if isinstance(operand, BooleanObject):
                return BooleanObject(not operand.value)
            raise OperationNotSupportedError("logical NOT", operand.type.value)
        elif op == TokenType.PLUS:
            if isinstance(operand, (IntegerObject, RealObject)):
                return operand  # Unary plus doesn't change the value
            raise OperationNotSupportedError("unary plus", operand.type.value)
        elif op == TokenType.MINUS:
            if isinstance(operand, IntegerObject):
                return IntegerObject(-operand.value)
            elif isinstance(operand, RealObject):
                return RealObject(-operand.value)
            raise OperationNotSupportedError("unary minus", operand.type.value)
        raise UnknownOperatorError(ErrorCode.UNKNOWN_UNARY_OP, node.token)

    def visit_Compound(self, node: Compound) -> None:
        for child in node.children:
            self.visit(child)

    def visit_ConstAssign(self, node: ConstAssign):
        ar = self.call_stack.peek()
        if node.const_type == ConstType.NON_ARRAY:
            const_name = node.left.value
            value = self.visit(node.right)
            ar[const_name] = value
        elif node.const_type == ConstType.ARRAY:
            const_name = node.left.value
            array_type = cast(ArrayType, node.type)
            element_type = ElementType.INTEGER  # Default

            if array_type.element_type.token.type == TokenType.BOOLEAN:
                element_type = ElementType.BOOL
            elif array_type.element_type.token.type == TokenType.INTEGER:
                element_type = ElementType.INTEGER
            elif array_type.element_type.token.type == TokenType.REAL:
                element_type = ElementType.REAL

            elements: dict[int, Object] = {}
            indices = list(
                range(
                    self.visit(array_type.lower).value,
                    self.visit(array_type.upper).value + 1,
                )
            )
            params = cast(Compound, node.right).children
            for k, v in zip(indices, params):
                elements[k] = self.visit(v)
            ar[const_name] = ArrayObject(elements, element_type)
            ar.set_meta(const_name, ElementType.ARRAY)

    def visit_Assign(self, node: Assign) -> None:
        var_value = self.visit(node.right)
        ar = self.call_stack.peek()

        if isinstance(node.left, IndexVar):
            # array [index] = value
            var_name = node.left.value
            index: int = self.visit(node.left.index)
            if var_name.find(".") != -1:
                record_name, record_key = var_name.split(".")
                record_obj = ar[record_name]
                if isinstance(record_obj, RecordClassObject):
                    array_obj = record_obj.fields[record_key]
                    if isinstance(array_obj, ArrayObject):
                        array_obj[index] = var_value
            else:
                array_obj = ar[var_name]
                if isinstance(array_obj, ArrayObject):
                    array_obj[index] = var_value
        elif isinstance(node.left, RecordVar):
            # user.Age = value
            record_name, record_key = node.left.name, node.left.key
            record_obj = ar[record_name]
            if isinstance(record_obj, (RecordClassObject, InstanceObject)):
                record_obj[record_key] = var_value
        else:
            # identifier = value
            var_name = node.left.value
            if var_name in ar.members_meta:
                meta = ar.get_meta(var_name)
                if meta.type == ElementType.STRING:
                    if isinstance(var_value, StringObject):
                        limit = meta.limit
                        limit = (
                            limit.value if isinstance(limit, IntegerObject) else limit
                        )
                        if limit > 0 and len(var_value.value) > limit:
                            message = f"Warning: String literal has more characters[{len(var_value.value)}] than short string length[{limit}]"
                            SpiUtil.print_w(message=message)
                            ar[var_name] = StringObject(var_value.value[0:limit], limit)
                        else:
                            ar[var_name] = var_value
                    elif isinstance(var_value, str):
                        limit = meta.limit
                        if limit > 0 and len(var_value) > limit:
                            message = f"Warning: String literal has more characters[{len(var_value)}] than short string length[{limit}]"
                            SpiUtil.print_w(message=message)
                            ar[var_name] = StringObject(var_value[0:limit], limit)
                        else:
                            ar[var_name] = StringObject(var_value, limit)
                else:
                    ar[var_name] = var_value
            else:
                ar[var_name] = var_value

    def visit_Var(self, node: Var) -> Any:
        var_name = node.value

        ar = self.call_stack.peek()
        if var_name.find(".") != -1:
            type_name, type_key = var_name.split(".")
            meta = ar.get_meta(type_name)
            if meta.is_enum:
                enum_decl: EnumDecl | None = ar.get(type_name)
                if enum_decl is None:
                    raise NullPointerError()
                index = enum_decl.reversed_entries[type_key]
                return EnumObject(index=index, name=type_key)
            elif meta.is_record_instance:
                # Handle record instance field access
                record_obj = ar.get(type_name)
                if isinstance(record_obj, RecordInstanceObject):
                    return record_obj[type_key]
                else:
                    raise InterpreterError(f"Expected record instance for {type_name}")
            else:
                raise InterpreterError(f"Unknown compound variable type: {var_name}")
        else:
            var_value = ar.get(var_name)
            return var_value

    def visit_RecordVar(self, node: RecordVar) -> Any:
        ar = self.call_stack.peek()
        record_obj = ar.get(node.name)
        return record_obj

    def visit_IndexVar(self, node: IndexVar) -> Any:
        var_name = node.value
        index: int = self.visit(node.index)
        ar = self.call_stack.peek()

        if isinstance(node.left, RecordVar):
            record_name, record_field = node.left.name, node.left.key
            meta = ar.get_meta(record_name)
            record_obj = ar[record_name]

            if isinstance(record_obj, RecordClassObject):
                field_value = record_obj.fields[record_field]

                if isinstance(field_value, StringObject):
                    return field_value[index]
                elif isinstance(field_value, ArrayObject):
                    return field_value[index]

            return None
        else:
            obj = ar.get(var_name)
            if isinstance(obj, StringObject):
                return obj[index]
            elif isinstance(obj, ArrayObject):
                return obj[index]
            return None

    def visit_NoOp(self, node: NoOp) -> None:
        pass

    def visit_WhileStatement(self, node: WhileStatement) -> None:
        while self.visit(node.condition).value is True:
            self.visit(node.block)

    def visit_ForStatement(self, node: ForStatement) -> None:
        ar = self.call_stack.peek()
        var_name = node.initialization.left.value
        self.visit(node.initialization)
        bound_value = self.visit(node.bound)
        var_obj = ar[var_name]

        if isinstance(var_obj, IntegerObject) and isinstance(
            bound_value, IntegerObject
        ):
            while var_obj <= bound_value:
                self.visit(node.block)
                var_obj.value += 1
            if var_obj > bound_value:
                ar[var_name] = bound_value
        else:
            raise TypeError("For loop variables must be integers")

    def visit_ProcedureDecl(self, node: ProcedureDecl) -> None:
        pass

    def visit_ProcedureCall(self, node: ProcedureCall) -> None:
        proc_name = node.proc_name
        proc_symbol = node.proc_symbol

        if proc_symbol is None:
            raise NullPointerError

        ar = ActivationRecord(
            name=proc_name,
            type=ARType.PROCEDURE,
            nesting_level=proc_symbol.scope_level + 1,
        )

        pre_ar = self.call_stack.peek()
        if pre_ar is not None:
            ar.copy_from(pre_ar, False)

        # deal with built-in procedure first
        if isinstance(proc_symbol, BuiltinProcedureSymbol):
            if proc_symbol.name.upper() == NativeMethod.WRITE.name:
                actual_params = node.actual_params

                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                # output actual params
                for argument_node in actual_params:
                    object = self.visit(argument_node)
                    if isinstance(object, EnumObject):
                        print(object.name, end="")
                    else:
                        print(object, end="")

                self.log(f"LEAVE: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return
            elif proc_symbol.name.upper() == NativeMethod.WRITELN.name:
                actual_params = node.actual_params

                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                # output actual params
                for argument_node in actual_params:
                    object = self.visit(argument_node)
                    if isinstance(object, EnumObject):
                        print(object.name, end="")
                    else:
                        print(object, end="")
                print()

                self.log(f"LEAVE: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return
            elif proc_symbol.name.upper() == NativeMethod.READ.name:
                actual_params = node.actual_params
                var_name = actual_params[0].value

                self.call_stack.push(ar)

                self.log(f"ENTER: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                # output actual params
                user_input = input("")
                ar[var_name] = user_input

                self.log(f"LEAVE: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return
            elif proc_symbol.name.upper() == NativeMethod.READLN.name:
                actual_params = node.actual_params
                var_name = actual_params[0].value

                self.call_stack.push(ar)

                self.log(f"ENTER: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                # output actual params
                user_input = input("")
                ar[var_name] = user_input

                self.log(f"LEAVE: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return
            elif proc_symbol.name.upper() == NativeMethod.SETLENGTH.name:
                actual_params = node.actual_params

                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                # core
                arr_name = actual_params[0].value
                new_length = self.visit(actual_params[1])
                element_type = ar.get_meta(arr_name).type
                if element_type == ElementType.STRING:
                    if len(ar[arr_name]) > new_length.value:
                        ar[arr_name] = ar[arr_name][0:new_length]
                else:
                    if ar.get_meta(arr_name).dynamic is False:
                        raise StaticArrayModifyLengthError()

                    new_length = (
                        new_length.value
                        if isinstance(new_length, IntegerObject)
                        else new_length
                    )
                    for i in range(0, new_length):
                        if i in ar[arr_name]:
                            continue
                        if element_type == ElementType.BOOL:
                            ar[arr_name][i] = BooleanObject(value=False)
                        if element_type == ElementType.INTEGER:
                            ar[arr_name][i] = IntegerObject(value=0)
                        if element_type == ElementType.REAL:
                            ar[arr_name][i] = RealObject(value=0.0)
                        if element_type == ElementType.ARRAY:
                            ar[arr_name][i] = ArrayObject(
                                elements={},
                                element_type=ElementType.UNKNOWN,
                                dynamic=True,
                            )

                self.log(f"LEAVE: PROCEDURE {proc_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return
            else:
                raise UnknownBuiltinProcedureError()
        else:
            formal_params = proc_symbol.formal_params
            actual_params = node.actual_params

            for param_symbol, argument_node in zip(formal_params, actual_params):
                ar[param_symbol.name] = self.visit(argument_node)

            self.call_stack.push(ar)

            self.log(f"ENTER: PROCEDURE {proc_name}")
            self.log(str(self.call_stack))

            # evaluate procedure body
            if proc_symbol.block_ast is None:
                raise NullPointerError
            self.visit(proc_symbol.block_ast)

            self.log(f"LEAVE: PROCEDURE {proc_name}")
            self.log(str(self.call_stack))

            self.call_stack.pop()
            pass

    def visit_FunctionDecl(self, node: FunctionDecl) -> None:
        pass

    def visit_IfStatement(self, node: IfStatement) -> None:
        condition_result = self.visit(node.condition)

        if isinstance(condition_result, BooleanObject):
            flag = condition_result.value
        else:
            flag = bool(condition_result)

        if flag is True:
            self.visit(node.then_branch)
            return
        else:
            for branch in node.else_if_branches:
                sub_condition = self.visit(branch)
                if isinstance(sub_condition, BooleanObject):
                    sub_flag = sub_condition.value
                else:
                    sub_flag = bool(sub_condition)

                if sub_flag is True:
                    self.visit(branch.then_branch)
                    return

        if node.else_branch is not None:
            self.visit(node.else_branch)

    def visit_CaseStatement(self, node: CaseStatement) -> None:
        matcher_value = self.visit(node.matcher)

        for condition, branch in node.branches:
            condition_value = self.visit(condition)
            if matcher_value == condition_value:
                self.visit(branch)
                return
            else:
                continue

        if node.else_branch is not None:
            self.visit(node.else_branch)

    def visit_FunctionCall(self, node: FunctionCall) -> Any:
        func_name = node.func_name
        func_symbol = node.func_symbol

        if func_symbol is None:
            raise NullPointerError

        ar = ActivationRecord(
            name=func_name,
            type=ARType.FUNCTION,
            nesting_level=func_symbol.scope_level + 1,
        )

        pre_ar = self.call_stack.peek()
        if pre_ar is not None:
            ar.copy_from(pre_ar, False)

        # deal with built-in function first
        if isinstance(func_symbol, BuiltinFunctionSymbol):
            if func_symbol.name.upper() == NativeMethod.LENGTH.name:
                actual_params = node.actual_params

                # [0] = LENGTH, [1] = ARRAY_NAME
                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                ar[RETURN_NUM] = IntegerObject(value=len(self.visit(actual_params[i])))

                self.log(f"LEAVE: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return ar[RETURN_NUM]
            elif func_symbol.name.upper() == NativeMethod.LOW.name:
                actual_params = node.actual_params

                # Process parameters
                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                # Get the array object
                array_obj = self.visit(actual_params[i])

                # Find the lowest index in the array
                if isinstance(array_obj, ArrayObject) and array_obj.elements:
                    lowest_index = min(array_obj.elements.keys())
                    ar[RETURN_NUM] = IntegerObject(lowest_index)
                else:
                    # Return a default value if array is empty or not an array
                    ar[RETURN_NUM] = IntegerObject(0)

                self.log(f"LEAVE: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return ar[RETURN_NUM]
            elif func_symbol.name.upper() == NativeMethod.HIGH.name:
                actual_params = node.actual_params

                # Process parameters
                for i in range(0, len(actual_params)):
                    ar[i] = self.visit(actual_params[i])

                self.call_stack.push(ar)

                self.log(f"ENTER: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                # Get the array object
                array_obj = self.visit(actual_params[i])

                # Find the highest index in the array
                if isinstance(array_obj, ArrayObject) and array_obj.elements:
                    highest_index = max(array_obj.elements.keys())
                    ar[RETURN_NUM] = IntegerObject(highest_index)
                else:
                    # Return a default value if array is empty or not an array
                    ar[RETURN_NUM] = IntegerObject(0)

                self.log(f"LEAVE: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return ar[RETURN_NUM]
            elif func_symbol.name.upper() == NativeMethod.ORD.name:
                actual_params = node.actual_params

                self.call_stack.push(ar)

                self.log(f"ENTER: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                ar[RETURN_NUM] = IntegerObject(value=self.visit(actual_params[1]).index)

                self.log(f"LEAVE: FUNCTION {func_name}")
                self.log(str(self.call_stack))

                self.call_stack.pop()
                return ar[RETURN_NUM]
            else:
                raise UnknownBuiltinFunctionError()
        else:
            formal_params = func_symbol.formal_params
            actual_params = node.actual_params

            for param_symbol, argument_node in zip(formal_params, actual_params):
                ar[param_symbol.name] = self.visit(argument_node)

            self.call_stack.push(ar)

            self.log(f"ENTER: FUNCTION {func_name}")
            self.log(str(self.call_stack))

            # evaluate procedure body
            if func_symbol.block_ast is None:
                raise NullPointerError
            self.visit(func_symbol.block_ast)

            self.log(f"LEAVE: FUNCTION {func_name}")
            self.log(str(self.call_stack))

            self.call_stack.pop()

            return ar[func_name]

    def visit_MethodCall(self, node: MethodCall) -> Any:
        method_full_name = node.method_full_name
        class_or_inst_name, method_name = method_full_name.split(".")
        method_symbol = node.method_symbol

        if method_symbol is None:
            raise UnknownMethodSymbolError

        ar = ActivationRecord(
            name=method_full_name,
            type=ARType.METHOD,
            nesting_level=method_symbol.scope_level + 1,
        )
        instance = None
        pre_ar = self.call_stack.peek()
        if pre_ar is not None:
            ar.copy_from(pre_ar, False)
            meta = pre_ar.get_meta(class_or_inst_name)
            if meta.is_instance:
                inst_name = class_or_inst_name
                instance = pre_ar.get(inst_name)
                # Copy fields from instance object to activation record
                if isinstance(instance, InstanceObject):
                    for k, v in instance.fields.items():
                        ar[k] = v
                else:
                    raise TypeError(f"Expected InstanceObject but got {type(instance)}")
            elif meta.is_class:
                class_name = class_or_inst_name
                class_decl: ClassDecl = pre_ar.get(SpiUtil.toClassName(class_name))
                fields = class_decl.fields.copy()
                instance = InstanceObject(class_decl.class_name, fields)
            else:
                raise UnknownMethodSymbolError()

        formal_params = method_symbol.formal_params
        actual_params = node.actual_params

        method_type = method_symbol.method_type
        if method_type == MethodType.PROCEDURE:
            for param_symbol, argument_node in zip(formal_params, actual_params):
                ar[param_symbol.name] = self.visit(argument_node)
        else:
            for param_symbol, argument_node in zip(formal_params, actual_params[1:]):
                ar[param_symbol.name] = self.visit(argument_node)

        self.call_stack.push(ar)

        self.log(f"ENTER: METHOD {method_full_name}")
        self.log(str(self.call_stack))

        # evaluate procedure body
        if method_symbol.block_ast is None:
            raise NullPointerError
        self.visit(method_symbol.block_ast)

        self.log(f"LEAVE: METHOD {method_full_name}")
        self.log(str(self.call_stack))

        # before leave method scope, update the instance fields with values from AR
        if isinstance(instance, InstanceObject):
            for k in instance.fields.keys():
                if k in ar.members:
                    instance.fields[k] = ar[k]

        self.call_stack.pop()

        method_type = method_symbol.method_type
        if method_type == MethodType.CONSTRUCTOR:
            # T.Create; BEGIN a:=?;b:=?; END; will not return any explicit value, but should generate an instance value for assignment
            return instance
        elif method_type == MethodType.FUNCTION:
            # T.m; BEGIN m := ? END; should assign to m as return value
            return ar[method_name]
        else:
            return None

    def visit_ExprGet(self, node: ExprGet) -> Any:
        # First visit the base object
        result = self.visit(node.object)

        # Process each GetItem in sequence
        for get_item in node.gets:
            if get_item.is_property:
                # Property access (like obj.property)
                if isinstance(result, (RecordInstanceObject, InstanceObject)):
                    result = result[get_item.key]
                else:
                    ar = self.call_stack.peek()
                    meta = ar.get_meta(result)
                    if meta.is_record_instance or meta.is_instance:
                        record_obj = ar[result]
                        if isinstance(
                            record_obj, (RecordInstanceObject, InstanceObject)
                        ):
                            result = record_obj[get_item.key]
                    else:
                        raise InterpreterError(
                            f"Cannot access property {get_item.key} on non-object value"
                        )
            else:
                # Index access (like arr[idx])
                index = self.visit(get_item.key)

                if isinstance(result, StringObject):
                    # String indexing (1-based in Pascal)
                    if isinstance(index, IntegerObject):
                        idx_val = index.value
                    else:
                        idx_val = index

                    result = result[idx_val]
                elif isinstance(result, ArrayObject):
                    # Array indexing
                    if isinstance(index, IntegerObject):
                        idx_val = index.value
                    else:
                        idx_val = index

                    result = result[idx_val]
                else:
                    raise InterpreterError(
                        "Cannot use index access on non-indexable value"
                    )

        return result

    def visit_GetItem(self, node: GetItem) -> Any:
        # This is mainly used by ExprGet and not visited directly
        if node.is_property:
            return node.key
        else:
            return self.visit(node.key)

    def visit_ExprSet(self, node: ExprSet) -> None:
        # Get the value to assign
        value = self.visit(node.value)

        # Get the base object and all but the last GetItem
        gets = node.expr_get.gets

        if not gets:
            # Check if the base object is a compound variable (like "exampleUser.ID")
            if isinstance(node.expr_get.object, Var) and "." in node.expr_get.object.value:
                # Handle compound variable assignment
                var_name = node.expr_get.object.value
                type_name, type_key = var_name.split(".", 1)
                ar = self.call_stack.peek()
                record_obj = ar.get(type_name)
                if isinstance(record_obj, RecordInstanceObject):
                    record_obj[type_key] = value
                    return
                else:
                    raise InterpreterError(f"Expected record instance for {type_name}")
            
            # No property/index access, just assign to the variable
            object_value = self.visit(node.expr_get.object)
            ar = self.call_stack.peek()
            ar[object_value] = value
            return

        # Navigate to the parent object that will have its property/index modified
        object_value = self.visit(node.expr_get.object)
        current_obj = object_value

        # Process all but the last GetItem to find the parent object
        for i in range(len(gets) - 1):
            get_item = gets[i]

            if get_item.is_property:
                # Property access
                if isinstance(current_obj, (RecordInstanceObject, InstanceObject)):
                    current_obj = current_obj[get_item.key]
                else:
                    ar = self.call_stack.peek()
                    meta = ar.get_meta(current_obj)
                    if meta.is_record_instance or meta.is_instance:
                        record_obj = ar[current_obj]
                        if isinstance(
                            record_obj, (RecordInstanceObject, InstanceObject)
                        ):
                            current_obj = record_obj[get_item.key]
                    else:
                        raise InterpreterError(
                            f"Cannot access property {get_item.key} on non-object value"
                        )
            else:
                # Index access
                index = self.visit(get_item.key)
                if isinstance(index, IntegerObject):
                    idx_val = index.value
                else:
                    idx_val = index

                if isinstance(current_obj, ArrayObject):
                    current_obj = current_obj[idx_val]
                else:
                    ar = self.call_stack.peek()
                    array_obj = ar[current_obj]
                    if isinstance(array_obj, ArrayObject):
                        current_obj = array_obj[idx_val]

        # Handle the final GetItem (the actual assignment target)
        last_get = gets[-1]
        ar = self.call_stack.peek()

        if last_get.is_property:
            # Property assignment
            if isinstance(current_obj, (RecordInstanceObject, InstanceObject)):
                current_obj[last_get.key] = value
            else:
                meta = ar.get_meta(current_obj)
                if meta.is_record_instance or meta.is_instance:
                    record_obj = ar[current_obj]
                    if isinstance(record_obj, (RecordInstanceObject, InstanceObject)):
                        record_obj[last_get.key] = value
                else:
                    # For simple variables that are mapped as member names
                    ar[current_obj] = value
        else:
            # Index assignment
            index = self.visit(last_get.key)
            if isinstance(index, IntegerObject):
                idx_val = index.value
            else:
                idx_val = index

            if isinstance(current_obj, ArrayObject):
                current_obj[idx_val] = copy.copy(value)
            elif isinstance(current_obj, StringObject):
                # Cannot modify string by index in most languages
                raise InterpreterError("Cannot modify string by index")
            else:
                # For variables in the activation record that are arrays
                array_obj = ar[current_obj]
                if isinstance(array_obj, ArrayObject):
                    array_obj[idx_val] = value

    def interpret(self):
        tree = self.tree
        if tree is None:
            return ""
        return self.visit(tree)
