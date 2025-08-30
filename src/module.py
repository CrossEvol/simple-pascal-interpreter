"""
Module system implementation for Pascal interpreter.

This module provides the core data structures for the Pascal module system,
including Module, Unit, and ModuleRegistry classes.
"""

from __future__ import annotations
from typing import Dict, List, Optional
from src.spi_ast import Block
from src.sematic_analyzer import ScopedSymbolTable
from src.symbol import Symbol
from src.visibility import VisibilityLevel


class ModuleSymbolTable(ScopedSymbolTable):
    """
    Enhanced symbol table with module-aware symbol resolution.
    
    This class extends ScopedSymbolTable to support:
    - Tracking imported modules and their symbols
    - Cross-module symbol resolution
    - Module-qualified symbol lookup
    """
    
    def __init__(self, scope_name: str, scope_level: int, 
                 enclosing_scope: ScopedSymbolTable | None = None, 
                 module_name: str | None = None) -> None:
        super().__init__(scope_name, scope_level, enclosing_scope)
        self.module_name = module_name
        self.imported_modules: Dict[str, ScopedSymbolTable] = {}
    
    def import_module_symbols(self, module_name: str, symbol_table: ScopedSymbolTable) -> None:
        """
        Import interface symbols from another module.
        Only symbols with INTERFACE visibility are imported.
        
        Args:
            module_name: Name of the module being imported
            symbol_table: The interface symbol table of the module to import
        """
        # Create a filtered symbol table containing only interface symbols
        filtered_table = ScopedSymbolTable(
            scope_name=f"{module_name}_interface_filtered",
            scope_level=symbol_table.scope_level,
            enclosing_scope=None
        )
        
        # Only import symbols with INTERFACE visibility
        for symbol_name, symbol in symbol_table._symbols.items():
            if hasattr(symbol, 'visibility') and symbol.visibility == VisibilityLevel.INTERFACE:
                filtered_table.insert(symbol)
        
        self.imported_modules[module_name] = filtered_table
    
    def lookup_with_modules(self, name: str) -> Symbol | None:
        """
        Lookup a symbol with module-aware resolution.
        
        Resolution order:
        1. Current scope and enclosing scopes (standard lookup)
        2. Imported modules (in import order)
        
        Args:
            name: Symbol name to lookup
            
        Returns:
            Symbol if found, None otherwise
        """
        # First try standard lookup in current scope and enclosing scopes
        symbol = self.lookup(name)
        if symbol is not None:
            return symbol
        
        # Then search in imported modules
        for module_name, module_symbols in self.imported_modules.items():
            symbol = module_symbols.lookup(name, current_scope_only=True)
            if symbol is not None:
                return symbol
        
        return None
    
    def resolve_qualified_name(self, module_name: str, symbol_name: str) -> Symbol | None:
        """
        Resolve a module-qualified symbol name (e.g., Math.ADD).
        
        Args:
            module_name: Name of the module
            symbol_name: Name of the symbol within the module
            
        Returns:
            Symbol if found, None otherwise
        """
        if module_name in self.imported_modules:
            return self.imported_modules[module_name].lookup(symbol_name, current_scope_only=True)
        return None
    
    def get_imported_modules(self) -> List[str]:
        """
        Get list of imported module names.
        
        Returns:
            List of module names that have been imported
        """
        return list(self.imported_modules.keys())
    
    def has_imported_module(self, module_name: str) -> bool:
        """
        Check if a module has been imported.
        
        Args:
            module_name: Name of the module to check
            
        Returns:
            True if module is imported, False otherwise
        """
        return module_name in self.imported_modules
    
    def lookup_with_visibility(self, name: str, requesting_module: str | None = None) -> Symbol | None:
        """
        Lookup a symbol with visibility enforcement.
        
        Args:
            name: Symbol name to lookup
            requesting_module: Name of the module making the request (None for current module)
            
        Returns:
            Symbol if found and accessible, None otherwise
        """
        # First try standard lookup in current scope and enclosing scopes
        symbol = self.lookup(name)
        if symbol is not None:
            # If we're in the same module, all symbols are accessible
            if requesting_module is None or requesting_module == self.module_name:
                return symbol
            # If requesting from different module, check visibility
            if hasattr(symbol, 'visibility') and symbol.visibility == VisibilityLevel.INTERFACE:
                return symbol
            # Implementation and private symbols are not accessible from outside
            return None
        
        # Then search in imported modules (these are already filtered to interface symbols)
        for module_name, module_symbols in self.imported_modules.items():
            symbol = module_symbols.lookup(name, current_scope_only=True)
            if symbol is not None:
                return symbol
        
        return None
    
    def insert_with_visibility(self, symbol: Symbol, visibility: VisibilityLevel) -> None:
        """
        Insert a symbol with explicit visibility level.
        
        Args:
            symbol: The symbol to insert
            visibility: The visibility level for the symbol
        """
        if hasattr(symbol, 'visibility'):
            symbol.visibility = visibility
        self.insert(symbol)
    
    def get_interface_symbols(self) -> Dict[str, Symbol]:
        """
        Get all symbols that are visible in the interface (public symbols).
        
        Returns:
            Dictionary of symbol names to symbols that have INTERFACE visibility
        """
        interface_symbols = {}
        for name, symbol in self._symbols.items():
            if hasattr(symbol, 'visibility') and symbol.visibility == VisibilityLevel.INTERFACE:
                interface_symbols[name] = symbol
        return interface_symbols
    
    def get_implementation_symbols(self) -> Dict[str, Symbol]:
        """
        Get all symbols that are implementation-only (private to the module).
        
        Returns:
            Dictionary of symbol names to symbols that have IMPLEMENTATION or PRIVATE visibility
        """
        impl_symbols = {}
        for name, symbol in self._symbols.items():
            if hasattr(symbol, 'visibility') and symbol.visibility in [VisibilityLevel.IMPLEMENTATION, VisibilityLevel.PRIVATE]:
                impl_symbols[name] = symbol
        return impl_symbols
    
    def set_current_section_visibility(self, visibility: VisibilityLevel) -> None:
        """
        Set the visibility level for symbols being added to this scope.
        This is used during parsing to mark symbols as interface or implementation.
        
        Args:
            visibility: The visibility level to use for new symbols
        """
        self._current_section_visibility = visibility
    
    def get_current_section_visibility(self) -> VisibilityLevel:
        """
        Get the current section visibility level.
        
        Returns:
            The current visibility level for new symbols
        """
        return getattr(self, '_current_section_visibility', VisibilityLevel.PRIVATE)
    
    def insert(self, symbol: Symbol) -> None:
        """
        Override insert to automatically set visibility based on current section.
        
        Args:
            symbol: The symbol to insert
        """
        if hasattr(symbol, 'visibility'):
            # If symbol doesn't have explicit visibility, use current section visibility
            if symbol.visibility == VisibilityLevel.PRIVATE:
                symbol.visibility = self.get_current_section_visibility()
        super().insert(symbol)
    
    def __str__(self) -> str:
        base_str = super().__str__()
        if self.imported_modules:
            imported_str = f"Imported modules: {list(self.imported_modules.keys())}"
            return base_str + "\n" + imported_str
        return base_str


class Module:
    """
    Base class representing a Pascal module with interface and implementation symbols.
    
    A module contains:
    - name: The module name
    - file_path: Path to the module file
    - interface_symbols: Symbol table for publicly accessible symbols
    - implementation_symbols: Symbol table for private implementation symbols
    - dependencies: List of module names this module depends on
    - is_loaded: Flag indicating if the module has been loaded
    """
    
    def __init__(self, name: str, file_path: str) -> None:
        self.name = name
        self.file_path = file_path
        self.interface_symbols = ModuleSymbolTable(
            scope_name=f"{name}_interface",
            scope_level=1,
            enclosing_scope=None,
            module_name=name
        )
        self.implementation_symbols = ModuleSymbolTable(
            scope_name=f"{name}_implementation", 
            scope_level=2,
            enclosing_scope=self.interface_symbols,
            module_name=name
        )
        self.dependencies: List[str] = []
        self.is_loaded = False
    
    def __str__(self) -> str:
        return f"<Module(name='{self.name}', file_path='{self.file_path}', loaded={self.is_loaded})>"
    
    __repr__ = __str__


class Unit(Module):
    """
    Pascal unit extending Module with AST storage for interface and implementation sections.
    
    A unit contains all Module functionality plus:
    - interface_ast: AST for the interface section
    - implementation_ast: AST for the implementation section
    """
    
    def __init__(self, name: str, file_path: str) -> None:
        super().__init__(name, file_path)
        self.interface_ast: Optional[Block] = None
        self.implementation_ast: Optional[Block] = None
    
    def __str__(self) -> str:
        return f"<Unit(name='{self.name}', file_path='{self.file_path}', loaded={self.is_loaded})>"
    
    __repr__ = __str__


class ModuleRegistry:
    """
    Central registry for managing loaded modules and their dependencies.
    
    The registry provides:
    - loaded_modules: Dictionary of loaded modules by name
    - dependency_graph: Dictionary tracking module dependencies
    - search_paths: List of directories to search for module files
    """
    
    def __init__(self) -> None:
        self.loaded_modules: Dict[str, Module] = {}
        self.dependency_graph: Dict[str, List[str]] = {}
        self.search_paths: List[str] = [".", "./stdlib"]
    
    def load_module(self, name: str, file_path: Optional[str] = None) -> Module:
        """
        Load a module by name, optionally specifying the file path.
        
        Args:
            name: The module name to load
            file_path: Optional explicit file path, if None will search in search_paths
            
        Returns:
            The loaded Module instance
            
        Raises:
            ModuleNotFoundError: If the module file cannot be found
        """
        if name in self.loaded_modules:
            return self.loaded_modules[name]
        
        if file_path is None:
            file_path = self.find_module_file(name)
        
        # Create a Unit instance (most modules will be units)
        module = Unit(name, file_path)
        self.loaded_modules[name] = module
        self.dependency_graph[name] = []
        
        return module
    
    def find_module_file(self, name: str) -> str:
        """
        Find the file path for a module by searching in search_paths.
        
        Args:
            name: The module name to find
            
        Returns:
            The full file path to the module
            
        Raises:
            ModuleNotFoundError: If the module file cannot be found
        """
        import os
        
        for search_path in self.search_paths:
            file_path = os.path.join(search_path, f"{name}.pas")
            if os.path.exists(file_path):
                return file_path
        
        # If not found, raise an error
        from src.error import ModuleNotFoundError
        raise ModuleNotFoundError(name, self.search_paths)
    
    def get_module(self, name: str) -> Optional[Module]:
        """
        Get a loaded module by name.
        
        Args:
            name: The module name
            
        Returns:
            The Module instance if loaded, None otherwise
        """
        return self.loaded_modules.get(name)
    
    def resolve_dependencies(self, module_name: str) -> List[str]:
        """
        Resolve the dependency order for a module using topological sort.
        
        This method performs a depth-first search to determine the correct loading
        order for modules, ensuring that dependencies are loaded before the modules
        that depend on them.
        
        Args:
            module_name: The module to resolve dependencies for
            
        Returns:
            List of module names in dependency order (dependencies first)
            
        Raises:
            CircularDependencyError: If circular dependencies are detected
        """
        visited = set()
        temp_visited = set()
        result = []
        dependency_path = []
        
        def visit(name: str) -> None:
            if name in temp_visited:
                # Circular dependency detected - build the cycle path
                cycle_start = dependency_path.index(name)
                cycle = dependency_path[cycle_start:] + [name]
                from src.error import CircularDependencyError
                raise CircularDependencyError(cycle)
            
            if name in visited:
                return
            
            temp_visited.add(name)
            dependency_path.append(name)
            
            # Visit dependencies first (depth-first traversal)
            for dep in self.dependency_graph.get(name, []):
                visit(dep)
            
            dependency_path.pop()
            temp_visited.remove(name)
            visited.add(name)
            result.append(name)
        
        visit(module_name)
        return result
    
    def check_circular_dependencies(self, module_name: str) -> bool:
        """
        Check if loading a module would create circular dependencies.
        
        Args:
            module_name: The module name to check
            
        Returns:
            True if circular dependencies exist, False otherwise
        """
        try:
            self.resolve_dependencies(module_name)
            return False
        except Exception:
            return True
    
    def add_dependency(self, module_name: str, dependency: str) -> None:
        """
        Add a dependency relationship between modules.
        
        Args:
            module_name: The module that depends on another
            dependency: The module that is depended upon
        """
        if module_name not in self.dependency_graph:
            self.dependency_graph[module_name] = []
        
        if dependency not in self.dependency_graph[module_name]:
            self.dependency_graph[module_name].append(dependency)
    
    def get_all_dependencies(self, module_name: str) -> List[str]:
        """
        Get all transitive dependencies for a module (not just direct dependencies).
        
        Args:
            module_name: The module to get all dependencies for
            
        Returns:
            List of all module names that this module depends on (transitively)
        """
        all_deps = set()
        visited = set()
        
        def collect_deps(name: str) -> None:
            if name in visited:
                return
            visited.add(name)
            
            for dep in self.dependency_graph.get(name, []):
                all_deps.add(dep)
                collect_deps(dep)
        
        collect_deps(module_name)
        return list(all_deps)
    
    def get_load_order_for_all_modules(self) -> List[str]:
        """
        Get the load order for all modules in the registry using topological sort.
        
        Returns:
            List of all module names in dependency order (dependencies first)
            
        Raises:
            CircularDependencyError: If circular dependencies are detected
        """
        visited = set()
        temp_visited = set()
        result = []
        dependency_path = []
        
        def visit(name: str) -> None:
            if name in temp_visited:
                # Circular dependency detected - build the cycle path
                cycle_start = dependency_path.index(name)
                cycle = dependency_path[cycle_start:] + [name]
                from src.error import CircularDependencyError
                raise CircularDependencyError(cycle)
            
            if name in visited:
                return
            
            temp_visited.add(name)
            dependency_path.append(name)
            
            # Visit dependencies first
            for dep in self.dependency_graph.get(name, []):
                visit(dep)
            
            dependency_path.pop()
            temp_visited.remove(name)
            visited.add(name)
            result.append(name)
        
        # Visit all modules in the dependency graph
        for module_name in self.dependency_graph.keys():
            if module_name not in visited:
                visit(module_name)
        
        return result
    
    def has_dependency(self, module_name: str, dependency: str) -> bool:
        """
        Check if a module has a specific dependency (direct or transitive).
        
        Args:
            module_name: The module to check
            dependency: The dependency to look for
            
        Returns:
            True if the module depends on the specified dependency
        """
        return dependency in self.get_all_dependencies(module_name)
    
    def get_dependents(self, module_name: str) -> List[str]:
        """
        Get all modules that depend on the specified module.
        
        Args:
            module_name: The module to find dependents for
            
        Returns:
            List of module names that depend on the specified module
        """
        dependents = []
        for mod_name, deps in self.dependency_graph.items():
            if module_name in deps:
                dependents.append(mod_name)
        return dependents
    
    def __str__(self) -> str:
        return f"<ModuleRegistry(loaded={len(self.loaded_modules)}, paths={self.search_paths})>"
    
    __repr__ = __str__