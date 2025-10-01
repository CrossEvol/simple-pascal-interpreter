# Simple Pascal Interpreter Makefile

# 默认目标
.PHONY: help
help:
	@echo "Simple Pascal Interpreter - Available targets:"
	@echo "  run FILE=<file>     - Run Pascal interpreter with stack and scope logging"
	@echo "  interpret FILE=<file> - Run Pascal interpreter normally"
	@echo "  main                - Run main entry point"
	@echo "  test                - Run interpreter tests"
	@echo "  dot FILE=<file>     - Generate AST visualization"
	@echo "  type FILE=<file>    - Run auto type annotation"
	@echo "  clean               - Clean generated files"
	@echo "  help                - Show this help message"
	@echo "  build 			     - Build onefile executable"

# 变量定义
PYTHON := python
UV_PYTHON := uv run python 
UV_PYTEST := $(UV_PYTHON) -m pytest
DOT := dot


# 运行解释器，带堆栈和作用域日志
.PHONY: main
main:
	@if [ -z "$(file)" ]; then \
		echo "Error: Please specify a Pascal file with file=<filename>"; \
		exit 1; \
	fi
	@$(PYTHON) main.py $(file) --stack --scope

# 简单运行解释器
.PHONY: run
run:
	@if [ -z "$(file)" ]; then \
		echo "Error: Please specify a Pascal file with file=<filename>"; \
		exit 1; \
	fi
	@$(PYTHON) main.py $(file)


# 运行测试
.PHONY: build
build: 
	pyinstaller --onefile main.py

# 运行测试
.PHONY: test
test: 
	$(UV_PYTEST) tests/* -v


# Run only lexer tests
.PHONY: test_lexer 
test_lexer:
	$(UV_PYTEST) tests/test_lexer.py -v

# Run only lexer tests
.PHONY: test_parser
test_parser:
	$(UV_PYTEST) tests/test_parser.py -v

# Run only lexer tests
.PHONY: test_semantic
test_semantic:
	$(UV_PYTEST) tests/test_semantic_analyzer.py -v

# Run only object tests
.PHONY: test_object
test_object:
	$(UV_PYTEST) tests/test_object.py -v


# Run only interpreter tests  
.PHONY: test_interpreter
test_interpreter:
	$(UV_PYTEST) tests/test_interpreter.py -v

# Run integration tests by fpc
.PHONY: test_fpc
test_fpc:
	$(PYTHON) test_pascal_examples_by_fpc.py

# Run integration tests by spi
.PHONY: test_spi
test_spi:
	$(PYTHON) test_pascal_examples_by_spi.py -v

# 生成AST可视化
.PHONY: dot
dot:
	@if [ -z "$(file)" ]; then \
		echo "Error: Please specify a Pascal file with file=<filename>"; \
		exit 1; \
	fi
	@$(PYTHON) scripts/gen_ast_dot.py $(file) > ast.dot
	@echo "AST DOT file generated: ast.dot"
	@if command -v $(DOT) >/dev/null 2>&1; then \
		$(DOT) -Tpng -o ast.png ast.dot && echo "AST visualization generated: ast.png"; \
	else \
		echo "Graphviz not installed. Install it to generate PNG: https://graphviz.org/download/"; \
	fi

# 自动类型注解
.PHONY: type
type:
	@if [ -z "$(FILE)" ]; then \
		echo "Error: Please specify a Python file with FILE=<filename>"; \
		exit 1; \
	fi
	@$(PYTHON) scripts/auto_type.py $(FILE)

# 清理生成的文件
.PHONY: clean
clean:
	@rm -f ast.dot ast.png
	@rm -f dist/main.exe
	@echo "Cleaned generated files"


