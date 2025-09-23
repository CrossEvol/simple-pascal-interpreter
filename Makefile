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
	@$(UV_PYTHON) main.py $(file) --stack --scope

# 简单运行解释器
.PHONY: run
run:
	@if [ -z "$(file)" ]; then \
		echo "Error: Please specify a Pascal file with file=<filename>"; \
		exit 1; \
	fi
	@$(PYTHON) main.py $(file)


# 运行测试
.PHONY: test
test: 
	$(UV_PYTEST) tests/* -v

# Run only mutability tests  
.PHONY: test_mutability
test_mutability:
	$(UV_PYTEST) tests/test_mutability_validation.py -v


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

# Run only interpreter tests  
.PHONY: test_interpreter
test_interpreter:
	$(UV_PYTEST) tests/test_interpreter.py -v


# 生成AST可视化
.PHONY: dot
dot:
	@if [ -z "$(FILE)" ]; then \
		echo "Error: Please specify a Pascal file with FILE=<filename>"; \
		exit 1; \
	fi
	@$(PYTHON) gen_ast_dot.py $(FILE) >> ast.dot
	@$(DOT) -Tpng -o ast.png ast.dot
	@echo "AST visualization generated: ast.png"

# 自动类型注解
.PHONY: type
type:
	@if [ -z "$(FILE)" ]; then \
		echo "Error: Please specify a Python file with FILE=<filename>"; \
		exit 1; \
	fi
	@$(PYTHON) auto_type.py $(FILE)

# 清理生成的文件
.PHONY: clean
clean:
	@rm -f ast.dot ast.png
	@echo "Cleaned generated files"

# 示例运行目标（使用项目中的示例文件）
.PHONY: example-basic
example-basic:
	@$(PYTHON) spi.py pas/basic_data_types.pas

.PHONY: example-function
example-function:
	@$(PYTHON) spi.py pas/function.pas

.PHONY: example-procedure
example-procedure:
	@$(PYTHON) spi.py pas/procedure.pas

