runs:
	@python spi.py ${file} --stack --scope

run:
	@python spi.py ${file} 

main:
	@python main.py

e2e:
	@echo "---------------> for integration testcases <---------------"
	python integration_test.py

test:
	python test_interpreter.py 
	@echo "---------------> for symbol visibility testcases <---------------"
	python test_symbol_visibility.py
	@echo "---------------> for visibility integration testcases <---------------"
	python test_visibility_integration.py

dot:
	@python gen_ast_dot.py ${file} >> temp/ast_$(shell date +%Y-%m-%d_%H-%M-%S).dot
	@dot -Tpng -o temp/ast_$(shell date +%Y-%m-%d_%H-%M-%S).png temp/ast_$(shell date +%Y-%m-%d_%H-%M-%S).dot

type:
	@python auto_type.py ${file}