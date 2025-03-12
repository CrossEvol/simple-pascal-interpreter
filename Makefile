runs:
	@python spi.py ${file} --stack --scope

run:
	@python spi.py ${file} 

main:
	@python main.py

test:
	python test_interpreter.py

dot:
	@python gen_ast_dot.py ${file} >> temp/ast_$(shell date +%Y-%m-%d_%H-%M-%S).dot
	@dot -Tpng -o temp/ast_$(shell date +%Y-%m-%d_%H-%M-%S).png temp/ast_$(shell date +%Y-%m-%d_%H-%M-%S).dot

type:
	@python auto_type.py ${file}