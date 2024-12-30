run:
	python spi.py ${file} --stack --scope

test:
	python test_interpreter.py

dot:
	python gen_ast_dot.py ${file} >> ast.dot
	dot -Tpng -o ast.png ast.dot

type:
	python auto_type.py ${file}