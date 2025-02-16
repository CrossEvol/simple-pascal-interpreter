###############################################################################
#  AST visualizer - generates a DOT file for Graphviz.                        #
#                                                                             #
#  To generate an image from the DOT file run $ dot -Tpng -o ast.png ast.dot  #
#                                                                             #
###############################################################################
import argparse
import textwrap

from src.lexer import Lexer
from src.spi_ast import *
from src.parser import Parser
from src.visitor import NodeVisitor


class ASTVisualizer(NodeVisitor):
    def __init__(self, parser: Parser) -> None:
        self.parser = parser
        self.n_count = 1
        self.dot_header = [
            textwrap.dedent(
                """\
        digraph astgraph {
          node [shape=circle, fontsize=12, fontname="Courier", height=.1];
          ranksep=.3;
          edge [arrowsize=.5]

        """
            )
        ]
        self.dot_body: list[str] = []
        self.dot_footer = ["}"]

    def visit_Program(self, node: Program) -> None:
        s = '  node{} [label="Program"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.block)

        s = "  node{} -> node{}\n".format(node._num, node.block._num)
        self.dot_body.append(s)

    def visit_Block(self, node: Block) -> None:
        s = '  node{} [label="Block"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

        for decl_node in node.declarations:
            s = "  node{} -> node{}\n".format(node._num, decl_node._num)
            self.dot_body.append(s)

        s = "  node{} -> node{}\n".format(node._num, node.compound_statement._num)
        self.dot_body.append(s)

    def visit_VarDecl(self, node: VarDecl) -> None:
        s = '  node{} [label="VarDecl"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.var_node)
        s = "  node{} -> node{}\n".format(node._num, node.var_node._num)
        self.dot_body.append(s)

        self.visit(node.type_node)
        s = "  node{} -> node{}\n".format(node._num, node.type_node._num)
        self.dot_body.append(s)

    def visit_ProcedureDecl(self, node: ProcedureDecl) -> None:
        s = '  node{} [label="ProcDecl:{}"]\n'.format(self.n_count, node.proc_name)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for param_node in node.formal_params:
            self.visit(param_node)
            s = "  node{} -> node{}\n".format(node._num, param_node._num)
            self.dot_body.append(s)

        self.visit(node.block_node)
        s = "  node{} -> node{}\n".format(node._num, node.block_node._num)
        self.dot_body.append(s)

    def visit_Param(self, node: Param) -> None:
        s = '  node{} [label="Param"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.var_node)
        s = "  node{} -> node{}\n".format(node._num, node.var_node._num)
        self.dot_body.append(s)

        self.visit(node.type_node)
        s = "  node{} -> node{}\n".format(node._num, node.type_node._num)
        self.dot_body.append(s)

    def visit_Type(self, node: Type) -> None:
        s = '  node{} [label="{}"]\n'.format(self.n_count, node.token.value)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_Num(self, node: Num) -> None:
        s = '  node{} [label="{}"]\n'.format(self.n_count, node.token.value)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_BinOp(self, node: BinOp) -> None:
        s = '  node{} [label="{}"]\n'.format(self.n_count, node.op.value)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.left)
        self.visit(node.right)

        for child_node in (node.left, node.right):
            s = "  node{} -> node{}\n".format(node._num, child_node._num)
            self.dot_body.append(s)

    def visit_UnaryOp(self, node: UnaryOp) -> None:
        s = '  node{} [label="unary {}"]\n'.format(self.n_count, node.op.value)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.expr)
        s = "  node{} -> node{}\n".format(node._num, node.expr._num)
        self.dot_body.append(s)

    def visit_Compound(self, node: Compound) -> None:
        s = '  node{} [label="Compound"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for child in node.children:
            self.visit(child)
            s = "  node{} -> node{}\n".format(node._num, child._num)
            self.dot_body.append(s)

    def visit_Assign(self, node: Assign) -> None:
        s = '  node{} [label="{}"]\n'.format(self.n_count, node.op.value)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.left)
        self.visit(node.right)

        for child_node in (node.left, node.right):
            s = "  node{} -> node{}\n".format(node._num, child_node._num)
            self.dot_body.append(s)

    def visit_Var(self, node: Var) -> None:
        s = '  node{} [label="{}"]\n'.format(self.n_count, node.value)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_NoOp(self, node: NoOp) -> None:
        s = '  node{} [label="NoOp"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_ProcedureCall(self, node: ProcedureCall) -> None:
        s = '  node{} [label="ProcCall:{}"]\n'.format(self.n_count, node.proc_name)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for param_node in node.actual_params:
            self.visit(param_node)
            s = "  node{} -> node{}\n".format(node._num, param_node._num)
            self.dot_body.append(s)

    def gen_dot(self):
        tree = self.parser.parse()
        self.visit(tree)
        return "".join(self.dot_header + self.dot_body + self.dot_footer)


def main() -> None:
    arg_parser = argparse.ArgumentParser(description="Generate an AST DOT file.")
    arg_parser.add_argument("fname", help="Pascal source file")
    args = arg_parser.parse_args()
    fname = args.fname
    text = open(fname, "r").read()

    lexer = Lexer(text)
    parser = Parser(lexer)
    viz = ASTVisualizer(parser)
    content = viz.gen_dot()
    print(content)


if __name__ == "__main__":
    main()
