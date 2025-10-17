###############################################################################
#  AST visualizer - generates a DOT file for Graphviz.                        #
#                                                                             #
#  To generate an image from the DOT file run $ dot -Tpng -o ast.png ast.dot  #
#                                                                             #
###############################################################################
import argparse
import textwrap

from spi import (
    AccessExpression,
    ArrayType,
    Assign,
    BinOp,
    Block,
    Bool,
    BreakStatement,
    CaseItem,
    CaseLabel,
    CaseStatement,
    Char,
    Compound,
    ConstDecl,
    ContinueStatement,
    EnumType,
    ForStatement,
    FunctionCall,
    FunctionDecl,
    IfStatement,
    IndexSuffix,
    InOperator,
    Lexer,
    MemberSuffix,
    NodeVisitor,
    NoOp,
    Num,
    Param,
    Parser,
    PrimitiveType,
    ProcedureCall,
    ProcedureDecl,
    Program,
    RecordField,
    RecordType,
    SetLiteral,
    String,
    StringType,
    SubrangeType,
    Type,
    TypeDeclaration,
    UnaryOp,
    Var,
    VarDecl,
    VariantCase,
    VariantPart,
    WhileStatement,
)
from spi.ast import UsesDeclaration


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

    def visit_FunctionDecl(self, node: FunctionDecl) -> None:
        s = '  node{} [label="FuncDecl:{}"]\n'.format(self.n_count, node.func_name)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for param_node in node.formal_params:
            self.visit(param_node)
            s = "  node{} -> node{}\n".format(node._num, param_node._num)
            self.dot_body.append(s)

        self.visit(node.return_type)
        s = "  node{} -> node{}\n".format(node._num, node.return_type._num)
        self.dot_body.append(s)

        self.visit(node.block_node)
        s = "  node{} -> node{}\n".format(node._num, node.block_node._num)
        self.dot_body.append(s)

    def visit_FunctionCall(self, node: FunctionCall) -> None:
        s = '  node{} [label="FuncCall:{}"]\n'.format(self.n_count, node.func_name)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for param_node in node.actual_params:
            self.visit(param_node)
            s = "  node{} -> node{}\n".format(node._num, param_node._num)
            self.dot_body.append(s)

    def visit_ConstDecl(self, node: ConstDecl) -> None:
        s = '  node{} [label="ConstDecl"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.var_node)
        s = "  node{} -> node{}\n".format(node._num, node.var_node._num)
        self.dot_body.append(s)

        self.visit(node.value_expr)
        s = "  node{} -> node{}\n".format(node._num, node.value_expr._num)
        self.dot_body.append(s)

    def visit_UsesDeclaration(self, node: UsesDeclaration) -> None:
        s = '  node{} [label="UsesDeclaration"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for unit_name in node.unit_names:
            self.visit(unit_name)
            s = "  node{} -> node{}\n".format(node._num, unit_name._num)
            self.dot_body.append(s)

    def visit_TypeDeclaration(self, node: TypeDeclaration) -> None:
        s = '  node{} [label="TypeDecl"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.type_name)
        s = "  node{} -> node{}\n".format(node._num, node.type_name._num)
        self.dot_body.append(s)

        self.visit(node.type_def)
        s = "  node{} -> node{}\n".format(node._num, node.type_def._num)
        self.dot_body.append(s)

    def visit_Bool(self, node: Bool) -> None:
        s = '  node{} [label="{}"]\n'.format(self.n_count, node.value)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_String(self, node: String) -> None:
        s = '  node{} [label="\\"{}\\""]\n'.format(self.n_count, node.value)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_Char(self, node: Char) -> None:
        s = "  node{} [label=\"'{}'\"]".format(self.n_count, node.value)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_IfStatement(self, node: IfStatement) -> None:
        s = '  node{} [label="If"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.condition)
        s = "  node{} -> node{}\n".format(node._num, node.condition._num)
        self.dot_body.append(s)

        self.visit(node.then_branch)
        s = "  node{} -> node{}\n".format(node._num, node.then_branch._num)
        self.dot_body.append(s)

        for elif_branch in node.else_if_branches:
            self.visit(elif_branch)
            s = "  node{} -> node{}\n".format(node._num, elif_branch._num)
            self.dot_body.append(s)

        if node.else_branch:
            self.visit(node.else_branch)
            s = "  node{} -> node{}\n".format(node._num, node.else_branch._num)
            self.dot_body.append(s)

    def visit_WhileStatement(self, node: WhileStatement) -> None:
        s = '  node{} [label="While"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.condition)
        s = "  node{} -> node{}\n".format(node._num, node.condition._num)
        self.dot_body.append(s)

        self.visit(node.block)
        s = "  node{} -> node{}\n".format(node._num, node.block._num)
        self.dot_body.append(s)

    def visit_ForStatement(self, node: ForStatement) -> None:
        s = '  node{} [label="For"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.initialization)
        s = "  node{} -> node{}\n".format(node._num, node.initialization._num)
        self.dot_body.append(s)

        self.visit(node.bound)
        s = "  node{} -> node{}\n".format(node._num, node.bound._num)
        self.dot_body.append(s)

        self.visit(node.block)
        s = "  node{} -> node{}\n".format(node._num, node.block._num)
        self.dot_body.append(s)

    def visit_CaseStatement(self, node: CaseStatement) -> None:
        s = '  node{} [label="Case"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.case_expr)
        s = "  node{} -> node{}\n".format(node._num, node.case_expr._num)
        self.dot_body.append(s)

        for case_item in node.case_items:
            self.visit(case_item)
            s = "  node{} -> node{}\n".format(node._num, case_item._num)
            self.dot_body.append(s)

        if node.else_stmt:
            self.visit(node.else_stmt)
            s = "  node{} -> node{}\n".format(node._num, node.else_stmt._num)
            self.dot_body.append(s)

    def visit_CaseItem(self, node: CaseItem) -> None:
        s = '  node{} [label="CaseItem"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for label in node.labels:
            self.visit(label)
            s = "  node{} -> node{}\n".format(node._num, label._num)
            self.dot_body.append(s)

        self.visit(node.statement)
        s = "  node{} -> node{}\n".format(node._num, node.statement._num)
        self.dot_body.append(s)

    def visit_CaseLabel(self, node: CaseLabel) -> None:
        s = '  node{} [label="Label:{}"]\n'.format(self.n_count, node.value)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_AccessExpression(self, node: AccessExpression) -> None:
        s = '  node{} [label="Access"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.base)
        s = "  node{} -> node{}\n".format(node._num, node.base._num)
        self.dot_body.append(s)

        for suffix in node.suffixes:
            self.visit(suffix)
            s = "  node{} -> node{}\n".format(node._num, suffix._num)
            self.dot_body.append(s)

    def visit_IndexSuffix(self, node: IndexSuffix) -> None:
        s = '  node{} [label="Index"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.index)
        s = "  node{} -> node{}\n".format(node._num, node.index._num)
        self.dot_body.append(s)

    def visit_MemberSuffix(self, node: MemberSuffix) -> None:
        s = '  node{} [label="Member:{}"]\n'.format(self.n_count, node.member.value)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_ArrayType(self, node: ArrayType) -> None:
        s = '  node{} [label="ArrayType"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.element_type)
        s = "  node{} -> node{}\n".format(node._num, node.element_type._num)
        self.dot_body.append(s)

        if node.bounds:
            self.visit(node.bounds)
            s = "  node{} -> node{}\n".format(node._num, node.bounds._num)
            self.dot_body.append(s)

    def visit_SubrangeType(self, node: SubrangeType) -> None:
        s = '  node{} [label="Subrange"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.lower)
        s = "  node{} -> node{}\n".format(node._num, node.lower._num)
        self.dot_body.append(s)

        self.visit(node.upper)
        s = "  node{} -> node{}\n".format(node._num, node.upper._num)
        self.dot_body.append(s)

    def visit_EnumType(self, node: EnumType) -> None:
        s = '  node{} [label="Enum:{}"]\n'.format(
            self.n_count, ",".join(node.enum_values)
        )
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_RecordType(self, node: RecordType) -> None:
        s = '  node{} [label="Record"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for field in node.fields:
            self.visit(field)
            s = "  node{} -> node{}\n".format(node._num, field._num)
            self.dot_body.append(s)

        if node.variant_part:
            self.visit(node.variant_part)
            s = "  node{} -> node{}\n".format(node._num, node.variant_part._num)
            self.dot_body.append(s)

    def visit_RecordField(self, node: RecordField) -> None:
        s = '  node{} [label="Field"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.name)
        s = "  node{} -> node{}\n".format(node._num, node.name._num)
        self.dot_body.append(s)

        self.visit(node.type_node)
        s = "  node{} -> node{}\n".format(node._num, node.type_node._num)
        self.dot_body.append(s)

    def visit_VariantPart(self, node: VariantPart) -> None:
        s = '  node{} [label="VariantPart"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.tag_field)
        s = "  node{} -> node{}\n".format(node._num, node.tag_field._num)
        self.dot_body.append(s)

        for variant_case in node.variant_cases:
            self.visit(variant_case)
            s = "  node{} -> node{}\n".format(node._num, variant_case._num)
            self.dot_body.append(s)

    def visit_VariantCase(self, node: VariantCase) -> None:
        s = '  node{} [label="VariantCase:{}"]\n'.format(
            self.n_count, ",".join(node.tag_values)
        )
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for field in node.fields:
            self.visit(field)
            s = "  node{} -> node{}\n".format(node._num, field._num)
            self.dot_body.append(s)

    def visit_SetLiteral(self, node: SetLiteral) -> None:
        s = '  node{} [label="Set"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for element in node.elements:
            self.visit(element)
            s = "  node{} -> node{}\n".format(node._num, element._num)
            self.dot_body.append(s)

    def visit_InOperator(self, node: InOperator) -> None:
        s = '  node{} [label="In"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.value)
        s = "  node{} -> node{}\n".format(node._num, node.value._num)
        self.dot_body.append(s)

        self.visit(node.set_expr)
        s = "  node{} -> node{}\n".format(node._num, node.set_expr._num)
        self.dot_body.append(s)

    def visit_BreakStatement(self, node: BreakStatement) -> None:
        s = '  node{} [label="Break"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_ContinueStatement(self, node: ContinueStatement) -> None:
        s = '  node{} [label="Continue"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_PrimitiveType(self, node: PrimitiveType) -> None:
        s = '  node{} [label="{}"]\n'.format(self.n_count, node.value)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_StringType(self, node: StringType) -> None:
        s = '  node{} [label="STRING"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        if node.limit:
            self.visit(node.limit)
            s = "  node{} -> node{}\n".format(node._num, node.limit._num)
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
