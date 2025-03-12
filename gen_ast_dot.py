###############################################################################
#  AST visualizer - generates a DOT file for Graphviz.                        #
#                                                                             #
#  To generate an image from the DOT file run $ dot -Tpng -o ast.png ast.dot  #
#                                                                             #
###############################################################################
import argparse
import textwrap

from src.lexer import Lexer
from src.parser import Parser
from src.spi_ast import *
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

    def visit_IfStatement(self, node: IfStatement) -> None:
        s = '  node{} [label="If"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.condition)
        s = '  node{} -> node{} [label="condition"]\n'.format(
            node._num, node.condition._num
        )
        self.dot_body.append(s)

        self.visit(node.then_branch)
        s = '  node{} -> node{} [label="then"]\n'.format(
            node._num, node.then_branch._num
        )
        self.dot_body.append(s)

        for i, elif_branch in enumerate(node.else_if_branches):
            self.visit(elif_branch)
            s = '  node{} -> node{} [label="elseif {}"]\n'.format(
                node._num, elif_branch._num, i + 1
            )
            self.dot_body.append(s)

        if node.else_branch:
            self.visit(node.else_branch)
            s = '  node{} -> node{} [label="else"]\n'.format(
                node._num, node.else_branch._num
            )
            self.dot_body.append(s)

    def visit_CaseStatement(self, node: CaseStatement) -> None:
        s = '  node{} [label="Case"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.matcher)
        s = '  node{} -> node{} [label="matcher"]\n'.format(
            node._num, node.matcher._num
        )
        self.dot_body.append(s)

        for i, (condition, statement) in enumerate(node.branches):
            self.visit(condition)
            self.visit(statement)
            s = '  node{} -> node{} [label="case {}"]\n'.format(
                node._num, condition._num, i + 1
            )
            self.dot_body.append(s)
            s = "  node{} -> node{}\n".format(condition._num, statement._num)
            self.dot_body.append(s)

        if node.else_branch:
            self.visit(node.else_branch)
            s = '  node{} -> node{} [label="else"]\n'.format(
                node._num, node.else_branch._num
            )
            self.dot_body.append(s)

    def visit_WhileStatement(self, node: WhileStatement) -> None:
        s = '  node{} [label="While"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.condition)
        s = '  node{} -> node{} [label="condition"]\n'.format(
            node._num, node.condition._num
        )
        self.dot_body.append(s)

        self.visit(node.block)
        s = '  node{} -> node{} [label="do"]\n'.format(node._num, node.block._num)
        self.dot_body.append(s)

    def visit_ForStatement(self, node: ForStatement) -> None:
        s = '  node{} [label="For"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.initialization)
        s = '  node{} -> node{} [label="init"]\n'.format(
            node._num, node.initialization._num
        )
        self.dot_body.append(s)

        self.visit(node.bound)
        s = '  node{} -> node{} [label="to"]\n'.format(node._num, node.bound._num)
        self.dot_body.append(s)

        self.visit(node.block)
        s = '  node{} -> node{} [label="do"]\n'.format(node._num, node.block._num)
        self.dot_body.append(s)

    def visit_ConstAssign(self, node: ConstAssign) -> None:
        s = '  node{} [label="Const:{}"]]\n'.format(self.n_count, node.op.value)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.left)
        self.visit(node.right)

        for child_node in (node.left, node.right):
            s = "  node{} -> node{}\n".format(node._num, child_node._num)
            self.dot_body.append(s)

        if node.type:
            self.visit(node.type)
            s = '  node{} -> node{} [label="type"]\n'.format(node._num, node.type._num)
            self.dot_body.append(s)

    def visit_EnumVar(self, node: EnumVar) -> None:
        s = '  node{} [label="{}:{}"]\n'.format(self.n_count, node.name, node.key)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_RecordVar(self, node: RecordVar) -> None:
        s = '  node{} [label="{}:{}"]\n'.format(self.n_count, node.name, node.key)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_IndexVar(self, node: IndexVar) -> None:
        s = '  node{} [label="IndexVar"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.left)
        s = '  node{} -> node{} [label="var"]\n'.format(node._num, node.left._num)
        self.dot_body.append(s)

        self.visit(node.index)
        s = '  node{} -> node{} [label="index"]\n'.format(node._num, node.index._num)
        self.dot_body.append(s)

    def visit_FieldDecl(self, node: FieldDecl) -> None:
        s = '  node{} [label="FieldDecl"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.var_node)
        s = "  node{} -> node{}\n".format(node._num, node.var_node._num)
        self.dot_body.append(s)

        self.visit(node.type_node)
        s = "  node{} -> node{}\n".format(node._num, node.type_node._num)
        self.dot_body.append(s)

    def visit_MethodDef(self, node: MethodDef) -> None:
        s = '  node{} [label="MethodDef:{}"]\n'.format(self.n_count, node.method_name)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for param_node in node.params:
            self.visit(param_node)
            s = "  node{} -> node{}\n".format(node._num, param_node._num)
            self.dot_body.append(s)

        self.visit(node.return_type)
        s = '  node{} -> node{} [label="returns"]\n'.format(
            node._num, node.return_type._num
        )
        self.dot_body.append(s)

    def visit_MethodDecl(self, node: MethodDecl) -> None:
        s = '  node{} [label="MethodDecl:{}"]\n'.format(
            self.n_count, node.method_full_name
        )
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for param_node in node.params:
            self.visit(param_node)
            s = "  node{} -> node{}\n".format(node._num, param_node._num)
            self.dot_body.append(s)

        self.visit(node.return_type)
        s = '  node{} -> node{} [label="returns"]\n'.format(
            node._num, node.return_type._num
        )
        self.dot_body.append(s)

        self.visit(node.block)
        s = "  node{} -> node{}\n".format(node._num, node.block._num)
        self.dot_body.append(s)

    def visit_ClassDecl(self, node: ClassDecl) -> None:
        s = '  node{} [label="ClassDecl:{}"]\n'.format(self.n_count, node.class_name)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for field in node.fields:
            self.visit(field)
            s = '  node{} -> node{} [label="field"]\n'.format(node._num, field._num)
            self.dot_body.append(s)

        for method in node.methods:
            self.visit(method)
            s = '  node{} -> node{} [label="method"]\n'.format(node._num, method._num)
            self.dot_body.append(s)

        if node.constructor:
            self.visit(node.constructor)
            s = '  node{} -> node{} [label="constructor"]\n'.format(
                node._num, node.constructor._num
            )
            self.dot_body.append(s)

        if node.destructor:
            self.visit(node.destructor)
            s = '  node{} -> node{} [label="destructor"]\n'.format(
                node._num, node.destructor._num
            )
            self.dot_body.append(s)

    def visit_RecordDecl(self, node: RecordDecl) -> None:
        s = '  node{} [label="RecordDecl:{}"]\n'.format(self.n_count, node.record_name)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for field in node.fields:
            self.visit(field)
            s = '  node{} -> node{} [label="field"]\n'.format(node._num, field._num)
            self.dot_body.append(s)

    def visit_EnumDecl(self, node: EnumDecl) -> None:
        s = '  node{} [label="EnumDecl:{}"]\n'.format(self.n_count, node.enum_name)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for value, name in node.entries.items():
            entry_node = self.n_count
            self.dot_body.append(
                '  node{} [label="{}={}"]\n'.format(entry_node, name, value)
            )
            self.n_count += 1
            s = "  node{} -> node{}\n".format(node._num, entry_node)
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
        s = '  node{} -> node{} [label="returns"]\n'.format(
            node._num, node.return_type._num
        )
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

    def visit_MethodCall(self, node: MethodCall) -> None:
        s = '  node{} [label="MethodCall:{}"]\n'.format(
            self.n_count, node.method_full_name
        )
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for param_node in node.actual_params:
            self.visit(param_node)
            s = "  node{} -> node{}\n".format(node._num, param_node._num)
            self.dot_body.append(s)

    def visit_GetItem(self, node: GetItem) -> None:
        if node.is_property:
            s = '  node{} [label="GetProp:{}"]\n'.format(self.n_count, node.key)
        else:
            s = '  node{} [label="GetIndex"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        if not node.is_property:
            self.visit(node.key)
            s = '  node{} -> node{} [label="index"]\n'.format(node._num, node.key._num)
            self.dot_body.append(s)

    def visit_ExprGet(self, node: ExprGet) -> None:
        s = '  node{} [label="ExprGet"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.object)
        s = '  node{} -> node{} [label="object"]\n'.format(node._num, node.object._num)
        self.dot_body.append(s)

        for i, get_item in enumerate(node.gets):
            self.visit(get_item)
            s = '  node{} -> node{} [label="get{}"]\n'.format(
                node._num, get_item._num, i + 1
            )
            self.dot_body.append(s)

    def visit_ExprSet(self, node: ExprSet) -> None:
        s = '  node{} [label="ExprSet"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.expr_get)
        s = '  node{} -> node{} [label="target"]\n'.format(
            node._num, node.expr_get._num
        )
        self.dot_body.append(s)

        self.visit(node.value)
        s = '  node{} -> node{} [label="value"]\n'.format(node._num, node.value._num)
        self.dot_body.append(s)

    def visit_ConstructorDecl(self, node: ConstructorDecl) -> None:
        s = '  node{} [label="ConstructorDecl"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for param_node in node.params:
            self.visit(param_node)
            s = "  node{} -> node{}\n".format(node._num, param_node._num)
            self.dot_body.append(s)

        self.visit(node.block)
        s = "  node{} -> node{}\n".format(node._num, node.block._num)
        self.dot_body.append(s)

    def visit_ConstructorDef(self, node: ConstructorDef) -> None:
        s = '  node{} [label="ConstructorDef"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for param_node in node.params:
            self.visit(param_node)
            s = "  node{} -> node{}\n".format(node._num, param_node._num)
            self.dot_body.append(s)

    def visit_ProcedureDef(self, node: ProcedureDef) -> None:
        s = '  node{} [label="ProcDef:{}"]\n'.format(self.n_count, node.proc_name)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for param_node in node.formal_params:
            self.visit(param_node)
            s = "  node{} -> node{}\n".format(node._num, param_node._num)
            self.dot_body.append(s)

    def visit_FunctionDef(self, node: FunctionDef) -> None:
        s = '  node{} [label="FuncDef:{}"]\n'.format(self.n_count, node.func_name)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        for param_node in node.formal_params:
            self.visit(param_node)
            s = "  node{} -> node{}\n".format(node._num, param_node._num)
            self.dot_body.append(s)

        self.visit(node.return_type)
        s = '  node{} -> node{} [label="returns"]\n'.format(
            node._num, node.return_type._num
        )
        self.dot_body.append(s)

    def visit_Decl(self, node: Decl) -> None:
        s = '  node{} [label="Decl"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_Member(self, node: Member) -> None:
        s = '  node{} [label="Member"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_Def(self, node: Def) -> None:
        s = '  node{} [label="Def"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_StringType(self, node: StringType) -> None:
        s = '  node{} [label="StringType"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        if node.limit:
            self.visit(node.limit)
            s = '  node{} -> node{} [label="limit"]\n'.format(
                node._num, node.limit._num
            )
            self.dot_body.append(s)

    def visit_PrimitiveType(self, node: PrimitiveType) -> None:
        s = '  node{} [label="PrimitiveType:{}"]\n'.format(
            self.n_count, node.token.value
        )
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_ArrayType(self, node: ArrayType) -> None:
        s = '  node{} [label="ArrayType"]\n'.format(self.n_count)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

        self.visit(node.lower)
        s = '  node{} -> node{} [label="lower"]\n'.format(node._num, node.lower._num)
        self.dot_body.append(s)

        self.visit(node.upper)
        s = '  node{} -> node{} [label="upper"]\n'.format(node._num, node.upper._num)
        self.dot_body.append(s)

        self.visit(node.element_type)
        s = '  node{} -> node{} [label="element_type"]\n'.format(
            node._num, node.element_type._num
        )
        self.dot_body.append(s)

        # Add dynamic flag if needed
        if node.dynamic:
            dyn_node = self.n_count
            self.dot_body.append('  node{} [label="dynamic=True"]\n'.format(dyn_node))
            self.n_count += 1
            s = '  node{} -> node{} [label="dynamic"]\n'.format(node._num, dyn_node)
            self.dot_body.append(s)

    def visit_ClassType(self, node: ClassType) -> None:
        s = '  node{} [label="ClassType:{}"]\n'.format(self.n_count, node.token.value)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_EnumType(self, node: EnumType) -> None:
        s = '  node{} [label="EnumType:{}"]\n'.format(self.n_count, node.token.value)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

    def visit_RecordType(self, node: RecordType) -> None:
        s = '  node{} [label="RecordType:{}"]\n'.format(self.n_count, node.token.value)
        self.dot_body.append(s)
        node._num = self.n_count
        self.n_count += 1

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
