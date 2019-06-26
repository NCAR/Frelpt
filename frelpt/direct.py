# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function


import pyloco

from .fparser_parse import Parser
from .node import walk_ast

class FrelptDirective(pyloco.Task):

    def __init__(self, parent):

        self.add_data_argument("target", help="filepath to the source file having 'pushdown' frelpt directive")

        self.add_option_argument("-m", "--macro", help="macro definitions used during compilation for multiple source files")
        self.add_option_argument("-i", "--include", help="include directories used during compilation for multiple source files")

        self.register_forward("tree", help="Abstract Syntax Tree object")
        self.register_forward("donode", help="DO stmt node")
        self.register_forward("subnodes", help="nodes within donode")

    def perform(self, targs):

        macro = {}
        if targs.macro:
            if isinstance(targs.macro, dict):
                macro.update(targs.macro)
            else:
                import pdb ;pdb.set_trace()

        include = []
        if targs.include:
            if isinstance(targs.include, (list, tuple)):
                include.extend(targs.include)
            else:
                import pdb ;pdb.set_trace()

        forward = {
            "target" : targs.target,
            "macro" : macro,
            "include" : include,
        }

        #self.log_debug(str(forward))

        argv = []
        parser = Parser(self.get_proxy())
        retval, _forward = parser.run(argv, forward=forward)

        target_tree = _forward["tree"]

        # locate pushdown directive
        # locate DO loop to push and collect pushing info
        # locate statements in the DO loop

        target_donode = None
        initial_nodes = []

        for node in walk_ast(target_tree, node_types=("comment",)):
            line = node.subnodes[0].lstrip()
            if line.startswith("!$frelpt"):
                clauses = line[8:].lstrip().split()
                if clauses and clauses[0] == "pushdown":
                    node_idx = node.parent.subnodes.index(node)
                    for child in node.parent.subnodes[node_idx+1:]:
                        if child.wrapped in parser.do_stmts:
                            donode_idx = child.parent.subnodes.index(child)
                            target_donode = child
                            initial_nodes = [child] + child.parent.subnodes[donode_idx+1:]
                            break
                        elif child.wrapped in parser.do_constructs:
                            import pdb; pdb.set_trace()

        self.add_forward(tree=target_tree)
        self.add_forward(donode=target_donode)
        self.add_forward(subnodes=initial_nodes)
