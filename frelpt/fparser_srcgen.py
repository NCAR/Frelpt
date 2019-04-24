# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

import os
import pyloco

from fparser.two.Fortran2003 import *
from fparser.two.utils import *

from .node import walk_ast

class LPTSourceGenerator(pyloco.Task):

    def __init__(self, parent):

        self.add_data_argument("trees", required=True, help="AST trees")

    def perform(self, targs):

        for path, tree in targs.trees.items():
            with open(path+".lpt", "w") as flpt:
                flpt.write(str(tree.wrapped))

#        for path, tree in targs.trees.items():
#            with open(path+".lpt", "w") as flpt:
#                for node in walk_ast(tree):
#                    if hasattr(node, "wrapped"):
#                        wrapped = node.wrapped
#
#                        if hasattr(wrapped, "tofortran"):
#                            flpt.write(wrapped.tofortran())
#
#                        elif isinstance(wrapped, (tuple, list)):
#                            pass
#
#                        else:
#                            import pdb; pdb.set_trace()
#                            flpt.write(str(wrapped))
#
#                    elif node:
#                        flpt.write(str(node))

        #import pdb; pdb.set_trace()
