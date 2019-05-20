# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

import os
import pyloco

class FrelptOutput(pyloco.Task):

    def __init__(self, parent):

        self.add_data_argument("trees", required=True, help="AST trees")

    def perform(self, targs):

        for path, tree in targs.trees.items():
            with open(path+".lpt", "w") as flpt:
                flpt.write(str(tree.wrapped))
