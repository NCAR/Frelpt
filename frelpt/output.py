# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

import os
import pyloco

class FrelptOutput(pyloco.Task):

    def __init__(self, parent):

        self.add_data_argument("trees", required=True, help="AST trees")

        self.add_option_argument("-o", "--outdir", default=os.path.join(os.getcwd(), "frelpt-output"), help="output directory")

    def perform(self, targs):

        paths = [p for p in targs.trees]
        cpath = os.path.commonpath(paths)

        for path, tree in targs.trees.items():
            rpath, fname = os.path.split(os.path.relpath(path, start=cpath))

            odir = os.path.join(os.path.join(targs.outdir, rpath))

            if not os.path.isdir(odir):
                os.makedirs(odir) 

            with open(os.path.join(odir, fname), "w") as flpt:
                flpt.write(str(tree.wrapped))
