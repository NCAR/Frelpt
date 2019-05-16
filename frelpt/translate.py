# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

import os
import pyloco

from frelpt.fparser_lptorg import LPTOrgTranslator, collect_do_loopcontrol
from frelpt.fparser_srcgen import LPTSourceGenerator

class FrelptTranslator(pyloco.Task):

    def __init__(self, parent):

        self.add_data_argument("donode", required=True, help="target do node")
        self.add_data_argument("dosubnodes", required=True, help="tarege subnodes of donode")
        self.add_data_argument("modules", required=True, help="module ASTs")
        self.add_data_argument("respaths", required=True, help="resolution paths")
        self.add_data_argument("invrespaths", required=True, help="inverted resolution paths")
        self.add_data_argument("trees", required=True, help="a container of ASTs")
        self.add_data_argument("macros", required=True, help="macro definitions used during compilation for multiple source files")
        self.add_data_argument("includes", required=True, help="include directories used during compilation for multiple source files")

        #evaluate=True, parameter_parse=True
        self.add_option_argument("-o", "--outdir", default=os.getcwd(), help="output directory")

        self.trees = None
        self.modules = None
        self.respaths = None
        self.invrespaths = None

        self.org_do_loopcontrol = None

    def perform(self, targs):

        self.trees = targs.trees
        self.modules = targs.modules
        self.respaths = targs.respaths
        self.invrespaths = targs.invrespaths

        # TODO: analyze loop-carried dependecy
        # if exists, stop

        # collect do variable and loop control variables

        self.org_do_loopcontrol = {}
        targs.donode.traverse(self.org_do_loopcontrol, func=collect_do_loopcontrol)

        # TODO: deepcopy of trees????
        #   - instead of deepcopy approach, use update session???

        parent = self.get_proxy()

        forward = {
            "donode": targs.donode,
            "subnodes": targs.dosubnodes,
            "loopcontrol": self.org_do_loopcontrol,
            "trees": targs.trees,
            "modules": targs.modules,
            "respaths": targs.respaths,
            "invrespaths": targs.invrespaths,
            "macros": targs.macros,
            "includes": targs.includes
        }

        argv = []
        translator = LPTOrgTranslator(parent)
        retval, _forward = translator.run(argv, forward=forward)

        forward = {
            "trees": _forward["trees"],
        }

        argv = []
        srcgen = LPTSourceGenerator(parent)
        retval, _forward = srcgen.run(argv, forward=forward)

