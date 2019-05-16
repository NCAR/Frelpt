# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

import os
import pyloco

#from frelpt.node import ConcreteSyntaxNode

from fparser.two.Fortran2003 import *
from fparser.two.utils import *

from frelpt.fparser_util import collect_entity_names
from frelpt.analyze import FrelptAnalyzer

class LPTFunctionTranslator(pyloco.Task):

    def __init__(self, parent):

        self.add_data_argument("funcname", required=True, help="function name")
        self.add_data_argument("promote", required=True, help="promotted actual variable")
        self.add_data_argument("loopcontrol", required=True, help="do loop control variables")
        self.add_data_argument("respaths", required=True, help="a list of resolution path")
        self.add_data_argument("invrespaths", required=True, help="a inverse list of resolution path")
        self.add_data_argument("macros", required=True, help="macro definitions used during compilation for multiple source files")
        self.add_data_argument("includes", required=True, help="include directories used during compilation for multiple source files")
        self.add_data_argument("trees", required=True, help="a container of ASTs")

        #evaluate=True, parameter_parse=True
        #self.add_option_argument("-o", "--outdir", default=os.getcwd(), help="output directory")

        #self.register_forward("trees", help="modified ASTs")

    def perform(self, targs):

        self.funcname = targs.funcname
        self.dummy_args = targs.promote
        self.loopctr = targs.loopcontrol
        self.respaths = targs.respaths
        self.invrespaths = targs.invrespaths
        self.macros = targs.macros
        self.includes = targs.includes
        self.trees = targs.trees
        
        # find typedecls of dummy arg
        self.typedecls = {}

        for funcnode in self.funcname.parent.parent.subnodes:
            if isinstance(funcnode.wrapped, Specification_Part):
                for specnode in funcnode.subnodes:
                    if isinstance(specnode.wrapped, Type_Declaration_Stmt):
                        entity_names = collect_entity_names(specnode.subnodes[2])
                        for entity in entity_names:
                            for darg in self.dummy_args:
                                if entity.wrapped == darg.wrapped:
                                    if darg in self.typedecls:
                                        raise Exception("Dupulicated arg typedecl: %s" % str(darg))
                                    else:
                                        self.typedecls[darg] = specnode

        #argv = []
        #analyzer = FrelptAnalyzer(parent)
        #retval, _forward = analyzer.run(argv, forward=forward)

        forward = {
            "node" : self.funcname.parent.parent.subnodes,
            "macros" : dict(self.macros),
            "includes" : dict(self.includes),
            "trees" : self.trees
        }

        parent = self.get_proxy()
        argv = []
        analyzer = FrelptAnalyzer(parent)
        retval, _forward = analyzer.run(argv, forward=forward)


        import pdb; pdb.set_trace()

        # find originating names that are resolved by the typedecls
        for declname, typedecl in self.typedecls.items():
            import pdb; pdb.set_trace()

        # if function: repleat with functiontranslator
        # else: fparser_lptterm.py
        # find another pvars impacted by this

        import pdb; pdb.set_trace()

