# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

import os
import pyloco

#from frelpt.node import ConcreteSyntaxNode

from fparser.two.Fortran2003 import *
from fparser.two.utils import *

from frelpt.transutil import promote_typedecl, add_dummy_args, add_actual_args
from frelpt.fparser_collect_promotion import CollectVars4Promotion
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
        self.add_data_argument("modules", required=True, help="identifier searcher")

        self.register_forward("trees", help="ASTs used during resolution")
        self.register_forward("modules", help="module ASTs")
        self.register_forward("respaths", help="resolution paths")
        self.register_forward("invrespaths", help="inverted resolution paths")

    def perform(self, targs):

        self.funcname = targs.funcname
        self.dummy_args = targs.promote
        self.loopctr = targs.loopcontrol
        self.respaths = targs.respaths
        self.invrespaths = targs.invrespaths
        self.macros = targs.macros
        self.includes = targs.includes
        self.trees = targs.trees
        self.modules = targs.modules

        forward = {
            "node" : self.funcname.parent.parent.subnodes,
            "macros" : dict(self.macros),
            "includes" : dict(self.includes),
            "trees" : self.trees,
            "modules" : self.modules,
            "respaths" : self.respaths,
            "invrespaths" : self.invrespaths,
        }

        parent = self.get_proxy()
        argv = []
        analyzer = FrelptAnalyzer(parent)
        retval, _forward = analyzer.run(argv, forward=forward)

        self.trees.update(_forward["trees"])
        self.modules.update(_forward["modules"])
        self.respaths.update(_forward["respaths"])
        self.invrespaths.update(_forward["invrespaths"])

        funccalls = []
        pvars = {}

        for dummy_arg in self.dummy_args:
            dummy_res_path = self.respaths[dummy_arg]
            res_name = dummy_res_path[-1]
            for res_path in self.invrespaths[res_name]:
                org_name = res_path[0]
                if org_name is not dummy_arg:

                    ##################################################
                    # collect function calls that propagates promotion
                    ##################################################
                    for funccall in self.collect_func_calls(org_name):
                        if funccall not in funccalls:
                            funccalls.append(funccall)

                    ##############################
                    # collect pvars to be promoted
                    ##############################
                    pvarcollector_parent = self.get_proxy()
                    pvarcollector = CollectVars4Promotion(pvarcollector_parent)
                    pvarcollector_forward = {
                        "nodes" : [org_name],
                        "respaths": self.respaths,
                        "invrespaths": self.invrespaths,
                    }
                    _, _pfwd = pvarcollector.run(["--log", "pvarcollector"], forward=pvarcollector_forward)
                    if org_name in pvars:
                        for pvar in _pfwd["pvars"]:
                            if pvar not in pvars[org_name]:
                                pvars[org_name].append(pvar)
                    else:
                        pvars[org_name] = _pfwd["pvars"]

            ########################
            # promote typedecl stmts
            ########################
            promote_typedecl(dummy_res_path)

        ########################
        # add dummy args
        ########################
        add_dummy_args(targs.funcname.parent, self.loopctr)

        ##############
        # promote vars 
        ##############
        #if pvars: import pdb; pdb.set_trace()
        #self.promote_vars(target_vars, arrvars, funccalls, global_vars, ptypedecls)

        ###################
        # promote funccalls
        ###################
        for funccall in funccalls:
            add_actual_args(funccall.parent, self.loopctr)
        #    gvars = self.promote_func_call(funccall, arr_actargs, global_vars)

        self.add_forward(trees=self.trees)
        self.add_forward(modules=self.modules)
        self.add_forward(respaths=self.respaths)
        self.add_forward(invrespaths=self.invrespaths)

    def collect_func_calls(self, node):

        nodecls = node.wrapped.__class__
        clsname = nodecls.__name__

        if clsname.endswith("_Stmt"):
            return []

        if nodecls in (Actual_Arg_Spec, Actual_Arg_Spec_List):

            if isinstance(node.parent.wrapped, Call_Stmt):
                return [node.parent.subnodes[0]]

            else:
                import pdb; pdb.set_trace()

        elif hasattr(node, "parent"):
            return self.collect_func_calls(node.parent)

        return []
