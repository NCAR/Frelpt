# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

import os
import pyloco

#from frelpt.node import ConcreteSyntaxNode

from fparser.two.Fortran2003 import *
from fparser.two.utils import *

from frelpt.fparser_argument import FparserArgument
from frelpt.transbase import FrelptTransBase
from frelpt.fparser_collect_promotion import CollectVars4Promotion
from frelpt.fparser_util import (collect_entity_names, promote_typedecl, add_loopctr_dummy_args, add_loopctr_actual_args,
        collect_funccall_stmt, is_actual_arg_spec_list, collect_names, is_classtype, is_dummy_arg_list)
from frelpt.analyze import FrelptAnalyzer

class LPTFunctionTranslator(pyloco.Task, FrelptTransBase):

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

        funccalls = {}
        pvars = []
        all_pvars = []
        argsplit_index = 0

        # check if exists polymorphic arguments
        if is_dummy_arg_list(self.funcname.parent.subnodes[2]):
            for darg in self.funcname.parent.subnodes[2].subnodes:
                res_darg = self.respaths[darg][-1]
                if is_classtype(res_darg):
                    argsplit_index += 1
                else:
                    break
        else:
            import pdb; pdb.set_trace()

        for dummy_arg in self.dummy_args:
            dummy_res_path = self.respaths[dummy_arg]
            res_name = dummy_res_path[-1]

            for res_path in self.invrespaths[res_name]:
                org_name = res_path[0]
                if org_name is not dummy_arg:

                    ##################################################
                    # collect function calls that propagates promotion
                    ##################################################
                    for funccall, actual_args in self._collect_func_calls(org_name):
                        if funccall not in funccalls:
                            funccalls[funccall] = actual_args

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

                    #pvars.extend(_pfwd["pvars"])

                    #new_pvars = []

                    # collection function calls from to-be-promoted vars
                    for pvar in _pfwd["pvars"]:
                        if collect_funccall_stmt(pvar) is None:
                            if pvar not in pvars:
                                pvars.append(pvar)
                        elif pvar not in all_pvars:
                            all_vars.append(pvar)
                    #        new_pvars.append(pvar)

                    if org_name not in pvars and collect_funccall_stmt(org_name) is None:
                        pvars.append(org_name)

                    if org_name not in all_pvars:
                        all_pvars.append(org_name)

                    #if org_name in pvars:
                    #    pvars[org_name] = []

                    #for new_pvar in new_pvars:
                    #    if new_pvars is not org_name and new_pvar not in pvars[org_name]:
                    #        pvars[org_name].append(new_pvars)

            ########################
            # promote typedecl stmts
            ########################
            promote_typedecl(dummy_res_path)

        ########################
        # add dummy args
        ########################
        add_loopctr_dummy_args(targs.funcname.parent, self.loopctr, argsplit_index=argsplit_index)

        ##############
        # promote vars 
        ##############
        #if pvars: import pdb; pdb.set_trace()
        arrvars = []
        global_vars = []
        ptypedecls = {} 
        self.promote_vars(pvars, arrvars, funccalls, global_vars, ptypedecls)

        ###################
        # promote funccalls
        ###################
        for funccall, actual_args in funccalls.items():
            org_funccall = funccall

            arg_indices = {}
            if is_actual_arg_spec_list(actual_args):
                for idx, arg in enumerate(actual_args.subnodes):
                    anames = collect_names(arg)
                    if any((n in all_pvars) for n in anames):
                        arg_indices[idx] = arg
            else:
                import pdb; pdb.set_trace()

            if "%" in funccall.subnodes:
                prefs = []

                for n1, n2 in zip(funccall.subnodes[1:-1], funccall.subnodes[2:]):
                    if n1 == "%":
                        prefs.append(n2)

                self.process_dataref(funccall.subnodes[0], prefs, actual_args, arg_indices, global_vars)

            else:
                self.promote_func_call(funccall, actual_args, arg_indices, global_vars)

            add_loopctr_actual_args(org_funccall.parent, self.loopctr)

        self.add_forward(trees=self.trees)
        self.add_forward(modules=self.modules)
        self.add_forward(respaths=self.respaths)
        self.add_forward(invrespaths=self.invrespaths)

    def _collect_func_calls(self, node):

        nodecls = node.wrapped.__class__
        clsname = nodecls.__name__

        if clsname.endswith("_Stmt"):
            return []

        if nodecls in (Actual_Arg_Spec, Actual_Arg_Spec_List):

            if isinstance(node.parent.wrapped, Call_Stmt):
                return [(node.parent.subnodes[0], node)]

            else:
                import pdb; pdb.set_trace()

        elif hasattr(node, "parent"):
            return self._collect_func_calls(node.parent)

        return []
