# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

import os
import pyloco

from frelpt.node import ConcreteSyntaxNode

from fparser.two.Fortran2003 import *
from fparser.two.utils import *

from frelpt.transbase import FrelptTransBase
from frelpt.fparser_argument import FparserArgument
from frelpt.fparser_lptfunc import LPTFunctionTranslator
from frelpt.fparser_collect_promotion import CollectVars4Promotion
from frelpt.fparser_util import (collect_names, get_parent_by_class, get_entity_decl_by_name,
                get_attr_spec, collect_nodes_by_class, collect_func_calls, promote_typedecl, collect_funccall_stmt,
                replace_dovar_with_section_subscript, append_subnode,
                remove_subnode, insert_subnode, is_descendant)

# collect start, stop, and step parameters
def collect_do_loopcontrol(node, bag, depth):

    if hasattr(node, "wrapped") and isinstance(node.wrapped, Loop_Control):

        scalar_logical_expr, counter_expr, optional_delim = node.wrapped.items

        if scalar_logical_expr is not None:
            raise Exception("'WHILE' loop is not supported for loop pushdown translation.")

        bag["dovar"] = counter_expr[0].pair
        bag["start"] = counter_expr[1][0].pair
        bag["stop"]  = counter_expr[1][1].pair
        if len(counter_expr[1]) > 2:
            bag["step"] = counter_expr[1][2].pair if len(counter_expr[1]) > 2 else None
        else:
            bag["step"] = None

        return True

# pushdown originating source file
class FrelptTranslator(pyloco.Task, FrelptTransBase):

    # register command line arguments and forward
    def __init__(self, parent):

        self.add_data_argument("donode", required=True, help="org do node for loop pushdown translation")
        self.add_data_argument("subnodes", required=True, help="org do subnodes for loop pushdown translation")
        #self.add_data_argument("loopcontrol", required=True, help="org do loop control node for loop pushdown translation")
        self.add_data_argument("trees", required=True, help="all abstract syntax trees")
        self.add_data_argument("modules", required=True, help="all modules")
        self.add_data_argument("respaths", required=True, help="a list of resolution path")
        self.add_data_argument("invrespaths", required=True, help="a inverse list of resolution path")
        self.add_data_argument("macros", required=True, help="macro definitions used during compilation for multiple source files")
        self.add_data_argument("includes", required=True, help="include directories used during compilation for multiple source files")

        #evaluate=True, parameter_parse=True
        self.add_option_argument("-o", "--outdir", default=os.getcwd(), help="output directory")

        self.register_forward("trees", help="modified ASTs")

    # main routine
    def perform(self, targs):

        # TODO: analyze loop-independent dependecy
        # if exists, ???

        # find array variables that use dovar in section subscripts
        #  - need promotion but not needs typedecl promotion

        # find variables acampanied by previous found array variables
        #  - need promotion and needs typedecl promotion

        # find subprogram call using dovar and locally promoted variables
        #  - need argument promotion and add loopcontrol in argument
          
        # 1) find array variables that uses dovar and promote the array variables
        # 2) find variables that needs to be promoted, which accompanied with the array variables
        # 3) resolve the variables and promote it in the typedecl resolver and
        #    promote all usage of the variable in the unit
        # 4) repeat from 1) with the promoted variables until there is no more promoted variables
        # 5) remove do loop
        # NOTE: promoted variables contains which typedecl stmt initiated the promotion so that
        #       the same promotion will not be repeated. It may not need to retain typedecl stmt
        #       , instead just mark that the variable is promoted. But until fully understood,
        #       keep the typedecl stmt as it allows to detect confliction from other typedecl stmt.

        self.trees = targs.trees
        self.modules = targs.modules
        self.respaths = targs.respaths
        self.invrespaths = targs.invrespaths
        self.macros = targs.macros
        self.includes = targs.includes

        self.analyzer_info = {
            "trees": self.trees,
            "modules": self.modules,
            "respaths": self.respaths,
            "invrespaths": self.invrespaths,
        }

        # typedecl stmts for variables within thie target do loop
        self.typedecls = {}

        # typedecl stmts for promotion
        ptypedecls = {} 

        self.loopctr = {}
        targs.donode.traverse(self.loopctr, func=collect_do_loopcontrol)

        ################################
        # collect arrvars to be promoted
        ################################
        arrvars = []
        for subnode in targs.subnodes:
            pvars = self.collect_array_vars(subnode)
            for pvar in pvars: 
                if pvar not in arrvars:
                    arrvars.append(pvar)

        ##################################################
        # collect function calls that propagetes promotion
        ##################################################
        funccalls = []
        arr_actargs = {}

        for subnode in targs.subnodes:
            for funccall in collect_func_calls(subnode, arrvars, arr_actargs, self.respaths):
                if funccall not in funccalls:
                    funccalls.append(funccall)

        funcstmts = []
        for funccall in funccalls:
            funcstmt = collect_funccall_stmt(funccall)
            if funcstmt and funcstmt not in funcstmts:
                funcstmts.append(funcstmt)

        # NOTE: generate promotion even if promotion is already done so that promotion compatibility can be checked.

        ##############################
        # collect pvars to be promoted
        ##############################
        pvarcollector_parent = self.get_proxy()
        pvarcollector = CollectVars4Promotion(pvarcollector_parent)
        pvarcollector_forward = {
            "nodes" : arrvars,
            "respaths": self.respaths,
            "invrespaths": self.invrespaths,
        }
        _, _pfwd = pvarcollector.run(["--log", "pvarcollector"], forward=pvarcollector_forward)
        target_vars = _pfwd["pvars"]

        global_vars = []

        ##############
        # promote vars 
        ##############
        self.promote_vars(target_vars, arrvars, funccalls, global_vars, ptypedecls)

        ##################################################
        # reposition this subnode to just above of do node
        ##################################################
        pnode = targs.donode.parent
        idxdo = pnode.subnodes.index(targs.donode)

        #import pdb; pdb.set_trace()
        for fidx, funcstmt in enumerate(funcstmts):
            pfunc = funcstmt.parent
            idx = pfunc.subnodes.index(funcstmt)
            f = remove_subnode(pfunc, idx)
            insert_subnode(pnode, idxdo+fidx, f)

        #################
        # promote arrvars
        #################
        for arrvar in arrvars:
            if any(is_descendant(arrvar, fs) for fs in funcstmts):
                self.promote_array_var(arrvar)

        ###################
        # promote funccalls
        ###################
        for funccall in funccalls:
            self.promote_func_call(funccall, arr_actargs, global_vars)

        ########################
        # promote typedecl stmts
        ########################
        for tdecl in ptypedecls:
            promote_typedecl(tdecl)
        
        ####################################
        # process global variable promotions
        ####################################
        for gvar in global_vars:
            self.promote_global_var(gvar)

        self.add_forward(trees=self.analyzer_info["trees"])

    # collect all array variables used inside of the do loop
    def collect_array_vars(self, node):

        arrvars = []

        for name in collect_names(node):
            respath = self.respaths.get(name, None)

            if respath:     # NOTE: it "should" be "is" as newly created node can not be res0
                res0 = respath[0]
                resname = self.respaths[res0][-1]
                resstmt = get_parent_by_class(resname, Type_Declaration_Stmt)

                if resstmt:
                    if name not in self.typedecls:
                        self.typedecls[name] = resstmt

                    type_spec, attr_specs, entity_decls = resstmt.subnodes
                    is_array = False

                    if attr_specs:
                        dim_spec = get_attr_spec(attr_specs, Dimension_Attr_Spec)

                        if dim_spec:
                            is_array = True

                    if entity_decls:
                        entity_decl = get_entity_decl_by_name(entity_decls, resname)
                        
                        if entity_decl:
                            objname, array_spec, char_length, init = entity_decl.subnodes

                            if array_spec:
                                is_array = True

                            if is_array:
                                if isinstance(name.parent.wrapped, Part_Ref):
                                    for sname in collect_names(name.parent.subnodes[1]):
                                        if sname.wrapped == self.loopctr["dovar"].wrapped:
                                            if name not in arrvars:
                                                arrvars.append(name)
                                else:
                                    print("PTYPE: ", str(name.parent.wrapped))
                                    import pdb; pdb.set_trace()
        return arrvars

    # find direct sub-statement of the do loop
    def collect_func_stmt(self, node, donode):
        while node and hasattr(node, "parent") and node.parent is not donode:
            node = node.parent

        return node

    # collect names in function call, and names of arrvars used as actual arguemnts
#    def collect_func_calls(self, node, arrvars, arr_actargs):
#        
#        func_calls = []
#        names = collect_names(node)
#
#        for name in names:
#            if name in self.respaths:
#                resname = self.respaths[name][-1]
#
#                if is_function_name(resname):
#                    import pdb; pdb.set_trace()
#                    #argnames = collect_names(name.parent.subnodes[1])
#
#                    #if any(argname in arrvars for argname in argnames):
#                    #    func_calls.append(name)                
#
#                elif is_subroutine_name(resname):
#                    argnames = collect_names(name.parent.subnodes[1])
#                    actargs = []
#                    arr_actargs[name] = actargs
#
#                    for argname in argnames:
#                        if argname in arrvars:
#                            actargs.append(argname)
#                            if name not in func_calls:
#                                func_calls.append(name)                
#
#                elif is_interface_name(resname):
#                    import pdb; pdb.set_trace()
#
#                    names = collect_names(node)
#            else:
#                raise Exception("'%s' is not resolved."%str(name))
#
#        return func_calls
#


    def promote_func_call(self, funccall, arr_actargs, global_vars):

        actual_args = funccall.parent.subnodes[1]

        if isinstance(actual_args.wrapped, Actual_Arg_Spec_List):

            start = Actual_Arg_Spec(self.loopctr["start"].wrapped.tofortran())            
            stop = Actual_Arg_Spec(self.loopctr["stop"].wrapped.tofortran())            
            if self.loopctr["step"]:
                step = Actual_Arg_Spec(self.loopctr["step"].wrapped.tofortran())            
            else:
                step = Actual_Arg_Spec("1")

            insert_subnode(actual_args, 0, ConcreteSyntaxNode(actual_args, "expr", step))
            insert_subnode(actual_args, 0, ConcreteSyntaxNode(actual_args, "expr", stop))
            insert_subnode(actual_args, 0, ConcreteSyntaxNode(actual_args, "expr", start))
        else:
            import pdb; pdb.set_trace()

        if funccall in self.respaths:

            # if no search of the file that contains the funccall
            # do analyze search and resolve first 

            parent = self.get_proxy()
            funcname = self.respaths[funccall][-1]

            dummy_args = []

            argsvc = FparserArgument(funccall, funcname)

            for aarg in arr_actargs[funccall]:
                dummy_args.append(argsvc.actual2dummy(aarg, index_offset=3))

            forward = {
                "funcname": funcname,
                "promote": dummy_args,
                "loopcontrol": self.loopctr,
                "respaths": self.analyzer_info["respaths"],
                "invrespaths": self.analyzer_info["invrespaths"],
                "macros": self.macros,
                "includes": self.includes,
                "trees": self.analyzer_info["trees"],
                "modules": self.analyzer_info["modules"],
            }

            argv = []
            translator = LPTFunctionTranslator(parent)
            retval, _forward = translator.run(argv, forward=forward)

            self.analyzer_info["trees"].update(_forward["trees"])
            self.analyzer_info["modules"].update(_forward["modules"])
            self.analyzer_info["respaths"].update(_forward["respaths"])
            self.analyzer_info["invrespaths"].update(_forward["invrespaths"])

        else:
            raise Exception("Function name '%s' is not resolved yet." % str(funccall))

    def promote_array_var(self, arrvar):

        if isinstance(arrvar.parent.wrapped, Part_Ref):
            replace_dovar_with_section_subscript(arrvar.parent.subnodes[1],
                                                 self.loopctr)
        else:
            import pdb; pdb.set_trace()

#    def promote_vars(self, pvars, arrvars, funccalls, globalvars, ptypedecls):
#
#        # TODO: actual promotion happens here
#        for pvar in pvars:
#            import pdb; pdb.set_trace() 
#
#        # collect typedecls to be promoted
#        for pvar in pvars:
#            ptypedecl = self.get_promote_typedecl(pvar)
#            if ptypedecl not in ptypedecls:
#                ptypedecls.append(ptypedecl)
#
#        for ptypedecl in ptypedecls:
#            # collect vars whose typedecl is ptypedecl
#            pvars = self.collect_pvars_from_typedecl(ptypedecl)
#            self.promote_vars(pvars, arrvars, funccalls, globalvars, ptypedecls)
#
#
#        # if pvar is global variables, just keep it for later processing after finishing all local promotions
#        # need to collect function calls
#
#    def get_promote_typedecl(self, node):
#        import pdb; pdb.set_trace()
#
