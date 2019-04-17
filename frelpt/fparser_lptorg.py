# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

import os
import pyloco

from fparser.two.Fortran2003 import *
from fparser.two.utils import *
from frelpt.fparser_util import (collect_names, get_parent_by_class, get_entity_decl_by_name,
                get_attr_spec, collect_nodes_by_class, is_function_name, is_subroutine_name,
                is_interface_name)

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

        return True

class LPTOrgTranslator(pyloco.Task):

    def __init__(self, parent):

        self.add_data_argument("donode", required=True, help="org do node for loop pushdown translation")
        self.add_data_argument("subnodes", required=True, help="org do subnodes for loop pushdown translation")
        self.add_data_argument("loopcontrol", required=True, help="org do loop control node for loop pushdown translation")
        self.add_data_argument("trees", required=True, help="all abstract syntax trees")
        self.add_data_argument("modules", required=True, help="all modules")
        self.add_data_argument("respaths", required=True, help="a list of resolution path")
        self.add_data_argument("invrespaths", required=True, help="a inverse list of resolution path")

        #evaluate=True, parameter_parse=True
        self.add_option_argument("-o", "--outdir", default=os.getcwd(), help="output directory")

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

    def collect_func_calls(self, node, arrvars):
        
        func_calls = []
        names = collect_names(node)

        for name in names:
            if name in self.respaths:
                resname = self.respaths[name][-1]

                if is_function_name(resname):
                    import pdb; pdb.set_trace()
                    #argnames = collect_names(name.parent.subnodes[1])

                    #if any(argname in arrvars for argname in argnames):
                    #    func_calls.append(name)                

                elif is_subroutine_name(resname):
                    argnames = collect_names(name.parent.subnodes[1])

                    if any(argname in arrvars for argname in argnames):
                        func_calls.append(name)                

                elif is_interface_name(resname):
                    import pdb; pdb.set_trace()

                    names = collect_names(node)
            else:
                raise Exception("'%s' is not resolved."%str(name))

        return func_calls

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

        self.loopctr = targs.loopcontrol
        self.trees = targs.trees
        self.modules = targs.modules
        self.respaths = targs.respaths
        self.invrespaths = targs.invrespaths
        self.typedecls = {}

        # collect arrvars to be promoted
        arrvars = []
        for subnode in targs.subnodes:
            pvars = self.collect_array_vars(subnode)
            for pvar in pvars: 
                if pvar not in arrvars:
                    arrvars.append(pvar)

        # collect function calls that propagetes promotion
        funccalls = []
        for subnode in targs.subnodes:
            funccalls.extend(self.collect_func_calls(subnode, arrvars))

        # NOTE: generate promotion even if promotion is already done so that promotion compatibility can be checked.
        import pdb; pdb.set_trace()

        target_vars = []
        for arrvar in arrvars:
            pvars = self.collect_pvars_from_arrvar(arrvar)
            for pvar in pvars:
                if pvar not in target_vars:
                    target_vars.append(var)

        globalvars = []

        # promote vars 
        global_vars = self.promote_vars(target_vars, arrvars, funccalls, globalvars)

        # promote arrvars
        for arrvar in arrvars:
            self.promote_array_var(arrvar)

        for funccall in funccalls:
            gvars = self.promote_func_call(funcall)
            global_vars.extend(gvars)

        # promote typedecl stmts
        for tdecl in typedecls:
            self.promote_typedecl(tdecl)

        # reposition this subnode to just above of do node
        pnode = targs.donode.parent
        idxdo = pnode.subnodes.index(targs.donode)
        pnode.subnodes.pop(idxdo)

        for subnode in targs.subnodes:
            pnode.subnodes.insert(idxdo, subnode) 
        
        # process global variable promotions
        for gvar in globalvars:
            self.promote_global_var(gvar)

        import pdb; pdb.set_trace()

    def promote_vars(self, pvars, arrvars, funccalls, globalvars):

        # actual promotion happens here
        import pdb; pdb.set_trace() 



        # collect typedecls to be promoted
        ptypedecls = []
        for pvar in pvars:
            ptypedecl = self.get_promote_typedecl(pvar)
            if ptypedecl not in ptypedecls:
                ptypedecls.append(ptypedecl)

        for ptypedecl in ptypedecls:

            # collect vars whose typedecl is ptypedecl
            pvars = self.collect_pvars_from_typedecl(ptypedecl)
            self.promote_vars(pvars, arrvars, funccalls, globalvars)


        # if pvar is global variables, just keep it for later processing after finishing all local promotions
        # need to collect function calls
