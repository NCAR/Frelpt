# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

import os
import pyloco

from fparser.two.Fortran2003 import *
from fparser.two.utils import *


def collect_do_loopcontrol(node, bag, depth):

    if hasattr(node, "wrapped") and isinstance(node.wrapped, Loop_Control):

        scalar_logical_expr, counter_expr, optional_delim = node.wrapped.items

        if scalar_logical_expr is not None:
            raise Exception("'WHILE' loop is not supported for loop pushdown translation.")

        bag["dovar"] = counter_expr[0]
        bag["start"], bag["stop"] = counter_expr[1][:2]
        if len(counter_expr[1]) > 2:
            bag["step"] = counter_expr[1][2] if len(counter_expr[1]) > 2 else None

        return True

class LPTOrgTranslator(pyloco.Task):

    def __init__(self, parent):

        self.add_data_argument("donode", required=True, help="org do node for loop pushdown translation")
        self.add_data_argument("subnodes", required=True, help="org do subnodes for loop pushdown translation")
        self.add_data_argument("loopcontrol", required=True, help="org do loop control node for loop pushdown translation")

        #evaluate=True, parameter_parse=True
        self.add_option_argument("-o", "--outdir", default=os.getcwd(), help="output directory")

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

        target_vars = []
        for arrvar in arrvars:
            pvars = self.collect_promote_vars(arrvar)
            for pvar in pvars:
                if pvar not in target_vars:
                    target_vars.append(var)

        # promote vars 
        global_vars = self.promote_vars(target_vars)

        # promote arrvars
        for arrvar in arrvars:
            self.promote_array_var(arrvar)

        for funccall in funccalls:
            gvars = self.promote_func_call(funcall)
            global_vars.extend(gvars)

        # reposition this subnode to just above of do node
        pnode = targs.donode.parent
        idxdo = pnode.subnodes.index(targs.donode)
        pnode.subnodes.pop(idxdo)

        for subnode in targs.subnodes:
            pnode.subnodes.insert(idxdo, subnode) 
        
        # process global variable promotions
        for gvar in global_vars:
            self.promote_global_var(gvar)

        import pdb; pdb.set_trace()

    def promote_vars(self):

        # collect typedecls to be promoted
        ptypedecls = []
        for arrvar in arrvars:
            pvars = self.collect_promote_vars(arrvar)
            for pvar in pvars:
                ptypedecl = self.get_promote_typedecl(pvar)
                if ptypedecl not in ptypedecls:
                    ptypedecls.append(ptypedecl)

        for ptypedecl in ptypedecls:

            # collect vars whose typedecl is ptypedecl
            pvars = self.collect_promote_vars()

            for pvar in pvars: 

                # promote pvars

                # collect cascading pvars that needs promotion due to pvar

                # collect typedecs for the new pvars and repeat....



        # if pvar is global variables, just keep it for later processing after finishing all local promotions
        # need to collect function calls

