# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

from frelpt.fparser_util import (get_stmt_parent, collect_names, get_typedecl_parent,
        wrap_stmt_with_doblock, get_partref, replace_subnode, is_partref, is_entitydecl,
        is_dtype_name, is_procedure_name, is_section_subscript_list, is_name)

# pushdown originating source file
class FrelptTransBase(object):

    # NOTE: assumes self.respaths exist
    def _is_var(self, name):

        if name in self.respaths:
            res = self.respaths[name][-1]
            return get_typedecl_parent(res)

        else:
            import pdb; pdb.set_trace()

#    def get_sec_sub_string(self, name):
#
#        tdecl_section = None
#
#        # get section from typedecl stmt
#        if is_partref(name.parent):
#            if name.parent.subnodes[1] in self.respaths:
#                res = self.respaths[name.parent.subnodes[1]][-1]
#
#                if is_entitydecl(res.parent):
#                    if res.parent.subnodes[1]:
#                        tdecl_section = [":"] * (len(res.parent.subnodes[1].wrapped.items) - 1)
#                else:
#                    tdecl = get_typedecl_parent(res)
#                    if tdecl:
#                        import pdb; pdb.set_trace()
#
#                    else:
#                        import pdb; pdb.set_trace()
#        else:
#            res = self.respaths[name][-1]
#            if is_entitydecl(res.parent):
#                if res.parent.subnodes[1]:
#                    tdecl_section = [":"] * (len(res.parent.subnodes[1].wrapped.items) - 1)
#            else:
#                tdecl = get_typedecl_parent(res)
#                if tdecl:
#                    import pdb; pdb.set_trace()
#                else:
#                    import pdb; pdb.set_trace()
#
#        # get section from exec stmt
#        exec_section = None
#
#        if is_partref(name.parent):
#            if name.parent.subnodes[0] in self.respaths:
#                res = self.respaths[name.parent.subnodes[0]][-1]
#                tdecl = get_typedecl_parent(res)
#                if tdecl:
#                    import pdb; pdb.set_trace()
#            else:
#                import pdb; pdb.set_trace()
#        
#
#        if tdecl_section and exec_section:
#            import pdb ;pdb.set_trace()
#
#        elif tdecl_section:
#            return ", " + ", ".join(tdecl_section)
#
#        elif exec_section:
#            import pdb ;pdb.set_trace()
#
#        else:
#            return ""

    def get_sec_sub_string(self, name):

        tdecl_section = None

        # get section from typedecl stmt
        if is_partref(name.parent) and name.parent.subnodes[0] is name:
            if name.parent.subnodes[0] in self.respaths:
                res = self.respaths[name.parent.subnodes[0]][-1]

                if is_entitydecl(res.parent):
                    if res.parent.subnodes[1]:
                        tdecl_section = [":"] * (len(res.parent.subnodes[1].wrapped.items) - 1)
                else:
                    tdecl = get_typedecl_parent(res)
                    if tdecl:
                        print("A", tdecl)
                        import pdb; pdb.set_trace()
        else:
            res = self.respaths[name][-1]
            if is_entitydecl(res.parent):
                if res.parent.subnodes[1]:
                    tdecl_section = [":"] * (len(res.parent.subnodes[1].wrapped.items) - 1)
            else:
                tdecl = get_typedecl_parent(res)
                if tdecl:
                    print("C", tdecl)
                    import pdb; pdb.set_trace()

        # get section from exec stmt
        exec_section = None

        if is_partref(name.parent) and name.parent.subnodes[0] is name:
            if name.parent.subnodes[0] in self.respaths:
                res = self.respaths[name.parent.subnodes[0]][-1]
                if is_entitydecl(res.parent):
                    if res.parent.subnodes[1]:
                        exec_section = name.parent.subnodes[1]
                else:
                    tdecl = get_typedecl_parent(res)
                    if tdecl:
                        print("E", tdecl)
                        import pdb; pdb.set_trace()

        if tdecl_section and exec_section:

            if is_section_subscript_list(exec_section):
                import pdb ;pdb.set_trace()

            else:
                return ", " + ", ".join(tdecl_section[1:]+[str(exec_section)])

        elif tdecl_section:
            return ", " + ", ".join(tdecl_section)

        elif exec_section:
            import pdb ;pdb.set_trace()

        else:
            return ""

    def promote_vars(self, pvars, arrvars, funccalls, globalvars, ptypedecls):

        # TODO: actual promotion happens here
        # TODO: add DO loop
        # TODO: promote all promotable variables

        # TODO: combine all pvars in a statement
        # TODO: promote all promotable variables in a one combinaiton
        # TODO: add arrvars to be promoted

        stmts = {}
        for pvar in pvars:
            stmt = get_stmt_parent(pvar)
            if stmt:
                if stmt in stmts:
                    stmts[stmt].append(pvar)

                else:
                    stmts[stmt] = [pvar]
                
        for stmt, stmt_pvars in stmts.items():

            # collect all names
            variables = {}

            for name in collect_names(stmt):
                if self._is_var(name) and name in stmt_pvars:
                    sub = self.get_sec_sub_string(name)
                    variables[name] = sub

            if any((v not in stmt_pvars) for v in variables):
                self.log_debug("Not all variable is classified as pvar: (%s, %s)" % (str(stmt_pvars), str(variables)))
                import pdb; pdb.set_trace()

            # wrap stmt in do loop
            # NOTE: assumes self.loopctr exists
            # update variables in stmt with Part_Ref
            for variable, sub in variables.items():
                if is_partref(variable.parent) and self._is_var(variable.parent.subnodes[0]):
                    parent = variable.parent.parent
                    subnode = variable.parent
                else:
                    parent = variable.parent
                    subnode = variable

                pref = get_partref(parent, "%s(frelpt_index%s)" % (str(variable), sub))
                var_index = parent.subnodes.index(subnode)
                replace_subnode(parent, var_index, pref)

            wrap_stmt_with_doblock(stmt, self.loopctr)

        # collect typedecls to be promoted

        ptypedecl = {}
        new_pvars = []

        for pvar in new_pvars:
            ptypedecl = self.get_promote_typedecl(pvar)
            if pvar in ptypedecl:
                import pdb; pdb.set_trace() # ERROR?
            else:
                ptypedecls[pvar] = ptypedecl

        for pvar, ptypedecl in ptypedecls.items():
            # collect vars whose typedecl is ptypedecl
            pvars = self.collect_pvars_from_typedecl(ptypedecl)
            self.promote_vars(pvars, arrvars, funccalls, globalvars, ptypedecls)


        # if pvar is global variables, just keep it for later processing after finishing all local promotions
        # need to collect function calls

    def get_promote_typedecl(self, node):
        import pdb; pdb.set_trace()

    def promote_func_call(self, callee, actual_args, arg_indices, global_vars, index_offset=0):

        dummy_args = []

        for idx, arg in enumerate(callee.parent.subnodes[2].subnodes):
            if idx-index_offset in arg_indices:
                dummy_args.append(arg)

        parent = self.get_proxy()

        forward = {
            "funcname": callee,
            "promote": dummy_args,
            "loopcontrol": self.loopctr,
            "respaths": self.respaths,
            "invrespaths": self.invrespaths,
            "macros": self.macros,
            "includes": self.includes,
            "trees": self.trees,
            "modules": self.modules,
        }

        argv = []
        translator = self.__class__(parent)
        retval, _forward = translator.run(argv, forward=forward)

        #import pdb; pdb.set_trace()

    def process_dataref(self, dref, prefs, actual_args, arg_indices, global_vars):

        if not prefs:
            return dref

        funccall = None

        if dref in self.respaths:
            resname = self.respaths[dref][-1]
            tdecl = get_typedecl_parent(resname)
            tdeclspec = tdecl.subnodes[0]
            if tdeclspec.subnodes[0] == "TYPE":
                if len(tdeclspec.subnodes[1].subnodes) == 1:
                    dtypename = tdeclspec.subnodes[1].subnodes[0]
                    resdtypename = self.respaths[dtypename][-1]
                    if is_dtype_name(resdtypename):
                        dtypedef = resdtypename.parent.parent.parent
                        names = collect_names(dtypedef)
                        for name in names:
                            if prefs[0].wrapped == name.wrapped:
                                # found bound procedure
                                resprocname = self.respaths[name][-1]
                                if is_procedure_name(resprocname):
                                    return self.promote_func_call(resprocname, actual_args, arg_indices, global_vars, index_offset=1)

                                elif len(prefs) > 1:
                                    return self.process_dataref(prefs[0], prefs[1:], actual_args, arg_indices, global_vars, index_offset=1)

                                else:
                                    import pdb; pdb.set_trace()
                        # bound procedure is not found
                        import pdb; pdb.set_trace()
                    else:
                        import pdb; pdb.set_trace()
                else:
                    import pdb; pdb.set_trace()
            else:
                import pdb; pdb.set_trace()
        else:
            import pdb; pdb.set_trace()

        return funccall
