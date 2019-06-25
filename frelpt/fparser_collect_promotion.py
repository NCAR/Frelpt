# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

# NOTE: if branching, copy respath list
# NOTE: use subnodes instead of content or items

import os
import pyloco

from fparser.two.Fortran2003 import *
from fparser.two.utils import *

from frelpt.node import IntrinsicProcedureNode 
from frelpt.fparser_util import collect_names, get_parent_by_class

# TODO: pvar collected if it is associated with pre-found pvars

class CollectVars4Promotion(pyloco.Task):
    """Collect variables to be promoted due to another promoted variable(s)
    """

    def _sort_keygen(self, **kwargs):
        def sortkey(self, node):
            return kwargs.get(node, 0)
        return sortkey

    def __init__(self, parent):

        self.add_data_argument("nodes", help="nodes that were decided to be promoted")
        self.add_option_argument("--respaths", recursive=True, help="resolution paths")
        self.add_option_argument("--invrespaths", recursive=True, help="inverted resolution paths")

        self.register_forward("pvars", help="nodes that should be promoted")

        self._sorts = {
            Program_Unit: self._sort_keygen(Comment=0, Main_Program=1, Module=2, External_Subprogram=3, Block_Data=4),
            Main_Program: self._sort_keygen(End_Program_Stmt=0, Specification_Part=1, Execution_Part=2, Internal_Subprogram_Part=3, Program_Stmt=4),
            Main_Program0: self._sort_keygen(End_Program0_Stmt=0, Specification_Part=1, Execution_Part=2, Internal_Subprogram_Part=3, Program_Stmt=4),
            Module: self._sort_keygen(End_Module_Stmt=0, Specification_Part=1, Module_Subprogram_Part=2, Module_Stmt=3),
            Block_Data: self._sort_keygen(End_Block_Data_Stmt=0, Specification_Part=1, Block_Data_Stmt=2),
            Specification_Part: self._sort_keygen(Implicit_Part=0, Use_Stmt=1, Import_Stmt=2, Declaration_Construct=3),
        }

        self.orgnode = None

    def perform(self, targs):

        pvars = []
        self.orgnodes = targs.nodes
        self.respaths = targs.respaths
        self.invrespaths = targs.invrespaths

        #for node in targs.nodes:
        #    self._collect(node, pvars, [], True)

        self.add_forward(pvars=pvars)

    def _collect(self, node, pvars, path, upward):

        self.log_debug("COLLECT4PROMO: %s, %s"%(node.__class__.__name__, str(node)))

        if node not in path:
            clsname = node.__class__.__name__

            if clsname.startswith("End_"):
                return

            path.append(node)

            getattr(self, "collect_"+clsname)(node, pvars, path, upward)

    def _bypass(self, node, pvars, path, upward):

        if upward:
            self._collect(node.parent, pvars, path, upward)

        else:
            self._subnode_collect(node, pvars, path)

    def _subnode_collect(self, node, pvars):

        for subnode in self._sorted_subnodes(node):
            if subnode not in path:
                newpath = list(path)

                self._collect(subnode, pvars, newpath, False)

    def _sorted_subnodes(self, node):
        if node in self._sorts:
            return sorted(node.subnodes, key=self._sorts[node], reverse=True)
        else:
            return node.subnodes

    def _is_var(self, name):

        if name in self.respaths:
            res = self.respaths[name][-1]
            return get_parent_by_class(res, Type_Declaration_Stmt)

        else:
            import pdb; pdb.set_trace()

    def collect_Actual_Arg_Spec_List(self, node, pvars, path, upward):
        pass

    def collect_Add_Operand(self, node, pvars, path, upward):

        if upward:
            if path[-2] is node.subnodes[0]:
                self._collect(node.subnodes[2], pvars, path, False)

            elif path[-2] is node.subnodes[2]:
                self._collect(node.subnodes[0], pvars, path, False)

            self._collect(node.parent, pvars, path, True)

        else:
            self._collect(node.subnodes[0], pvars, path, False)
            self._collect(node.subnodes[2], pvars, path, False)

    def collect_Assignment_Stmt(self, node, pvars, path, upward):

        if upward:
            if path[-2] is node.subnodes[0]:
                self._collect(node.subnodes[2], pvars, path, False)

            elif path[-2] is node.subnodes[2]:
                self._collect(node.subnodes[0], pvars, path, False)

        else:
            import pdb; pdb.set_trace()

    def collect_Call_Stmt(self, node, pvars, path, upward):
        pass

    def collect_Level_2_Expr(self, node, pvars, path, upward):

        if upward:
            if path[-2] is node.subnodes[0]:
                self._collect(node.subnodes[2], pvars, path, False)

            elif path[-2] is node.subnodes[2]:
                self._collect(node.subnodes[0], pvars, path, False)

            self._collect(node.parent, pvars, path, True)

        else:
            self._collect(node.subnodes[0], pvars, path, False)
            self._collect(node.subnodes[2], pvars, path, False)


    def collect_Name(self, node, pvars, path, upward):

        self.log_debug("In Name '%s': %s, %s, %s" % (str(node), str(node not in self.orgnodes),
                       str(node not in pvars), str(self._is_var(node))))
        if node not in self.orgnodes and node not in pvars and self._is_var(node):
            self.log_debug("Collected '%s'" % str(node))
            pvars.append(node)

        if upward:
            self._collect(node.parent, pvars, path, upward)

    def collect_Part_Ref(self, node, pvars, path, upward):

        if upward:
            name = node.subnodes[0]

            if name in self.respaths:
                resname = self.respaths[name][-1]
                parent = get_parent_by_class(resname, (Function_Stmt,
                                             Interface_Stmt))

                if parent is None:
                    if resname.__class__ not in (IntrinsicProcedureNode, ):
                        self._collect(node.parent, pvars, path, upward)

                elif isinstance(parent.wrapped, Function_Stmt):
                    self._collect(node.parent, pvars, path, upward)

                elif isinstance(parent.wrapped, Interface_Stmt):
                    import pdb; pdb.set_trace()
        else:
            self._collect(node.subnodes[0], pvars, path, upward)

#    def _parse(self, usenode, modname):
#
#        if modname.wrapped.string in self.modules:
#            return self.modules[modname.wrapped.string]
#
#        mypath = usenode.topnode().filepath
#
#        includes = self.includes.get(mypath, [])
#        for include in includes:
#            for entry in os.listdir(include):
#                base, ext = os.path.splitext(entry)
#                if ext in fortran_exts:
#                    filepath = os.path.join(include, entry)
#                    if filepath in self.includes and filepath not in self.trees:
#                        _macro = self.macros.get(filepath, {})                                    
#                        _include = self.includes.get(filepath, [])                                    
#                
#                        forward = {
#                            "macro" : dict(_macro),
#                            "include" : list(_include),
#                        }
#
#                        argv = [filepath]
#                        parser = Parser(self.get_proxy())
#                        retval, _forward = parser.run(argv, forward=forward)
#                            
#                        tree = _forward["tree"]
#                        self.trees[filepath] = tree
#
#                        self._add_modules(tree)
#
#                        # check if mod name exists
#                        for subnode in tree.subnodes:
#                            if isinstance(subnode.wrapped, Module):
#                                mod_stmt_node = subnode.subnodes[0]
#                                mod_stmt_name = mod_stmt_node.subnodes[1]
#                                if mod_stmt_name.wrapped.string not in self.modules:
#                                    self.modules[mod_stmt_name.wrapped.string] = subnode
#                                if is_name_equal(modname, mod_stmt_name):
#                                    return tree
#
#    def _sorted_subnodes(self, node):
#        if node in self._sorts:
#            return sorted(node.subnodes, key=self._sorts[node], reverse=True)
#        else:
#            return node.subnodes
#
#    def _add_modules(self, node):
#
#        for subnode in node.topnode().subnodes:
#            if isinstance(subnode.wrapped, Module):
#                mod_stmt_node = subnode.subnodes[0]
#                mod_stmt_name = mod_stmt_node.subnodes[1]
#                if mod_stmt_name.wrapped.string not in self.modules:
#                    self.modules[mod_stmt_name.wrapped.string] = subnode
#
#    def _analyze(self, respath):
#
#        for analyzer in self.analyzers:
#            import pdb ;pdb.set_trace()
#
#    def _resolve(self, node, res, path, upward):
#
#        if node not in path:
#            clsname = node.__class__.__name__
#            if clsname.startswith("End_"):
#                return
#            path.append(node)
#            if clsname.endswith("_List"):
#                return self._bypass(node, res, path, upward)
#            else:
#                return getattr(self, "resolve_"+clsname)(node, res, path, upward)
#
#    def _search_resolve(self, *nodes):
#        for n1 in nodes:
#            forward = { "node": n1 }
#            _, _fwd = self.searcher.run([], forward=forward)
#            for n2, res in _fwd["ids"].items():
#                self.run(n2, res)
#
#    def _subnode_resolve(self, node, res, path):
#
#        for subnode in self._sorted_subnodes(node):
#            if subnode not in path:
#                newpath = list(path)
#                if self._resolve(subnode, res, newpath, False):
#                    path.extend(newpath[len(path):])
#                    return True
#
#    def _bypass(self, node, res, path, upward):
#        if upward:
#            return self._resolve(node.parent, res, path, upward)
#        else:
#            return self._subnode_resolve(node, res, path)
#
#    def _resolve_implicit_rules(self, node, res, path, upward):
#        import pdb; pdb.set_trace()
#
#    def _resolve_intrinsics(self, node, res, path, upward):
#
#        if path[0].wrapped.string.lower() in Intrinsic_Procedures:
#            resnode = ConcreteSyntaxNode(None, ["expr"], path[0])
#            path.append(resnode)
#            return True
#
#    def resolve_Assignment_Stmt(self, node, res, path, upward):
#        if upward:
#            return self._resolve(node.parent, res, path, upward)
#
#    def resolve_Block_Nonlabel_Do_Construct(self, node, res, path, upward):
#        if upward:
#            return self._resolve(node.parent, res, path, upward)
# 
#    def resolve_Call_Stmt(self, node, res, path, upward):
#        if upward:
#            return self._resolve(node.parent, res, path, upward)
#
#    def resolve_Comment(self, node, res, path, upward):
#        pass
#
#    def resolve_Contains_Stmt(self, node, res, path, upward):
#        pass
#
#    def resolve_Entity_Decl(self, node, res, path, upward):
#        """
#        <entity-decl> = <object-name> [ ( <array-spec> ) ]
#            [ * <char-length> ] [ <initialization> ]
#                        | <function-name> [ * <char-length> ]
#        """
#
#        if upward:
#            import pdb; pdb.set_trace()
#
#        else:
#            name, array_spec, char_length, init = node.subnodes
#            if self._resolve(name, res, path, upward):
#                self._search_resolve(array_spec, char_length, init)
#                return True
#
#    def resolve_Execution_Part(self, node, res, path, upward):
#        if upward:
#            return self._resolve(node.parent, res, path, upward)
#
#    def resolve_Implicit_Stmt(self, node, res, path, upward):
#
#        if node not in res.implicit_rules:
#            res.implicit_rules.append(node)
#
#        if upward:
#            return self._resolve(node.parent, res, path, upward)
#
#    def resolve_Implicit_Part(self, node, res, path, upward):
#        return self._bypass(node, res, path, upward)
#
#    def resolve_Level_2_Expr(self, node, res, path, upward):
#        if upward:
#            return self._resolve(node.parent, res, path, upward)
#
#    def resolve_Loop_Control(self, node, res, path, upward):
#        if upward:
#            return self._resolve(node.parent, res, path, upward)
#
#    def resolve_Main_Program(self, node, res, path, upward):
#        """
#            <main-program> = <program-stmt>
#                         [ <specification-part> ]
#                         [ <execution-part> ]
#                         [ <internal-subprogram-part> ]
#                         <end-program-stmt>
#        """
#
#        if upward:
#            if self._subnode_resolve(node, res, path):
#                return True
#            return self._resolve(node.parent, res, path, upward)
#
#    def resolve_Module(self, node, res, path, upward):
#        """
#        <module> = <module-stmt>
#                       [ <specification-part> ]
#                       [ <module-subprogram-part> ]
#                       <end-module-stmt>
#        """
#        if upward:
#            import pdb; pdb.set_trace()
#        else:
#            # NOTE: mod should pass this if res can be part of Module
#            return self._subnode_resolve(node, res, path)
#
#    def resolve_Module_Stmt(self, node, res, path, upward):
#        """
#        <module-stmt> = MODULE <module-name>
#        """
#        if Module_Stmt in res:
#            return self._resolve(node.subnodes[1], res, path, upward)
#
#    def resolve_Module_Subprogram_Part(self, node, res, path, upward):
#        """
#        <module-subprogram-part> = <contains-stmt>
#                                       <module-subprogram>
#                                       [ <module-subprogram> ]...
#        """
#        return self._bypass(node, res, path, upward)
#
#    def resolve_Name(self, node, res, path, upward):
#        if upward: # start resolve
#            return self._resolve(node.parent, res, path, upward)
#        elif len(path) > 1 and is_name_equal(path[0], path[-1]):
#            return True
#
#    def resolve_Nonlabel_Do_Stmt(self, node, res, path, upward):
#        if upward:
#            return self._resolve(node.parent, res, path, upward)
#
#    def resolve_Part_Ref(self, node, res, path, upward):
#        """
#        <part-ref> = <part-name> [ ( <section-subscript-list> ) ]
#        """
#        if upward:
#            return self._resolve(node.parent, res, path, upward)
#
#    def resolve_Program(self, node, res, path, upward):
#        """
#        Fortran 2003 rule R201
#        program is program-unit
#                   [ program-unit ] ...
#        :F03R:`202`::
#            <program-unit> = <main-program>
#                             | <external-subprogram>
#                             | <module>
#                             | <block-data>
#        """
#        # TODO: handles resolution with external-subprogram, module, block-data
#        #import pdb; pdb.set_trace()
#        if upward:
#            # resolve with intrinsic names
#            if self._resolve_intrinsics(node, res, path, upward):
#                return True
#            elif self._resolve_implicit_rules(node, res, path, upward):
#                import pdb; pdb.set_trace()
#            else:
#                import pdb; pdb.set_trace()
#        else:
#            # from USE stmt
#            return self._subnode_resolve(node, res, path)
#
#    def resolve_Program_Stmt(self, node, res, path, upward):
#        if not upward and Program_Stmt in res:
#            return self._resolve(node.subnodes[1], res, path, upward)
#
#    def resolve_Subroutine_Stmt(self, node, res, path, upward):
#        """
#        <subroutine-stmt>
#        = [ <prefix> ] SUBROUTINE <subroutine-name>
#          [ ( [ <dummy-arg-list> ] ) [ <proc-language-binding-spec> ] ]
#        """
#        if upward:
#            return self._resolve(node.parent, res, path, upward)
#        elif Subroutine_Stmt in res:
#            prefix, name, dummy_args, binding_spec = node.subnodes
#            return self._resolve(name, res, path, upward)
#
#    def resolve_Subroutine_Subprogram(self, node, res, path, upward):
#        """
#        <subroutine-subprogram> = <subroutine-stmt>
#                                     [ <specification-part> ]
#                                     [ <execution-part> ]
#                                     [ <internal-subprogram-part> ]
#                                  <end-subroutine-stmt>
#        """
#        return self._bypass(node, res, path, upward)
#
#    def resolve_Specification_Part(self, node, res, path, upward):
#        # TODO: resolve with typedecl first? than use?
#        # typedecl .. -> use -> implicit
#        return self._subnode_resolve(node, res, path)
#
#    def resolve_Tuple(self, node, res, path, upward):
#        return self._bypass(node, res, path, upward)
#           
#    def resolve_Type_Declaration_Stmt(self, node, res, path, upward):
#        """<type-declaration-stmt> = <declaration-type-spec> [ [ ,
#                <attr-spec> ]... :: ] <entity-decl-list>
#
#        """
#
#        if upward:
#            # assuming self-resolve is handled by entity_decls
#            return self._resolve(node.parent, res, path, upward)
#        elif Type_Declaration_Stmt in res:
#            type_spec, attr_specs, entity_decls = node.subnodes
#            if self._resolve(entity_decls, res, path, upward):
#                self._search_resolve(type_spec, attr_specs)
#                return True
#
#    def resolve_Use_Stmt(self, node, res, path, upward):
#        """
#            Fortran 2003 rule R1109
#
#            use-stmt is USE [ [ , module-nature ] :: ] module-name [ , rename-list ]
#                    or USE [ [ , module-nature ] :: ] module-name ,
#                        ONLY : [ only-list ]
#        """
#
##        def _parse(usenode, modname):
##
##            if modname.wrapped.string in self.modules:
##                return self.modules[modname.wrapped.string]
##
##            mypath = usenode.topnode().filepath
##
##            includes = self.includes.get(mypath, [])
##            for include in includes:
##                for entry in os.listdir(include):
##                    base, ext = os.path.splitext(entry)
##                    if ext in fortran_exts:
##                        filepath = os.path.join(include, entry)
##                        if filepath in self.includes and filepath not in self.trees:
##                            _macro = self.macros.get(filepath, {})                                    
##                            _include = self.includes.get(filepath, [])                                    
##                    
##                            forward = {
##                                "target" : filepath,
##                                "macro" : dict(_macro),
##                                "include" : list(_include),
##                            }
##
##                            argv = ["Parser"]
##                            parser = Parser(pyloco.Manager())
##                            retval, _forward = parser.run(argv, forward, {})
##                                
##                            tree = _forward["tree"]
##                            self.trees[filepath] = tree
##
##                            self._add_modules(tree)
##
##                            # check if mod name exists
##                            for subnode in tree.subnodes:
##                                if isinstance(subnode.wrapped, Module):
##                                    mod_stmt_node = subnode.subnodes[0]
##                                    mod_stmt_name = mod_stmt_node.subnodes[1]
##                                    if mod_stmt_name.wrapped.string not in self.modules:
##                                        self.modules[mod_stmt_name.wrapped.string] = subnode
##                                    if is_name_equal(modname, mod_stmt_name):
##                                        return tree
#
#        if not upward:
#            mod_nature, dcolon, mod_name, only_spec, rename_onlylist = node.subnodes
#
#            topnode = None
#
#            if only_spec and "only" in only_spec.lower():
#                if isinstance(rename_onlylist.wrapped, Name):
#                    if is_name_equal(path[0], rename_onlylist):
#                        topnode = self._parse(node, mod_name)
#                    else:
#                        return
#                elif isinstance(rename_onlylist.wrapped, Only_List):
#                    for only in rename_onlylist.subnodes:
#                        if isinstance(only.wrapped, Name):
#                            if is_name_equal(path[0], only):
#                                topnode = self._parse(node, mod_name)
#                                break
#                        else:
#                            import pdb; pdb.set_trace()
#                else:
#                    import pdb; pdb.set_trace()
#            else:
#                topnode = self._parse(node, mod_name)
#                # read 
#
#            if topnode:
#                return self._resolve(topnode, res, path, False)
#
##        check if module exists in pre-parsed modules in self.modules
##        check include paths and find all source files and check module name
##        parse new module
#        
