# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

# NOTE: if branching, copy respath list
# NOTE: use subnodes instead of content or items

import os
import pyloco

from fparser.two.Fortran2003 import *
from fparser.two.utils import *


from frelpt.fparser_parse import Parser
from frelpt.fparser_search import Searcher
from frelpt.util import is_name_equal

fortran_exts = [".f", ".f90", ".f95", ".f03", ".F", ".F90", ".F95", ".F03"]

# TODO: resoving order within subnodes
#     - exec part -> spec part
#     - .. -> typedecl -> use
# TODO: spec stmt does not resolve, but add node in resolution related bag

class Resolver(pyloco.Task):

    def _sort_keygen(self, **kwargs):
        def sortkey(self, node):
            return kwargs.get(node, 0)
        return sortkey

    def __init__(self, parent):

        self.add_data_argument("node", help="node to search")
        self.add_data_argument("resolvers", help="a list of candidate resolvers")
        self.add_data_argument("macros", help="macro definitions")
        self.add_data_argument("includes", help="include paths")
        self.add_data_argument("analyzers", help="in-search analyzers")
        self.add_data_argument("searcher", help="identifier searcher")

        self.add_option_argument("--trees", recursive=True, help="ast tree of source files")
        self.add_option_argument("--modules", recursive=True, help="identifier searcher")
        self.add_option_argument("--respaths", recursive=True, help="identifier searcher")
        self.add_option_argument("--invrespaths", recursive=True, help="identifier searcher")

        self.register_forward("trees", help="AST trees")
        self.register_forward("modules", help="module ASTs")
        self.register_forward("respaths", help="resolution paths")
        self.register_forward("invrespaths", help="inverted resolution paths")

        # sort priorities: decending order
        self._sorts = {
            Program_Unit: self._sort_keygen(Comment=0, Main_Program=1, Module=2, External_Subprogram=3, Block_Data=4),
            Main_Program: self._sort_keygen(End_Program_Stmt=0, Specification_Part=1, Execution_Part=2, Internal_Subprogram_Part=3, Program_Stmt=4),
            Main_Program0: self._sort_keygen(End_Program0_Stmt=0, Specification_Part=1, Execution_Part=2, Internal_Subprogram_Part=3, Program_Stmt=4),
            Module: self._sort_keygen(End_Module_Stmt=0, Specification_Part=1, Module_Subprogram_Part=2, Module_Stmt=3),
            Block_Data: self._sort_keygen(End_Block_Data_Stmt=0, Specification_Part=1, Block_Data_Stmt=2),
            Specification_Part: self._sort_keygen(Implicit_Part=0, Use_Stmt=1, Import_Stmt=2, Declaration_Construct=3),
        }

        # TODO: add more sorted nodes

#
#Execution_Part_Construct : Comment', 'Executable_Construct', 'Format_Stmt',
#                      'Entry_Stmt', 'Data_Stmt']
#
#Executable_Construct_C201'
#'Action_Stmt_C201', 'Associate_Stmt', 'Case_Construct', 'Comment',
#        'Do_Construct', 'Forall_Construct', 'If_Construct',
#        'Select_Type_Construct', 'Where_Construct'
#
#'Execution_Part_Construct_C201 : 'Comment', 'Executable_Construct_C201', 'Format_Stmt',
#                      'Entry_Stmt', 'Data_Stmt']
#
#Executable_Construct: 'Action_Stmt', 'Associate_Stmt', 'Case_Construct', 'Comment',
#        'Do_Construct', 'Forall_Construct', 'If_Construct',
#        'Select_Type_Construct', 'Where_Construct'
#
#Action_Stmt : 
#'Allocate_Stmt', 'Assignment_Stmt', 'Backspace_Stmt',
#                      'Call_Stmt', 'Close_Stmt', 'Comment', 'Continue_Stmt',
#                      'Cycle_Stmt', 'Deallocate_Stmt', 'Endfile_Stmt',
#                      'End_Function_Stmt', 'End_Subroutine_Stmt', 'Exit_Stmt',
#                      'Flush_Stmt', 'Forall_Stmt', 'Goto_Stmt', 'If_Stmt',
#                      'Inquire_Stmt', 'Nullify_Stmt', 'Open_Stmt',
#                      'Pointer_Assignment_Stmt', 'Print_Stmt', 'Read_Stmt',
#                      'Return_Stmt', 'Rewind_Stmt', 'Stop_Stmt', 'Wait_Stmt',
#                      'Where_Stmt', 'Write_Stmt', 'Arithmetic_If_Stmt',
#                      'Computed_Goto_Stmt'
#
#
#Action_Stmt_C201:
#'Allocate_Stmt', 'Assignment_Stmt', 'Backspace_Stmt',
#                      'Call_Stmt', 'Close_Stmt', 'Comment', 'Continue_Stmt',
#                      'Cycle_Stmt', 'Deallocate_Stmt', 'Endfile_Stmt',
#                      'Exit_Stmt',
#                      'Flush_Stmt', 'Forall_Stmt', 'Goto_Stmt', 'If_Stmt',
#                      'Inquire_Stmt', 'Nullify_Stmt', 'Open_Stmt',
#                      'Pointer_Assignment_Stmt', 'Print_Stmt', 'Read_Stmt',
#                      'Return_Stmt', 'Rewind_Stmt', 'Stop_Stmt', 'Wait_Stmt',
#                      'Where_Stmt', 'Write_Stmt', 'Arithmetic_If_Stmt',
#                      'Computed_Goto_Stmt'
#

    subclass_names = []
    use_names = ['Block_Data_Stmt', 'Specification_Part',
                 'End_Block_Data_Stmt']
    def perform(self, targs):

        self.macros = targs.macros
        self.includes = targs.includes
        self.analyzers = targs.analyzers
        self.resolvers = targs.resolvers
        self.searcher = targs.searcher

        self.trees = targs.trees if targs.trees else {}
        self.modules = targs.modules if targs.modules else {}
        self.respaths = targs.respaths if targs.respaths else {}
        self.invrespaths = targs.invrespaths if targs.invrespaths else {}

        self._add_modules(targs.node)

        respath = []

        self.respaths[targs.node] = respath

        self.log_debug("Resolving '%s'"%str(targs.node.wrapped))

        if self._resolve(targs.node, targs.resolvers, respath, True):
            self.invrespaths[respath[-1]] = respath
            self._analyze(respath)
            self.log_debug(str(respath))
        else:
            # try implicit 
            import pdb; pdb.set_trace()

        self.add_forward(trees=self.trees)
        self.add_forward(modules=self.modules)
        self.add_forward(respaths=self.respaths)
        self.add_forward(invrespaths=self.invrespaths)

    def _parse(self, usenode, modname):

        if modname.wrapped.string in self.modules:
            return self.modules[modname.wrapped.string]

        mypath = usenode.topnode().filepath

        includes = self.includes.get(mypath, [])
        for include in includes:
            for entry in os.listdir(include):
                base, ext = os.path.splitext(entry)
                if ext in fortran_exts:
                    filepath = os.path.join(include, entry)
                    if filepath in self.includes and filepath not in self.trees:
                        _macro = self.macros.get(filepath, {})                                    
                        _include = self.includes.get(filepath, [])                                    
                
                        forward = {
                            "macro" : dict(_macro),
                            "include" : list(_include),
                        }

                        argv = [filepath]
                        parser = Parser(self.get_proxy())
                        retval, _forward = parser.run(argv, forward=forward)
                            
                        tree = _forward["tree"]
                        self.trees[filepath] = tree

                        self._add_modules(tree)

                        # check if mod name exists
                        for subnode in tree.subnodes:
                            if isinstance(subnode.wrapped, Module):
                                mod_stmt_node = subnode.subnodes[0]
                                mod_stmt_name = mod_stmt_node.subnodes[1]
                                if mod_stmt_name.wrapped.string not in self.modules:
                                    self.modules[mod_stmt_name.wrapped.string] = subnode
                                if is_name_equal(modname, mod_stmt_name):
                                    return tree

    def _sorted_subnodes(self, node):
        if node in self._sorts:
            return sorted(node.subnodes, key=self._sorts[node], reverse=True)
        else:
            return node.subnodes

    def _add_modules(self, node):

        for subnode in node.topnode().subnodes:
            if isinstance(subnode.wrapped, Module):
                mod_stmt_node = subnode.subnodes[0]
                mod_stmt_name = mod_stmt_node.subnodes[1]
                if mod_stmt_name.wrapped.string not in self.modules:
                    self.modules[mod_stmt_name.wrapped.string] = subnode

    def _analyze(self, respath):

        for analyzer in self.analyzers:
            import pdb ;pdb.set_trace()

    def _resolve(self, node, res, path, upward):

        if node not in path:
            clsname = node.__class__.__name__
            if clsname.startswith("End_"):
                return
            path.append(node)
            if clsname.endswith("_List"):
                return self._bypass(node, res, path, upward)
            else:
                return getattr(self, "resolve_"+clsname)(node, res, path, upward)

    def _search_resolve(self, *nodes):
        for n1 in nodes:
            forward = { "node": n1 }
            _, _fwd = self.searcher.run([], forward=forward)
            for n2, res in _fwd["ids"].items():
                self.run(n2, res)

    def _subnode_resolve(self, node, res, path):

        for subnode in self._sorted_subnodes(node):
            if subnode not in path:
                newpath = list(path)
                if self._resolve(subnode, res, newpath, False):
                    path.extend(newpath[len(path):])
                    return True

    def _bypass(self, node, res, path, upward):
        if upward:
            return self._resolve(node.parent, res, path, upward)
        else:
            return self._subnode_resolve(node, res, path)

    def resolve_Block_Nonlabel_Do_Construct(self, node, res, path, upward):
        if upward:
            return self._resolve(node.parent, res, path, upward)
 
    def resolve_Call_Stmt(self, node, res, path, upward):
        if upward:
            return self._resolve(node.parent, res, path, upward)

    def resolve_Comment(self, node, res, path, upward):
        pass

    def resolve_Contains_Stmt(self, node, res, path, upward):
        pass

    def resolve_Entity_Decl(self, node, res, path, upward):
        """
        <entity-decl> = <object-name> [ ( <array-spec> ) ]
            [ * <char-length> ] [ <initialization> ]
                        | <function-name> [ * <char-length> ]
        """

        if upward:
            import pdb; pdb.set_trace()

        else:
            name, array_spec, char_length, init = node.subnodes
            if self._resolve(name, res, path, upward):
                self._search_resolve(array_spec, char_length, init)
                return True

    def resolve_Execution_Part(self, node, res, path, upward):
        if upward:
            return self._resolve(node.parent, res, path, upward)

    def resolve_Implicit_Stmt(self, node, res, path, upward):

        if node not in res.implicit_rules:
            res.implicit_rules.append(node)

        if upward:
            return self._resolve(node.parent, res, path, upward)

    def resolve_Implicit_Part(self, node, res, path, upward):
        return self._bypass(node, res, path, upward)

    def resolve_Loop_Control(self, node, res, path, upward):
        if upward:
            return self._resolve(node.parent, res, path, upward)

    def resolve_Main_Program(self, node, res, path, upward):
        """
            <main-program> = <program-stmt>
                         [ <specification-part> ]
                         [ <execution-part> ]
                         [ <internal-subprogram-part> ]
                         <end-program-stmt>
        """

        if upward:
            if self._subnode_resolve(node, res, path):
                return True
            return self._resolve(node.parent, res, path, upward)

    def resolve_Module(self, node, res, path, upward):
        """
        <module> = <module-stmt>
                       [ <specification-part> ]
                       [ <module-subprogram-part> ]
                       <end-module-stmt>
        """
        if upward:
            import pdb; pdb.set_trace()
        else:
            # NOTE: mod should pass this if res can be part of Module
            return self._subnode_resolve(node, res, path)

    def resolve_Module_Stmt(self, node, res, path, upward):
        """
        <module-stmt> = MODULE <module-name>
        """
        if Module_Stmt in res:
            return self._resolve(node.subnodes[1], res, path, upward)

    def resolve_Module_Subprogram_Part(self, node, res, path, upward):
        """
        <module-subprogram-part> = <contains-stmt>
                                       <module-subprogram>
                                       [ <module-subprogram> ]...
        """
        return self._bypass(node, res, path, upward)

    def resolve_Name(self, node, res, path, upward):
        if upward: # start resolve
            return self._resolve(node.parent, res, path, upward)
        elif len(path) > 1 and is_name_equal(path[0], path[-1]):
            return True

    def resolve_Nonlabel_Do_Stmt(self, node, res, path, upward):
        if upward:
            return self._resolve(node.parent, res, path, upward)

    def resolve_Part_Ref(self, node, res, path, upward):
        """
        <part-ref> = <part-name> [ ( <section-subscript-list> ) ]
        """
        if upward:
            return self._resolve(node.parent, res, path, upward)

    def resolve_Program(self, node, res, path, upward):
        """
        Fortran 2003 rule R201
        program is program-unit
                   [ program-unit ] ...
        :F03R:`202`::
            <program-unit> = <main-program>
                             | <external-subprogram>
                             | <module>
                             | <block-data>
        """
        # TODO: handles resolution with external-subprogram, module, block-data
        #import pdb; pdb.set_trace()
        if upward:
            import pdb; pdb.set_trace()
        else:
            # from USE stmt
            return self._subnode_resolve(node, res, path)

    def resolve_Program_Stmt(self, node, res, path, upward):
        if not upward and Program_Stmt in res:
            return self._resolve(node.subnodes[1], res, path, upward)

    def resolve_Subroutine_Stmt(self, node, res, path, upward):
        """
        <subroutine-stmt>
        = [ <prefix> ] SUBROUTINE <subroutine-name>
          [ ( [ <dummy-arg-list> ] ) [ <proc-language-binding-spec> ] ]
        """
        if upward:
            return self._resolve(node.parent, res, path, upward)
        elif Subroutine_Stmt in res:
            prefix, name, dummy_args, binding_spec = node.subnodes
            return self._resolve(name, res, path, upward)

    def resolve_Subroutine_Subprogram(self, node, res, path, upward):
        """
        <subroutine-subprogram> = <subroutine-stmt>
                                     [ <specification-part> ]
                                     [ <execution-part> ]
                                     [ <internal-subprogram-part> ]
                                  <end-subroutine-stmt>
        """
        return self._bypass(node, res, path, upward)

    def resolve_Specification_Part(self, node, res, path, upward):
        # TODO: resolve with typedecl first? than use?
        # typedecl .. -> use -> implicit
        return self._subnode_resolve(node, res, path)

    def resolve_Tuple(self, node, res, path, upward):
        return self._bypass(node, res, path, upward)
           
    def resolve_Type_Declaration_Stmt(self, node, res, path, upward):
        """<type-declaration-stmt> = <declaration-type-spec> [ [ ,
                <attr-spec> ]... :: ] <entity-decl-list>

        """

        if upward:
            # assuming self-resolve is handled by entity_decls
            return self._resolve(node.parent, res, path, upward)
        elif Type_Declaration_Stmt in res:
            type_spec, attr_specs, entity_decls = node.subnodes
            if self._resolve(entity_decls, res, path, upward):
                self._search_resolve(type_spec, attr_specs)
                return True

    def resolve_Use_Stmt(self, node, res, path, upward):
        """
            Fortran 2003 rule R1109

            use-stmt is USE [ [ , module-nature ] :: ] module-name [ , rename-list ]
                    or USE [ [ , module-nature ] :: ] module-name ,
                        ONLY : [ only-list ]
        """

#        def _parse(usenode, modname):
#
#            if modname.wrapped.string in self.modules:
#                return self.modules[modname.wrapped.string]
#
#            mypath = usenode.topnode().filepath
#
#            includes = self.includes.get(mypath, [])
#            for include in includes:
#                for entry in os.listdir(include):
#                    base, ext = os.path.splitext(entry)
#                    if ext in fortran_exts:
#                        filepath = os.path.join(include, entry)
#                        if filepath in self.includes and filepath not in self.trees:
#                            _macro = self.macros.get(filepath, {})                                    
#                            _include = self.includes.get(filepath, [])                                    
#                    
#                            forward = {
#                                "target" : filepath,
#                                "macro" : dict(_macro),
#                                "include" : list(_include),
#                            }
#
#                            argv = ["Parser"]
#                            parser = Parser(pyloco.Manager())
#                            retval, _forward = parser.run(argv, forward, {})
#                                
#                            tree = _forward["tree"]
#                            self.trees[filepath] = tree
#
#                            self._add_modules(tree)
#
#                            # check if mod name exists
#                            for subnode in tree.subnodes:
#                                if isinstance(subnode.wrapped, Module):
#                                    mod_stmt_node = subnode.subnodes[0]
#                                    mod_stmt_name = mod_stmt_node.subnodes[1]
#                                    if mod_stmt_name.wrapped.string not in self.modules:
#                                        self.modules[mod_stmt_name.wrapped.string] = subnode
#                                    if is_name_equal(modname, mod_stmt_name):
#                                        return tree

        if not upward:
            mod_nature, dcolon, mod_name, only_spec, rename_onlylist = node.subnodes

            topnode = None

            if only_spec and "only" in only_spec.lower():
                if isinstance(rename_onlylist.wrapped, Name):
                    if is_name_equal(path[0], rename_onlylist):
                        topnode = self._parse(node, mod_name)
                    else:
                        return
                elif isinstance(rename_onlylist.wrapped, Only_List):
                    for only in rename_onlylist.subnodes:
                        if isinstance(only.wrapped, Name):
                            if is_name_equal(path[0], only):
                                topnode = self._parse(node, mod_name)
                                break
                        else:
                            import pdb; pdb.set_trace()
                else:
                    import pdb; pdb.set_trace()
            else:
                topnode = self._parse(node, mod_name)
                # read 

            if topnode:
                return self._resolve(topnode, res, path, False)

#        check if module exists in pre-parsed modules in self.modules
#        check include paths and find all source files and check module name
#        parse new module
        
