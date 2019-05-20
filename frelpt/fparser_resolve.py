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

from frelpt.intrinsics import Intrinsic_Procedures
from frelpt.node import IntrinsicProcedureNode

fortran_exts = [".f", ".f90", ".f95", ".f03", ".F", ".F90", ".F95", ".F03"]

# TODO: resoving order within subnodes
#     - exec part -> spec part
#     - .. -> typedecl -> use
# TODO: spec stmt does not resolve, but add node in resolution related bag

class Resolver(pyloco.Task):

    class _Intrinsic_Resolver_Name(object):
        def __init__(self, name):
            self.name = name

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

        # TODO: update not assign
        self.macros = {}
        self.includes = {}
        self.analyzers = []
        self.resolvers = None
        self.searcher = None

        self.trees =  {}
        self.modules = {}
        self.respaths = {}
        self.invrespaths = {}

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

#    subclass_names = []
#    use_names = ['Block_Data_Stmt', 'Specification_Part',
#                 'End_Block_Data_Stmt']

    def perform(self, targs):

        if targs.macros:
            self.macros.update(targs.macros)

        if targs.includes:
            self.includes.update(targs.includes)

        if targs.analyzers:
            self.analyzers = targs.analyzers

        if targs.resolvers:
            self.resolvers = targs.resolvers

        if targs.searcher:
            self.searcher = targs.searcher


        if targs.trees:
            self.trees.update(targs.trees)

        if targs.modules:
            self.modules.update(targs.modules)

        if targs.respaths:
            self.respaths.update(targs.respaths)

        if targs.invrespaths:
            self.invrespaths.update(targs.invrespaths)

        self._add_modules(targs.node)

        respath = []
        pending = {"implicit_nodes": []}

        self.respaths[targs.node] = respath

        self.log_debug("Resolving '%s'"%str(targs.node.wrapped))

        if self._resolve(targs.node, self.resolvers, respath, pending, True):
            if respath[-1] in self.invrespaths:
                self.invrespaths[respath[-1]].append(respath)

            else:
                self.invrespaths[respath[-1]] = [respath]

            self._analyze(respath)

            # NOTE: process any pending tasks
            #if pending:
            #    import pdb; pdb.set_trace()

            #self.log_debug(str(respath))
        else:

            # try implicit 
            if pending["implicit_nodes"]:
                pass

            # try common blocks 

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
                            "macro" : _macro,
                            "include" : _include,
                            "respaths": self.respaths,
                            "invrespaths": self.invrespaths,
                        }

                        argv = [filepath]
                        parser = Parser(self.get_proxy())
                        retval, _forward = parser.run(argv, forward=forward)
                            
                        tree = _forward["tree"]
                        self.trees[filepath] = tree

                        self._add_modules(tree)

                        #######################################
                        # analyze application
                        #######################################
                        from .analyze import FrelptAnalyzer
                        forward = {
                            "node" : [tree],
                            "macros" : self.macros,
                            "includes" : self.includes,
                            "trees" : self.trees,
                            "modules": self.modules,
                            "respaths": self.respaths,
                            "invrespaths": self.invrespaths,

                        }

                        analyzer = FrelptAnalyzer(self.get_proxy())
                        retval, _forward = analyzer.run([], forward=forward)

                        self.trees.update(_forward["trees"])
                        self.modules.update(_forward["modules"])
                        self.respaths.update(_forward["respaths"])
                        self.invrespaths.update(_forward["invrespaths"])

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

    def _resolve(self, node, res, path, pending, upward):

        if node not in path:
            clsname = node.__class__.__name__
            if clsname.startswith("End_"):
                return
            path.append(node)
            if clsname.endswith("_List"):
                return self._bypass(node, res, path, pending, upward)
            else:
                return getattr(self, "resolve_"+clsname)(node, res, path, pending, upward)

    def _search_resolve(self, *nodes):
        for n1 in nodes:
            forward = { "node": n1 }
            _, _fwd = self.searcher.run([], forward=forward)
            for n2, res in _fwd["ids"].items():
                forward = {
                    "node": n2,
                    "resolvers": res,
                    "macros": self.macros,
                    "includes": self.includes,
                    "analyzers": self.analyzers,
                    "searcher": self.searcher,
                }
                self.run([], forward=forward)

    def _subnode_resolve(self, node, res, path, pending):

        for subnode in self._sorted_subnodes(node):
            if subnode not in path:
                newpath = list(path)
                if self._resolve(subnode, res, newpath, pending, False):
                    path.extend(newpath[len(path):])
                    return True

    def _bypass(self, node, res, path, pending, upward):
        if upward:
            return self._resolve(node.parent, res, path, pending, upward)
        else:
            return self._subnode_resolve(node, res, path, pending)

    def _resolve_implicit_rules(self, node, res, path, pending, upward):
        import pdb; pdb.set_trace()

    def _resolve_intrinsics(self, node, res, path, pending, upward):

        if path[0].wrapped.string.lower() in Intrinsic_Procedures:
            resnode = IntrinsicProcedureNode(None, ["expr"], path[0])
            path.append(resnode)
            return True

    def resolve_Add_Operand(self, node, res, path, pending, upward):

        return self._bypass(node, res, path, pending, upward)

    def resolve_Assignment_Stmt(self, node, res, path, pending, upward):
        if upward:
            return self._resolve(node.parent, res, path, pending, upward)

    def resolve_Block_Nonlabel_Do_Construct(self, node, res, path, pending, upward):
        if upward:
            return self._resolve(node.parent, res, path, pending, upward)
 
    def resolve_Call_Stmt(self, node, res, path, pending, upward):
        if upward:
            return self._resolve(node.parent, res, path, pending, upward)

    def resolve_Comment(self, node, res, path, pending, upward):
        pass

    def resolve_Contains_Stmt(self, node, res, path, pending, upward):
        pass

    def resolve_Declaration_Type_Spec(self, node, res, path, pending, upward):
        """
        <declaration-type-spec> = <intrinsic-type-spec>
                                  | TYPE ( <derived-type-spec> )
                                  | CLASS ( <derived-type-spec> )
                                  | CLASS ( * )
        """

        if upward:
            return self._resolve(node.parent, res, path, pending, upward)

    def resolve_Derived_Type_Def(self, node, res, path, pending, upward):

        if upward:
            if not self._subnode_resolve(node, res, path, pending):
                return self._resolve(node.parent, res, path, pending, upward)
            return True
        else:
            return self._subnode_resolve(node, res, path, pending)

    def resolve_Derived_Type_Stmt(self, node, res, path, pending, upward):

        if upward:
            return self._resolve(node.parent, res, path, pending, upward)
        elif Derived_Type_Stmt in res:
            attr_specs, name, Type_Param_Name_List = node.subnodes
            if self._resolve(name, res, path, pending, upward):
                self._search_resolve(attr_specs, Type_Param_Name_List)
                return True

    def resolve_Entity_Decl(self, node, res, path, pending, upward):
        """
        <entity-decl> = <object-name> [ ( <array-spec> ) ]
            [ * <char-length> ] [ <initialization> ]
                        | <function-name> [ * <char-length> ]
        """

        if upward:
            import pdb; pdb.set_trace()

        else:
            name, array_spec, char_length, init = node.subnodes
            if self._resolve(name, res, path, pending, upward):
                self._search_resolve(array_spec, char_length, init)
                return True

    def resolve_Execution_Part(self, node, res, path, pending, upward):
        if upward:
            return self._resolve(node.parent, res, path, pending, upward)

    def resolve_Implicit_Stmt(self, node, res, path, pending, upward):

        if upward:
            return self._resolve(node.parent, res, path, pending, upward)
        else:
            if node not in pending["implicit_nodes"]:
                pending["implicit_nodes"].append(node)

    def resolve_Implicit_Part(self, node, res, path, pending, upward):
        return self._bypass(node, res, path, pending, upward)

    def resolve_Level_2_Expr(self, node, res, path, pending, upward):
        if upward:
            return self._resolve(node.parent, res, path, pending, upward)

    def resolve_Loop_Control(self, node, res, path, pending, upward):
        if upward:
            return self._resolve(node.parent, res, path, pending, upward)

    def resolve_Main_Program(self, node, res, path, pending, upward):
        """
            <main-program> = <program-stmt>
                         [ <specification-part> ]
                         [ <execution-part> ]
                         [ <internal-subprogram-part> ]
                         <end-program-stmt>
        """

        if upward:
            if self._subnode_resolve(node, res, path, pending):
                return True
            return self._resolve(node.parent, res, path, pending, upward)

    def resolve_Module(self, node, res, path, pending, upward):
        """
        <module> = <module-stmt>
                       [ <specification-part> ]
                       [ <module-subprogram-part> ]
                       <end-module-stmt>
        """
#        #return self._bypass(node, res, path, pending, upward)
#        if upward:
#            import pdb; pdb.set_trace()
#        else:
#            # NOTE: mod should pass this if res can be part of Module
#            return self._subnode_resolve(node, res, path, pending)

        if upward:
            if not self._subnode_resolve(node, res, path, pending):
                return self._resolve(node.parent, res, path, pending, upward)
            return True
        else:
            return self._subnode_resolve(node, res, path, pending)

    def resolve_Module_Stmt(self, node, res, path, pending, upward):
        """
        <module-stmt> = MODULE <module-name>
        """
        if Module_Stmt in res:
            return self._resolve(node.subnodes[1], res, path, pending, upward)

    def resolve_Module_Subprogram_Part(self, node, res, path, pending, upward):
        """
        <module-subprogram-part> = <contains-stmt>
                                       <module-subprogram>
                                       [ <module-subprogram> ]...
        """
        return self._bypass(node, res, path, pending, upward)

    def resolve_Name(self, node, res, path, pending, upward):
        if upward: # start resolve
            return self._resolve(node.parent, res, path, pending, upward)
        elif len(path) > 1 and is_name_equal(path[0], path[-1]):
            return True

    def resolve_Nonlabel_Do_Stmt(self, node, res, path, pending, upward):
        if upward:
            return self._resolve(node.parent, res, path, pending, upward)

    def resolve_Part_Ref(self, node, res, path, pending, upward):
        """
        <part-ref> = <part-name> [ ( <section-subscript-list> ) ]
        """
        if upward:
            return self._resolve(node.parent, res, path, pending, upward)

    def resolve_Procedure_Designator(self, node, res, path, pending, upward):

        if upward:
            return self._resolve(node.parent, res, path, pending, upward)

    def resolve_Program(self, node, res, path, pending, upward):
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
            # resolve with intrinsic names
            if self._resolve_intrinsics(node, res, path, pending, upward):
                return True
            elif self._resolve_implicit_rules(node, res, path, pending, upward):
                import pdb; pdb.set_trace()
            else:
                import pdb; pdb.set_trace()
        else:
            # from USE stmt
            return self._subnode_resolve(node, res, path, pending)

    def resolve_Program_Stmt(self, node, res, path, pending, upward):
        if not upward and Program_Stmt in res:
            return self._resolve(node.subnodes[1], res, path, pending, upward)

    def resolve_Specific_Binding(self, node, res, path, pending, upward):

        import pdb; pdb.set_trace()

    def resolve_Subroutine_Stmt(self, node, res, path, pending, upward):
        """
        <subroutine-stmt>
        = [ <prefix> ] SUBROUTINE <subroutine-name>
          [ ( [ <dummy-arg-list> ] ) [ <proc-language-binding-spec> ] ]
        """
        if upward:
            return self._resolve(node.parent, res, path, pending, upward)
        elif Subroutine_Stmt in res:
            prefix, name, dummy_args, binding_spec = node.subnodes
            return self._resolve(name, res, path, pending, upward)

    def resolve_Subroutine_Subprogram(self, node, res, path, pending, upward):
        """
        <subroutine-subprogram> = <subroutine-stmt>
                                     [ <specification-part> ]
                                     [ <execution-part> ]
                                     [ <internal-subprogram-part> ]
                                  <end-subroutine-stmt>
        """
        if upward:
            if not self._subnode_resolve(node, res, path, pending):
                return self._resolve(node.parent, res, path, pending, upward)
            return True
        else:
            return self._subnode_resolve(node, res, path, pending)

    def resolve_Specification_Part(self, node, res, path, pending, upward):
        # TODO: resolve with typedecl first? than use?
        # typedecl .. -> use -> implicit
        #if upward:
        #    return self._resolve(node.parent, res, path, pending, upward)
        #else:
        #    return self._subnode_resolve(node, res, path, pending)
        #return self._bypass(node, res, path, pending, upward)

        if upward:
            if not self._subnode_resolve(node, res, path, pending):
                return self._resolve(node.parent, res, path, pending, upward)
            return True
        else:
            return self._subnode_resolve(node, res, path, pending)

    def resolve_Tuple(self, node, res, path, pending, upward):
        return self._bypass(node, res, path, pending, upward)
           
    def resolve_Type_Bound_Procedure_Part(self, node, res, path, pending, upward):

        return self._bypass(node, res, path, pending, upward)

    def resolve_Type_Declaration_Stmt(self, node, res, path, pending, upward):
        """<type-declaration-stmt> = <declaration-type-spec> [ [ ,
                <attr-spec> ]... :: ] <entity-decl-list>

        """

        if upward:
            # assuming self-resolve is handled by entity_decls
            return self._resolve(node.parent, res, path, pending, upward)
        elif Type_Declaration_Stmt in res:
            type_spec, attr_specs, entity_decls = node.subnodes
            if self._resolve(entity_decls, res, path, pending, upward):
                self._search_resolve(type_spec, attr_specs)
                return True

    def resolve_Type_Name(self, node, res, path, pending, upward):

        if upward:
            return self._resolve(node.parent, res, path, pending, upward)
        elif Derived_Type_Stmt in res:
            return self._subnode_resolve(node, res, path, pending)

    def resolve_Use_Stmt(self, node, res, path, pending, upward):
        """
            Fortran 2003 rule R1109

            use-stmt is USE [ [ , module-nature ] :: ] module-name [ , rename-list ]
                    or USE [ [ , module-nature ] :: ] module-name ,
                        ONLY : [ only-list ]
        """

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
                out = self._resolve(topnode, res, path, pending, False)
                if not out:
                    import pdb; pdb.set_trace()
                return out

#        check if module exists in pre-parsed modules in self.modules
#        check include paths and find all source files and check module name
#        parse new module
        
