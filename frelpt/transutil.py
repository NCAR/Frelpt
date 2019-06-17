# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

from fparser.two.Fortran2003 import *
from fparser.two.utils import *

from frelpt.fparser_util import (collect_names, is_interface_name, is_function_name,
    is_subroutine_name, insert_subnode, replace_subnode, collect_nodes_by_class,
    append_subnode)
from frelpt.node import ConcreteSyntaxNode

def collect_func_calls(node, arrvars, arr_actargs, respaths):
    
    func_calls = []
    names = collect_names(node)

    for name in names:
        if name in respaths:
            resname = respaths[name][-1]

            if is_function_name(resname):
                print("FUNCTION")
                import pdb; pdb.set_trace()

            elif is_subroutine_name(resname):
                argnames = collect_names(name.parent.subnodes[1])
                actargs = []
                arr_actargs[name] = actargs

                for argname in argnames:
                    if argname in arrvars:
                        actargs.append(argname)
                        if name not in func_calls:
                            func_calls.append(name)                

            elif is_interface_name(resname):
                print("INTERFACE")
                import pdb; pdb.set_trace()
        else:
            raise Exception("'%s' is not resolved."%str(name))

    return func_calls


def promote_typedecl(respath):

    typedecl = None
    node = respath[-1]

    while hasattr(node, "parent"):
        node = node.parent
        if node.wrapped.__class__ is Type_Declaration_Stmt:
            typedecl = node
            break

    if typedecl:
        type_spec, attr_specs, entity_decls = typedecl.subnodes

        if isinstance(entity_decls.wrapped, Entity_Decl):
            name, array_spec, char_length, init = entity_decls.subnodes

            if array_spec:
                if isinstance(array_spec.wrapped, Assumed_Shape_Spec):

                    speclistnode = Assumed_Shape_Spec_List(":,"+array_spec.wrapped.tofortran())
                    speclist = ConcreteSyntaxNode(entity_decls, "expr", speclistnode)
                    replace_subnode(entity_decls, 1, speclist)

                    if char_length or init:
                        import pdb; pdb.set_trace()

                elif isinstance(array_spec.wrapped, Assumed_Shape_Spec_List):
                    import pdb; pdb.set_trace()


                else:
                    import pdb; pdb.set_trace()

            elif attr_specs:
                import pdb; pdb.set_trace()

            else:
                import pdb; pdb.set_trace()

        elif isinstance(entity_decls.wrapped, Entity_Decl_List):
            import pdb; pdb.set_trace()

        else:
            import pdb; pdb.set_trace()

def add_dummy_args(funcstmt, loopctr):

    # add dummy args
    if isinstance(funcstmt.wrapped, Subroutine_Stmt):
        prefix, name, dummy_args, binding_spec = funcstmt.subnodes

        if isinstance(dummy_args.wrapped, Dummy_Arg_List):
            dummyargsnode = Dummy_Arg_List("frelpt_start, frelpt_stop, frelpt_step,"+dummy_args.wrapped.tofortran())
            dummyargs = ConcreteSyntaxNode(funcstmt, "expr", dummyargsnode)
            replace_subnode(funcstmt, 2, dummyargs)

        else:
            import pdb; pdb.set_trace()
    else:
        import pdb; pdb.set_trace()

    # add typedecls for dummy args
    specpart = collect_nodes_by_class(funcstmt.parent, Specification_Part)

    if specpart:
        intentnode = Type_Declaration_Stmt("INTEGER, INTENT(IN) :: frelpt_start, frelpt_stop, frelpt_step")
        intent = ConcreteSyntaxNode(specpart[0], "stmt", intentnode)
        append_subnode(specpart[0], intent)
        dovarnode = Type_Declaration_Stmt("INTEGER :: frelpt_index")
        dovar = ConcreteSyntaxNode(specpart[0], "stmt", dovarnode)
        append_subnode(specpart[0], dovar)

    else:
        import pdb; pdb.set_trace()

def add_actual_args(callstmt, loopctr):

    # add actual args
    if isinstance(callstmt.wrapped, Call_Stmt):
        proc, actual_args = callstmt.subnodes

        if isinstance(actual_args.wrapped, Actual_Arg_Spec_List):
            actualargsnode = Actual_Arg_Spec_List("frelpt_start, frelpt_stop, frelpt_step,"+actual_args.wrapped.tofortran())
            actualargs = ConcreteSyntaxNode(callstmt, "expr", actualargsnode)
            replace_subnode(callstmt, 1, actualargs)

        else:
            import pdb; pdb.set_trace()
    else:
        import pdb; pdb.set_trace()

def collect_funccall_stmt(funccall):

    while hasattr(funccall, "parent"):
        funccall = funccall.parent
        if isinstance(funccall.wrapped, (Call_Stmt, Function_Reference)):
            return funccall
    
    import pdb ;pdb.set_trace()
