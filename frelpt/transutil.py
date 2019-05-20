# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

from fparser.two.Fortran2003 import *
from fparser.two.utils import *

from frelpt.fparser_util import (collect_names, is_interface_name, is_function_name,
    is_subroutine_name, insert_subnode, replace_subnode)
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
