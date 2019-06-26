# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

import copy

from fparser.two.Fortran2003 import *
from fparser.two.utils import *

from fparser.common.readfortran import FortranStringReader
from frelpt.node import ConcreteSyntaxNode, generate

def _get_parent(node, ngreats=1):

    if not isinstance(ngreats, int) or ngreats < 0:
        return None

    p = node 

    for idx in range(ngreats):

        if p is None:
            return None

        p = p.parent if hasattr(p, "parent") else None

    return p

def get_stmt_parent(node):

    if node is None:
        return None

    elif isinstance(node.wrapped, StmtBase):
        return node

    elif hasattr(node, "parent"):
        return get_stmt_parent(node.parent)

def collect_nodes_by_class(node, cls):

    def _collect(node, bag, depth):
        if hasattr(node, "wrapped"):
            if isinstance(cls, (list, tuple)):
                for c in cls:
                    if isinstance(node.wrapped, c):
                        bag.append(node)

            elif isinstance(node.wrapped, cls):
                bag.append(node)

    nodes = []
    node.traverse(nodes, node=node, func=_collect)

    return nodes


def collect_names(node):
    return collect_nodes_by_class(node, Name)


def get_attr_spec(node, attrcls):
    return collect_nodes_by_class(node, attrcls)


def is_subroutine_name(name):

    p = _get_parent(name)
    pp = _get_parent(name, 2)

    if (p and isinstance(p.wrapped, Subroutine_Stmt) and pp.wrapped and
            isinstance(pp.wrapped, Subroutine_Subprogram)):
        return True


def is_function_name(name):

    p = _get_parent(name)
    pp = _get_parent(name, 2)

    if (p and isinstance(p.wrapped, Function_Stmt) and pp.wrapped and
            isinstance(pp.wrapped, Function_Subprogram)):
        return True


def is_procedure_name(name):

    return is_function_name(name) or is_subroutine_name(name)

def is_interface_name(name):

    p = _get_parent(name)
    pp = _get_parent(name, 2)

    if (p and isinstance(p.wrapped, Interface_Stmt) and pp.wrapped and
            isinstance(pp.wrapped, Interface_Block)):
        return True

    return False

def is_dtype_name(name):

    pp = _get_parent(name, 2)
    ppp = _get_parent(name, 3)

    if (pp and isinstance(pp.wrapped, Derived_Type_Stmt) and ppp.wrapped and
            isinstance(ppp.wrapped, Derived_Type_Def)):
        return True

    return False

def is_subprogram_name(name):

    return (is_function_name(name) or is_subroutine_name(name) or
            is_interface_name(name))


def get_entity_decl_by_name(node, name):

    for entity_decl in collect_nodes_by_class(node, Entity_Decl):
        objname = entity_decl.subnodes[0]
        if objname.wrapped == name.wrapped:
            return entity_decl


def get_parent_by_class(node, pcls):

    if pcls is None:
        return None

    while node:
        if hasattr(node, "wrapped"):
            if isinstance(node.wrapped, pcls):
                return node
        else:
            import pdb; pdb.set_trace()

        node = node.parent if hasattr(node, "parent") else None


def is_array_var(typedeclstmt, varname):

    type_spec, attr_specs, entity_decls = typedeclstmt.subnodes

    if attr_specs:
        dim_spec = get_attr_spec(attr_specs, Dimension_Attr_Spec)
        if dim_spec:
            return True

    if entity_decls:
        entity_decl = get_entity_decl_by_name(entity_decls, varname)
        
        if entity_decl:
            name, array_spec, char_length, init = entity_decl.subnodes
            if array_spec:
                return True

    return False


def is_descendant(node1, node2):

        while node1 is not node2:
            if not node1 or not hasattr(node1, "parent"):
                return False
            node1 = node1.parent
            if node1 is node2:
                return True
            
        return False

def replace_dovar_with_section_subscript(node, loopctr):

    def _generate(lctr):

        def _f():

            start = lctr["start"].wrapped.tofortran()
            stop = lctr["stop"].wrapped.tofortran()

            if lctr["step"]:
                step = lctr["step"].wrapped.tofortran()
                st = Subscript_Triplet("%s:%s:%s" % (start, stop, step))

            else:
                st = Subscript_Triplet("%s:%s" % (start, stop))

            return generate(st)
            #return ConcreteSyntaxNode(None, "expr", st)

        return _f

    replace_name_by_generator(node, loopctr["dovar"], _generate(loopctr))

# TODO: change tuple to list in items attr
# TODO: create a function that generate a fparser node and assign into Node tree

def replace_subnode(node, idx, dest):
    dest.parent = node
    if hasattr(node.wrapped, "items"):
        _t = list(node.wrapped.items)
        _t[idx] = dest.wrapped
        node.wrapped.items = tuple(_t)
    elif hasattr(node.wrapped, "content"):
        node.wrapped.content[idx] = dest.wrapped
    node.subnodes[idx] = dest

def append_subnode(node, dest):
    dest.parent = node
    if hasattr(node.wrapped, "items"):
        _t = list(node.wrapped.items)
        _t.append(dest.wrapped)
        node.wrapped.items = tuple(_t)
    elif hasattr(node.wrapped, "content"):
        node.wrapped.content.append(dest.wrapped)
    node.subnodes.append(dest)

def remove_subnode(node, idx):
    subnode = node.subnodes.pop(idx)
    subnode.parent = None
    if hasattr(node.wrapped, "items"):
        _t = list(node.wrapped.items)
        _t.pop(idx)
        node.wrapped.items = tuple(_t)
    elif hasattr(node.wrapped, "content"):
        node.wrapped.content.pop(idx)
    return subnode

def insert_subnode(node, idx, dest):
    dest.parent = node
    if hasattr(node.wrapped, "items"):
        _t = list(node.wrapped.items)
        _t.insert(idx, dest.wrapped)
        node.wrapped.items = tuple(_t)
    elif hasattr(node.wrapped, "content"):
        node.wrapped.content.insert(idx, dest.wrapped)
    node.subnodes.insert(idx, dest)

def replace_name_by_generator(target, name, generator):

    def _replace(node, bag, depth):

        if hasattr(node, "wrapped") and node.wrapped == bag[0].wrapped:
            idx = node.parent.subnodes.index(node)
            replace_subnode(node.parent, idx, bag[1]())

    bag = [name, generator]
    target.traverse(bag, node=target, func=_replace)

def collect_entity_names(node):

    if isinstance(node.wrapped, Entity_Decl):
        return [node.subnodes[0]]    
    else:
        import pdb; pdb.set_trace()

def get_typedecl_parent(node):

    return get_parent_by_class(node,  Type_Declaration_Stmt)

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


def promote_typedecl(respath, loopctr):

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
                    speclist = generate(speclistnode)
                    #speclist = ConcreteSyntaxNode(entity_decls, "expr", speclistnode)
                    replace_subnode(entity_decls, 1, speclist)

                    if char_length or init:
                        import pdb; pdb.set_trace()

                elif isinstance(array_spec.wrapped, Assumed_Shape_Spec_List):
                    import pdb; pdb.set_trace()


                elif isinstance(array_spec.wrapped, (Explicit_Shape_Spec, Explicit_Shape_Spec_List)):

                    if loopctr["step"]:
                        speclistnode = Explicit_Shape_Spec_List("frelpt_start:frelpt_stop:frelpt_step, " + array_spec.wrapped.tofortran())
                    else:
                        speclistnode = Explicit_Shape_Spec_List("frelpt_start:frelpt_stop, " + array_spec.wrapped.tofortran())

                    speclist = generate(speclistnode)
                    #speclist = ConcreteSyntaxNode(entity_decls, "expr", speclistnode)
                    replace_subnode(entity_decls, 1, speclist)

                else:
                    import pdb; pdb.set_trace()

            elif attr_specs:

                dimattr = get_attr_spec(attr_specs, Dimension_Attr_Spec)

                if dimattr:
                    import pdb; pdb.set_trace()

            else:
                import pdb; pdb.set_trace()

        elif isinstance(entity_decls.wrapped, Entity_Decl_List):
            import pdb; pdb.set_trace()

        else:
            import pdb; pdb.set_trace()

def add_loopctr_dummy_args(funcstmt, loopctr, argsplit_index=0):

    # add dummy args
    if isinstance(funcstmt.wrapped, Subroutine_Stmt):
        prefix, name, dummy_args, binding_spec = funcstmt.subnodes

        if isinstance(dummy_args.wrapped, Dummy_Arg_List):
            if argsplit_index == 0:
                dummyargsnode = Dummy_Arg_List("frelpt_start, frelpt_stop, frelpt_step,"+dummy_args.wrapped.tofortran())
            else:
                prefix = [str(d) for d in dummy_args.subnodes[:argsplit_index]]
                frelpt_args = ["frelpt_start", "frelpt_stop", "frelpt_step"]
                postfix = [str(d) for d in dummy_args.subnodes[argsplit_index:]]
                dummyargsnode = Dummy_Arg_List(", ".join(prefix+frelpt_args+postfix))
            dummyargs = generate(dummyargsnode)
            #dummyargs = ConcreteSyntaxNode(funcstmt, "expr", dummyargsnode)
            replace_subnode(funcstmt, 2, dummyargs)

        else:
            import pdb; pdb.set_trace()
    else:
        import pdb; pdb.set_trace()

    # add typedecls for dummy args
    specpart = collect_nodes_by_class(funcstmt.parent, Specification_Part)

    if specpart:
        intentnode = Type_Declaration_Stmt("INTEGER, INTENT(IN) :: frelpt_start, frelpt_stop, frelpt_step")
        intent = generate(intentnode)
        #intent = ConcreteSyntaxNode(specpart[0], "stmt", intentnode)
        append_subnode(specpart[0], intent)
        dovarnode = Type_Declaration_Stmt("INTEGER :: frelpt_index")
        dovar = generate(dovarnode)
        #dovar = ConcreteSyntaxNode(specpart[0], "stmt", dovarnode)
        append_subnode(specpart[0], dovar)

    else:
        import pdb; pdb.set_trace()

def add_loopctr_actual_args(callstmt, loopctr):

    # add actual args
    if isinstance(callstmt.wrapped, Call_Stmt):
        proc, actual_args = callstmt.subnodes

        if isinstance(actual_args.wrapped, Actual_Arg_Spec_List):
            actualargsnode = Actual_Arg_Spec_List("frelpt_start, frelpt_stop, frelpt_step,"+actual_args.wrapped.tofortran())
            actualargs = generate(actualargsnode)
            #actualargs = ConcreteSyntaxNode(callstmt, "expr", actualargsnode)
            replace_subnode(callstmt, 1, actualargs)

        else:
            import pdb; pdb.set_trace()
    else:
        import pdb; pdb.set_trace()

def wrap_stmt_with_doblock(stmt, loopctr):

    doblock_str = """DO frelpt_index=frelpt_start, frelpt_stop, frelpt_step
    %s
END DO
""" % str(stmt)

    doblock_node = Do_Block(FortranStringReader(doblock_str))
    doblock = generate(doblock_node)

    parent = stmt.parent
    idx_stmt = parent.subnodes.index(stmt)
    replace_subnode(parent, idx_stmt, doblock)

def collect_funccall_stmt(funccall):

    while hasattr(funccall, "parent"):
        funccall = funccall.parent
        if funccall and isinstance(funccall.wrapped, (Call_Stmt, Function_Reference)):
            return funccall

def get_partref(parent, text):

    return generate(Part_Ref(text))
    #return ConcreteSyntaxNode(parent, "expr", Part_Ref(text))


def is_partref(node):

    return hasattr(node, "wrapped") and isinstance(node.wrapped, Part_Ref)

def is_entitydecl(node):

    return hasattr(node, "wrapped") and isinstance(node.wrapped, Entity_Decl)

def is_actual_arg_spec_list(node):

    return hasattr(node, "wrapped") and isinstance(node.wrapped, Actual_Arg_Spec_List)

def is_actual_arg_spec(node):

    return hasattr(node, "wrapped") and isinstance(node.wrapped, Actual_Arg_Spec)

def is_section_subscript_list(node):

    return hasattr(node, "wrapped") and isinstance(node.wrapped, Section_Subscript_List)

def is_structure_constructor_2(node):

    return hasattr(node, "wrapped") and isinstance(node.wrapped, Structure_Constructor_2)

def is_name(node):

    return hasattr(node, "wrapped") and isinstance(node.wrapped, Name)


def is_classtype(node):

    tdecl = get_typedecl_parent(node)
    tdeclspec = tdecl.subnodes[0]
    return tdeclspec.subnodes[0] == "CLASS"

def is_dummy_arg_list(node):

    return hasattr(node, "wrapped") and isinstance(node.wrapped, Dummy_Arg_List)

