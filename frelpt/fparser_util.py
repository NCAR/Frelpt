# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

from fparser.two.Fortran2003 import *
from fparser.two.utils import *


def _get_parent(node, ngreats=1):

    if not isinstance(ngreats, int) or ngreats < 0:
        return None

    p = node 

    for idx in range(ngreats):

        if p is None:
            return None

        p = p.parent if hasattr(p, "parent") else None

    return p

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


def is_interface_name(name):

    p = _get_parent(name)
    pp = _get_parent(name, 2)

    if (p and isinstance(p.wrapped, Interface_Stmt) and pp.wrapped and
            isinstance(pp.wrapped, Interface_Block)):
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
            if isinstance(pcls, (list, tuple)):
                for pc in pcls:
                    if isinstance(node.wrapped, pc):
                        return node
            elif isinstance(node.wrapped, pcls):
                return node
        else:
            import pdb; pdb.set_trace()

        node = node.parent if hasattr(node, "parent") else None

    return None

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
