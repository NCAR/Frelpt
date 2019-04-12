# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

from fparser.two.Fortran2003 import *
from fparser.two.utils import *

def _collect_by_cls(node, cls):

    def _collect(node, bag, depth):
        if hasattr(node, "wrapped") and isinstance(node.wrapped, cls):
            bag.append(node)

    nodes = []
    node.traverse(nodes, node=node, func=_collect)

    return nodes

def collect_names(node):
    return _collect_by_cls(node, Name)

def get_attr_spec(node, attrcls):
    return _collect_by_cls(node, attrcls)

def get_entity_decl_by_name(node, name):

    for entity_decl in _collect_by_cls(node, Entity_Decl):
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
