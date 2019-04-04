# -*- coding: utf-8 -*-
from __future__ import (absolute_import, division,
                        print_function, unicode_literals)
#from builtins import *

import types
from copy import copy, deepcopy

# Node provides infrastructure, not feature
# Node attribute is dictionary or dictionary-like user object
# Node methods are replacable in part or as a whole
# Node traverse is based on user-provided search algorithm and attrs and methods
# Node infrastructure provides following services:
# - python dynamic attribute service
# - Node creation/modification/deletion

import sys
sys.setrecursionlimit(1999)

class UpperNodeException(Exception):
    pass

class StopNodeException(Exception):
    pass

class SiblingNodeException(Exception):
    pass

class Node(object):

    STOP_SEARCH = -1

    #__slots__ = ['_attrs', '_methods', 'uppernode', 'subnodes']

    _shared_attrs = {}
    _shared_methods = {}

    def __init__(self, uppernode=None, subnodes=None, attrs=None, methods=None,
        shared_attrs=None, shared_methods=None):

        object.__setattr__(self, '_attrs', attrs if attrs else {})
        object.__setattr__(self, '_methods', methods if methods else {})

        if shared_attrs:
            self._shared_attrs.update(shared_attrs)
        if shared_methods:
            self._shared_methods.update(shared_methods)

        self.uppernode = uppernode
        self.subnodes = subnodes if subnodes else []

    # attributes
    def __getattr__(self, name):

        if name in object.__getattribute__(self, "_attrs"):
            return object.__getattribute__(self, "_attrs")[name]
        elif name in object.__getattribute__(self, "_methods"):
            try:
                return types.MethodType(object.__getattribute__(self, "_methods")[name], self, Node)
            except:
                try:
                    return types.MethodType(object.__getattribute__(self, "_methods")[name], self)
                except:
                    pass
        elif name in object.__getattribute__(self, "_shared_attrs"):
            return object.__getattribute__(self, "_shared_attrs")[name]
        elif name in object.__getattribute__(self, "_shared_methods"):
            try:
                return types.MethodType(object.__getattribute__(self, "_shared_methods")[name], self, Node)
            except:
                try:
                    return types.MethodType(object.__getattribute__(self, "_shared_methods")[name], self)
                except:
                    pass

        node_name = ':"%s"'%self.name if "name" in self._attrs or "name" in self._shared_attrs else ""
        raise AttributeError("%s%s object has no attribute '%s'."% (
            self.__class__.__name__, node_name, name))

    def __setattr__(self, name, value):

        if name in ('_attrs', '_methods', '_shared_attrs', '_shared_methods'):
            if name in dir(self):
                raise AttributeError("'%s' attribute is not mutable."%name)
            else:
                object.__setattr__(self, name, value)
        elif name in ['uppernode', 'subnodes']:
            object.__setattr__(self, name, value)
        elif name in self.__class__.__dict__.keys():
            raise AttributeError("'%s' attribute is not mutable."%name)
        elif callable(value):
            self._methods[name] = value
        else:
            self._attrs[name] = value

    def __delattr__(self, name):

        if name in ('_attrs', '_methods', '_shared_attrs', '_shared_methods'):
            raise AttributeError("'%s' attribute is not mutable."%name)
        elif name in self._methods:
            del self._methods[name]
        elif name in self._attrs:
            del self._attrs[name]
        else:
            object.__delattr__(self, name)

    def __hasattr__(self, name):

        try:
            getattr(self, name)
            return True
        except:
            return False

    def getattr_shared(self, name):

        if name in self._shared_attrs:
            return self._shared_attrs[name]
        elif name in self._shared_methods:
            try:
                return types.MethodType(self._shared_methods[name], self, Node)
            except TypeError:
                return types.MethodType(self._shared_methods[name], self)

    def setattr_shared(self, name, value):
        if callable(value):
            self._shared_methods[name] = value
        else:
            self._shared_attrs[name] = value

    def delattr_shared(self, name):
        if name in self._shared_methods:
            del self._shared_methods[name]
        elif name in self._shared_attrs:
            del self._shared_attrs[name]

    def hasattr_shared(self, name):
        return True if name in self._shared_attrs or \
            name in self._shared_methods else False

    def __str__(self):
        if hasattr(self, 'name'):
            return self.name
        else:
            return self.__class__.__name__

    def __unicode__(self):
        if hasattr(self, 'name'):
            return u'%s'%self.name
        else:
            return u'%s'%self.__class__.__name__

    def __repr__(self):
        return "%s %s"%(self.__class__, str(self))

    def dirall(self):
        return self._shared_attrs.keys() + self._shared_methods.keys() + \
            self._attrs.keys() + self._methods.keys() + dir(self)

    def treeview(self, *args):
        attrs = [a+'='+repr(getattr(self, a)) for a in args if hasattr(self, a)]
        lines = [str(self)+(' (%s)'%', '.join(attrs) if attrs else '')]
        lines.extend(["-"+n.treeview(*args) for n in self.subnodes])
        return "\n".join(lines).replace("\n-", "\n---|")

    # sequences
    def __len__(self):
        return len(self.subnodes)

    def __iter__(self):
        return iter(self.subnodes)

    def __reversed__(self):
        return reversed(self.subnodes)

    def __getitem__(self, key):
        return self.subnodes[key]

    def __setitem__(self, key, value):
        self.subnodes[key] = value

    def __delitem__(self, key):
        del self.subnodes[key]

    def __contains__(self, item):
        return item in self.subnodes

    #def __missing__(self, key):
    #    return item in self.subnodes

    def __copy__(self):

        node = self.__class__()

        # copy methods
        for name, method in self._shared_methods.items():
            node._shared_methods[name] = method
        for name, method in self._methods.items():
            node._methods[name] = method

        # copy attributes
        for name, attr in self._shared_attrs.items():
            node._shared_attrs[name] = copy(attr)
        for name, attr in self._attrs.items():
            node._attrs[name] = copy(attr)

        # copy subnodes
        for subnode in self.subnodes:
            node.add_subnode(subnode)

        return node

    def __deepcopy__(self, memo={}):
        if self in memo:
            return memo[self]
        else:
            return self.clone(memo=memo)

    def clone(self, memo={}):

        if self in memo:
            return memo[self]

        node = self.__class__()

        # save node in memo
        memo[self] = node

        # copy methods
        #for name, method in self._shared_methods.items():
        #    node._shared_methods[name] = method
        for name, method in self._methods.items():
            node._methods[name] = method

        # copy attributes
        #for name, attr in self._shared_attrs.items():
        #    node._shared_attrs[name] = deepcopy(attr, memo=memo)
        for name, attr in self._attrs.items():
            node._attrs[name] = deepcopy(attr, memo=memo)

        # clone subnodes
        for subnode in self.subnodes:
            if subnode in memo:
                node.add_subnode(memo[subnode])
            else:
                node.add_subnode(subnode.clone(memo=memo))

        return node

    # pickling
#    def __getinitargs__(self):
#        import pdb; pdb.set_trace()
#
#    def __getnewargs__(self):
#        import pdb; pdb.set_trace()
#

#    def dump(self, nodes):
#        def _nodeid(attrs, notes, depth=0):
#            if depth > 3:
#                return attrs
#            attrsid = id(attrs)
#            if attrsid in notes:
#                return notes[attrsid]
#            if isinstance(attrs, Node):
#                notes[attrsid] = attrs
#                attrs = attrsid
#            else:
#                try:
#                    for k, v in attrs.items():
#                        attrs[_nodeid(k, notes, depth=depth+1)] = \
#                            _nodeid(v, notes, depth=depth+1)
#                except AttributeError as e:
#                    try:
#                        for i in range(len(attrs)):
#                            attrs[i] = _nodeid(attrs[i], notes, depth=depth+1)
#                    except Exception as e:
#                        pass
#                notes[attrsid] = attrs
#
#            return attrs
#
#        nodes[id(self)] = state = {}
#        if self.uppernode:
#            state['uppernode'] = id(self.uppernode)
#        else:
#            state['uppernode'] = None
#
#        notes = {}
#        #state['attrs'] = _nodeid(self._attrs, notes)
#        #state['attrs'] = self._attrs
#        state['methods'] = self._methods
#        state['subnodes'] = []
#        for subnode in self.subnodes:
#            state['subnodes'].append(id(subnode))
#            subnode.dump(nodes)
#
#    def load(self, state, nodeid, nodemap):
#
#        nodemap[nodeid] = self
#
#        for sid, sitems in state['nodes'].items():
#            subnode = Node(
#                uppernode=sitems['uppernode'],
#                subnodes=sitems['subnodes'],
#                #attrs=sitems['attrs'],
#                methods=sitems['methods']
#            )
#            subnode.load(state, sid, nodemap)
#
#    def __getstate__(self):
#        state = {}
#        #state['shared_attrs'] = self._shared_attrs
#        state['shared_methods'] = self._shared_methods
#        state['nodeid'] = id(self)
#        state['nodes'] = nodes = {}
#        self.dump(nodes)
#        return state
#
#    def __setstate__(self, state):
#
#        #import pdb; pdb.set_trace()
#        #self._shared_attrs.update(state['shared_attrs'])
#        self._shared_methods.update(state['shared_methods'])
#
#        self.__init__()
#        nodemap = {}
#        self.load(state, state['nodeid'], nodemap)
#    def __reduce__(self):
#        import pdb; pdb.set_trace()
#
#    def __reduce_ex__(self):
#        import pdb; pdb.set_trace()

    def __getstate__(self):
        state = self.__dict__.copy()
        state['shared_attrs'] = self._shared_attrs
        state['shared_methods'] = self._shared_methods
        return state

    def __setstate__(self, state):
        self._shared_attrs.update(state['shared_attrs'])
        self._shared_methods.update(state['shared_methods'])
        del state['shared_attrs']
        del state['shared_methods']
        self.__dict__.update(state)

    # node manipulation
    def add_subnode(self, node, index=None):
        node.uppernode = self
        if not index:
            self.subnodes.append(node)
        else:
            self.subnodes.insert(index, node)

    def pop_subnode(self, index):
        return self.subnodes.pop(index)

    def get_subnodes(self):
        return iter(self.subnodes)

    def get_uppernodes(self):
        node = self.uppernode
        while isinstance(node, self.__class__):
            yield node
            node = node.uppernode
        return

#    def get_rightnode(self, stopnode=None):
#
#        node = self
#        while type(node) == type(self):
#            try:
#                node = node._get_rightnode(stopnode=stopnode)
#                break
#            except (UpperNodeException, StopNodeException):
#                node = None
#            except SiblingNodeException:
#                node = node.uppernode
#
#        if type(node) == type(self):
#            while len(node.subnodes) > 0:
#                node = node.subnodes[0]
#
#        return node
#
#
#    def _get_rightnode(self, stopnode=None):
#
#        if type(self.uppernode) != type(self):
#            raise UpperNodeException()
#
#        if self.uppernode is stopnode:
#            raise StopNodeException()
#
#        myid = id(self)
#        rightnode = None
#
#        for idx, subnode in enumerate(self.uppernode.subnodes):
#            if id(subnode) == myid:
#                if idx+1 == len(self.uppernode.subnodes):
#                    raise SiblingNodeException()
#                rightnode = self.uppernode.subnodes[idx+1]
#
#        if rightnode is stopnode:
#            raise StopNodeException()
#
#        return rightnode
#
#    def get_leftnode(self, stopnode=None):
#
#        node = self
#        while type(node) == type(self):
#            try:
#                node = node._get_leftnode(stopnode=stopnode)
#                break
#            except (UpperNodeException, StopNodeException):
#                node = None
#            except SiblingNodeException:
#                node = node.uppernode
#
#        if type(node) == type(self):
#            while len(node.subnodes) > 0:
#                node = node.subnodes[-1]
#
#        return node
#
#    def _get_leftnode(self, stopnode=None):
#
#        if type(self.uppernode) != type(self):
#            raise UpperNodeException()
#
#        if self.uppernode is stopnode:
#            raise StopNodeException()
#
#        myid = id(self)
#        leftnode = None
#
#        for idx, subnode in enumerate(self.uppernode.subnodes):
#            if id(subnode) == myid:
#                if idx == 0:
#                    raise SiblingNodeException()
#                leftnode = self.uppernode.subnodes[idx-1]
#
#        if leftnode is stopnode:
#            raise StopNodeException()
#
#        return leftnode

    def insert_after(self, node):
        previdx = self.uppernode.subnodes.index(self)
        self.uppernode.subnodes.insert(previdx+1, node)
        node.uppernode = self.uppernode

    def insert_before(self, node):
        nextidx = self.uppernode.subnodes.index(self)
        self.uppernode.subnodes.insert(nextidx, node)
        node.uppernode = self.uppernode

    # attribute and methods manipulations
    # such as swapping methods

    def search(self, action, move, basket={}, premove=None, postmove=None, stopnode=None):

        node = premove(self, basket) if premove is not None else self

        while type(node) == type(self) and action(node, basket) != self.STOP_SEARCH:
            node = move(node, basket, stopnode)
            if node is stopnode: break

        return postmove(node, basket) if postmove is not None else  node

#def DFS_LF(node, basket, stopnode):
#    """Depth-first search from the left of a tree."""
#
#    if len(node.subnodes) > 0:
#        newnode = node.subnodes[0]
#    else:
#        newnode = node
#        while type(node) == type(newnode):
#            try:
#                newnode = newnode._get_rightnode(stopnode=stopnode)
#                break
#            except (UpperNodeException, StopNodeException):
#                newnode = None
#            except SiblingNodeException:
#                newnode = newnode.uppernode
#
#    return newnode
#
#def DFS_RF(node, basket, stopnode):
#    """Depth-first search from the right of a tree."""
#
#    if len(node.subnodes) > 0:
#        newnode = node.subnodes[-1]
#    else:
#        newnode = node
#        while type(node) == type(newnode):
#            try:
#                newnode = newnode._get_leftnode(stopnode=stopnode)
#                break
#            except (UpperNodeException, StopNodeException):
#                newnode = None
#            except SiblingNodeException:
#                newnode = newnode.uppernode
#
#    return newnode

#def UPWARDS(node, basket, stopnode):
#    """Upward search."""
#
#    if node is stopnode:
#        return None
#
#    return node.uppernode
#
#def NO_SEARCH(node, basket, stopnode):
#    """No search."""
#    return None
#
