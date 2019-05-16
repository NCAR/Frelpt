# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

from fparser.two.Fortran2003 import *
from fparser.two.utils import *

from frelpt.fparser_util import get_parent_by_class


class FparserArgument(object):

    def __init__(self, callername, calleename):

        self.callername = callername
        self.calleename = calleename

    def actual2dummy(self, aarg, index_offset=0):

        # get the top level actual argument from aarg
        toparg = None
        pnode = aarg

        while pnode and hasattr(pnode, "parent"):
            if isinstance(pnode.parent.wrapped, Actual_Arg_Spec_List):
                toparg = pnode
                break
            pnode = pnode.parent

        if toparg is None:
            raise Exception("Top argument is not found")

        # locate dummy argument
        if isinstance(toparg.wrapped, Actual_Arg_Spec):
            lhs, rhs = toparg.subnodes

            if lhs:
                import pdb; pdb.set_trace()
            else:
                argidx = toparg.parent.subnodes.index(toparg)
                return self.get_dummyarg_by_index(argidx-index_offset)

        else:
            argidx = toparg.parent.subnodes.index(toparg)
            return self.get_dummyarg_by_index(argidx-index_offset)

    def get_dummyarg_by_index(self, index):

        if isinstance(self.calleename.parent.wrapped, Subroutine_Stmt):
            arglist = self.calleename.parent.subnodes[2]
            return arglist.subnodes[index]
        else:
            import pdb; pdb.set_trace()
