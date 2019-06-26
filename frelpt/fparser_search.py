# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

import sys
import os
import inspect
import pyloco

from fparser.two.Fortran2003 import *
from fparser.two.utils import *

from frelpt.error import FparserSearchException

sys.setrecursionlimit(1000)

class Constant_Name(Base):
    subclass_names = ['Name']
    use_names = []

class Int_Constant_Name(Base):
    subclass_names = ['Constant_Name']
    use_names = []

class Scalar_Int_Constant_Name(Base):
    subclass_names = ['Int_Constant_Name']
    use_names = []

class Kind_Param(Base):

    subclass_names = []
    use_names = ['Scalar_Int_Constant_Name']

Type_Name.use_names.append("Name")

# NOTE: (resstmt, scope_stmt, forstmt)

res_all_stmts = {
    Derived_Type_Stmt: (None, None),
    Type_Param_Def_Stmt: (Derived_Type_Def, None), # within Type Construct
    Data_Component_Def_Stmt: (Derived_Type_Def, None), # within Type Construct
    Proc_Component_Def_Stmt: (Derived_Type_Def, None), # within Type Construct
    Specific_Binding: (Derived_Type_Def, None), # within Type Construct
    Generic_Binding: (Derived_Type_Def, None), # within Type Construct
    Final_Binding: (Derived_Type_Def, None), # within Type Construct
    Enumerator_Def_Stmt: (Enum_Def, None), # within Enum_Def
    Type_Declaration_Stmt: (None, None),
    Namelist_Stmt: (None, None),
    Equivalence_Stmt: (None, None),
    External_Stmt: (None, None),
    Common_Stmt: (None, None),
    Bind_Stmt: (None, None),
    Associate_Stmt: (Associate_Construct, None), # within associate construct
    Select_Type_Stmt: (Select_Type_Construct, None), # within select construct
    Do_Stmt: (None, (Cycle_Stmt, Exit_Stmt)),
    Label_Do_Stmt: (None, (Cycle_Stmt, Exit_Stmt)), # for Cycle_Stmt and Exit_Stmt
    Format_Stmt: (None, (Write_Stmt, Read_Stmt)),
    Module_Stmt: (None, (Use_Stmt,)),
    Use_Stmt: (None, None),
    Interface_Stmt: (None, None),
    Import_Stmt: (Interface_Block, None), # within interface construct
    Procedure_Declaration_Stmt: (None, None),
    Function_Stmt: (None, None),
    Subroutine_Stmt: (None, (Call_Stmt,)),
    Entry_Stmt: (None, None),
    Stmt_Function_Stmt: (None, None)
}

# NOTE: spec stmts like access stmt should be handled at second round (previously) of analysis
# NOTE: spec stmts like common stmt should be handled for lazy resolution
# NOTE: lable may also need to be handled beforehand

spec_stmts = [
    Access_Stmt,
    Allocatable_Stmt,
    Asynchronous_Stmt, 
    Data_Stmt,
    Dimension_Stmt,
    Intent_Stmt,
    Intrinsic_Stmt,
    Optional_Stmt,
    Pointer_Stmt,
    Protected_Stmt,
    Save_Stmt,
    Target_Stmt,
    Volatile_Stmt,
    Value_Stmt
]
    

lazy_res_stmts = [
    Implicit_Stmt,
]

res_variable = set([
    Type_Declaration_Stmt,
    Equivalence_Stmt,
    Common_Stmt,
    Namelist_Stmt,
    Use_Stmt,
    Function_Stmt,
    Subroutine_Stmt,
])

res_expr = set([
    Type_Declaration_Stmt,
    Function_Stmt,
    Interface_Stmt,
    Equivalence_Stmt,
    Common_Stmt,
    Namelist_Stmt,
    Stmt_Function_Stmt,
    Use_Stmt,
    External_Stmt,
])

res_function = set([
    Function_Stmt,
    Equivalence_Stmt,
    Interface_Stmt,
    External_Stmt,
    Use_Stmt,
])

res_subroutine = set([
    Subroutine_Stmt,
    Equivalence_Stmt,
    Interface_Stmt,
    External_Stmt,
    Use_Stmt,
])

res_subprogram = res_function | res_subroutine

res_typedecl = set([
    Type_Declaration_Stmt,
    Use_Stmt,
])

res_derivedtypedecl = set([
    Derived_Type_Stmt,
    Equivalence_Stmt,
    Use_Stmt,
])

res_typedecllocal = set([
    Type_Declaration_Stmt,
    Equivalence_Stmt,
])

res_proc = res_subprogram | set([
    Procedure_Declaration_Stmt,
])

res_usename = set([
    Derived_Type_Stmt,
    Type_Declaration_Stmt,
    Namelist_Stmt,
    External_Stmt,
    Common_Stmt,
    Use_Stmt,
    Interface_Stmt,
    Procedure_Declaration_Stmt,
    Function_Stmt,
    Subroutine_Stmt,
    Stmt_Function_Stmt
])

res_part = set([
    Type_Param_Def_Stmt,
    Data_Component_Def_Stmt,
    Proc_Component_Def_Stmt,
    Specific_Binding,
    Generic_Binding,
    Final_Binding,
])


intrinsic_expr = (
    "Int_Expr",
    "Logical_Expr",
)

class PassSet(set):

    def __init__(self, iterable=[]):
        self.elements = iterable

    def __or__(self, other):
        return other

    def __ior__(self, other):
        return other

    def __ror__(self, other):
        return other

    def __and__(self, other):
        return other & set(self.elements)

    def __iand__(self, other):
        return other & set(self.elements)

    def __rand__(self, other):
        return other & set(self.elements)

name_resolvers = {
    "Program_Name": [Program_Stmt],
    "Object_Name": [],
    "Part_Name": [],
    "Scalar_Variable_Name": [],
    "Type_Name": [Derived_Type_Stmt, Use_Stmt],
    "Procedure_Component_Name": [],
    "Procedure_Name": [],
    "Binding_Name": [],
    "Type_Param_Name": [],
    "Entry_Name": [Entry_Stmt],
    "Type_Param_Name_List": [],
    "Component_Name": [],
    "Interface_Name": [],
    "Arg_Name": [],
    "Procedure_Entity_Name": [],
    "Binding_Name_List": [],
    "Final_Subroutine_Name_List": [],
    "Final_Subroutine_Name": [],
    "Function_Name": [Function_Subprogram, Use_Stmt],
    "Subroutine_Name": [Subroutine_Subprogram, Use_Stmt],
    "Procedure_Name_List": [],
    "Object_Name_List": [],
    "Entity_Name": [],
    "Common_Block_Name": [],
    "Proc_Pointer_Name": [],
    "Variable_Name": [],
    "Array_Name": [],
    "External_Name_List": [],
    "External_Name": [],
    "Intrinsic_Procedure_Name_List": [],
    "Intrinsic_Procedure_Name": [],
    "Proc_Entity_Name": [],
    "Entity_Name_List": [],
    "Do_Construct_Name": [Label_Do_Stmt, Nonlabel_Do_Stmt],
    "Index_Name": [],
    "Associate_Construct_Name": [Associate_Stmt],
    "Associate_Name": [],
    "Case_Construct_Name": [],
    "Forall_Construct_Name": [Forall_Construct_Stmt],
    "Where_Construct_Name": [Where_Construct_Stmt],
    "If_Construct_Name": [If_Then_Stmt],
    "Select_Construct_Name": [Select_Type_Stmt],
    "Block_Data_Name": [],
}

class ResBuilder(object):

    def __init__(self, top):

        import fparser.two.Fortran2003 as f2003
        import fparser.two.utils as futil

        self.top = top
        self.f2003 = f2003
        self.futil = futil

        self._grammar = {}
        self._resmap = {}

    def build_grammar(self):

        resmapfile = "resmap.csv"

        if os.path.isfile(resmapfile):
            with open(resmapfile) as fh:
                for line in fh:
                    items = line.strip().split(",")

                    if items:
                        _n, _r = items[0], items[1:]
                        node = globals()[_n]

                        if _r:
                            res = set()

                            for _rn in _r:
                                if not _rn:
                                    continue
                                elif _rn == "PASS":
                                    res = PassSet(name_resolvers[_n])
                                else:
                                    res.add(globals()[_rn])

                            self._resmap[node] = res

                        else:
                            self._resmap[node] = set()
        else:
            self._rhsnodes(self.top)
            self._resolvers([self.top])

            for node in self._grammar:
                if node not in self._resmap:
                    name = node.__name__

                    if name.endswith("_Name") or name.endswith("_Name_List"):
                        print("Unprocessed name: " + name)
                        self._resmap[node] = PassSet(name_resolvers[name])

                    elif name.endswith("_List"):
                        _n = globals()[name[:-5]]

                        if _n in self._resmap:
                            self._resmap[globals()[name]] = self._resmap[_n]

                        else:
                            raise Exception("Unprocessed List: %s" % name)

                    elif name in ("Name", "Int_Variable"):
                        self._resmap[node] = set()

                    else:
                        raise Exception("Unprocessed node: %s" % name)

        # TODO: create res map instead of calculating everytime
        with open("resmap.csv", "w", newline="") as f:
            for n, res in self._resmap.items():
                f.write("%s," % n.__name__)
                if isinstance(res, PassSet):
                    f.write("PASS\n")
                else:
                    lres = list(res)
                    if lres:
                        for r in lres[:-1]: 
                            f.write("%s," % r.__name__)
                        f.write("%s\n" % lres[-1].__name__)
                    else:
                        f.write("\n")

    def _getnode(self, name):

        node = getattr(self.f2003, name, None)
        if node:
            return node

        node = getattr(self.futil, name, None)
        if node:
            return node
 
        if name in globals():
            return globals()[name]
       
        raise Exception("'%s' is not found." % name)

    def _rhsnodes(self, node):

        if node not in self._grammar:
            rhsnodes = []

            for sname in getattr(node, "subclass_names", []):
                snode = self._getnode(sname)
                rhsnodes.append(snode)

            for uname in getattr(node, "use_names", []):
                unode = self._getnode(uname)
                rhsnodes.append(unode)

            if node.__name__.endswith("Literal_Constant"):
                rhsnodes.append(Kind_Param)

            self._grammar[node] = rhsnodes

            for node in rhsnodes:
                self._rhsnodes(node)

    def __getitem__(self, node):

        return self._resmap[node]

    def _resnode(self, path):
        """Run res_*"""

        node = path[-1]
        name = node.__name__
        return getattr(self, "res_" + name)(path)

    def _resres(self, path):
        """Run res_* if exists else _resolvers"""

        node = path[-1]

        f = getattr(self, "res_" + node.__name__, None)

        if f:
            r = f(path)

        else:
            r = self._resolvers(path)

        if node in self._resmap:
            self._resmap[node] |= r

        else:
            self._resmap[node] = r

        return self._resmap[node]

    def _resolvers(self, path):
        """Run res_* for stmts or _resres for the others"""

        node = path[-1]
        name = node.__name__

        if name.endswith("_Stmt"):
            if name.startswith("End_"):
                r = set()
            else:
                r = self._resnode(path)

        else:
            r = set()

            for subnode in self._grammar[node]:

                if subnode is node:
                    continue

                if subnode not in path:
                    newpath = list(path)+[subnode] 
                    r |= self._resres(newpath)

        if node in self._resmap:
            self._resmap[node] |= r

        else:
            self._resmap[node] = r

        #if len(self._resmap[node]) == 0:
        #    print("EEEE", node)

        return self._resmap[node]

    def _subresolvers(self, path):
        """Run _resres for subnodes"""

        node = path[-1]
        rhs = self._grammar[node]

        if len(rhs) == 0:
            r = set()

        elif len(rhs) > 1:
            r = set()

            for subnode in rhs:
                r |= self._resres(list(path) + [subnode])
        else:
            path.append(rhs[0])
            r = self._resres(path)

        if node in self._resmap:
            self._resmap[node] |= r

        else:
            self._resmap[node] = r

        return self._resmap[node]

    def res_Ac_Value(self, path):

        path.append(Ac_Implied_Do)
        return res_expr | self._resres(path)

    def res_Actual_Arg(self, path):

        r1 = self._resres(list(path) + [Proc_Component_Ref])
        path.append(Alt_Return_Spec)
        return res_expr | res_variable | res_proc | r1 | self._resres(path)

        return res_expr | res_variable | res_proc

    def res_Access_Id(self, path):

        return res_usename
        
    def res_Access_Stmt(self, path):

        path.append(Access_Id_List)
        return self._resres(path)

    def res_Action_Stmt(self, path):

        return self._subresolvers(path)

    def res_Add_Operand(self, path):

        path.append(Mult_Operand)
        return self._resres(path)

    def res_Allocate_Object(self, path):

        return res_typedecl

    def res_Allocate_Stmt(self, path):

        return self._subresolvers(path)

    def res_Allocation(self, path):

        path.append(Allocate_Shape_Spec_List)
        return res_variable | self._resres(path)

    def res_Allocatable_Stmt(self, path):

        return self._subresolvers(path)

    def res_And_Operand(self, path):

        return self._subresolvers(path)

    def res_Arithmetic_If_Stmt(self, path):

        return self._subresolvers(path)

    def res_Assignment_Stmt(self, path):

        return res_typedecl | res_expr

    def res_Associate_Stmt(self, path):

        path.append(Association_List)
        return self._resres(path)

    def res_Association(self, path):

        path.append(Selector)
        return self._resres(path)

    def res_Asynchronous_Stmt(self, path):

        #import pdb; pdb.set_trace()
        return res_typedecl

    def res_Backspace_Stmt(self, path):

        path.append(Position_Spec_List)
        return res_typedecl | self._resres(path)

    def res_Bind_Entity(self, path):

        return res_typedecl | set([Common_Stmt])

    def res_Bind_Stmt(self, path):

        r1 = self._resres(list(path) + [Language_Binding_Spec])
        path.append(Bind_Entity_List)
        return r1 | self._resres(path)

    def res_Binding_PASS_Arg_Name(self, path):

        return set()

    def res_Binding_Private_Stmt(self, path):

        return set()

    def res_Block_Data_Stmt(self, path):

        return set()

    def res_Call_Stmt(self, path):

        path.append(Actual_Arg_Spec_List)
        return res_subroutine | self._resres(path)

    def res_Case_Stmt(self, path):

        path.append(Case_Selector)
        return self._resres(path)

    def res_Close_Stmt(self, path):

        return self._subresolvers(path)

    def res_Common_Block_Object(self, path):

        return set()

    def res_Common_Block_Object_List(self, path):

        return self._subresolvers(path)

    def res_Common_Stmt(self, path):

        # NOTE: seperate handling is required for lazy resolution
        #       due to global storage sharing
        path.append(Common_Block_Object_List)
        return self._resres(path)

    def res_Component_Data_Source(self, path):

        return res_expr | res_proc | res_variable

    def res_Component_Decl(self, path):

        r1 = self._resres(list(path) + [Component_Array_Spec])
        r2 = self._resres(list(path) + [Char_Length])
        path.append(Component_Initialization)
        return r1 | r2 | self._resres(path)

    def res_Component_Def_Stmt(self, path):

        return self._subresolvers(path)

    def res_Computed_Goto_Stmt(self, path):

        return self._subresolvers(path)

    def res_Constant_Name(self, path):

        return res_variable

    def res_Contains_Stmt(self, path):

        return set()

    def res_Continue_Stmt(self, path):

        return set()

    def res_Cycle_Stmt(self, path):

        return set([Do_Stmt, Label_Do_Stmt])

    def res_Data_Component_Def_Stmt(self, path):

        r1 = self._resres(list(path) + [Declaration_Type_Spec])
        r2 = self._resres(list(path) + [Component_Attr_Spec_List])
        path.append(Component_Decl_List)
        return r1 | r2 | self._resres(path)

    def res_Data_Stmt_Value(self, path):

        return self._subresolvers(path)

    def res_Data_Implied_Do(self, path):

        return self._subresolvers(path)

    def res_Data_Pointer_Component_Name(self, path):

        return set([Data_Component_Def_Stmt])

    def res_Data_Pointer_Object(self, path):

        path.append(Data_Pointer_Component_Name)
        return res_typedecl | self._resres(path)

    def res_Data_Stmt(self, path):

        return self._subresolvers(path)

    def res_Data_Stmt_Object(self, path):

        path.append(Data_Implied_Do)
        return res_typedecl | self._resres(path)

    def res_Data_Target(self, path):

        return res_variable | res_expr

    def res_Derived_Type_Spec(self, path):

        path.append(Type_Param_Spec_List)
        return res_derivedtypedecl | self._resres(path)

    def res_Derived_Type_Stmt(self, path):

        path.append(Type_Attr_Spec_List)
        return self._resres(path)

    def res_Deallocate_Stmt(self, path):

        return self._subresolvers(path)

    def res_Designator(self, path):

        r1 = self._resres(list(path) + [Array_Section])
        r2 = self._resres(list(path) + [Array_Element])
        r3 = self._resres(list(path) + [Structure_Component])
        path.append(Substring)
        return res_typedecl | res_proc | r1 | r2 | r3 | self._resres(path)
        
    def res_Dimension_Stmt(self, path):

        path.append(Array_Spec)
        return res_typedecl | self._resres(path)

    def res_Do_Term_Action_Stmt(self, path):

        return self._subresolvers(path)

    def res_Do_Term_Shared_Stmt(self, path):

        return self._subresolvers(path)

    def res_Dummy_Arg_Name(self, path):

        return set([Type_Declaration_Stmt])

    def res_Else_If_Stmt(self, path):

        path.append(Scalar_Logical_Expr)
        return self._resres(path)

    def res_Else_Stmt(self, path):

        return set()

    def res_Elsewhere_Stmt(self, path):

        return set()

    def res_Endfile_Stmt(self, path):

        path.append(Position_Spec_List)
        return res_typedecl | self._resres(path)

    def res_Entity_Decl(self, path):

        r1 = self._resres(list(path) + [Array_Spec])
        r2 = self._resres(list(path) + [Char_Length])
        path.append(Initialization)
        r3 = self._resres(path)
        return r1 | r2 | r3

    def res_Entry_Stmt(self, path):

        r1 = self._resres(list(path) + [Dummy_Arg_List])
        path.append(Suffix)
        return r1 | self._resres(path)

    def res_Enum_Def_Stmt(self, path):

        return set()

    def res_Enumerator_Def_Stmt(self, path):

        return self._subresolvers(path)
        #import pdb; pdb.set_trace()

    def res_Equivalence_Stmt(self, path):

        return self._subresolvers(path)

    def res_Equivalence_Object(self, path):

        # TODO: refine this
        return res_variable

    def res_Equiv_Operand(self, path):

        path.append(Or_Operand)
        return self._resres(path)

    def res_Errmsg_Variable(self, path):

        return res_typedecl

    def res_Exit_Stmt(self, path):

        return set([Do_Stmt, Label_Do_Stmt])

    def res_Expr(self, path):

        path.append(Level_5_Expr)
        return res_expr | self._resres(path)
        #return self._subresolvers(path)

    def res_External_Stmt(self, path):

        return set()

    def res_Final_Binding(self, path):

        return set([Subroutine_Stmt])

    def res_Flush_Stmt(self, path):

        path.append(Position_Spec_List)
        return res_typedecl | self._resres(path)

    def res_Forall_Assignment_Stmt(self, path):

        return self._subresolvers(path)

    def res_Forall_Construct_Stmt(self, path):

        path.append(Forall_Header)
        return self._resres(path)

    def res_Forall_Triplet_Spec(self, path):

        r1 = self._resres(list(path) + [Subscript])
        path.append(Stride)
        r2 = self._resres(path)
        return res_typedecl | r1 | r2

    def res_Forall_Stmt(self, path):

        return self._subresolvers(path)

    def res_Format_Stmt(self, path):

        return self._subresolvers(path)

    def res_Function_Reference(self, path):

        return self._subresolvers(path)

    def res_Function_Stmt(self, path):

        r1 = self._resres(list(path) + [Prefix])
        r2 = self._resres(list(path) + [Suffix])
        path.append(Dummy_Arg_Name_List)
        return r1 | r2 | self._resres(path)

    def res_Generic_Binding(self, path):

        path.append(Access_Spec)
        r = self._resres(path)
        return r | set([Function_Stmt, Subroutine_Stmt])

    def res_Generic_Name(self, path):

        return set()

    def res_Goto_Stmt(self, path):

        #import pdb; pdb.set_trace()
        return set()

    def res_If_Then_Stmt(self, path):

        path.append(Scalar_Logical_Expr)
        return self._resres(path)

    def res_If_Stmt(self, path):

        path.append(Action_Stmt_C802)
        return res_expr | self._resres(path)

    def res_Implicit_Part_Stmt(self, path):

        return self._subresolvers(path)
        #self._grammar[path[-1]]

    def res_Implicit_Stmt(self, path):

        return self._subresolvers(path)

    def res_Import_Name(self, path):

        # NOTE: this can import any entity in host scope
        #       ignore for dev.
        #return res_all_stmts
        return set()

    def res_Import_Stmt(self, path):
        return self._subresolvers(path)

    def res_Inquire_Stmt(self, path):

        return self._subresolvers(path)

    def res_Intent_Stmt(self, path):

        return res_typedecl

    def res_Interface_Stmt(self, path):

        return self._subresolvers(path)

    def res_Intrinsic_Stmt(self, path):

        return set()

    def res_Iomsg_Variable(self, path):

        return res_typedecl

    def res_Keyword(self, path):

        return set()

    def res_Level_1_Expr(self, path):

        return self._subresolvers(path)

    def res_Level_2_Unary_Expr(self, path):

        return self._subresolvers(path)

    def res_Level_2_Expr(self, path):

        path.append(Level_2_Unary_Expr)
        return self._resres(path)

    def res_Level_3_Expr(self, path):

        path.append(Level_2_Expr)
        return self._resres(path)

    def res_Level_4_Expr(self, path):

        return self._subresolvers(path)

    def res_Level_5_Expr(self, path):

        path.append(Equiv_Operand)
        return self._resres(path)

    def res_Label_Do_Stmt(self, path):

        path.append(Loop_Control)
        return self._resres(path)

    def res_Local_Defined_Operator(self, path):

        return set()

    def res_Local_Name(self, path):

        return set()

    def res_Masked_Elsewhere_Stmt(self, path):

        path.append(Mask_Expr)
        return self._resres(path)

    def res_Module_Name(self, path):

        return set()

    def res_Module_Stmt(self, path):

        return set()

    def res_Mult_Operand(self, path):

        #return self._subresolvers(path)
        path.append(Level_1_Expr)
        return self._resres(path)

    def res_Name(self, path):

        raise Exception("Unresolved path: %s" % str(path))

    def res_Named_Constant(self, path):

        return set()

    def res_Namelist_Group_Name(self, path):

        return set([Namelist_Stmt])

    def res_Namelist_Group_Object(self, path):

        return set()

    def res_Namelist_Stmt(self, path):

        path.append(Namelist_Group_Object_List)
        return self._resres(path)

    def res_Nonlabel_Do_Stmt(self, path):

        path.append(Loop_Control)
        return self._resres(path)

    def res_Null_Init(self, path):

        return set()

    def res_Nullify_Stmt(self, path):

        return self._subresolvers(path)

    def res_Object_Name_Deferred_Shape_Spec_List_Item(self, path):

        path.append(Deferred_Shape_Spec_List)
        return set([Type_Declaration_Stmt, Equivalence_Stmt]) | self._resres(path)

    def res_Only_Use_Name(self, path):

        return set()

    def res_Open_Stmt(self, path):

        return self._subresolvers(path)

    def res_Optional_Stmt(self, path):

        return res_typedecl

    def res_Or_Operand(self, path):

        path.append(And_Operand)
        return res_typedecl | self._resres(path)

    def res_Parent_String(self, path):

        r1 = self._resres(list(path) + [Array_Element])
        r2 = self._resres(list(path) + [Scalar_Structure_Component])
        path.append(Scalar_Constant)
        return res_variable | r1 | r2 | self._resres(path)

    def res_Parenthesis(self, path):

        return res_expr

    def res_Part_Ref(self, path):

        path.append(Section_Subscript_List)
        return res_typedecl | self._resres(path)

    def res_Pointer_Assignment_Stmt(self, path):

        return self._subresolvers(path)

    def res_Pointer_Decl(self, path):

        path.append(Deferred_Shape_Spec_List)
        return res_typedecl | res_proc | self._resres(path)

    def res_Pointer_Object(self, path):

        path.append(Structure_Component)
        return res_variable | res_typedecl | self._resres(path)

    def res_Pointer_Stmt(self, path):

        return self._subresolvers(path)

    def res_Primary(self, path):

        r1 = self._resres(list(path) + [Array_Constructor])
        r2 = self._resres(list(path) + [Structure_Constructor])
        r3 = self._resres(list(path) + [Function_Reference])
        r3 = self._resres(list(path) + [Type_Param_Inquiry])
        path.append(Parenthesis)
        return res_expr | r1 | r2 | r3 | self._resres(path)

    def res_Print_Stmt(self, path):

        return self._subresolvers(path)


    def res_Proc_Decl(self, path):

        path.append(Null_Init)
        return self._resres(path)

    def res_Proc_Pointer_Object(self, path):

        path.append(Proc_Component_Ref)
        return res_typedecl | self._resres(path)

    def res_Proc_Component_Ref(self, path):

        return res_variable | set([Proc_Component_Def_Stmt])

    def res_Proc_Target(self, path):

        path.append(Proc_Component_Ref)
        return res_expr | res_proc | self._resres(path)

    def res_Procedure_Component_Name(self, path):

        return set([Proc_Component_Def_Stmt])

    def res_Protected_Stmt(self, path):

        return res_typedecl

    def res_Parameter_Stmt(self, path):

        return self._subresolvers(path)

    def res_Parent_Type_Name(self, path):

        return set([Derived_Type_Stmt])

    def res_Position_Spec(self, path):

        return res_typedecl

    def res_Private_Components_Stmt(self, path):

        return set()

    def res_Proc_Binding_Stmt(self, path):

        return self._subresolvers(path)

    def res_Proc_Component_Def_Stmt(self, path):

        r1 = self._resres(list(path) + [Proc_Interface])
        r2 = self._resres(list(path) + [Proc_Component_Attr_Spec_List])
        path.append(Proc_Decl_List)
        return r1 | r2 | self._resres(path)

    def res_Proc_Component_PASS_Arg_Name(self, path):

        return set()

    def res_Proc_Interface(self, path):

        path.append(Declaration_Type_Spec)
        return set([Interface_Stmt]) | self._resres(path)

    def res_Procedure_Declaration_Stmt(self, path):

        r1 = self._resres(list(path) + [Proc_Interface])
        r2 = self._resres(list(path) + [Proc_Decl_List])
        path.append(Proc_Attr_Spec_List)
        return r1 | self._resres(path)

    def res_Procedure_Designator(self, path):

        r1 = self._resres(list(path) + [Proc_Component_Ref])
        path.append(Data_Ref)
        return res_proc | self._resres(path) | set([Proc_Component_Def_Stmt])

    def res_Procedure_Stmt(self, path):

        return set([Function_Stmt, Subroutine_Stmt])

    def res_Program_Stmt(self, path):

        return set()

    def res_Rename(self, path):

        return self._subresolvers(path)

    def res_Read_Stmt(self, path):

        return self._subresolvers(path)

    def res_Result_Name(self, path):

        return res_typedecl

    def res_Return_Stmt(self, path):

        return self._subresolvers(path)

    def res_Rewind_Stmt(self, path):

        return self._subresolvers(path)

    def res_Save_Stmt(self, path):

        return self._subresolvers(path)
        #return res_typedecl

    def res_Saved_Entity(self, path):

        return res_typedecl | res_proc | set([Common_Stmt])

    def res_Scalar_Int_Variable(self, path):

        return res_variable

    def res_Sequence_Stmt(self, path):

        return set()

    def res_Select_Case_Stmt(self, path):

        path.append(Case_Expr)
        return self._resres(path)

    def res_Select_Type_Stmt(self, path):

        path.append(Selector)
        return self._resres(path)

    def res_Specific_Binding(self, path):

        r = self._resres(list(path) + [Binding_Attr_List])
        return set([Interface_Stmt]) | r | set([Function_Stmt, Subroutine_Stmt])

    def res_Specification_Stmt(self, path):

        return self._subresolvers(path)

    def res_Stmt_Function_Stmt(self, path):

        path.append(Scalar_Expr)
        return self._resres(path)

    def res_Stop_Stmt(self, path):

        return self._subresolvers(path)

    def res_Stride(self, path):

        return res_expr

    def res_Subroutine_Stmt(self, path):

        r1 = self._resres(list(path) + [Prefix])
        path.append(Proc_Language_Binding_Spec)
        r2 = self._resres(path)
        return r1 | r2

    def res_Subscript(self, path):

        return res_expr

    def res_Target_Stmt(self, path):

        return self._subresolvers(path)

    def res_Target_Entity_Decl(self, path):

        path.append(Array_Spec)
        return res_typedecl | res_proc | self._resres(path)

    def res_Type_Guard_Stmt(self, path):

        path.append(Type_Spec)
        return self._resres(path)

    def res_Type_Param_Def_Stmt(self, path):

        return self._subresolvers(path)

    def res_Type_Param_Decl(self, path):

        path.append(Scalar_Int_Initialization_Expr)
        return self._resres(path)

    def res_Type_Param_Value(self, path):

        return res_expr

    def res_Type_Declaration_Stmt(self, path):

        return self._subresolvers(path)

    def res_Type_Param_Inquiry(self, path):

        path.append(Designator)
        return set([Type_Param_Def_Stmt]) | self._resres(path)

    def res_Use_Defined_Operator(self, path):

        return set()

    def res_Use_Name(self, path):

        return set()

    def res_Use_Stmt(self, path):

        # NOTE: resolver should handle use stmt
        return self._subresolvers(path)

    def res_Volatile_Stmt(self, path):

        return res_typedecl

    def res_Value_Stmt(self, path):

        return res_typedecl

    def res_Variable(self, path):

        return res_variable

    def res_Vector_Subscript(self, path):

        return res_expr

    def res_Wait_Stmt(self, path):

        return self._subresolvers(path)

    def res_Where_Stmt(self, path):

        #import pdb; pdb.set_trace()
        return self._subresolvers(path)

    def res_Where_Assignment_Stmt(self, path):

        return self._subresolvers(path)

    def res_Where_Construct_Stmt(self, path):

        path.append(Mask_Expr)
        return self._resres(path)

    def res_Write_Stmt(self, path):

        return self._subresolvers(path)

    def done(self):

        total, done = len(self._grammar), len(self._resmap)
        print("Completed %d of %d ( %3.2f )" % (done, total, float(done)/float(total)))

res = ResBuilder(Program)
res.build_grammar()

class Searcher(pyloco.Task):

    def __init__(self, parent):

        self.add_data_argument("node", help="node to search")

        self.register_forward("ids", help="identifiers collected")

    def perform(self, targs):

        ids = {}

        if targs.node:
            #self.log_debug("Searching '%s'"%str(targs.node.wrapped))

            self._search(targs.node, ids, set(res_all_stmts.keys()))

            #self.log_debug(str([n.wrapped for n in ids.keys()]))
            #self.log_debug(str([n.items() for n in ids.values()]))

        self.add_forward(ids=ids)

    def _search(self, node, ids, rtypes):

        clsname = node.__class__.__name__

        if clsname.startswith("End_"):
            return

        if clsname == "Name":
            ids[node] = rtypes
            return

        if hasattr(node, "wrapped"):
            if node.wrapped.__class__ in (list,):
                for subnode in node.subnodes:
                    self._search(subnode, ids, set(rtypes))
            else:
                rtypes = rtypes & res[node.wrapped.__class__]

        elif node in (None,) or isinstance(node, (str,)):
            return

        else:
            print("UNKNWON NODE TYPE: ", node.__class__, node)
            import pdb; pdb.set_trace()

        if clsname.endswith("_List"):
            for subnode in node.subnodes:
                self._search(subnode, ids, set(rtypes))
        else:
            getattr(self, "search_"+clsname)(node, ids, rtypes)


    def _search_subnodes(self, node, ids, rtypes, includes=None, excludes=[]):

        subnodes = []
        nsubnodes = len(node.subnodes)
        rtypes = rtypes & res[node.wrapped.__class__]

        if includes:
            for item in includes:
                if item in node.subnodes:
                    subnodes.append(item)
                elif isinstance(item, int) and item < nsubnodes:
                    subnodes.append(node.subnodes[item])
        else:
            subnodes = node.subnodes

        for idx, subnode in enumerate(subnodes):

            if idx in excludes or subnode in excludes:
                continue

            self._search(subnode, ids, rtypes)

    def _search_noname(self, node, ids, rtypes):

        if node.__class__.__name__ != "Name":
            self._search(node, ids, rtypes)


    def search_Access_Stmt(self, node, ids, rtypes):

        self._search(node.subnodes[1], ids, rtypes)

    def search_Add_Operand(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes, excludes=[1])

    def search_Actual_Arg(self, node, ids, rtypes):
        """
        <actual-arg> = <expr>
                     | <variable>
                     | <procedure-name>
                     | <proc-component-ref>
                     | <alt-return-spec>
        """
        import pdb; pdb.set_trace()

    def search_Actual_Arg_Spec(self, node, ids, rtypes):
        """
        <actual-arg-spec> = [ <keyword> = ] <actual-arg>
        """
        import pdb; pdb.set_trace()


    def resolve_Allocate_Stmt(self, node, ids, rtypes):

        import pdb; pdb.set_trace()

    def search_Assignment_Stmt(self, node, ids, rtypes):
        """
        <assignment-stmt> = <variable> = <expr>
        """
 
        self._search(node.subnodes[0], ids, rtypes)
        self._search(node.subnodes[2], ids, rtypes)

    def search_Assumed_Shape_Spec(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)

    def search_Attr_Spec(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)

    def search_Block_Nonlabel_Do_Construct(self, node, ids, rtypes):
        """
         R826_2

        <block-nonlabel-do-construct> = <nonlabel-do-stmt>
                                         [ <execution-part-construct> ]...
                                         <end-do-stmt>
        """
        self._search_subnodes(node, ids, rtypes)


    def search_Call_Stmt(self, node, ids, rtypes):
        """
        <call-stmt> = CALL <procedure-designator>
                      [ ( [ <actual-arg-spec-list> ] ) ]
        """

        self._search(node.subnodes[0], ids, rtypes)
        self._search(node.subnodes[1], ids, rtypes)

    def search_Comment(self, node, ids, rtypes):
        pass

    def search_Contains_Stmt(self, node, ids, rtypes):
        pass

    def search_Declaration_Type_Spec(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)

    def search_Derived_Type_Def(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)

    def search_Derived_Type_Stmt(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes, excludes=[1])

    def search_Dimension_Attr_Spec(self, node, ids, rtypes):
        """
        <dimension-attr-spec> = DIMENSION ( <array-spec> )
        """
        self._search(node.subnodes[1], ids, rtypes)

    def search_Entity_Decl(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes, excludes=[0])

    def search_Execution_Part(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)

    def search_Explicit_Shape_Spec(self, node, ids, rtypes):
        """
        <explicit-shape-spec> = [ <lower-bound> : ] <upper-bound>
        """
        self._search(node.subnodes[0], ids, rtypes)
        self._search(node.subnodes[1], ids, rtypes)

    def search_Function_Stmt(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes, excludes=[1])

    def search_Function_Subprogram(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)


    def search_If_Construct(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)

    def search_If_Stmt(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)

    def search_Implicit_Part(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)

    def search_Implicit_Stmt(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)

    def search_Int_Literal_Constant(self, node, ids, rtypes):

        self._search(node.subnodes[1], ids, rtypes)

    def search_Internal_Subprogram_Part(self, node, ids, rtypes):

        self._search(node.subnodes[1], ids, rtypes)

    def search_Intrinsic_Type_Spec(self, node, ids, rtypes):
        """
        <intrinsic-type-spec> = INTEGER [ <kind-selector> ]
                                | REAL [ <kind-selector> ]
                                | DOUBLE COMPLEX
                                | COMPLEX [ <kind-selector> ]
                                | CHARACTER [ <char-selector> ]
                                | LOGICAL [ <kind-selector> ]
        Extensions:
                                | DOUBLE PRECISION
                                | BYTE
        """

        i_type, selector = node.subnodes

        if i_type == "CHARACTER":
            self._search(node.subnodes[1], ids, rtypes)
        else: 
            self._search(node.subnodes[1], ids, rtypes)

    def search_Level_2_Expr(self, node, ids, rtypes):
        """
        <level-2-expr> = [ [ <level-2-expr> ] <add-op> ] <add-operand>
        <level-2-expr> = [ <level-2-expr> <add-op> ] <add-operand>
                         | <level-2-unary-expr>
        <add-op>   = +
                     | -
        """
 
        self._search(node.subnodes[0], ids, rtypes)
        self._search(node.subnodes[2], ids, rtypes)

    def search_Logical_Literal_Constant(self, node, ids, rtypes):
        pass

    def search_Loop_Control(self, node, ids, rtypes):
        """
            R830

            <loop-control> = [ , ] <do-variable> = scalar-int-expr,
                                                   scalar-int-expr
                                                   [ , <scalar-int-expr> ]
                             | [ , ] WHILE ( <scalar-logical-expr> )
        """

        scalar_logical_expr, counter_expr, optional_delim = node.subnodes

        if scalar_logical_expr is not None:
            self._search(scalar_logical_expr, ids, rtypes)
        elif counter_expr[0] is not None and counter_expr[1] is not None:
            self._search(counter_expr[0], ids, rtypes)
            self._search(counter_expr[1], ids, rtypes)

    def search_Module(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)

    def search_Module_Stmt(self, node, ids, rtypes):
        pass

    def search_Module_Subprogram_Part(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)

    def search_Mult_Operand(self, node, ids, rtypes):

        self._search(node.subnodes[0], ids, rtypes)
        self._search(node.subnodes[2], ids, rtypes)

    def search_Nonlabel_Do_Stmt(self, node, ids, rtypes):
        """
            R829

            <nonlabel-do-stmt> = [ <do-construct-name> : ] DO [ <loop-control> ]
        """

        self._search(node.subnodes[1], ids, rtypes)


    def search_NoneType(self, node, ids, rtypes):
        pass

    def search_Part_Ref(self, node, ids, rtypes):
        """
        <part-ref> = <part-name> [ ( <section-subscript-list> ) ]
        """
        self._search(node.subnodes[0], ids, rtypes) # assumes rtypes are already specified
        self._search(node.subnodes[1], ids, rtypes)

    def search_Pointer_Assignment_Stmt(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)

    def search_Prefix_Spec(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)

    def search_Procedure_Designator(self, node, ids, rtypes):
        """
        <procedure-designator> = <procedure-name>
                                 | <proc-component-ref>
                                 | <data-ref> % <binding-name>
        """

        if "%" in node.subnodes:
            idx = node.subnodes.index("%")
            self._search_subnodes(node, ids, rtypes, includes=node.subnodes[:idx])
        else:
            self._search_subnodes(node, ids, rtypes)

    def search_Program(self, node, ids, rtypes):
        self._search_subnodes(node, ids, rtypes)

    def search_Real_Literal_Constant(self, node, ids, rtypes):

        self._search(node.subnodes[1], ids, rtypes)

    def search_Specific_Binding(self, node, ids, rtypes):
        """
        <specific-binding> = PROCEDURE [ ( <interface-name> ) ] [
            [ , <binding-attr-list> ] :: ] <binding-name> [ => <procedure-name> ]

        iname, mylist, dcolon, Binding_Name(line), pname
        """

        if node.subnodes[4] is None:
            self._search_subnodes(node, ids, rtypes, excludes=[2])
        else:
            self._search_subnodes(node, ids, rtypes, excludes=[2,3])

    def search_Specification_Part(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)

    def search_Structure_Constructor_2(self, node, ids, rtypes):

        self._search(node.subnodes[1], ids, rtypes)

    def search_Subroutine_Stmt(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes, excludes=[1])

    def search_Subroutine_Subprogram(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)

    def search_Subscript_Triplet(self, node, ids, rtypes):
        """
        <subscript-triplet> = [ <subscript> ] : [ <subscript> ] [ : <stride> ]
        """

        self._search_subnodes(node, ids, rtypes)

    def search_Suffix(self, node, ids, rtypes):

        self._search(node.subnodes[0], ids, rtypes)

    def search_Type_Bound_Procedure_Part(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)

    def search_Type_Declaration_Stmt(self, node, ids, rtypes):
        """
        <type-declaration-stmt> = <declaration-type-spec> [
            [ , <attr-spec> ]... :: ] <entity-decl-list>
        """

        self._search(node.subnodes[0], ids, rtypes)
        self._search(node.subnodes[1], ids, rtypes)
        self._search_noname(node.subnodes[2], ids, rtypes)

    def search_Type_Name(self, node, ids, rtypes):

        self._search_subnodes(node, ids, rtypes)

    def search_Tuple(self, node, ids, rtypes):

        for subnode in node.subnodes:
            self._search(subnode, ids, rtypes)

    def search_Use_Stmt(self, node, ids, rtypes):
        pass
