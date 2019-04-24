# -*- coding: utf-8 -*-

from __future__ import unicode_literals, print_function

import pyloco

from fparser.two.Fortran2003 import *
from fparser.two.utils import *

from frelpt.error import FparserSearchException


class Res(object):

    def __init__(self, *items):

        self._items = set()

        for item in items:
            if isinstance(item, Res):
                self._items |= item._items
            elif isinstance(item, (list, tuple, set)):
                self._items |= set(item)
            else:
                self._items.add(item)

        # NOTE: or may use a private class for Implict Rules
        self.implicit_rules = []

    def __contains__(self, item):
        return item in self._items

    def __len__(self):
        return len(self._items)

    def __or__(self, other):
        return Res(self, other)

    def __and__(self, other):
        if other is None:
            return Res(self)
        else:
            return Res(self._items & other._items)

    def items(self):
        return list(self._items)

#xyz-list := xyz [ , xyz ] ...
#xyz-name := name
#scalar-xyz := xyz

# primary types

# constant : N/A
# designator : TypeDecl
# array-constructor : N/A
# structure-constructor : Type
# function-reference : Function, Interface, External
# type-param-inquiry
# type-param-name
# (expr)

# find primary types that can be expr, name, ...
# find possible resolvers of the primitive types

# PREVIOUS VERSION
#res_default = [ TypeDeclarationStatement ]
##term_external = [ External, Procedure ] # TEMP
#term_external = [ External ]
#term_typedecl = [ TypeDeclarationStatement ]
#res_typestmt = [ TypeStmt ]
#res_derivedtype = [ Type, TypeDecl ]
#res_associate = [ Associate ]
#res_kind = [ TypeDeclarationStatement ] + res_derivedtype
#res_typespec = [ TypeDeclarationStatement ] + res_derivedtype
#res_value = [ TypeDeclarationStatement, Function, Interface ] + term_external + res_associate
#term_subroutine = [ Subroutine, Interface ] + term_external
#term_function = [ Function, Interface ] + term_external
#res_subprogram = [ Subroutine, Function, Interface ] + term_external
#res_common = [ Common ]
#res_ptr_object = [ SpecificBinding, TypeDeclarationStatement ]
#res_target = res_subprogram + term_typedecl
#res_anything = res_typespec + res_subprogram + [ SpecificBinding, Common, Type, TypeDecl ]

# # resolver types
# # NOTE: primary res is a statement level
# 
# # terminal resolvers
# term_pass            = Res()
# term_external        = Res(External_Stmt)
# term_function        = Res(Function_Stmt)
# term_interface       = Res(Interface_Stmt)
# term_subroutine      = Res(Subroutine_Stmt)
# term_typedecl        = Res(Type_Declaration_Stmt)
# term_parameter       = Res(Parameter_Stmt)
# term_specific_binding= Res(Specific_Binding)
# term_generic_binding = Res(Generic_Binding)
# term_final_binding   = Res(Final_Binding)
# term_type_name       = Res(Derived_Type_Stmt)
# term_type_param_name = Res(Type_Param_Def_Stmt)
# term_use_name        = Res(Use_Stmt)
# 
# # helper resolvers
# res_dummy_procedure         = term_function
# res_external_procedure      = term_external
# res_constant_name           = term_typedecl | term_parameter
# res_object_designator       = term_external | term_typedecl | term_parameter
# res_digit_string    = term_pass
# 
# # res_procedure_designator    = term_external | term_function | term_interface | res_proc_binding
# # res_procedure_designator    = res_procedure_name | res_proc_component_ref | res_proc_binding
# 
# # for forward declaration
# #res_expr                    = (res_designator | res_array_constructor | res_structure_constructor |
# #                               term_function_reference | res_type_param_inquiry | term_type_param_name)
# # res_expr = res_object_designator | res_procedure_designator
# 
# 
# #     <designator> = <object-name>
# #                    | <array-element>
# #                    | <array-section>
# #                    | <structure-component>
# #                    | <substring>
# #     <substring-range> = [ <scalar-int-expr> ] : [ <scalar-int-expr> ]
# #     <structure-component> = <data-ref>
# 
# #     <procedure-designator> = <procedure-name>
# #                              | <proc-component-ref>
# #                              | <data-ref> % <binding-name>
# 
# 
# res_constant        = res_literal_constant | res_named_constant
# res_expr            = res_constant | res_designator | res_array_constructor | res_structure_constructor | res_function_reference | res_type_param_inquiry | res_type_param_name
# 
# # final resolvers
# res_designator              = res_object_designator | res_procedure_designator
# res_int_expr                = res_expr
# res_scalar_int_initialization_expr = res_int_initialization_expr
# term_function_reference      = term_function
# res_module_subprogram       = term_function | term_subroutine
# res_module_procedure        = res_module_subprogram
# res_procedure_pointer       = term_pass # FOR DEBUG ONLY
# res_specific_intrinsic_function =  term_pass # FOR DEBUG ONLY
# res_procedure_name          = (res_external_procedure | res_dummy_procedure | res_module_procedure |
#                                res_procedure_pointer | res_specific_intrinsic_function)
# res_char_expr               = res_expr
# res_default_char_expr       = res_expr
# res_numeric_expr            = res_expr
# res_initialization_expr     = res_expr
# res_char_initialization_expr= res_char_expr
# res_logical_initialization_expr = res_logical_expr
# res_int_constant_name       = res_constant_name
# res_scalar_int_constant_name= res_int_constant_name
# res_kind_param              = res_digit_string | res_scalar_int_constant_name

#for name in dir():
#    if name.startswith("res_"):
#        res = globals()[name]
#        print("SS", name, res.items())
#        if 0 in res:
#            res._items.remove(0)
#            res._items.union(res_expr._items)
# # terminal resolvers
# term_type_param_name = Res(Type_Param_Def_Stmt)

# terminal resolvers
term_pass                   = Res()
term_type_declaration_stmt  = Res(Type_Declaration_Stmt)
term_function_stmt          = Res(Function_Stmt)
term_interface_stmt         = Res(Interface_Stmt)
term_subroutine_stmt        = Res(Subroutine_Stmt)
term_use_stmt               = Res(Use_Stmt)
term_external_stmt          = Res(External_Stmt)
term_parameter_stmt         = Res(Parameter_Stmt)
term_derived_type_stmt      = Res(Derived_Type_Stmt)
term_specific_binding       = Res(Specific_Binding)
term_generic_binding        = Res(Generic_Binding)
term_final_binding          = Res(Final_Binding)

# combined resolvers
res_variable                = term_type_declaration_stmt | term_use_stmt
res_type_name               = term_derived_type_stmt | term_use_stmt
res_label                   = term_pass # do not support label for now
res_procedure_name          = term_function_stmt | term_subroutine_stmt | term_external_stmt | term_interface_stmt | term_use_stmt

#res_expr                    = Res() # FOR DEV.
res_expr                    = term_type_declaration_stmt | term_function_stmt | term_interface_stmt | term_subroutine_stmt | term_use_stmt | term_external_stmt | term_parameter_stmt | term_derived_type_stmt | term_specific_binding | term_generic_binding | term_final_binding
res_primary                 = res_expr

# expr resolver
res_constant_name           = term_type_declaration_stmt | term_parameter_stmt
res_int_constant_name       = res_constant_name
res_scalar_int_constant_name= res_int_constant_name
res_kind_param              = res_scalar_int_constant_name
res_int_literal_constant    = res_kind_param
res_real_literal_constant   = res_kind_param
res_signed_int_literal_constant = res_int_literal_constant
res_signed_real_literal_constant = res_real_literal_constant
res_named_constant          = res_constant_name
res_real_part               = res_signed_int_literal_constant | res_signed_real_literal_constant | res_named_constant
res_imag_part               = res_signed_int_literal_constant | res_signed_real_literal_constant | res_named_constant
res_complex_literal_constant= res_real_part | res_imag_part
res_logical_literal_constant= res_kind_param
res_char_literal_constant   = res_kind_param
res_binary_constant         = term_pass
res_octal_constant          = term_pass
res_hex_constant            = term_pass
res_boz_literal_constant    = res_binary_constant | res_octal_constant | res_hex_constant
res_literal_constant        = res_int_literal_constant | res_real_literal_constant | res_complex_literal_constant | res_logical_literal_constant | res_char_literal_constant | res_boz_literal_constant
res_constant                = res_literal_constant | res_named_constant
res_int_expr                = res_expr
res_scalar_int_expr         = res_int_expr
res_subscript               = res_scalar_int_expr
res_stride                  = res_scalar_int_expr
res_subscript_triplet       = res_subscript | res_stride
res_vector_subscript        = res_int_expr
res_section_subscript       = res_subscript | res_subscript_triplet | res_vector_subscript
res_part_name               = res_variable
res_part_ref                = res_part_name | res_section_subscript
res_data_ref                = res_part_ref
res_array_element           = res_data_ref
res_structure_component     = res_data_ref
res_variable_name           = res_variable
res_scalar_variable_name    = res_variable_name
res_scalar_structure_component = res_data_ref
res_scalar_constant         = res_constant
res_parent_string           = res_scalar_variable_name | res_array_element | res_scalar_structure_component | res_scalar_constant
res_substring_range         = res_scalar_int_expr
res_substring               = res_parent_string | res_substring_range
res_array_section           = res_data_ref | res_substring_range
res_designator              = res_variable | res_array_element | res_array_section | res_structure_component | res_substring
res_ac_implied_do           = term_pass # FOR DEBUG ONLY
res_ac_value                = res_expr | res_ac_implied_do
res_int_initialization_expr = res_int_expr
res_scalar_int_initialization_expr = res_int_initialization_expr
res_kind_selector           = res_scalar_int_initialization_expr
res_type_param_value        = res_scalar_int_expr
res_char_length             = res_type_param_value
res_length_selector         = res_type_param_value | res_char_length
res_char_selector           = res_length_selector | res_type_param_value | res_scalar_int_initialization_expr
res_intrinsic_type_spec     = res_kind_selector | res_char_selector
res_type_param_spec         = res_type_param_value
res_derived_type_spec       = res_type_name | res_type_param_spec
res_type_spec               = res_intrinsic_type_spec | res_derived_type_spec
res_ac_spec                 = res_type_spec | res_ac_value
res_array_constructor       = res_ac_spec
res_component_spec          = term_pass # FOR DEBUG ONLY
res_structure_constructor   = res_derived_type_spec | res_component_spec
res_procedure_component_name= term_pass # FOR DEBUG ONLY
res_proc_component_ref      = res_variable | res_procedure_component_name
res_proc_binding            = term_specific_binding | term_generic_binding | term_final_binding
res_procedure_designator    = res_procedure_name | res_proc_component_ref | res_proc_binding
res_alt_return_spec         = res_label
res_actual_arg              = res_expr | res_variable | res_procedure_name | res_proc_component_ref | res_alt_return_spec
res_actual_arg_spec         = res_actual_arg
res_function_reference      = res_procedure_designator | res_actual_arg_spec
res_type_param_name         = term_pass # FOR DEBUG ONLY
res_type_param_inquiry      = res_designator | res_type_param_name
#res_expr                    = res_constant | res_designator | res_array_constructor | res_structure_constructor | res_function_reference | res_type_param_inquiry | res_type_param_name

# derived resolvers
res_int_variable            = res_variable
res_scalar_int_variable     = res_int_variable
res_do_variable             = res_scalar_int_variable
res_logical_expr            = res_expr
res_scalar_logical_expr     = res_logical_expr
res_loop_control            = res_do_variable | res_scalar_int_expr | res_scalar_logical_expr

res_specification_expr      = res_expr
res_lower_bound             = res_specification_expr
res_upper_bound             = res_specification_expr
res_explicit_shape_spec     = res_lower_bound | res_upper_bound
res_assumed_shape_spec      = res_lower_bound
res_assumed_size_spec       = res_explicit_shape_spec | res_lower_bound
res_deferred_shape_spec     = term_pass
res_array_spec              = (res_assumed_shape_spec | res_assumed_size_spec | res_deferred_shape_spec |
                               res_explicit_shape_spec)

res_level_1_expr            = res_primary
res_mult_operand            = res_level_1_expr
res_add_operand             = res_mult_operand
res_level_2_expr            = res_add_operand

class Searcher(pyloco.Task):

    def __init__(self, parent):

        self.add_data_argument("node", help="node to search")

        self.register_forward("ids", help="identifiers collected")

    def perform(self, targs):

        ids = {}

        if targs.node:
            self.log_debug("Searching '%s'"%str(targs.node.wrapped))

            self._search(targs.node, ids)

            self.log_debug(str([n.wrapped for n in ids.keys()]))
            self.log_debug(str([n.items() for n in ids.values()]))

        self.add_forward(ids=ids)

    def _search(self, node, ids, rtypes=None):
        clsname = node.__class__.__name__
        if clsname.startswith("End_"):
            return
        if clsname.endswith("_List"):
            for subnode in node.subnodes:
                clsname = subnode.__class__.__name__
                getattr(self, "search_"+clsname)(subnode, ids, rtypes=rtypes)
        else:
            getattr(self, "search_"+clsname)(node, ids, rtypes=rtypes)

    def search_Actual_Arg(self, node, ids, rtypes=None):
        """
        <actual-arg> = <expr>
                     | <variable>
                     | <procedure-name>
                     | <proc-component-ref>
                     | <alt-return-spec>
        """
        import pdb; pdb.set_trace()

    def search_Actual_Arg_Spec(self, node, ids, rtypes=None):
        """
        <actual-arg-spec> = [ <keyword> = ] <actual-arg>
        """
        import pdb; pdb.set_trace()

    def search_Assignment_Stmt(self, node, ids, rtypes=None):
        """
        <assignment-stmt> = <variable> = <expr>
        """
 
        self._search(node.subnodes[0], ids, rtypes=res_variable & rtypes)
        self._search(node.subnodes[2], ids, rtypes=res_expr & rtypes)

    def search_Call_Stmt(self, node, ids, rtypes=None):
        """
        <call-stmt> = CALL <procedure-designator>
                      [ ( [ <actual-arg-spec-list> ] ) ]
        """

        self._search(node.subnodes[0], ids, rtypes=res_procedure_designator & rtypes)
        self._search(node.subnodes[1], ids, rtypes=res_actual_arg_spec & rtypes)

    def search_Dimension_Attr_Spec(self, node, ids, rtypes=None):
        """
        <dimension-attr-spec> = DIMENSION ( <array-spec> )
        """
        self._search(node.subnodes[1], ids, rtypes=res_array_spec & rtypes)

    def search_Explicit_Shape_Spec(self, node, ids, rtypes=None):
        """
        <explicit-shape-spec> = [ <lower-bound> : ] <upper-bound>
        """
        self._search(node.subnodes[0], ids, rtypes=res_lower_bound & rtypes)
        self._search(node.subnodes[1], ids, rtypes=res_upper_bound & rtypes)

    def search_Int_Literal_Constant(self, node, ids, rtypes=None):

        self._search(node.subnodes[1], ids, rtypes=res_kind_param & rtypes)

    def search_Intrinsic_Type_Spec(self, node, ids, rtypes=None):
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
            self._search(node.subnodes[1], ids, rtypes=res_char_selector & rtypes)
        else: 
            self._search(node.subnodes[1], ids, rtypes=res_kind_selector & rtypes)

    def search_Level_2_Expr(self, node, ids, rtypes=None):
        """
        <level-2-expr> = [ [ <level-2-expr> ] <add-op> ] <add-operand>
        <level-2-expr> = [ <level-2-expr> <add-op> ] <add-operand>
                         | <level-2-unary-expr>
        <add-op>   = +
                     | -
        """
 
        self._search(node.subnodes[0], ids, rtypes=res_level_2_expr & rtypes)
        self._search(node.subnodes[2], ids, rtypes=res_add_operand & rtypes)

    def search_Loop_Control(self, node, ids, rtypes=None):
        """
            R830

            <loop-control> = [ , ] <do-variable> = scalar-int-expr,
                                                   scalar-int-expr
                                                   [ , <scalar-int-expr> ]
                             | [ , ] WHILE ( <scalar-logical-expr> )
        """

        scalar_logical_expr, counter_expr, optional_delim = node.subnodes

        if scalar_logical_expr is not None:
            self._search(scalar_logical_expr, ids, rtypes=res_scalar_logical_expr & rtypes)
        elif counter_expr[0] is not None and counter_expr[1] is not None:
            self._search(counter_expr[0], ids, rtypes=res_scalar_int_expr & rtypes)
            self._search(counter_expr[1], ids, rtypes=res_scalar_int_expr & rtypes)

    def search_Name(self, node, ids, rtypes=None):
        """
            Fortran 2003 rule R304
            name is letter [ alphanumeric_character ]...
        """

        if not rtypes:
            raise FparserSearchException("Resolver type for '%s', is not specified."%node.wrapped.string)

        ids[node] = rtypes

    def search_Nonlabel_Do_Stmt(self, node, ids, rtypes=None):
        """
            R829

            <nonlabel-do-stmt> = [ <do-construct-name> : ] DO [ <loop-control> ]
        """

        self._search(node.subnodes[1], ids, rtypes=res_loop_control & rtypes)


    def search_NoneType(self, node, ids, rtypes=None):
        pass

    def search_Part_Ref(self, node, ids, rtypes=None):
        """
        <part-ref> = <part-name> [ ( <section-subscript-list> ) ]
        """
        self._search(node.subnodes[0], ids, rtypes=rtypes) # assumes rtypes are already specified
        self._search(node.subnodes[1], ids, rtypes=res_section_subscript & rtypes)

    def search_Subscript_Triplet(self, node, ids, rtypes=None):
        """
        <subscript-triplet> = [ <subscript> ] : [ <subscript> ] [ : <stride> ]
        """
        lhs_obj, rhs_obj, stride_obj = node.subnodes

        self._search(lhs_obj, ids, rtypes=res_subscript & rtypes)
        self._search(rhs_obj, ids, rtypes=res_subscript & rtypes)
        self._search(stride_obj, ids, rtypes=res_stride & rtypes)

    def search_Tuple(self, node, ids, rtypes=None):

        for node in node.subnodes:
            self._search(node, ids, rtypes=rtypes)
