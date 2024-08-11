open! Core
open! Import

type position = Source_code_position.t

and location = Ast.location =
  { loc_start : position
  ; loc_end : position
  ; loc_ghost : bool
  }

and location_stack = location list

and 'a loc = 'a Ast.loc =
  { txt : 'a
  ; loc : location
  }

and longident = Ast.longident =
  | Lident of string
  | Ldot of longident * string
  | Lapply of longident * longident

and longident_loc = longident loc

and rec_flag = Ast.rec_flag =
  | Nonrecursive
  | Recursive

and direction_flag = Ast.direction_flag =
  | Upto
  | Downto

and private_flag = Ast.private_flag =
  | Private
  | Public

and mutable_flag = Ast.mutable_flag =
  | Immutable
  | Mutable

and virtual_flag = Ast.virtual_flag =
  | Virtual
  | Concrete

and override_flag = Ast.override_flag =
  | Override
  | Fresh

and closed_flag = Ast.closed_flag =
  | Closed
  | Open

and label = string

and arg_label = Ast.arg_label =
  | Nolabel
  | Labelled of string
  | Optional of string

and variance = Ast.variance =
  | Covariant
  | Contravariant
  | NoVariance

and injectivity = Ast.injectivity =
  | Injective
  | NoInjectivity

and constant = Ast.constant =
  | Pconst_integer of string * char option
  | Pconst_char of char
  | Pconst_string of string * location * string option
  | Pconst_float of string * char option

and attribute = Ast.attribute =
  { attr_name : string loc
  ; attr_payload : payload
  ; attr_loc : location
  }

and extension = string loc * payload
and attributes = attribute list

and payload = Ast.payload =
  | PStr of structure
  | PSig of signature
  | PTyp of core_type
  | PPat of pattern * expression option

and core_type = Ast.core_type =
  { ptyp_desc : core_type_desc
  ; ptyp_loc : location
  ; ptyp_loc_stack : location_stack
  ; ptyp_attributes : attributes
  }

and core_type_desc = Ast.core_type_desc =
  | Ptyp_any
  | Ptyp_var of string
  | Ptyp_arrow of arg_label * core_type * core_type
  | Ptyp_tuple of core_type list
  | Ptyp_constr of longident_loc * core_type list
  | Ptyp_object of object_field list * closed_flag
  | Ptyp_class of longident_loc * core_type list
  | Ptyp_alias of core_type * string
  | Ptyp_variant of row_field list * closed_flag * label list option
  | Ptyp_poly of string loc list * core_type
  | Ptyp_package of package_type
  | Ptyp_extension of extension

and package_type = longident_loc * (longident_loc * core_type) list

and row_field = Ast.row_field =
  { prf_desc : row_field_desc
  ; prf_loc : location
  ; prf_attributes : attributes
  }

and row_field_desc = Ast.row_field_desc =
  | Rtag of label loc * bool * core_type list
  | Rinherit of core_type

and object_field = Ast.object_field =
  { pof_desc : object_field_desc
  ; pof_loc : location
  ; pof_attributes : attributes
  }

and object_field_desc = Ast.object_field_desc =
  | Otag of label loc * core_type
  | Oinherit of core_type

and pattern = Ast.pattern =
  { ppat_desc : pattern_desc
  ; ppat_loc : location
  ; ppat_loc_stack : location_stack
  ; ppat_attributes : attributes
  }

and pattern_desc = Ast.pattern_desc =
  | Ppat_any
  | Ppat_var of string loc
  | Ppat_alias of pattern * string loc
  | Ppat_constant of constant
  | Ppat_interval of constant * constant
  | Ppat_tuple of pattern list
  | Ppat_construct of longident_loc * (string loc list * pattern) option
  | Ppat_variant of label * pattern option
  | Ppat_record of (longident_loc * pattern) list * closed_flag
  | Ppat_array of pattern list
  | Ppat_or of pattern * pattern
  | Ppat_constraint of pattern * core_type
  | Ppat_type of longident_loc
  | Ppat_lazy of pattern
  | Ppat_unpack of string option loc
  | Ppat_exception of pattern
  | Ppat_extension of extension
  | Ppat_open of longident_loc * pattern

and expression = Ast.expression =
  { pexp_desc : expression_desc
  ; pexp_loc : location
  ; pexp_loc_stack : location_stack
  ; pexp_attributes : attributes
  }

and expression_desc = Ast.expression_desc =
  | Pexp_ident of longident_loc
  | Pexp_constant of constant
  | Pexp_let of rec_flag * value_binding list * expression
  | Pexp_function of cases
  | Pexp_fun of arg_label * expression option * pattern * expression
  | Pexp_apply of expression * (arg_label * expression) list
  | Pexp_match of expression * cases
  | Pexp_try of expression * cases
  | Pexp_tuple of expression list
  | Pexp_construct of longident_loc * expression option
  | Pexp_variant of label * expression option
  | Pexp_record of (longident_loc * expression) list * expression option
  | Pexp_field of expression * longident_loc
  | Pexp_setfield of expression * longident_loc * expression
  | Pexp_array of expression list
  | Pexp_ifthenelse of expression * expression * expression option
  | Pexp_sequence of expression * expression
  | Pexp_while of expression * expression
  | Pexp_for of pattern * expression * expression * direction_flag * expression
  | Pexp_constraint of expression * core_type
  | Pexp_coerce of expression * core_type option * core_type
  | Pexp_send of expression * label loc
  | Pexp_new of longident_loc
  | Pexp_setinstvar of label loc * expression
  | Pexp_override of (label loc * expression) list
  | Pexp_letmodule of string option loc * module_expr * expression
  | Pexp_letexception of extension_constructor * expression
  | Pexp_assert of expression
  | Pexp_lazy of expression
  | Pexp_poly of expression * core_type option
  | Pexp_object of class_structure
  | Pexp_newtype of string loc * expression
  | Pexp_pack of module_expr
  | Pexp_open of open_declaration * expression
  | Pexp_letop of letop
  | Pexp_extension of extension
  | Pexp_unreachable

and case = Ast.case =
  { pc_lhs : pattern
  ; pc_guard : expression option
  ; pc_rhs : expression
  }

and letop = Ast.letop =
  { let_ : binding_op
  ; ands : binding_op list
  ; body : expression
  }

and binding_op = Ast.binding_op =
  { pbop_op : string loc
  ; pbop_pat : pattern
  ; pbop_exp : expression
  ; pbop_loc : location
  }

and value_description = Ast.value_description =
  { pval_name : string loc
  ; pval_type : core_type
  ; pval_prim : string list
  ; pval_attributes : attributes
  ; pval_loc : location
  }

and type_declaration = Ast.type_declaration =
  { ptype_name : string loc
  ; ptype_params : (core_type * (variance * injectivity)) list
  ; ptype_cstrs : (core_type * core_type * location) list
  ; ptype_kind : type_kind
  ; ptype_private : private_flag
  ; ptype_manifest : core_type option
  ; ptype_attributes : attributes
  ; ptype_loc : location
  }

and type_kind = Ast.type_kind =
  | Ptype_abstract
  | Ptype_variant of constructor_declaration list
  | Ptype_record of label_declaration list
  | Ptype_open

and label_declaration = Ast.label_declaration =
  { pld_name : string loc
  ; pld_mutable : mutable_flag
  ; pld_type : core_type
  ; pld_loc : location
  ; pld_attributes : attributes
  }

and constructor_declaration = Ast.constructor_declaration =
  { pcd_name : string loc
  ; pcd_vars : string loc list
  ; pcd_args : constructor_arguments
  ; pcd_res : core_type option
  ; pcd_loc : location
  ; pcd_attributes : attributes
  }

and constructor_arguments = Ast.constructor_arguments =
  | Pcstr_tuple of core_type list
  | Pcstr_record of label_declaration list

and type_extension = Ast.type_extension =
  { ptyext_path : longident_loc
  ; ptyext_params : (core_type * (variance * injectivity)) list
  ; ptyext_constructors : extension_constructor list
  ; ptyext_private : private_flag
  ; ptyext_loc : location
  ; ptyext_attributes : attributes
  }

and extension_constructor = Ast.extension_constructor =
  { pext_name : string loc
  ; pext_kind : extension_constructor_kind
  ; pext_loc : location
  ; pext_attributes : attributes
  }

and type_exception = Ast.type_exception =
  { ptyexn_constructor : extension_constructor
  ; ptyexn_loc : location
  ; ptyexn_attributes : attributes
  }

and extension_constructor_kind = Ast.extension_constructor_kind =
  | Pext_decl of string loc list * constructor_arguments * core_type option
  | Pext_rebind of longident_loc

and class_type = Ast.class_type =
  { pcty_desc : class_type_desc
  ; pcty_loc : location
  ; pcty_attributes : attributes
  }

and class_type_desc = Ast.class_type_desc =
  | Pcty_constr of longident_loc * core_type list
  | Pcty_signature of class_signature
  | Pcty_arrow of arg_label * core_type * class_type
  | Pcty_extension of extension
  | Pcty_open of open_description * class_type

and class_signature = Ast.class_signature =
  { pcsig_self : core_type
  ; pcsig_fields : class_type_field list
  }

and class_type_field = Ast.class_type_field =
  { pctf_desc : class_type_field_desc
  ; pctf_loc : location
  ; pctf_attributes : attributes
  }

and class_type_field_desc = Ast.class_type_field_desc =
  | Pctf_inherit of class_type
  | Pctf_val of (label loc * mutable_flag * virtual_flag * core_type)
  | Pctf_method of (label loc * private_flag * virtual_flag * core_type)
  | Pctf_constraint of (core_type * core_type)
  | Pctf_attribute of attribute
  | Pctf_extension of extension

and 'a class_infos = 'a Ast.class_infos =
  { pci_virt : virtual_flag
  ; pci_params : (core_type * (variance * injectivity)) list
  ; pci_name : string loc
  ; pci_expr : 'a
  ; pci_loc : location
  ; pci_attributes : attributes
  }

and class_description = class_type class_infos
and class_type_declaration = class_type class_infos

and class_expr = Ast.class_expr =
  { pcl_desc : class_expr_desc
  ; pcl_loc : location
  ; pcl_attributes : attributes
  }

and class_expr_desc = Ast.class_expr_desc =
  | Pcl_constr of longident_loc * core_type list
  | Pcl_structure of class_structure
  | Pcl_fun of arg_label * expression option * pattern * class_expr
  | Pcl_apply of class_expr * (arg_label * expression) list
  | Pcl_let of rec_flag * value_binding list * class_expr
  | Pcl_constraint of class_expr * class_type
  | Pcl_extension of extension
  | Pcl_open of open_description * class_expr

and class_structure = Ast.class_structure =
  { pcstr_self : pattern
  ; pcstr_fields : class_field list
  }

and class_field = Ast.class_field =
  { pcf_desc : class_field_desc
  ; pcf_loc : location
  ; pcf_attributes : attributes
  }

and class_field_desc = Ast.class_field_desc =
  | Pcf_inherit of override_flag * class_expr * string loc option
  | Pcf_val of (label loc * mutable_flag * class_field_kind)
  | Pcf_method of (label loc * private_flag * class_field_kind)
  | Pcf_constraint of (core_type * core_type)
  | Pcf_initializer of expression
  | Pcf_attribute of attribute
  | Pcf_extension of extension

and class_field_kind = Ast.class_field_kind =
  | Cfk_virtual of core_type
  | Cfk_concrete of override_flag * expression

and class_declaration = class_expr class_infos

and module_type = Ast.module_type =
  { pmty_desc : module_type_desc
  ; pmty_loc : location
  ; pmty_attributes : attributes
  }

and module_type_desc = Ast.module_type_desc =
  | Pmty_ident of longident_loc
  | Pmty_signature of signature
  | Pmty_functor of functor_parameter * module_type
  | Pmty_with of module_type * with_constraint list
  | Pmty_typeof of module_expr
  | Pmty_extension of extension
  | Pmty_alias of longident_loc

and functor_parameter = Ast.functor_parameter =
  | Unit
  | Named of string option loc * module_type

and signature = signature_item list

and signature_item = Ast.signature_item =
  { psig_desc : signature_item_desc
  ; psig_loc : location
  }

and signature_item_desc = Ast.signature_item_desc =
  | Psig_value of value_description
  | Psig_type of rec_flag * type_declaration list
  | Psig_typesubst of type_declaration list
  | Psig_typext of type_extension
  | Psig_exception of type_exception
  | Psig_module of module_declaration
  | Psig_modsubst of module_substitution
  | Psig_recmodule of module_declaration list
  | Psig_modtype of module_type_declaration
  | Psig_modtypesubst of module_type_declaration
  | Psig_open of open_description
  | Psig_include of include_description
  | Psig_class of class_description list
  | Psig_class_type of class_type_declaration list
  | Psig_attribute of attribute
  | Psig_extension of extension * attributes

and module_declaration = Ast.module_declaration =
  { pmd_name : string option loc
  ; pmd_type : module_type
  ; pmd_attributes : attributes
  ; pmd_loc : location
  }

and module_substitution = Ast.module_substitution =
  { pms_name : string loc
  ; pms_manifest : longident_loc
  ; pms_attributes : attributes
  ; pms_loc : location
  }

and module_type_declaration = Ast.module_type_declaration =
  { pmtd_name : string loc
  ; pmtd_type : module_type option
  ; pmtd_attributes : attributes
  ; pmtd_loc : location
  }

and 'a open_infos = 'a Ast.open_infos =
  { popen_expr : 'a
  ; popen_override : override_flag
  ; popen_loc : location
  ; popen_attributes : attributes
  }

and open_description = longident_loc open_infos
and open_declaration = module_expr open_infos

and 'a include_infos = 'a Ast.include_infos =
  { pincl_mod : 'a
  ; pincl_loc : location
  ; pincl_attributes : attributes
  }

and include_description = module_type include_infos
and include_declaration = module_expr include_infos

and with_constraint = Ast.with_constraint =
  | Pwith_type of longident_loc * type_declaration
  | Pwith_module of longident_loc * longident_loc
  | Pwith_modtype of longident_loc * module_type
  | Pwith_modtypesubst of longident_loc * module_type
  | Pwith_typesubst of longident_loc * type_declaration
  | Pwith_modsubst of longident_loc * longident_loc

and module_expr = Ast.module_expr =
  { pmod_desc : module_expr_desc
  ; pmod_loc : location
  ; pmod_attributes : attributes
  }

and module_expr_desc = Ast.module_expr_desc =
  | Pmod_ident of longident_loc
  | Pmod_structure of structure
  | Pmod_functor of functor_parameter * module_expr
  | Pmod_apply of module_expr * module_expr
  | Pmod_constraint of module_expr * module_type
  | Pmod_unpack of expression
  | Pmod_extension of extension

and structure = structure_item list

and structure_item = Ast.structure_item =
  { pstr_desc : structure_item_desc
  ; pstr_loc : location
  }

and structure_item_desc = Ast.structure_item_desc =
  | Pstr_eval of expression * attributes
  | Pstr_value of rec_flag * value_binding list
  | Pstr_primitive of value_description
  | Pstr_type of rec_flag * type_declaration list
  | Pstr_typext of type_extension
  | Pstr_exception of type_exception
  | Pstr_module of module_binding
  | Pstr_recmodule of module_binding list
  | Pstr_modtype of module_type_declaration
  | Pstr_open of open_declaration
  | Pstr_class of class_declaration list
  | Pstr_class_type of class_type_declaration list
  | Pstr_include of include_declaration
  | Pstr_attribute of attribute
  | Pstr_extension of extension * attributes

and value_binding = Ast.value_binding =
  { pvb_pat : pattern
  ; pvb_expr : expression
  ; pvb_attributes : attributes
  ; pvb_loc : location
  }

and module_binding = Ast.module_binding =
  { pmb_name : string option loc
  ; pmb_expr : module_expr
  ; pmb_attributes : attributes
  ; pmb_loc : location
  }

and toplevel_phrase = Ast.toplevel_phrase =
  | Ptop_def of structure
  | Ptop_dir of toplevel_directive

and toplevel_directive = Ast.toplevel_directive =
  { pdir_name : string loc
  ; pdir_arg : directive_argument option
  ; pdir_loc : location
  }

and directive_argument = Ast.directive_argument =
  { pdira_desc : directive_argument_desc
  ; pdira_loc : location
  }

and directive_argument_desc = Ast.directive_argument_desc =
  | Pdir_string of string
  | Pdir_int of string * char option
  | Pdir_ident of longident
  | Pdir_bool of bool

and cases = case list [@@deriving sexp_of, compare]
