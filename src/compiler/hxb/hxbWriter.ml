open Globals
open Ast
open Type

open HxbChunks
open HxbEnums

exception HxbWriteFailure of string

type 'a pool = ('a, int) Hashtbl.t * 'a DynArray.t
let create_pool : 'a . unit -> 'a pool = fun () ->
	Hashtbl.create 0, DynArray.create ()
let grow_pools : 'a . 'a pool DynArray.t -> int -> unit = fun pools index ->
	while index >= DynArray.length pools do
		DynArray.add pools (create_pool ())
	done

class ['a] hxb_writer
	(file_ch : 'a IO.output)
	(current_module : module_def)
	= object(self)

	val mutable ch : bytes IO.output = IO.output_bytes ()

	val mutable last_file = ""
	val mutable last_delta = 0

	val string_pool : string pool = create_pool ()
	val doc_pool : string pool = create_pool ()
	val external_classes_pool : path pool = create_pool ()
	val external_enums_pool : path pool = create_pool ()
	val external_abstracts_pool : path pool = create_pool ()
	val external_typedefs_pool : path pool = create_pool ()
	val internal_classes_pool : tclass pool = create_pool ()
	val internal_enums_pool : tenum pool = create_pool ()
	val internal_abstracts_pool : tabstract pool = create_pool ()
	val internal_typedefs_pool : tdef pool = create_pool ()

	val internal_class_field_pools : tclass_field pool DynArray.t = DynArray.create ()
	val external_class_field_pools : tclass_field pool DynArray.t = DynArray.create ()
	val internal_enum_field_pools : tenum_field pool DynArray.t = DynArray.create ()
	val external_enum_field_pools : tenum_field pool DynArray.t = DynArray.create ()

	(* Primitives *)

	method write_u8 v =
		IO.write_byte ch v

	method write_u32 v =
		IO.write_real_i32 ch v

	method write_f64 v =
		IO.write_double ch v

	method write_uleb128 v =
		let b = v land 0x7F in
		let rest = v lsr 7 in
		if rest = 0 then
			self#write_u8 b
		else begin
			self#write_u8 (b lor 0x80);
			self#write_uleb128 rest
		end

	method write_leb128 v =
		let b = v land 0x7F in
		let rest = v asr 7 in
		if (rest = 0 && (b land 0x40 = 0)) || (rest = -1 && (b land 0x40 = 0x40)) then
			self#write_u8 b
		else begin
			self#write_u8 (b lor 0x80);
			self#write_leb128 rest
		end

	method write_str_raw v =
		IO.nwrite ch v

	method write_str v =
		self#write_uleb128 (String.length v);
		self#write_str_raw (Bytes.unsafe_of_string v)

	method write_list : 'b . 'b list -> ('b -> unit) -> unit = fun v f ->
		self#write_uleb128 (List.length v);
		List.iter f v

	method write_bool v =
		self#write_u8 (if v then 1 else 0)

	method write_bools v =
		let acc = ref 0 in
		List.iteri (fun shift b -> if b then acc := !acc lor (1 lsl shift)) v;
		self#write_u8 !acc

	method write_enum : 'b . 'b -> ('b -> int) -> unit = fun v f ->
		self#write_u8 (f v)

	method write_nullable : 'b . 'b option -> ('b -> unit) -> unit = fun v f -> match v with
		| None -> self#write_u8 0
		| Some v ->
			self#write_u8 1;
			f v

	method write_delta v =
		let delta = v - last_delta in
		last_delta <- v;
		self#write_leb128 delta

	(* Haxe, globals.ml and misc *)

	method write_pos v =
		(* TODO: header if (Option.get last_header).config_store_positions; then *)
			(* TODO: file present *)
			self#write_delta v.pmin;
			let file_present = (v.pfile = last_file) in
			(* TODO: delta? *)
			self#write_leb128 ((v.pmax lsl 1) lor (if file_present then 1 else 0));
			if file_present then
				last_file <- v.pfile;
				self#write_pstr last_file

	method write_path (t1, t2) =
		self#write_list t1 self#write_pstr;
		self#write_pstr t2

	method index_pool : 'b . 'b pool -> 'b -> int = fun pool v ->
		let hash, arr = pool in
		try
			Hashtbl.find hash v
		with Not_found ->
			let index = DynArray.length arr in
			Hashtbl.add hash v index;
			DynArray.add arr v;
			index

	method write_pstr v =
		self#write_uleb128 (self#index_pool string_pool v)

	method index_class_ref v =
		if v.cl_module = current_module then
			self#index_pool internal_classes_pool v
		else
			-1 - (self#index_pool external_classes_pool v.cl_path)

	method write_class_ref v =
		self#write_leb128 (self#index_class_ref v)

	method index_enum_ref v =
		if v.e_module = current_module then
			self#index_pool internal_enums_pool v
		else
			-1 - (self#index_pool external_enums_pool v.e_path)

	method write_enum_ref v =
		self#write_leb128 (self#index_enum_ref v)

	method write_abstract_ref v =
		self#write_leb128 (if v.a_module = current_module then
				self#index_pool internal_abstracts_pool v
			else
				-1 - (self#index_pool external_abstracts_pool v.a_path))

	method write_typedef_ref v =
		self#write_leb128 (if v.t_module = current_module then
				self#index_pool internal_typedefs_pool v
			else
				-1 - (self#index_pool external_typedefs_pool v.t_path))

	method write_class_field_ref t v =
		let t_index = self#index_class_ref t in
		let index = (if t_index < 0 then begin
			let t_index = -t_index - 1 in
			grow_pools external_class_field_pools t_index;
			-1 - (self#index_pool (DynArray.get external_class_field_pools t_index) v)
		end else begin
			grow_pools internal_class_field_pools t_index;
			self#index_pool (DynArray.get internal_class_field_pools t_index) v
		end) in
		self#write_leb128 index

	method write_enum_field_ref t v =
		let t_index = self#index_enum_ref t in
		let index = (if t_index < 0 then begin
			let t_index = -t_index - 1 in
			grow_pools external_enum_field_pools t_index;
			-1 - (self#index_pool (DynArray.get external_enum_field_pools t_index) v)
		end else begin
			grow_pools internal_enum_field_pools t_index;
			self#index_pool (DynArray.get internal_enum_field_pools t_index) v
		end) in
		self#write_leb128 index

	method write_forward_type t =
		self#write_str (snd t.mt_path);
		self#write_pos t.mt_pos;
		self#write_pos t.mt_name_pos;
		self#write_bool t.mt_private

	(* Haxe, ast.ml *)

	method write_binop v =
		self#write_enum v HxbEnums.Binop.to_int

	method write_unop v =
		self#write_enum v HxbEnums.Unop.to_int

	method write_constant = function
		| Int v ->
			self#write_u8 0;
			self#write_pstr v
		| Float v ->
			self#write_u8 1;
			self#write_pstr v
		| Ident v ->
			self#write_u8 2;
			self#write_pstr v
		| Regexp (v1, v2) ->
			self#write_u8 3;
			self#write_pstr v1;
			self#write_pstr v2
		| String (v, SDoubleQuotes) ->
			self#write_u8 4;
			self#write_pstr v
		| String (v, SSingleQuotes) ->
			self#write_u8 5;
			self#write_pstr v

	method write_type_path v =
		self#write_list v.tpackage self#write_pstr;
		self#write_pstr v.tname;
		self#write_list v.tparams self#write_type_param;
		self#write_nullable v.tsub self#write_pstr

	method write_placed_type_path (t1, t2) =
		self#write_type_path t1;
		self#write_pos t2

	method write_type_param = function
		| TPType v ->
			self#write_u8 0;
			self#write_type_hint v
		| TPExpr v ->
			self#write_u8 1;
			self#write_expr v

	method write_complex_type = function
		| CTPath v ->
			self#write_u8 0;
			self#write_type_path v
		| CTFunction (v1, v2) ->
			self#write_u8 1;
			self#write_list v1 self#write_type_hint;
			self#write_type_hint v2
		| CTAnonymous v ->
			self#write_u8 2;
			self#write_list v self#write_field
		| CTParent v ->
			self#write_u8 3;
			self#write_type_hint v
		| CTExtend (v1, v2) ->
			self#write_u8 4;
			self#write_list v1 self#write_placed_type_path;
			self#write_list v2 self#write_field
		| CTOptional v ->
			self#write_u8 5;
			self#write_type_hint v
		| CTNamed (v1, v2) ->
			self#write_u8 6;
			self#write_placed_name v1;
			self#write_type_hint v2
		| CTIntersection v ->
			self#write_u8 7;
			self#write_list v self#write_type_hint

	method write_type_hint (t1, t2) =
		self#write_complex_type t1;
		self#write_pos t2

	method write_function v =
		self#write_list v.f_params self#write_type_param_decl;
		self#write_list v.f_args self#write_function_arg;
		self#write_nullable v.f_type self#write_type_hint;
		self#write_nullable v.f_expr self#write_expr

	method write_function_arg (t1, t2, t3, t4, t5) =
		self#write_placed_name t1;
		self#write_bool t2;
		self#write_list t3 self#write_metadata_entry;
		self#write_nullable t4 self#write_type_hint;
		self#write_nullable t5 self#write_expr

	method write_placed_name (t1, t2) =
		self#write_pstr t1;
		self#write_pos t2

	method write_expr_def = function
		| EConst v ->
			self#write_u8 0;
			self#write_constant v
		| EArray (v1, v2) ->
			self#write_u8 1;
			self#write_expr v1;
			self#write_expr v2
		| EBinop (v1, v2, v3) ->
			self#write_u8 2;
			self#write_binop v1;
			self#write_expr v2;
			self#write_expr v3
		| EField (v1, v2) ->
			self#write_u8 3;
			self#write_expr v1;
			self#write_pstr v2
		| EParenthesis v ->
			self#write_u8 4;
			self#write_expr v
		| EObjectDecl v ->
			self#write_u8 5;
			self#write_list v self#write_object_field
		| EArrayDecl v ->
			self#write_u8 6;
			self#write_list v self#write_expr
		| ECall (v1, v2) ->
			self#write_u8 7;
			self#write_expr v1;
			self#write_list v2 self#write_expr
		| ENew (v1, v2) ->
			self#write_u8 8;
			self#write_placed_type_path v1;
			self#write_list v2 self#write_expr
		| EUnop (v1, v2, v3) ->
			self#write_u8 9;
			self#write_unop (v1, v2);
			self#write_expr v3
		| EVars v ->
			self#write_u8 10;
			self#write_list v self#write_var
		| EFunction (FKAnonymous, v) ->
			self#write_u8 11;
			self#write_function v
		| EFunction (FKNamed (v1, v2), v3) ->
			self#write_u8 12;
			self#write_placed_name v1;
			self#write_bool v2;
			self#write_function v3
		| EFunction (FKArrow, v) ->
			self#write_u8 13;
			self#write_function v
		| EBlock v ->
			self#write_u8 14;
			self#write_list v self#write_expr
		| EFor (v1, v2) ->
			self#write_u8 15;
			self#write_expr v1;
			self#write_expr v2
		| EIf (v1, v2, None) ->
			self#write_u8 16;
			self#write_expr v1;
			self#write_expr v2
		| EIf (v1, v2, Some v3) ->
			self#write_u8 17;
			self#write_expr v1;
			self#write_expr v2;
			self#write_expr v3
		| EWhile (v1, v2, NormalWhile) ->
			self#write_u8 18;
			self#write_expr v1;
			self#write_expr v2
		| EWhile (v1, v2, DoWhile) ->
			self#write_u8 19;
			self#write_expr v1;
			self#write_expr v2
		| ESwitch (v1, v2, None) ->
			self#write_u8 20;
			self#write_expr v1;
			self#write_list v2 self#write_case
		| ESwitch (v1, v2, Some (None, v3)) ->
			self#write_u8 21;
			self#write_expr v1;
			self#write_list v2 self#write_case;
			self#write_pos v3
		| ESwitch (v1, v2, Some (Some v3, _)) ->
			self#write_u8 22;
			self#write_expr v1;
			self#write_list v2 self#write_case;
			self#write_expr v3
		| ETry (v1, [v2]) ->
			self#write_u8 23;
			self#write_expr v1;
			self#write_catch v2
		| ETry (v1, v2) ->
			self#write_u8 24;
			self#write_expr v1;
			self#write_list v2 self#write_catch
		| EReturn None -> self#write_u8 25
		| EReturn (Some v) ->
			self#write_u8 26;
			self#write_expr v
		| EBreak -> self#write_u8 27
		| EContinue -> self#write_u8 28
		| EUntyped v ->
			self#write_u8 29;
			self#write_expr v
		| EThrow v ->
			self#write_u8 30;
			self#write_expr v
		| ECast (v, None) ->
			self#write_u8 31;
			self#write_expr v
		| ECast (v1, Some v2) ->
			self#write_u8 32;
			self#write_expr v1;
			self#write_type_hint v2
		| EDisplay (v, DKCall) ->
			self#write_u8 33;
			self#write_expr v
		| EDisplay (v, DKDot) ->
			self#write_u8 34;
			self#write_expr v
		| EDisplay (v, DKStructure) ->
			self#write_u8 35;
			self#write_expr v
		| EDisplay (v, DKMarked) ->
			self#write_u8 36;
			self#write_expr v
		| EDisplay (v, DKPattern false) ->
			self#write_u8 37;
			self#write_expr v
		| EDisplay (v, DKPattern true) ->
			self#write_u8 38;
			self#write_expr v
		| EDisplayNew v ->
			self#write_u8 39;
			self#write_placed_type_path v
		| ETernary (v1, v2, v3) ->
			self#write_u8 40;
			self#write_expr v1;
			self#write_expr v2;
			self#write_expr v3
		| ECheckType (v1, v2) ->
			self#write_u8 41;
			self#write_expr v1;
			self#write_type_hint v2
		| EMeta (v1, v2) ->
			self#write_u8 42;
			self#write_metadata_entry v1;
			self#write_expr v2

	method write_object_field ((t1, t2, t3), t4) =
		self#write_pstr t1;
		self#write_pos t2;
		self#write_bool (t3 = DoubleQuotes);
		self#write_expr t4

	method write_var (t1, t2, t3, t4) =
		self#write_placed_name t1;
		self#write_bool t2;
		self#write_nullable t3 self#write_type_hint;
		self#write_nullable t4 self#write_expr

	method write_case (t1, t2, t3, t4) =
		self#write_list t1 self#write_expr;
		self#write_nullable t2 self#write_expr;
		self#write_nullable t3 self#write_expr;
		self#write_pos t4

	method write_catch (t1, t2, t3, t4) =
		self#write_placed_name t1;
		self#write_type_hint t2;
		self#write_expr t3;
		self#write_pos t4

	method write_expr (t1, t2) =
		self#write_expr_def t1;
		self#write_pos t2

	method write_type_param_decl v =
		self#write_placed_name v.tp_name;
		self#write_list v.tp_params self#write_type_param_decl;
		self#write_nullable v.tp_constraints self#write_type_hint;
		self#write_list v.tp_meta self#write_metadata_entry

	method write_doc = function
		| None -> self#write_u8 0
		| Some v -> self#write_uleb128 (1 + (self#index_pool doc_pool v))

	method write_metadata_entry (t1, t2, t3) =
		self#write_pstr (Meta.to_string t1);
		self#write_list t2 self#write_expr;
		self#write_pos t3

	method write_placed_access (t1, t2) =
		self#write_enum t1 HxbEnums.Access.to_int;
		self#write_pos t2

	method write_field v =
		self#write_placed_name v.cff_name;
		self#write_doc v.cff_doc;
		self#write_pos v.cff_pos;
		self#write_list v.cff_meta self#write_metadata_entry;
		self#write_list v.cff_access self#write_placed_access;
		match v.cff_kind with
			| FVar (v1, v2) ->
				self#write_u8 0;
				self#write_nullable v1 self#write_type_hint;
				self#write_nullable v2 self#write_expr
			| FFun v ->
				self#write_u8 1;
				self#write_function v
			| FProp ((acc1, pos1), (acc2, pos2), t, e) ->
				self#write_u8 (match (acc1, acc2) with
					| "get", "get" -> 2
					| "get", "set" -> 3
					| "get", "null" -> 4
					| "get", "default" -> 5
					| "get", "never" -> 6
					| "set", "get" -> 7
					| "set", "set" -> 8
					| "set", "null" -> 9
					| "set", "default" -> 10
					| "set", "never" -> 11
					| "null", "get" -> 12
					| "null", "set" -> 13
					| "null", "null" -> 14
					| "null", "default" -> 15
					| "null", "never" -> 16
					| "default", "get" -> 17
					| "default", "set" -> 18
					| "default", "null" -> 19
					| "default", "default" -> 20
					| "default", "never" -> 21
					| "never", "get" -> 22
					| "never", "set" -> 23
					| "never", "null" -> 24
					| "never", "default" -> 25
					| "never", "never" -> 26
					| _ -> raise (HxbWriteFailure "write_field"));
				self#write_pos pos1;
				self#write_pos pos2;
				self#write_nullable t self#write_type_hint;
				self#write_nullable e self#write_expr

	(* Haxe, type.ml *)

	method write_type = function
		| TLazy t -> self#write_type (lazy_type t)
		| TMono {contents = None} -> self#write_u8 0
		| TMono {contents = Some v} ->
			self#write_u8 1;
			self#write_type v
		| TEnum (v, []) ->
			self#write_u8 2;
			self#write_enum_ref v
		| TEnum (v1, v2) ->
			self#write_u8 3;
			self#write_enum_ref v1;
			self#write_list v2 self#write_type
		| TInst (v, []) ->
			self#write_u8 4;
			self#write_class_ref v
		| TInst (v1, v2) ->
			self#write_u8 5;
			self#write_class_ref v1;
			self#write_list v2 self#write_type
		| TType (v, []) ->
			self#write_u8 6;
			self#write_typedef_ref v
		| TType (v1, v2) ->
			self#write_u8 7;
			self#write_typedef_ref v1;
			self#write_list v2 self#write_type
		| TFun (v1, v2) ->
			self#write_u8 8;
			self#write_list v1 self#write_tfun_arg;
			self#write_type v2
		| TAnon v ->
			self#write_u8 9;
			self#write_anon_type v
		| t when t == t_dynamic -> self#write_u8 10
		| TDynamic v ->
			self#write_u8 11;
			self#write_type v
		| TAbstract (v, []) ->
			self#write_u8 12;
			self#write_abstract_ref v
		| TAbstract (v1, v2) ->
			self#write_u8 13;
			self#write_abstract_ref v1;
			self#write_list v2 self#write_type

	method write_tfun_arg (t1, t2, t3) =
		self#write_pstr t1;
		self#write_bool t2;
		self#write_type t3

	method write_type_params v =
		self#write_list v (fun (t1, t2) ->
			self#write_pstr t1;
			self#write_type t2)

	method write_tconstant = function
		| TInt v ->
			self#write_u8 0;
			self#write_leb128 (Int32.to_int v) (* TODO *)
		| TFloat v ->
			self#write_u8 1;
			self#write_pstr v
		| TString v ->
			self#write_u8 2;
			self#write_pstr v
		| TBool false -> self#write_u8 3
		| TBool true -> self#write_u8 4
		| TNull -> self#write_u8 5
		| TThis -> self#write_u8 6
		| TSuper -> self#write_u8 7

	method write_tvar_extra (t1, t2) =
		self#write_type_params t1;
		self#write_nullable t2 self#write_typed_expr

	method write_tvar v =
		self#write_leb128 v.v_id;
		self#write_pstr v.v_name;
		self#write_type v.v_type;
		self#write_enum v.v_kind HxbEnums.TVarKind.to_int;
		self#write_bools [v.v_capture; v.v_final];
		self#write_nullable v.v_extra self#write_tvar_extra;
		self#write_list v.v_meta self#write_metadata_entry;
		self#write_pos v.v_pos

	method write_tfunc v =
		self#write_list v.tf_args self#write_tfunc_arg;
		self#write_type v.tf_type;
		self#write_typed_expr v.tf_expr

	method write_tfunc_arg (t1, t2) =
		self#write_tvar t1;
		self#write_nullable t2 self#write_typed_expr

	method write_anon_type v =
		(* TODO: v.a_fields *)
		match !(v.a_status) with
			| Closed -> self#write_u8 0
			| Opened -> self#write_u8 1
			| Const -> self#write_u8 2
			| Extend v ->
				self#write_u8 3;
				self#write_list v self#write_type
			| Statics v ->
				self#write_u8 4;
				self#write_class_ref v
			| EnumStatics v ->
				self#write_u8 5;
				self#write_enum_ref v
			| AbstractStatics v ->
				self#write_u8 6;
				self#write_abstract_ref v

	method write_typed_expr_def = function
		| TConst v ->
			self#write_u8 0;
			self#write_tconstant v
		| TLocal v -> raise (Failure "todo")
		| TArray (v1, v2) ->
			self#write_u8 2;
			self#write_typed_expr v1;
			self#write_typed_expr v2
		| TBinop (v1, v2, v3) ->
			self#write_u8 3;
			self#write_binop v1;
			self#write_typed_expr v2;
			self#write_typed_expr v3
		| TField (v1, FInstance (v2, v3, v4)) ->
			self#write_u8 4;
			self#write_typed_expr v1;
			self#write_class_ref v2;
			self#write_list v3 self#write_type;
			self#write_class_field_ref v2 v4
		| TField (v1, FStatic (v2, v3)) ->
			self#write_u8 5;
			self#write_typed_expr v1;
			self#write_class_ref v2;
			self#write_class_field_ref v2 v3
		| TField (v1, FAnon (_)) -> raise (Failure "todo")
		| TField (v1, FDynamic v2) ->
			self#write_u8 7;
			self#write_typed_expr v1;
			self#write_pstr v2
		| TField (v1, FClosure (None, _)) -> raise (Failure "todo")
		| TField (v1, FClosure (Some (v2, v3), v4)) ->
			self#write_u8 9;
			self#write_typed_expr v1;
			self#write_class_ref v2;
			self#write_list v3 self#write_type;
			self#write_class_field_ref v2 v4
		| TField (v1, FEnum (v2, v3)) ->
			self#write_u8 10;
			self#write_typed_expr v1;
			self#write_enum_ref v2;
			self#write_enum_field_ref v2 v3
		| TTypeExpr (TClassDecl v) ->
			self#write_u8 11;
			self#write_class_ref v
		| TTypeExpr (TEnumDecl v) ->
			self#write_u8 12;
			self#write_enum_ref v
		| TTypeExpr (TTypeDecl v) ->
			self#write_u8 13;
			self#write_typedef_ref v
		| TTypeExpr (TAbstractDecl v) ->
			self#write_u8 14;
			self#write_abstract_ref v
		| TParenthesis v ->
			self#write_u8 15;
			self#write_typed_expr v
		| TObjectDecl v ->
			self#write_u8 16;
			self#write_list v self#write_tobject_field
		| TArrayDecl v ->
			self#write_u8 17;
			self#write_list v self#write_typed_expr
		| TCall (v1, v2) ->
			self#write_u8 18;
			self#write_typed_expr v1;
			self#write_list v2 self#write_typed_expr
		| TNew (v1, v2, v3) ->
			self#write_u8 19;
			self#write_class_ref v1;
			self#write_list v2 self#write_type;
			self#write_list v3 self#write_typed_expr
		| TUnop (op, flag, v) ->
			self#write_u8 20;
			self#write_unop (op, flag);
			self#write_typed_expr v
		| TFunction v ->
			self#write_u8 21;
			self#write_tfunc v
		| TVar (v, None) ->
			self#write_u8 22;
			self#write_tvar v
		| TVar (v1, Some v2) ->
			self#write_u8 23;
			self#write_tvar v1;
			self#write_typed_expr v2
		| TBlock v ->
			self#write_u8 24;
			self#write_list v self#write_typed_expr
		| TFor (v1, v2, v3) ->
			self#write_u8 25;
			self#write_tvar v1;
			self#write_typed_expr v2;
			self#write_typed_expr v3
		| TIf (v1, v2, None) ->
			self#write_u8 26;
			self#write_typed_expr v1;
			self#write_typed_expr v2
		| TIf (v1, v2, Some v3) ->
			self#write_u8 27;
			self#write_typed_expr v1;
			self#write_typed_expr v2;
			self#write_typed_expr v3
		| TWhile (v1, v2, NormalWhile) ->
			self#write_u8 28;
			self#write_typed_expr v1;
			self#write_typed_expr v2
		| TWhile (v1, v2, DoWhile) ->
			self#write_u8 29;
			self#write_typed_expr v1;
			self#write_typed_expr v2
		| TSwitch (v1, v2, None) ->
			self#write_u8 30;
			self#write_typed_expr v1;
			self#write_list v2 self#write_tcase
		| TSwitch (v1, v2, Some v3) ->
			self#write_u8 31;
			self#write_typed_expr v1;
			self#write_list v2 self#write_tcase;
			self#write_typed_expr v3
		| TTry (v1, [v2]) ->
			self#write_u8 32;
			self#write_typed_expr v1;
			self#write_tcatch v2
		| TTry (v1, v2) ->
			self#write_u8 33;
			self#write_typed_expr v1;
			self#write_list v2 self#write_tcatch
		| TReturn None -> self#write_u8 34
		| TReturn (Some v) ->
			self#write_u8 35;
			self#write_typed_expr v
		| TBreak -> self#write_u8 36
		| TContinue -> self#write_u8 37
		| TThrow v ->
			self#write_u8 38;
			self#write_typed_expr v
		| TCast (v, None) ->
			self#write_u8 39;
			self#write_typed_expr v
		| TCast (v1, Some (TClassDecl v2)) ->
			self#write_u8 40;
			self#write_typed_expr v1;
			self#write_class_ref v2
		| TCast (v1, Some (TEnumDecl v2)) ->
			self#write_u8 41;
			self#write_typed_expr v1;
			self#write_enum_ref v2
		| TCast (v1, Some (TTypeDecl v2)) ->
			self#write_u8 42;
			self#write_typed_expr v1;
			self#write_typedef_ref v2
		| TCast (v1, Some (TAbstractDecl v2)) ->
			self#write_u8 43;
			self#write_typed_expr v1;
			self#write_abstract_ref v2
		| TMeta (v1, v2) ->
			self#write_u8 44;
			self#write_metadata_entry v1;
			self#write_typed_expr v2
		| TEnumParameter (v1, v2, v3) ->
			let t = (match follow v1.etype with TEnum (t, _) -> t | _ -> assert false) in
			self#write_u8 45;
			self#write_typed_expr v1;
			self#write_enum_field_ref t v2;
			self#write_uleb128 v3
		| TEnumIndex v ->
			self#write_u8 46;
			self#write_typed_expr v
		| TIdent v ->
			self#write_u8 47;
			self#write_pstr v

	method write_tobject_field ((t1, t2, t3), t4) =
		self#write_pstr t1;
		self#write_pos t2;
		self#write_bool (t3 = DoubleQuotes);
		self#write_typed_expr t4

	method write_tcase (t1, t2) =
		self#write_list t1 self#write_typed_expr;
		self#write_typed_expr t2

	method write_tcatch (t1, t2) =
		self#write_tvar t1;
		self#write_typed_expr t2

	method write_typed_expr v =
		self#write_typed_expr_def v.eexpr;
		self#write_type v.etype;
		self#write_pos v.epos

	method write_base_type v =
		self#write_doc v.mt_doc;
		self#write_list v.mt_meta self#write_metadata_entry;
		self#write_type_params v.mt_params;
		self#write_list v.mt_using self#write_class_using

	method write_class_using (t1, t2) =
		self#write_class_ref t1;
		self#write_pos t2

	method write_class_field t v =
		self#write_pstr v.cf_name;
		self#write_type v.cf_type;
		self#write_pos v.cf_pos;
		self#write_pos v.cf_name_pos;
		self#write_doc v.cf_doc;
		self#write_list v.cf_meta self#write_metadata_entry;
		(match v.cf_kind with
			| Method MethNormal -> self#write_u8 0
			| Method MethInline -> self#write_u8 1
			| Method MethDynamic -> self#write_u8 2
			| Method MethMacro -> self#write_u8 3
			| Var {v_read; v_write} ->
				let sub_access = (function
					| AccNormal -> 0, None
					| AccNo -> 1, None
					| AccNever -> 2, None
					| AccCtor -> 3, None
					| AccResolve -> 4, None
					| AccCall -> 5, None
					| AccInline -> 6, None
					| AccRequire (v1, v2) -> 7, Some (v1, v2)) in
				let r, rs = sub_access v_read in
				let w, ws = sub_access v_write in
				self#write_u8 (10 + r * 15 + w);
				let sub_access = (function
					| None -> ()
					| Some (v1, v2) ->
						self#write_pstr v1;
						self#write_nullable v2 self#write_pstr) in
				sub_access rs;
				sub_access ws);
		self#write_type_params v.cf_params;
		self#write_nullable v.cf_expr self#write_typed_expr;
		self#write_nullable v.cf_expr_unoptimized self#write_tfunc;
		self#write_list v.cf_overloads (self#write_class_field_ref t);
		self#write_bools [
			has_class_field_flag v CfPublic;
			has_class_field_flag v CfExtern;
			has_class_field_flag v CfFinal;
			has_class_field_flag v CfOverridden;
			has_class_field_flag v CfModifiesThis
		]

	method write_class_type v =
		self#write_base_type (t_infos (TClassDecl v));
		(match v.cl_kind with
			| KNormal -> self#write_u8 0
			| KTypeParameter v ->
				self#write_u8 1;
				self#write_list v self#write_type
			| KExpr v ->
				self#write_u8 2;
				self#write_expr v
			| KGeneric -> self#write_u8 3
			| KGenericInstance (v1, v2) ->
				self#write_u8 4;
				self#write_param_class_type (v1, v2)
			| KMacroType -> self#write_u8 5
			| KGenericBuild v ->
				self#write_u8 6;
				self#write_list v self#write_field
			| KAbstractImpl v ->
				self#write_u8 7;
				self#write_abstract_ref v);
		self#write_bools [
			v.cl_extern;
			v.cl_final;
			v.cl_interface
		];
		self#write_nullable v.cl_super self#write_param_class_type;
		self#write_list v.cl_implements self#write_param_class_type;
		self#write_list v.cl_ordered_fields (self#write_class_field v);
		self#write_list v.cl_ordered_statics (self#write_class_field v);
		self#write_nullable v.cl_dynamic self#write_type;
		self#write_nullable v.cl_array_access self#write_type;
		self#write_nullable v.cl_constructor (self#write_class_field_ref v);
		self#write_nullable v.cl_init self#write_typed_expr;
		self#write_list v.cl_overrides (self#write_class_field_ref v)

	method write_param_class_type (t1, t2) =
		self#write_class_ref t1;
		self#write_list t2 self#write_type

	method write_enum_field v =
		self#write_pstr v.ef_name;
		self#write_type v.ef_type;
		self#write_pos v.ef_pos;
		self#write_pos v.ef_name_pos;
		self#write_doc v.ef_doc;
		self#write_type_params v.ef_params;
		self#write_list v.ef_meta self#write_metadata_entry

	method write_enum_type v =
		self#write_base_type (t_infos (TEnumDecl v));
		self#write_def_type v.e_type;
		self#write_bool v.e_extern;
		List.iter (fun name -> self#write_enum_field (PMap.find name v.e_constrs)) v.e_names

	method write_abstract_type v =
		self#write_base_type (t_infos (TAbstractDecl v));
		self#write_list v.a_ops (self#write_abstract_binop v);
		self#write_list v.a_unops (self#write_abstract_unop v);
		self#write_nullable v.a_impl self#write_class_ref;
		self#write_type v.a_this;
		self#write_list v.a_from self#write_type;
		self#write_list v.a_from_field (self#write_abstract_from_to v);
		self#write_list v.a_to self#write_type;
		self#write_list v.a_to_field (self#write_abstract_from_to v);
		self#write_list v.a_array (fun f -> self#write_class_field_ref (Option.get v.a_impl) f);
		self#write_nullable v.a_read (fun f -> self#write_class_field_ref (Option.get v.a_impl) f);
		self#write_nullable v.a_write (fun f -> self#write_class_field_ref (Option.get v.a_impl) f);

	method write_abstract_binop v (t1, t2) =
		self#write_binop t1;
		self#write_class_field_ref (Option.get v.a_impl) t2

	method write_abstract_unop v (op, flag, t) =
		self#write_unop (op, flag);
		self#write_class_field_ref (Option.get v.a_impl) t

	method write_abstract_from_to v (t1, t2) =
		self#write_type t1;
		self#write_class_field_ref (Option.get v.a_impl) t2

	method write_def_type v =
		self#write_base_type (t_infos (TTypeDecl v));
		self#write_type v.t_type

	(* File structure *)

	method write_header () =
		self#write_bool true; (* TODO: config_store_positions *)
		self#write_path current_module.m_path

	method write_string_pool () =
		DynArray.iter self#write_str (snd string_pool)

	method write_doc_pool () =
		(* TODO: config_store_docs *)
		DynArray.iter self#write_str (snd doc_pool)

	method write_type_list () =
		DynArray.iter self#write_path (snd external_classes_pool);
		DynArray.iter self#write_path (snd external_enums_pool);
		DynArray.iter self#write_path (snd external_abstracts_pool);
		DynArray.iter self#write_path (snd external_typedefs_pool);
		DynArray.iter (fun t -> self#write_forward_type (t_infos (TClassDecl t))) (snd internal_classes_pool);
		DynArray.iter (fun t -> self#write_forward_type (t_infos (TEnumDecl t))) (snd internal_enums_pool);
		DynArray.iter (fun t -> self#write_forward_type (t_infos (TAbstractDecl t))) (snd internal_abstracts_pool);
		DynArray.iter (fun t -> self#write_forward_type (t_infos (TTypeDecl t))) (snd internal_typedefs_pool)

	method write_field_list () =
		let write_enum_fields pool = DynArray.iter (fun f -> self#write_str f.ef_name) (snd pool) in
		let write_class_fields pool = DynArray.iter (fun f -> self#write_str f.cf_name) (snd pool) in
		DynArray.iter write_class_fields external_class_field_pools;
		DynArray.iter write_class_fields internal_class_field_pools;
		DynArray.iter write_enum_fields external_enum_field_pools;
		DynArray.iter write_enum_fields internal_enum_field_pools

	method read_module_extra () =
		(* TODO *)
		()

	method write_type_declarations () =
		List.iter (fun t -> match t with TClassDecl t -> self#write_class_type t | _ -> ()) current_module.m_types;
		List.iter (fun t -> match t with TEnumDecl t -> self#write_enum_type t | _ -> ()) current_module.m_types;
		List.iter (fun t -> match t with TAbstractDecl t -> self#write_abstract_type t | _ -> ()) current_module.m_types;
		List.iter (fun t -> match t with TTypeDecl t -> self#write_def_type t | _ -> ()) current_module.m_types

	method create_chunk chk_id f =
		last_file <- "";
		last_delta <- 0;
		ch <- IO.output_bytes ();
		f ();
		let chk_data = IO.close_out ch in
		let chk_size = Bytes.length chk_data in
		let chk_checksum = Int32.of_int 0 in (* TODO *)
		{chk_id; chk_size; chk_data; chk_checksum}

	method write_chunk chk =
		IO.write_real_i32 file_ch (Int32.of_int chk.chk_size);
		IO.nwrite file_ch (Bytes.unsafe_of_string chk.chk_id);
		IO.nwrite file_ch chk.chk_data;
		IO.write_real_i32 file_ch chk.chk_checksum

	method write_hxb () =
		let type_declarations = self#create_chunk "TYPE" self#write_type_declarations in
		let field_list = self#create_chunk "FLDL" self#write_field_list in
		let type_list = self#create_chunk "TYPL" self#write_type_list in
		let doc_pool = self#create_chunk "dOCS" self#write_doc_pool in
		let string_pool = self#create_chunk "STRI" self#write_string_pool in
		self#write_chunk (self#create_chunk "HHDR" self#write_header);
		self#write_chunk string_pool;
		self#write_chunk doc_pool;
		self#write_chunk type_list;
		self#write_chunk field_list;
		self#write_chunk type_declarations;
		self#write_chunk (self#create_chunk "HEND" (fun () -> ()))

end
