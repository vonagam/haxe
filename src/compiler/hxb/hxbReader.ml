open Globals
open Ast
open Type

open HxbChunks
open HxbEnums

exception HxbReadFailure of string

class hxb_reader
	(ch : IO.input)
	(resolve_type : path -> module_type)
	= object(self)

	val mutable last_header : hxb_chunk_header option = None
	val mutable last_string_pool : hxb_chunk_string_pool option = None
	val mutable last_doc_pool : hxb_chunk_doc_pool option = None
	val mutable last_type_list : hxb_chunk_type_list option = None
	val mutable last_field_list : hxb_chunk_field_list option = None
	val mutable last_type_declarations : hxb_chunk_type_declarations option = None
	val mutable last_module_extra : hxb_chunk_module_extra option = None

	val mutable last_file = ""
	val mutable last_delta = 0

	val mutable current_module : module_def option = None

	(* Primitives *)

	method read_u8 () =
		IO.read_byte ch

	method read_u32 () =
		IO.read_real_i32 ch

	method read_f64 () =
		IO.read_double ch

	method read_uleb128 () =
		let b = self#read_u8 () in
		if b >= 0x80 then
			(b land 0x7F) lor ((self#read_uleb128 ()) lsl 7)
		else
			b

	method read_leb128 () =
		let rec read acc shift =
			let b = self#read_u8 () in
			let cont = b >= 0x80 in
			let acc = ((b land 0x7F) lsl shift) lor acc in
			if cont then
				read acc (shift + 7)
			else
				(b, acc, shift + 7)
		in
		let last, acc, shift = read 0 0 in
		if (last land 0x40) <> 0 then
			acc lor ((lnot 0) lsl shift)
		else
			acc

	method read_str_raw len =
		IO.really_nread ch len

	method read_str () =
		Bytes.unsafe_to_string (self#read_str_raw (self#read_uleb128 ()))

	method read_list : 'b . (unit -> 'b) -> 'b list = fun f ->
		let rec read n =
			if n = 0 then
				[]
			else
				(f ()) :: (read (n - 1))
		in
		List.rev (read (self#read_uleb128 ()))

	method read_arr : 'b . (unit -> 'b) -> 'b array = fun f ->
		Array.init (self#read_uleb128 ()) (fun _ -> f ())

	method read_bool () =
		(self#read_u8 ()) <> 0

	method read_bools n =
		let b = self#read_u8 () in
		let rec read i =
			if i = n then
				[]
			else
				((b land (1 lsl i)) <> 0) :: (read (i + 1))
		in
		read 0

	method read_bools2 () = match self#read_bools 2 with
		| [a; b] -> a, b
		| _ -> assert false
	method read_bools3 () = match self#read_bools 3 with
		| [a; b; c] -> a, b, c
		| _ -> assert false
	method read_bools4 () = match self#read_bools 4 with
		| [a; b; c; d] -> a, b, c, d
		| _ -> assert false
	method read_bools5 () = match self#read_bools 5 with
		| [a; b; c; d; e] -> a, b, c, d, e
		| _ -> assert false

	method read_enum : 'b . (int -> 'b) -> 'b = fun f ->
		f (self#read_u8 ())

	method read_nullable : 'b . (unit -> 'b) -> 'b option = fun f ->
		if (self#read_u8 ()) <> 0 then
			Some (f ())
		else
			None

	method read_delta () =
		let v = self#read_leb128 () in
		last_delta <- last_delta + v;
		last_delta

	(* Haxe, globals.ml and misc *)

	method read_pos () =
		if not (Option.get last_header).config_store_positions; then
			null_pos
		else
			let pmin = self#read_delta () in
			let pmax_flag = self#read_leb128 () in
			let pmax, file_present = pmax_flag lsr 1, (pmax_flag land 1) <> 0 in
			if file_present then
				last_file <- self#read_pstr ();
			{pfile = last_file; pmin; pmax}

	method read_path () =
		let t1 = self#read_list self#read_pstr in
		let t2 = self#read_pstr () in
		t1, t2

	method read_pstr () =
		(Option.get last_string_pool).data.(self#read_uleb128 ())

	(* TODO: refactor the read_*_ref functions into one + 4 tiny ones? *)
	method read_class_ref () =
		let type_list = (Option.get last_type_list) in
		let index = self#read_uleb128 () in
		if index < (Array.length type_list.external_classes) then
			match (resolve_type type_list.external_classes.(index)) with
				| TClassDecl t -> t
				| _ -> raise (HxbReadFailure "expected class type")
		else
			raise (Failure "n/a")

	method read_enum_ref () =
		let type_list = (Option.get last_type_list) in
		let index = self#read_uleb128 () in
		if index < (Array.length type_list.external_enums) then
			match (resolve_type type_list.external_enums.(index)) with
				| TEnumDecl t -> t
				| _ -> raise (HxbReadFailure "expected class type")
		else
			raise (Failure "n/a")

	method read_abstract_ref () =
		let type_list = (Option.get last_type_list) in
		let index = self#read_uleb128 () in
		if index < (Array.length type_list.external_abstracts) then
			match (resolve_type type_list.external_abstracts.(index)) with
				| TAbstractDecl t -> t
				| _ -> raise (HxbReadFailure "expected class type")
		else
			raise (Failure "n/a")

	method read_typedef_ref () =
		let type_list = (Option.get last_type_list) in
		let index = self#read_uleb128 () in
		if index < (Array.length type_list.external_typedefs) then
			match (resolve_type type_list.external_typedefs.(index)) with
				| TTypeDecl t -> t
				| _ -> raise (HxbReadFailure "expected class type")
		else
			raise (Failure "n/a")

	(* TODO: field refs *)

	(* Haxe, ast.ml *)

	method read_binop () =
		self#read_enum HxbEnums.Binop.from_int

	method read_unop () =
		self#read_enum HxbEnums.Unop.from_int

	method read_constant () = self#read_enum (function
		| 0 -> Int (self#read_pstr ())
		| 1 -> Float (self#read_pstr ())
		| 2 -> Ident (self#read_pstr ())
		| 3 ->
			let t1 = self#read_pstr () in 
			Regexp (t1, self#read_pstr ())
		| 4 -> String (self#read_pstr (), SDoubleQuotes)
		| 5 -> String (self#read_pstr (), SSingleQuotes)
		| _ -> raise (HxbReadFailure "read_constant"))

	method read_type_path () =
		let tpackage = self#read_list self#read_pstr in
		let tname = self#read_pstr () in
		let tparams = self#read_list self#read_type_param in
		let tsub = self#read_nullable self#read_pstr in
		{tpackage; tname; tparams; tsub}

	method read_placed_type_path () =
		let t1 = self#read_type_path () in
		t1, (self#read_pos ())

	method read_type_param () = self#read_enum (function
		| 0 -> TPType (self#read_type_hint ())
		| 1 -> TPExpr (self#read_expr ())
		| _ -> raise (HxbReadFailure "read_type_param"))

	method read_complex_type () = self#read_enum (function
		| 0 -> CTPath (self#read_type_path ())
		| 1 ->
			let t1 = self#read_list self#read_type_hint in
			CTFunction (t1, self#read_type_hint ())
		| 2 -> CTAnonymous (self#read_list self#read_field)
		| 3 -> CTParent (self#read_type_hint ())
		| 4 ->
			let t1 = self#read_list self#read_placed_type_path in
			CTExtend (t1, self#read_list self#read_field)
		| 5 -> CTOptional (self#read_type_hint ())
		| 6 ->
			let t1 = self#read_placed_name () in
			CTNamed (t1, self#read_type_hint ())
		| 7 -> CTIntersection (self#read_list self#read_type_hint)
		| _ -> raise (HxbReadFailure "read_complex_type"))

	method read_type_hint () =
		let t1 = self#read_complex_type () in
		t1, (self#read_pos ())

	method read_function () =
		let f_params = self#read_list self#read_type_param_decl in
		let f_args = self#read_list self#read_function_arg in
		let f_type = self#read_nullable self#read_type_hint in
		let f_expr = self#read_nullable self#read_expr in
		{f_params; f_args; f_type; f_expr}

	method read_function_arg () =
		let t1 = self#read_placed_name () in
		let t2 = self#read_bool () in
		let t3 = self#read_list self#read_metadata_entry in
		let t4 = self#read_nullable self#read_type_hint in
		t1, t2, t3, t4, (self#read_nullable self#read_expr)

	method read_placed_name () =
		let t1 = self#read_pstr () in
		t1, (self#read_pos ())

	method read_expr_def () = self#read_enum (function
		| 0 -> EConst (self#read_constant ())
		| 1 ->
			let t1 = self#read_expr () in
			EArray (t1, self#read_expr ())
		| 2 ->
			let t1 = self#read_binop () in
			let t2 = self#read_expr () in
			EBinop (t1, t2, self#read_expr ())
		| 3 ->
			let t1 = self#read_expr () in
			EField (t1, self#read_pstr ())
		| 4 -> EParenthesis (self#read_expr ())
		| 5 -> EObjectDecl (self#read_list self#read_object_field)
		| 6 -> EArrayDecl (self#read_list self#read_expr)
		| 7 ->
			let t1 = self#read_expr () in
			ECall (t1, self#read_list self#read_expr)
		| 8 ->
			let t1 = self#read_placed_type_path () in
			ENew (t1, self#read_list self#read_expr)
		| 9 ->
			let op, flag = self#read_unop () in
			EUnop (op, flag, self#read_expr ())
		| 10 -> EVars (self#read_list self#read_var)
		| 11 -> EFunction (FKAnonymous, self#read_function ())
		| 12 ->
			let t1 = self#read_placed_name () in
			let t2 = self#read_bool () in
			EFunction (FKNamed (t1, t2), self#read_function ())
		| 13 -> EFunction (FKArrow, self#read_function ())
		| 14 -> EBlock (self#read_list self#read_expr)
		| 15 ->
			let t1 = self#read_expr () in
			EFor (t1, self#read_expr ())
		| 16 ->
			let t1 = self#read_expr () in
			EIf (t1, self#read_expr (), None)
		| 17 ->
			let t1 = self#read_expr () in
			let t2 = self#read_expr () in
			EIf (t1, t2, Some (self#read_expr ()))
		| 18 ->
			let t1 = self#read_expr () in
			EWhile (t1, self#read_expr (), NormalWhile)
		| 19 ->
			let t1 = self#read_expr () in
			EWhile (t1, self#read_expr (), DoWhile)
		| 20 ->
			let t1 = self#read_expr () in
			ESwitch (t1, self#read_list self#read_case, None)
		| 21 ->
			let t1 = self#read_expr () in
			let t2 = self#read_list self#read_case in
			ESwitch (t1, t2, Some (None, self#read_pos ()))
		| 22 ->
			let e = self#read_expr () in
			let cases = self#read_list self#read_case in
			let edef = self#read_expr () in
			ESwitch (e, cases, Some ((Some edef), snd edef))
		| 23 ->
			let t1 = self#read_expr () in
			ETry (t1, [self#read_catch ()])
		| 24 ->
			let t1 = self#read_expr () in
			ETry (t1, self#read_list self#read_catch)
		| 25 -> EReturn None
		| 26 -> EReturn (Some (self#read_expr ()))
		| 27 -> EBreak
		| 28 -> EContinue
		| 29 -> EUntyped (self#read_expr ())
		| 30 -> EThrow (self#read_expr ())
		| 31 -> ECast (self#read_expr (), None)
		| 32 ->
			let t1 = self#read_expr () in
			ECast (t1, Some (self#read_type_hint ()))
		| 33 -> EDisplay (self#read_expr (), DKCall)
		| 34 -> EDisplay (self#read_expr (), DKDot)
		| 35 -> EDisplay (self#read_expr (), DKStructure)
		| 36 -> EDisplay (self#read_expr (), DKMarked)
		| 37 -> EDisplay (self#read_expr (), DKPattern false)
		| 38 -> EDisplay (self#read_expr (), DKPattern true)
		| 39 -> EDisplayNew (self#read_placed_type_path ())
		| 40 ->
			let t1 = self#read_expr () in
			let t2 = self#read_expr () in
			ETernary (t1, t2, self#read_expr ())
		| 41 ->
			let t1 = self#read_expr () in
			ECheckType (t1, self#read_type_hint ())
		| 42 ->
			let t1 = self#read_metadata_entry () in
			EMeta (t1, self#read_expr ())
		| _ -> raise (HxbReadFailure "read_expr_def"))

	method read_object_field () =
		let t1 = self#read_pstr () in
		let t2 = self#read_pos () in
		let t3 = (if (self#read_bool ()) then DoubleQuotes else NoQuotes) in
		(t1, t2, t3), (self#read_expr ())

	method read_var () =
		let t1 = self#read_placed_name () in
		let t2 = self#read_bool () in
		let t3 = self#read_nullable self#read_type_hint in
		t1, t2, t3, (self#read_nullable self#read_expr)

	method read_case () =
		let t1 = self#read_list self#read_expr in
		let t2 = self#read_nullable self#read_expr in
		let t3 = self#read_nullable self#read_expr in
		t1, t2, t3, (self#read_pos ())

	method read_catch () =
		let t1 = self#read_placed_name () in
		let t2 = self#read_type_hint () in
		let t3 = self#read_expr () in
		t1, t2, t3, (self#read_pos ())

	method read_expr () =
		let t1 = self#read_expr_def () in
		t1, (self#read_pos ())

	method read_type_param_decl () =
		let tp_name = self#read_placed_name () in
		let tp_params = self#read_list self#read_type_param_decl in
		let tp_constraints = self#read_nullable self#read_type_hint in
		let tp_meta = self#read_list self#read_metadata_entry in
		{tp_name; tp_params; tp_constraints; tp_meta}

	method read_doc () =
		let index = self#read_uleb128 () in
		if (index = 0) || (last_doc_pool = None) then
			None
		else
			Some ((Option.get last_doc_pool).data.(index - 1))

	method read_metadata_entry () =
		let t1 = Meta.from_string (self#read_pstr ()) in
		let t2 = self#read_list self#read_expr in
		t1, t2, (self#read_pos ())

	method read_placed_access () =
		let t1 = self#read_enum HxbEnums.Access.from_int in
		t1, (self#read_pos ())

	method read_field () =
		let cff_name = self#read_placed_name () in
		let cff_doc = self#read_doc () in
		let cff_pos = self#read_pos () in
		let cff_meta = self#read_list self#read_metadata_entry in
		let cff_access = self#read_list self#read_placed_access in
		let cff_kind = self#read_enum (function
			| 0 ->
				let t1 = self#read_nullable self#read_type_hint in
				FVar (t1, self#read_nullable self#read_expr)
			| 1 -> FFun (self#read_function ())
			| v when (v >= 2) && (v <= 26) ->
				let pos1 = self#read_pos () in
				let pos2 = self#read_pos () in
				let t = self#read_nullable self#read_type_hint in
				let e = self#read_nullable self#read_expr in
				(match v with
					| 2 -> FProp (("get", pos1), ("get", pos2), t, e)
					| 3 -> FProp (("get", pos1), ("set", pos2), t, e)
					| 4 -> FProp (("get", pos1), ("null", pos2), t, e)
					| 5 -> FProp (("get", pos1), ("default", pos2), t, e)
					| 6 -> FProp (("get", pos1), ("never", pos2), t, e)
					| 7 -> FProp (("set", pos1), ("get", pos2), t, e)
					| 8 -> FProp (("set", pos1), ("set", pos2), t, e)
					| 9 -> FProp (("set", pos1), ("null", pos2), t, e)
					| 10 -> FProp (("set", pos1), ("default", pos2), t, e)
					| 11 -> FProp (("set", pos1), ("never", pos2), t, e)
					| 12 -> FProp (("null", pos1), ("get", pos2), t, e)
					| 13 -> FProp (("null", pos1), ("set", pos2), t, e)
					| 14 -> FProp (("null", pos1), ("null", pos2), t, e)
					| 15 -> FProp (("null", pos1), ("default", pos2), t, e)
					| 16 -> FProp (("null", pos1), ("never", pos2), t, e)
					| 17 -> FProp (("default", pos1), ("get", pos2), t, e)
					| 18 -> FProp (("default", pos1), ("set", pos2), t, e)
					| 19 -> FProp (("default", pos1), ("null", pos2), t, e)
					| 20 -> FProp (("default", pos1), ("default", pos2), t, e)
					| 21 -> FProp (("default", pos1), ("never", pos2), t, e)
					| 22 -> FProp (("never", pos1), ("get", pos2), t, e)
					| 23 -> FProp (("never", pos1), ("set", pos2), t, e)
					| 24 -> FProp (("never", pos1), ("null", pos2), t, e)
					| 25 -> FProp (("never", pos1), ("default", pos2), t, e)
					| 26 -> FProp (("never", pos1), ("never", pos2), t, e)
					| _ -> assert false)
			| _ -> raise (Invalid_argument "enum")) in
		{cff_name; cff_doc; cff_pos; cff_meta; cff_access; cff_kind}

	(* Haxe, type.ml *)

	method read_type () = self#read_enum (function
		| 0 -> TMono (ref None)
		| 1 -> TMono (ref (Some (self#read_type ())))

		| 10 -> t_dynamic
		| 11 -> TDynamic (self#read_type ())

		| _ -> raise (HxbReadFailure "read_type"))

	method read_tfun_arg () =
		let t1 = self#read_pstr () in
		let t2 = self#read_bool () in
		t1, t2, (self#read_type ())

	method read_type_params () =
		self#read_list (fun () ->
			let t1 = self#read_pstr () in
			t1, (self#read_type ()))

	method read_tconstant () = self#read_enum (function
		| 0 -> TInt (Int32.of_int (self#read_leb128 ())) (* TODO *)
		| 1 -> TFloat (self#read_pstr ())
		| 2 -> TString (self#read_pstr ())
		| 3 -> TBool false
		| 4 -> TBool true
		| 5 -> TNull
		| 6 -> TThis
		| 7 -> TSuper
		| _ -> raise (HxbReadFailure "read_tconstant"))

	method read_tvar_extra () =
		let t1 = self#read_type_params () in
		t1, (self#read_nullable self#read_typed_expr)

	method read_tvar () =
		let v_id = self#read_leb128 () in
		let v_name = self#read_pstr () in
		let v_type = self#read_type () in
		let v_kind = self#read_enum HxbEnums.TVarKind.from_int in
		let v_capture, v_final = self#read_bools2 () in
		let v_extra = self#read_nullable self#read_tvar_extra in
		let v_meta = self#read_list self#read_metadata_entry in
		let v_pos = self#read_pos () in
		{v_id; v_name; v_type; v_kind; v_capture; v_final; v_extra; v_meta; v_pos}

	method read_tfunc () =
		let tf_args = self#read_list self#read_tfunc_arg in
		let tf_type = self#read_type () in
		let tf_expr = self#read_typed_expr () in
		{tf_args; tf_type; tf_expr}

	method read_tfunc_arg () =
		let t1 = self#read_tvar () in
		t1, (self#read_nullable self#read_typed_expr)
		(*
	method read_anon_type () =
		let a_fields = [] in (* TODO *)
		let a_status = self#read_enum (function
			| 0 -> Closed
			| 1 -> Opened
			| 2 -> Const
			| 3 -> Extend (self#read_list self#read_type)

			| _ -> raise (HxbReadFailure "read_anon_type")) in
		{a_fields; a_status}
*)
	method read_typed_expr_def () = self#read_enum (function
		| 0 -> TConst (self#read_tconstant ())

		| 2 ->
			let t1 = self#read_typed_expr () in
			TArray (t1, self#read_typed_expr ())
		| 3 ->
			let t1 = self#read_binop () in
			let t2 = self#read_typed_expr () in
			TBinop (t1, t2, self#read_typed_expr ())

		| 11 -> TTypeExpr (TClassDecl (self#read_class_ref ()))
		| 12 -> TTypeExpr (TEnumDecl (self#read_enum_ref ()))
		| 13 -> TTypeExpr (TTypeDecl (self#read_typedef_ref ()))
		| 14 -> TTypeExpr (TAbstractDecl (self#read_abstract_ref ()))
		| 15 -> TParenthesis (self#read_typed_expr ())
		| 16 -> TObjectDecl (self#read_list self#read_tobject_field)
		| 17 -> TArrayDecl (self#read_list self#read_typed_expr)
		| 18 ->
			let t1 = self#read_typed_expr () in
			TCall (t1, self#read_list self#read_typed_expr)
		| 19 ->
			let t1 = self#read_class_ref () in
			let t2 = self#read_list self#read_type in
			TNew (t1, t2, self#read_list self#read_typed_expr)
		| 20 ->
			let op, flag = self#read_unop () in
			TUnop (op, flag, self#read_typed_expr ())
		| 21 -> TFunction (self#read_tfunc ())
		| 22 -> TVar (self#read_tvar (), None)
		| 23 ->
			let t1 = self#read_tvar () in
			TVar (t1, Some (self#read_typed_expr ()))
		| 24 -> TBlock (self#read_list self#read_typed_expr)
		| 25 ->
			let t1 = self#read_tvar () in
			let t2 = self#read_typed_expr () in
			TFor (t1, t2, self#read_typed_expr ())
		| 26 ->
			let t1 = self#read_typed_expr () in
			TIf (t1, self#read_typed_expr (), None)
		| 27 ->
			let t1 = self#read_typed_expr () in
			let t2 = self#read_typed_expr () in
			TIf (t1, t2, Some (self#read_typed_expr ()))
		| 28 ->
			let t1 = self#read_typed_expr () in
			TWhile (t1, self#read_typed_expr (), NormalWhile)
		| 29 ->
			let t1 = self#read_typed_expr () in
			TWhile (t1, self#read_typed_expr (), DoWhile)
		| 30 ->
			let t1 = self#read_typed_expr () in
			TSwitch (t1, self#read_list self#read_tcase, None)
		| 31 ->
			let t1 = self#read_typed_expr () in
			let t2 = self#read_list self#read_tcase in
			TSwitch (t1, t2, Some (self#read_typed_expr ()))
		| 32 ->
			let t1 = self#read_typed_expr () in
			TTry (t1, [self#read_tcatch ()])
		| 33 ->
			let t1 = self#read_typed_expr () in
			TTry (t1, self#read_list self#read_tcatch)
		| 34 -> TReturn None
		| 35 -> TReturn (Some (self#read_typed_expr ()))
		| 36 -> TBreak
		| 37 -> TContinue
		| 38 -> TThrow (self#read_typed_expr ())
		| 39 -> TCast (self#read_typed_expr (), None)
		| 40 ->
			let t1 = self#read_typed_expr () in
			TCast (t1, Some (TClassDecl (self#read_class_ref ())))
		| 41 ->
			let t1 = self#read_typed_expr () in
			TCast (t1, Some (TEnumDecl (self#read_enum_ref ())))
		| 42 ->
			let t1 = self#read_typed_expr () in
			TCast (t1, Some (TTypeDecl (self#read_typedef_ref ())))
		| 43 ->
			let t1 = self#read_typed_expr () in
			TCast (t1, Some (TAbstractDecl (self#read_abstract_ref ())))
		| 44 ->
			let t1 = self#read_metadata_entry () in
			TMeta (t1, self#read_typed_expr ())

		| 46 -> TEnumIndex (self#read_typed_expr ())
		| 47 -> TIdent (self#read_pstr ())
		| _ -> raise (HxbReadFailure "read_typed_expr_def"))

	method read_tobject_field () =
		let t1 = self#read_pstr () in
		let t2 = self#read_pos () in
		let t3 = (if (self#read_bool ()) then DoubleQuotes else NoQuotes) in
		(t1, t2, t3), (self#read_typed_expr ())

	method read_tcase () =
		let t1 = self#read_list self#read_typed_expr in
		t1, (self#read_typed_expr ())

	method read_tcatch () =
		let t1 = self#read_tvar () in
		t1, (self#read_typed_expr ())

	method read_typed_expr () =
		let eexpr = self#read_typed_expr_def () in
		let etype = self#read_type () in
		let epos = self#read_pos () in
		{eexpr; etype; epos}

	method read_base_type () =
		let mt_module = self#stub_module () in
		let mt_path = self#read_path () in
		let mt_pos = self#read_pos () in
		let mt_name_pos = self#read_pos () in
		let mt_private = self#read_bool () in
		let mt_doc = self#read_doc () in
		let mt_meta = self#read_list self#read_metadata_entry in
		let mt_params = self#read_type_params () in
		let mt_using = self#read_list self#read_class_using in
		{mt_module; mt_path; mt_pos; mt_name_pos; mt_private; mt_doc; mt_meta; mt_params; mt_using}

	method read_class_using () =
		let t1 = self#read_class_ref () in
		t1, (self#read_pos ())

	method stub_module () = raise (Failure "not implemented")

	method read_class_field () =
		let cf_name = self#read_pstr () in
		let cf_type = self#read_type () in
		let cf_pos = self#read_pos () in
		let cf_name_pos = self#read_pos () in
		let cf_doc = self#read_doc () in
		let cf_meta = self#read_list self#read_metadata_entry in
		let cf_kind = self#read_enum (function
			| 0 -> Method(MethNormal)
			| 1 -> Method(MethInline)
			| 2 -> Method(MethDynamic)
			| 3 -> Method(MethMacro)
			| v when (v >= 10) && (v <= 234) ->
				let v = v - 10 in
				let r = v / 15 in
				let w = v mod 15 in
				let sub_access = (function
					| 0 -> AccNormal
					| 1 -> AccNo
					| 2 -> AccNever
					| 3 -> AccCtor
					| 4 -> AccResolve
					| 5 -> AccCall
					| 6 -> AccInline
					| 7 ->
						let t1 = self#read_pstr () in
						AccRequire (t1, self#read_nullable self#read_pstr)
					| _ -> raise (HxbReadFailure "read_typed_expr_def cf_kind")) in
				let v_read = sub_access r in
				let v_write = sub_access w in
				Var {v_read; v_write}
			| _ -> raise (HxbReadFailure "read_typed_expr_def cf_kind")) in
		let cf_params = self#read_type_params () in
		let cf_expr = self#read_nullable self#read_typed_expr in
		let cf_expr_unoptimized = self#read_nullable self#read_tfunc in
		let cf_overloads = [] in (* TODO *)
		let cf_flags_public, cf_flags_extern, cf_flags_final, cf_flags_overridden, cf_flags_modifies_this = self#read_bools5 () in
		let cf_flags = ref 0 in
		if cf_flags_public then cf_flags := set_flag !cf_flags (int_of_class_field_flag CfPublic);
		if cf_flags_extern then cf_flags := set_flag !cf_flags (int_of_class_field_flag CfExtern);
		if cf_flags_final then cf_flags := set_flag !cf_flags (int_of_class_field_flag CfFinal);
		if cf_flags_overridden then cf_flags := set_flag !cf_flags (int_of_class_field_flag CfOverridden);
		if cf_flags_modifies_this then cf_flags := set_flag !cf_flags (int_of_class_field_flag CfModifiesThis);
		let cf_flags = !cf_flags in
		{cf_name; cf_type; cf_pos; cf_name_pos; cf_doc; cf_meta; cf_kind; cf_params; cf_expr; cf_expr_unoptimized; cf_overloads; cf_flags}

	(*method read_class_type () =
		let base = self#read_base_type () in
		let cl_kind = self#read_enum (function
			| 0 -> KNormal
			| 1 -> KTypeParameter (self#read_list self#read_type)
			| 2 -> KExpr (self#read_expr ())
			| 3 -> KGeneric

			| 5 -> KMacroType

			| _ -> raise (HxbReadFailure "read_class_type cl_kind"))
		let cl_extern, cl_final, cl_interface = self#read_bools3 () in*)

	method read_enum_field ef_index =
		let ef_name = self#read_pstr () in
		let ef_type = self#read_type () in
		let ef_pos = self#read_pos () in
		let ef_name_pos = self#read_pos () in
		let ef_doc = self#read_doc () in
		let ef_params = self#read_type_params () in
		let ef_meta = self#read_list self#read_metadata_entry in
		{ef_name; ef_type; ef_pos; ef_name_pos; ef_doc; ef_index; ef_params; ef_meta}

	(* File structure *)

	method read_chunk_header () =
		last_file <- "";
		last_delta <- 0;
		let chk_id = Bytes.to_string (self#read_str_raw 4) in
		let chk_size = Int32.to_int (self#read_u32 ()) in
		{chk_id; chk_size}

	method read_header () =
		let config_store_positions = self#read_bool () in
		let module_path = self#read_path () in
		{config_store_positions; module_path}

	method read_string_pool () =
		let data = self#read_arr self#read_str in
		({data} : hxb_chunk_string_pool)

	method read_doc_pool () =
		let data = self#read_arr self#read_str in
		({data} : hxb_chunk_doc_pool)

	method read_type_list () =
		let external_classes = self#read_arr self#read_path in
		let external_enums = self#read_arr self#read_path in
		let external_abstracts = self#read_arr self#read_path in
		let external_typedefs = self#read_arr self#read_path in
		let internal_classes = self#read_arr self#read_str in
		let internal_enums = self#read_arr self#read_str in
		let internal_abstracts = self#read_arr self#read_str in
		let internal_typedefs = self#read_arr self#read_str in
		{external_classes; external_enums; external_abstracts; external_typedefs; internal_classes; internal_enums; internal_abstracts; internal_typedefs}

	method read_field_list () =
		let type_list = Option.get last_type_list in
		let read1 = Array.map (fun _ -> self#read_arr self#read_str) in
		let read2 = Array.map (fun _ -> self#read_arr self#read_str) in
		let class_fields = Array.append (read1 type_list.external_classes) (read2 type_list.internal_classes) in
		let enum_fields = Array.append (read1 type_list.external_enums) (read2 type_list.internal_enums) in
		{class_fields; enum_fields}

	method read_type_declarations () =
		raise (Failure "lol")

	method read_module_extra () =
		raise (Failure "lol")

	method read_hxb () =
		if (Bytes.to_string (self#read_str_raw 4)) <> "hxb1" then
			raise (HxbReadFailure "magic");
		(* TODO: allow chunk reordering by reading chunk headers only, then
			processing chunk types in a well-defined order. *)
		last_header <- Some (self#read_header ());
		let rec loop () =
			let chunk_header = self#read_chunk_header () in
			(if (match chunk_header.chk_id with
				| "HHDR" -> raise (HxbReadFailure "duplicate header")
				| "HEND" -> false
				| "STRI" -> last_string_pool <- Some (self#read_string_pool ()); true
				| "dOCS" -> last_doc_pool <- Some (self#read_doc_pool ()); true
				| "TYPL" -> last_type_list <- Some (self#read_type_list ()); true
				| "FLDL" -> last_field_list <- Some (self#read_field_list ()); true
				| "TYPE" -> last_type_declarations <- Some (self#read_type_declarations ()); true
				| "xTRA" -> last_module_extra <- Some (self#read_module_extra ()); true
				| _ -> raise (HxbReadFailure "unknown chunk") (* TODO: skip if ancillary *)) then
				ignore (self#read_u32 ()); (* TODO: checksum verify *)
				loop ());
			()
		in loop ()

end
