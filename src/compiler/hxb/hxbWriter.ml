open Globals
open Ast
open Type

class hxb_constant_pool_writer = object(self)
	val lut = Hashtbl.create 0
	val pool = DynArray.create ()

	method get_index (s : string) =
		try
			Hashtbl.find lut s
		with Not_found ->
			let index = DynArray.length pool in
			Hashtbl.add lut s index;
			DynArray.add pool s;
			index

	method export : 'a . 'a IO.output -> unit = fun ch ->
		IO.write_real_i32 ch (Int32.of_int (DynArray.length pool));
		DynArray.iter (fun s ->
			let b = Bytes.of_string s in
			IO.write_real_i32 ch (Int32.of_int (Bytes.length b));
			IO.nwrite ch b;
		) pool;
end

class ['a] hxb_writer (ch : 'a IO.output) (cp : hxb_constant_pool_writer) = object(self)

	(* basic *)

	method write_byte b =
		IO.write_byte ch b;

	method write_bool b =
		self#write_byte (if b then 1 else 0)

	method write_ui16 i =
		IO.write_ui16 ch i;

	method write_i32 i =
		IO.write_real_i32 ch (Int32.of_int i);

	method write_string s =
		self#write_i32 (cp#get_index s);

	method write_list8 : 'b . 'b list -> ('b -> unit) -> unit = fun l f ->
		self#write_byte (List.length l);
		List.iter f l;

	method write_list16 : 'b . 'b list -> ('b -> unit) -> unit = fun l f ->
		self#write_ui16 (List.length l);
		List.iter f l;

	method write_option : 'b . 'b option -> ('b -> unit) -> unit = fun v f -> match v with
		| None -> self#write_byte 0
		| Some v ->
			self#write_byte 1;
			f v

	method write_path (path : path) =
		self#write_list8 (fst path) self#write_string;
		self#write_string (snd path);

	(* basic compounds *)

	method write_pos p =
		self#write_string p.pfile;
		self#write_i32 p.pmin;
		self#write_i32 p.pmax;

	method write_metadata_entry ((meta,el,p) : metadata_entry) =
		self#write_string (Meta.to_string meta);
		(* TODO: el -_- *)
		self#write_pos p

	method write_metadata ml =
		self#write_list16 ml self#write_metadata_entry

	method write_type_params params =
		self#write_list16 params (fun (s,t) ->
			self#write_string s;
			match follow t with
			| TInst({cl_kind = KTypeParameter tl},_) ->
				self#write_types tl;
			| _ ->
				assert false
		)

	(* type instance *)

	method write_type_instance t =
		let write_function_arg (n,o,t) =
			self#write_string n;
			self#write_bool o;
			self#write_type_instance t;
		in
		match t with
		| TMono r ->
			begin match !r with
			| None -> self#write_byte 0
			| Some t ->
				self#write_byte 1;
				self#write_type_instance t
			end
		| TInst(c,[]) ->
			self#write_byte 10;
			self#write_path c.cl_path
		| TEnum(en,[]) ->
			self#write_byte 11;
			self#write_path en.e_path
		| TType(td,[]) ->
			self#write_byte 12;
			self#write_path td.t_path
		| TAbstract(a,[]) ->
			self#write_byte 13;
			self#write_path a.a_path
		| TInst(c,tl) ->
			self#write_byte 14;
			self#write_path c.cl_path;
			self#write_types tl
		| TEnum(en,tl) ->
			self#write_byte 15;
			self#write_path en.e_path;
			self#write_types tl
		| TType(td,tl) ->
			self#write_byte 16;
			self#write_path td.t_path;
			self#write_types tl
		| TAbstract(a,tl) ->
			self#write_byte 17;
			self#write_path a.a_path;
			self#write_types tl
		| TFun([],t) when ExtType.is_void (follow t) ->
			self#write_byte 30;
		| TFun(args,t) when ExtType.is_void (follow t) ->
			self#write_byte 31;
			self#write_list16 args write_function_arg;
		| TFun(args,t) ->
			self#write_byte 32;
			self#write_list16 args write_function_arg;
		| TLazy r ->
			self#write_type_instance (lazy_type r);
		| TDynamic t ->
			if t == t_dynamic then self#write_byte 40
			else begin
				self#write_byte 41;
				self#write_type_instance t;
			end
		| TAnon an ->
			begin match !(an.a_status) with
			| Closed -> self#write_byte 50
			| Opened -> self#write_byte 51
			| Const -> self#write_byte 52
			| Extend _ -> self#write_byte 53
			| Statics _ -> self#write_byte 54
			| EnumStatics _ -> self#write_byte 55
			| AbstractStatics _ -> self#write_byte 56
			end;
			let l = PMap.fold (fun cf l -> cf :: l) an.a_fields [] in
			self#write_list16 l self#write_class_field;
			begin match !(an.a_status) with
			| Extend tl -> self#write_types tl
			| Statics c -> self#write_path c.cl_path
			| EnumStatics en -> self#write_path en.e_path
			| AbstractStatics a -> self#write_path a.a_path
			| Closed
			| Opened
			| Const ->
				()
			end;

	method write_types tl =
		self#write_list16 tl self#write_type_instance

	(* field *)

	method write_field_kind = function
		| Method MethNormal -> self#write_byte 0;
		| Method MethInline -> self#write_byte 1;
		| Method MethDynamic -> self#write_byte 2;
		| Method MethMacro -> self#write_byte 3;
		(* normal read *)
		| Var {v_read = AccNormal; v_write = AccNormal } -> self#write_byte 10
		| Var {v_read = AccNormal; v_write = AccNo } -> self#write_byte 11
		| Var {v_read = AccNormal; v_write = AccNever } -> self#write_byte 12
		| Var {v_read = AccNormal; v_write = AccCtor } -> self#write_byte 13
		| Var {v_read = AccNormal; v_write = AccCall } -> self#write_byte 14
		(* inline read *)
		| Var {v_read = AccInline; v_write = AccNever } -> self#write_byte 20
		(* getter read *)
		| Var {v_read = AccCall; v_write = AccNormal } -> self#write_byte 30
		| Var {v_read = AccCall; v_write = AccNo } -> self#write_byte 31
		| Var {v_read = AccCall; v_write = AccNever } -> self#write_byte 32
		| Var {v_read = AccCall; v_write = AccCtor } -> self#write_byte 33
		| Var {v_read = AccCall; v_write = AccCall } -> self#write_byte 34
		(* weird/overlooked combinations *)
		| Var {v_read = r;v_write = w } ->
			self#write_byte 100;
			let f = function
				| AccNormal -> self#write_byte 0
				| AccNo -> self#write_byte 1
				| AccNever -> self#write_byte 2
				| AccCtor -> self#write_byte 3
				| AccResolve -> self#write_byte 4
				| AccCall -> self#write_byte 5
				| AccInline -> self#write_byte 6
				| AccRequire(s,so) ->
					self#write_byte 7;
					self#write_string s;
					self#write_option so self#write_string
			in
			f r;
			f w;

	method write_class_field cf =
		self#write_string cf.cf_name;
		self#write_type_instance cf.cf_type;
		self#write_pos cf.cf_pos;
		self#write_pos cf.cf_name_pos;
		self#write_option cf.cf_doc self#write_string;
		self#write_metadata cf.cf_meta;
		self#write_type_params cf.cf_params;
		self#write_field_kind cf.cf_kind;
		(* TODO: kind, expr, expr_unoptimized *)
		self#write_list16 cf.cf_overloads self#write_class_field;
		(* self#write_i32 cf.cf_flags *)

	(* module *)

	method write_class_kind = function
		| KNormal ->
			self#write_byte 0
		| KTypeParameter tl ->
			self#write_byte 1;
			self#write_types tl;
		| KExpr e ->
			self#write_byte 2;
			(* TODO *)
		| KGeneric ->
			self#write_byte 3;
		| KGenericInstance(c,tl) ->
			self#write_byte 4;
			self#write_path c.cl_path;
			self#write_types tl
		| KMacroType ->
			self#write_byte 5;
		| KGenericBuild l ->
			self#write_byte 6;
			(* TODO *)
		| KAbstractImpl a ->
			self#write_byte 7;
			self#write_path a.a_path

	method write_module_type mt =
		let infos = t_infos mt in
		self#write_path infos.mt_path;
		self#write_pos infos.mt_pos;
		self#write_pos infos.mt_name_pos;
		self#write_bool infos.mt_private;
		self#write_option infos.mt_doc self#write_string;
		self#write_metadata infos.mt_meta;
		self#write_type_params infos.mt_params;
		(* TODO: using *)
		match mt with
		| TClassDecl c ->
			self#write_byte 0;
			self#write_class_kind c.cl_kind;
			self#write_bool c.cl_extern;
			self#write_bool c.cl_final;
			self#write_bool c.cl_interface;
			let write_relation (cr,tl) =
				self#write_path cr.cl_path;
				self#write_types tl;
			in
			self#write_option c.cl_super write_relation;
			self#write_list16 c.cl_implements write_relation;
			self#write_list16 c.cl_ordered_statics self#write_class_field;
			self#write_list16 c.cl_ordered_fields self#write_class_field;
			self#write_option c.cl_dynamic self#write_type_instance;
			self#write_option c.cl_array_access self#write_type_instance;
			self#write_option c.cl_constructor self#write_class_field;
			(* TODO: init *)
			self#write_list16 c.cl_overrides (fun cf -> self#write_string cf.cf_name);
		| _ ->
			()
			(* TODO *)

	method write_module m =
		self#write_i32 m.m_id;
		self#write_path m.m_path;
		self#write_list16 m.m_types self#write_module_type;
		(* TODO: m_extra *)
end