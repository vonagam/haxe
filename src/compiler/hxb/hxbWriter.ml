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

let pmap_to_list map = PMap.foldi (fun k x l -> (k,x) :: l) map []
let hashtbl_to_list h = Hashtbl.fold (fun k x l -> (k,x) :: l) h []

class ['a] hxb_writer (ch : 'a IO.output) (cp : hxb_constant_pool_writer) = object(self)

	(* basic *)

	method write_byte b =
		IO.write_byte ch b;

	method write_bool b =
		self#write_byte (if b then 1 else 0)

	method write_ui16 i =
		IO.write_ui16 ch i;

	method write_i16 i =
		IO.write_i16 ch i;

	method write_i32 i =
		IO.write_real_i32 ch (Int32.of_int i);

	method write_float f =
		IO.write_double ch f

	method write_string s =
		self#write_i32 (cp#get_index s);

	method write_bytes b =
		self#write_i32 (Bytes.length b);
		IO.nwrite ch b;

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

	val mutable last_type = Bytes.create 0

	method write_type_instance' t =
		let write_function_arg (n,o,t) =
			self#write_string n;
			self#write_bool o;
			self#write_type_instance' t;
		in
		match t with
		| TMono r ->
			begin match !r with
			| None -> self#write_byte 0
			| Some t ->
				self#write_byte 1;
				self#write_type_instance' t
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
			self#write_type_instance' (lazy_type r);
		| TDynamic t ->
			if t == t_dynamic then self#write_byte 40
			else begin
				self#write_byte 41;
				self#write_type_instance' t;
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
			let l = pmap_to_list an.a_fields in
			self#write_list16 l (fun (_,cf) -> self#write_class_field cf);
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

	method write_type_instance t =
		let bytes =
			let ch = IO.output_bytes() in
			let type_writer = new hxb_writer ch cp in
			type_writer#write_type_instance' t;
			IO.close_out ch
		in
		if bytes = last_type then
			self#write_byte 0xFF
		else begin
			last_type <- bytes;
			IO.nwrite ch bytes
		end;

	method write_types tl =
		self#write_list16 tl self#write_type_instance

	(* texpr *)

	method write_var v =
		self#write_i32 v.v_id;
		self#write_string v.v_name;
		self#write_type_instance v.v_type;
		(* TODO: kind *)
		self#write_bool v.v_capture;
		self#write_bool v.v_final;
		self#write_option v.v_extra (fun (tl,eo) ->
			self#write_type_params tl;
			self#write_option eo self#write_texpr;
		);
		self#write_metadata v.v_meta;
		self#write_pos v.v_pos;

	method write_texpr (e : texpr) =
		self#write_pos e.epos;
		let curmin = ref e.epos.pmin in
		let curmax = ref e.epos.pmax in
		let check_diff p =
			let dmin = p.pmin - !curmin in
			let dmax = p.pmax - !curmax in
			self#write_i16 dmin;
			self#write_i16 dmax;
			curmin := p.pmin;
			curmax := p.pmax;
		in
		let rec loop e =
			self#write_type_instance e.etype;
			check_diff e.epos;
			match e.eexpr with
			(* values 0-19 *)
			| TConst ct ->
				begin match ct with
				| TNull ->
					self#write_byte 0;
				| TThis ->
					self#write_byte 1;
				| TSuper ->
					self#write_byte 2;
				| TBool false ->
					self#write_byte 3;
				| TBool true ->
					self#write_byte 4;
				| TInt i32 ->
					self#write_byte 5;
					IO.write_real_i32 ch i32;
				| TFloat f ->
					self#write_byte 6;
					self#write_string f;
				| TString s ->
					self#write_byte 7;
					self#write_string s
				end
			(* vars 20-29 *)
			| TLocal v ->
				self#write_byte 20;
				self#write_i32 v.v_id;
			| TVar(v,None) ->
				self#write_byte 21;
				self#write_var v;
			| TVar(v,Some e1) ->
				self#write_byte 22;
				self#write_var v;
				loop e1;
			(* blocks 30-49 *)
			| TBlock [] ->
				self#write_byte 30;
			| TBlock el ->
				let l = List.length el in
				begin match l with
				| 1 -> self#write_byte 31;
				| 2 -> self#write_byte 32;
				| 3 -> self#write_byte 33;
				| 4 -> self#write_byte 34;
				| 5 -> self#write_byte 35;
				| _ ->
					if l <= 0xFF then begin
						self#write_byte 36;
						self#write_byte l;
					end else if l < 0xFFFF then begin
						self#write_byte 37;
						self#write_ui16 l;
					end else begin
						self#write_byte 38;
						self#write_i32 l;
					end;
				end;
				List.iter loop el
			(* function 50-59 *)
			| TFunction tf ->
				self#write_byte 50;
				self#write_list16 tf.tf_args (fun (v,eo) ->
					self#write_var v;
					self#write_option eo loop
				);
				self#write_type_instance tf.tf_type;
				loop tf.tf_expr;
			(* texpr compounds 60-79 *)
			| TArray(e1,e2) ->
				self#write_byte 60;
				loop e1;
				loop e2;
			| TParenthesis e1 ->
				self#write_byte 61;
				loop e1;
			| TArrayDecl el ->
				self#write_byte 62;
				loop_el el;
			| TObjectDecl fl ->
				self#write_byte 63;
				self#write_list16 fl (fun ((name,p,qs),e) ->
					self#write_string name;
					self#write_pos p;
					(* TODO: qs *)
					loop e
				);
			| TCall(e1,el) ->
				self#write_byte 64;
				loop e1;
				loop_el el;
			| TMeta(m,e1) ->
				self#write_byte 65;
				self#write_metadata_entry m;
				loop e1;
			(* branching 80-89 *)
			| TIf(e1,e2,None) ->
				self#write_byte 80;
				loop e1;
				loop e2;
			| TIf(e1,e2,Some e3) ->
				self#write_byte 81;
				loop e1;
				loop e2;
				loop e3;
			| TSwitch(e1,cases,def) ->
				self#write_byte 82;
				loop e1;
				self#write_list16 cases (fun (el,e) ->
					loop_el el;
					loop e;
				);
				self#write_option def loop;
			| TTry(e1,catches) ->
				self#write_byte 83;
				loop e1;
				self#write_list16 catches  (fun (v,e) ->
					self#write_var v;
					loop e
				);
			| TWhile(e1,e2,flag) ->
				self#write_byte (if flag = NormalWhile then 84 else 85);
				loop e1;
				loop e2;
			| TFor(v,e1,e2) ->
				self#write_byte 86;
				self#write_var v;
				loop e1;
				loop e2;
			(* control flow 90-99 *)
			| TReturn None ->
				self#write_byte 90;
			| TReturn (Some e1) ->
				self#write_byte 91;
				loop e1;
			| TContinue ->
				self#write_byte 92;
			| TBreak ->
				self#write_byte 93;
			| TThrow e1 ->
				self#write_byte 94;
				loop e1;
			(* access 100-119 *)
			| TEnumIndex e1 ->
				self#write_byte 100;
				loop e1;
			| TEnumParameter(e1,ef,i) ->
				self#write_byte 101;
				loop e1;
				self#write_string ef.ef_name; (* TODO: not sure what to do with this... *)
				self#write_i32 i;
			| TField(e1,FInstance(c,tl,cf)) ->
				self#write_byte 102;
				loop e1;
				self#write_path c.cl_path;
				self#write_types tl;
				self#write_string cf.cf_name;
			| TField(e1,FStatic(c,cf)) ->
				self#write_byte 103;
				loop e1;
				self#write_path c.cl_path;
				self#write_string cf.cf_name;
			| TField(e1,FAnon cf) ->
				self#write_byte 104;
				loop e1;
				self#write_string cf.cf_name;
			| TField(e1,FClosure(Some(c,tl),cf)) ->
				self#write_byte 105;
				loop e1;
				self#write_path c.cl_path;
				self#write_types tl;
				self#write_string cf.cf_name;
			| TField(e1,FClosure(None,cf)) ->
				self#write_byte 106;
				loop e1;
				self#write_string cf.cf_name;
			| TField(e1,FEnum(en,ef)) ->
				self#write_byte 107;
				loop e1;
				self#write_path en.e_path;
				self#write_string ef.ef_name;
			| TField(e1,FDynamic s) ->
				self#write_byte 108;
				loop e1;
				self#write_string s;
			(* module types 120-139 *)
			| TTypeExpr (TClassDecl c) ->
				self#write_byte 120;
				self#write_path c.cl_path;
			| TTypeExpr (TEnumDecl en) ->
				self#write_byte 121;
				self#write_path en.e_path;
			| TTypeExpr (TAbstractDecl a) ->
				self#write_byte 122;
				self#write_path a.a_path
			| TTypeExpr (TTypeDecl td) ->
				self#write_byte 123;
				self#write_path td.t_path
			| TCast(e1,None) ->
				self#write_byte 124;
				loop e1;
			| TCast(e1,Some md) ->
				self#write_byte 125;
				loop e1;
				self#write_path (t_infos md).mt_path
			| TNew(c,tl,el) ->
				self#write_byte 126;
				self#write_path c.cl_path;
				self#write_types tl;
				loop_el el;
			(* unops 140-159 *)
			| TUnop(Increment,Prefix,e1) ->
				self#write_byte 140;
				loop e1;
			| TUnop(Decrement,Prefix,e1) ->
				self#write_byte 141;
				loop e1;
			| TUnop(Not,Prefix,e1) ->
				self#write_byte 142;
				loop e1;
			| TUnop(Neg,Prefix,e1) ->
				self#write_byte 143;
				loop e1;
			| TUnop(NegBits,Prefix,e1) ->
				self#write_byte 144;
				loop e1;
			| TUnop(Increment,Postfix,e1) ->
				self#write_byte 145;
				loop e1;
			| TUnop(Decrement,Postfix,e1) ->
				self#write_byte 146;
				loop e1;
			| TUnop(Not,Postfix,e1) ->
				self#write_byte 147;
				loop e1;
			| TUnop(Neg,Postfix,e1) ->
				self#write_byte 148;
				loop e1;
			| TUnop(NegBits,Postfix,e1) ->
				self#write_byte 149;
				loop e1;
			(* binops 160-219 *)
			| TBinop(op,e1,e2) ->
				let rec idx op = match op with
					| OpAdd -> 0
					| OpMult -> 1
					| OpDiv -> 2
					| OpSub -> 3
					| OpAssign -> 4
					| OpEq -> 5
					| OpNotEq -> 6
					| OpGt -> 7
					| OpGte -> 8
					| OpLt -> 9
					| OpLte -> 10
					| OpAnd -> 11
					| OpOr -> 12
					| OpXor -> 13
					| OpBoolAnd -> 14
					| OpBoolOr -> 15
					| OpShl -> 16
					| OpShr -> 17
					| OpUShr -> 18
					| OpMod -> 19
					| OpInterval -> 20
					| OpArrow -> 21
					| OpIn -> 22
					| OpAssignOp op -> 30 + idx op
				in
				self#write_byte (160 + idx op);
				loop e1;
				loop e2;
			(* rest 250-254 *)
			| TIdent s ->
				self#write_byte 250;
				self#write_string s;
		and loop_el el =
			self#write_ui16 (List.length el);
			List.iter loop el
		in
		loop e

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
		self#write_option cf.cf_expr self#write_texpr;
		(* TODO: expr_unoptimized *)
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
		self#write_list8 infos.mt_using (fun (c,p) ->
			self#write_path c.cl_path;
			self#write_pos p;
		);
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
			self#write_option c.cl_init self#write_texpr;
			self#write_list16 c.cl_overrides (fun cf -> self#write_string cf.cf_name);
		| _ ->
			self#write_byte 1;
			(* TODO *)

	method write_module m =
		self#write_i32 m.m_id;
		self#write_path m.m_path;
		self#write_list16 m.m_types self#write_module_type;
		let extra = m.m_extra in
		self#write_string extra.m_file;
		self#write_string (Digest.to_hex extra.m_sign);
		self#write_list16 extra.m_display.m_inline_calls (fun (p1,p2) ->
			self#write_pos p1;
			self#write_pos p2;
		);
		self#write_list16 extra.m_display.m_type_hints (fun (p,t) ->
			self#write_pos p;
			self#write_type_instance t;
		);
		self#write_list8 extra.m_check_policy (fun pol -> self#write_byte (Obj.magic pol)); (* TODO: don't be lazy *)
		self#write_float extra.m_time;
		self#write_option extra.m_dirty (fun m -> self#write_path m.m_path);
		self#write_i32 extra.m_added;
		self#write_i32 extra.m_mark;
		self#write_list16 (pmap_to_list extra.m_deps) (fun (i,m) ->
			self#write_i32 i;
			self#write_path m.m_path;
		);
		self#write_i32 extra.m_processed;
		self#write_byte (Obj.magic extra.m_kind); (* TODO: don't be lazy *)
		self#write_list16 (pmap_to_list extra.m_binded_res) (fun (s1,s2) ->
			self#write_string s1;
			self#write_bytes (Bytes.unsafe_of_string s2);
		);
		self#write_list16 extra.m_if_feature (fun (s,(c,cf,b)) ->
			self#write_string s;
			self#write_path c.cl_path;
			self#write_string cf.cf_name;
			self#write_bool b;
		);
		self#write_list16 (hashtbl_to_list extra.m_features) (fun (s,b) ->
			self#write_string s;
			self#write_bool b;
		);
end