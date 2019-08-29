open Globals
open Ast
open Type

type hxb_chunk = {
	chk_id : string;
	chk_size : int;
}

type hxb_chunk_header = {
	config_store_positions : bool;
	module_path : path;
}

type hxb_chunk_string_pool = {
	data : string list;
}

type hxb_chunk_doc_pool = {
	data : string list;
}

type hxb_chunk_type_list = {
	external_classes : path list;
	external_enums : path list;
	external_abstracts : path list;
	external_typedefs : path list;
	internal_classes : string list;
	internal_enums : string list;
	internal_abstracts : string list;
	internal_typedefs : string list;
}

type hxb_chunk_field_list = {
	class_fields : string list list;
	enum_fields : string list list;
}

type hxb_chunk_type_declarations = {
	class_declarations : tclass list;
	enum_declarations : tenum list;
	abstract_declarations : tabstract list;
	typedef_declarations : tdef list;
}

type hxb_chunk_module_extra = module_def_extra
