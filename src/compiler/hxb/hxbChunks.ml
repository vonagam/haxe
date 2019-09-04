open Globals
open Ast
open Type

type hxb_chunk = {
	chk_id : string;
	chk_size : int;
	chk_data : bytes;
	chk_checksum : int32;
}

type hxb_chunk_header = {
	config_store_positions : bool;
	module_path : path;
}

type hxb_chunk_string_pool = {
	data : string array;
}

type hxb_chunk_doc_pool = {
	data : string array;
}

type hxb_chunk_type_list = {
	external_classes : path array;
	external_enums : path array;
	external_abstracts : path array;
	external_typedefs : path array;
	internal_classes : (string * pos * pos * bool) array;
	internal_enums : (string * pos * pos * bool) array;
	internal_abstracts : (string * pos * pos * bool) array;
	internal_typedefs : (string * pos * pos * bool) array;
}

type hxb_chunk_field_list = {
	class_fields : string array array;
	enum_fields : string array array;
}

type hxb_chunk_type_declarations = {
	class_declarations : tclass list;
	enum_declarations : tenum list;
	abstract_declarations : tabstract list;
	typedef_declarations : tdef list;
}

type hxb_chunk_module_extra = module_def_extra
