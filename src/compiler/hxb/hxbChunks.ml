open Globals
open Ast
open Type

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
	external_classes : type_path list;
	external_enums : type_path list;
	external_abstracts : type_path list;
	external_typedefs : type_path list;
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
