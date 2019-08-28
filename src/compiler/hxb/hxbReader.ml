open Globals
open Ast
open Type

class hxb_reader (ch : IO.input) = object(self)
	(* Primitives *)

	method read_u8 =
		IO.read_byte ch

	method read_u32 =
		IO.read_real_i32 ch

	method read_f64 =
		IO.read_double ch

	method read_uleb128 =
		let b = self#read_u8 in
		if b >= 0x80 then
			(b land 0x7F) lor (self#read_uleb128 lsl 7)
		else
			b

	method read_leb128 =
		let rec read acc shift =
			let b = self#read_u8 in
			let cont = b >= 0x80 in
			let acc = ((b land 0x7F) lsl shift) lor acc in
			if cont then
				read acc (shift + 7)
			else
				(b, acc, shift + 7)
		in
		let last, acc, shift = read 0 0 in
		if (last land 0x40) != 0 then
			acc lor ((lnot 0) lsl shift)
		else
			acc

	method read_str_raw len =
		IO.really_nread ch len

	method read_str =
		self#read_str_raw (self#read_uleb128)

	method read_arr : 'b . (unit -> 'b) -> 'b list = fun f ->
		let rec read n =
			if n = 0 then
				[]
			else
				(f ()) :: (read (n - 1))
		in
		List.rev (read (self#read_uleb128))

	method read_bools : (bool -> unit) list -> unit = fun fs ->
		let b = self#read_u8 in
		for i = 0 to (List.length fs) - 1 do
			List.nth fs i ((b land (1 lsl i)) != 0)
		done

	method read_enum =
		self#read_u8

	method read_nullable : 'b . (unit -> 'b) -> 'b option = fun f ->
		if self#read_u8 != 0 then
			Some (f ())
		else
			None

	val mutable last_delta = 0

	method read_delta =
		let v = self#read_leb128 in
		last_delta <- last_delta + v;
		last_delta

end
