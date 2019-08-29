open Globals
open Ast
open Type

module HxbEnums = struct
	module ModuleCheckPolicy = struct
		let to_int = function
			| NoCheckFileTimeModification -> 0
			| CheckFileContentModification -> 1
			| NoCheckDependencies -> 2
			| NoCheckShadowing -> 3
		let from_int = function
			| 0 -> NoCheckFileTimeModification
			| 1 -> CheckFileContentModification
			| 2 -> NoCheckDependencies
			| 3 -> NoCheckShadowing
			| _ -> raise (Invalid_argument "enum")
	end
	module ModuleKind = struct
		let to_int = function
			| MCode -> 0
			| MMacro -> 1
			| MFake -> 2
			| MExtern -> 3
			| MImport -> 4
		let from_int = function
			| 0 -> MCode
			| 1 -> MMacro
			| 2 -> MFake
			| 3 -> MExtern
			| 4 -> MImport
			| _ -> raise (Invalid_argument "enum")
	end
	module Binop = struct
		let rec to_int = function
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
			| OpInterval -> 10
			| OpArrow -> 21
			| OpIn -> 22
			| OpAssignOp v -> 40 + to_int v
		let rec from_int = function
			| 0 -> OpAdd
			| 1 -> OpMult
			| 2 -> OpDiv
			| 3 -> OpSub
			| 4 -> OpAssign
			| 5 -> OpEq
			| 6 -> OpNotEq
			| 7 -> OpGt
			| 8 -> OpGte
			| 9 -> OpLt
			| 10 -> OpLte
			| 11 -> OpAnd
			| 12 -> OpOr
			| 13 -> OpXor
			| 14 -> OpBoolAnd
			| 15 -> OpBoolOr
			| 16 -> OpShl
			| 17 -> OpShr
			| 18 -> OpUShr
			| 19 -> OpMod
			| 20 -> OpInterval
			| 21 -> OpArrow
			| 22 -> OpIn
			| v when (v >= 40) && (v <= 62) -> OpAssignOp(from_int (v - 40))
			| _ -> raise (Invalid_argument "enum")
	end
	module Unop = struct
		let to_int = function
			| Increment, Prefix -> 0
			| Decrement, Prefix -> 1
			| Not, Prefix -> 2
			| Neg, Prefix -> 3
			| NegBits, Prefix -> 4
			| Increment, Postfix -> 40
			| Decrement, Postfix -> 41
			| Not, Postfix -> 42
			| Neg, Postfix -> 43
			| NegBits, Postfix -> 44
		let from_int = function
			| 0 -> Increment, Prefix
			| 1 -> Decrement, Prefix
			| 2 -> Not, Prefix
			| 3 -> Neg, Prefix
			| 4 -> NegBits, Prefix
			| 40 -> Increment, Postfix
			| 41 -> Decrement, Postfix
			| 42 -> Not, Postfix
			| 43 -> Neg, Postfix
			| 44 -> NegBits, Postfix
			| _ -> raise (Invalid_argument "enum")
	end
	module Access = struct
		let to_int = function
			| APublic -> 0
			| APrivate -> 1
			| AStatic -> 2
			| AOverride -> 3
			| ADynamic -> 4
			| AInline -> 5
			| AMacro -> 6
			| AFinal -> 7
			| AExtern -> 8
		let from_int = function
			| 0 -> APublic
			| 1 -> APrivate
			| 2 -> AStatic
			| 3 -> AOverride
			| 4 -> ADynamic
			| 5 -> AInline
			| 6 -> AMacro
			| 7 -> AFinal
			| 8 -> AExtern
			| _ -> raise (Invalid_argument "enum")
	end
	module TVarKind = struct
		let to_int = function
			| VGenerated -> 0
			| VInlined -> 1
			| VInlinedConstructorVariable -> 2
			| VExtractorVariable -> 3
			| VUser(TVOLocalVariable) -> 4
			| VUser(TVOArgument) -> 5
			| VUser(TVOForVariable) -> 6
			| VUser(TVOPatternVariable) -> 7
			| VUser(TVOCatchVariable) -> 8
			| VUser(TVOLocalFunction) -> 9
		let from_int = function
			| 0 -> VGenerated
			| 1 -> VInlined
			| 2 -> VInlinedConstructorVariable
			| 3 -> VExtractorVariable
			| 4 -> VUser(TVOLocalVariable)
			| 5 -> VUser(TVOArgument)
			| 6 -> VUser(TVOForVariable)
			| 7 -> VUser(TVOPatternVariable)
			| 8 -> VUser(TVOCatchVariable)
			| 9 -> VUser(TVOLocalFunction)
			| _ -> raise (Invalid_argument "enum")
	end
end
