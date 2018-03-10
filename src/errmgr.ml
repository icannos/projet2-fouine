
(* Structure pour conserver les metadata des noeuds de l'ast *)
module IntHash =
        struct
          type t = int
          let equal i j = i=j
          let hash i = i land max_int
        end
;;

module AstMetaData  = Hashtbl.Make(IntHash);;

let metadata = (AstMetaData.create 100);;

exception UnknownIdentifier of string;;
exception DivisionByZero;;
exception CannotApply of string;;
exception BadArgument of string;;


let error_handler start_pos end_pos =
  let data : Lexing.position * Lexing.position = (start_pos, end_pos)
  and node_id = (AstMetaData.length metadata) in
  AstMetaData.add metadata node_id data; node_id
;;
  

(* let error_display node_id = function *)
