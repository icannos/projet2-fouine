

(* Structure pour conserver les metadata des noeuds de l'ast *)
module IntHash =
        struct
          type t = int
          let equal i j = i=j
          let hash i = i land max_int
        end
;;

module AstMetaData  = Hashtbl.Make(IntHash);;


let metadata = (AstMetaData.create 100);; (* On commence avec 100 noeuds, la table grandira automatiquement au besoin *)

let pf = Printf.printf;; (* print format strings *)

(* On définira ici l'ensemble des Exceptions que l'on souhaite gérer dans le parcours d'arbre *)
exception UnknownIdentifier of string;;
exception UnknownReference of string;;
exception NotReference of string;;
exception DivisionByZero;;
exception CannotApply of string;;
exception NotFunction of string;;
exception BadArgument of string*string;;
exception UnificationFails of string * string;;
exception FindingIdentifierFailed;;
exception PatternMatchingFails of string;;
exception Fail;;

(* Type related errors *)

exception TypesDoNotMatch of string * string;;

(* Cette fonction est appelée à la création de chaque noeud de l'AST elle renvoie un entier qui permet de récupérer les metadata associées au noeud dans la table de hash et ajoute dans ladite table
les infos que l'on souhaite conserver, pour l'instant on garde uniquement les informations de position du début de l'erreur et de la fin: Voir Lexing.position *)
let error_handler ()  =
  let data : Lexing.position * Lexing.position = (Parsing.symbol_start_pos ()), (Parsing.symbol_end_pos ())
  and node_id = (AstMetaData.length metadata) in
  AstMetaData.add metadata node_id data; node_id
;;


let getdata node_id : Lexing.position * Lexing.position = AstMetaData.find metadata node_id;;
let getcolumn (pos: Lexing.position) = pos.pos_cnum - pos.pos_bol;;

(* Cette fonction est appelée automatiquement dès qu'une erreur se produit dans le parcours de l'AST, elle prend en paramètre l'exception levée, il suffit de la modifier pour changer le comportement,
l'affichage des erreurs *)
let error_display node_id except =
  let (start_pos, end_pos): Lexing.position * Lexing.position = getdata node_id in
   pf "An error occured between line %d, character %d and line %d, character %d: \n" start_pos.pos_lnum (getcolumn start_pos) end_pos.pos_lnum (getcolumn end_pos);
   (match except with
   | UnknownIdentifier x -> pf "Identifier %s is unknown in this scope \n" x
   | UnknownReference x -> pf "Reference %s is unknown in this scope \n" x
   | DivisionByZero |Division_by_zero -> pf "Division by zero \n"
   | CannotApply s -> pf "%s is not appliable \n" s
   | NotFunction s -> pf "%s is not a function \n" s
   | PatternMatchingFails s -> pf "Pattern Matching failed: %s" s
   | Invalid_argument s -> pf "Bad operation argument: %s\n" s
   | UnificationFails (se, sv) -> pf "Unable to unify %s with %s\n" se sv
   | BadArgument (s1,s2) -> pf "%s is a bad argument for %s \n" s1 s2
   | x -> raise x)

   ;
   exit 0;;
