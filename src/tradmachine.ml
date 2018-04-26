open Expr;;
open Env;;
open Errmgr;;


type instruction =
  | C of int
  | Add
  | Mul
  | Sub
  | Div
  | Let of string
  | Access of string
  | Endlet

type code = instruction list

type environnement = (string * int) list

type pile = int list


let rec compile ee =
  (*prend un arbre de fouine pur et renvoie le code associé*)
  let (node_id, e) = ee in
  try matche e with
  | Const k -> [C k]
  | Add(e1,e2) -> (compile e2)@(compile e1)@[Add]
  | Mul(e1,e2) -> (compile e2)@(compile e1)@[Mul]
  | Sub(e1,e2) -> (compile e2)@(compile e1)@[Sub]
  | Div(e1,e2) -> (compile e2)@(compile e1)@[Div]
  | Let((patt,e1),e2) -> (compile e1)@[Let patt]@(compile e2)@[Endlet]
  | Identifier x -> [Access x]
                
                
         
with x -> error_display node_id x ; raise Fail
;;
