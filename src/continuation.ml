open Affichage;;
open Expr;;
open Env;;
open Errmgr;;
open Constructeur;;

  (*de même que pour traduction, on va avoir besoin de plein de k et de kE*)

let  nbk =ref 0;;
let  nbkE =ref 0;;

(*fonction qui renvoie un sn où n est un numéro par encore utilisé*)
let newk () =
  nbk := !nbk + 1;
 (0,Identifier ("k" ^ (string_of_int !nbk)));;

let newkE () =
  nbkE := !nbkE + 1;
 (0,Identifier ("kE" ^ (string_of_int !nbkE)));;


  let rec trad_expr ee =
  let (node_id, e) = ee in

  try match e with
      (*cas de brique élémentaires, juste les donner à k: fun k kE -> k 52*)
      | Const x -> let k = newk() in let kE = newkE() in
                                     mkFun k (mkFun kE (mkApp k mkConst x))
      | Identifier x ->  let k = newk() in let kE = newkE() in
                                     mkFun k (mkFun kE (mkApp k mkIdentifier x))


   with x -> error_display node_id x; raise Fail

;;
