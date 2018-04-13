open Affichage;;
open Expr;;
open Env;;
open Errmgr;;
open Constucteur;

(*deux références globales qui permettront d'éviter les conflits dans la traduction*)
let ref nbv =0;;
let ref nbs =0;; 

(*des méta-constructeurs qui évitent un code de traduction illisiblesont dans constructeurs*)

  
let rec trad_expr ee =
  let (node_id, e) = ee in

  try match e with
      | Add(e1,e2) -> let  
  with x -> error_display node_id x; raise Fail
