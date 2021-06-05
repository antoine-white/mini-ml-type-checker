(*
Blanco Antoine
Ziar Mehdi
*)

(* Question 1 *)

type type_primitif =
  |Int
  |Bool
  |Char
;;

type t_type =
  |TFun of t_type * t_type
  |TPaire of t_type * t_type
  |TPrim of type_primitif
;;

type exp_deco =
  |Var_d of string
  |Const_d of string
  |Ope_d of string 
  |Fun_d of string * exp_deco * t_type
  |App_d  of (  exp_deco) * (  exp_deco)  
  |Paire_d of (  exp_deco) * (  exp_deco)
  |Dec_d of string * t_type *  exp_deco *   exp_deco 
;;

(* question 2*)
(*transforme un t_type en chaine de caractères*)
let rec t_typeToStr t =
  match t with
  |TFun(t1,t2) -> t_typeToStr(t1) ^ " -> " ^ t_typeToStr(t2)
  |TPaire(t1,t2) -> "(" ^ t_typeToStr(t1) ^ "," ^ t_typeToStr(t2) ^ ")"
  |TPrim(p) ->
    match p with
    |Int -> "int"
    |Bool -> "bool"
    |Char -> "char"
;;

(*transforme un exp_deco en chaine de caractères*)
let rec exp_dToStr exp =
  match exp with
  |Var_d(s) -> " " ^ s ^ " "
  |Ope_d(s) -> s
  |Fun_d(s,e,t) -> "fun " ^ s ^ " : " ^ t_typeToStr(t)  ^ " -> " ^ (exp_dToStr(e))
  |App_d(a,b) -> exp_dToStr(a) ^ " " ^ exp_dToStr(b)
  |Paire_d(a,b) -> "(" ^exp_dToStr(a) ^ "," ^ exp_dToStr(b) ^ ")"
  |Dec_d(s,t,a1,a2) -> "let " ^ s ^ " : " ^ t_typeToStr(t) ^ " = " ^ exp_dToStr(a1) ^ " in " ^ exp_dToStr(a2)
  |Const_d(v) -> " " ^ v ^ " "
;;

(* exemples *)
exp_dToStr (Dec_d("y",TPrim Int,Const_d("1"),Const_d("true")));;
(* donne "let y : int =  1  in  true"*)

exp_dToStr (Fun_d("x",
    Dec_d("succ",TFun(TPrim Int,TPrim Int),
      Fun_d("x",App_d(Ope_d("+"),Paire_d(Var_d("x"),Const_d("1"))),TPrim Int),
      Paire_d(Var_d("x"),App_d(Var_d("succ"),Const_d("1")))
    ),TPrim Char));; 
(* donne "fun x : char -> let succ : int -> int = fun x : int -> + ( x , 1 ) in ( x , succ   1 )" *)

(* question 3 *)

(* ensemble des constantes*)
let _Ec = [
  ("1",TPrim Int);
  ("2",TPrim Int);
  ("3",TPrim Int);
  ("4",TPrim Int);
  ("5",TPrim Int);
  ("true",TPrim Bool);
  ("false",TPrim Bool);
  ("'a'",TPrim Char);
  ("'b'",TPrim Char);
  ("'c'",TPrim Char);
  ("'d'",TPrim Char);
];;

(* ensemble des opérations prédéfinies*)
let _Op = [
  ("+",TFun((TPaire(TPrim Int,TPrim Int)),TPrim Int));
  ("-",TFun((TPaire(TPrim Int,TPrim Int)),TPrim Int));
  ("<",TFun((TPaire(TPrim Int,TPrim Int)),TPrim Bool));
  (">",TFun((TPaire(TPrim Int,TPrim Int)),TPrim Bool));
  ("if_then_else",TFun((TPaire(TPrim Bool,TPaire(TPrim Int, TPrim Int))),TPrim Int))
];;

(* question 4 *)

let rec validate_exp_d exp env =
  match exp with
  |Const_d(const) -> if List.mem_assoc const _Ec then ((List.assoc const _Ec),env) else failwith ("Type de la constante " ^ const  ^ " inconnu")
  |Ope_d(ope) -> if List.mem_assoc ope _Op then ((List.assoc ope _Op),env) else failwith ("Operateur inconnu : " ^ ope)
  |Var_d(id) -> if List.mem_assoc id env then ((List.assoc id env),env) else failwith ("variable inconnue; identifiant : " ^ id)      
  |Fun_d(param,e,t_param) -> let (t1,env2) = validate_exp_d e ((param,t_param)::env) in ((TFun(t_param,t1)),env2)
  |Paire_d(a,b) -> let ((t1,_),(t2,_)) = (validate_exp_d a env,validate_exp_d b env) in (TPaire(t1,t2),env)
  |Dec_d(s,t,a1,a2) -> 
    let (t1,_) = (validate_exp_d a1 env) in 
      if t1 = t then (validate_exp_d a2 ((s,t)::env))
      else failwith ("Type attendu " ^ t_typeToStr(t) ^ " pour la variable " ^ s ^ " mais le type de l'expression est " ^ t_typeToStr(t1))
  |App_d(a,b) ->
    let ((t1,_),(t2,_)) = ((validate_exp_d a env),(validate_exp_d b env)) in 
    match t1 with
    |TFun(t_param,t_res) -> if t_param = t2 then (t_res,env) else failwith "on en peut faire l'application"
    |_ -> failwith "le premier membre de l'application doit être de type TFunc"
;;

(* question 5*)

(*expTest1 correspond à fun x : char -> let succ : int -> int = fun x : int -> + (x, 1) in (succ 1, x)*)
let expTest1 = 
  Fun_d("x",
    Dec_d("succ",TFun(TPrim Int,TPrim Int),
      Fun_d("x",App_d(Ope_d("+"),Paire_d(Var_d("x"),Const_d("1"))),TPrim Int),
      Paire_d(Var_d("x"),App_d(Var_d("succ"),Const_d("1")))
    ),TPrim Char)  
;;

validate_exp_d (expTest1) [];; (* TFun (TPrim Char, TPaire (TPrim Char, TPrim Int))*)

(*expTest2 correspond à fun x : char -> let succ : int -> int = fun y : int -> + (y, 1) in (succ 1, x)*)
let expTest2 = 
  Fun_d("x",
    Dec_d("succ",TFun(TPrim Int,TPrim Int),
      Fun_d("y",App_d(Ope_d("+"),Paire_d(Var_d("y"),Const_d("1"))),TPrim Int),
      Paire_d(Var_d("x"),App_d(Var_d("succ"),Const_d("1")))
    ),TPrim Char)  
;;
validate_exp_d (expTest2) [];; (* TFun (TPrim Char, TPaire (TPrim Char, TPrim Int))*)