(*** CSI 3520 Devoir 2 ***)
(*** Luc-Cyril Bonnet ***)
(*** 8234136 ***)
(*** OCaml version 4.07.0 ***)
(* Si vous utilisez la version disponible sur les machines de laboratoire via VCL, le
   la version est 4.05.0 ***)

(**************)
(* PROBLÈME 1 *)
(**************)

(* Pour chaque partie du problème 1, remplissez la chaîne en
   expliquant pourquoi le code n'est pas bien typé.  Suivez ensuite
   les instructions et modifiez le code ou le type afin qu'il passe le
   contrôle de type. Décommentez l'expression pour la soumission. *)

(* Problème 1a - Donnez votre explication dans exp1a, puis corrigez le
   côté droit de la la déclaration "let" pour qu'il corresponde au
   type indiqué. (Ne changez pas le côté gauche.)  *)


let exp1a : string = "Il faur une liste de type (int * string * char) et non une liste qui contient ine int, in strnig et un char"
let prob1a : (int * string * char) list = [(1, "2", '3')];;


(* Problème 1b - Donnez votre explication dans exp1b, puis corrigez le
   type de la variable prob1b pour qu'il corresponde au type de
   l'expression sur le côté droit. (Ne changez pas le le côté droite.)
   *)


let exp1b : string = "Même idee que le probleme 1, la liste doit etre d'éléments du type (string * int) "
let prob1b : (string * int) list = [("aa",3);("bb",2);("cc",1)];;


(* Problème 1c -  - Donnez votre explication dans exp1a, puis corrigez le
   côté droit de la la déclaration "let" pour qu'il corresponde au
   type indiqué. (Ne changez pas le côté gauche.)  *)


let exp1c : string = "la concatenation a pour type a' -> a' list -> a' list  cela veut dire que l'element de gauche doit etre du type 'a. le bout de la liste doit etre aussi du tyle float list list"
let prob1c : float list list =  [2.0] :: [3.0] :: [4.0; 5.0; 6.2]  ::  [7.0; 8.0] :: [[9.2]] 


(**************)
(* PROBLÈME 2 *)
(**************)

(* Remplissez les expressions ci-dessous pour qu'elles soient bien
   typées:
 *
 * REMARQUE: pour les types "option" et "list" et aussi pour les types
 * des fonctions, vous devez fournir une réponse non triviale.
 * Cela signifie
 * que les listes doivent être non vides,
 * que les éléments d'un type d'option doivent utiliser "Some", et
 * que le corps des fonctions doit utiliser les arguments pour générer
   le résultat.
 * exemple de problèmes:
 *   let x : int option = ???
 *   let y : int list = ???
 *   let f (x: int) (y: int) : int = ???
 * réponses incorrectes:
 *   let x : int option = None
 *   let y : int list = []
 *   let f (x: int) (y: int) : int = 7
 * réponses correctes possibles:
 *   let x : int option = Some 1
 *   let y : int list = [1]
 *   let y : int list = [1; 2]
 *   let y : int list = 1 :: [2]
 *   let f (x: int) (y: int) : int = x + y
 *   let f (x: int) (y: int) : int = 
 *         String.length  ((string_of_int x) ^ (string_of_int y))
 *)

(* Problème 2a *)
  
let prob2a : (float * ((string * int) option list)) list = [1.0,  [Some ("text", 1)]]


(* Problème 2b *)
(* un étudiant est une paire de (nom, age option) *)

type student = string * int option


let prob2b : string * student list option = "one", Some [("Max", Some 20); ("Sam", Some 19) ]



(* Problème 2c *)
(* Complétez l'expression en faisant un appel bien typé
   à la fonction f. *)


let prob2c =
  let rec f arg =
    match arg with
    | (a, b) :: xs -> if a then (b + (f xs)) else f xs
    | _ -> 0
  in f [(true,9)]




(**************)
(* PROBLÈME 3 *)
(**************)

(* Problème 3a. Implémentez une fonction include_in qui prend en
   argument deux listes d'entiers et vérifie si tous les éléments de
   la première liste sont inclus dans la deuxième liste dans le même
   ordre. (Dans la deuxième liste, il peut y avoir d'autres entiers
   entre les éléments.) Par exemple:

   included_in [1;2;3] [4;1;6;5;2;3] = true
   included_in [1;2;3] [4;1;6;5;2] = false
   included_in [1;2;3] [4;1;6;3;5;2] = false

*)

let rec included_in (xs:int list) (ys:int list) : bool =
  let rec f lst1 lst2 =
    match (lst1, lst2) with
    | [],[] -> true
    | _,[] -> false
    | [], _ -> true
    | (hd1::tl1), (hd2::tl2) -> if hd1 = hd2 then f tl1 tl2
                                else f lst1 tl2
    in f xs ys
 

(* Problème 3b. Réécrivez la fonction ci-dessus pour qu'elle soit
   polymorphe, c’est-à-dire qu’elle devrait fonctionner sur des listes
   contenant des éléments de n'importe quel type.  Donnez au moins un
   test (appelez votre fonction au moins une fois) avec un type
   différent de "int". *)

let rec included_in_poly (xs:'a list) (ys:'a list) : bool =
  let rec f lst1 lst2 =
    match (lst1, lst2) with
    | [],[] -> true
    | _,[] -> false
    | [], _ -> true
    | (hd1::tl1), (hd2::tl2) -> if hd1 = hd2 then f tl1 tl2
                                else f lst1 tl2
    in f xs ys

(*************)
(* PROBLEM 4 *)
(*************)

(* Ecrivez une fonction qui convertit un entier en nombre naturel s'il
   en existe un. Utilisez le type "option" parce que toutes les entrées
   entières ne peuvent pas être converties. *)

type nat = Zero | Succ of nat

let rec convert (x:int) : nat option =
let rec f i =
 if i = 0 then Zero
  else Succ (f (i-1))
  in
  match x with 
    | 0 -> Some Zero
    | x -> if x<0 then None
            else Some (Succ (f (x-1)))
    