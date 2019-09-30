(*** CSI 3520 Devoir 1 ***)
(*** LUC-CYRIL BONNET ***)
(*** 8234136 ***)
(*** OCaml version 4.07.0 ***)
(* Si vous utilisez la version disponible sur les machines de
   laboratoire via VCL, la version est 4.05.0 *)

(* Vous n'avez pas à comprendre la définition de la fonction
   "undefined" ci-dessous. Une partie de votre travail consiste à
   remplacer tous les appels aux fonctions dans le code ci-dessous
   avec les réponses exigées. *)

let undefined : unit -> 'a = fun () -> failwith "undefined"

(* 1. Veuillez définir ces variables avec les valeurs appropriées.
   Vérifiéz que vos définitions passe le contrôle du type.  Vous
   pouvez le faire en copiant et en collant vos définitions dans la
   fenêtre shell qui exécute OCaml, ou en remplissant le code et en
   chargeant le fichier en utilisant #use, comme indiqué en classe et
   au laboratoire, ou en utilisant une plugin installé dans votre
   éditeur, par exemple, Ctrl+c puis Ctrl+e en Emacs en mode Touareg
   *)
                                     
(* 1a. Créez une chaîne avec votre prénom *)
let name : string = "Luc-Cyril"

(* 1b. En utilisant la chaîne de la question 1.a, appliquez un
   opérateur de chaîne pour créer une nouvelle chaîne qui contient
   votre prénom et votre nom. *)
let fullname : string = name ^ " Bonnet"

(* 1c. Créez une chaîne contenant votre adresse e-mail *)
let email : string = "lbonn041@uottawa.ca"

(* 1d. Remplacer (Other "...") dans class_year avec l'élément approprié
   ci-dessous *)
(* ie: remplacez (Other "...") par SecondYear ou ThirdYear  *)
type year = FirstYear | SecondYear | ThirdYear | FourthYear | Other of string

let class_year : year = FourthYear

(* 1e. Remplacez le .... par quelque chose que vous espérez apprendre
   dans ce cours *)
let learning : string = "I hope to learn OCaml and lambda calcul"

let print = Printf.printf

let print_survey () = 
  let string_year = 
    (match class_year with
       | FirstYear -> "2022"
       | SecondYear -> "2021"
       | ThirdYear -> "2020"
       | FourthYear -> "2019"
       | Other s -> "Other: " ^ s
    ) in
    (print "----------------------------------------\n";
     print "Name: %s\n" name;
     print "Email: %s\n" email;
     print "Year: %s\n" string_year; 
     print "%s\n" learning;
     print "----------------------------------------\n\n")

(* Tapez "print_survey ()" à l'interpréteur OCaml pour tester votre code ci-dessus. *)


(* Problème 2 - Remplissez les types: *)
(* Remplacez chaque ??? avec le type approprié de chaque
   expression. Veillez supprimer les commentaires de chaque
   sous-problème pour vérifiéz que vos définitions passe le
   contrôle du type avant la soumission. *)
(* Notez que les expressions peuvent ne rien faire d’utile - et en
   fait pourrait avoir des bugs intéressants! - mais tout ce que vous
   devriez faire est de remplacer le ??? pour vérifier les types. *)

(* Problème 2a. *)
(*
let prob2a : int  = let add3 y = 3 + y in add3 7
*)

(* Problème 2b. *)
(*
let prob2b : char = char_of_int((int_of_char 'b') + 1)
*)

(* Problème 2c. *)
(*
let rec prob2c (x : double) : bool =
  prob2c (if (x > 3.0) then prob2c x else 4.0);;
*)

(* Problème 2d. *)
(* 
let rec prob2d (x : bool) (y: int) : bool =
  let z = y > 3 in
  if x then prob2d (not x) y else z
*)


(* Problème 3 - Expliquez pourquoi chacun des 3a, 3b, 3c ne compilera
   pas (utilisez les chaînes exp3a, exp3b et exp3c pour vos réponses)
   et changez le code pour qu'il puisse être compiler, et enlevez les
   commentaires.  Ne changez pas les types donnés ci-dessous. *)

(* Problème 3a. *)

let prob3a : float = 
  let add3a x y = x +. y in 
  add3a 3.9 4.0


let exp3a : string = "On ne peut pas faire une operation arithmetique sur les float entre un float (3.9) et un int (4). la fonction doit prendre comme entree deux floats et pas un float et int. Pour compiler, il faut changer le 4 a un 4.0"

(* Problème 3b. *)
let prob3b : int = 
  let rec mult x y =
    if x <= 0 then 0 
    else y + (mult (x-1) y)
  in
  mult 3 10

let exp3b : string = "3b est une fonction recursive donc il faut ajouter 'rec' apres le 'let'"

(* Problème 3c. Astuce: consultez la section "Core Expression Syntax"
   dans les notes de cours (la dernière section de la première série
   de notes sur OCaml). *)

let prob3c : int = 
  let add_from_n_to_m n m =
   let rec aux x y =
    if x > m then y 
    else aux (x+1) (y+x)
   in
   aux n 0
  in
  add_from_n_to_m 5 10


let exp3c : string = "la fonction aux fait appel a des expressions arithmethique donc doit etre mis en parentheres"


(* Problème 4 *)
(* Considérez le programme incomplet suivant *)
(*
let g (a:int) (b:int) : int = 

let rec problem4 (x: (int*char) ) (y:int) : float = 
  let (v,w) = x in
  let w' = int_of_char w in
  (problem4 (float_of_int y, w) (g v w')) +. 2.1

 *)

(* Problème 4a *)
(* Remplacez chaque ?? ci-dessus par le type de l'expression
   correspondante, et définissez une fonction g qui contient les types
   corrects. *)

(* Problème 4b *)
(* Réécrivez la fonction problem4 afin que g soit une fonction
   locale. (Définissez g_local dans problem4c en utilisant "let..in") *)

(* Problème 4c *)
(* Expliquez dans exp4 un problème qui reste dans la fonction
   problem4b. *)
  
let exp4c : string = ""


(* Problème 5 *)

(* Implémentez l’algorithme suivant pour le calcul des puissances des
   nombres (des entier non négatif) très grands.  Cet algorithme est
   plus efficace que l'algorithme qui simplement multiplie le nombre
   par lui-même le nombre de fois requis.  Pour calculer x à la
   puissance k, on procède comme suit:

    - Si k = 0, le résultat est 1.
    - Si k est pair, le résultat est x*x à la puissance de k/2.
    - Sinon, soit n la valeur de x à la puissance k−1. Le résultat est x * n. 

   Incluez au moins 3 tests. (Appelez votre fonction au moins 3
   fois avec des valeurs différentes.)  *)

(* Votre algorithme ne doit fonctionner que pour les valeurs 0 et
   supérieures. Votre function devrait appeler (bad_inputs x k) si x<0
   ou k<0.  La définition de l'exception suivante est utilisée par
   bad_inputs, mais il n'est pas nécessaire de le comprendre. *)


exception BadInputs of int * int
let bad_inputs x k = raise (BadInputs (x,k))

let prob5 : int =
  let rec pow x k =
    if k < 0 then 
      bad_inputs x k 
    else if x < 0 then
      bad_inputs x k
    else if k = 0 then
      1
    else if k mod 2 = 0 then
      pow (x*x) (k/2)
    else
      x * pow x (k-1);
  in 
  pow 3 3;
  pow 10 45;
  pow 33 10;