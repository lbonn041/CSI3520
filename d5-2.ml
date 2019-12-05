(*** CSI 3520 Devoir 5 ***)
(*** Luc-Cyril Bonnet ***)
(*** 8234136 ***)
(*** 4.07.0 ***)
(* Si vous utilisez la version disponible sur les machines de laboratoire via VCL, le
   la version est 4.05.0 ***)

(**************)
(* PROBLÈME 1 *)
(**************)

(* Consid\'erez l'expression OCaml suivante. *)

let _ =
  let x = 5 in
  let f y =
    (let z = 7 in
     let g w = w + x + y + z in
     g) in
  let h = f 3 in
  h 2

(* Dessinez le tas d'activation pour l'exécution de ce code.  (Il
   s'agit ici d'un "tas" parce qu'une fonction est renvoyée d'une
   portée imbriquée, et le bloc d'activation de la fonction qui
   renvoie la fonction ne peut pas être dépilé.)  Pour cette question,
   supposons qu'aucun bloc d'activation ne sera dépilé et qu'ils
   seront récupérés ultérieurement par un ramasse-miettes.  Dans vos
   blocs d'activation, incluez les liens dynamiques, des liens
   statiques, des variables locales, des paramètres et l'adresse de
   valeur de retour dans les blocs d'activation pour les appels à
   f. De plus, dans les blocs d'activation pour les appels à h,
   incluez un résultat intermédiaire pour la valeur de x+y+z. *)


(**************)
(* PROBLÈME 2 *)
(**************)

(* Vous trouverez ci-dessous une version récursive terminale de la
   fonction factorielle, semblable à celle du manuel de cours et des
   notes de cours. *)

let tlfact n =
  let rec aux n a =
    if n<=1 then a else aux (n-1) (n*a) in
  aux n 1

(* Ecrivez une version récursive terminale d'une fonction qui prend
   comme argument une liste d'entiers et renvoie la somme de tous les
   éléments de la liste d'entrée. Vous pouvez utiliser le même style
   que tlfact. *)

let rec tlsum xs = 
  match xs with
  | [] -> 0
  | hd::tl -> hd + tlsum tl 

let prob2 = tlsum [1;10;100]


(**************)
(* PROBLÈME 3 *)
(**************)

(* Problème 3(a) *)
(* Ecrivez une fonction qui trouve le nombre maximum dans une liste
   d’entiers positifs. Cette fonction devrait lever une exception dans
   deux cas distincts. Dans le cas où la liste est vide, levez une
   exception qui ne prend aucun argument. Dans le cas où la liste
   contient au moins un nombre inférieur ou égal à 0, levez une
   exception qui prend un argument. Les données renvoyées quand cette
   exception est levée doivent correspondre à la somme des nombres
   dans la liste d'entrée.  Vous pouvez appeler votre fonction défini
   pour la question précédente pour calculer la somme.  Quelques tests
   défini ci-dessous illustrent le comportement requis de cette
   fonction. *)

exception EmptyList
exception NegativeInList of int

let sum_neg xs =
let rec aux ctr lst =
  match lst with
  | hd::tl -> if hd < 0 then aux (ctr+hd) tl
              else aux ctr tl
  | [] -> ctr
in aux 0 xs

let find_max xs =
match xs with
| [] -> raise EmptyList
| _ -> let rec aux current lst =
        match lst with
        | hd::tl -> if hd < 0 then raise (NegativeInList (sum_neg xs))
                    else if hd > current then aux hd tl
                    else aux current tl
        | [] -> current
in aux 0 xs


(*
let test3a1 = find_max [2;4;93;8;6;90]
   (*val test3a1 : int = 93*)
let test3a2 = find_max [2;4;-93;-8;6;-90]
   (*Exception: NegativeInList (-179).*)
let test3a3 = find_max []
   (*Exception: EmptyList.*)
*)



(* Problème 3(b) *)
(* Définissez une fonction qui appelle votre solution de la question
   3(a) et renvoie une chaîne de caractères. (Remplacez la chaîne vide
   dans la définition ci-dessous avec une définition de fonction
   correcte. Elle doit gérer toutes les exceptions. Les exemples
   ci-dessous illustrent quelle chaîne doit être renvoyée dans chaque
   cas (y compris tous les cas dans lesquels une exception est levée
   ou non). *)

let try_find_max xs = 
try string_of_int (find_max xs)
with
| EmptyList -> "Pas de maximum. La liste est vide."
| NegativeInList x -> "La liste contient un nombre négatif; la somme est " ^ string_of_int x

let test3b1 = try_find_max [2;4;93;8;6;90]
(* val test3b1 : string = "93" *)
let test3b2 = try_find_max [2;4;-93;-8;6;-90]
(* val test3b2 : string = "La liste contient un nombre négatif; la somme est -179" *)
let test3b3 = try_find_max []
(* val test3b3 : string = "Pas de maximum. La liste est vide." *)


(**************)
(* PROBLÈME 4 *)
(**************)

(* Le code ci-dessous apparaît dans dans le manuel de cours et dans
   les notes de cours. *)

type 'a delay =
  | EV of 'a
  | UN of (unit -> 'a)

let ev (d:'a delay) =
  match d with
  | EV x -> x
  | UN f -> f()

let force (d:'a delay ref) =
  let v = ev !d in
  (d := EV v; v)

(* Problème 4(a) *)
(* Implémentez une version de la fonction f4 ci-dessous qui utilise le
   style de passage de paramétres appelé "passage par besion" «
   call-by-need ».  Les 3 arguments doivent être du type "int delay ref"
   au lieu du type "int".  Les arguments ne doivent être évalués
   que s'ils sont nécessaires au calcul. Ils ne doivent être évalués
   qu'une fois, puis stockés. *)

let f4 x y z =
  if z < 0 then x+x else y*y

let f4_cbn (x: int delay ref) (y: int delay ref) (z: int delay ref) : int =
  let xx = ref (UN (fun () -> ev !x + ev !x)) in
  let yy = ref (UN (fun () -> ev !y * ev !y)) in
  if ev !z < 0 then force xx else force yy

(* Problème 4(b) *)
(* En utilisant la fonction "fib" ci-dessous, appelez votre fonction "f4_cbn"
   de la question 4(b) deux fois, en utilisant les arguments x=(fib 10)
   et y=(fib 40). Dans le premier appel, utilisez z=1 et dans le
   deuxième appel, utilisez z=(-1).  Expliquez pourquoi l’un des appels
   est plus lent que l’autre.  Expliquez pourquoi les deux appels sont
   plus rapides que les appels à "f4" ci-dessous. *)

let rec fib (n:int) =
  if n=0 || n=1 then 1 else fib (n-1) + fib (n-2)

let f4_1_new = f4_cbn (ref (UN (fun() -> fib(10)))) (ref (UN (fun() -> fib(40)))) (ref (EV (1)) )
let f4_2_new = f4_cbn (ref (UN (fun() -> fib(10)))) (ref (UN (fun() -> fib(40)))) (ref (EV (-1)) )



let f4_1 = f4 (fib 10) (fib 40) 1
let f4_2 = f4 (fib 10) (fib 40) (-1)

(* 
L'appel que a 1 comme valeur de z est plus lent puisqu'il faut qu'il apelle la fonction fib 40 qui prends plus de temps que fib 10

Les deux appels sont plus rapides puisque la fonction f4 evalue les deux parametres 
mais f4_cbn evalue seulement les parametres qui sont necessaires. Cela nous empeche 
donc de calculer les deux appels a fib mais d'en calculer juste un et retourner le resultat.
*)