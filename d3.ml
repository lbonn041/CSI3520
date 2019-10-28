(*** CSI 3520 Devoir 3 ***)
(*** Luc-Cyril Bonnet ***)
(*** 8234136 ***)
(*** OCaml version 4.07.0 ***)
(* Si vous utilisez la version disponible sur les machines de laboratoire via VCL, le
   la version est 4.05.0 ***)

(*************)
(* PROBLÈME 1 *)
(*************)

(* Problème 1a: Vous trouverez ci-dessous la définition du type de
   données pour la logique propositionnelle du Lab 4, qui comprend
   connecteurs pour la conjonction, (l'opérateur AND / \), disjonction
   (l'opérateur OR \ /) et implication logique (=>).
 *)

type prop = string

type form =
  | True
  | False
  | Prop of prop 
  | And of form * form
  | Or of form * form
  | Imp of form * form

(* Définissez une fonction "count_atoms" qui prend un argument du type
   "form" en entrée et compte le nombre d'atomes. Les atomes incluent
   "True", "False" et "Prop". Par exemple, la formule ci-dessous
   contient 6 atomes, qui incluent 2 occurrences de p, 1 occurrence de
   q, 2 occurrences de r et 1 occurrence de False. (Notez que chaque
   occurrence d'un atome, même si elle se répète, compte pour 1
   atome.)

   ((p \/ q) /\ (False \/ p \/ r)) -> r *)

let form3a : form = Imp(And(Prop "p",Prop "q"),Or(Prop "r",Prop "s"))

let rec count_atoms (f:form) : int =
   let rec func p ctr =
      match p with
      | Imp(x,y) -> ctr + func x ctr + func y ctr
      | And(x,y) -> ctr + func x ctr + func y ctr
      | Or(x,y) -> ctr + func x ctr + func y ctr
      | True -> 1
      | False -> 1
      | Prop _ -> 1
   in 
      func f 0;

(* Problème 1b: Considérez les nouveaux types form' et env ci-dessous. *)

type form' =
  | True'
  | False'
  | Prop' of prop * bool
  | And' of form' * form'
  | Or' of form' * form'
  | Imp' of form' * form'

type env = (prop * bool) list

(* Le type "form'" est similaire à "form", sauf que le constructeur de
   variables propositionnelles "Prop'" inclut un argument booléen
   supplémentaire pour indiquer la valeur de vérité de la
   proposition. Le type "env" est une liste de variables
   propositionnelles et leurs valeurs de vérité. Définissez une
   fonction "get_env" qui prend une proposition en entrée et renvoie
   un "env".  Cett fonction doit simplement extraire les informations
   des variables propositionnelles d'une formule et renvoie ces
   informations sous la forme d'une liste. Par exemple, si l'entrée
   est la formule

  (((p,true) \/ (q,false)) /\ (False \/ (p,false) \/ (r,true))) -> (r,true)

   la sortie devrait être une liste contenant 5 paires. Ne supprimez pas
   les doublons et ne vous inquiétez pas des incohérences (la même
   variable propositionnelle associée à "true" et à "false").
 *)
         
let rec get_env (f:form') : env = 
   match f with
   | True' -> []
   | False' -> []
   | Prop' (v,b) -> [(v,b)]
   | And'(x,y) -> get_env x @ get_env y
   | Or'(x,y) -> get_env x @ get_env y
   | Imp'(x,y) -> get_env x @ get_env y


(* Problème 1c: Définissez une fonction "simplify_env" qui prend un
   "env" en entrée et renvoie un "option". Le résultat doit être None
   si l’entrée est incohérente (c’est-à-dire qu’il existe au moins une
   variable propositionnelle couplée à la fois à "true" et à
   "false"). Sinon, la fonction devrait retourner la liste avec les
   doublons supprimés. Par exemple, considérons les 2 formules
   ci-dessous:

   (((p,true) \/ (q,false)) /\ (False \/ (p,false) \/ (r,true))) -> (r,true)
   (((p,true) \/ (q,false)) /\ (False \/ (p,true) \/ (r,true))) -> (r,true)

   La fonction doit renvoyer None pour la première formule en raison
   des occurrences de (p,true) et (p,false). Pour la deuxième formule,
   la fonction doit renvoyer une liste de longueur 3 (comme argument à
   Some), où les éléments incluent (p,true), (q,false) et
   (r,true). Astuce: vous aurez probablement besoin de fonctions
   auxiliaires. Vous pouvez choisir de les définir en tant que
   fonctions locales dans la fonction principale, mais ce n'est pas
   obligatoire.  *)





(*************)
(* PROBLÈME 2 *)
(*************)

(* Vous trouverez ci-dessous la signature d'un module pour une version
   fonctionnelle d'une structure de données de file d'attente dans
   laquelle tous les éléments de la file d'attente sont des
   chaînes. Dans une file d'attente, les premières éléments arrivées
   sont les premières à sortir de la file (PEPS en français, pour
   premier entré/premier sorti, FIFO en anglais pour First in, first
   out). En d'autres termes, une fois qu'un nouvel élément est ajouté,
   tous les éléments ajoutés auparavant doivent sortir de la file
   d'attente avant que le nouvel élément puisse sortir. En lisant les
   types et les commentaires, vous verrez les différences entre les
   piles, telles qu'étudiées en classe, et les files d'attente. *)

module type StringQueue =
  sig
    (* t est une file d'attente dont tous les éléments ont le type
       string. *)
    type t

    (* Créer une file d'attente vide. *)
    val empty : unit -> t

    (* Pour vérifier si une file d'attente est vide. *)
    val is_empty : t -> bool

    (* [enqueue x q] est la file d'attente [q] après avoir ajouté [x]
       à la fin. *)
    val enqueue : string -> t -> t

    (* [peek q] est [Some x], où [x] est l'élément situé au début de
       la file d'attente ou [None] si la file d'attente est vide. *)
    val peek : t -> string option

    (* [q dequeue q] est [Some q'], où [q'] est la file d'attente
       contenant tous les éléments de [q] à l'exception du début de
       [q], ou [None] si [q] est vide. *)
    val dequeue : t -> t option
end
         

(* Problème 2a: Implémentez une file d'attente (définissez un module
   appelé ReverseListStringQueue contenant une structure dont le type
   est StringQueue). Représentez vos files d'attente sous forme de
   listes, dont les éléments sont dans l'ordre inverse, ce qui
   signifie que le premier élément de la liste est le dernier qui a
   été placé dans la file d'attente et que le dernier élément de la
   liste est le début de la file d'attente. En d'autres termes, vous
   devez représenter une file d'attente sous forme de liste, où la
   liste [sn; ...; s2; s1] représente la file d'attente avec [s1] à
   l'avant, suivi de [s2], ..., suivi de [sn]. Vous êtes bien entendu
   autorisé à implémenter des fonctions auxiliaires dans la structure
   qui n'apparaissent pas dans la signature ci-dessus.  *)

module ReverseListStringQueue : StringQueue =
   struct
      type t = string list
      let empty () : t = []
          
      let is_empty (lst:t) : bool = 
         match lst with
         | [] -> true
         | _::_ -> false

      let enqueue (s:string) (lst:t) : t =
         lst @ [s]


      let peek (lst:t) : string option =
         let l = List.rev lst in
            match l with
            | [] -> None
            | hd::tl -> Some hd

      (* [q dequeue q] est [Some q'], où [q'] est la file d'attente
         contenant tous les éléments de [q] à l'exception du début de
         [q], ou [None] si [q] est vide. *)
      let dequeue (lst:t) : t option =
         let l = List.rev lst in
            match l with
            | [] -> None
            | hd::tl -> Some (List.rev tl)
   end



let q0 = ReverseListStringQueue.empty ()
let q1 = ReverseListStringQueue.enqueue "hello" q0
let q2 = ReverseListStringQueue.enqueue "bonjour" q1
let  i = ReverseListStringQueue.peek q2
let  j = let rest = ReverseListStringQueue.dequeue q2 in
         match rest with
         | Some q -> ReverseListStringQueue.peek q
         | None -> None



(* Problème 2b: Modifiez la signature StringQueue ci-dessus afin
   qu'elle soit polymorphe dans le sens où elle peut être utilisée
   pour créer des files d'attente contenant des éléments de tout
   type, pas uniquement des chaînes. (Cela devrait être
   relativement facile.)  *)

module type Queue = 
   sig
    (* t est une file d'attente dont tous les éléments ont le type
       string. *)
    type 'a queue

    (* Créer une file d'attente vide. *)
    val empty : unit -> 'a queue

    (* Pour vérifier si une file d'attente est vide. *)
    val is_empty : 'a queue -> bool

    (* [enqueue x q] est la file d'attente [q] après avoir ajouté [x]
       à la fin. *)
    val enqueue : 'a -> 'a queue -> 'a queue

    (* [peek q] est [Some x], où [x] est l'élément situé au début de
       la file d'attente ou [None] si la file d'attente est vide. *)
    val peek : 'a queue -> 'a option

    (* [q dequeue q] est [Some q'], où [q'] est la file d'attente
       contenant tous les éléments de [q] à l'exception du début de
       [q], ou [None] si [q] est vide. *)
    val dequeue : 'a queue -> 'a queue option
end


(* Problème 2c: Modifiez votre solution à la question 1a en
   introduisant une nouvelle structure appelée ReverseListQueue qui a
   le type Queue afin qu’elle puisse être utilisée pour créer des
   files d’éléments de tout type. (Cela devrait aussi être
   relativement facile.)  *)

module ReverseListQueue : Queue =
   struct
      type 'a queue = 'a list
      let empty () : 'a queue = []
          
      let is_empty (lst:'a queue) : bool = 
         match lst with
         | [] -> true
         | _::_ -> false

      let enqueue (s:'a) (lst:'a queue) : 'a queue =
         lst @ [s]


      let peek (lst:'a queue) : 'a option =
         let l = List.rev lst in
            match l with
            | [] -> None
            | hd::tl -> Some hd

      (* [q dequeue q] est [Some q'], où [q'] est la file d'attente
         contenant tous les éléments de [q] à l'exception du début de
         [q], ou [None] si [q] est vide. *)
      let dequeue (lst:'a queue) : 'a queue option =
         let l = List.rev lst in
            match l with
            | [] -> None
            | hd::tl -> Some (List.rev tl)
   end

let q3 = ReverseListQueue.empty ()
let q4 = ReverseListQueue.enqueue "hello" q3
let q5 = ReverseListQueue.enqueue "bonjour" q4
let i1 = ReverseListQueue.peek q5
let j1 = let rest = ReverseListQueue.dequeue q5 in
         match rest with
         | Some q -> ReverseListQueue.peek q
         | None -> None
let q6 = ReverseListQueue.empty ()
let q7 = ReverseListQueue.enqueue (3,"three") q6
let q8 = ReverseListQueue.enqueue (4,"four") q7
let i2 = ReverseListQueue.peek q8
let j2 = let rest = ReverseListQueue.dequeue q8 in
         match rest with
         | Some q -> ReverseListQueue.peek q
         | None -> None

 

(* Problème 2d: Remplissez tous les "..." dans la version modifiée
   ci-dessous de la signature StringQueue ci-dessus, de sorte qu'elle
   devienne la signature d'une version impérative d'une file d'attente
   de chaînes appelée ImpStringQueue. *)


module type ImpStringQueue =
  sig
    (* t est une file d'attente dont tous les éléments ont le type
       string. *)
    type t

    (* Créer une file d'attente vide. *)
    val empty : unit -> t ref

    (* Pour vérifier si une file d'attente est vide. *)
    val is_empty : t ref -> bool

    (* [enqueue x q] modifie [q] en ajoutant [x] à la fin. *)
    val enqueue : string -> t ref -> unit

    (* [dequeue q] renvoie [Some x], où [x] est l’élément situé au
       début de la file, ou [Aucun] si la file est vide. Il modifie
       également [q] en supprimant l’élément au début, si il existe un
       tel élément, sinon aucune modification n’est effectuée. *)
    val dequeue : t ref -> string option
end
 

    
(* Problème 2e: Modifiez votre solution à la question 1a (en utilisant
   à nouveau des listes inversées) en introduisant une nouvelle
   structure appelée ImpListStringQueue de type ImpStringQueue afin
   qu’elle implémente une file d’attente impérative. *)

module ImpListStringQueue : ImpStringQueue = 
   struct
      type t = string list

      let empty () : t ref = ref []
          
      let is_empty (lst:t ref) : bool = 
         match !lst with
         | [] -> true
         | _::_ -> false

      let enqueue (s:string) (lst:t ref) : unit =
         lst :=  !lst @ [s]

      let dequeue (lst:t ref) : string option =
         let l = List.rev !lst in
            match l with
            | [] -> None
            | hd::tl -> (lst:= List.rev tl; Some hd)
   end

let q' = ImpListStringQueue.empty ()
let _  = ImpListStringQueue.enqueue "hello" q'
let _  = ImpListStringQueue.enqueue "bonjour" q'
let  i = ImpListStringQueue.dequeue q'
let  j = ImpListStringQueue.dequeue q'
let  k = ImpListStringQueue.dequeue q'

