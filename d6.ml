(*** CSI 3520 Devoir 6 ***)
(*** Luc-Cyril Bonnet ***)
(*** 8234136 ***)
(*** Ocaml version 4.07.0 ***)
(* Si vous utilisez la version disponible sur les machines de laboratoire via VCL, la version est 4.05.0 ***)

(**************************************************)
(* PROBLÈME 1: Organisation « orientée fonction » *)
(**************************************************)

(* Considérez le code OCaml suivant. *)

type fBalance = float
type fInterestRate = float
type fTransactionCount = int
               
type bankAccount =
  | Bank of fBalance
  | Savings of fBalance * fInterestRate
  | Chequing of fBalance * fTransactionCount


(* Problème 1(a) *)
(* Implémentez une fonction qui prend comme argument une liste
   d'éléments de type bank_account et renvoie une liste de soldes de
   compte (fBalance). Votre fonction doit préserver l'ordre. Par
   exemple, le premier élément du résultat doit être le solde du
   premier compte dans l'argument d'entrée. *)

let get_fBalances l =
  let rec aux lst soldes =
    match lst with
    | [] -> soldes
    | hd::tl -> match hd with 
                | Bank x -> aux tl (soldes @ [x])
                | Savings (x, _) -> aux tl (soldes @ [x])
                | Chequing (x,_) -> aux tl (soldes @ [x])
    in aux l []


(* Problème 1(b) *)
(* Écrivez du code de test: Créez une liste qui contient 3 comptes
   bancaires (un de chaque type). Donnez la valeur 0 au nombre
   d'opérations pour le compte de chèques (fTransactionCount). Appelez
   votre fonction donnée comme solution au problème 1(a) à votre liste
   de comptes. *)

  let test = [Bank 20.0; Bank 30.0; Savings (10.0, 5.0); Chequing (50.0, 2)];;
  get_fBalances test;;


(********************************************)
(* PROBLÈME 2:  Organisation orientée objet *)
(********************************************)

(* Mettez votre code pour les problèmes 2(a) et 2(b) ici. *)

exception Not_enough_money of float;;

class bank_account = object
  val mutable balance : fBalance = 0.0

  method get_balance = balance

  method deposit (amount : float) =
    balance <- balance +. amount

  method withdraw (amount : float) =
    if amount <= balance then
      balance <- (balance -. amount)
    else
      raise (Not_enough_money balance)

  method to_bankAccount = Bank balance

end;;


class savings_account = object
  inherit bank_account as super
  val mutable interest_rate : fInterestRate = 0.2

  method get_interest_rate = interest_rate

  method set_interest_rate (new_rate : float) = interest_rate <- new_rate

  method add_interest =
    balance <- (balance *. interest_rate)

  method to_bankAccount = Savings (super#get_balance, interest_rate)
end;;


class chequing_account = object
  inherit bank_account as super
  val mutable transaction_count : fTransactionCount = 0

  method get_transaction_count = transaction_count

  method deposit (amount : float) =
    super#deposit amount;
    transaction_count <- transaction_count + 1

  method withdraw (amount : float) =
    super#withdraw amount;
    transaction_count <- transaction_count + 1

  method to_bankAccount = Chequing (super#get_balance, transaction_count)

end;;

              
(* Problème 2(a) *)
(* Implémentez une hiérarchie d'héritage des comptes
   bancaires (version orientée objet du type de données du problème
   1). Les classes saving_account et chequing_account doivent être des
   sous-classes de la classe bank_account. Les arguments des
   constructeurs Bank, Savings et Chequing du type de données
   bankAccount devraient devenir des variables
   d'instance (attributs). Les valeurs initiales doivent être 0.0 pour
   le solde (fBalance), 0.2 pour le taux d'intérêt (fInterestRate) et
   0 pour le nombre de transactions (fTransactionCount). Définissez
   une méthode appelée get_balance qui renvoie le solde.

   Utilisez le style de programmation suivant.
    - N'utilisez pas de classes abstraites.
    - Les attributs et méthodes doivent apparaître dans la classe la
      plus élevée possible dans la hiérarchie afin de maximiser
      l'héritage. Remplacez ("override") les méthodes uniquement
      lorsque cela est nécessaire.
    - Dans une sous-classe, n'utilisez pas directement les attributs
      de la super-classe. Par exemple, si l'implémentation
      d'une méthode dans savings_account doit accéder "balance",
      elle doit alors s'appeler "get_balance".

   Implémentez des méthodes appelées "deposit" et "withdraw" qui
   prennent un argument qui représente le montant à ajouter ou à
   soustraire du solde. Si le montant à retirer est supérieur au
   solde, levez une exception qui prend un argument. Les données
   renvoyées quand cette exception est levée doivent correspondre au
   solde du compte.

   Ajoutez des méthodes dans savings_account pour obtenir et définir
   le taux d'intérêt. Ajoutez également une méthode "add_interest" qui
   modifie le solde en ajoutant des intérêts au solde à l'aide du taux
   d'intérêt.

   Dans la classe chequing_account, redéfinissez les
   méthodes "deposit" et "withdraw" afin qu’elles incrémentent
   également le nombre de transactions. Ajoutez une méthode pour
   obtenir la valeur du nombre de transactions, mais ne permettez pas
   aux clients de le changer. *)


(* Problème 2(b) *)
(* Ajoutez une "to_bankAccount" à chaque classe.  Cette méthode doit
   transformer un objet en une valeur correspondante de type
   bankAccount. (Il doit renvoyer un élément de type bankAccount où
   les valeurs des arguments sont déterminées à partir des valeurs des
   attributs. *)
   

(**********************************************)
(* PROBLÈME 3: "Constructeurs" orientés objet *)
(**********************************************)

(* Problème 3(a) *)
(* Écrivez une fonction qui prend un argument, puis crée un objet
   bank_account, puis utilise l'argument pour mettre à jour le solde
   du compte. Implémentez des fonctions similaires pour
   savings_account et chequing_account. La fonction qui crée un compte
   d'épargne doit prendre un argument supplémentaire qui doit être
   utilisé pour définir le taux d'intérêt. *)

let construct_bank_account (amount : float) : bank_account =
  let new_acc = new bank_account in
  new_acc#deposit amount;
  new_acc;;


let construct_savings (amount : float) (rate : float) : savings_account =
  let new_acc = new savings_account in
  new_acc#deposit amount;
  new_acc#set_interest_rate rate;
  new_acc;;
   
let construct_chequing (amount : float) : chequing_account =
  let new_acc = new chequing_account in
  new_acc#deposit amount;
  new_acc;;



(* Problème 3(b) *)
(* En utilisant les mêmes données que celles que vous avez utilisées
   pour créer votre solution à 1(b), créez un objet de chaque classe,
   puis créez une liste qui contient tous les trois. Il pourrait être
   nécessaire d'utiliser l'opérateur de coercion "<:" du chapitre 12
   de "Real World OCaml". (Voir les notes de cours.) *)

  let ba1 = construct_bank_account 10.0
  let ba2 = construct_savings 5.0 7.0
  let ba3 = construct_chequing 15.0;;
  let ba_list = [ba1;(ba2 : savings_account :> bank_account);(ba3 : chequing_account :> bank_account)];;


(* Problème 3(c) *)
(* Refaites le problème 1(a), en écrivant cette fois une version
   orientée objet (une fonction qui prend une liste d'objets de type
   bank_account et renvoie une liste de soldes). Appelez votre
   fonction en utilisant comme argument la liste créée comme solution
   au problème 3(b). *)

let get_oBalances1 l = 
  let rec aux l soldes= 
    match l with
    | [] -> soldes
    | hd::tl -> aux tl (soldes @ [hd#get_balance])
  in aux l []


(*********************************************************)
(* PROBLÈME 4: Conversion en style « orientée fonction » *)
(*********************************************************)
(* Écrivez une fonction qui renvoie une liste de soldes de compte.
   Ces valeurs doivent être identiques à celles qui ont été renvoyées
   dans la solution du problème 3(c), mais cette fois, votre fonction
   doit d'abord créer une liste d'objets du type bank_account, puis
   les convertir en éléments de type bankAccount, puis appeler votre
   fonction de problème 1(a) *)

let get_oBalances1 l = 
  let rec aux lst soldes =
    match lst with
  | [] -> soldes
  | hd::tl -> aux tl (soldes @ [hd#get_balance])
  in aux l []  