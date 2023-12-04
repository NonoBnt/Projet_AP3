#load "btreeS.cmo";;
open BtreeS;;
#load "bst.cmo";;
open Bst;;
#load "useBtree.cmo";;
open UseBtree;;
#show UseBtree;;
#show Bst;;

(*--------------------------------------------------------------------------------------*)
(*---------------------------------- Exercice 1 ----------------------------------------*)
(*--------------------------------------------------------------------------------------*)


(*----------------------------*)
(*-------- Question 1 --------*)
(*----------------------------*)

(* initialisation de Ramdom pour utiliser la fonction Random.int *)
Random.init(1000);;

(* fonction bst_rnd_create : crée un arbre binaire de recherche avec des valeurs aléatoires *)
let rec bst_rnd_create(nb, nb2, res : int * int * 'a t_btree) : 'a t_btree =
  if nb <= 0
  then res
  else
    let res2 = bst_linsert(res, Random.int nb2) in
    bst_rnd_create(nb -1, nb2, res2)
;;

(*----------------------------*)
(*-------- Question 2 --------*)
(*----------------------------*)

(* fonction balance : determine le déséquilibre de l'arbre passé en paramètre *)
let rec balance(t : 'a t_btree) : int =
  if bt_isempty(t)
  then 0
  else
    let (g, d) : int * int = (heigth(bt_subleft(t)), heigth(bt_subright(t))) in
    abs(g-d) + balance(bt_subleft(t)) + balance(bt_subright(t))
;;

(* fonction average_tree_balance : determine le déséquilibre moyen des arbres binaires de recherche à partir de x arbres crées avec des valeurs aléatoires *)
(* y est la taille de l'arbre que l'on souhaite *)
let average_tree_balance(x, y : int * int) : int =
  let res : int ref = ref 0 in
  for i = 0 to x
  do
    (
      let t : int t_btree = bst_rnd_create(y, 200, bt_empty()) in
      res := !res + balance(t);
    )
  done;
  !res/x
;;

(*----------------------------*)
(*-------- Question 3 --------*)
(*----------------------------*)

(* créée une liste qui contient une suite de chiffres allant de x à x + y *)
let rec to_list_up(x, y: int * int) : int list =
  if x == y
  then []
  else x :: to_list_up(x +1, y)
;;

(* créée une liste qui contient une suite de chiffres allant de x à x - y *)
let rec to_list_down(x, y: int * int) : int list =
  if x == y || x == 0
  then []
  else x :: to_list_down(x-1, y)
;;

(* créée une liste de 1000 éléments contenant plusieurs suites de chiffres, chaque suite de chiffres commence à un chiffre aléatoire x2 et se finit à x2 +y/10 *)
(* y est la taille des arbres voulu divisé par 10 car il y a une boucle for qui s'execute 10 fois donc 10 * (y/10) reviens à y et permet de faire plusieurs suite et non une seule *)
let rec list_arbre_up(x, y : int * int) : int list =
  let res : int list ref = ref [] in
  for i = 0 to 9
  do
    let x2 : int = Random.int x in
    res := to_list_up(x2, x2 + y/10) @ !res;
  done;
  !res
;;

(* créée une liste de 1000 éléments contenant plusieurs suites de chiffres, chaque suite de chiffres commence à un chiffre aléatoire x2 et se finit à x2 - y/10 *)
(* y est la taille des arbres voulu divisé par 10 car il y a une boucle for qui s'execute 10 fois donc 10 * (y/10) reviens à y et permet de faire plusieurs suite et non une seule *)
let rec list_arbre_down(x, y : int * int) : int list =
  let res : int list ref = ref [] in
  for i = 0 to 9
  do
    let x2 : int = Random.int x in
    res := to_list_down(x2, x2 - y/10) @ !res;
  done;
  !res
;;


(* calcule le déséquilibre moyen d'arbres créés à partir d'une liste de plusieurs suites croissantes *)
let average_tree_balance_up(x, y: int * int): int =
  let res : int ref = ref 0 in
  for i = 0 to x
  do
    (
      let t : int t_btree =  bst_lbuild(list_arbre_up(200, y)) in
      res := !res + balance(t);
    )
  done;
  !res/x
;;

(* calcule le déséquilibre moyen d'arbres créés à partir d'une liste de plusieurs suites décroissantes *)
let average_tree_balance_down(x, y : int * int) : int =
  let res : int ref = ref 0 in
  for i = 0 to x
  do
    (
      let t : int t_btree =  bst_lbuild(list_arbre_up(200, y)) in
      res := !res + balance(t);
    )
  done;
  !res/x
;;

(* nombre d'arbres à créer pour le test *)
let t : int = 1000;;

(* taille des arbres que l'on créé pour le test *)
let taille : int = 10;;
(*let taille : int = 100;;*)
(*let taille : int = 1000;;*)

(*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*)
(* Chacunes des expérimentations ci-dessous ont été réalisé 10 fois d'affilée et les valeurs ont été reportées dans un tableau pour avoir une estimation significative du déséquilibre moyen *)

(* cas avec des suites croissantes *) 
average_tree_balance_up(t, taille);;

(* cas avec des suites décroissantes *) 
average_tree_balance_down(t, taille);;

(* cas avec des valeurs aléatoires *) 
average_tree_balance(t, taille);;

