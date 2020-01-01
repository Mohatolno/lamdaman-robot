(**

   Chers programmeuses et programmeurs de λman, votre mission consiste
   à compléter ce module pour faire de vos λmen les meilleurs robots
   de la galaxie. C'est d'ailleurs le seul module qui vous pouvez
   modifier pour mener à bien votre mission.

   La fonction à programmer est

         [decide : memory -> observation -> action * memory]

   Elle est appelée à chaque unité de temps par le Λserver avec de
   nouvelles observations sur son environnement. En réponse à ces
   observations, cette fonction décide quelle action doit effectuer le
   robot.

   L'état du robot est représenté par une valeur de type [memory].  La
   fonction [decide] l'attend en argument et renvoie une nouvelle
   version de cette mémoire. Cette nouvelle version sera passée en
   argument à [decide] lors du prochain appel.

*)

open World
open Space

(** Le Λserver transmet les observations suivantes au λman: *)
type observation = World.observation

(** Votre λman peut se déplacer : il a une direction D et une vitesse V.

    La direction est en radian dans le repère trigonométrique standard :

    - si D = 0. alors le robot pointe vers l'est.
    - si D = Float.pi /. 2.  alors le robot pointe vers le nord.
    - si D = Float.pi alors le robot pointe vers l'ouest.
    - si D = 3 * Float.pi / 2 alors le robot pointe vers le sud.
    (Bien entendu, ces égalités sont à lire "modulo 2 * Float.pi".)

    Son déplacement en abscisse est donc V * cos D * dt et en ordonnée
   V * sin D * dt.

    Votre λman peut communiquer : il peut laisser du microcode sur sa
   position courante pour que d'autres λmen puissent le lire.  Un
   microcode s'autodétruit au bout d'un certain nombre d'unités de
   temps mais si un microcode est laissé près d'un autre microcode
   identique, ils fusionnent en un unique microcode dont la durée de
   vie est le somme des durées de vie des deux microcodes initiaux.
   Construire un microcode demande de l'énergie au robot : chaque
   atome lui coûte 1 point d'énergie. Heureusement, l'énergie augmente
   d'1 point toutes les unités de temps.

    Pour terminer, votre λman peut couper des arbres de Böhm. Les
   arbres de Böhm ont un nombre de branches variables. Couper une
   branche prend une unité de temps et augmente le score de 1
   point. Si on ramène cette branche au vaisseau, un second point est
   accordé.

    Pour finir, le monde est malheureusement très dangereux : on y
   trouve des bouches de l'enfer dans lesquelles il ne faut pas tomber
   ainsi que des champs de souffrances où la vitesse de votre robot
   est modifiée (de -50% à +50%).

*)

type action =
  | Move of Space.angle * Space.speed
  (** [Move (a, v)] est l'angle et la vitesse souhaités pour la
     prochaine unité de temps. La vitesse ne peut pas être négative et
     elle ne peut excéder la vitesse communiquée par le serveur. *)

  | Put of microcode * Space.duration
  (** [Put (microcode, duration)] pose un [microcode] à la position courante
      du robot. Ce microcode s'autodétruira au bout de [duration] unité de
      temps. Par contre, s'il se trouve à une distance inférieure à
      [Space.small_distance] d'un microcode similaire, il est fusionné
      avec ce dernier et la durée de vie du microcode résultant est
      la somme des durées de vide des deux microcodes. *)

  | ChopTree
  (** [ChopTree] coupe une branche d'un arbre de Böhm situé une distance
      inférieure à [Space.small_distance] du robot. Cela augmente le score
      de 1 point. *)

  | Wait
  (** [Wait] ne change rien jusqu'au prochain appel. *)

  | Die of string
  (** [Die] est produit par les robots dont on a perdu le signal. *)

[@@deriving yojson]

(**

   Le problème principal de ce projet est le calcul de chemin.

   On se dote donc d'un type pour décrire un chemin : c'est une
   liste de positions dont la première est la source du chemin
   et la dernière est sa cible.

*)
type path = Space.position list

(** Version lisible des chemins. *)
let string_of_path path =
  String.concat " " (List.map string_of_position path)

(**

   Nous vous proposons de structurer le comportement du robot
   à l'aide d'objectifs décrits par le type suivant :

*)
type objective =
  | Initializing            (** Le robot doit s'initialiser.       *)
  | Chopping                (** Le robot doit couper des branches. *)
  | GoingTo of path * path
  (** Le robot suit un chemin. Le premier chemin est la liste des
      positions restantes tandis que le second est le chemin initial.
      On a donc que le premier chemin est un suffixe du second. *)

(** Version affichable des objectifs. *)
let string_of_objective = function
  | Initializing -> "initializing"
  | Chopping -> "chopping"
  | GoingTo (path, _) ->
     Printf.sprintf
       "going to %s" (String.concat " " (List.map string_of_position path))

(**

  Comme dit en introduction, le robot a une mémoire qui lui permet de
   stocker des informations sur le monde et ses actions courantes.

  On vous propose de structurer la mémoire comme suit:

*)
type memory = {
    known_world : World.t option;      (** Le monde connu par le robot.     *)
    graph       : Graph.t;             (** Un graphe qui sert de carte.     *)
    objective   : objective;           (** L'objectif courant du robot.     *)
    targets     : Space.position list; (** Les points où il doit se rendre. *)
}

(**

   Initialement, le robot ne sait rien sur le monde, n'a aucune cible
   et doit s'initialiser.

*)
let initial_memory = {
    known_world = None;
    graph       = Graph.empty;
    objective   = Initializing;
    targets     = [];
}

(**

   Traditionnellement, la fonction de prise de décision d'un robot
   est la composée de trois fonctions :

   1. "discover" qui agrège les observations avec les observations
      déjà faites dans le passé.

   2. "plan" qui met à jour le plan courant du robot en réaction
      aux nouvelles observations.

   3. "next_action" qui décide qu'elle est l'action à effectuer
       immédiatement pour suivre le plan.

*)

(** [discover] met à jour [memory] en prenant en compte les nouvelles
    observations. *)
let discover visualize observation memory =
  let seen_world = World.world_of_observation observation in
  let known_world =
    match memory.known_world with
    | None -> seen_world
    | Some known_world -> World.extends_world known_world seen_world
  in
  if visualize then Visualizer.show ~force:true known_world;
  { memory with known_world = Some known_world }


(**

   Pour naviguer dans le monde, le robot construit une carte sous la
   forme d'un graphe.

   Les noeuds de ce graphe sont des positions clées
   du monde. Pour commencer, vous pouvez y mettre les sommets des
   polygones de l'enfer, le vaisseau, le robot et les arbres.

   Deux noeuds sont reliés par une arête si le segment dont ils
   sont les extremités ne croisent pas une bouche de l'enfer.

 *)




(*idée 
reruperer tous les polygones de lenfer
->puis la listes de leur nodes
->recuperer la listes de tous les nodes du graphes
->constuire tous les segments possibles
->tester si les segments sont valides cad quils passent pas par hell
->leur attribuer les distances pour obtenir les edges
->creer le graphe
*)


    (*dans la tache2 on maj les fonctions precedentes 
de sorte a eviter les bouches de l'enfer*)


  (*les nodes du graphe sont :
    les nodes des polygones 
    la listes des arbres
    la position du robot 
    la position du vaisseau
*)

(*les nodes des polygones de hell*)
(*let nodes_polygones observation =
  let liste = space.polygone observation.arroud.kind(*le match peut etre*)in
  let rec aux res 
  in match liste with
     |[]->res 
     |a::b ->space.vertices a ::(aux res)
    in aux [];;
*)
(*la liste de tous les nodes*)



    (*let liste_nodes observation =
let nodes_hell= nodes_polygones observation in 
  [observation.position]@[observation.trees]@[nodes_hell]
     *)

(*je recupere la liste de tous les segements 
pour chaque node je le relis a tous les autres 
puis j'enleve la repitition 
a la fin je verfie si ca passe pas dans hell 
c'est quoi le poid du segement
la distance de se segment
a la fin je leur attribue l'attribut distance que je vais calculer pour chaque segment
donc je transforme la liste de segment en une liste de edge
*)


(*attention a ne pas rajouter les nodes de hell avec les autres *)
(*renvoie le liste des segement 
ps:faut juste enlever les segments sous forme de point *)
(*segment intersect et polygone_segment*)







(*
let rec segment_liste l = match l with 
	[]->[]
	|a::b->(List.map(fun a ->( List.hd l,a )) l)
	@ (segment_liste b);;
 *)

(*pour chaque node je verifie qu'il passe pas par ceux de hell *)
(**)







(*let segment_valide observation =
  (*verfier qu'on a pas les nodes de hell*)
  let ln=liste_nodes observation in
  let lh=
  let ls=segment_liste ln in
  match ls with
    []->[]
 *)

let visibility_graph observation memory =
  Graph.empty (* Students, this is your job! *)

  
  (*                  
  let n = 
  (*j'avoute au graph des nodes*)
  let grp = add_node memory.graph n
                      in  make (liste_node )
           
   *)

          
(**

   Il nous suffit maintenant de trouver le chemin le plus rapide pour
   aller d'une source à une cible dans le graphe.

 *)




  (*
let shortest_path graph source target : path =
  [] (* Students, this is your job! *)

   *)
(**

   [plan] doit mettre à jour la mémoire en fonction de l'objectif
   courant du robot.

   Si le robot est en train de récolter du bois, il n'y a rien à faire
   qu'attendre qu'il est fini.

   Si le robot est en train de suivre un chemin, il faut vérifier que
   ce chemin est encore valide compte tenu des nouvelles observations
   faites, et le recalculer si jamais ce n'est pas le cas.

   Si le robot est en phase d'initialisation, il faut fixer ses cibles
   et le faire suivre un premier chemin.

*)


  (*corriger le goingto *)
let plan visualize observation memory = match memory.objective with
  | Initializing ->
     {
       known_world = memory.known_world;
       graph = visibility_graph observation memory;
       objective = GoingTo((World.tree_positions observation.trees) @ [observation.spaceship], []);  
       targets = (World.tree_positions observation.trees) @ [observation.spaceship]                  
     }
  | Chopping -> memory
  | GoingTo (path1, _) -> {memory with targets = path1}
 ;;




let chemin_initial ch_complet ch_restant =
  let rec aux acc l = match l with
    |[] -> acc
    |t::q -> if List.mem t ch_restant
    then aux acc q
    else aux (t::acc) q
  in List.rev (aux [] ch_complet)
;;
 


 (* 
   let plan visualize observation memory = match memory.objective with
  | Initializing ->
     {
       known_world = memory.known_world;
       graph = Graph.empty;
       objective = GoingTo(World.tree_positions observation.trees @ [observation.spaceship], []);   
       targets = World.tree_positions observation.trees @ [observation.spaceship]                   
     }
  | Chopping -> memory 
  | GoingTo (path1, path2) ->
     {
       known_world = memory.known_world;
       graph = memory.graph;
       (*on suppose ici le traitement que pour une obsrvation unique*)
       objective = GoingTo(path1, path2);
       targets = path1
     }
 ;;
*)
(**

   Next action doit choisir quelle action effectuer immédiatement en
   fonction de l'objectif courant.

   Si l'objectif est de s'initialiser, la plannification a mal fait
   son travail car c'est son rôle d'initialiser le robot et de lui
   donner un nouvel objectif.

   Si l'objectif est de couper du bois, coupons du bois! Si on vient
   de couper la dernière branche, alors il faut changer d'objectif
   pour se déplacer vers une autre cible.

   Si l'objectif est de suivre un chemin, il faut s'assurer que
   la vitesse et la direction du robot sont correctes.

*)

 
let next_action visualize observation memory = match memory.objective with
  |Initializing -> failwith "next_action : la planification n'a pas fait son role"
  |Chopping ->
    begin
      let knw_tree = (Option.get memory.known_world).trees in
      let tree = Option.get (World.tree_at knw_tree observation.position) in
      if tree.branches > 0 then
ChopTree,
{memory with known_world =
        Some World.(update_tree (Option.get memory.known_world) tree {tree_position = tree.tree_position;branches = tree.branches-1});
}
      else
let (a,b) = List.hd (List.tl memory.targets) and (c,d) = observation.position in
let angle = atan2 (b -. d) (a -. c) in
Move(Space.angle_of_float angle, observation.max_speed),
{memory with objective = GoingTo(List.tl memory.targets,
    chemin_initial (World.tree_positions observation.trees @ [observation.spaceship]) (List.tl memory.targets))
}
    end
  |GoingTo(path1, _) ->
    let knw_tree = (Option.get memory.known_world).trees in
    let tree = World.tree_at knw_tree observation.position in      
    match tree with
    |None -> let (a,b) = List.hd path1 and (c,d) = observation.position in
    let angle = atan2 (b -. d) (a -. c) in
    Move(Space.angle_of_float angle, observation.max_speed), memory
    |Some _ ->
      Move(observation.angle, Space.speed_of_float 0.), {memory with objective = Chopping}
;;

(*

let next_action visualize observation memory = match memory.objective with
  |Initializing -> failwith "erreur initialisation de planification "
  |Chopping -> let knw = (Option.get memory.known_world).trees in
      let tree = Option.get (tree_at knw observation.position) in
      if tree.branches > 0
      then ChopTree,
   {
     known_world =
Some (update_tree (Option.get memory.known_world) tree {tree_position = tree.tree_position; branches = tree.branches-1}) ;
     graph = memory.graph;
     objective = memory.objective;
     targets = memory.targets;
   }
      else Move(observation.angle, observation.max_speed), memory
              (** GoingTo(List.tl memory.targets, chemin_initial (World.tree_positions observation.trees) memory.targets), memory)*)
  |GoingTo(path1, path2) ->  Move (Space.angle_of_float 0., Space.speed_of_float 1.),  memory
;;
*)

(*Space.angle_of_float (3. *. Float.pi /. 2.)*)

(*
let next_action visualize observation memory = match memory.objective with
  |Initializing ->
  |Chopping -> if (tree_at world observation.position).branches > 0
      then ChopTree, memory
      else GoingTo(List.tl memory.targets, (*utiliser fx aux *), mettre a jour memory
  |GoingTo(path1, path2) ->
    Move (Space.angle_of_float 0., Space.speed_of_float 1.), memory
;;*)
(**

   Comme promis, la fonction de décision est la composition
   des trois fonctions du dessus.

*)
let decide visualize observation memory : action * memory =
  let memory = discover visualize observation memory in
  let memory = plan visualize observation memory in
  next_action visualize observation memory