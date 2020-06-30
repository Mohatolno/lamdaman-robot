
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

module Key =
  struct
    type t = Graph.node
    let compare = compare
  end

module Priority =
  struct
    type t = float
    let infinity = 100000000.
    let to_string x = string_of_float x
    let compare = compare
  end  

module Q = PriorityQueue.Make(Key) (Priority);;
       
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
       ;;

(**

  Comme dit en introduction, le robot a une mémoire qui lui permet de
   stocker des informations sur le monde et ses actions courantes.

  On vous propose de structurer la mémoire comme suit:

 *)
type memory = {
  id          : int;                 (** identifiant de la memoire        *)
  start       : bool;                (** statut robot fin initialisation  *)        
  known_world : World.t option;      (** Le monde connu par le robot.     *)
  graph       : Graph.t;             (** Un graphe qui sert de carte.     *)
  objective   : objective;           (** L'objectif courant du robot.     *)
  targets     : Space.position list; (** Les points où il doit se rendre. *)
}
;;
(**

   Initialement, le robot ne sait rien sur le monde, n'a aucune cible
   et doit s'initialiser.

*)
let initial_memory = {
  id          = -1;
  start       = false;
  known_world = None;
  graph       = Graph.empty;
  objective   = Initializing;
  targets     = [];
}
;;
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

;;
(**

   Pour naviguer dans le monde, le robot construit une carte sous la
   forme d'un graphe.

   Les noeuds de ce graphe sont des positions clées
   du monde. Pour commencer, vous pouvez y mettre les sommets des
   polygones de l'enfer, le vaisseau, le robot et les arbres.

   Deux noeuds sont reliés par une arête si le segment dont ils
   sont les extremités ne croisent pas une bouche de l'enfer.

 *)



(** trie une liste de paire de (position * distance) *)
let rec rev_append left right = match left with
    [] -> right
   |t::q -> rev_append q (t::right)
;;  
let insert (x,d) l = 
  let rec aux ll acc = match ll with
    |[] -> rev_append acc [(x,d)]
    |(n,d')::q -> if d=d' then rev_append acc ll
		  else if d<d' then rev_append acc ((x,d)::ll) 
		  else aux q ((n,d')::acc) 
  in aux l []
;;
let sort_list l =
  let rec aux ll acc = match ll with
    | [] -> acc
    | t::q -> aux q (insert t acc)
  in aux l []
;;

(** renvoie une liste de segments à partir d'une liste de position *)
let set_segments l =
  try
    let rec aux ll (acc : segment list) = match ll with
      |[] -> acc
      |[_] -> acc
      |t::q -> aux q ((List.map (fun x -> (t,x)) q) @ acc)
    in aux l []
  with _ -> failwith "set_segments"
;;

(** renvoie la liste des segments des tous les hells *)
(**let set_hells_segments hells_list =
  let rec aux ll (acc : segment list) = match ll with
  |[] -> acc
  |t::q -> aux q ((polygon_segments t) @ acc)
  in aux hells_list []
;;*)
 
(** renvoie un booleen si un segment coupe ou pas les segments d'un hell *)
let segment_cut_hell seg hell_seg_list =
  List.exists (segment_intersects seg) hell_seg_list
;;

(** renvoie la liste des segments qui ne coupent pas un hell*)
let valid_segments seg_list hell_seg_list =
  try
    let rec aux l acc = match l with
      |[] -> acc
      |t::q -> if (segment_cut_hell t hell_seg_list) then aux q acc
      else aux q (t::acc)
    in aux seg_list []
  with _ -> failwith "all_valid_segments"
;;

(** convertie une distance en float *)
let float_to_distance d = match d with
  |Distance a -> a
;;
 
(** renvoie la liste des aretes valides du graphe *)
let set_edges valid_segments_list =
  try
    let rec aux l (acc : Graph.edge list) = match l with
      |[] -> acc
      |(p1,p2)::q -> let dist = float_to_distance (dist2 p1 p2) in
    (**let dist = sqrt (((x2 -. x1)**2.) +. ((y2 -. y1)**2.)) in*)
    aux q ((p1,p2,dist)::acc)
    in aux valid_segments_list []
  with _ -> failwith "set_edges"
;;

(** retourne les segments des champs de souffrance *)
let ground_segments world = List.(
  Space.polygons world.space (( <> ) Hell)
  |> map Space.polygon_segments
  |> flatten
)

   
let max_list list =
  let rec aux l = match l with
    |[] -> failwith "impossible"
    |[x] -> x
    |t::tt::q -> aux ((max t tt)::q)
  in aux list
;;

let min_list list =
  let rec aux l = match l with
    |[] -> failwith "impossible"
    |[x] -> x
    |t::tt::q -> aux ((min t tt)::q)
  in aux list
;;

(** retourne la plus petite et la plus grand position d'un polygon en fonction de ses coordonnées ainsi la longueur de son coté *)
let min_max_position_dist_poly poly =
  let l_vertic = vertices poly in
  let l_segments = polygon_segments poly in
  let (seg_x,seg_y) = List.hd l_segments in
  let seg_dist = float_to_distance (dist2 seg_x seg_y) in
  let min_pos = min_list l_vertic in
  let max_pos = max_list l_vertic in
  (min_pos,max_pos,seg_dist)
;;
  
(** retourne les sommets des hells/champs de souffrance elargis *)
let increase_seg poly_list inc =
  try
    let rec aux l acc = match l with
      |[] -> acc
      |t::q -> let ((x1,y1), (x2,y2), cote) = min_max_position_dist_poly t in
	       aux q ((x1 -. inc,y1 -. inc)::(x1 -. inc,y1 +. cote +. inc)::(x2 +. inc,y2 +. inc)::(x2 +. inc,y2 -. cote -. inc)::acc)  
    in aux poly_list []
  with _ -> failwith "increase_seg"
;;

(** prend une liste de polygons et renvoie une nouvelle liste avec les sommets elargis*)
(**let extend_polygons poly_list inc =
  try
    let rec aux l acc = match l with
      |[] -> acc
      |t::q -> let ((x1,y1), (x2,y2), cote) = min_max_position_dist_poly t in
		   aux q ((Space.make [(x1 -. inc,y1 -. inc);
				      (x2 +. inc,y2 -. cote -. inc);
				      (x2 +. inc,y2 +. inc);
				      (x1 -. inc,y1 +. cote +. inc)] (Space.content t))::acc)
    in aux poly_list []
  with _ -> failwith "extend_polygons"
;;*)


(** retourne les polygones de souffrance qui depassent la valeur de la marge de la vitesse du robot en fonction d'un seuil
    [marginSpeed] en %,
 *)
let polygons_not_allowed_margin_speed list_poly_souff marginSpeed robotSpeed =
  let rec aux l acc = match l with
    |[] -> acc
    |t::q -> match (content t) with
	     |Ground f -> let robot_speed = Space.float_of_speed robotSpeed in
			  let now_speed = robot_speed *. f in
			  if (100. -. ((now_speed *. 100.) /. robot_speed)) > marginSpeed
			  then aux q (t::acc)
			  else aux q acc
	     |_ -> aux q acc
  in aux list_poly_souff []
;;
  
(** creer un Space.t à partir d'une liste de polygons *)
let create_space list_poly =
  let rec aux l acc = match l with
    |[] -> acc
    |t::q -> aux q (Space.extends_space acc (Space.polygon t))
  in aux list_poly Space.empty
;;

(** determine si une position se trouve dans un space de type ground *)
let inside_ground space position =
  match (Space.inside position (( <> ) Hell) space) with
  |None -> false
  |Some _ -> true
;;
 
let visibility_graph observation memory =
  try
    let nodes_arbres : (Graph.node list)  = tree_positions (Option.get memory.known_world).trees in
 
    let list_polygon : ((kind polygon) list) = polygons (Option.get memory.known_world).space ((=)Hell) in
    let list_polygon_souff : ((kind polygon) list) = polygons (Option.get memory.known_world).space ((<>)Hell) in
    let poly_souff_notAllowed_speed = polygons_not_allowed_margin_speed list_polygon_souff (10.) observation.speed in  
    
    let nodes_hell = increase_seg list_polygon 1. in    
    let nodes_hell_notIN_ground_notAllowed_speed =
      List.filter (fun x -> false=(inside_ground (create_space poly_souff_notAllowed_speed) x)) nodes_hell in
    let nodes_ground = increase_seg list_polygon_souff 1. in
    let nodes_ground_notIN_hell = List.filter (fun x -> false=(inside_hell (Option.get memory.known_world) x)) nodes_ground in
    let list_nodes =
      observation.position::(observation.spaceship)::(nodes_arbres @ nodes_hell_notIN_ground_notAllowed_speed @ nodes_ground_notIN_hell) in
    
    let all_segments = set_segments list_nodes in    
    let segments_hell = hell_segments (Option.get memory.known_world) in
    let segments_ground_notAllow_speed = List.flatten (List.map (fun p -> polygon_segments p) poly_souff_notAllowed_speed) in   
    let segments_not_cut_hells  = valid_segments all_segments segments_hell in
    let list_segments_not_cut_grounds = valid_segments segments_not_cut_hells segments_ground_notAllow_speed in
    
    let edges = set_edges list_segments_not_cut_grounds in
    (**(**---------------------------------------------------------- LEE ----------------------------------------------------------*)
    let list_polygon : ((kind polygon) list) = polygons (Option.get memory.known_world).space ((=)Hell) in
    let hells_extended = extend_polygons list_polygon 1. in
    let list_polygon_souff : ((kind polygon) list) = polygons (Option.get memory.known_world).space ((<>)Hell) in
    let polygon_souff_extended = extend_polygons list_polygon_souff 1. in
    let poly_souff_notAllowed_speed = polygons_not_allowed_margin_speed list_polygon_souff (10.) observation.speed in
    let poly_souff_notAllowed_speed_extended = extend_polygons poly_souff_notAllowed_speed 1. in
    
    let nodes_arbres : (Graph.node list)  = tree_positions (Option.get memory.known_world).trees in
    let nodes_hell = List.flatten (List.map (fun x -> vertices x) hells_extended) in
    let nodes_hell_notIN_ground_notAllowed_speed =
      List.filter (fun x -> false=(inside_ground (create_space poly_souff_notAllowed_speed_extended) x)) nodes_hell in
    let nodes_ground = List.flatten (List.map (fun x -> vertices x) polygon_souff_extended) in
    
    let nodes_ground_notIN_hell = List.filter (fun x -> false=(inside_hell (Option.get memory.known_world) x)) nodes_ground in
    
    let list_nodes =
      observation.position::(observation.spaceship)::(nodes_arbres @ nodes_hell_notIN_ground_notAllowed_speed @ nodes_ground_notIN_hell) in

    let all_segments = set_segments list_nodes in
    let segments_hell = hell_segments (Option.get memory.known_world) in
    let segments_ground_notAllow_speed = List.flatten (List.map (fun p -> polygon_segments p) poly_souff_notAllowed_speed) in
    
    let segments_not_cut_hells  = valid_segments all_segments segments_hell in
    let list_segments_not_cut_grounds = valid_segments segments_not_cut_hells segments_ground_notAllow_speed in     
    let edges = set_edges list_segments_not_cut_grounds in *)
    
    Graph.make list_nodes edges
  with _ -> failwith "visibility_graph : erreur"
;;
  
  
(**

   Il nous suffit maintenant de trouver le chemin le plus rapide pour
   aller d'une source à une cible dans le graphe.

 *)

(** fonctions de mise à jour des distances et des predecesseurs *)
let update_dist list_dist node =
  let (n,_) = node in
  List.filter (fun x -> n!=(fst x)) list_dist
and update_pred list_pred node =
  let (n,_) = node in
  List.filter (fun x -> n!=(fst x)) list_pred
;;

(** =============================================== DJIKSTRA =============================================================================

(** initialisation des tabeaux des distances et predecesseurs *)
let initialisation source_node nodes_list =
  try
    let rec aux l acc_dist acc_pred = match l with
      |[] -> (acc_dist, acc_pred)
      |t::q -> if source_node = t then aux q ((t,0.)::acc_dist) ((t,(infinity,infinity))::acc_pred)
      else aux q ((t,Priority.infinity)::acc_dist) ((t,(infinity,infinity))::acc_pred)
    in aux nodes_list [] []
  with _ -> failwith "initialisation"
;;

(** fonction qui met à jour les distances, predecesseurs et la file des aretes sortants du minimum extrait*)
let maj list_dist list_pred graph queue min =
  try
    let rec aux aretes acc_dist acc_pred acc_queue = match aretes with
      |[] -> (acc_dist, acc_pred, acc_queue)
      |(_,p2,dist)::q ->
let v = List.find (fun x -> p2=(fst x)) acc_dist in
if snd v > (snd min) +. dist
then aux q ((fst v,snd min +. dist)::(update_dist acc_dist v)) ((fst v,fst min)::(update_pred acc_pred v)) (Q.decrease acc_queue (fst v) (snd min +. dist))
      else aux q acc_dist acc_pred acc_queue
    in aux (Graph.out graph (fst min)) list_dist list_pred queue
  with _ -> failwith "maj"
;;

 
(** boucle de mise de tous les noeuds (minimum) extraits à tour de role *)
let djisktra_maj queue list_dist list_pred graph  =
  try
    let rec aux len acc_dist acc_pred acc_queue = match len with
      |0 -> (acc_dist, acc_pred)
      |_ -> let (p,n) = Option.get (Q.get_min acc_queue) in
   let min = (n,p) in
   let queue = Q.remove_min acc_queue in
   let (dist,pred,queue) = maj acc_dist acc_pred graph queue min in
 aux (Q.length queue) dist pred queue
    in aux (Q.length queue) list_dist list_pred queue
  with _ -> failwith "djikstra : erreur"
;;

(** fonction qui insere tous les noeuds dans la file *)
let fill_queue queue list =
  try
    let rec aux l acc_queue = match l with
      |[] -> acc_queue
      |(n,p)::q -> aux q (Q.insert acc_queue n p)
    in aux list queue
  with _ -> failwith "fill_queue"
;;

 
(** algorithme de djikstra, retourne les distances et les predecesseurs *)
let djikstra graph source =
  try
    let nodes_list = Graph.nodes graph in
    let (dist,pred) = initialisation source nodes_list in
    let queue = Q.empty in
    let queue = fill_queue queue dist in              
    djisktra_maj queue dist pred graph
  with _ -> failwith "djikstra"
;;

       
(** Retourne le chemin entre la source et la cible *)
let find_predecessor source target list_pred =
  try
    (**Printf.eprintf "\n len predecessors -- %d \n" (List.length list_pred);*)
    let rec aux cible acc_path =
      let (node,parent) = try List.find (fun x -> cible=(fst x)) list_pred with Not_found -> failwith "pas de cible trouve dans l_pred" in
      if parent = (infinity,infinity) then begin (**Printf.eprintf "\n nouveau chemin : %s \n" (string_of_path acc_path);*) acc_path end
      else aux parent (node::acc_path)
    in aux target []
with _ -> failwith "find_predecessor : erreur"
;;
========================================================================================================================================*)

(**=================================================== Algo A* =========================================================================*)
  
(** initialisation des tabeaux des distances et predecesseurs *)
let initialization source target nodes_list =
  try
    let queue = Q.empty in
    let queue = Q.insert queue source (float_to_distance (dist2 source target)) in
    let ensemble = [] in    
    let rec aux l acc_dist acc_pred = match l with
      |[] -> (acc_dist, acc_pred, queue, ensemble)
      |t::q -> if source = t then aux q ((t,0.)::acc_dist) ((t,(infinity,infinity))::acc_pred)
	       else aux q ((t,Priority.infinity)::acc_dist) ((t,(infinity,infinity))::acc_pred)
    in aux nodes_list [] []
  with _ -> failwith "initialisation"
;;
  
(** fonction qui met à jour les distances, predecesseurs et la file des aretes sortants du minimum extrait*)
let loop_every_adjacent list_dist list_pred graph queue min ensemble target =
  try
    let rec aux aretes acc_dist acc_pred acc_queue = match aretes with
      |[] -> (acc_dist, acc_pred, acc_queue)
      |(_,p2,dist)::q -> let v = List.find (fun x -> p2=(fst x)) acc_dist in
			 match v with
			 |v when (Q.priority acc_queue (fst v)) != Priority.infinity ->
			   begin
			     if snd v > (snd min) +. dist then 
			       aux q ((fst v,snd min +. dist)::(update_dist acc_dist v)) ((fst v,fst min)::(update_pred acc_pred v))
				   (Q.decrease acc_queue (fst v) ((snd min +. dist) +. (float_to_distance (dist2 (fst v) target))))	   
			     else aux q acc_dist acc_pred acc_queue
			   end
			 |v when (List.mem (fst v) ensemble)=true ->
			   begin
			     if snd v > (snd min) +. dist then 
			       aux q ((fst v,snd min +. dist)::(update_dist acc_dist v)) ((fst v,fst min)::(update_pred acc_pred v))
				   (Q.insert acc_queue (fst v) ((snd min +. dist) +.(float_to_distance (dist2 (fst v) target))))	   
			     else aux q acc_dist acc_pred acc_queue
			   end
			 |_ ->
			   begin
			     aux q ((fst v,snd min +. dist)::(update_dist acc_dist v)) ((fst v,fst min)::(update_pred acc_pred v))
				 (Q.insert acc_queue (fst v) ((snd min +. dist) +. (float_to_distance (dist2 (fst v) target))))
			   end	  
    in aux (Graph.out graph (fst min)) list_dist list_pred queue
  with _ -> failwith "loop_every_adjacent : erreur"
;;
  
  
(** fonction traitant un minimum extrait dans la file *)
let loop_while_queue_not_empty queue list_dist list_pred graph ensemble target =
  try
    let rec aux len acc_dist acc_pred acc_queue acc_ensemble = match len with
      |0 -> failwith "there are not path exist!!!"
      |_ -> let (p,n) = Option.get (Q.get_min acc_queue) in
	    (*let min = (n,p) in*)
	    let min = List.find (fun x -> n=(fst x)) acc_dist in
	    let new_ens = n::acc_ensemble in
	    let new_queue = Q.remove_min acc_queue in
	    if n=target then 
	      (acc_dist, acc_pred)
	    else
	      let (dist,pred,queue) = loop_every_adjacent acc_dist acc_pred graph new_queue min new_ens target in
	      aux (Q.length queue) dist pred queue new_ens
    in aux (Q.length queue) list_dist list_pred queue ensemble
  with _ -> failwith "loop_while_queue_not_empty : erreur"
;;

(** Retourne le chemin entre la source et la cible *)
let find_predecessor source target list_pred =
  try
    let rec aux cible acc_path =
      let (node,parent) = try List.find (fun x -> cible=(fst x)) list_pred with Not_found -> failwith "pas de cible trouve dans l_pred" in
      if parent = (infinity,infinity) then acc_path
      else aux parent (node::acc_path)
    in aux target []
with _ -> failwith "find_predecessor : erreur"
;;
(**===================================================================================================================================*)

let shortest_path graph source target : path =
  try
    let nodes_list = Graph.nodes graph in
    let (dist, pred, queue, ensemble) = initialization source target nodes_list in
    let (list_dist, list_pred) = loop_while_queue_not_empty queue dist pred graph ensemble target in
    find_predecessor source target list_pred    
  with _ -> failwith "shortespath : erreur"
;;
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


(** FONCTIONS AUXILIAIRES POUR PLAN--------------------------------------------------------------------------------------------- *)
 
(** renvoie les segments d'un chemin *)
let segs_of_path path robot =
  let rec aux l acc = match l with
    |[] -> acc
    |[_] -> acc
    |t::tt::q -> aux (tt::q) ((t,tt)::acc)
  in List.rev (aux (robot::path) [])
;;  

(** determine si un chemin est valide  *)
let rec path_is_valid path_seg segs_hell = match path_seg with
    |[] -> true
    |t::q -> if (segment_cut_hell t segs_hell) then false
    else path_is_valid q segs_hell
;;

(**renvoie un int aleatoire compris entre x et y*)
let rec random_range x y = match (Random.int(y)) with
  |value when value < x -> random_range x y
  |value -> value
;;

(** retourner les elements d'une liste dans un interval donné *)
let get_elem_interval deb fin list bool_last_index =
  let rec aux ind acc = match ind with
    |x when x = (if bool_last_index then (List.length list) else fin) -> acc
    |_ -> aux (ind+1) ((List.nth list ind)::acc)
  in List.rev (aux deb [])
;;

(** retourne l'index (position) d'un robot dans une list contenant des id_memory/microcode *)
let get_index_robot list id_memory =
  let rec aux l acc = match l with
    |[] -> -1
    |t::q -> if t = id_memory then acc
    else aux q (acc+1)
  in aux list 1
;;

(** genere la liste des objectifs d'un robot en fonction du nombre de robots *)
let generate_targets list_microdes targets id_memory =
  let list_id_microcodes = List.map (fun x -> match x.microcode with
					      | MicroAtom i -> i
					      | MicroList _ -> -100) list_microdes in  
  let index_robot = get_index_robot list_id_microcodes id_memory in
  if index_robot = -1 then []
  else
    let nbre_robot = List.length list_microdes in
    let nbre_targets = List.length targets in
    let nbre_targets_per_robot = nbre_targets / nbre_robot in
    if index_robot = nbre_robot
    then
      get_elem_interval ((index_robot-1) * nbre_targets_per_robot)
			(((index_robot-1) * nbre_targets_per_robot) + nbre_targets_per_robot) targets true
    else
      get_elem_interval ((index_robot-1) * nbre_targets_per_robot)
			(((index_robot-1) * nbre_targets_per_robot) + nbre_targets_per_robot) targets false
;;
  

(** 
    cettes fonction retourne une liste des arbres triée du plus proche au plus eloignée de la position initiale d'un robot.
    On pouvait faire autrement en creant un graphe qui contient les positions des arbres et leurs aretes puis ensuite
    utiliser djikstra qui va retourner les distances par rapport à un sommet, mais on a jugé que cela allait utiliser des ressources dont
    on n'avait pas besoin comme la liste des predecesseurs...
 *)
let get_targets_shorted_by_dist_from_robot list_tree_pos robot_pos =
  let rec aux l acc = match l with
    |[] -> acc
    |t::q -> aux q ((t,dist2 robot_pos t)::acc)
  in sort_list (aux list_tree_pos [])
;;
  
(** -------------------------------------------------------------------------------------------------------------------------- *)
  
let plan visualize observation memory = match memory.objective with
  |Initializing ->
    let tar = World.tree_positions observation.trees in
    let list_microcodes = observation.messages in
    let microcodes_len = List.length list_microcodes in
    if (microcodes_len = 0) then
      {memory with
        known_world = memory.known_world;
        graph = visibility_graph observation memory;
        objective = GoingTo([List.hd tar], tar);
        targets = [List.hd tar]                  
      }
    else
      if (List.length tar) < (microcodes_len) then
	begin
	  let reduce_robot_list = get_elem_interval 0 (List.length tar) (observation.messages) false in
	  let obj = generate_targets reduce_robot_list tar memory.id in
	  match obj with
	  |[] ->
	    {
	      id = memory.id;
	      start = true;
	      known_world = memory.known_world;
	      graph = visibility_graph observation memory;
	      objective = GoingTo([observation.spaceship], [observation.spaceship]);
	      targets = [observation.spaceship]                  
	    }
	  |[x]->
	    {
	      id = memory.id;
	      start = true;
	      known_world = memory.known_world;
	      graph = visibility_graph observation memory;
	      objective = GoingTo([x], [x]);
	      targets = [x] @ [observation.spaceship]                  
	    }
	  |_::_ -> failwith "impossible d'avoir plus d'un element"
	end
      else
	let obj = get_targets_shorted_by_dist_from_robot (generate_targets observation.messages tar memory.id) observation.position in
	let obj = List.map (fun x -> fst x) obj in
	{
	  id = memory.id;
	  start = true;
	  known_world = memory.known_world;
	  graph = visibility_graph observation memory;
	  objective = GoingTo([List.hd obj], tar);
	  targets = obj @ [observation.spaceship]                  
	}
  |Chopping -> memory
  |GoingTo (path1, path2) ->
    let path_segments = segs_of_path path1 observation.position in
    let hellsegments = hell_segments (Option.get memory.known_world) in
    let list_polygon_souff : ((kind polygon) list) = polygons (Option.get memory.known_world).space ((<>)Hell) in
    let poly_souff_notAllowed_speed = polygons_not_allowed_margin_speed list_polygon_souff (0.) observation.speed in
    let segments_ground_notAllow_speed = List.flatten (List.map (fun p -> polygon_segments p) poly_souff_notAllowed_speed) in
    Printf.eprintf "\n chemin is valide : %b \n" (path_is_valid path_segments hellsegments) ;
    if ((path_is_valid path_segments hellsegments) && (path_is_valid path_segments segments_ground_notAllow_speed))
    then {memory with graph = visibility_graph observation memory}
    else
      let short_path = shortest_path (visibility_graph observation memory) observation.position (List.hd memory.targets) in
      {memory with objective = GoingTo(short_path, short_path); graph = visibility_graph observation memory}
;;
 
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
   
 (** cette fonction auxiliaire faites par nous revoie le chemin initial à travers le chemin complet qui est la liste des cibles *)
let rec get_path chemin_initial new_target =
  try
    match chemin_initial with
    |[] -> failwith "new_target n'existe pas dans la liste des arbres : cela etant impossible"
    |t::q -> if t = new_target
    then chemin_initial
    else get_path q new_target
  with _ -> failwith "get_path"
;;
 
 
let next_action visualize observation memory = match memory.objective with
  |Initializing -> failwith "next_action : la planification n'a pas fait son role"
  |Chopping ->
    begin
      if(observation.speed = Space.speed_of_float 0.) then
	let tree = Option.get (World.tree_at (observation.trees) observation.position) in
	if tree.branches > 0 then
	  ChopTree, memory (**{memory with known_world =
			      Some World.(update_tree (Option.get memory.known_world) tree {tree_position = tree.tree_position;
			      branches = tree.branches-1});
			      }*)
	else
	  if List.length (List.tl memory.targets) >= 3 then
	    let obj = get_targets_shorted_by_dist_from_robot (List.tl (List.rev (List.tl memory.targets))) observation.position in
	    let obj = List.map (fun x -> fst x) obj in	    
	    let (a,b) = List.hd obj and (c,d) = observation.position in
	    let angle = atan2 (b -. d) (a -. c) in
	    Move(Space.angle_of_float angle, observation.max_speed),
	    {memory with objective = GoingTo([List.hd obj], [List.hd obj]);
			 targets = obj @ [observation.spaceship]; graph = visibility_graph observation memory;}
	  else
	    let (a,b) = List.hd (List.tl memory.targets) and (c,d) = observation.position in
	    let angle = atan2 (b -. d) (a -. c) in
	    Move(Space.angle_of_float angle, observation.max_speed),
	    {memory with objective = GoingTo([List.hd (List.tl memory.targets)], [List.hd (List.tl memory.targets)]);
			 targets = (List.tl memory.targets); graph = visibility_graph observation memory;}
      else
	let (a,b) = List.hd memory.targets and (c,d) = observation.position in
	let angle = atan2 (b -. d) (a -. c) in
	Move(Space.angle_of_float angle, observation.max_speed),
	{memory with objective = GoingTo([List.hd (List.tl memory.targets)], [List.hd (List.tl memory.targets)]);
		     graph = visibility_graph observation memory}
    end
  |GoingTo(path1, path2) ->
    Printf.eprintf "\n robot : %d  --  trees : %s \n" memory.id (string_of_path memory.targets) ;
    Printf.eprintf "\n target : %s \n" (string_of_path ([List.hd memory.targets])) ;
    Printf.eprintf "\n chemin : %s \n" (string_of_path path1) ;
    Printf.eprintf "\n ---------------------------------------------------------------------------------  \n"  ;
    Visualizer.show_path path1;
    if memory.id = -1
    then
      Move(observation.angle, Space.speed_of_float 0.), {memory with id = (random_range 1 1000) ; graph = visibility_graph observation memory}
    else
      begin
        if memory.start = false then
          Put(MicroAtom memory.id, duration_of_int 500),
          {memory with objective = Initializing; graph = visibility_graph observation memory}
        else
          let knw_tree = (Option.get memory.known_world).trees in
          let tree = World.tree_at knw_tree observation.position in      
          match tree with
          |None ->
            let (x2,y2) = List.hd path1 and (x1,y1) = observation.position in
	    if (close (x1,y1) (x2,y2) 1.) then
              if (List.length memory.targets = 1 && (x2,y2) = observation.spaceship) then
                Move(observation.angle, Space.speed_of_float 0.), memory
              else
                begin
                  let (a,b) = List.hd (List.tl path1) and (c,d) = observation.position in
                  let angle = atan2 (b -. d) (a -. c) in
                  Move(Space.angle_of_float angle, observation.speed),
                  {memory with objective = GoingTo(List.tl path1, path2); graph = visibility_graph observation memory}  
                end
	    else if (close (x1,y1) (x2,y2) 5.) then
	      Move(observation.angle, Space.speed_of_float 0.5), memory
            else
              let (a,b) = List.hd path1 and (c,d) = observation.position in
              let angle = atan2 (b -. d) (a -. c) in
              Move(Space.angle_of_float angle, observation.max_speed),
              {memory with objective = GoingTo(path1, path2); graph = visibility_graph observation memory}
          |Some t ->
            if t.tree_position = List.hd memory.targets then
              Move(observation.angle, Space.speed_of_float 0.), {memory with objective = Chopping}
            else
              let (a,b) = List.hd path1 and (c,d) = observation.position in
              let angle = atan2 (b -. d) (a -. c) in
              Move(Space.angle_of_float angle, observation.max_speed),
              {memory with objective = GoingTo(path1, path2); graph = visibility_graph observation memory}
      end
;;
 
 
(**

   Comme promis, la fonction de décision est la composition
   des trois fonctions du dessus.

*)
let decide visualize observation memory : action * memory =
  let memory = discover visualize observation memory in
  let memory = plan visualize observation memory in
  Visualizer.show_graph (visibility_graph observation memory) observation (List.hd memory.targets);
  next_action visualize observation memory
