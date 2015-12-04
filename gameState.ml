type territory = string
type continent = string
type player_id = int (* player -1 means no one owns the territory *)
type t = {
           (* list of (territories, owner, and number of armies) *)
           territories : (territory * player_id * int) list;
           active_player: player_id;
           (* list of (continents, list of territories in the continents) *)
           continents : (continent * territory list) list;
           continent_owners : (continent * player_id) list;
           (* list of player ids associated with (if they are human, name) *)
           player_info : (player_id * (bool * string)) list;
           (* list of territories, associated with a list of its neighbors *)
           map : (territory * territory list) list
         }
let no_one = -1

let tarray = [|
  "Alaska";
  "Alberta";
  "Central America";
  "Eastern United States";
  "Greenland";
  "Northwest Territory";
  "Ontario";
  "Quebec";
  "Western United States";
  "Argentina";
  "Brazil";
  "Peru";
  "Venezuela";
  "Great Britain";
  "Iceland";
  "Northern Europe";
  "Scandinavia";
  "Southern Europe";
  "Ukraine";
  "Western Europe";
  "Congo";
  "East Africa";
  "Egypt";
  "Madagascar";
  "North Africa";
  "South Africa";
  "Afghanistan";
  "China";
  "India";
  "Irkutsk";
  "Japan";
  "Kamchatka";
  "Middle East";
  "Mongolia";
  "Siam";
  "Siberia";
  "Ural";
  "Yakutsk";
  "Eastern Australia";
  "Indonesia";
  "New Guinea";
  "Western Australia" |]

let cints = [
("North America", [1;2;3;4;5;6;7;8;9]);
("South America", [10;11;12;13]);
("Europe", [14;15;16;17;18;19;20]);
("Africa", [21;22;23;24;25;26]);
("Asia", [27;28;29;30;31;32;33;34;35;36;37;38]);
("Australia",[39;40;41;42])]

let carray = [|
  [2;6;32];
  [1;6;7;9];
  [4;9;13];
  [3;7;8;9];
  [6;7;8;15];
  [1;2;5;7];
  [2;4;5;6;8;9];
  [4;5;7];
  [2;3;4;7];
  [11;12];
  [10;12;13;25];
  [10;11;13];
  [3;11;12];
  [15;16;17;20];
  [5;14;17];
  [14;17;18;19;20];
  [14;15;16;19];
  [16;19;20;23;25;33];
  [16;17;18;27;33;37];
  [14;16;18;25];
  [22;25;26];
  [21;23;24;25;33];
  [18;22;25;33];
  [22;26];
  [11;18;20;21;22;23];
  [21;22;24];
  [19;28;29;33;37];
  [27;29;34;35;36;37];
  [27;28;33;35];
  [32;34;36;38];
  [32;34];
  [1;30;31;34;38];
  [18;19;22;23;27;29];
  [28;30;31;32;36];
  [28;29;40];
  [28;30;34;37;38];
  [19;27;28;36];
  [30;32;36];
  [41;42];
  [35;41;42];
  [39;40;42];
  [39;40;41] |]

(*Internal functions*)

let create_territories () =
  Array.to_list (Array.map (fun n -> (n,-1,0)) tarray)

let create_continents () =
  let f (n,x) = (n, List.map (fun y -> tarray.(y-1)) x) in
  List.map f cints

let create_connections () =
  let f x = List.map (fun y -> tarray.(y-1)) x in
  Array.to_list (Array.mapi (fun n x -> (x, f carray.(n))) tarray)

let create_cowners () =
  List.map (fun (x,_) -> (x, no_one)) cints

(*External functions*)

(** creates a player id with given value *)
let create_player (n: int) : player_id =
  n

(** returns a new state with player_id added to players*)
let add_player (state: t) (pid: player_id) (name: string) (human: bool) : t =
  let current_players = state.player_info in
  let new_players = (pid, (human, name)) :: current_players in
  {state with player_info = new_players}

let new_state () =
  (* for now, a very small map
   * TODO: read territory list from file? *)
  { territories = create_territories ();
    active_player = -1;
    continents = create_continents ();
    continent_owners = [];
    player_info = [];
    map = create_connections ()
  }

(** only players still in the game *)
let get_player_id_list (gs:t) : player_id list =
  let rec get_ids lst =
    match lst with
    | [] -> []
    | (x,(_,_))::tl -> x::(get_ids tl)
  in get_ids (gs.player_info)

(** given the player_id get back all the territories belong to the person*)
let get_territories (gs:t) (player:player_id) :territory list =
  let rec get_terri lst =
    match lst with
    | [] -> []
    | (t, id, _)::tl -> (
      if id = player then t :: (get_terri tl)
      else get_terri tl)
  in get_terri gs.territories

(** use to identify it is a computer player or human*)
let get_is_human (gs:t) (id:player_id) : bool =
  let rec find lst =
    match lst with
    | [] -> failwith "Player not found."
    | (x,(b,_))::tl -> if(x = id) then b else find tl
  in find (gs.player_info)

(** get the name of the player corresponding to the given id*)
let get_name (gs:t) (id:player_id) : string =
  let rec find lst =
    match lst with
    | [] -> failwith "Player not found."
    | (x,(_,n))::tl -> if(x = id) then n else find tl
  in find (gs.player_info)

(** get the current active player_id*)
let get_active_player (gs:t) : player_id =
  gs.active_player

(** get the number of armies on the specified territory*)
let get_armies (gs:t) (terr: territory) : int =
  let rec find lst =
    match lst with
    | [] -> failwith "Territory not found."
    | (t,_,num)::tl -> if t=terr then num else find tl
  in
  find gs.territories

(** give back the id of the owner of the given territory*)
let get_territory_owner (gs:t) (terr:territory) : player_id =
  let rec find lst =
    match lst with
    | [] -> failwith "Territory not found."
    | (t,n,_)::tl -> if (t = terr) then n else find tl
  in find gs.territories

(** get a list of continents controlled by player_id, returns [] if no
  * continents are held *)
let get_continents (gs:t) (id:player_id) : continent list =
  let rec find lst =
    match lst with
    | [] -> []
    | (c,n)::tl -> if(n = id) then c::(find tl) else find tl
  in find gs.continent_owners

(** returns a list of all continents in the game *)
let get_all_continents (state: t) : continent list =
  let pairs = state.continents in
  let rec get_continent_list c_list =
    match c_list with
    | [] -> []
    | (c, _) :: tl -> c :: get_continent_list tl
  in
  get_continent_list pairs

(** returns a list of all territories in the game *)
let get_all_territories (state: t) : territory list =
  let tuples = state.territories in
  let rec get_territory_list t_list =
    match t_list with
    | [] -> []
    | (t, _, _) :: tl -> t :: get_territory_list tl
  in
  get_territory_list tuples

(** given the territory and the number of armies intended to change the number
  * on the territory to the new specified value*)
let set_num_armies (gs:t) (terr:territory) (n:int) : t =
  let rec find lst =
    match lst with
    | [] -> []
    | (t,i,x)::tl ->
      if(t = terr) then (t,i,n)::find tl
      else (t,i,x)::find tl
  in {gs with territories = (find gs.territories)}

(** given the territory and the new owner's playerid to
  * set the new owner of the territory*)
let set_territory_owner (state: t) (terr: territory) (pid: player_id): t =
    let territories = state.territories in
    (*returns an updated territory list, where the owner of terr is pid*)
    let rec find tlist =
        match tlist with
        | [] -> []
        | (t, id, n)::tl ->
        if t = terr then (t, pid, n) :: tl
        else (t, id, n) :: (find tl)
    in
    {state with territories = find territories}

(** remove a player from the game by giving the player_id*)
let remove_player (gs:t) (player:player_id) : t =
  let helper (x, _) = (x!=player) in
  let new_list = List.filter helper gs.player_info in
  {gs with player_info = new_list}

(** set the active player to the next player*)
let set_next_player (gs:t) : t =
  (* let (og,(_,_)) = List.hd (gs.player_info) in
  let rec find lst =
    match lst with
    | [] -> failwith "No players in game."
    | (i1,(_,_))::(i2,(_,_))::tl -> if(i1 = gs.active_player)
        then i2 else find tl
    | (i,(_,_))::tl -> if(i = gs.active_player) then og else find tl
  in {gs with active_player = (find gs.player_info)} *)
  let current = gs.active_player in
  (*returns the index of active player in player_info*)
  let rec find lst =
    if current = no_one then 0 else
    match lst with
    | [] -> failwith "No players in game"
    | (id, (_, _))::tl -> (
      if id = current then
        match tl with
        | [] -> let (nid, (_,_)) = List.hd (gs.player_info) in nid
        | (nid, (_,_))::_ -> nid
      else find tl)
  in
  {gs with active_player = (find gs.player_info)}

(** set the active to the given player_id*)
let set_active_player (state: t) (pid: player_id): t =
    {state with active_player = pid}

(** sets player_id to be the owner of continent *)
let set_continent_owner (state: t) (pid: player_id) (cont: continent): t =
    let cont_list = state.continent_owners in
    let removed = List.remove_assoc cont cont_list in
    let new_list = (cont, pid)::removed in
    {state with continent_owners = new_list}

(** return the name of the given territory *)
let string_of_territory (gs:t) (terr:territory) : string =
  terr

(** return the name of the given continent *)
let string_of_continent (gs:t) (c:continent) : string =
  c

(** returns a list of territories adjacent to terr1 *)
let get_adjacency (gs:t) (terr1:territory) : territory list =
  (* let neighbors = List.assoc terr1 gs.map in
  List.mem terr2 neighbors *)
  List.assoc terr1 gs.map

(** return the continent that the territory is in *)
let get_continent_of_terr (gs:t) (terr:territory) : continent =
  let (cont, _) =
    List.hd (
      List.filter
        (fun (_, tlist) ->
          List.mem terr tlist
        )
      gs.continents
    ) in
  cont

(** get a list of territories in the given continent *)
let get_continent_territories (gs:t) (cont:continent) : territory list =
  List.assoc cont gs.continents