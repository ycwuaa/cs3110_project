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
  { territories = [
      ("Brazil", -1, 0);
      ("Argentina", -1, 0)];
    active_player = -1;
    continents = [];
    continent_owners = [];
    player_info = [];
    map = [
      ("Brazil", ["Argentina"]);
      ("Argentina", ["Brazil"])]
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
    | (x, _, _)::t -> x::(get_terri t)
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

(** get a list of continents controlled by player_id, returns [] if no continents are held *)
let get_continents (gs:t) (id:player_id) : continent list =
  let rec find lst =
    match lst with
    | [] -> []
    | (c,n)::tl -> if(n = id) then c::(find tl) else find tl
  in find gs.continent_owners

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
  let rec find lst n =
    if current = no_one then 0 else
    match lst with
    | [] -> failwith "No players in game"
    | (id, (_, _))::tl -> (
      if id = current then
        if n >= (List.length gs.player_info) then 0 else n+1
      else find tl (n+1))
  in
  let index = find gs.player_info 0 in
  let (id, (h, n)) = List.nth gs.player_info index in
  {gs with active_player = id}

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

(** returns true if the two territories are adjacent *)
let check_adjacency (gs:t) (terr1:territory) (terr2:territory) : bool =
  let neighbors = List.assoc terr1 gs.map in
  List.mem terr2 neighbors