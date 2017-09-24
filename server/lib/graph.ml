open Core
open Util
open Types

module RT = Runtime

module NodeMap = Int.Map
type nodemap = Node.node NodeMap.t [@@deriving eq]

let pp_nodemap nm =
  let to_s ~key ~data = (show_id key) ^ ": " ^ (Node.show_node data) in
  let objs = NodeMap.mapi ~f:to_s nm in
  "{"
  ^ (String.concat ~sep:", " (NodeMap.data objs))
  ^ "}"


(* ------------------------- *)
(* Graph *)
(* ------------------------- *)
type oplist = Op.op list [@@deriving eq, yojson, show]
type targetpair = (id * string)
type graph = { name : string
             ; ops : oplist
             ; nodes : nodemap [@printer fun fmt nm -> fprintf fmt "%s" (pp_nodemap nm)]
             } [@@deriving eq, show]


let get_node (g: graph) (id: id) : Node.node =
  NodeMap.find_exn g.nodes id

let has_node (id: id) (g: graph) : bool =
  NodeMap.mem g.nodes id

let create (name : string) : graph ref =
  ref { name = name
      ; ops = []
      ; nodes = NodeMap.empty
      }

let get_children (g: graph) (id: id) : Node.node list =
  g.nodes
  |> NodeMap.data
  |> List.filter ~f:(fun n ->
      n#arguments
     |> RT.ArgMap.data
     |> List.filter ~f:((=) (RT.AEdge id))
     |> List.is_empty
     |> not)

let gfns (g: graph) : Node.gfns =
  { getf = get_node g
  ; get_children = get_children g
  }


(* ------------------------- *)
(* Updating *)
(* ------------------------- *)
let change_node (id: id) (g: graph) ~(f: (Node.node option -> Node.node option)) : graph =
  { g with nodes = NodeMap.change g.nodes id ~f }

let update_node (id: id) (g : graph) ~(f: (Node.node -> Node.node option)) : graph =
  change_node
    id
    g
    (function Some node -> f node
            | None -> Exception.raise "can't update missing node")

let update_node_position (id: id) (loc: loc) (g: graph) : graph =
  update_node id g ~f:(fun n -> n#update_loc loc; Some n)

let set_arg (a: RT.argument) (t: id) (param: string) (g: graph) : graph =
  update_node t g
    ~f:(fun n ->
        if not (n#has_parameter param)
        then Exception.raise ("Node " ^ n#name ^ " has no parameter " ^ param);
        n#set_arg param a;
        Some n)

let set_const (t: id) (param: string) (v : string) (g: graph) : graph =
  set_arg (RT.AConst (RT.parse v)) t param g

let set_edge (s : id) (t : id) (param: string) (g: graph) : graph =
  set_arg (RT.AEdge s) t param g

let clear_args (id: id) (g: graph) : graph =
  update_node id g ~f:(fun n -> n#clear_args; Some n)

let delete_arg (t: id) (param:string) (g: graph) : graph =
  update_node t g ~f:(fun n -> n#delete_arg param; Some n)

let add_node (node : Node.node) (g : graph) : graph =
  if has_node node#id g then
    Exception.raise "A node with this ID already exists!";
  change_node node#id g ~f:(fun x -> Some node)

let incoming_nodes id g : (id * string) list =
  g.nodes
  |> Int.Map.data
  |> List.map
    ~f:(fun n ->
        Map.filter_mapi n#arguments
          ~f:(fun ~key ~data -> match data with | RT.AConst _ -> None
                                                | RT.AEdge i ->
                                                  if i = id
                                                  then Some (n#id,key)
                                                  else None))
  |> List.map ~f:String.Map.data
  |> List.concat

let rec delete_node id (g: graph) : graph =
  let node = get_node g id in
  let deps = node#dependent_nodes (gfns g) in
  let nodes = incoming_nodes id g in
  let g = List.fold_left ~init:g nodes
      ~f:(fun g_ (id2, param) -> delete_arg id2 param g_) in
  let g = update_node id ~f:(fun x -> None) g in
  let g = List.fold_left ~init:g deps
      ~f:(fun g_ d ->
          if has_node d g_
           then delete_node d g_
           else g_) in
  g

(* ------------------------- *)
(* Ops *)
(* ------------------------- *)
let apply_op (op : Op.op) (g : graph ref) : unit =
  g :=
    !g |>
    match op with
    | Add_fn_call (id, loc, name) ->
      add_node (new Node.func id loc name)
    | Add_datastore (id, loc, table) ->
      add_node (new Node.datastore id loc table)
    | Add_value (id, loc, expr) ->
      add_node (new Node.value id loc expr)
    | Add_anon (nid, loc, rid, argids, anon_names) ->
        let newx = loc.x + 30 in
      (fun g ->
         argids
         |> List.zip_exn anon_names
         |> List.mapi ~f:(fun i (argname, argid) -> new Node.argnode
                          argid
                          { x=newx + (i * 60); y=loc.y + 40 }
                          argname
                          i
                          nid
                          rid
                          argids)
         |> List.append [ new Node.anonfn nid
                          { x=newx; y=loc.y }
                          rid
                          argids
                        ; new Node.returnnode rid
                          { x=newx + 125; y=loc.y + 135 }
                          nid
                          argids]
         |> List.fold_left ~init:g ~f:(fun g n -> add_node n g))
    | Update_node_position (id, loc) -> update_node_position id loc
    | Set_constant (target, param, value) ->
      set_const target param value
    | Set_edge (src, target, param) -> set_edge src target param
    | Delete_arg (target, param) -> delete_arg target param
    | Clear_args (id) -> clear_args id
    | Delete_node (id) -> delete_node id
    | Noop -> ident
    | _ ->
      Util.inspecT "op is" op;
      failwith "applying unimplemented op"

let add_op (op: Op.op) (g: graph ref) : unit =
  if op <> Noop then
    (apply_op op g;
     g := { !g with ops = !g.ops @ [op]})

(* ------------------------- *)
(* Serialization *)
(* ------------------------- *)
let filename_for name = "appdata/" ^ name ^ ".dark"

let load name : graph ref =
  let g = create name in
  name
  |> filename_for
  |> Util.readfile ~default:"[]"
  |> Yojson.Safe.from_string
  |> oplist_of_yojson
  |> Result.ok_or_failwith
  |> List.iter ~f:(fun op -> add_op op g);
  g

let save (g : graph) : unit =
  let filename = filename_for g.name in
  g.ops
  |> oplist_to_yojson
  |> Yojson.Safe.pretty_to_string
  |> (fun s -> s ^ "\n")
  |> Util.writefile filename


(* ------------------------- *)
(* To Frontend JSON *)
(* ------------------------- *)
let node_value (n: Node.node) (g: graph) : (string * string * string) =
  try
    let dv = Node.execute n#id (gfns g) in
    ( RT.to_repr dv
    , RT.get_type dv
    , dv |> RT.dval_to_yojson |> Yojson.Safe.pretty_to_string)
  with
  | Exception.UserException e -> ("Error: " ^ e, "Error", "Error")

let to_frontend_nodes (g: graph) : Yojson.Safe.json =
  g.nodes
  |> NodeMap.data
  |> List.map ~f:(fun n -> n#to_frontend (node_value n g))
  |> Node.nodejsonlist_to_yojson

let to_frontend (g : graph) : Yojson.Safe.json =
  `Assoc [ ("nodes", to_frontend_nodes g)]

let to_frontend_string (g: graph) : string =
  g |> to_frontend |> Yojson.Safe.pretty_to_string ~std:true
