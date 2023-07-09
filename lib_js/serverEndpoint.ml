(* open! Types
open! ServerIo

let ( let* ) = ServerIo.bind

type 'x io = 'x ServerIo.t
type payload = Types.payload

let hash_find ~descr h k =
  match Hashtbl.find_opt h k with
  | Some x -> return x
  | None -> error_with descr

let get_peer (session : Server.session) role =
  hash_find session.peers role
    ~descr:
      (Format.asprintf "No role %a for session %a" Role.pp role
         ConversationId.pp session.conversation_id)

let get_session t =
  match !t with
  | Some session -> return session
  | None -> error_with "Session not established"

let send :
    t ->
    kind:Types.kind ->
    role:string ->
    label:string ->
    payload:payload ->
    unit io =
 fun t ~kind ~role ~label ~payload ->
  assert (kind = `Established);
  let* session = get_session t in
  let* payload = t.add_label label payload in
  let* peer = get_peer session (Role.create role) in
  ConcurrentQueue.enqueue peer.response_queue payload;
  return ()

let receive :
    t ->
    kind:Types.kind ->
    subpath:string ->
    roles:string list ->
    (string * string * payload) io =
 fun t ~kind ~subpath ~roles ->
  match kind with
  | `Established ->
      let* session = get_session t in
      let queues =
        roles
        |> List.map (fun role ->
               let peer = Hashtbl.find session.peers (Role.create role) in
               peer.request_queue)
      in
      let* request = ConcurrentQueue.dequeue_one_of queues in
      ()

let msg_closing (session : Server.session) =
  ServerIo.mpst_error
    (Format.asprintf "Session %a for entrypoint %a is closed" ConversationId.pp
       session.conversation_id EntrypointId.pp
       session.entrypoint_ref.spec.entrypoint_id)

let close : t -> unit =
 fun t ->
  match t.session with
  | Some session ->
      Server.kill_session t.entrypoint session.conversation_id
        (msg_closing session)
  | None -> ()

let get_label_default payload =
  match Kxclib.Jv.pump_field "label" payload with
  | `obj (("label", `str lab) :: _) -> return lab
  | _ -> error_with "no label"

let add_label_default label (payload : payload) =
  match payload with
  | `obj xs -> return (`obj (("label", `str label) :: xs))
  | _ -> error_with "payload is not a json object"

let create ?(get_label = get_label_default) ?(add_label = add_label_default)
    entrypoint =
  { entrypoint; session = None; get_label; add_label } *)
