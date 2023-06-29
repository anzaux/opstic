open! Types
open! ServerIo

let ( let* ) = ServerIo.bind

type 'x io = 'x ServerIo.t
type payload = Types.payload

type t = {
  entrypoint : Server.entrypoint;
  mutable session : Server.session option;
  get_label : payload -> string io;
  add_label : string -> payload -> payload io;
}

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
  match t.session with
  | Some session -> return session
  | None -> error_with "Session not established"

let send_to_client t role msg : unit io =
  let* session = get_session t in
  let* peer = get_peer session role in
  ConcurrentQueue.enqueue peer.response_queue msg;
  return ()

let receive_from_client t (kind : Opstic.kind) role : payload io =
  let* session =
    match kind with
    | `Established -> get_session t
    | (`Greeting | `GreetingWithId) as kind -> (
        match t.session with
        | Some session ->
            let* () = Server.accept_greeting kind t.entrypoint session role in
            return session
        | None ->
            let* session =
              Server.new_session_from_greeting kind t.entrypoint role
            in
            t.session <- Some session;
            return session)
  in
  let* peer = get_peer session role in
  let* req = ConcurrentQueue.dequeue peer.request_queue in
  return req.request_body

let send :
    t ->
    kind:Opstic.kind ->
    role:string ->
    label:string ->
    payload:payload ->
    unit io =
 fun t ~kind ~role ~label ~payload ->
  assert (kind = `Established);
  let* payload = t.add_label label payload in
  send_to_client t (Role.create role) payload

let receive :
    t -> kind:Opstic.kind -> roles:string list -> (string * string * payload) io
    =
 fun t ~kind ~roles ->
  match roles with
  | [ role ] ->
      let* payload = receive_from_client t kind (Role.create role) in
      let* label = t.get_label payload in
      return (role, label, payload)
  | _ ->
      error_with
        (Format.asprintf "Multiple roles given: %s" (String.concat "," roles))

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
  { entrypoint; session = None; get_label; add_label }
