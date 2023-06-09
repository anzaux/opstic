open! Kxclib
include Types
module ServerIo = ServerIo
module ConcurrentQueue = ConcurrentQueue

module Server = struct
  include Server

  let handle_request (server : Server.t) (request : Json.jv) :
      Json.jv ServerIo.t =
    let open ServerIo in
    let ( let* ) = ServerIo.bind in
    let ( let+ ) m f = ServerIo.map f m in
    let access fld msg =
      match Jv.access [ `f fld ] request with
      | Some (`str x) -> return x
      | _ -> error_with msg
    in
    let* entrypoint_id =
      access "entrypoint" "entrypoint not given" |> map EntrypointId.create
    in
    let* role = access "role" "role not given" |> map Role.create in
    let conversation_id () =
      access "conversation_id" "conversation_id not given"
      |> map ConversationId.create
    in
    let http_session_id () =
      access "session_id" "session_id not given" |> map SessionId.create
    in
    let* mode_str = access "mode" "mode not given" in
    let* entrypoint_kind =
      let open Server.Handler in
      match mode_str with
      | "start" -> return (StartLeader role)
      | "start_follower" ->
          let+ conversation_id = conversation_id () in
          StartFollower (role, conversation_id)
      | "join" -> return (Join role)
      | "join_correlation" ->
          let+ conversation_id = conversation_id () in
          JoinCorrelation (role, conversation_id)
      | "session" ->
          let+ http_session_id = http_session_id () in
          InSession http_session_id
      | _ -> error_with (Format.asprintf "wrong mode: %s" mode_str)
    in
    Server.Handler.handle_entry server ~entrypoint_id ~entrypoint_kind ~request
end

module Mpst_js = struct end
