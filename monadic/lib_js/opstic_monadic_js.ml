open! Kxclib
open! Opstic_js

module Mpst_js = struct
  include Opstic_monadic.Make (ServerIo) (Opstic_js.ServerEndpoint)

  type 'a entrypoint = { witness : 'a; entrypoint : Server.entrypoint }

  let handle_request = Server.handle_entry

  let create_server = Server.create_server

  let register_entrypoint t ~(witness : 'a) ~id ~my_role ~other_roles :
      'a entrypoint =
    let s_entrypoint =
      Server.register_entrypoint t ~id:(EntrypointId.create id)
        ~my_role:(Role.create my_role)
        ~other_roles:(List.map Role.create other_roles)
    in
    { witness; entrypoint = s_entrypoint }

  let create : 'a entrypoint -> 'a Comm.ep =
   fun ent ->
    let ep = ServerEndpoint.create ent.entrypoint in
    create ep ent.witness
end
