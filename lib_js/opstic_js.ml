open! Kxclib
include Types
module Monad = Monad
module ConcurrentQueue = ConcurrentQueue
module Server = Server

module Mpst_js = struct
  type 'a entrypoint = { witness : 'a; entrypoint : Server.entrypoint }

  let handle_request = Server.handle_entry
  let create_server = Server.create_server

  (* let register_entrypoint t ~(witness : 'a) ~id ~my_role ~other_roles :
       'a entrypoint =
     let spec =
       Server.
         {
           service_id = ServiceId.create id;
           my_role = Role.create my_role;
           other_roles = List.map Role.create other_roles;
         }
     in
     let s_entrypoint = Server.register_entrypoint t ~spec in
     { witness; entrypoint = s_entrypoint } *)
end
