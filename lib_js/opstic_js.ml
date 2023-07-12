open! Kxclib
include Types
module Monad = Monad
module ConcurrentQueue = ConcurrentQueue
module Server = Server

module Mpst_js = struct
  type 'a service = { witness : 'a; service : Server.service }

  let handle_request = Server.handle_request
  let create_server = Server.Server0.create

  (* let register_service t ~(witness : 'a) ~id ~my_role ~other_roles :
       'a service =
     let spec =
       Server.
         {
           service_id = ServiceId.create id;
           my_role = Role.create my_role;
           other_roles = List.map Role.create other_roles;
         }
     in
     let s_service = Server.register_service t ~spec in
     { witness; service = s_service } *)
end
