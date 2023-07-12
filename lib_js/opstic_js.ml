open! Kxclib
include Types
module Monad = Monad
module ConcurrentQueue = ConcurrentQueue
module ServerImpl = ServerImpl

module Mpst_js = struct
  type 'a service = { witness : 'a; service : ServerImpl.service }

  let handle_request = ServerImpl.Server.handle_request
  let create_server = ServerImpl.Server.create

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
