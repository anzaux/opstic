open! Kxclib
include Types
module ServerIo = ServerIo
module ConcurrentQueue = ConcurrentQueue
module Server = Server
module ServerEndpoint = ServerEndpoint
module Mpst_js = Opstic.Make (ServerIo) (ServerEndpoint)

(* Hashtbl.replace http_session_ids role (Some http_session_id); *)
