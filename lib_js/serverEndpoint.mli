type 'x io = 'x ServerIo.t
type payload = Types.payload
type t

val send :
  t ->
  kind:Opstic.kind ->
  role:string ->
  label:string ->
  payload:Types.payload ->
  unit ServerIo.t

val receive :
  t ->
  kind:Opstic.kind ->
  roles:string list ->
  (string * string * Types.payload) ServerIo.t

val close : t -> unit

val create :
  ?get_label:(payload -> string io) ->
  ?add_label:(string -> payload -> payload io) ->
  Server.entrypoint ->
  t
