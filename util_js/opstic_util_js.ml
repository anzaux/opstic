module ServerIo = ServerIo
module ConcurrentQueue = ConcurrentQueue

type 'a ok_or_error = ('a, ServerIo.error) result
type 'a waiting = 'a ok_or_error -> unit
