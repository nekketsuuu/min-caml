val parser_emit : out_channel -> Syntax.t -> unit
val kNormal_emit : out_channel -> KNormal.t -> unit

type level =
  | Parser
  | Typing
  | KNormal
  | Alpha
  | Iter
  | Closure
  | Virtual
  | Simm
  | RegAlloc
  | Emit

val level_of_string : string -> level
