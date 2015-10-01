val parser_emit : out_channel -> Syntax.t -> unit

type level =
  | Parser
  | Typing
  | KNormal
  | Alpha
  | Closure
  | Virtual
  | Simm
  | RegAlloc
  | Emit

val level_of_string : string -> level
