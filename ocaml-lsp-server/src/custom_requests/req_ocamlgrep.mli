(** Custom LSP request: project-wide search for OCaml expression patterns.

    ocamlgrep searches a compiled OCaml project for sub-expressions that match
    a given pattern.  Patterns are ordinary OCaml expressions extended with
    wildcard syntax: [__] matches any expression, [__1]/[__2] are numbered
    metavariables (enforcing structural equality across occurrences), and
    [(e : t)] constrains the type of a match.  The search is type-aware —
    it operates on [.cmt] typed trees rather than source text, so it is
    unaffected by variable naming or formatting.

    A dedicated custom method is warranted because the query is
    project-wide (not buffer-local) and requires no cursor position, which
    does not fit any standard LSP request shape.  The method is
    [ocamllsp/ocamlgrep]; the server advertises [handleOcamlgrep] in its
    experimental capabilities. *)

open Import

val meth : string
val capability : string * [> `Bool of bool ]
val on_request : params:Jsonrpc.Structured.t option -> State.t -> Json.t Fiber.t
