(* Handler for the [ocamllsp/ocamlgrep] custom LSP request.

   Params:  { textDocument: { uri }, query: string }
   Response: { findings: [{ uri, range, lines }], warnings: string[], errors: string[] }

   The [uri] in params identifies any file in the target project; it is
   used only to determine the workspace root — the search itself is
   project-wide.  The ocamlgrep-lib library handles project discovery,
   .cmt enumeration via [dune describe workspace], and pattern matching.

   No merlin pipeline is involved: ocamlgrep-lib depends directly on
   compiler-libs.common and calls dune as an external process. *)

open Import

let meth = "ocamllsp/ocamlgrep"
let capability = "handleOcamlgrep", `Bool true

module Request_params = struct
  type t =
    { text_document : TextDocumentIdentifier.t
    ; query : string
    }

  let t_of_yojson json =
    let open Yojson.Safe.Util in
    let text_document =
      json |> member "textDocument" |> TextDocumentIdentifier.t_of_yojson
    in
    let query = json |> member "query" |> to_string in
    { text_document; query }
  ;;
end

let yojson_of_finding workspace_root (f : Ocamlgrep.Scan.finding) =
  let abs_path = Filename.concat workspace_root (Ocamlgrep.Scan.finding_filename f) in
  let uri = Uri.of_path abs_path in
  `Assoc
    [ "uri",   `String (Uri.to_string uri)
    ; "range", Range.yojson_of_t (Range.of_loc f.loc)
    ; "lines", `List (List.map ~f:(fun s -> `String s) f.lines)
    ]
;;

let yojson_of_response workspace_root = function
  | Ok (findings, warnings) ->
    let strs ss = `List (List.map ~f:(fun s -> `String s) ss) in
    `Assoc
      [ "findings", `List (List.map ~f:(yojson_of_finding workspace_root) findings)
      ; "warnings", strs warnings
      ; "errors",   `List []
      ]
  | Error msg ->
    `Assoc
      [ "findings", `List []
      ; "warnings", `List []
      ; "errors",   `List [ `String msg ]
      ]
;;

let raise_error fmt =
  Printf.ksprintf
    (fun msg ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make ~code:InternalError ~message:msg ()))
    fmt
;;

let on_request ~params state =
  Fiber.of_thunk (fun () ->
    let params = (Option.value ~default:(`Assoc []) params :> Yojson.Safe.t) in
    let Request_params.{ text_document = { uri = _ }; query } =
      match Request_params.t_of_yojson params with
      | v -> v
      | exception exn -> raise_error "params: %s" (Printexc.to_string exn)
    in
    let workspace_root =
      match State.workspace_root state |> Uri.to_path with
      | v -> v
      | exception exn -> raise_error "workspace_root: %s" (Printexc.to_string exn)
    in
    (* ocamlgrep-lib is synchronous; run it directly in the fiber thunk.
       For large projects this blocks briefly while dune describe workspace
       runs; acceptable for a demo, and mirrors what merlin-based requests do. *)
    let result = Ocamlgrep.Scan.search ~root:workspace_root ~query in
    Fiber.return (yojson_of_response workspace_root result))
;;
