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
    let textDocumentPosition = Lsp.Types.TextDocumentPositionParams.t_of_yojson json in
    let query = json |> member "query" |> to_string in
    { text_document = textDocumentPosition.textDocument; query }
  ;;

  let _yojson_of_t { text_document; query } =
    `Assoc
      [ "textDocument", TextDocumentIdentifier.yojson_of_t text_document
      ; "query", `String query
      ]
  ;;
end

(* Convert a merlin finding to an LSP-style JSON object.
   The file path is made absolute using the workspace root. *)
let yojson_of_finding workspace_root (f : Query_protocol.ocamlgrep_finding) =
  let abs_path = Filename.concat workspace_root f.loc.loc_start.pos_fname in
  let uri = Uri.of_path abs_path in
  `Assoc
    [ "uri", `String (Uri.to_string uri)
    ; "range", Range.yojson_of_t (Range.of_loc f.loc)
    ; "lines", `List (List.map ~f:(fun s -> `String s) f.lines)
    ]
;;

let yojson_of_result workspace_root (r : Query_protocol.ocamlgrep_result) =
  `Assoc
    [ "findings", `List (List.map ~f:(yojson_of_finding workspace_root) r.findings)
    ; "warnings", `List (List.map ~f:(fun s -> `String s) r.warnings)
    ]
;;

let dispatch merlin workspace_root query =
  Document.Merlin.with_pipeline_exn merlin (fun pipeline ->
    let result = Query_commands.dispatch pipeline (Query_protocol.Ocamlgrep (query, None)) in
    yojson_of_result workspace_root result)
;;

let on_request ~params state =
  Fiber.of_thunk (fun () ->
    let params = (Option.value ~default:(`Assoc []) params :> Yojson.Safe.t) in
    let Request_params.{ text_document = { uri }; query } =
      Request_params.t_of_yojson params
    in
    let workspace_root = State.workspace_root state |> Uri.to_path in
    let doc = Document_store.get state.State.store uri in
    match Document.kind doc with
    | `Other -> Fiber.return `Null
    | `Merlin merlin -> dispatch merlin workspace_root query)
;;
