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
    let result =
      Query_commands.dispatch pipeline
        (Query_protocol.Ocamlgrep (query, Some workspace_root))
    in
    yojson_of_result workspace_root result)
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
    let Request_params.{ text_document = { uri }; query } =
      match Request_params.t_of_yojson params with
      | v -> v
      | exception exn -> raise_error "params: %s" (Printexc.to_string exn)
    in
    let workspace_root =
      match State.workspace_root state |> Uri.to_path with
      | v -> v
      | exception exn -> raise_error "workspace_root: %s" (Printexc.to_string exn)
    in
    let doc =
      match Document_store.get state.State.store uri with
      | v -> v
      | exception exn -> raise_error "document_store: %s" (Printexc.to_string exn)
    in
    match Document.kind doc with
    | `Other -> Fiber.return `Null
    | `Merlin merlin ->
      let open Fiber.O in
      let* result = Fiber.collect_errors (fun () -> dispatch merlin workspace_root query) in
      (match result with
       | Ok json -> Fiber.return json
       | Error [] -> raise_error "dispatch: unknown error"
       | Error (e :: _) ->
         raise_error "dispatch: %s" (Printexc.to_string e.Exn_with_backtrace.exn)))
;;
