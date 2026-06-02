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

let to_merlin_loc
    ({loc_start; loc_end; loc_ghost} : Ocamlgrep.location) : Loc.t = {
  loc_start;
  loc_end;
  loc_ghost;
}

let yojson_of_finding root (finding : Ocamlgrep.finding) =
  let source = finding.loc.loc_start.pos_fname in
  let abs_path = Filename.concat root source in
  let uri = Uri.of_path abs_path in
  `Assoc
    [ "uri", `String (Uri.to_string uri)
    ; "range", Range.yojson_of_t (Range.of_loc (to_merlin_loc finding.loc))
    ; "lines", `List (List.map ~f:(fun s -> `String s) finding.lines)
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
    (* Search each workspace folder independently.  Non-Dune folders are
       skipped silently by Scan.search.  For most users there is exactly one
       folder; multi-root workspaces just get merged findings. *)
    let folders =
      Workspaces.workspace_folders (State.workspaces state)
    in
    let json_findings = ref [] in
    let json_warnings = ref [] in
    let json_errors = ref [] in
    List.iter
      ~f:(fun (ws : WorkspaceFolder.t) ->
        let root = Uri.to_path ws.uri in
        match Ocamlgrep.search ~root query with
        | Error msg ->
            let msg = `String (sprintf "workspace folder %s: %s" root msg) in
            json_errors := msg :: !json_errors
        | Ok (findings, ws_warnings) ->
          json_warnings :=
            List.rev_append
              (List.map ~f:(fun s -> `String s) ws_warnings)
              !json_warnings;
          List.iter
            ~f:(fun f ->
               json_findings := yojson_of_finding root f :: !json_findings)
            findings)
      folders;
    let response =
      `Assoc [
        "findings", `List (List.rev !json_findings);
        "warnings", `List (List.rev !json_warnings);
        "errors", `List (List.rev !json_errors)
      ]
    in
    Fiber.return response)
;;
