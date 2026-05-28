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

let yojson_of_finding workspace_root (f : Ocamlgrep.Match.finding) =
  (* f.loc is compiler-libs.Location.t; field access resolves correctly by type
     even though [Location] in this scope refers to the LSP Location module. *)
  let source = f.loc.loc_start.pos_fname in
  let abs_path = Filename.concat workspace_root source in
  let uri = Uri.of_path abs_path in
  `Assoc
    [ "uri", `String (Uri.to_string uri)
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
      ; "errors", `List []
      ]
  | Error msg ->
    `Assoc
      [ "findings", `List []
      ; "warnings", `List []
      ; "errors", `List [ `String msg ]
      ]
;;

let raise_error fmt =
  Printf.ksprintf
    (fun msg ->
      Jsonrpc.Response.Error.raise
        (Jsonrpc.Response.Error.make ~code:InternalError ~message:msg ()))
    fmt
;;

(* Run the search against the workspace rooted at [workspace_root].
   Uses Dune_workspace.describe with an explicit root so the LSP server does
   not need to change directory.  Cmt paths from dune are relative to the
   project root, so we join them with [workspace_root] to get absolute paths
   suitable for Cmt_format.read_cmt. *)
let do_search ~workspace_root ~query =
  match Ocamlgrep.Match.parse_query query with
  | exception Failure msg -> Error msg
  | expr ->
    (match Ocamlgrep.Dune_workspace.describe ~root:workspace_root () with
    | Error msg -> Error msg
    | Ok ws ->
      let modules = Ocamlgrep.Dune_workspace.get_modules ws in
      let build_prefix = ws.build_context ^ "/" in
      let strip_build s =
        if String.starts_with ~prefix:build_prefix s
        then
          String.sub s (String.length build_prefix)
            (String.length s - String.length build_prefix)
        else s
      in
      let findings = ref [] in
      let warnings = ref [] in
      let total = List.length modules in
      let successes = ref 0 in
      List.iter
        (fun (m : Ocamlgrep.Dune_workspace.module_) ->
          match m.cmt, m.impl with
          | None, _ | _, None -> ()
          | Some rel_cmt, Some impl_path ->
            let source = strip_build impl_path in
            let abs_source = Filename.concat ws.root source in
            let abs_cmt = Filename.concat ws.root rel_cmt in
            (try
               match Cmt_format.read_cmt abs_cmt with
               | { Cmt_format.cmt_source_digest = Some digest; _ } as cmt ->
                 if Sys.file_exists abs_source && digest = Digest.file abs_source
                 then begin
                   let src_lines =
                     String.split_on_char '\n'
                       (In_channel.with_open_text abs_source In_channel.input_all)
                     |> Array.of_list
                   in
                   let results =
                     Ocamlgrep.Match.search expr cmt ~source ~src_lines
                   in
                   incr successes;
                   List.iter (fun f -> findings := f :: !findings) results
                 end else incr successes
               | _ -> ()
             with _ -> ()))
        modules;
      if !successes < total
      then begin
        let missing = total - !successes in
        let pct = !successes * 100 / total in
        warnings
          := Printf.sprintf
               "%d/%d cmt files found (%d%% coverage); %d missing — run 'dune build \
                @check' to generate them"
               !successes
               total
               pct
               missing
          :: !warnings
      end;
      Ok (List.rev !findings, List.rev !warnings))
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
    let result = do_search ~workspace_root ~query in
    Fiber.return (yojson_of_response workspace_root result))
;;
