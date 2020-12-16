
module Fmt = CCFormat
module H = Tyxml_html
module Log = (val Logs.src_log (Logs.Src.create "zulip_log_view"))
module DJ = Decoders_yojson.Basic.Decode

let spf = Printf.sprintf

module Date = struct
  type t = float
  let now = Unix.gettimeofday
  let to_string self =
    let t = Unix.gmtime self in
    spf "%d-%02d-%02d %d:%d:%d (GMT)" (1900+t.tm_year) (1+t.tm_mon) t.tm_mday t.tm_hour t.tm_min t.tm_sec
  let pp out self = Fmt.string out (to_string self)
end


module Topic_idx = struct
  type t = {
    title: string;
    latest_date: Date.t;
    size: int;
  }

  let dec_l : t list DJ.decoder =
    let open DJ in
    key_value_pairs_seq @@ fun title ->
    let+ latest_date = field "latest_date" int >|= float_of_int
    and+ size = field "size" int in
    {title; latest_date; size}

end

module Stream_idx = struct
  type t = {
    name: string;
    id: int;
    latest_id: int;
    topic_data: Topic_idx.t list;
  }

  let dec_l : t list DJ.decoder =
    let open DJ in
    field "streams" @@
    key_value_pairs_seq @@ fun name ->
    let+ id = field "id" int
    and+ latest_id = field "latest_id" int
    and+ topic_data = field "topic_data" Topic_idx.dec_l in
    { name; id; latest_id; topic_data }
end

module Msg = struct
  type t = {
    content: string; (* raw html *)
    id: int;
    sender_full_name: string;
    timestamp: Date.t;
  }

  let dec =
    let open DJ in
    let+ content = field "content" string
    and+ id = field "id" int
    and+ sender_full_name = field "sender_full_name" string
    and+ timestamp = field "timestamp" int >|= float_of_int in
    {content; id; sender_full_name; timestamp}

  let dec_l : t list DJ.decoder = DJ.list dec
end

exception Err of int * string
let error ?(code=500) s = raise (Err (code,s))
let errorf ?code fmt = Fmt.kasprintf (error ?code) fmt

let ret_html (h:_ H.elt list) =
  let open H in
  let h =
    html
      (head (title (txt "zulip log viewer"))
         [meta ~a:[a_charset "utf-8"] ();
          link ~rel:[`Stylesheet] ~href:"/css/" ();
         ])
      (body h)
  in
  Fmt.asprintf "%a@." (H.pp ()) h

module Server(Config: sig
    val dir : string
    val port: int
  end)
= struct

  module Serv = Tiny_httpd
  let urlencode = Tiny_httpd_util.percent_encode
  let urldecode = Tiny_httpd_util.percent_decode
  let to_hex s = spf "\"%s\"" @@ CCString.flat_map (fun c -> spf "%x" (Char.code c)) s

  (* the weird '.' encoding into files for topics *)
  let dotencode_filename s =
    CCString.flat_map
      (function
        | (' ' | '!' | '"' | '#' | '$' | '%' | '&' | '\'' | '*' | '+'
          | ',' | '/' | ':' | ';' | '=' | '?' | '@' | '[' | ']' | '~'
          | '^' | '.')
          as c ->
          spf ".%X" (Char.code c)
        | c when Char.code c > 127 -> spf ".%X" (Char.code c)
        | c -> String.make 1 c)
      s

  let slugify = CCString.map (function ' ' -> '-' | c -> c)
  let dotencode s = urlencode s |> CCString.map (function ' ' -> '-' | '%' -> '.' | c -> c)
  let dotdecode s = CCString.map (function '.' -> '%' | c -> c) s |> urldecode

  let (//) = Filename.concat
  let (let*?) x f =
    let res =
      try
        let resp = f x in
        Ok resp
      with Err (code, msg) ->
        Log.err (fun k->k "error (code %d):\n%s" code msg);
        Error (code, msg)
    in
    Serv.Response.make_string res

  let root _req : Serv.Response.t =
    Log.debug (fun k->k "at %a: serve /" Date.pp (Date.now()));
    let*? h =
      let file = Config.dir // "stream_index.json" in
      let streams = match DJ.decode_file Stream_idx.dec_l file with
        | Ok x -> x
        | Error e ->
          errorf ~code:404 "cannot read '%s':\n%s" file (DJ.string_of_error e)
      in
      let open H in
      let h_str (str:Stream_idx.t) =
        let url =
          spf "/stream/%d/%s" str.id (urlencode str.name)
        in
        li ~a:[a_class ["list-group-item"]] [
          a ~a:[a_href url] [h3 [txt (str.name)]];
          details ~a:[a_open ()] (summary [txt "topics"]) [
            let h_top (top:Topic_idx.t) =
              let open Topic_idx in
              let url =
                spf "/topic/%d/%s/%s" str.id (urlencode str.name)
                  (urlencode top.title)
              in
              li ~a:[a_class ["list-group-item"]] [
                span [
                  a ~a:[a_href url] [b [txt top.title]]
                ];
                span ~a:[a_class ["badge"; "bg-warning"]]
                  [ txt (spf " %d" top.size) ];
                span ~a:[a_class ["text-secondary"]] [
                  txt (spf " [%s]" (Date.to_string top.latest_date));
                ]
              ]
            in
            ul ~a:[a_class ["list-group"]] @@ List.map h_top str.topic_data
          ];
        ]
      in
      [h1 [txt "Streams"];
       ul ~a:[a_class ["list-group"]] @@ List.map h_str streams;
      ]
    in
    ret_html h

  let goto_root () = H.(a ~a:[a_href "/"] [txt "back to root"])

  let dir_of_stream id name : string =
    spf "%d-%s" id (slugify name)

  let stream id name _req : Serv.Response.t =
    Log.debug (fun k->k "at %a: serve stream %d/%s" Date.pp (Date.now()) id name);
    let open H in
    let*? h =
      let dirname = Config.dir // dir_of_stream id name in
      Log.debug (fun k->k "dirname should be '%s'" dirname);
      let items =
        try Sys.readdir dirname |> Array.to_list
        with _ -> errorf "error while reading stream dir '%s'" dirname
      in
      let topics =
        CCList.filter_map
          (fun s ->
             if Filename.check_suffix s ".json" then (
               let name = Filename.chop_suffix s ".json" in
               let name = match dotdecode name with
                 | None -> "<error>"
                 | Some s -> s
               in
               Some name
             ) else None)
          items
      in
      let h_topic topic_name : _ H.elt =
        let url =
          spf "/topic/%d/%s/%s" id (urlencode name) (urlencode topic_name)
        in
        li ~a:[a_class ["list-group-item"]] [
          a ~a:[a_href url] [txt topic_name]
        ]
      in
      [ goto_root();
        h2 [txt (spf "Stream %s" name)];
        ul ~a:[a_class ["list-group"]] (List.map h_topic topics);
      ]
    in
    ret_html h

  let topic id strname name _req : Serv.Response.t =
    Log.debug (fun k->k "at %a: serve topic %d/%s/%s" Date.pp (Date.now()) id strname name);
    let open H in
    let*? h =
      let file =
        Config.dir // dir_of_stream id strname
        // spf "%s.json" (dotencode_filename name) in
      Log.debug (fun k->k "file should be '%s' (exists: %b)" file (Sys.file_exists file));
      let messages = match DJ.decode_file Msg.dec_l file with
        | Ok x -> x
        | Error e -> errorf "could not read messages:\n%s" (DJ.string_of_error e)
      in
      (* render a single message *)
      let h_msg (m:Msg.t) : _ H.elt =
        li ~a:[a_class ["list-group-item"; "card"]] [
          div ~a:[a_class ["card-title"]] [
            h5 [txt m.sender_full_name];
            p ~a:[a_class ["text-secondary"]] [txt (Date.to_string m.timestamp)];
          ];
          div ~a:[a_class ["card-body"]] [
            div ~a:[a_class ["card-text"]] [
              (H.Unsafe.data m.content : [>`P] H.elt);
            ];
          ];
          div ~a:[a_class ["card-footer"]] [
          ];
        ]
      in
      [ p[goto_root()];
        p[a ~a:[a_href (spf "/stream/%d/%s" id (urlencode strname))]
            [txt (spf "back to %s" strname)]];
        h2 [txt name];
        (*txt (spf "topic id=%d %s/%s" id strname name) *)
        ul ~a:[a_class ["list-group"]] (List.map h_msg messages)
      ]
    in
    ret_html h

  let css req =
    let md5 = to_hex @@ Digest.string Web_data.css in
    if Serv.Request.get_header req "If-None-Match" = Some md5 then (
      Serv.Response.make_raw ~code:304 "" (* cache hit *)
    ) else (
      let headers = ["ETag", md5; "Content-Type", "text/css"] in
      Serv.Response.make_string ~headers (Ok Web_data.css)
    )

  let run () =
    let server = Serv.create ~port:Config.port () in
    Serv.add_route_handler server Serv.Route.(return) root;
    Serv.add_route_handler server
      Serv.Route.(exact "stream" @/ int @/ string_urlencoded @/ return) stream;
    Serv.add_route_handler server
      Serv.Route.(exact "topic" @/ int @/ string_urlencoded @/ string_urlencoded @/ return) topic;
    Serv.add_route_handler server Serv.Route.(exact "css" @/ return) css;
    Serv.run server
end

let () =
  let debug = ref false in
  let dirs = ref [] in
  let port = ref 8085 in
  let opts = [
    "-d", Arg.Set debug, " enable debug";
    "-p", Arg.Set_int port, " port to listen on";
  ] |> Arg.align in
  Arg.parse opts (fun f->dirs := f :: !dirs) "zulip_log_view [dir]+ [opt]*";
  Logs.set_reporter (Logs.format_reporter ());
  if !debug then Logs.set_level ~all:true (Some Logs.Debug);
  begin match !dirs with
    | [dir] ->
      Log.app (fun k->k "running on http://127.0.0.1:%d" !port);
      let module S = Server(struct
          let port = !port
          let dir = dir
        end)
      in
      begin match S.run () with
        | Ok () -> ()
        | Error e ->
          Log.err (fun k->k "error: %s" (Printexc.to_string e));
          exit 1
      end
    | _ -> failwith "provide exactly one directory";
  end
