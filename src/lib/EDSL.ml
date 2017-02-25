type 'a t = 'a Language.t
type 'a cli_option = 'a Language.cli_option
type 'a option_spec = 'a Language.option_spec
type ('a, 'b) cli_options = ('a, 'b) Language.cli_options
type fd_redirection = Language.fd_redirection
let (//) = Filename.concat

include Language.Construct

open Nonstd
module String = Sosa.Native_string

let case condition body = `Case (condition, seq body)
let default d = `Default (seq d)
let switch l =
  let default = ref None in
  let cases =
    List.filter_map l ~f:(function
      | `Default d when !default <> None ->
        failwith "Cannot build switch with >1 defaults"
      | `Default d -> default := (Some d); None
      | `Case t -> Some t)
  in
  make_switch ~default:(Option.value ~default:nop !default) cases

let string_concat sl =
  (* This is a pretty unefficient implementation: *)
  let out s = call [string "printf"; string "%s"; s] in
  seq (List.map sl ~f:out) |> output_as_string

type file = <
  get : string t;
  set : string t -> unit t;
  append : string t -> unit t;
  delete: unit t;
  path: string t;
>
let tmp_file ?tmp_dir name : file =
  let default_tmp_dir = "/tmp" in
  let get_tmp_dir =
    Option.value tmp_dir
      ~default:begin
        output_as_string (
          (* https://en.wikipedia.org/wiki/TMPDIR *)
          if_then_else (getenv (string "TMPDIR") <$> string "")
            (call [string "printf"; string "%s"; getenv (string "TMPDIR")])
            (exec ["printf"; "%s"; default_tmp_dir])
        )
      end
  in
  let path =
    let clean =
      String.map name ~f:(function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' as c -> c
        | other -> '_') in
    string_concat [
      get_tmp_dir;
      string "/";
      string
        (sprintf "genspio-tmp-file-%s-%s" clean Digest.(string name |> to_hex));
    ]
  in
  let tmp = string_concat [path; string "-tmp"] in
  object
    method get = output_as_string (call [string "cat"; path])
    method path = path
    method set v =
      seq [
        (* call [string "echo"; string "Setting "; string name]; *)
        (* call [string "echo"; string "Setting "; path; string " to "; v]; *)
        (* call [string "echo"; tmp]; *)
        v >> exec ["cat"] |> write_output ~stdout:tmp;
        call [string "mv"; string "-f"; tmp; path];
      ]
    method append v =
      seq [
        seq [
          call [string "cat"; path];
          v >> exec ["cat"];
        ] |> write_output ~stdout:tmp;
        call [string "mv"; string "-f"; tmp; path];
      ]
    method delete =
      call [string "rm"; string "-f"; path; tmp]
  end

let with_failwith f =
  let msg = tmp_file "msg" in
  let ret = tmp_file "ret" in
  let varname = string "tttttt" in
  with_signal
    ~catch:(seq [
        call [string "printf"; string "FAILURE: %s"; msg#get];
        setenv varname ret#get;
        msg#delete;
        ret#delete;
        call [string "exit"; getenv varname];
      ])
    (fun throw ->
       f (fun ~message ~return ->
           seq [
             msg#set message;
             ret#set (Integer.to_string return);
          (* call [string "echo"; pid#get]; *)
          (* call [string "ps"; pid#get]; *)
             throw;
             (* call [string "kill"; string "-s"; string "USR1"; pid#get] *)
        ]))

let if_seq ~t ?e c =
  match e with
  | None -> if_then c (seq t)
  | Some f -> if_then_else c (seq t) (seq f) 


let eprintf fmt l =
  with_redirections
    (call (string "printf" :: string "--" :: fmt :: l)) [
    to_fd (int 1) (int 2);
  ]

module Command_line = struct
  type 'a cli_option = {
    switches: string list;
    doc: string;
    default: 'a;
  }
  type _ option_spec =
    | Opt_flag:   bool t cli_option -> bool t option_spec
    | Opt_string: string t cli_option -> string t option_spec
  and (_, _) cli_options =
    | Opt_end: string -> ('a, 'a) cli_options
    | Opt_cons: 'c option_spec * ('a, 'b) cli_options -> ('c -> 'a, 'b) cli_options

  module Arg = struct
    let string ?(default = string "") ~doc switches =
      Opt_string {switches; doc; default}
    let flag ?(default = bool false) ~doc switches =
      Opt_flag {switches; doc; default}

    let (&) x y = Opt_cons (x, y)
    let usage s = Opt_end s

  end

  let parse
      (options: ('a, unit t) cli_options)
      (action: 'a) : unit t =
    let prefix = Common.Unique_name.variable "getopts" in
    let variable {switches; doc;} =
      sprintf "%s_%s" prefix (String.concat ~sep:"" switches|> Digest.string |> Digest.to_hex) in
    let inits = ref [] in
    let to_init s = inits := s :: !inits in
    let cases = ref [] in
    let to_case s = cases := s :: !cases in
    let help_intro = ref "" in
    let help = ref [] in
    let to_help s = help := s :: !help in
    let string_of_var var =
      getenv (string var) in
    (* Output_as_string (Raw_cmd (sprintf "printf \"${%s}\"" var)) in *)
    let bool_of_var var =
      succeeds (Language.Raw_cmd (sprintf "{ ${%s} ; } " var)) in
    let unit_t =
      let rec loop
        : type a b.
          a -> (a, b) cli_options -> b =
        fun f -> function
        | Opt_end doc ->
          help_intro := doc;
          f
        | Opt_cons (Opt_string x, more) ->
          let var = variable x in
          to_init (
            setenv (string var) x.default);
          (* sprintf "export %s=$(%s)" *)
          (*          var (continue x.default |> expand_octal)); *)
          to_case (
            case (List.fold ~init:(bool false) x.switches ~f:(fun p s ->
                p ||| (string s =$= getenv (string "1"))))
              [
                if_seq (getenv (string "2") =$= string "")
                  ~t:[
                    eprintf
                      (string "ERROR option '%s' requires an argument\\n")
                      [getenv (string "1")];
                    fail;
                  ]
                  ~e:[
                    setenv (string var) (getenv (string "2"));
                  ];
                exec ["shift"];
                exec ["shift"];
              ]
          );
          (* (seq  *)
          (*      "if [ -n \"$2\" ]"; *)
          (*      sprintf "then export %s=\"$2\" " var; *)
          (*      sprintf "else printf \"ERROR -%c requires an argument\\n\" \ *)
                  (*               >&2" x.switch; *)
          (*      die "Command line parsing error: Aborting"; *)
          (*      "fi"; *)
          (*      "shift"; *)
          (*      "shift"; *)
          (*    ])); *)
          ksprintf to_help "* `%s <string>`: %s"
            (String.concat ~sep:"," x.switches) x.doc;
          loop (f (string_of_var var)) more
        | Opt_cons (Opt_flag x, more) ->
          let var = variable x in
          to_init (
            if_then_else x.default
              (setenv (string var) (string "true"))
              (setenv (string var) (string "false"))
          );
          (* sprintf *)
          (*          "export %s=$(if %s ; then printf 'true' ; else printf 'false' ; fi)" var *)
          (*          (continue x.default)); *)
          to_case (
            case (List.fold ~init:(bool false) x.switches ~f:(fun p s ->
                p ||| (string s =$= getenv (string "1"))))
              [
                setenv (string var) (string "true");
                exec ["shift"];
              ]
              (* sprintf "-%c) %s ;;" *)
              (*   x.switch (seq [ *)
              (*       sprintf "export %s=true" var; *)
              (*       "shift"; *)
              (*     ]) *)
          );
          ksprintf to_help "* `%s`: %s"
            (String.concat ~sep:"," x.switches) x.doc;
          (* ksprintf to_help "* `-%c`: %s" x.switch x.doc; *)
          loop (f (bool_of_var var)) more
      in
      loop action options
    in
    let help_msg =
      sprintf "%s\n\nOptions:\n\n%s\n"
        !help_intro (String.concat ~sep:"\n" (List.rev !help))
    in
    let help_flag_var = ksprintf string "%s_help" prefix in
    let while_loop =
      let body =
        let help_case =
          let help_switches = ["-h"; "-help"; "--help"] in
          case
            (List.fold ~init:(bool false) help_switches ~f:(fun p s ->
                 p ||| (string s =$= getenv (string "1")))) [
            setenv help_flag_var (string "true");
            string help_msg >>  exec ["cat"];
            exec ["break"];
          ]
        in
        let dash_dash_case =
          case (getenv (string "1") =$= string "--") [
            eprintf (string "WARNING: dash-dash arg: %s\\n") [getenv (string "1")];
            exec ["shift"];
            exec ["break"];
          ] in
        let anon_case =
          case (getenv (string "#") <$> string "0") [
            eprintf (string "WARNING: annon arg: %s\\n") [getenv (string "1")];
            exec ["shift"];
          ] in
        let default_case =
          default [
            eprintf (string "WARNING: should be empty: '%s'\\n") [getenv (string "1")];
            exec ["break"];
          ] in
        let cases =
          help_case :: List.rev !cases @ [dash_dash_case; anon_case; default_case] in
        seq [
          eprintf (string "While loop start: $1: %s\n") [getenv (string "1")];
          switch cases;
          eprintf (string "While loop end: $1: %s\n") [getenv (string "1")];
        ] in
      loop_while (bool true) ~body
    in
    (*   let sep = if params.statement_separator = " \n " then "\n" else " " in *)
    (*   String.concat ~sep ( *)
    (*     [ *)
    (*       "while :;"; " do case $1 in"; *)
    (*       "-h|-help|--help) "; *)
    (*       sprintf "export %s_help=true ; " prefix; *)
    (*       sprintf "%s ;" *)
    (*         (continue Construct.(string help_msg *)
    (*                              >>  exec ["cat"])); *)
    (*       " break ;;" *)
    (*     ] *)
    (*     @ List.rev !cases *)
    (*     @ [ *)
    (*       "--) shift ; break ;;"; *)
    (*       "-?*\)"; *)
    (*       "printf 'ERROR: Unknown option: %s\\n' \"$1\" >&2 ;"; *)
    (*       die "Command line parsing error: Aborting"; *)
    (*       ";;"; *)
    (*       "*\) if [ $# -eq 0 ] ; "; *)
    (*       "then echo \" $1 $# \" ; break ;"; *)
    (*       sprintf *)
    (*         " else export %s_args=\"${%s_args} %s\" ; shift ; " *)
    (*         prefix prefix *)
    (*         (continue (Output_as_string (Raw_cmd "printf \"$1\""))) ; *)
    (*       "fi ;; "; *)
    (*       "esac;"; *)
    (*       "done"] *)
    (*   ) *)
    (* in *)
    seq [
      setenv help_flag_var (string "false");
      seq (List.rev !inits);
      while_loop;
      if_then_else (bool_of_var (sprintf "%s_help" prefix))
        (nop)
        unit_t;
    ]
(*
seq (
  sprintf "export %s_args=" prefix
  :: sprintf "export %s_help=false" prefix
  :: List.rev !inits @ [
    while_loop;
    continue Construct.(
        if_then_else (bool_of_var (sprintf "%s_help" prefix))
          (nop)
          unit_t);
  ])
  assert false
*)

end
