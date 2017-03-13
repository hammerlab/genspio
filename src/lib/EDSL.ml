type 'a t = 'a Language.t
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
  string_concat_list (list sl)

let string_list_to_string l = list_to_string ~f:(fun e -> e) l
let string_list_of_string s = list_of_string ~f:(fun e -> e) s

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
      (action: anon: string list t -> 'a) : unit t =
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
    let bool_of_var var =
      getenv (string var) |> Bool.of_string in
    let anonarg_var = prefix ^ "_anon" |> string in
    let anon =
      getenv anonarg_var |> string_list_of_string in
    let applied_action =
      (** 
        The [loop] function below is building 3 pieces of Genspio code at once:

        - variable initializations
        - individual case statements (including variable assignments)
          that are part of the ["while true { switch { .... } }"] loop 
          that incrementally interprets each command line argument.
        - [applied_action] (of type [unit t]) is the
          the result of applying the [action] function to all the elements of
          [options] + the list of anonymous arguments.
          It is hence the (user-provided) code that uses the parsed arguments.
          The [loop] function builds the closure as the loop goes since
          [options] is a “difference list”, see also:
          {{:https://drup.github.io/2016/08/02/difflists/}Drup's blog post}.

        The 2 first items are agglomerated in the [inits] and [cases]
        references.
      *)
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
          ksprintf to_help "* `%s <string>`: %s"
            (String.concat ~sep:"," x.switches) x.doc;
          loop (f (string_of_var var)) more
        | Opt_cons (Opt_flag x, more) ->
          let var = variable x in
          to_init (
            setenv (string var) (Bool.to_string x.default)
          );
          to_case (
            case (List.fold ~init:(bool false) x.switches ~f:(fun p s ->
                p ||| (string s =$= getenv (string "1"))))
              [
                setenv (string var) (Bool.to_string (bool true));
                exec ["shift"];
              ]
          );
          ksprintf to_help "* `%s`: %s"
            (String.concat ~sep:"," x.switches) x.doc;
          loop (f (bool_of_var var)) more
      in
      loop (action ~anon) options
    in
    let help_msg =
      sprintf "%s\n\nOptions:\n\n%s\n"
        !help_intro (String.concat ~sep:"\n" (List.rev !help))
    in
    let help_flag_var = ksprintf string "%s_help" prefix in
    let while_loop =
      let body =
        let append_anon_arg_to_list =
          setenv anonarg_var (
            list_append
              (string_list_of_string (getenv anonarg_var))
              (list [getenv (string "1")])
            |> string_list_to_string
          ) in
        let help_case =
          let help_switches = ["-h"; "-help"; "--help"] in
          case
            (List.fold ~init:(bool false) help_switches ~f:(fun p s ->
                 p ||| (string s =$= getenv (string "1")))) [
            setenv help_flag_var (Bool.to_string (bool true));
            string help_msg >>  exec ["cat"];
            exec ["break"];
          ]
        in
        let dash_dash_case =
          case (getenv (string "1") =$= string "--") [
            exec ["shift"];
            loop_while (getenv (string "#") <$> string "0") ~body:begin
              seq [
                append_anon_arg_to_list;
                exec ["shift"];
              ]
            end;
            exec ["break"];
          ] in
        let anon_case =
          case (getenv (string "#") <$> string "0") [
            append_anon_arg_to_list;
            exec ["shift"];
          ] in
        let default_case =
          default [
            exec ["break"];
          ] in
        let cases =
          help_case :: List.rev !cases @ [dash_dash_case; anon_case; default_case] in
        seq [
          switch cases;
        ] in
      loop_while (bool true) ~body
    in
    seq [
      setenv help_flag_var (Bool.to_string (bool false));
      setenv anonarg_var (string_list_to_string (list []));
      seq (List.rev !inits);
      while_loop;
      if_then_else (bool_of_var (sprintf "%s_help" prefix))
        (nop)
        applied_action;
    ]

end
