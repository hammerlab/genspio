type 'a t = 'a Language.t

type c_string = Language.c_string

type byte_array = Language.byte_array

type fd_redirection = Language.fd_redirection

let ( // ) = Filename.concat

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
      | `Default d ->
          default := Some d ;
          None
      | `Case t -> Some t )
  in
  make_switch ~default:(Option.value ~default:nop !default) cases

let string_list_to_string l =
  Elist.to_string ~f:(fun e -> to_byte_array e) l |> to_c_string

let string_list_of_string s =
  Elist.of_string ~f:(fun e -> to_c_string e) (to_byte_array s)

type file =
  < get: byte_array t
  ; get_c: c_string t
  ; set: byte_array t -> unit t
  ; set_c: c_string t -> unit t
  ; append: byte_array t -> unit t
  ; delete: unit t
  ; path: c_string t >

let tmp_file ?tmp_dir name : file =
  let default_tmp_dir = "/tmp" in
  let get_tmp_dir =
    Option.value tmp_dir
      ~default:
        ( get_stdout
            ((* https://en.wikipedia.org/wiki/TMPDIR *)
             if_then_else
               C_string.(getenv (c_string "TMPDIR") <$> c_string "")
               (call
                  [c_string "printf"; c_string "%s"; getenv (c_string "TMPDIR")])
               (exec ["printf"; "%s"; default_tmp_dir]))
        |> to_c_string )
  in
  let path =
    let clean =
      String.map name ~f:(function
        | ('a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-') as c -> c
        | other -> '_' )
    in
    C_string.concat_list
      [ get_tmp_dir
      ; c_string "/"
      ; c_string
          (sprintf "genspio-tmp-file-%s-%s" clean
             Digest.(string name |> to_hex)) ]
  in
  let tmp = C_string.concat_list [path; string "-tmp"] in
  object (self)
    method get = get_stdout (call [string "cat"; path])
    method get_c = self#get |> to_c_string
    method path = path
    method set v =
      seq
        [ (* call [string "echo"; string "Setting "; string name]; *)
          (* call [string "echo"; string "Setting "; path; string " to "; v]; *)
          (* call [string "echo"; tmp]; *)
          v >> exec ["cat"] |> write_output ~stdout:tmp
        ; call [string "mv"; string "-f"; tmp; path] ]
    method set_c c = self#set (to_byte_array c)
    method append v =
      seq
        [ seq [call [string "cat"; path]; v >> exec ["cat"]]
          |> write_output ~stdout:tmp
        ; call [string "mv"; string "-f"; tmp; path] ]
    method delete = call [string "rm"; string "-f"; path; tmp]
  end

let if_seq ~t ?e c =
  match e with
  | None -> if_then c (seq t)
  | Some f -> if_then_else c (seq t) (seq f)

let printf fmt l = call (string "printf" :: string "--" :: fmt :: l)

let eprintf fmt l = with_redirections (printf fmt l) [to_fd (int 1) (int 2)]

module Command_line = struct
  type 'a cli_option = {switches: string list; doc: string; default: 'a}

  type _ option_spec =
    | Opt_flag: bool t cli_option -> bool t option_spec
    | Opt_string: c_string t cli_option -> c_string t option_spec

  and (_, _) cli_options =
    | Opt_end: string -> ('a, 'a) cli_options
    | Opt_cons:
        'c option_spec * ('a, 'b) cli_options
        -> ('c -> 'a, 'b) cli_options

  module Arg = struct
    let string ?(default= string "") ~doc switches =
      Opt_string {switches; doc; default}

    let flag ?(default= bool false) ~doc switches =
      Opt_flag {switches; doc; default}

    let ( & ) x y = Opt_cons (x, y)

    let usage s = Opt_end s
  end

  let parse (options: ('a, unit t) cli_options)
      (action: anon:c_string list t -> 'a) : unit t =
    let prefix = Common.Unique_name.variable "getopts" in
    let variable {switches; doc} =
      sprintf "%s_%s" prefix
        (String.concat ~sep:"" switches |> Digest.string |> Digest.to_hex)
    in
    let inits = ref [] in
    let to_init s = inits := s :: !inits in
    let cases = ref [] in
    let to_case s = cases := s :: !cases in
    let help_intro = ref "" in
    let help = ref [] in
    let to_help s = help := s :: !help in
    let string_of_var var = getenv (string var) in
    let bool_of_var var = getenv (string var) |> Bool.of_string in
    let anon_tmp =
      ksprintf tmp_file "parse-cli-%s"
        (Marshal.to_string options [] |> Digest.string |> Digest.to_hex)
    in
    let anon = anon_tmp#get_c |> string_list_of_string in
    let applied_action =
      (* 
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
      let rec loop : type a b. a -> (a, b) cli_options -> b =
       fun f -> function
        | Opt_end doc ->
            help_intro := doc ;
            f
        | Opt_cons (Opt_string x, more) ->
            let var = variable x in
            to_init (setenv (string var) x.default) ;
            to_case
              (case
                 (List.fold ~init:(bool false) x.switches ~f:(fun p s ->
                      p ||| C_string.(c_string s =$= getenv (c_string "1")) ))
                 [ if_seq
                     C_string.(getenv (string "2") =$= string "")
                     ~t:
                       [ eprintf
                           (string "ERROR option '%s' requires an argument\\n")
                           [getenv (string "1")]
                       ; fail "Wrong command line" ]
                     ~e:[setenv (string var) (getenv (string "2"))]
                 ; exec ["shift"]
                 ; exec ["shift"] ]) ;
            ksprintf to_help "* `%s <string>`: %s"
              (String.concat ~sep:"," x.switches)
              x.doc ;
            loop (f (string_of_var var)) more
        | Opt_cons (Opt_flag x, more) ->
            let var = variable x in
            to_init (setenv (string var) (Bool.to_string x.default)) ;
            to_case
              (case
                 (List.fold ~init:(bool false) x.switches ~f:(fun p s ->
                      p ||| C_string.equals (string s) (getenv (string "1")) ))
                 [ setenv (string var) (Bool.to_string (bool true))
                 ; exec ["shift"] ]) ;
            ksprintf to_help "* `%s`: %s"
              (String.concat ~sep:"," x.switches)
              x.doc ;
            loop (f (bool_of_var var)) more
      in
      loop (action ~anon) options
    in
    let help_msg =
      sprintf "%s\n\nOptions:\n\n%s\n" !help_intro
        (String.concat ~sep:"\n" (List.rev !help))
    in
    let help_flag_var = ksprintf string "%s_help" prefix in
    let while_loop =
      let body =
        let append_anon_arg_to_list =
          (* This assumes knowledge of the internal representation of lists... *)
          seq
            [ anon_tmp#append (byte_array " ")
            ; anon_tmp#append
                ( Elist.make [getenv (string "1")]
                |> string_list_to_string |> to_byte_array ) ]
        in
        let help_case =
          let help_switches = ["-h"; "-help"; "--help"] in
          case
            (List.fold ~init:(bool false) help_switches ~f:(fun p s ->
                 p ||| C_string.(c_string s =$= getenv (c_string "1")) ))
            [ setenv help_flag_var (Bool.to_string (bool true))
            ; byte_array help_msg >> exec ["cat"]
            ; exec ["break"] ]
        in
        let dash_dash_case =
          case
            C_string.(getenv (c_string "1") =$= c_string "--")
            [ exec ["shift"]
            ; loop_while
                C_string.(getenv (c_string "#") <$> c_string "0")
                ~body:(seq [append_anon_arg_to_list; exec ["shift"]])
            ; exec ["break"] ]
        in
        let anon_case =
          case
            C_string.(getenv (c_string "#") <$> c_string "0")
            [append_anon_arg_to_list; exec ["shift"]]
        in
        let default_case = default [exec ["break"]] in
        let cases =
          (help_case :: List.rev !cases)
          @ [dash_dash_case; anon_case; default_case]
        in
        seq [switch cases]
      in
      loop_while (bool true) ~body
    in
    seq
      [ setenv help_flag_var (Bool.to_string (bool false))
      ; anon_tmp#set_c (string_list_to_string (Elist.make []))
      ; seq (List.rev !inits)
      ; while_loop
      ; if_then_else
          (bool_of_var (sprintf "%s_help" prefix))
          nop applied_action ]
end

let loop_until_true ?(attempts= 20) ?(sleep= 2)
    ?(on_failed_attempt= fun nth ->
                           printf (string "%d.") [Integer.to_string nth]) cmd =
  let intvar =
    let varname = string "C_ATTEMPTS" in
    object
      method set v = setenv ~var:varname (Integer.to_string v)
      method get = getenv varname |> Integer.of_string
    end
  in
  seq
    [ intvar#set (int 1)
    ; loop_while
        (Integer.(intvar#get <= int attempts) &&& not cmd)
        ~body:
          (seq
             [ on_failed_attempt intvar#get
             ; intvar#set Integer.(intvar#get + int 1)
             ; if_then
                 Integer.(intvar#get <= int attempts)
                 (exec ["sleep"; sprintf "%d" sleep]) ])
    ; exec ["printf"; "\\n"]
    ; if_then_else
        Integer.(intvar#get > int attempts)
        (seq [(* sprintf "Command failed %d times!" attempts; *) exec ["false"]])
        (seq [(* sprintf "Command failed %d times!" attempts; *) exec ["true"]])
    ]
  |> returns ~value:0

let silently u =
  let dev_null = string "/dev/null" in
  write_output ~stdout:dev_null ~stderr:dev_null u

let succeeds_silently u = silently u |> succeeds

let seq_and l = List.fold l ~init:(bool true) ~f:(fun u v -> u &&& succeeds v)

let output_markdown_code tag f =
  seq
    [ exec ["printf"; sprintf "``````````%s\\n" tag]
    ; f
    ; exec ["printf"; sprintf "\\n``````````\\n"] ]

let cat_markdown tag file =
  output_markdown_code tag @@ call [string "cat"; file]

let fresh_name suf =
  let x = object
            method v = 42
          end in
  sprintf "g-%d-%d-%s" (Oo.id x) (Random.int 100_000) suf

let sanitize_name n =
  String.map n ~f:(function
    | ('0'..'9' | 'a'..'z' | 'A'..'Z' | '-') as c -> c
    | other -> '_' )

let default_on_failure ~step:(i, u) ~stdout ~stderr =
  seq
    [ printf (ksprintf c_string "Step '%s' FAILED:\\n" i) []
    ; cat_markdown "stdout" stdout
    ; cat_markdown "stderr" stderr
    ; exec ["false"] ]

let check_sequence ?(verbosity= `Announce ">> ")
    ?(on_failure= default_on_failure)
    ?(on_success= fun ~step ~stdout ~stderr -> nop) ?(tmpdir= "/tmp") cmds =
  let tmp_prefix = fresh_name "-cmd" in
  let tmpout which id =
    c_string
      ( tmpdir
      // sprintf "genspio-check-sequence-%s-%s-%s" tmp_prefix which
           (sanitize_name id) )
  in
  let stdout id = tmpout "stdout" id in
  let stderr id = tmpout "stderr" id in
  let log id u =
    match verbosity with
    | `Silent -> write_output ~stdout:(stdout id) ~stderr:(stderr id) u
    | `Announce prompt ->
        seq
          [ printf (ksprintf c_string "%s %s\\n" prompt id) []
          ; write_output ~stdout:(stdout id) ~stderr:(stderr id) u ]
    | `Output_all -> u
  in
  let check idx (nam, u) next =
    let id = sprintf "%d. %s" idx nam in
    if_seq
      (log id u |> succeeds)
      ~t:
        [on_success ~step:(id, u) ~stdout:(stdout id) ~stderr:(stderr id); next]
      ~e:[on_failure ~step:(id, u) ~stdout:(stdout id) ~stderr:(stderr id)]
  in
  let rec loop i = function
    | one :: more -> check i one (loop (i + 1) more)
    | [] -> exec ["true"]
  in
  loop 1 cmds

let on_stdin_lines body =
  let fresh = Common.Unique_name.variable "read_stdin" in
  loop_while
    (exec ["read"; "-r"; fresh] |> succeeds)
    ~body:(seq [exec ["export"; fresh]; body (getenv (string fresh))])
