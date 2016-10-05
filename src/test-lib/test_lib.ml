
open Nonstd
module String = Sosa.Native_string
open Pvem_lwt_unix.Deferred_result

let check_command s ~verifies =
  Pvem_lwt_unix.System.Shell.execute s
  >>= fun (out, err, exit_status) ->
  List.fold verifies ~init:(return []) ~f:(fun prev_m v ->
      prev_m >>= fun prev ->
      match v with
      | `Exits_with i ->
        let l =
          if exit_status = `Exited i
          then (true, "exited well") :: prev
          else (
            false,
            sprintf "%s:\nout:\n%s\nerr:\n%s"
              (Pvem_lwt_unix.System.Shell.status_to_string exit_status)
              out
              err
          ) :: prev
        in
        return l)
  >>= fun results ->
  List.filter ~f:(fun (t, _) -> t = false) results |> return

let command ?name ?(args = []) s ~verifies = `Command (name, s, args, verifies)

let run_with_shell ~shell l =
  Pvem_lwt_unix.Deferred_list.while_sequential l ~f:(function
    | `Command (name, s, args, verifies) ->
      check_command (shell s args) ~verifies
      >>= begin function
      | [] ->
        return None
      | failures ->
        return (Some (
            (sprintf "#### Command%s:\n%s\n#### Args: %s\n#### Failures:\n%s\n"
               (Option.value_map name ~default:"" ~f:(sprintf " “%s”"))
               s
               (String.concat ~sep:" " args)
               (List.map failures ~f:(fun (_, msg) -> sprintf "* %s" msg)
                |> String.concat ~sep:"\n"))))
      end)
  >>= fun l ->
  let failures = List.filter_opt l in
  return (`Total (List.length l), `Failures failures)

type shell = {
  executable: string [@main ];
  command: string -> string list -> string;
  get_version: string;
} [@@deriving make]

let avaialable_shells () =
  let exec l =
    List.map ~f:Filename.quote l |> String.concat ~sep:" " in
  let dash_like bin ~get_version =
    make_shell bin
      ~command:(fun s args -> exec ([bin; "-x"; "-c"; s; "--"] @ args))
      ~get_version
  in
  let busybox =
    make_shell "busybox"
      ~command:(fun s args -> exec (["busybox"; "ash"; "-x"; "-c"; s; "--"] @ args))
      ~get_version:"busybox | head -n 1"
  in
  let package_version package =
    (* for when there is no `--version`, `-V`, etc. we go the “debian” way *)
    sprintf "dpkg -s %s | grep ^Version" package in
  let candidates = [
    dash_like "dash" ~get_version:(package_version "dash");
    dash_like "bash" ~get_version:"bash --version | head -n 1";
    dash_like "sh" ~get_version:(package_version "sh");
    busybox;
    dash_like "ksh" ~get_version:"ksh --version 2>&1";
    dash_like "mksh" ~get_version:(package_version "mksh");
    dash_like "posh" ~get_version:(package_version "posh");
    dash_like "zsh" ~get_version:"zsh --version";
  ] in
  let forgotten = ref [] in
  Pvem_lwt_unix.Deferred_list.while_sequential candidates ~f:(fun sh ->
      Pvem_lwt_unix.System.Shell.execute (sprintf "which %s" sh.executable)
      >>= function
      | (_, _, `Exited 0) ->
        Pvem_lwt_unix.System.Shell.execute sh.get_version
        >>= fun (version, _, _) ->
        return (Some (sh, String.strip version))
      | _ -> forgotten := sh.executable :: !forgotten; return None)
  >>| List.filter_opt
  >>= fun l ->
  return (l, !forgotten)

let run l =
  let important_shells = ["bash"; "dash"; "busybox"] in
  avaialable_shells ()
  >>= fun (shells, forgotten) ->
  Pvem_lwt_unix.Deferred_list.while_sequential shells
    ~f:begin fun (shell, version) ->
      let start = Unix.gettimeofday () in
      run_with_shell ~shell:shell.command l
      >>= fun (`Total total, `Failures failures) ->
      let finish = Unix.gettimeofday () in
      return (`Shell shell, `Version version,
              `Total total, `Failures failures, `Time (finish -. start))
    end
  >>= fun test_results ->
  printf "\n%s\n" (String.make 80 '-');
  printf "\n\n### All Tests\n\nSummary:\n\n%!";
  Pvem_lwt_unix.Deferred_list.while_sequential test_results
    ~f:begin fun (`Shell sh, `Version v, `Total t, `Failures fl, `Time dur) ->
      printf "* Test %S (`%s`):\n    - %d / %d failures\n%!"
        sh.executable (sh.command "<command>" ["<arg1>"; "<arg2>"; "<arg-n>"])
        (List.length fl) t;
      printf "    - time: %0.2f s.\n%!" dur;
      printf "    - version: `%S`.\n%!" v;
      begin match fl with
      | [] -> return ()
      | more ->
        let content = String.concat fl ~sep:"\n\n\n" in
        let path =
          sprintf "/tmp/genspio-test-%s-failures.txt" sh.executable in
        Pvem_lwt_unix.IO.write_file path ~content
        >>= fun () ->
        printf "    - Cf. `%s`.\n%!" path;
        return ()
      end
    end
  >>= fun _ ->
  begin match forgotten with
  | [] ->
    printf "\nAll “known” shells were tested ☺\n%!"
  | more ->
    printf "\nSome shells were not found hence not tested: %s.\n%!"
      (String.concat ~sep:", " more)
  end;
  printf "\n%!";
  printf "\n%s\n\n" (String.make 80 '-');
  let actuall_success =
    if List.exists important_shells (List.mem ~set:forgotten)
    then `Failed "Some important shells were not found"
    else if List.exists test_results ~f:(function
      | `Shell sh, _, _, `Failures fl, `Time _ ->
        List.mem sh.executable ~set:important_shells && fl <> [])
    then `Failed "Some important shells had failed tests" 
    else `Succeeded
  in
  return actuall_success
