let () =
    if Array.length Sys.argv <= 1 then
    (
        print_endline "not enough arg"
    )
    else
    (
        try
        (
            let machine = Machine.create Sys.argv.(1) in
            Machine.display machine
        )
        with e ->
            let msg = Printexc.to_string e
            and stack = Printexc.get_backtrace () in
            Printf.eprintf "THERE WAS AN ERROR: %s%s\n" msg stack;
        (*
        with _ -> print_endline "json error";
        *)
    )
