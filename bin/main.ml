let display_help() =
        print_endline "usage: ft_turing [-h] jsonfile input\n\npositional arguments:\n  jsonfile            json description of the machine\n\n  input               input of the machine\n\noptional arguments:\n  -h, --help          show this help message and exit\n  -g, --gen          generate mt_unary_add.json\n  -s                 compute space complexity\n  -t                 compute time complexity"

let check_stop_flag =
    if Array.length Sys.argv >= 2 && (Sys.argv.(1) = "--help" || Sys.argv.(1) = "-h") then
    (
        display_help();
        true
    )
    else if Array.length Sys.argv >= 2 && (Sys.argv.(1) = "--gen" || Sys.argv.(1) = "-g") then
    (
        try
        (
            let machine = Gen_tm.gen_tm in
            Machine.display machine;
            true
        )
        with e ->
            let msg = Printexc.to_string e in
            Printf.eprintf "\027[31mERROR\027[0m: %s\n" msg;
        true
    )
    else if Array.length Sys.argv != 3 && (Array.length Sys.argv != 4 || (not (Sys.argv.(3) = "-t") && not (Sys.argv.(3) = "-s"))) then
    (
        display_help();
        true
    )
    else
        false

let () =
    if check_stop_flag = false then
    (
        try
        (
            let machine = Machine.create Sys.argv.(1) in
            let opt =
                (if Array.length Sys.argv = 4 then
                    (
                    if Sys.argv.(3) = "-t" then
                        1
                    else if Sys.argv.(3) = "-s" then
                        2
                    else
                        0
                    )
                else
                    0) in
            let head = Head.create machine Sys.argv.(2) opt in
            Machine.simple_display machine;
            Head.run head
        )
        with e ->
            let msg = Printexc.to_string e in
            Printf.eprintf "\027[31mERROR\027[0m: %s\n" msg;
    )
    else
    (
    )
