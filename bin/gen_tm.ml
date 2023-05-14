open Machine
(*
sSTsI1s1RI+s1RI.e.LTeI1h.RI.h.R|111+11
[seh]STsI1s1RI+s1RI.e.LTeI1h.RI.h.R|111+11
[seh]STsI1s1RI+s1RI.e.LTeI1h.RI.h.R|C11+11

first char of input is the initial state
the 'S' is the start of transitions definition
the 'T' is the start of a transition definition of the state specified as the next char
the 'I' is the start of an instruction in the transition, next 4 char are in order | read | to_state | write | action |
the '|' is the start of the band
the 'C' is the actual cursor
*)

let name = "tm_unary_add"

let tm_states = ['s'; 'e']
let tm_final = 'h'
let tm_blank = '.'
let tm_alphabet = ['1'; '+'; tm_blank]

let cursor = 'C'
let states_mark = 'S'
let transitions_mark = 'T'
let instruction_mark = 'I'
let action_right = 'R'
let action_left = 'L'
let band_mark = '|'
let blank = ' '
let alphabet = [ cursor; states_mark; transitions_mark; instruction_mark; action_right; action_left; band_mark; blank ] @ tm_states @ [ tm_final ] @ tm_alphabet
let initial = "init"
let final = "HALT"

let right = true
let left = false

let gen_init =
    {
        transition_name = "init";
        instructions = ((List.map (fun tm_state ->
            {
                read = tm_state;
                to_state = "init_" ^ (String.make 1 tm_state);
                write = tm_state;
                action = right
            }) tm_states) @
            [
                {
                    read = tm_final;
                    to_state = final;
                    write = tm_final;
                    action = right
                }
            ])
    }

let gen_init_state = List.map (fun tm_state ->
    let tran_name = "init_" ^ (String.make 1 tm_state) in
    {
        transition_name = tran_name;
        instructions = List.map (fun al ->
            if al = band_mark then
                { read = al; to_state = "init_" ^ (String.make 1 tm_state) ^ "_get_val"; write = al; action = right }
            else
                { read = al; to_state = tran_name; write = al; action = right }
        ) alphabet
    }) tm_states

let gen_end_return = List.map (fun tm_state ->
    {
        transition_name = "end_return_" ^ (String.make 1 tm_state);
        instructions = (List.map (fun al ->
            {
                read = al;
                to_state = "init_" ^ (String.make 1 tm_state) ^ "_get_val";
                write = al;
                action = left
            }
        ) alphabet)
    }) tm_states

let gen_init_state_get_val = List.map (fun tm_state ->
    let tran_name = "init_" ^ (String.make 1 tm_state) ^ "_get_val" in
    {
        transition_name = tran_name;
        instructions = (List.map (fun al ->
            {
                read = al;
                to_state = "go_to_states_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 al);
                write = cursor;
                action = left
            }
        ) tm_alphabet) @
        [
            { read = blank; to_state = "end_return_" ^ (String.make 1 tm_state); write = tm_blank; action = right };
            { read = band_mark; to_state = "init_" ^ (String.make 1 tm_state) ^ "_get_val"; write = band_mark; action = right }
        ]
    }) tm_states

let gen_go_to_states_state_val =
    List.concat_map (fun tm_state ->
        List.map (fun v ->
            let tran_name = "go_to_states_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v) in
            {
                transition_name = tran_name;
                instructions = List.map (fun al ->
                    if al = states_mark then
                        { read = al; to_state = "find_state_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v); write = al; action = right }
                    else
                        { read = al; to_state = tran_name; write = al; action = left }
                ) alphabet
            }
        ) tm_alphabet
    ) tm_states

let gen_find_state_state_val =
    List.concat_map (fun tm_state ->
        List.map (fun v ->
            let tran_name = "find_state_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v) in
            {
                transition_name = tran_name;
                instructions = List.map (fun al ->
                    if al = transitions_mark then
                        { read = al; to_state = "good_state_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v); write = al; action = right }
                    else if al = band_mark then
                        { read = al; to_state = final; write = al; action = right }
                    else
                        { read = al; to_state = tran_name; write = al; action = right }
                ) alphabet
            }
        ) tm_alphabet
    ) tm_states

let gen_good_state_state_val =
    List.concat_map (fun tm_state ->
        List.map (fun v ->
            {
                transition_name = "good_state_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v);
                instructions = List.map (fun al ->
                    if al = tm_state then
                        { read = al; to_state = "find_instrs_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v); write = tm_state; action = right }
                    else if al = band_mark then
                        { read = al; to_state = final; write = al; action = right }
                    else
                        { read = al; to_state = "find_state_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v); write = al; action = right }
                ) alphabet
            }
        ) tm_alphabet
    ) tm_states

let gen_find_instrs_state_val =
    List.concat_map (fun tm_state ->
        List.map (fun v ->
            let tran_name = "find_instrs_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v) in
            {
                transition_name = tran_name;
                instructions = List.map (fun al ->
                    if al = instruction_mark then
                        { read = al; to_state = "find_instr_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v); write = al; action = right }
                    else if al = transitions_mark then
                        { read = al; to_state = final; write = al; action = right }
                    else if al = band_mark then
                        { read = al; to_state = final; write = al; action = right }
                    else
                        { read = al; to_state = tran_name; write = al; action = right }
                ) alphabet
            }
        ) tm_alphabet
    ) tm_states

let gen_find_instr_state_val =
    List.concat_map (fun tm_state ->
        List.map (fun v ->
            {
                transition_name = "find_instr_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v);
                instructions = List.map (fun al ->
                    if al = v then
                        { read = al; to_state = "get_instr"; write = al; action = right }
                    else
                        { read = al; to_state = "find_instrs_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v); write = al; action = right }
                ) tm_alphabet
            }
        ) tm_alphabet
    ) tm_states

let gen_get_instr =
    {
        transition_name = "get_instr";
        instructions = (List.map (fun al ->
            { read = al; to_state = "get_instr_" ^ (String.make 1 al); write = al; action = right }
        ) tm_states) @ [ { read = tm_final; to_state = "get_instr_HALT"; write = tm_final; action = right } ]
    }

let gen_get_instr_state =
    List.map (fun tm_state ->
        {
            transition_name = "get_instr_" ^ (String.make 1 tm_state);
            instructions = List.map (fun al ->
                {
                    read = al;
                    to_state = "get_instr_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 al);
                    write = al;
                    action = right
                }
            ) tm_alphabet
        }
    ) tm_states

let gen_get_instr_HALT =
    {
        transition_name = "get_instr_HALT";
        instructions = List.map (fun al ->
            {
                read = al;
                to_state = "instr_HALT_" ^ (String.make 1 al);
                write = al;
                action = right
            }
        ) tm_alphabet
    }

let gen_get_instr_state_val =
    List.concat_map (fun tm_state ->
        List.map (fun v ->
            {
                transition_name = "get_instr_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v);
                instructions =
                    [
                        {
                            read = action_right;
                            to_state = ("instr_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v) ^ "_R");
                            write = action_right;
                            action = right
                        };
                        {
                            read = action_left;
                            to_state = ("instr_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v) ^ "_L");
                            write = action_left;
                            action = right
                        }
                    ]
            }
        ) tm_alphabet
    ) tm_states

let gen_instr_state_val_r =
    List.concat_map (fun tm_state ->
        List.map (fun v ->
            {
                transition_name = "instr_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v) ^ "_R";
                instructions = List.map (fun al ->
                    if al = cursor then
                        { read = al; to_state = "init_" ^ (String.make 1 tm_state) ^ "_get_val"; write = v; action = right }
                    else
                        { read = al; to_state = "instr_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v) ^ "_R"; write = al; action = right }
                ) alphabet
            }
        ) tm_alphabet
    ) tm_states

let gen_instr_HALT_val =
    List.map (fun v ->
        {
            transition_name = "instr_HALT_" ^ (String.make 1 v);
            instructions = List.map (fun al ->
                if al = cursor then
                    { read = al; to_state = final; write = v; action = right }
                else
                    { read = al; to_state = "instr_HALT_" ^ (String.make 1 v); write = al; action = right }
            ) alphabet
        }
    ) tm_alphabet

let gen_instr_state_val_l =
    List.concat_map (fun tm_state ->
        List.map (fun v ->
            {
                transition_name = "instr_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v) ^ "_L";
                instructions = List.map (fun al ->
                    if al = cursor then
                        { read = al; to_state = "init_" ^ (String.make 1 tm_state) ^ "_get_val"; write = v; action = left }
                    else
                        { read = al; to_state = "instr_" ^ (String.make 1 tm_state) ^ "_" ^ (String.make 1 v) ^ "_L"; write = al; action = right }
                ) alphabet
            }
        ) tm_alphabet
    ) tm_states

let gen_tm_transitions =
    [gen_init] @
    gen_init_state @
    gen_end_return @
    gen_init_state_get_val @
    gen_go_to_states_state_val @
    gen_find_state_state_val @
    gen_good_state_state_val @
    gen_find_instrs_state_val @
    gen_find_instr_state_val @
    [gen_get_instr] @
    gen_get_instr_state @
    [gen_get_instr_HALT] @
    gen_get_instr_state_val @
    gen_instr_state_val_r @
    gen_instr_HALT_val @
    gen_instr_state_val_l

let gen_tm =
    let transitions = gen_tm_transitions in
    let transitions_name = (List.map (fun tran -> tran.transition_name) transitions) @ [final] in
    (* machine *)
    {
        name = name;
        alphabet = List.map (fun al -> String.make 1 al) alphabet;
        blank = blank;
        states = transitions_name;
        initial = initial;
        finals = [final];
        transitions = transitions;
    }
