open Machine

type head =
{
    mutable band : string;
    mutable index : int;

    machine : Machine.machine;
    mutable actual_state : string;

    mutable watch_dog : int;

    opt_compute_time : bool;
    mutable time_cnt : int;
    opt_compute_space : bool;
    mutable space_cnt : int;
}

exception Invalid_input of string
exception Invalid_instruction of string

let display head =
    Printf.printf "BAND: [%s]\n" head.band;
    Printf.printf "index: %d\n" head.index;
    Printf.printf "state: %s\n" head.actual_state

let rec get_blank_padd n blank =
    if n <= 1 then (String.make 1 blank) else (String.make 1 blank) ^ (get_blank_padd (n - 1) blank)

let fill_band band blank =
    if String.length band >= 20 then
        band
    else
    (
        let to_add = 20 - (String.length band) in
        band ^ (get_blank_padd to_add blank)
    )

let create machine band opt =
    if (String.exists (fun c -> ((List.exists (fun s -> (String.get s 0) = c) machine.alphabet) = false)) band) then
        raise (Invalid_input "Input characters are not part of the alphabet")
    else
    (
        let head = {
            band;
            index = 0;
            machine;
            actual_state = machine.initial;
            watch_dog = 10000;
            opt_compute_time = (if opt = 1 then true else false);
            time_cnt = 0;
            opt_compute_space = (if opt = 2 then true else false);
            space_cnt = 0;
        } in
        head.band <- fill_band head.band head.machine.blank;
        head
    )

let set_char_in_band head next_state new_char dir =
    let actual_char = (String.get head.band head.index) in
    let first_part = String.sub head.band 0 head.index in
    let last_part = String.sub head.band (head.index + 1) (String.length head.band - (head.index + 1)) in
    (* display *)
    Printf.printf "[%s\027[5;92m%c\027[0m%s] (%s, %c) -> (%s, %c, %s)\n" first_part actual_char last_part head.actual_state (String.get head.band head.index) next_state new_char (if dir = true then "RIGHT" else "LEFT");
    (* compute space or time complexity *)
    if head.opt_compute_space = true && actual_char = head.machine.blank then
        head.space_cnt <- head.space_cnt + 1;
    if head.opt_compute_time = true then
        head.time_cnt <- head.time_cnt + 1;
    (* Set new char in band *)
    head.band <- first_part ^ (String.make 1 new_char) ^ last_part

let get_next_instruction head =
    let actual_char = (String.get head.band head.index) in
    let transition = List.find (fun tran -> tran.transition_name = head.actual_state) head.machine.transitions in
    try
    (
        let instruction = List.find (fun inst -> inst.read = actual_char) transition.instructions in
        instruction.to_state,
        instruction.write,
        instruction.action
    )
    with _ -> raise (Invalid_instruction (Printf.sprintf "No instruction in transition '%s' for character '%c'" head.actual_state actual_char))

let go_right head =
    head.index <- head.index + 1;
    if head.index >= (String.length head.band) then
        head.band <- (head.band ^ (String.make 1 head.machine.blank))

let go_left head =
    if head.index = 0 then
        raise (Invalid_instruction "tying to go LEFT before start")
    else
        head.index <- head.index - 1

let run head =
    while not (List.mem head.actual_state head.machine.finals) do
        let next_state, new_char, dir = get_next_instruction head in
        set_char_in_band head next_state new_char dir;
        head.actual_state <- next_state;
        if dir = true then go_right head else go_left head;

        head.watch_dog <- head.watch_dog - 1;
        if head.watch_dog = 0 then
            raise (Invalid_instruction "Too many instruction executed must be lost in loop");
    done;
    Printf.printf "[%s]\n" head.band;
    if head.opt_compute_space = true then
        Printf.printf "space complexity: %d\n" head.space_cnt
    else if head.opt_compute_time = true then
        Printf.printf "time complexity: %d\n" head.time_cnt
