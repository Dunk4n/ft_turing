type instruction =
{
    read : char;
    to_state : string;
    write : char;
    action : bool;
}

type transition =
{
    transition_name : string;
    instructions : instruction list;
}

type machine =
{
    name : string;
    alphabet : string list;
    blank : char;
    states : string list;
    initial : string;
    finals : string list;

    transitions : transition list;
}

let display_instruction instruction =
    Printf.printf "            { \"read\" : \"%c\", \"to_state\" : \"%s\", \"write\" : \"%c\", \"action\" : \"%s\" }" instruction.read instruction.to_state instruction.write (if instruction.action then "RIGHT" else "LEFT")

let display_transition transition =
    Printf.printf "        \"%s\":\n        [\n" transition.transition_name;
    for cnt = 0 to (List.length transition.instructions) -1 do
        display_instruction (List.nth transition.instructions cnt);
        if cnt < ((List.length transition.instructions) - 1) then
            Printf.printf ",";
        Printf.printf "\n"
    done;
    Printf.printf "        ]"

let display machine =
    Printf.printf "{\n    \"name\" : \"%s\",\n" machine.name;
    Printf.printf "    \"alphabet\" : [";
    for cnt = 0 to (List.length machine.alphabet) - 1 do
        Printf.printf " \"%s\"" (List.nth machine.alphabet cnt);
        if cnt < ((List.length machine.alphabet) - 1) then
            Printf.printf ","
    done;
    Printf.printf " ],\n";
    Printf.printf "    \"blank\" : \"%c\",\n" machine.blank;
    Printf.printf "    \"states\" : [";
    for cnt = 0 to (List.length machine.states) -1 do
        Printf.printf " \"%s\"" (List.nth machine.states cnt);
        if cnt < ((List.length machine.states) - 1) then
            Printf.printf ","
    done;
    Printf.printf " ],\n";
    Printf.printf "    \"initial\" : \"%s\",\n" machine.initial;
    Printf.printf "    \"finals\" : [";
    for cnt = 0 to (List.length machine.finals) - 1 do
        Printf.printf " \"%s\"" (List.nth machine.finals cnt);
        if cnt < ((List.length machine.finals) - 1) then
            Printf.printf ","
    done;
    Printf.printf " ],\n    \"transitions\" :\n    {\n";
    for cnt = 0 to (List.length machine.transitions) -1 do
        display_transition (List.nth machine.transitions cnt);
        if cnt < ((List.length machine.transitions) - 1) then
            Printf.printf ",";
        Printf.printf "\n"
    done;
    Printf.printf "    }\n}\n"

let simple_display_instruction name instruction =
    Printf.printf "(%s, %c) -> (%s, %c, %s)\n" name instruction.read instruction.to_state instruction.write (if instruction.action = true then "RIGHT" else "LEFT")

let simple_display machine =
    Printf.printf "********************************************************************************\n";
    Printf.printf "*                              %*s                                       *\n" (String.length machine.name) " ";
    Printf.printf "*                              %s                                       *\n" machine.name;
    Printf.printf "*                              %*s                                       *\n" (String.length machine.name) " ";
    Printf.printf "********************************************************************************\n";
    Printf.printf "Alphabet : [";
    for cnt = 0 to (List.length machine.alphabet) - 1 do
        if (cnt < (List.length machine.alphabet) - 1) then
            Printf.printf " %s," (List.nth machine.alphabet cnt)
        else
            Printf.printf " %s" (List.nth machine.alphabet cnt)
    done;
    Printf.printf " ]\n";
    Printf.printf "States : [";
    for cnt = 0 to (List.length machine.states) -1 do
        if (cnt < (List.length machine.states) - 1) then
            Printf.printf " %s," (List.nth machine.states cnt)
        else
            Printf.printf " %s" (List.nth machine.states cnt)
    done;
    Printf.printf " ]\n";
    Printf.printf "Initial : %s\n" machine.initial;
    Printf.printf "Finals : [";
    for cnt = 0 to (List.length machine.finals) - 1 do
        if (cnt < (List.length machine.finals) - 1) then
            Printf.printf " %s," (List.nth machine.finals cnt)
        else
            Printf.printf " %s" (List.nth machine.finals cnt)
    done;
    Printf.printf " ]\n";
    List.iter (fun transition -> (List.iter (fun instruction -> simple_display_instruction transition.transition_name instruction) transition.instructions)) machine.transitions;
    Printf.printf "********************************************************************************\n"

exception Invalid_json_machine of string

let rec uniq list =
    match list with
    | [] -> true
    | h::t -> if (List.mem h t) then false else uniq t

let check_alphabet alphabet =
    (* TODO check alphabet elm is displayable ? *)
    if (List.length alphabet) <= 1 then
        false
    else if List.exists (fun elm -> String.length elm != 1) alphabet then
        false
    else if uniq alphabet = false then
        false
    else
        true

let check_states states =
    if (List.length states) <= 1 then
        false
    else if List.exists (fun elm -> String.length elm = 0) states then
        false
    else if uniq states = false then
        false
    else
        true

let check_finals finals states =
    if (List.length finals) = 0 then
        false
    else if uniq finals = false then
        false
    else if (List.exists (fun elm -> (List.mem elm states) = false) finals) then
        false
    else
        true

let check_transitions_name transitions_name finals states =
    if (List.length transitions_name) = 0 then
        false
    else if (List.length transitions_name) > (List.length states) then
        false
    else if uniq transitions_name = false then
        false
    else if List.exists (fun elm -> List.mem elm finals) transitions_name = true then
        false
    (* check if all transitions are in states *)
    else if (List.exists (fun elm -> (List.mem elm states) = false) transitions_name) then
        false
    else
        true

let get_instruction json_instruction states alphabet =
    let open Yojson.Basic.Util in
    let read = json_instruction |> member "read" |> to_string in
    let to_state = json_instruction |> member "to_state" |> to_string in
    let write = json_instruction |> member "write" |> to_string in
    let action = json_instruction |> member "action" |> to_string in
    if (String.length read) != 1 || (String.length write) != 1 then
        raise (Invalid_json_machine "instruction of transition invalide or missing");
    if (action <> "RIGHT") && (action <> "LEFT") then
        raise (Invalid_json_machine "instruction of transition invalide or missing");
    if (not (List.mem read alphabet)) || (not (List.mem write alphabet)) then
        raise (Invalid_json_machine "instruction of transition invalide or missing");
    if not (List.mem to_state states) then
        raise (Invalid_json_machine "instruction of transition invalide or missing");
    {
        read = read.[0];
        to_state;
        write = write.[0];
        action = (if action = "RIGHT" then true else false);
    }

let create machine_json_filepath =
    let open Yojson.Basic.Util in
    let json = Yojson.Basic.from_file machine_json_filepath in
    let name = json |> member "name" |> to_string in
    let alphabet = json |> member "alphabet" |> to_list |> filter_string in
    let blank = json |> member "blank" |> to_string in
    let states = json |> member "states" |> to_list |> filter_string in
    let initial = json |> member "initial" |> to_string in
    let finals = json |> member "finals" |> to_list |> filter_string in

    let json_transitions = json |> member "transitions" in
    let transitions_name = json_transitions |> keys in

    if String.length name = 0 then
        raise (Invalid_json_machine "machine name invalide or missing")
    else if (check_alphabet alphabet) = false then
        raise (Invalid_json_machine "alphabet invalide or missing")
    else if String.length blank <> 1 || (List.mem blank alphabet) = false then
        raise (Invalid_json_machine "blank invalide or missing")
    else if (check_states states) = false then
        raise (Invalid_json_machine "states invalide or missing")
    else if (List.mem initial states) = false then
        raise (Invalid_json_machine "initial invalide or missing")
    else if (check_finals finals states) = false then
        raise (Invalid_json_machine "finals invalide or missing")
    else if (check_transitions_name transitions_name finals states) = false then
        raise (Invalid_json_machine "transitions invalide or missing")
    else
    (
        let transitions = List.init (List.length transitions_name) (fun pos ->
            let json_transitions = (json_transitions |> member (List.nth transitions_name pos) |> to_list) in
            {
                transition_name = (List.nth transitions_name pos);
                instructions = (List.init (List.length json_transitions) (fun pos_instruction ->
                    get_instruction (List.nth json_transitions pos_instruction) states alphabet
                ));
            }
        ) in
        let machine =
        {
            name;
            alphabet;
            blank = String.get blank 0;
            states;
            initial;
            finals;
            transitions;
        } in
        (* check if all states are defined *)
        if (List.exists (fun state ->
            (List.mem state machine.finals) = false &&
            (List.exists (fun transition -> state = transition.transition_name) machine.transitions) = false
        ) machine.states) then
            raise (Invalid_json_machine "state missing definition")
        else
            machine
    )
