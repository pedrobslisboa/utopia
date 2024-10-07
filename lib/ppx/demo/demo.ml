module Grade = struct
  type t = HighSchool | College | Master | Phd

  let of_string_opt = function
    | "high-school" -> Some HighSchool
    | "college" -> Some College
    | "master" -> Some Master
    | "phd" -> Some Phd
    | _ -> None

  let to_string = function
    | HighSchool -> "high-school"
    | College -> "college"
    | Master -> "master"
    | Phd -> "phd"
end

module CustomName = struct
  type t = string

  let of_string_opt = function
    | ("pedro" | "dave" | "jave") as name -> Some name
    | _ -> None

  let to_string = function
    | ("pedro" | "dave" | "jave") as name -> name
    | _ -> "unknown"
end

module Profile = struct
  type t = { name : CustomName.t; age : int; grade : Grade.t option }
end [@route "/profile/:name/:age"]

let _ = print_endline Profile.route

(* Should match *)
let _ =
  match Profile.of_path "/profile/pedro/30?grade=college" with
  | Some { name; age; grade = Some grade } ->
      Printf.printf "name: %s, age: %d, grade: %s\n" name age
        (grade |> Grade.to_string)
  | _ -> print_endline "no match"

(* Should not match *)
let _ =
  match Profile.of_path "/profile/foo/10?grade=college" with
  | Some { name; age; grade = Some grade } ->
      Printf.printf "name: %s, age: %d, grade: %s\n" name age
        (grade |> Grade.to_string)
  | _ -> print_endline "no match"

let _ =
  print_endline
    (Profile.to_path { name = "pedro"; age = 30; grade = Some HighSchool })
let _ =
  print_endline
    (Profile.to_path { name = "pedro"; age = 30; grade = None })
