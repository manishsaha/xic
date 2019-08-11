open Core
open Lexing

let _DEBUG_ = ref 0
let debug level format =
  ksprintf (fun s -> if !_DEBUG_ >= level then Printf.printf "%s\n%!" s else ()) format

module Hash_table = Hashtbl

type id = string [@@deriving sexp]
type escaped_char = Uchar.t
type location = string * int * int
type error_info = {
  err_type: string;
  err_msg: string;
  err_loc: location;
}

module type ConfigSpec = sig
  val in_path : id
  val diag_out_path : id
  val asm_out_path : id
  val diag_flags : (id * bool) list
  val diag_info : (id, id * [ `Unvisited | `Visited ]) Hash_table.t
  val libs : (id * id) list
  val disable_all_opts : bool
  val no_reorder : bool
  val irgen : bool
  val irrun : bool
  val optir_flags : (id, bool) Hash_table.t
  val optcfg_flags : (id, bool) Hash_table.t
  val enabled_opts : (id, bool) Hash_table.t
  val noextension : bool
end

let _SUPPORTED_OPTS_ = ["cf"; "reg"; "mc"; "cse"; "copy"; "dce"]

exception Xic_error of error_info

let compose = Fn.compose
let non = Fn.non
let const = Fn.const

let is_pow2 n = Int64.(n > 0L) && Int64.popcount n = 1
let floor_log2 n =
  let rec floor_log2 n cur =
    Int64.(if n < 2L then cur else floor_log2 (n / 2L) (succ cur))
  in
  floor_log2 n 0L

let int64_of_bigint i = Bigint.to_int64_exn i

let escape_string s =
  let s = String.escaped s in
  String.Search_pattern.(replace_all (create "'") ~in_:s ~with_:"\\'")

(* Returns escape string for a given character, represented as a uchar.
 * ASCII characters become actual chars, but general Unicode are encoded
 * as "\\xh{1-4}" where h is a hex digit. *)
let string_of_uchar u =
  Base.(
    if Uchar.is_char u
    then u |> Uchar.to_char_exn |> Char.to_string |> escape_string
    else Printf.sprintf "\\x%x" (Uchar.to_scalar u)
  )

(* Concatenate list of escaped Unicode characters given input [uchars]. *)
let string_of_uchar_list uchars =
  let escapes = List.map uchars ~f:string_of_uchar in
  escapes |> String.concat

(* Concatenate array of escaped Unicode characters given input [uchars]. *)
let string_of_uchar_array uchars =
  uchars |> Array.to_list |> string_of_uchar_list

(* Concatenate list of strings with spaces separating. *)
let spacify_list = String.concat ~sep:" "

(* Concatenate list of strings with spaces separating. *)
let spacify_map ~f = compose (String.concat ~sep:" ") (List.map ~f)

let encode_id id =
  String.Search_pattern.replace_all (String.Search_pattern.create "_")
    ~in_:id ~with_:"__"

(* Generate a valid path from an optional [dir] *)
let make_path dir =
  match dir with
  | None -> "./"
  | Some s -> begin
      if s.[String.length s - 1] <> '/'
      then s ^ "/" else s (* allow for trailing '/' in designated path *)
    end

(* Return true if [ext] represents a valid Xi program file extension *)
let valid_ext ext =
  ext = "xi" || ext = "ixi"

(* Return the extension of a file, or empty string if no extension found
   e.g. get_ext "foo/bar/baz.txt" -> "txt"
   e.g. get_ext "foo/bar/baz" -> "" *)
let get_ext file =
  match snd Filename.(basename file |> split_extension) with
  | None -> ""
  | Some s -> s

(* Return the name of a file WITHOUT the extension, or no-op if no extension found.
   e.g. remove_ext "foo/asdf.txt" -> "foo/asdf" *)
let remove_ext file =
  fst (Filename.split_extension file)

(* Return the name of [file] with the extension removed. *)
let base_filename file =
  Filename.basename file |> remove_ext

(* Return filename from relative path 
   e.g. get_filename "foo/bar/baz.txt" -> "baz.txt"
   e.g. get_filename "foo.txt" -> "foo.txt" *)
let get_filename path =
  let open String in
  try 
    let len = length path in
    let s = rindex_exn path '/' + 1 in
    sub path ~pos:s ~len:(len-s)
  with _ -> path

(* Return an associative list mapping interface names to interface paths
 * for all interface files in [lib_path]. *)
let get_libs lib_path =
  let files = Sys.ls_dir lib_path in
  List.fold files ~init:[] ~f:(fun ints file ->
      let ext = get_ext file in
      let name = base_filename file in
      if ext <> "ixi" then ints
      else
        let path = Filename.concat lib_path file in
        (name, path) :: ints
    )
  |> List.rev

(* Translates lexing position [p] into a location (filename, row, col). *)
let location_of_position p =
  (p.pos_fname, p.pos_lnum, p.pos_cnum - p.pos_bol + 1)

(* Handle exception [exn], printing errors to [cout]. *)
let handle_exn err_msg l c cout =
  Out_channel.fprintf cout "%d:%d error: %s\n" l c err_msg

(* Global counters for generating unique names *)
let var_count = ref 0
let label_count = ref 0

(* Increments [counter] and return a new unique name *)
let make_new_name counter prefix =
  incr counter;
  prefix ^ (Int.to_string !counter)

(* Thunk to increment the global counter and return a new unique temporary *)
let make_new_tmp () = `IRTemp (make_new_name var_count "t")

(* Thunk to increment the global counter and return a new unique label *)
let make_new_label () = make_new_name label_count "l"

(* Thunk to increment the global counter and return a new unique abstract reg *)
let make_new_abs_reg () = `AbsReg (make_new_name var_count "t")

let nth_arg n = "_ARG" ^ Int.to_string n
let nth_ret n = "_RET" ^ Int.to_string n

module Poly_set = struct
  include Hash_set

  let create m = create m ()

  let union_inplace (s1 : 'a t) (s2 : 'a t) : unit = iter s2 ~f:(add s1)

  let union (s1 : 'a t) (s2 : 'a t) : 'a t =
    let res = copy s1 in
    union_inplace res s2;
    res

  let all_pairs (s : 'a t) : ('a * 'a) list =
    let l = to_list s in
    List.(
      cartesian_product l l
      |> filter ~f:(fun (a, b) -> a <> b)
    )
end

(* Thunk to create an empty Poly_set *)
let empty () = Poly_set.create (module String)

(* Special top function to avoid unnecessary computation over all equality pairs *)
let s_top_func () =
  let set = Poly_set.create (module String) in
  Poly_set.add set "TOP";
  set

(* Special meet function to handle Poly_sets that have "TOP" element *)
let s_meet s1 s2 =
  let set =
    if Poly_set.exists s1 ~f:(fun elt -> elt = "TOP") then s2
    else if Poly_set.exists s2 ~f:(fun elt -> elt = "TOP") then s1
    else Poly_set.inter s1 s2
  in
  Poly_set.remove set "TOP";
  set

let surprised_pikachu =
  "??????~~~~~~~~~::::::~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~::~~==+++++++++===========
  ????:~~~~~~~~~::::::::~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~==++++++++=======~~~~~
  :~~~~~~~~~~~~~:::::::::~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~==+++++++====~~~~~~~::
  ~~~~~~~~~~~~~::::::::::~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~:~~==+++++++===~~~~~:::::
  ~~~~~~~~~~~~~::::::::::~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~==+++++++===~~~~~:::::
  ~~~~~~~~~~~~::::::::::::~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~==++++++===~~~~~::::::
  ~~~~~~~~~~~:::::::::::::~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~:~==++++++===~~~~~~:::::
  ~~~~~~~~~~~::::::::::::::~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~=========~~~~~~~:::::
  ~~~~~~~~~~:::,::~~~:~::::~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~=======~~~~~~~~~::::
  ~~~~~~~~~~,:::=+=:~~~~::::~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~:~~~~~~~~~~~~~~~~~~::::
  ~~~~~~~~~:,,::+++++~~~::::~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~::~~~~~~~~~~~~~~~~::::
  ~~~~~~~~~:::,:++++++++,~::~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~:::::::::::::::::,+,::
  ~~~~~~~~~~~~~,,++++++++++,:~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~::::::::::::,=+++++,::
  ~~~~~~~:~~~~~~:++++++++++++=~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~::::::::,~+++++++++::,
  ~~~~~~~~~~~~~~~,+++++++++++++?~~~~~~~~~~~~~~~~~~~~~~~~~~~~~::::,+++++++++++++:,:
  ~~~~~~:~~~~~~~::?++++++++++++++?~~~~~~~~~~~~~~~~~~~~~~~~~~~,++++++++++++++++,,::
  ~~~~~~~~~~~~~::::++++++++++++++++.~~~~~~~~~~~~~~~~~~~~~~,+++++++++++++++++++,:::
  ~~~~~:~~~~~~~:::::?+++++++++++++++=,,++++++++++++++++++++++++++++++++++++++,::::
  ~~~~~~~~~~~~:::::::?++++++++++++++++++++++++++++++++++++++++++++++++++++++,:::::
  ~~~~::::::::::::::::.++++++++++++++++++++++++++++++++++++++++++++++++++++:::::::
  ~~~~:::::::::::::::::,+++++++++++++++++++++++++++++++++++++++++++++++++=~~~~::::
  ~~~::::::::::::::::::,,,++=+++++++++++++++++++++++++++++++++++++++++++:~~~~~::::
  ~~~::::,,,,,,::::::::,,,:+++++++++++++++++++++++++++++++++++++++,++?:~~~~~~~::::
  ~~:,,,,,,,,,,,:::::::::,++++++++++++++++++++++++++++++++++++++++++:::~~~~~~:::::
  ~~,,,,,,,,,,,,,::::::::,++++++++++++++++++++++++++++++++++++++++++?:::::::::::::
  ~:,,,,,,,,,,,,,::::::::+++++++:II:,+++++++++++++++++??7,:++++++++++,::::::::::::
  ~,,,,,,,,,,,,,,,::::::++++++++:?I::+++++++++++++++++,+?,:,++++++++++::::::::::::
  :,,,,,,,,,,,,,,,:::::,++++++++,::::+++++++++++++++++?,::,?++++++++++,:::::::::::
  :,,,,,,,,,,,,,,,:::::?++++++++++,+++++++++++++++++++++,:+++++++++++++:::::::::::
  ,,,,,,,,,,,,,,,,:::::+++++++++++++++++++,,,?+++++++++++++++++++++++++,::::::::::
  ,,,,,,,,,,,,,,,,::::,++?~~++++++++++++++++++++++++++++++++++~~~+++++++::::::::::
  :,,,,,,,,,,,,,::::::++~~~~~~=+++++++++++++++++++++++++++++~~~~~~~+++++,:::::::::
  ::::,,,,:,,:::::::::+~~~~~~~~++++++++++++++++++++++++++++~~~~~~~~~+++++:::::::::
  :::::::::::::::::::~++~~~~~~=+++++++++++:+++++=?+++++++++~~~~~~~~~+++++:::::::::
  ::::::::::::::::~~~~++++~~+++++++++++++:+++++++:+++++++++++~~~~~+++++++,~:::::::
  ::::::~~~~~~~~~~~~~~~++++++++++++++++++:++++++++++++++++++++++++++++++++~~~:::::
  ::::~~~~~~~~~~~~~~~~~+++++++++++++++++++:++++++,++++++++++++++++++++++++~~~~::::
  ::::~~~~~~~~~~~~~~~~~,+++++++++++++++++++++,,~++++++++++++++++++++++++++,~~~::::
  ::::~~~~~~~~~~~~~~~~~~,++++++++++++++++++++++++++++++++++++++++++++++++++~~~::::
  :::::~~~~~~~~~~~~~~~~::++++++++++++++++++++++++++++++++++++++++++++++++++~~:::::
  ::::::::~~~~~~~~~~~~::::+++++++++++++++++++++++++++++++++++++++++++++++++,::::::
  ::::::::::::::::::::::::+++++++++++++++++++++++++++++++++++++++++++++++++,::::::
  :::::::::::::::::::::::,++++++++++++++++++++++++++++++++++++++++++++++++++::::::
  ~::::::::::::::::::::::+++++++++++++++++++++++++++++++++++++++++++++++++++::::::"

