type ('a, 'system) t

type 'a system_t

type ('src, 'dest, 'system) converter_t

type status_t =
  | Success
  | Bad_arg
  | Exists
  | No_unit
  | Os
  | Not_same_system
  | Meaningless
  | No_second
  | Visit_error
  | Cant_format
  | Syntax
  | Unknown
  | Open_arg
  | Open_env
  | Open_default
  | Parse

type encoding_t =
  | ASCII
  | ISO_8859_1
  | UTF8

exception Error of status_t

external init : unit -> unit = "ml_init"

external get_status : unit -> status_t = "ml_ut_get_status"

external read_xml : string -> 'a system_t = "ml_ut_read_xml"
external read_xml_default : unit -> [`default] system_t = "ml_ut_read_xml_default"

let read_xml _k filename = read_xml filename

external are_convertible :
  (_, 'system) t -> (_, 'system) t -> bool = "ml_ut_are_convertible"
external get_converter :
  src:('s, 'system) t ->
  dest:('d, 'system) t ->
  ('s, 'd, 'system) converter_t =
  "ml_ut_get_converter"

external convert : (_, _, _) converter_t -> float -> float =
  "ml_cv_convert_double"

external convert_array1_to :
  (_, _, _) converter_t ->
  src:(float, 'a, 'b) Bigarray.Array1.t -> 
  dest:(float, 'a, 'b) Bigarray.Array1.t ->
  unit = "ml_cv_convert_bigarray"

external convert_array_to :
  (_, _, _) converter_t ->
  src:float array ->
  dest:float array ->
  unit = "ml_cv_convert_array"

let convert_array1 conv a =
  let a' =
    Bigarray.Array1.create
      (Bigarray.Array1.kind a)
      (Bigarray.Array1.layout a)
      (Bigarray.Array1.dim a)
  in
  convert_array1_to conv ~src:a ~dest:a';
  a'

let convert_array conv a =
  let a' = Array.make (Array.length a) 0.0 in
  convert_array_to conv ~src:a ~dest:a';
  a'

external parse :
  'system system_t -> string -> encoding_t -> ('a, 'system) t =
  "ml_ut_parse"

let parse system _k s encoding = parse system s encoding

external get_name : (_, _) t -> encoding_t -> string = "ml_ut_get_name"
external get_symbol : (_, _) t -> encoding_t -> string = "ml_ut_get_symbol"

(* Set the default message handler to ignore messages rather than writing them
   to stderr.  Register our custom exception. *)
let () =
  Callback.register_exception "ut status exception" (Error Success);
  init ()

