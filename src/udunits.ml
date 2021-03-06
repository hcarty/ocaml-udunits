type 'system t

type 'a system_t

type 'system converter_t

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

external are_convertible :
  'system t -> 'system t -> bool = "ml_ut_are_convertible"
external get_converter :
  src:'system t ->
  dest:'system t ->
  'system converter_t =
  "ml_ut_get_converter"

external convert : _ converter_t -> float -> float =
  "ml_cv_convert_double"

external convert_array1_to :
  _ converter_t ->
  src:(float, 'a, 'b) Bigarray.Array1.t -> 
  dest:(float, 'a, 'b) Bigarray.Array1.t ->
  unit = "ml_cv_convert_bigarray"

external convert_array_to :
  _ converter_t ->
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
  'system system_t -> string -> encoding_t -> 'system t =
  "ml_ut_parse"

let parse ?(encoding = ASCII) system u = parse system u encoding

external get_name : _ t -> encoding_t -> string = "ml_ut_get_name"
external get_symbol : _ t -> encoding_t -> string = "ml_ut_get_symbol"
external format : _ t -> encoding_t -> bool -> bool -> int -> string =
  "ml_ut_format"

let get_name ?(encoding = ASCII) u = get_name u encoding
let get_symbol ?(encoding = ASCII) u = get_symbol u encoding
let format
    ?(encoding = ASCII) ?(names = true) ?(basic = false) ?(max_length = 1024) u
  =
  format u encoding names basic max_length

external is_dimensionless : _ t -> bool = "ml_ut_is_dimensionless"
external scale_by : 'system t -> float -> 'system t = "ml_ut_scale"
external offset_by : 'system t -> float -> 'system t =
  "ml_ut_offset"
external raise_to : 'system t -> float -> 'system t = "ml_ut_raise"
external root_by : 'system t -> float -> 'system t = "ml_ut_root"
external log_by : 'system t -> float -> 'system t = "ml_ut_log"
external invert : 'system t -> 'system t = "ml_ut_invert"
external multiply : 'system t -> 'system t -> 'system t =
  "ml_ut_multiply"
external divide : 'system t -> 'system t -> 'system t =
  "ml_ut_divide"

let () =
  (* Register custom exception *)
  Callback.register_exception "ut status exception" (Error Success);
  (* Set the default message handler to ignore messages rather than writing
     them to stderr. *)
  init ()

