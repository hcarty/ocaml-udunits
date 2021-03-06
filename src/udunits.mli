type 'system t
(** A type defining a unit *)

type 'a system_t
(** A unit system *)

type 'system converter_t
(** A converter from one unit to another *)

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
  (** Possible UDUNITS result status codes *)

type encoding_t =
  | ASCII
  | ISO_8859_1
  | UTF8
  (** Possible text encodings accepted by and returned by UDUNITS *)

exception Error of status_t
(** Exception raised if a function call fails *)

val get_status : unit -> status_t
(** [get_status ()] will return the status code from the last UDUNITS call.  One
    example of where this can be useful is determining why a call to
    {!are_convertible} returned [false]. *)

val read_xml : string -> 'a system_t
(** [read_xml filename] reads the unit system from [filename].

    @raise Error if [filename] does not exist or is otherwise invalid *)

val read_xml_default : unit -> [`default] system_t
(** [read_xml_default ()] is like {!read_xml} except that it reads the default
    unit system defintion that comes with UDUNITS. *)

val are_convertible : 'system t -> 'system t -> bool
(** [are_convertible a b] will return [true] if a conversion is possible from
    [a] to [b], [false] otherwise.  If [are_convertible] returns false then
    {!get_status} can tell why a conversion is not allowed. *)

val get_converter : src:'system t -> dest:'system t -> 'system converter_t
(** [get_converter ~src ~dest] returns a converter from the unit defined in
    [src] to the unit defined in [dest]. *)

val convert : _ converter_t -> float -> float
(** [convert converter x] converts [x] according to [converter]. *)

val convert_array1_to :
  _ converter_t ->
  src:(float, 'a, 'b) Bigarray.Array1.t -> 
  dest:(float, 'a, 'b) Bigarray.Array1.t ->
  unit
(** [convert_array1_to converter ~src ~dest] converts the values in [src]
    according to [converter] placing the results in [dest].  Conversion
    starts at the first value of each array and proceeds until [dest] has
    been filled.  [src] and [dest] can be the same value if you want to
    modify [src] directly.

    @raise Error if [dest] has more elements than [src] *)

val convert_array_to :
  _ converter_t ->
  src:float array ->
  dest:float array ->
  unit
(** [convert_array_to converter ~src ~dest] converts the values in [src]
    according to [converter] placing the results in [dest].  Conversion
    starts at the first value of each array and proceeds until [dest] has
    been filled.  [src] and [dest] can be the same value if you want to
    modify [src] directly.

    @raise Error if [dest] has more elements than [src] *)

val convert_array1 :
  _ converter_t ->
  (float, 'a, 'b) Bigarray.Array1.t ->
  (float, 'a, 'b) Bigarray.Array1.t
(** [convert_array1 converter src] converts the values in [src]
    according to [converter], returning the results as a newly allocated
    array. *)

val convert_array : _ converter_t -> float array -> float array
(** [convert_array1 converter src] converts the values in [src]
    according to [converter], returning the results as a newly allocated
    array. *)

val parse :
  ?encoding:encoding_t -> 'system system_t -> string -> 'system t
(** [parse ?encoding system s] will return a unit defined by [s] under
    [system].  [encoding] refers to the encoding of [s] and defaults to
    [ASCII].

    @raise Error if there is an error parsing [s] *)

val get_name : ?encoding:encoding_t -> _ t -> string
val get_symbol : ?encoding:encoding_t -> _ t -> string
(** [get_name] and [get_symbol] return string representations of a named
    unit. *)

val format :
  ?encoding:encoding_t ->
  ?names:bool ->
  ?basic:bool ->
  ?max_length:int ->
  _ t ->
  string
(** [format u] returns a string representation of [u].

    @param names indicates if the string should use unit names ([true]) or
    symbols ([false]).  Defaults to [true].
    @param basic indicates if the unit should be broken down into base units
    ([true]) or left in composite form ([false]).  Defaults to [false].
    @param max_length sets a maximum length for the returned string.
    Defaults to [1024]. *)

val is_dimensionless : _ t -> bool
(** [is_dimensionless u] returns [true] if [u] is dimensionless (ex.
    radians). *)

val scale_by : 'system t -> float -> 'system t
(** [scale_by u x] returns a new unit which is [u] scaled by [x]. *)

val offset_by : 'system t -> float -> 'system t
(** [offset_by u x] returns a new unit which is [u] offset by [x]. *)

val raise_to : 'system t -> float -> 'system t
(** [raise_to u x] returns a new unit which is [u] raised to the [x] power. *)

val root_by : 'system t -> float -> 'system t
(** [root_by u x] returns a new unit which is the [x]'th root of u. *)

val log_by : 'system t -> float -> 'system t
(** [log_by u x] returns the logarithmic unit corresponding to the base [x] and
    reference level [u]. *)

val invert : 'system t -> 'system t
(** [invert u] returns the reciprocal of [u]. *)

val multiply : 'system t -> 'system t -> 'system t
(** [multiply a b] returns the result of multiplying [a] by [b]. *)

val divide : 'system t -> 'system t -> 'system t
(** [divide a b] returns the result of dividing [a] by [b]. *)

