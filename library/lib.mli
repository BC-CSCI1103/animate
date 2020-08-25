(* file: lib.mli
   author: Bob Muller

   This is the interface file for the Lib module used in CSCI 1103.
*)
val closeEnough : ?error:float -> float -> float -> bool

val range : int -> int list

val pi : float

val i2S : int -> string
val i2F : int -> float
val f2S : float -> string
val f2I : float -> int

val explode : string -> char list
val implode : char list -> string

val (%) : int -> int -> int

val fmt  : ('a, unit, string) format -> 'a
val pfmt : ('a, out_channel, unit) format -> 'a

val run_test : string -> (unit -> bool) -> unit
val run_failing_test : string -> (unit -> bool) -> unit


(** wav.mli, wav.ml --- A lightweight WAV reader/writer

    [MIT License] Copyright (C) 2015 Akinori ABE *)

type wav_data =
  | MONORAL of float array
  | STEREO of (float * float) array

(** [load filename] loads a wav file of path [filename].
    @return [(fs, x)] where [fs] is the sampling rate and [x] is wav data. *)
val load : string -> int * wav_data

(** [save ?sampling_bits ~sampling_rate filename x] saves data [x] into a wav
    file of path [filename].
    @param sampling_bits quantization bits ([8] or [16]).
    @param sampling_rate the sampling rate of [data] (Hz). *)
val save : ?sampling_bits:int -> sampling_rate:int -> string -> wav_data -> unit
