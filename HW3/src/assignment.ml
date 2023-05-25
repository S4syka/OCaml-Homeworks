let add a b = failwith "TODO add"

let filter p xs = failwith "TODO filter"

let starts_with haystack hay = failwith "TODO starts_with"

module type Readable = sig
  type t
  type arg
  val begin_read : arg -> t
  val end_read : t -> unit
  val at_end : t -> bool
  val read_line : t -> (t * string)
end

module ReadableString : Readable with type t = string list and type arg = string  = struct
  type t = string list

  type arg = string
 
  let begin_read arg = 
    String.split_on_char '\n' arg;;
    
  let at_end = function 
    |x :: xs -> false
    |[] -> true 

  let read_line = function
    |x :: xs -> (xs, x)
    |[] -> ([], "")

  let end_read list : unit = ();;
end;;

module ReadableFile : Readable with type t = in_channel * (string option) and type arg = string = struct
  type t = in_channel * (string option)
  type arg = string

  let begin_read arg = 
    let ic = open_in arg in
    try (ic, Some (input_line ic))
  with End_of_file -> (ic, None)

  let at_end = function
    |(_, None) -> true
    |(_, Some _) -> false
  
  let read_line = function
    |(ic, Some x) -> let next_line = try Some (input_line ic) with End_of_file -> None in
    ((ic, next_line), x)
    |(ic, None) -> ((ic, None), "")

  let end_read (ic, _) : unit =
    close_in ic      
end

module Reader (Rd: Readable) : sig
  val read_all : Rd.t -> (Rd.t * string) include Readable with type t = Rd.t and type arg = Rd.arg
end = struct
  type t = Rd.t
  type arg = Rd.arg
  let begin_read = Rd.begin_read
  let end_read = Rd.end_read
  let at_end = Rd.at_end
  let read_line = Rd.read_line
  let rec read_all inf = match Rd.at_end inf with
  |true -> (inf, "")
  |false  ->   let (inf1, v1) = Rd.read_line inf in  
             let (_, v2) = read_all inf1 in
             if (at_end inf1 = false) then 
            (inf1, v1 ^ "\n" ^ v2)
            else (inf1, v1)
end

let rs = ReadableFile.begin_read "/home/eke_bichi/TextFilesForOcaml/test.txt";;
let e = ReadableFile.at_end rs
let rs, l1 = ReadableFile.read_line rs
let rs, l2 = ReadableFile.read_line rs
let e = ReadableFile.at_end rs
let _ = ReadableFile.end_read rs;;

module R = Reader(ReadableFile)

let r = R.begin_read "/home/eke_bichi/TextFilesForOcaml/test.txt"
let r,t = R.read_all r
let _ = R.end_read r