
(* there is a 

   structure Dict : DICT 

*)

signature DICT =
sig
  type ('k,'v) dict 

  val empty   : ('k,'v) dict 
  val insert  : ('k * 'k -> order) * ('k,'v) dict  * ('k * 'v) -> ('k,'v) dict 
  val lookup  : ('k * 'k -> order) * ('k,'v) dict  * 'k -> 'v option

  (* assumes key is in the dictionary *)
  val lookup' : ('k * 'k -> order) * ('k,'v) dict * 'k -> 'v 

  (* number of (key,value) pairs in the dictionary *)
  val size    : ('k,'v) dict  -> int

  (* computes the sequence of all (key,value) pairs in the dictionary,
     ordered from smallest key to largest key
     *)
  val toSeq : ('k,'v) dict  -> ('k * 'v) Seq.seq  

  (* merge (cmp, combine, d1,d2) == d where
     - k in d if and only if k is in d1 or k is in d2
     - If k~v in d1 and k is not in d2, then k ~ v in d
     - If k~v in d2 and k is not in d1, then k ~ v in d
     - If k~v1 in d1 and k~v2 in d2, then k ~ combine (v1, v2) in d
     *)
  val merge  : ('k * 'k -> order) * ('v * 'v -> 'v) * ('k,'v) dict  * ('k,'v) dict  -> ('k,'v) dict 


  val map : ('a -> 'b) * ('k,'a) dict -> ('k,'b) dict

end

