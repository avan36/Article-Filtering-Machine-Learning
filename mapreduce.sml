
signature MAP_REDUCE_SEQ = MAP_REDUCE where type 'a mapreducable = 'a Seq.seq

structure SeqMR :> MAP_REDUCE_SEQ  =
struct
    type 'a mapreducable = 'a Seq.seq
    val mapreduce = Seq.mapreduce
end

signature MAP_REDUCE_FILE = MAP_REDUCE where type 'a mapreducable = (unit -> TextIO.instream) * (string -> 'a option)

structure FileMR :> MAP_REDUCE_FILE =
struct

    type 'a mapreducable = (unit -> TextIO.instream)
                           * (string -> 'a option) (* parse a line as an 'a *)
        
    (* note: combines in reverse order, which is OK since n is commutative *)
    fun mapreduce (l : 'a -> 'b, e : 'b, n : 'b * 'b -> 'b, (stream,parse)) = 
        let val c = ref 0

            val s = stream ()

            fun loop (cur : 'b) : 'b =
                case TextIO.inputLine s of
                    NONE => cur
                  | SOME line => (case parse line of
                                      NONE => loop cur
                                    | SOME newval => 
                                          (c := !c + 1; 
                                           (case !c mod 1000 = 0 of 
                                                true => print ("Progress: reading document " ^ Int.toString (!c) ^ "\n")
                                              | false => ());
                                           loop (n (l newval, cur))))
        in
            loop e
        end

end
