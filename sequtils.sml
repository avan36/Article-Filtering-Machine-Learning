
structure SeqUtils : SEQUTILS =
struct

    fun s2l s = Seq.mapreduce (fn x => [x], [], op@, s)
    fun seq l = List.foldr (Seq.cons) (Seq.empty()) l

    fun words (s : string) : string Seq.seq = 
        (* String.tokens shoudl return a sequence *)
        seq (String.tokens (fn s => s = #" ") s)

    fun words_punc (s : string) : string Seq.seq = 
        (* String.tokens shoudl return a sequence *)
        seq (String.tokens (fn s => Char.isSpace s orelse (Char.isPunct s andalso not (s = #"'"))) s)

    val explode = seq o String.explode 
    val implode = String.implode o s2l

    fun contains (p,s) = Seq.mapreduce (p, false, (fn (x,y) => x orelse y), s) 

    fun strictSuffixes s = Seq.tabulate (fn i => Seq.drop (i + 1, s), (Seq.length s) - 1)

end

