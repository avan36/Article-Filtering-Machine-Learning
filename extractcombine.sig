
signature EXTRACT_COMBINE =
sig

    val extractcombine :   ('k * 'k -> order)        (* comparison function for keys *)
                         * ('a -> ('k * 'v) Seq.seq) (* extractor: keys are not nec unique *)
                         * ('v * 'v -> 'v)           (* combiner *)
                         * 'a MR.mapreducable 
                         -> ('k,'v) Dict.dict

end
