

structure ExtractCombine :> EXTRACT_COMBINE =
struct
    fun extractcombine(cmp: 'k * 'k -> order, extractor: 'a -> ('k * 'v) Seq.seq, combiner: 'v * 'v -> 'v, docs: 'a MR.mapreducable): ('k, 'v) Dict.dict = 
      let 
      fun merge_dict(x,y) = Dict.merge(cmp, combiner, x, y)
      fun seq_reduction(x) = 
          Seq.mapreduce(fn(x) => Dict.insert(cmp, Dict.empty, x), Dict.empty, merge_dict, extractor(x))
      val a = MR.mapreduce(fn(x) => seq_reduction(x), Dict.empty, merge_dict, docs)
      in
        a
      end
    
end