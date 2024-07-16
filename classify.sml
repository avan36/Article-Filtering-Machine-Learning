
structure NaiveBayes :> NAIVE_BAYES_CLASSIFIER =
struct

    type category = string

    type labeled_document = category * string Seq.seq
    type document = string Seq.seq

    type statistics = 
          (category,int) Dict.dict           (* maps each category to number of documents with that category *)
        * (category,int) Dict.dict           (* maps each category to number of words in documents with that category *)
        * (category * string, int) Dict.dict (* maps each (cat,word) to frequency *)
        * category Seq.seq                   (* list of categories (no duplicates) *)
        * int                                (* total number of documents *)
        * int                                (* total number of different words *)


    fun gather (train : labeled_document MR.mapreducable) : statistics =
      let
        fun simple_add(x,y) = x + y

        val a = ExtractCombine.extractcombine(String.compare, fn(category, doc) => Seq.singleton(category,1), simple_add, train)
        val b = ExtractCombine.extractcombine (String.compare, fn(category, doc) => Seq.singleton(category, Seq.length(doc)), simple_add, train)
        val c = ExtractCombine.extractcombine(fn((x1,y1), (x2,y2)) => String.compare(x1 ^ y1, x2 ^ y2), fn(category, doc) => Seq.map(fn word => ((category, word), 1), doc), simple_add, train)
        val d = Seq.map(fn(x,y) => x, Dict.toSeq(ExtractCombine.extractcombine(String.compare, fn(category, doc) => Seq.singleton(category,0), simple_add, train)))
        val e = Seq.reduce(simple_add, 0, Seq.map(fn(x,y) => y, Dict.toSeq(a))) 
        val f = Dict.size(ExtractCombine.extractcombine(String.compare, fn(category, doc) => Seq.map(fn word => (word, 1), doc), simple_add, train))
      in
        (a, b, c, d, e, f)
      end   


    (* TASK *)
    fun possible_classifications 
        ((num_docs_by_cat,
          num_words_by_cat,
          freqs,
          all_categories, 
          total_num_docs,
          total_num_words) : statistics,
         test_doc : document) : (category * real) Seq.seq =
        
        let
        fun total_number_words_in_category(cat) = Dict.lookup'(String.compare,num_words_by_cat, cat)
        fun probability_words_in_category(cat) = Seq.mapreduce(fn w => case (Dict.lookup(fn((c,w),(c',w')) => String.compare(c ^ w, c' ^ w'), freqs, (cat, w))) of
                                                        SOME x' => Math.ln(Real.fromInt(x') / Real.fromInt(total_number_words_in_category(cat)))
                                                        | NONE => Math.ln(1.0 / Real.fromInt(total_num_words)), 0.0, fn(x,y) => (x + y), test_doc)
        fun number_docs(cat) = Dict.lookup'(String.compare, num_docs_by_cat, cat)
        in
        Seq.map(fn(cat, num_docs) => (cat, Math.ln(Real.fromInt(number_docs(cat)) / Real.fromInt(total_num_docs)) + probability_words_in_category(cat)),  Dict.toSeq(num_docs_by_cat))
        end

    (* TASK *)
    fun classify (stats : statistics, test_doc : document) : (category * real) = 
    let fun cmp((cat1, prob1), (cat2, prob2)) = case prob1 <= prob2 of true => (cat2, prob2)
                                                                      | false => (cat1, prob1)
      in
      Seq.reduce(cmp, ("", Real.negInf), possible_classifications(stats, test_doc))
      end

    (* TASK *)
    fun train_classifier (train : labeled_document MR.mapreducable) : document -> (category * real) =
        let val (a,b,c,d,e,f) = gather(train)
        fun doc(d) = classify((a,b,c,d,e,f),d)
        in 
          doc
        end

    (*
    Small train w/ small test gets 3/8 = 37.5% correct.
    Medium train w/ medium test gets 680/808 = 84.15% correct.
    Big train w/big test gets about the same, 70122/78899 = 88.87% correct

    As shown, more data made the model more accurate.  And yes, more training data seems to have made the medium tests more accurate.
    Testing the large training set on the medium test, I got a 87.12%.
    *)
        
end
