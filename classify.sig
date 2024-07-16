signature CLASSIFIER = 
sig

    type category = string

    type document = string Seq.seq
    type labeled_document = category * document
        
    val train_classifier : labeled_document MR.mapreducable 
                         -> (document -> (category * real))
end

signature NAIVE_BAYES_CLASSIFIER = 
sig

    type category = string

    type labeled_document = category * string Seq.seq
    type document = string Seq.seq
        
    val train_classifier : labeled_document MR.mapreducable -> (document -> (category * real))


    (* ---------------------------------------------------------------------- *)
    (* internal components that are exported only for testing *)

    type statistics = 
          (category,int) Dict.dict           (* maps each category to number of documents with that category *)
        * (category,int) Dict.dict           (* maps each category to number of words in documents with that category *)
        * (category * string, int) Dict.dict (* maps each (cat,word) to frequency *)
        * category Seq.seq                   (* list of categories (no duplicates) *)
        * int                                (* total number of documents *)
        * int                                (* total number of different words *)

    val gather : labeled_document MR.mapreducable -> statistics 

    val possible_classifications : statistics * document -> (category * real) Seq.seq
    val classify : statistics * document -> category * real

end 

