
structure TestClassify
  :> 
  sig
      type labeled_document = NaiveBayes.labeled_document 
      type document = NaiveBayes.document 
      val print_stats          : labeled_document MR.mapreducable -> unit
      val print_stats_nofreqs  : labeled_document MR.mapreducable -> unit
      val print_possibles      : labeled_document MR.mapreducable * labeled_document -> unit
      val number_correct       : labeled_document MR.mapreducable * labeled_document MR.mapreducable -> int * int
      val print_predictions    : labeled_document MR.mapreducable * labeled_document MR.mapreducable -> unit
  end
  =
struct
    type labeled_document = NaiveBayes.labeled_document
    type document = NaiveBayes.document

    fun print_stats' (printfreqs : bool, train : NaiveBayes.labeled_document MR.mapreducable) : unit =
        let 
            val (num_docs_by_cat,
                 num_words_by_cat,
                 freqs,
                 all_categories, 
                 total_num_docs,
                 total_num_words) = NaiveBayes.gather train 
                
            val () = print ("Number of documents by category:\n" ^ 
                            (Seq.mapreduce (fn (cat,count) => "  " ^ cat ^ " " ^ Int.toString count ^ "\n",
                                            "",
                                            (op^),
                                            Dict.toSeq num_docs_by_cat))
                            )

            val () = print ("Number of words by category:\n" ^ 
                            (Seq.mapreduce (fn (cat,count) => "  " ^ cat ^ " " ^ Int.toString count ^ "\n",
                                            "",
                                            (op^),
                                            (Dict.toSeq num_words_by_cat))
                            ))

            val _ = case printfreqs of 
                true =>
                    print ("Frequencies:\n" ^ 
                           Seq.mapreduce (fn ((cat, word),count) => ("  " ^ cat ^ ": " ^  word ^ " occured " ^ Int.toString count ^ " times \n"),
                                          "",
                                          (op^), 
                                          (Dict.toSeq freqs)))
              | false => ()

            val () = print ("All categories: " ^ Seq.reduce (fn (c1,c2) => c1 ^ " " ^ c2, "", all_categories) ^ "\n")
            val () = print ("Total number of documents:" ^ Int.toString total_num_docs ^ "\n")
            val () = print ("Total number of distinct words:" ^ Int.toString total_num_words ^ "\n")
        in 
            ()
        end

    fun print_stats(train) = print_stats' (true, train)
    fun print_stats_nofreqs(train) = print_stats' (false,train)

    fun print_possibles (train : labeled_document MR.mapreducable, (cat, words) : NaiveBayes.labeled_document) : unit = 
        let val probs = NaiveBayes.possible_classifications (NaiveBayes.gather train, words)
        in 
            print ("Correct Category: " ^ cat ^ "\n" ^
                    "Scores:\n" ^ Seq.mapreduce (fn (c,r) => "  " ^ c ^ " " ^ Real.toString r, "", (fn (s1,s2) => s1 ^ "\n" ^ s2), probs))
        end

    fun number_correct (train : labeled_document MR.mapreducable, test : labeled_document MR.mapreducable) : int * int = 
        let 
            val cl = NaiveBayes.train_classifier train
        in 
            MR.mapreduce (fn (correct_answer, words) =>
                          let val (predicted, _) = cl words
                          in 
                              case predicted = correct_answer of
                                  true => (1,1)
                                | false => (0,1)
                          end,
                          (0,0),
                          fn ((cor1,tot1),(cor2,tot2)) => (cor1 + cor2 , tot1 + tot2),
                          test)
        end

    fun print_predictions (train : labeled_document MR.mapreducable, test : labeled_document MR.mapreducable) : unit =
        let 
            val cl = NaiveBayes.train_classifier train
        in 
            print (MR.mapreduce (fn (correct_answer, words) =>
                                 let val (predicted, _) = cl words
                                     val correctstring = "Given Categories: " ^ correct_answer ^ "\n"
                                     val predstring    = "Predicted: " ^ predicted ^ "\n"
                                     val doc           = (Seq.reduce (fn (s1,s2) => s1 ^ " " ^ s2, "", words)) ^ "\n"
                                     val report        = correctstring ^ predstring ^ doc ^ "\n"
                                 in 
                                     (case correct_answer = predicted of
                                          true => "CORRECT\n" ^ report
                                        | false => "INCORRECT\n" ^ report)
                                 end,
                                "",
                                op^,
                                test))
        end
end

