
structure TestSeq =
struct

    open TestClassify

    val seq = Seq.fromlist

    val simplest_train = seq [ ("ECAT", seq ["stock"]), ("GCAT", seq ["congress"]) ]
    val simple_train   = seq [ ("ECAT", seq ["stock","price"]), ("GCAT", seq ["congress","court"]) ]
    val cross_train    = seq [ ("ECAT", seq ["stock","price","fell"]), ("GCAT", seq ["congress","court","fell"]) ]
    val dups_train     = seq [ ("ECAT", seq ["stock","price","stock","price"]), ("GCAT", seq ["congress","court","court","congress"]) ]

    val doc1 = ("ECAT", seq ["stock"])
    val doc2 = ("GCAT", seq ["congress"])
    val doc3 = ("GCAT", seq ["court","fell"])
    val doc4 = ("ECAT", seq ["stock","ticker"])

    val docs14 = seq[doc1,doc2,doc3,doc4]

end
