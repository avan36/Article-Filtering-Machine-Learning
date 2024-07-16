
(* assumes MR is FileMR *)

structure TestFile =
struct

    open TestClassify

    fun parse_line (s : string) : labeled_document option = 
        let val [cats , noncats] = String.tokens (fn #"\t" => true | _ => false) s
            val toplevel = (Seq.filter (fn s => String.isSuffix "CAT" s, Seq.fromlist (String.tokens (fn #"," => true | _ => false) cats)))
        in 
            case Seq.length toplevel of
                0 => NONE
              | _ => SOME (Seq.nth (0, toplevel),
                           SeqUtils.words noncats)
        end

    fun open_file (s : string) : labeled_document FileMR.mapreducable  =
        (fn () => TextIO.openIn s, parse_line)

    fun time (f : unit -> 'a) : {gcsys : IntInf.int, gcusr : IntInf.int, ngcsys : IntInf.int, ngcusr : IntInf.int} * 'a = 
        let val t = Timer.startCPUTimer ()
            val a = f ()
            val {gc={sys=gcsys,usr=gcusr},
                 nongc={sys=ngcsys,usr=ngcusr}} = Timer.checkCPUTimes t
        in
            ({gcsys = Time.toSeconds gcsys, 
              gcusr = Time.toSeconds gcusr,
              ngcsys = Time.toSeconds ngcsys,
              ngcusr = Time.toSeconds ngcusr}, a)
        end

end
