
structure TreeDict : DICT =
struct

  fun log2 (n : int) : int = 
      case n of 
          0 => 0 (* hack *)
        | 1 => 1
        | _ => 1 + log2 (n div 2)

  datatype ('k, 'v) tree =
      Empty
    | Node of ('k, 'v) tree * ('k * 'v) * ('k, 'v) tree

  type ('k,'v) dict = ('k, 'v) tree 

  val empty = Empty

  fun size t =
        case t of
            Empty => 0
          | Node(l,_,r) => 1 + size l + size r
      
  fun insert (cmp, d, (k, v)) =
    case d of
      Empty => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
        EQUAL => Node (L, (k, v), R)
      | LESS => Node (insert (cmp, L, (k, v)), (k', v'), R)
      | GREATER => Node (L, (k', v'), insert (cmp, R, (k, v)))

  fun lookup (cmp, d, k) =
    case d of
      Empty => NONE
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
        EQUAL => SOME v'
      | LESS => lookup (cmp, L, k)
      | GREATER => lookup (cmp, R, k)

  fun toString (kvts, d) =
      case d of
          Empty => ""
        | Node(l,kv,r) => toString (kvts, l) ^ " " ^ kvts kv ^ " " ^ toString (kvts, r)

  fun lookup' (cmp : 'k * 'k -> order, d, k) = case (lookup (cmp, d, k)) of NONE => raise Fail "key not found in dictionary" | SOME v => v
      

  (* TASK: copy your split and merge here and rename merge to merge' *)

    fun splitAt(x: ('k,'v) dict, kCheck: 'k, cmp: ('k * 'k -> order)): ('k, 'v) tree * ('k, 'v) tree * 'v option  = (*Returns  tree less than k, tree greater than k, value stored with k if k was a tree, using an option *)
     case x of 
        Empty => (Empty, Empty, NONE)
        | Node(left,(k,v),right) => (case cmp(k, kCheck) of
          EQUAL => (left, right, SOME(v))
          (*We now look to the right with splitAt *)
          | LESS => let val (right_left, right_right, right_value) = splitAt(right, kCheck, cmp)
            in (Node(left, (k,v), right_left), right_right, right_value) end
          (*We now look to the left with splitAt *)
          | GREATER => let val (left_left, left_right, left_value) = splitAt(left, kCheck, cmp) 
          in (left_left, Node(left_right,(k,v),right), left_value) end 
    )
    
    fun merge' (cmp: ('k * 'k -> order), combine: ('v * 'v -> 'v), d1: ('k, 'v) dict, d2: ('k, 'v) dict): ('k, 'v) dict =
    case d1 of 
      Empty => d2
    | Node(d1_l, (d1_k, d1_v), d1_r) => let
        val (less_in_d2, greater_in_d2, value_in_d2) = splitAt(d2, d1_k, cmp)
      in
      case value_in_d2 of 
        SOME(x) => 
          let val part1 = merge'(cmp, combine, d1_l,less_in_d2)
              val part2 = (d1_k, combine(d1_v,x))
              val part3 = merge'(cmp, combine, d1_r, greater_in_d2)
          in Node(part1,part2,part3) end
        | NONE => let
                  val part1 = merge'(cmp, combine, d1_l, less_in_d2)
                  val part2 = (d1_k, d1_v)
                  val part3 = merge'(cmp, combine, d1_r, greater_in_d2)
                  in Node(part1,part2,part3) end end

  (* optimize inserts: if merging with a 1-element dictionary, insert instead, because it only needs to walk down one path of the tree *)

  fun insertWith (cmp : 'k * 'k -> order, c : 'v * 'v -> 'v, d : ('k,'v) dict, (k : 'k, v : 'v)) : ('k,'v) dict =
    case d of
      Empty => Node (empty, (k,v), empty)
    | Node (L, (k', v'), R) =>
      case cmp (k,k') of
          EQUAL => Node (L, (k, (c(v,v'))), R)
        | LESS => Node (insertWith (cmp, c, L, (k, v)), (k', v'), R)
        | GREATER => Node (L, (k', v'), insertWith (cmp, c, R, (k, v)))

  fun merge (cmp : 'k * 'k -> order, c : 'v * 'v -> 'v, d1 : ('k,'v) dict , d2 : ('k,'v) dict) : ('k,'v) dict = 
      case d1 of
          Node(Empty, kv1, Empty) => insertWith (cmp, c, d2, kv1)
        | _ => case d2 of
                 Node(Empty, kv2, Empty) => insertWith (cmp, c, d1, kv2)
               | _ => merge' (cmp, c, d1,d2)

  (* TASK: copy toSeq here *)
  fun toSeq(dict: ('k, 'v) dict): ('k * 'v) Seq.seq =
    (case dict of
      Empty => Seq.empty()
      | Node(l, (k,v),r) => let 
      val s1 = toSeq(l)
      val s2 = toSeq(r)
      val Middle = Seq.singleton((k,v))
      in
        Seq.append(s1,Seq.append(Middle, s2))
      end)

  fun map (f, d) = 
      case d of
          Empty => Empty
        | Node(l,(k,v),r) => Node (map (f, l) , (k, f v) , map (f, r))

                     
end

structure Dict :> DICT = TreeDict
