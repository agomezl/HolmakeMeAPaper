structure ppLib = struct
open preamble ppTheory

fun omitprinter _ _ sys _ gs d = sys {gravs=gs,depth=d,binderp=false} o fst o dest_comb
val _ = temp_add_user_printer ("omitprinter",``OMIT x``,omitprinter)

fun hideprinter _ _ sys _ gs d = sys {gravs=gs,depth=d,binderp=false} o fst o dest_comb
val _ = temp_add_user_printer ("hideprinter",``HIDE x``,hideprinter)

fun munge_infix newtok tmname =
    let
      val f = fixity newtok
    in
      remove_rules_for_term tmname ;
      add_rule {term_name = tmname, fixity = valOf f,
                pp_elements = [HardSpace 1, TOK newtok, BreakSpace(1,2)],
                block_style = (AroundEachPhrase, (PP.INCONSISTENT, 0)),
                paren_style = OnlyIfNecessary}
    end

val _ = munge_infix "=" "="
val _ = munge_infix "⇒" "==>"
val _ = munge_infix "⊆" "SUBSET"
val _ = munge_infix "⇔" "<=>"

val _ = add_rule {term_name = "FUNPOW", fixity = Suffix 2100,
                  paren_style = OnlyIfNecessary,
                  block_style = (AroundEachPhrase, (PP.INCONSISTENT, 0)),
                  pp_elements = [TOK "(FUNPOW1)", TM, TOK "(FUNPOW2)"]}

val _ = overload_on("length",``list$LENGTH``);
val _ = add_rule {term_name = "length", fixity = Closefix,
                  paren_style = OnlyIfNecessary,
                  block_style = (AroundEachPhrase, (PP.INCONSISTENT, 0)),
                  pp_elements = [TOK "(LENGTH1)", TM, TOK "(LENGTH2)"]}

(* ----------------------------------------------------------------------
    printing let terms
   ---------------------------------------------------------------------- *)

fun letprinter (tyg, tmg) backend printer ppfns (pgr,lgr,rgr) depth tm =
  let
    open term_pp_types smpp term_pp_utils
    fun syspr gravs t =
      printer { gravs = gravs, depth = decdepth depth, binderp = false } t
    fun pr_vstruct v =
      case v of
          Simple t => printer {gravs = (Top,Top,Top), depth = decdepth depth,
                               binderp = true} t
        | Restricted _ => raise UserPP_Failed
    fun my_strip_abs tm = let
      fun recurse acc t = let
        val (v, body) = pp_dest_abs tmg t
      in
        recurse (v::acc) body
      end handle HOL_ERR _ => (List.rev acc, t)
    in
      recurse [] tm
    end
    val strip_vstructs = term_pp_utils.strip_vstructs tmg
    fun strip_nvstructs n tm = let
      fun strip n acc tm =
        if n <= 0 then (List.rev acc, tm)
        else let
          val (bvar, body) = dest_vstruct tmg {binder=NONE,restrictor=NONE} tm
        in
          strip (n - 1) (bvar :: acc) body
        end
    in
      strip n [] tm
    end

    (* allow any constant that overloads to the string "LET" to be treated as
       a let. *)
    fun is_let0 n tm = let
      val (let_tm,f_tm) = dest_comb(rator tm)
    in
      term_grammar.grammar_name tmg let_tm = SOME "LET" andalso
      (length (#1 (my_strip_abs f_tm)) >= n orelse is_let0 (n + 1) f_tm)
    end handle HOL_ERR _ => false
    val is_let = is_let0 1

    val {add_string, ublock, add_break, ...} = ppfns:ppstream_funs
    fun paren c b p =
      if b then
        ublock c 1 (
          add_string "(" >> p >> add_string ")"
        )
      else p
    fun spacep b = if b then add_break(1, 0) else nothing
    fun find_base acc tm =
      if is_let tm then let
        val (let_tm, args) = strip_comb tm
      in
        find_base (List.nth(args, 1)::acc) (hd args)
      end
      else (acc, tm)

    fun strip_let acc tm =
      if is_let tm then
        let
          val (values, abstr) = find_base [] tm
          val (varnames, body) = strip_nvstructs (length values) abstr
          val name_value_pairs = ListPair.zip (varnames, values)
        in
          strip_let (name_value_pairs :: acc) body
        end
      else (List.rev acc, tm)
    val (andbindings, body) = strip_let [] tm

    fun pr_leteq (bv, tm2) = let
      val (args, rhs_t) = strip_vstructs {binder=NONE,restrictor=NONE} tm2
      val fnarg_bvars = List.concat (map (free_vars o bv2term) args)
      val bvfvs = free_vars (bv2term bv)
    in
      block PP.INCONSISTENT 2
          (record_bvars bvfvs (pr_vstruct bv) >>
           spacep true >>
           record_bvars fnarg_bvars
             (pr_list pr_vstruct (spacep true) args >>
              spacep (not (null args)) >>
              add_string "=" >> add_break (1, 0) >>
              block PP.INCONSISTENT 0 (syspr (Top, Top, Top) rhs_t))) >>
        return bvfvs
    end

    fun record_bvars new =
      (* overriding term_pp_utils's version; this one has a different type
         also *)
      getbvs >- (fn old => setbvs (HOLset.addList(old,new)))

    fun pr_letandseq nvpairs =
      block PP.INCONSISTENT 0
           (mappr_list pr_leteq
                       (add_string " " >> add_string "and" >> spacep true)
                       nvpairs >-
           (return o List.concat)) >-
           record_bvars

    fun pr_let0 tm =
        block PP.CONSISTENT 0 (
         block PP.INCONSISTENT 4 (
           add_string "let" >> add_string " " >>
           pr_list pr_letandseq
                   (add_string " " >> add_string ";" >> add_break (1, 2))
                   andbindings >> add_break(1,0) >> add_string "in"
         ) >> add_break(1,2) >>
         block PP.INCONSISTENT 0 (syspr (RealTop, RealTop, rgr) body)
        )

    fun pr_let lgrav rgrav tm = let
      val addparens = rgrav <> RealTop andalso rgrav <> Top
    in
      getbvs >-
      (fn oldbvs => paren PP.CONSISTENT addparens (pr_let0 tm) >> setbvs oldbvs)
    end
in
  if is_let tm then pr_let lgr rgr tm
  else raise UserPP_Failed
end

val _ = temp_add_user_printer ("bool.LET", ``LET f x``, letprinter)

end
