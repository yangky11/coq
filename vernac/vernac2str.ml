open Printf


let string_of_string s = "\"" ^ String.escaped(s) ^ "\""

(*
let string_of_option string_of_value opt =
  match opt with
  | Some v   -> "Some " ^ (string_of_value v)
  | None     -> "None"


let string_of_list string_of_element l =
  "[" ^ (String.concat "; " (List.map string_of_element l)) ^ "]"



let string_of_Vernacexpr__vernac_flag = function
  | Vernacexpr.VernacProgram          -> "Vernacexpr.VernacProgram"
  | Vernacexpr.VernacPolymorphic poly -> "Vernacexpr.VernacPolymorphic " ^ (string_of_bool poly)
  | Vernacexpr.VernacLocal local      -> "Vernacexpr.VernacLocal " ^ (string_of_bool local)


let string_of_Vernacexpr__cumulative_inductive_parsing_flag = function
  | Vernacexpr.GlobalCumulativity    -> "Vernacexpr.GlobalCumulativity"
  | Vernacexpr.GlobalNonCumulativity -> "Vernacexpr.GlobalNonCumulativity"
  | Vernacexpr.LocalCumulativity     -> "Vernacexpr.LocalCumulativity"
  | Vernacexpr.LocalNonCumulativity  -> "Vernacexpr.LocalNonCumulativity"
*)

let string_of_Loc__source = function
  | Loc.InFile f     ->  "Loc.InFile " ^ (string_of_string f)
  | Loc.ToplevelInput -> "Loc.ToplevelInput"


let string_of_Loc__t {Loc.fname; line_nb; bol_pos; line_nb_last; bol_pos_last; bp; ep} =
  sprintf "Loc.t{Loc.fname = %s; Loc.line_nb = %d; Loc.bol_pos = %d; Loc.line_nb_last = %d; Loc.bol_pos_last = %d; Loc.bp = %d; Loc.ep = %d}"
   (string_of_Loc__source fname) line_nb bol_pos line_nb_last bol_pos_last bp ep

(*
let string_of_CAst__t string_of_v {CAst.v; loc} =
  sprintf "CAst.t{CAst.v = %s; CAst.loc = %s}" (string_of_v v) (string_of_option string_of_Loc__t loc)


let string_of_Names__lstring = string_of_CAst__t string_of_string


let string_of_Names__lident = string_of_CAst__t string_of_string


let string_of_Names__Name__t = function
  | Names.Name.Anonymous    -> "Names.Name.Anonymous"
  | Names.Name.Name id      ->  "Names.Name.Name " ^ (string_of_string id)


let string_of_Names__lname = string_of_CAst__t string_of_Names__Name__t


let string_of_Extend__production_level = function
  | Extend.NextLevel    -> "Extend.NextLevel"
  | Extend.NumLevel i   -> "Extend.NumLevel " ^ (string_of_int i)


let string_of_Extend__gram_assoc = function
  | Extend.NonA     -> "Extend.NonA"
  | Extend.RightA   -> "Extend.RightA"
  | Extend.LeftA    -> "Extend.LeftA"


let string_of_Extend__constr_as_binder_kind = function
  | Extend.AsIdent           -> "Extend.AsIdent"
  | Extend.AsIdentOrPattern  -> "Extend.AsIdentOrPattern"
  | Extend.AsStrictPattern   -> "Extend.AsStrictPattern"


let string_of_Extend__constr_entry_key_gen string_of_a cekg =
  match cekg with
  | Extend.ETName        -> "Extend.ETName"
  | Extend.ETReference   -> "Extend.ETReference"
  | Extend.ETBigint      -> "Extend.ETBigint"
  | Extend.ETBinder b    -> "Extend.ETBinder " ^ (string_of_bool b)
  | Extend.ETConstr a    -> "Extend.ETConstr " ^ (string_of_a a)
  | Extend.ETConstrAsBinder (cabk, a) ->
    sprintf "Extend.ETConstrAsBinder (%s, %s)" (string_of_Extend__constr_as_binder_kind cabk) (string_of_a a)
  | Extend.ETPattern (b, i) ->
    sprintf "Extend.ETPattern (%s, %s)" (string_of_bool b) (string_of_option string_of_int i)
  | Extend.ETOther (s1, s2) ->
    sprintf "Extend.ETOther (%s, %s)" (string_of_string s1) (string_of_string s2)


let string_of_Extend__simple_constr_prod_entry_key scpek =
  string_of_Extend__constr_entry_key_gen (string_of_option string_of_Extend__production_level) scpek


let string_of_Flags__compat_version = function
  | Flags.V8_6     -> "Flags.V8_6"
  | Flags.V8_7     -> "Flags.V8_7"
  | Flags.Current  -> "Flags.Current"


let string_of_Vernacexpr__syntax_modifier = function
  | Vernacexpr.SetItemLevel (sl, pl) ->
    sprintf "Vernacexpr.SetItemLevel (%s, %s)" (string_of_list string_of_string sl) (string_of_Extend__production_level pl)
  | Vernacexpr.SetItemLevelAsBinder (sl, cabk, pl) ->
    sprintf "Vernacexpr.SetItemLevelAsBinder (%s, %s, %s)" (string_of_list string_of_string sl)
     (string_of_Extend__constr_as_binder_kind cabk) (string_of_option string_of_Extend__production_level pl)
  | Vernacexpr.SetLevel i -> "Vernacexpr.SetLevel " ^ (string_of_int i)
  | Vernacexpr.SetAssoc ga -> "Vernacexpr.SetAssoc " ^ (string_of_Extend__gram_assoc ga)
  | Vernacexpr.SetEntryType (s, scpek) ->
      sprintf "Vernacexpr.SetEntryType (%s, %s)" (string_of_string s) (string_of_Extend__simple_constr_prod_entry_key scpek)
  | Vernacexpr.SetOnlyParsing          -> "Vernacexpr.SetOnlyParsing"
  | Vernacexpr.SetOnlyPrinting         -> "Vernacexpr.SetOnlyPrinting"
  | Vernacexpr.SetCompatVersion cv     -> "Vernacexpr.SetCompatVersion " ^ (string_of_Flags__compat_version cv)
  | Vernacexpr.SetFormat (s, ls)       ->
      sprintf "Vernacexpr.SetFormat (%s, %s)" (string_of_string s) (string_of_Names__lstring ls)



let string_of_Misctypes__or_by_notation_r string_of_a obnr =
  match obnr with
  | Misctypes.AN a -> "Misctypes.AN " ^ (string_of_a a)
  | Misctypes.ByNotation (s, so) ->
      sprintf "Misctypes.ByNotation (%s, %s)" (string_of_string s) (string_of_option string_of_string so)


let string_of_Misctypes__or_by_notation string_of_a obn =
  string_of_CAst__t (string_of_Misctypes__or_by_notation_r string_of_a) obn


let string_of_DirPath__t t = string_of_list string_of_string t


let string_of_Libnames__full_path {Libnames.dirpath; basename} =
  sprintf "Libnames.full_path{Libnames.dirpath = %s; Libnames.basename = %s}"
    (string_of_DirPath__t dirpath) (string_of_string basename)


let string_of_Libnames__qualid qid = string_of_Libnames__full_path qid


let string_of_Libnames__reference_r = function
  | Libnames.Qualid qid -> "Libnames.Qualid " ^ (string_of_Libnames__qualid qid)
  | Libnames.Ident id -> "Libnames.Ident " ^ (string_of_string id)


let string_of_Libnames__reference = string_of_CAst__t string_of_Libnames__reference_r


let string_of_Vernacexpr__class_rawexpr = function
  | Vernacexpr.FunClass  -> "Vernacexpr.FunClass"
  | Vernacexpr.SortClass -> "Vernacexpr.SortClass"
  | Vernacexpr.RefClass r_obn -> "Vernacexpr.RefClass " ^ (string_of_Misctypes__or_by_notation string_of_Libnames__reference r_obn)


let string_of_Misctypes__universe_kind string_of_a uk =
  match uk with
  | Misctypes.UAnonymous   -> "Misctypes.UAnonymous"
  | Misctypes.UUnknown     -> "Misctypes.UUnknown"
  | Misctypes.UNamed a     -> "Misctypes.UNamed " ^ (string_of_a a)


let string_of_Glob_term__level_info li = string_of_Misctypes__universe_kind string_of_Libnames__reference li


let string_of_Glob_term__sort_info =
  string_of_list (string_of_option
    (fun (r, i) -> sprintf "(%s, %s)" (string_of_Libnames__reference r) (string_of_int i)))


let string_of_Misctypes__glob_sort_gen string_of_a gsg =
  match gsg with
  | Misctypes.GProp    -> "Misctypes.GProp"
  | Misctypes.GSet     -> "Misctypes.GSet"
  | Misctypes.GType a  -> "Misctypes.GType " ^ (string_of_a a)


let string_of_Glob_term__glob_sort =
  string_of_Misctypes__glob_sort_gen string_of_Glob_term__sort_info


let string_of_Glob_term__glob_level = string_of_Misctypes__glob_sort_gen string_of_Glob_term__level_info


let string_of_Constrexpr__instance_expr ie = string_of_list string_of_Glob_term__glob_level ie


let string_of_Decl_kinds__binding_kind = function
  | Decl_kinds.Explicit -> "Decl_kinds.Explicit"
  | Decl_kinds.Implicit -> "Decl_kinds.Implicit"


let string_of_Constrexpr__binder_kind = function
  | Constrexpr.Default bik -> "Constrexpr.Default " ^ (string_of_Decl_kinds__binding_kind bik)
  | Constrexpr.Generalized (bik1, bik2, b) ->
    sprintf "Constrexpr.Generalized (%s, %s, %s)" (string_of_Decl_kinds__binding_kind bik1)
    (string_of_Decl_kinds__binding_kind bik2) (string_of_bool b)


let string_of_Constrexpr__prim_token = function
  | Constrexpr.Numeral (rnn, si) ->
    sprintf "Constrexpr.Numeral (%s, %s)" rnn (string_of_bool si)
  | Constrexpr.String s -> "Constrexpr.String " ^ (string_of_string s)


let string_of_Constrexpr__explicitation = function
  | Constrexpr.ExplByPos (i, ido) ->
      sprintf "Constrexpr.ExplByPos (%s, %s)" (string_of_int i) (string_of_option string_of_string ido)
  | Constrexpr.ExplByName id ->
      "Constrexpr.ExplByName " ^ (string_of_string id)


let string_of_Constr__case_style = function
  | Constr.LetStyle          -> "Constr.LetStyle"
  | Constr.IfStyle           -> "Constr.IfStyle "
  | Constr.LetPatternStyle   -> "Constr.LetPatternStyle"
  | Constr.MatchStyle        -> "Constr.MatchStyle"
  | Constr.RegularStyle      -> "Constr.RegularStyle"


let string_of_Canary__t t = "##WARNING##"


let string_of_Names__MBId__t (i, id, d) =
  sprintf "(%d, %s, %s)" i (string_of_string id) (string_of_DirPath__t d)


let  rec string_of_Names__ModPath__t = function
  | Names.ModPath.MPfile d -> "Names.ModPath.MPfile " ^ (string_of_DirPath__t d)
  | Names.ModPath.MPbound mbt -> "Names.ModPath.MPbound " ^ (string_of_Names__MBId__t mbt)
  | Names.ModPath.MPdot (mpt, lt) ->
      sprintf "Names.ModPath.MPdot (%s, %s)" (string_of_Names__ModPath__t mpt) lt


let string_of_Names__KerName__t {Names.KerName.canary; modpath; dirpath; knlabel; refhash} =
  sprintf "Names.KerName.t{Names.KerName.canary = %s; Names.KerName.modpath = %s; Names.KerName.dirpath = %s; Names.KerName.knlabel = %s; Names.KerName.refhash = %d}"
    (string_of_Canary__t canary) (string_of_Names__ModPath__t modpath) (string_of_DirPath__t dirpath) knlabel refhash


let string_of_Libnames__object_name (fp, kn) = sprintf "(%s, %s)" (string_of_Libnames__full_path fp)
 (string_of_Names__KerName__t kn)


let string_of_Names__KerPair__t = function
  | Names.KerPair.Same knt ->
      "Names.KerPair.Same " ^ (string_of_Names__KerName__t knt)
  | Names.KerPair.Dual (knt1, knt2) ->
      sprintf "Names.KerPair.Dual (%s, %s)" (string_of_Names__KerName__t knt1)
        (string_of_Names__KerName__t knt2)


let string_of_Names__MutInd__t = function
  | Names.MutInd.Same knt ->
      "Names.MutInd.Same " ^ (string_of_Names__KerName__t knt)
  | Names.MutInd.Dual (knt1, knt2) ->
      sprintf "Names.MutInd.Dual (%s, %s)" (string_of_Names__KerName__t knt1)
        (string_of_Names__KerName__t knt2)


let string_of_Names__Constant__t = function
  | Names.Constant.Same knt ->
      "Names.Constant.Same " ^ (string_of_Names__KerName__t knt)
  | Names.Constant.Dual (knt1, knt2) ->
      sprintf "Names.Constant.Dual (%s, %s)" (string_of_Names__KerName__t knt1)
        (string_of_Names__KerName__t knt2)


let string_of_Names__inductive (mit, i) =
  sprintf "(%s, %s)" (string_of_Names__MutInd__t mit) (string_of_int i)


let string_of_Names__constructor (id, i) =
  sprintf "(%s, %s)" (string_of_Names__inductive id) (string_of_int i)


let string_of_Names__global_reference = function
  | Names.VarRef v ->
      "Names.VarRef " ^ v
  | Names.ConstRef c ->
      "Names.ConstRef " ^ (string_of_Names__Constant__t c)
  | Names.IndRef id ->
      "Names.IndRef " ^ (string_of_Names__inductive id)
  | Names.ConstructRef cons ->
      "Names.ConstructRef " ^ (string_of_Names__constructor cons)


let string_of_Evar_kinds__obligation_definition_status = function
  | Evar_kinds.Define b -> "Evar_kinds.Define " ^ (string_of_bool b)
  | Evar_kinds.Expand   -> "Evar_kinds.Expand"


let string_of_Evar_kinds__matching_var_kind = function
  | Evar_kinds.FirstOrderPatVar id   -> "Evar_kinds.FirstOrderPatVar " ^ (string_of_string id)
  | Evar_kinds.SecondOrderPatVar id  -> "Evar_kinds.FirstOrderPatVar " ^ (string_of_string id)


let string_of_Evar_kinds__t = function
  | Evar_kinds.ImplicitArg (grt, (i, ido), b) ->
      sprintf "Evar_kinds.ImplicitArg (%s, (%s, %s), %s)"
        (string_of_Names__global_reference grt) (string_of_int i)
          (string_of_option string_of_string ido) (string_of_bool b)

  | Evar_kinds.BinderType nt -> "Evar_kinds.BinderType " ^ (string_of_Names__Name__t nt)
  | Evar_kinds.NamedHole id -> "Evar_kinds.NamedHole " ^ (string_of_string id)
  | Evar_kinds.QuestionMark (ods, nt) ->
      sprintf "Evar_kinds.QuestionMark (%s, %s)"
        (string_of_Evar_kinds__obligation_definition_status ods) (string_of_Names__Name__t nt)

  | Evar_kinds.CasesType b -> "Evar_kinds.CasesType " ^ (string_of_bool b)
  | Evar_kinds.InternalHole -> "Evar_kinds.InternalHole"
  | Evar_kinds.TomatchTypeParameter (id, i) ->
      sprintf "Evar_kinds.TomatchTypeParameter (%s, %d)" (string_of_Names__inductive id) i

  | Evar_kinds.GoalEvar -> "Evar_kinds.GoalEvar"
  | Evar_kinds.ImpossibleCase -> "Evar_kinds.ImpossibleCase"
  | Evar_kinds.MatchingVar mvk -> "Evar_kinds.MatchingVar " ^ (string_of_Evar_kinds__matching_var_kind mvk)
  | Evar_kinds.VarInstance id -> "Evar_kinds.VarInstance " ^ (string_of_string id)
  | Evar_kinds.SubEvar et -> "Evar_kinds.SubEvar " ^ (string_of_int et)


let string_of_Misctypes__intro_pattern_naming_expr = function
  | Misctypes.IntroIdentifier id   -> "Misctypes.IntroIdentifier " ^ (string_of_string id)
  | Misctypes.IntroFresh id        -> "Misctypes.IntroFresh " ^ (string_of_string id)
  | Misctypes.IntroAnonymous       -> "Misctypes.IntroAnonymous"


let string_of_Genarg__ArgT__tag = Genarg.ArgT.repr


let rec string_of_Genarg__genarg_type : type a b c. (a, b, c) Genarg.genarg_type -> string = function
  | Genarg.ExtraArg argt ->
      "Genarg.ExtraArg " ^ (string_of_Genarg__ArgT__tag argt)
  | Genarg.ListArg lgt ->
      "Genarg.ListArg " ^ (string_of_Genarg__genarg_type lgt)
  | Genarg.OptArg ogt ->
      "Genarg.OptArg " ^ (string_of_Genarg__genarg_type ogt)
  | Genarg.PairArg (gt1, gt2) ->
      sprintf "Genarg.PairArg (%s, %s)" (string_of_Genarg__genarg_type gt1)
        (string_of_Genarg__genarg_type gt2)


let string_of_Genarg__abstract_argument_type : type a b. (a, b) Genarg.abstract_argument_type -> string = function
  | Genarg.Rawwit gt -> "Genarg.Rawwit " ^ (string_of_Genarg__genarg_type gt)
  | Genarg.Glbwit gt -> "Genarg.Glbwit " ^ (string_of_Genarg__genarg_type gt)
  | Genarg.Topwit gt -> "Genarg.Topwit " ^ (string_of_Genarg__genarg_type gt)


let string_of_Genarg__rlevel = "`rlevel"


let string_of_Genarg__glevel = "`glevel"


let string_of_Genarg__tlevel = "`tlevel"


let string_of_Genarg__raw_generic_argument rga = string_of_string "##WARNING##"
  (*
  try
    Pp.string_of_ppcmds (Pputils.pr_raw_generic (Global.env ()) rga)
  with _ -> "##WARNING##"*)


let string_of_Misctypes__cast_type string_of_a ct =
  match ct with
  | Misctypes.CastConv a      -> "Misctypes.CastConv " ^ (string_of_a a)
  | Misctypes.CastVM a        -> "Misctypes.CastVM " ^ (string_of_a a)
  | Misctypes.CastCoerce      -> "Misctypes.CastCoerce"
  | Misctypes.CastNative a    -> "Misctypes.CastNative " ^ (string_of_a a)


let string_of_Constrexpr__abstraction_kind = function
  | Constrexpr.AbsLambda  -> "Constrexpr.AbsLambda"
  | Constrexpr.AbsPi      -> "Constrexpr.AbsPi"


let rec string_of_Constrexpr__constr_expr expr = string_of_CAst__t string_of_Constrexpr__constr_expr_r expr


and string_of_Constrexpr__constr_expr_r = function
  | Constrexpr.CRef (r, ie) ->
      sprintf "Constrexpr.CRef (%s, %s)" (string_of_Libnames__reference r) (string_of_option string_of_Constrexpr__instance_expr ie)

  | Constrexpr.CFix (l, fl) ->
      sprintf "Constrexpr.CFix (%s, %s)" (string_of_Names__lident l) (string_of_list string_of_Constrexpr__fix_expr fl)

  | Constrexpr.CCoFix (lid, cel) ->
      sprintf "Constrexpr.CCoFix (%s, %s)" (string_of_Names__lident lid)
        (string_of_list string_of_Constrexpr__cofix_expr cel)

  | Constrexpr.CProdN (ll, ce) ->
      sprintf "Constrexpr.CProdN (%s, %s)" (string_of_list string_of_Constrexpr__local_binder_expr ll)
        (string_of_Constrexpr__constr_expr ce)

  | Constrexpr.CLambdaN (ll, ce) ->
      sprintf "Constrexpr.CLambdaN (%s, %s)" (string_of_list string_of_Constrexpr__local_binder_expr ll)
      (string_of_Constrexpr__constr_expr ce)

  | Constrexpr.CLetIn (ln, ce1, ceo, ce2) ->
      sprintf "Constrexpr.CLetIn (%s, %s, %s, %s)" (string_of_Names__lname ln)
        (string_of_Constrexpr__constr_expr ce1) (string_of_option string_of_Constrexpr__constr_expr ceo)
          (string_of_Constrexpr__constr_expr ce2)

  | Constrexpr.CAppExpl ((pf, r, ieo) , cel)->
      sprintf "Constrexpr.CAppExpl ((%s, %s, %s), %s)" (string_of_option string_of_int pf)
        (string_of_Libnames__reference r) (string_of_option string_of_Constrexpr__instance_expr ieo)
          (string_of_list string_of_Constrexpr__constr_expr cel)

  | Constrexpr.CApp ((pf, ce), l) ->
      sprintf "Constrexpr.CApp ((%s, %s), %s)" (string_of_option string_of_int pf)
        (string_of_Constrexpr__constr_expr ce) (string_of_list (fun (ce1, e) ->
          sprintf "(%s, %s)" (string_of_Constrexpr__constr_expr ce1)
            (string_of_option (string_of_CAst__t string_of_Constrexpr__explicitation) e)) l)

  | Constrexpr.CRecord l ->
      "Constrexpr.CRecord " ^
        (string_of_list (fun (r, ce) -> sprintf "(%s, %s)" (string_of_Libnames__reference r) (string_of_Constrexpr__constr_expr ce)) l)

  | Constrexpr.CCases (cs, ceo, cael, be) ->
      sprintf "Constrexpr.CCases (%s, %s, %s, %s)" (string_of_Constr__case_style cs)
        (string_of_option string_of_Constrexpr__constr_expr ceo)
          (string_of_list string_of_Constrexpr__case_expr cael)
            (string_of_list string_of_Constrexpr__branch_expr be)

  | Constrexpr.CLetTuple (ll, (lo, ceo), ce1, ce2) ->
      sprintf "Constrexpr.CLetTuple (%s, (%s, %s), %s, %s)" (string_of_list string_of_Names__lname ll)
        (string_of_option string_of_Names__lname lo) (string_of_option string_of_Constrexpr__constr_expr ceo)
          (string_of_Constrexpr__constr_expr ce1) (string_of_Constrexpr__constr_expr ce2)

  | Constrexpr.CIf (ce1, (lo, ceo), ce2, ce3) ->
      sprintf "Constrexpr.CIf (%s, (%s, %s), %s, %s)" (string_of_Constrexpr__constr_expr ce1)
        (string_of_option string_of_Names__lname lo) (string_of_option string_of_Constrexpr__constr_expr ceo)
        (string_of_Constrexpr__constr_expr ce2) (string_of_Constrexpr__constr_expr ce3)

  | Constrexpr.CHole (ekto, ipne, rgao) ->
      sprintf "Constrexpr.CHole (%s, %s, %s)" (string_of_option string_of_Evar_kinds__t ekto)
        (string_of_Misctypes__intro_pattern_naming_expr ipne)
          (string_of_option string_of_Genarg__raw_generic_argument rgao)

  | Constrexpr.CPatVar p -> " Constrexpr.CPatVar " ^ p
  | Constrexpr.CEvar (en, idcel) ->
      sprintf "Constrexpr.CEvar (%s, %s)" (string_of_string en)
        (string_of_list (fun (id, ce) -> sprintf "(%s, %s)" (string_of_string id) (string_of_Constrexpr__constr_expr ce)) idcel)

  | Constrexpr.CSort gs -> " Constrexpr.CSort " ^ (string_of_Glob_term__glob_sort gs)
  | Constrexpr.CCast (ce, ct) ->
      sprintf "Constrexpr.CCast (%s, %s)" (string_of_Constrexpr__constr_expr ce)
        (string_of_Misctypes__cast_type string_of_Constrexpr__constr_expr  ct)

  | Constrexpr.CNotation (n, cns) ->
      sprintf "Constrexpr.CNotation (%s, %s)" (string_of_string n) (string_of_Constrexpr_constr_notation_substitution cns)

  | Constrexpr.CGeneralization (bik, ako, ce) ->
      sprintf "Constrexpr.CGeneralization (%s, %s, %s)" (string_of_Decl_kinds__binding_kind bik)
        (string_of_option string_of_Constrexpr__abstraction_kind ako)
          (string_of_Constrexpr__constr_expr ce)

  | Constrexpr.CPrim pt -> "Constrexpr.CPrim " ^ (string_of_Constrexpr__prim_token pt)
  | Constrexpr.CDelimiters (s, ce) ->
    sprintf "Constrexpr.CDelimiters (%s, %s)" (string_of_string s) (string_of_Constrexpr__constr_expr ce)


and string_of_Constrexpr__recursion_order_expr = function
  | Constrexpr.CStructRec -> "Constrexpr.CStructRec"
  | Constrexpr.CWfRec ce  -> "Constrexpr.CWfRec " ^ (string_of_Constrexpr__constr_expr ce)
  | Constrexpr.CMeasureRec (ce, ceo) ->
    sprintf "Constrexpr.CMeasureRec (%s, %s)" (string_of_Constrexpr__constr_expr ce)
      (string_of_option string_of_Constrexpr__constr_expr ceo)


and string_of_Constrexpr_cases_pattern_expr cpe = string_of_CAst__t string_of_Constrexpr__cases_pattern_expr_r cpe


and string_of_Constrexpr__cases_pattern_expr_r = function
  | Constrexpr.CPatAlias (cpe, ln) ->
      sprintf "Constrexpr.CPatAlias (%s, %s)" (string_of_Constrexpr_cases_pattern_expr cpe)
        (string_of_Names__lname ln)

  | Constrexpr.CPatCstr (r, cpelo, cpel) ->
      sprintf "Constrexpr.CPatCstr (%s, %s, %s)" (string_of_Libnames__reference r)
        (string_of_option (string_of_list string_of_Constrexpr_cases_pattern_expr) cpelo)
        (string_of_list string_of_Constrexpr_cases_pattern_expr cpel)

  | Constrexpr.CPatAtom ro ->
      "Constrexpr.CPatAtom " ^ (string_of_option string_of_Libnames__reference ro)

  | Constrexpr.CPatOr l ->
      "Constrexpr.CPatOr " ^ (string_of_list string_of_Constrexpr_cases_pattern_expr l)

  | Constrexpr.CPatNotation (n, cpns, cpel) ->
      sprintf "Constrexpr.CPatNotation (%s, %s, %s)" (string_of_string n)
        (string_of_Constrexpr__cases_pattern_notation_substitution cpns)
          (string_of_list string_of_Constrexpr_cases_pattern_expr cpel)

  | Constrexpr.CPatPrim pt ->
      "Constrexpr.CPatPrim " ^ (string_of_Constrexpr__prim_token pt)

  | Constrexpr.CPatRecord l ->
      "Constrexpr.CPatRecord " ^ (string_of_list (fun (r, cpe) ->
        sprintf "(%s, %s)" (string_of_Libnames__reference r)
          (string_of_Constrexpr_cases_pattern_expr cpe)) l)

  | Constrexpr.CPatDelimiters (s, cpe) ->
      sprintf "Constrexpr.CPatDelimiters (%s, %s)" (string_of_string s) (string_of_Constrexpr_cases_pattern_expr cpe)

  | Constrexpr.CPatCast (cpe, ce) ->
      sprintf "Constrexpr.CPatCast (%s, %s)" (string_of_Constrexpr_cases_pattern_expr cpe)
        (string_of_Constrexpr__constr_expr ce)


and string_of_Constrexpr__cases_pattern_exprand cpe = string_of_CAst__t string_of_Constrexpr__cases_pattern_expr_r cpe


and string_of_Constrexpr__cases_pattern_notation_substitution (cpel, cpell) =
  sprintf "(%s, %s)" (string_of_list string_of_Constrexpr__cases_pattern_exprand cpel)
    (string_of_list (string_of_list string_of_Constrexpr__cases_pattern_exprand) cpell)


and string_of_Constrexpr__local_binder_expr = function
  | Constrexpr.CLocalAssum (l, bk, ce) ->
      sprintf "Constrexpr.CLocalAssum (%s, %s, %s)" (string_of_list string_of_Names__lname l) (string_of_Constrexpr__binder_kind bk)
        (string_of_Constrexpr__constr_expr ce)
  | Constrexpr.CLocalDef (ln, ce, ceo) ->
      sprintf " Constrexpr.CLocalDef (%s, %s, %s)" (string_of_Names__lname ln)
        (string_of_Constrexpr__constr_expr ce)
          (string_of_option string_of_Constrexpr__constr_expr ceo)
  | Constrexpr.CLocalPattern ast ->
      "Constrexpr.CLocalPattern " ^ (string_of_CAst__t
        (fun (cpe, ceo) -> sprintf "(%s, %s)" (string_of_Constrexpr_cases_pattern_expr cpe)
          (string_of_option string_of_Constrexpr__constr_expr ceo)) ast)


and string_of_Constrexpr__fix_expr (l, (lo, roe), lbel, ce1, ce2) =
  sprintf "(%s, (%s, %s), %s, %s, %s)" (string_of_Names__lident l) (string_of_option string_of_Names__lident lo)
    (string_of_Constrexpr__recursion_order_expr roe) (string_of_list string_of_Constrexpr__local_binder_expr lbel) (string_of_Constrexpr__constr_expr ce1)
      (string_of_Constrexpr__constr_expr ce2)


and string_of_Constrexpr__case_expr (ce, lno, cpeo) =
  sprintf "(%s, %s, %s)" (string_of_Constrexpr__constr_expr ce)
    (string_of_option string_of_Names__lname lno)
      (string_of_option string_of_Constrexpr_cases_pattern_expr cpeo)


and string_of_Constrexpr__branch_expr be =
  string_of_CAst__t (fun (cpell, ce) -> sprintf "(%s, %s)"
    (string_of_list (string_of_list string_of_Constrexpr__cases_pattern_exprand) cpell)
      (string_of_Constrexpr__constr_expr ce)) be


and string_of_Constrexpr__cofix_expr (l, lbel, ce1, ce2) =
  sprintf "(%s, %s, %s, %s)" (string_of_Names__lident l)
    (string_of_list string_of_Constrexpr__local_binder_expr lbel) (string_of_Constrexpr__constr_expr ce1)
      (string_of_Constrexpr__constr_expr ce2)


and string_of_Constrexpr_constr_notation_substitution (cel, cell, cpel, lbell) =
  sprintf "(%s, %s, %s, %s)" (string_of_list string_of_Constrexpr__constr_expr cel)
    (string_of_list (string_of_list string_of_Constrexpr__constr_expr) cell)
      (string_of_list string_of_Constrexpr_cases_pattern_expr cpel)
        (string_of_list (string_of_list string_of_Constrexpr__local_binder_expr) lbell)


let string_of_Vernacexpr__opacity_flag = function
  | Vernacexpr.Opaque       -> "Vernacexpr.Opaque"
  | Vernacexpr.Transparent  -> "Vernacexpr.Transparent"


let string_of_Vernacexpr__proof_end = function
  | Vernacexpr.Admitted -> "Vernacexpr.Admitted"
  | Vernacexpr.Proved (opf, lo) ->
      sprintf "Vernacexpr.Proved (%s, %s)" (string_of_Vernacexpr__opacity_flag opf)
        (string_of_option string_of_Names__lident lo)


let string_of_Decl_kinds__theorem_kind = function
  | Decl_kinds.Theorem      -> "Decl_kinds.Theorem"
  | Decl_kinds.Lemma        -> "Decl_kinds.Lemma"
  | Decl_kinds.Fact         -> "Decl_kinds.Fact"
  | Decl_kinds.Remark       -> "Decl_kinds.Remark"
  | Decl_kinds.Property     -> "Decl_kinds.Property"
  | Decl_kinds.Proposition  -> "Decl_kinds.Proposition"
  | Decl_kinds.Corollary    -> "Decl_kinds.Corollary"


let string_of_Misctypes__gen_universe_decl string_of_a string_of_b
  {Misctypes.univdecl_instance=ui; univdecl_extensible_instance=uei;
    univdecl_constraints=uc; univdecl_extensible_constraints=uec} =
      sprintf "Misctypes.gen_universe_decl{Misctypes.univdecl_instance = %s; Misctypes.univdecl_extensible_instance = %s; Misctypes.univdecl_constraints = %s; Misctypes.univdecl_extensible_constraints = %s}"
        (string_of_a ui) (string_of_bool uei) (string_of_b uc) (string_of_bool uec)


let string_of_Univ__constraint_type = function
  | Univ.Lt -> "Univ.Lt"
  | Univ.Le -> "Univ.Le"
  | Univ.Eq -> "Univ.Eq"


let string_of_Glob_term__glob_constraint (gl1, ct, gl2) =
  sprintf "(%s, %s, %s)" (string_of_Glob_term__glob_level gl1)
    (string_of_Univ__constraint_type ct) (string_of_Glob_term__glob_level gl2)


let string_of_Constrexpr__universe_decl_expr = string_of_Misctypes__gen_universe_decl
  (string_of_list string_of_Names__lident) (string_of_list string_of_Glob_term__glob_constraint)


let string_of_Constrexpr__ident_decl (l, udeo) =
  sprintf "(%s, %s)" (string_of_Names__lident l)
    (string_of_option string_of_Constrexpr__universe_decl_expr udeo)


let string_of_Vernacexpr__proof_expr (id, (lbel, ce)) =
  sprintf "(%s, (%s, %s))" (string_of_Constrexpr__ident_decl id)
    (string_of_list string_of_Constrexpr__local_binder_expr lbel)
      (string_of_Constrexpr__constr_expr ce)


let string_of_Decl_kinds__discharge = function
  | Decl_kinds.DoDischarge -> "Decl_kinds.DoDischarge"
  | Decl_kinds.NoDischarge -> "Decl_kinds.NoDischarge"


let string_of_Decl_kinds__definition_object_kind = function
  | Decl_kinds.Definition            -> "Decl_kinds.Definition"
  | Decl_kinds.Coercion              -> "Decl_kinds.Coercion"
  | Decl_kinds.SubClass              -> "Decl_kinds.SubClass"
  | Decl_kinds.CanonicalStructure    -> "Decl_kinds.CanonicalStructure"
  | Decl_kinds.Example               -> "Decl_kinds.Example"
  | Decl_kinds.Fixpoint              -> "Decl_kinds.Fixpoint"
  | Decl_kinds.CoFixpoint            -> "Decl_kinds.CoFixpoint"
  | Decl_kinds.Scheme                -> "Decl_kinds.Scheme"
  | Decl_kinds.StructureComponent    -> "Decl_kinds.StructureComponent"
  | Decl_kinds.IdentityCoercion      -> "Decl_kinds.IdentityCoercion"
  | Decl_kinds.Instance              -> "Decl_kinds.Instance"
  | Decl_kinds.Method                -> "Decl_kinds.Method"
  | Decl_kinds.Let                   -> "Decl_kinds.Let"


let string_of_Constrexpr__name_decl (l, udeo) =
  sprintf "(%s, %s)" (string_of_Names__lname l)
    (string_of_option string_of_Constrexpr__universe_decl_expr udeo)


let string_of_Genredexpr__glob_red_flag string_of_a {Genredexpr.rBeta; rMatch;
  rFix; rCofix; rZeta; rDelta; rConst} =
    sprintf "Genredexpr.glob_red_flag{Genredexpr.rBeta = %s; Genredexpr.rMatch = %s; Genredexpr.rFix = %s; Genredexpr.rCofix = %s; Genredexpr.rZeta = %s; Genredexpr.rDelta = %s; Genredexpr.rConst = %s}"
      (string_of_bool rBeta) (string_of_bool rMatch) (string_of_bool rFix) (string_of_bool rCofix)
        (string_of_bool rZeta) (string_of_bool rDelta) (string_of_list string_of_a rConst)


let string_of_Locus__occurrences_gen string_of_a og =
  match og with
    | Locus.AllOccurrences  -> "Locus.AllOccurrences"
    | Locus.AllOccurrencesBut al ->
        "Locus.AllOccurrencesBut " ^ (string_of_list string_of_a al)
    | Locus.NoOccurrences -> "Locus.NoOccurrences"
    | Locus.OnlyOccurrences al ->
        "Locus.OnlyOccurrences " ^ (string_of_list string_of_a al)


let string_of_Misctypes__or_var string_of_a ov =
  match ov with
    | Misctypes.ArgArg a -> "Misctypes.ArgArg " ^ (string_of_a a)
    | Misctypes.ArgVar l -> "Misctypes.ArgVar " ^ (string_of_Names__lident l)


let string_of_Locus__occurrences_expr = string_of_Locus__occurrences_gen (string_of_Misctypes__or_var string_of_int)


let string_of_Locus__with_occurrences string_of_a (oe, a) =
  sprintf "(%s, %s)" (string_of_Locus__occurrences_expr oe) (string_of_a a)


let string_of_Util__union string_of_a string_of_b u =
  match u with
    | Util.Inl a -> "Util.Inl " ^ (string_of_a a)
    | Util.Inr b -> "Util.Inr " ^ (string_of_b b)


let rec string_of_Vernacexpr__section_subset_expr = function
  | Vernacexpr.SsEmpty     -> "Vernacexpr.SsEmpty"
  | Vernacexpr.SsType      -> "Vernacexpr.SsType"
  | Vernacexpr.SsSingl id  -> "Vernacexpr.SsSingl " ^ (string_of_Names__lident id)
  | Vernacexpr.SsCompl sse -> "Vernacexpr.SsCompl " ^ (string_of_Vernacexpr__section_subset_expr sse)
  | Vernacexpr.SsUnion (sse1, sse2) ->
      sprintf "Vernacexpr.SsUnion (%s, %s)" (string_of_Vernacexpr__section_subset_expr sse1)
      (string_of_Vernacexpr__section_subset_expr sse2)

  | Vernacexpr.SsSubstr  (sse1, sse2) ->
      sprintf "Vernacexpr.SsSubstr (%s, %s)" (string_of_Vernacexpr__section_subset_expr sse1)
        (string_of_Vernacexpr__section_subset_expr sse2)

  | Vernacexpr.SsFwdClose sse -> "Vernacexpr.SsFwdClose " ^ (string_of_Vernacexpr__section_subset_expr sse)


let string_of_Genredexpr__red_expr_gen string_of_a string_of_b string_of_c reg =
  match reg with
    | Genredexpr.Red b -> "Genredexpr.Red " ^ (string_of_bool b)
    | Genredexpr.Hnf   -> "Genredexpr.Hnf"
    | Genredexpr.Simpl (grf, woo) ->
        sprintf "Genredexpr.Simpl (%s, %s)" (string_of_Genredexpr__glob_red_flag string_of_b grf)
          (string_of_option (string_of_Locus__with_occurrences (string_of_Util__union string_of_b string_of_c)) woo)

    | Genredexpr.Cbv grf ->
        "Genredexpr.Cbv " ^ (string_of_Genredexpr__glob_red_flag string_of_b grf)
    | Genredexpr.Cbn grf ->
        "Genredexpr.Cbn " ^ (string_of_Genredexpr__glob_red_flag string_of_b grf)
    | Genredexpr.Lazy grf ->
        "Genredexpr.Lazy " ^ (string_of_Genredexpr__glob_red_flag string_of_b grf)
    | Genredexpr.Unfold wol ->
        "Genredexpr.Unfold " ^ (string_of_list (string_of_Locus__with_occurrences string_of_b) wol)
    | Genredexpr.Fold al ->
        "Genredexpr.Fold " ^ (string_of_list string_of_a al)
    | Genredexpr.Pattern wol ->
        "Genredexpr.Pattern " ^ (string_of_list (string_of_Locus__with_occurrences string_of_a) wol)
    | Genredexpr.ExtraRedExpr s -> "Genredexpr.ExtraRedExpr " ^ (string_of_string s)
    | Genredexpr.CbvVm woo ->
        "Genredexpr.CbvVm " ^ (string_of_option (string_of_Locus__with_occurrences
          (string_of_Util__union string_of_b string_of_c)) woo)

    | Genredexpr.CbvNative woo ->
        "Genredexpr.CbvNative " ^ (string_of_option (string_of_Locus__with_occurrences
        (string_of_Util__union string_of_b string_of_c)) woo)


let string_of_Genredexpr__raw_red_expr = string_of_Genredexpr__red_expr_gen
  string_of_Constrexpr__constr_expr (string_of_Misctypes__or_by_notation string_of_Libnames__reference)
    string_of_Constrexpr__constr_expr


let string_of_Vernacexpr__definition_expr = function
  | Vernacexpr.ProveBody (lbel, ce) ->
      sprintf "Vernacexpr.ProveBody (%s, %s)" (string_of_list string_of_Constrexpr__local_binder_expr lbel)
        (string_of_Constrexpr__constr_expr ce)

  | Vernacexpr.DefineBody (lbel, rreo, ce, ceo) ->
      sprintf "Vernacexpr.DefineBody (%s, %s, %s, %s)"
      (string_of_list string_of_Constrexpr__local_binder_expr lbel)
        (string_of_option string_of_Genredexpr__raw_red_expr rreo)
          (string_of_Constrexpr__constr_expr ce)
            (string_of_option string_of_Constrexpr__constr_expr ceo)


let string_of_Decl_kinds__assumption_object_kind = function
  | Decl_kinds.Definitional  -> "Decl_kinds.Definitional"
  | Decl_kinds.Logical       -> "Decl_kinds.Logical"
  | Decl_kinds.Conjectural   -> "Decl_kinds.Conjectural"


let string_of_Vernacexpr__inline = function
  | Vernacexpr.NoInline       -> "Vernacexpr.NoInline"
  | Vernacexpr.DefaultInline  -> "Vernacexpr.DefaultInline"
  | Vernacexpr.InlineAt i     -> "Vernacexpr.InlineAt " ^ (string_of_int i)


let string_of_Vernacexpr__with_coercion string_of_a (cf, a) =
  sprintf "(%s, %s)" (string_of_bool cf) (string_of_a a)


let string_of_Vernacexpr__with_instance string_of_a (iflag, a) =
  sprintf "(%s, %s)" (string_of_option string_of_bool iflag) (string_of_a a)


let string_of_Vernacexpr__decl_notation (ls, ce, sno) =
  sprintf "(%s, %s, %s)" (string_of_Names__lstring ls) (string_of_Constrexpr__constr_expr ce)
    (string_of_option string_of_string sno)


let string_of_Vernacexpr__with_notation string_of_a (a, dnl) =
  sprintf "(%s, %s)" (string_of_a a) (string_of_list string_of_Vernacexpr__decl_notation dnl)


let string_of_Vernacexpr__with_priority string_of_a (a, io) =
  sprintf "(%s, %s)" (string_of_a a) (string_of_option string_of_int io)


let string_of_Declarations__recursivity_kind = function
  | Declarations.Finite    -> "Declarations.Finite"
  | Declarations.CoFinite  -> "Declarations.CoFinite"
  | Declarations.BiFinite  -> "Declarations.BiFinite"


let string_of_Vernacexpr__inductive_kind = function
  | Vernacexpr.Inductive_kw   -> "Vernacexpr.Inductive_kw"
  | Vernacexpr.CoInductive    -> "Vernacexpr.CoInductive"
  | Vernacexpr.Variant        -> "Vernacexpr.Variant"
  | Vernacexpr.Record         -> "Vernacexpr.Record"
  | Vernacexpr.Structure      -> "Vernacexpr.Structure"
  | Vernacexpr.Class b        -> "Vernacexpr.Class " ^ (string_of_bool b)


let string_of_Vernacexpr__constructor_expr = string_of_Vernacexpr__with_coercion
  (fun (lid, ce) -> sprintf "(%s, %s)" (string_of_Names__lident lid) (string_of_Constrexpr__constr_expr ce))


let string_of_Vernacexpr__local_decl_expr = function
  | Vernacexpr.AssumExpr (ln, ce) ->
      sprintf "Vernacexpr.AssumExpr (%s, %s)" (string_of_Names__lname ln)
        (string_of_Constrexpr__constr_expr ce)
  | Vernacexpr.DefExpr (ln, ce, ceo) ->
      sprintf "Vernacexpr.DefExpr (%s, %s, %s)" (string_of_Names__lname ln)
        (string_of_Constrexpr__constr_expr ce) (string_of_option string_of_Constrexpr__constr_expr ceo)


let string_of_Vernacexpr__constructor_list_or_record_decl_expr = function
  | Vernacexpr.Constructors coel ->
      "Vernacexpr.Constructors " ^ (string_of_list string_of_Vernacexpr__constructor_expr coel)
  | Vernacexpr.RecordDecl (lido, ldewiwpwnl) ->
      sprintf "Vernacexpr.RecordDecl (%s, %s)" (string_of_option string_of_Names__lident lido)
        (string_of_list (string_of_Vernacexpr__with_notation (string_of_Vernacexpr__with_priority
          (string_of_Vernacexpr__with_instance string_of_Vernacexpr__local_decl_expr))) ldewiwpwnl)


let string_of_Vernacexpr__inductive_expr (wc, lbel, ceo, ik, clorde) =
  sprintf "(%s, %s, %s, %s, %s)" (string_of_Vernacexpr__with_coercion string_of_Constrexpr__ident_decl  wc)
    (string_of_list string_of_Constrexpr__local_binder_expr lbel)
      (string_of_option string_of_Constrexpr__constr_expr ceo)
        (string_of_Vernacexpr__inductive_kind ik)
          (string_of_Vernacexpr__constructor_list_or_record_decl_expr clorde)


let string_of_Vernacexpr__fixpoint_expr (idl, loroe, lbel, ce, ceo) =
  sprintf "(%s, %s, %s, %s, %s)" (string_of_Constrexpr__ident_decl idl)
    ((fun (lo, roe) -> sprintf "(%s, %s)" (string_of_option string_of_Names__lident lo) (string_of_Constrexpr__recursion_order_expr roe)) loroe)
      (string_of_list string_of_Constrexpr__local_binder_expr lbel)
        (string_of_Constrexpr__constr_expr ce) (string_of_option string_of_Constrexpr__constr_expr ceo)


let stirng_of_Vernacexpr__bullet = function
  | Vernacexpr.Dash i -> "Vernacexpr.Dash " ^ (string_of_int i)
  | Vernacexpr.Star i -> "Vernacexpr.Star " ^ (string_of_int i)
  | Vernacexpr.Plus i -> "Vernacexpr.Plus " ^ (string_of_int i)


let string_of_Vernacexpr__goal_selector = function
  | Vernacexpr.SelectNth i -> "Vernacexpr.SelectNth " ^ (string_of_int i)
  | Vernacexpr.SelectList iil ->
    "Vernacexpr.SelectList " ^ (string_of_list
      (fun (i1, i2) -> sprintf "(%s, %s)" (string_of_int i1) (string_of_int i2)) iil)

  | Vernacexpr.SelectId id -> "Vernacexpr.SelectId " ^ (string_of_string id)
  | Vernacexpr.SelectAll -> "Vernacexpr.SelectAll"


let string_of_Sorts__family = function
 | Sorts.InProp -> "Sorts.InProp"
 | Sorts.InSet  -> "Sorts.InSet"
 | Sorts.InType -> "Sorts.InType"


let string_of_Vernacexpr__scheme = function
  | Vernacexpr.InductionScheme (b, r_obn, se) ->
      sprintf "Vernacexpr.InductionScheme (%s, %s, %s)" (string_of_bool b)
        (string_of_Misctypes__or_by_notation string_of_Libnames__reference r_obn)
          (string_of_Sorts__family se)

  | Vernacexpr.CaseScheme (b, r_obn, se) ->
    sprintf "Vernacexpr.CaseScheme (%s, %s, %s)" (string_of_bool b)
      (string_of_Misctypes__or_by_notation string_of_Libnames__reference r_obn)
        (string_of_Sorts__family se)

  | Vernacexpr.EqualityScheme r_obn ->
      "Vernacexpr.EqualityScheme " ^ (string_of_Misctypes__or_by_notation string_of_Libnames__reference r_obn)


let string_of_Vernacexpr__cofixpoint_expr (id, lbel, ce, ceo) =
  sprintf "(%s, %s, %s, %s)" (string_of_Constrexpr__ident_decl id)
    (string_of_list string_of_Constrexpr__local_binder_expr lbel)
      (string_of_Constrexpr__constr_expr ce) (string_of_option string_of_Constrexpr__constr_expr ceo)


let string_of_Constrexpr__with_declaration_ast = function
  | Constrexpr.CWith_Module (idlt, qt) ->
      sprintf "Constrexpr.CWith_Module (%s, %s)" (string_of_CAst__t (string_of_list string_of_string) idlt)
        (string_of_CAst__t string_of_Libnames__qualid qt)

  | Constrexpr.CWith_Definition (idlt, udeo, ce) ->
      sprintf "Constrexpr.CWith_Definition (%s, %s, %s)" (string_of_CAst__t (string_of_list string_of_string) idlt)
        (string_of_option string_of_Constrexpr__universe_decl_expr udeo)
          (string_of_Constrexpr__constr_expr ce)


let rec string_of_Constrexpr__module_ast ma = string_of_CAst__t string_of_Constrexpr__module_ast_r ma


and string_of_Constrexpr__module_ast_r = function
  | Constrexpr.CMident qid ->
      "Constrexpr.CMident " ^ (string_of_Libnames__qualid qid)
  | Constrexpr.CMapply (ma1, ma2) ->
      sprintf "Constrexpr.CMapply (%s, %s)" (string_of_Constrexpr__module_ast ma1)
        (string_of_Constrexpr__module_ast ma2)
  | Constrexpr.CMwith (ma, wda) ->
      sprintf "Constrexpr.CMwith (%s, %s)" (string_of_Constrexpr__module_ast ma)
        (string_of_Constrexpr__with_declaration_ast wda)


let string_of_Vernacexpr__module_ast_inl (ma, i) =
  sprintf "(%s, %s)" (string_of_Constrexpr__module_ast ma) (string_of_Vernacexpr__inline i)


let string_of_Vernacexpr__module_binder (bo, lidl, mai) =
  sprintf "(%s, %s, %s)" (string_of_option string_of_bool bo)
    (string_of_list string_of_Names__lident lidl) (string_of_Vernacexpr__module_ast_inl mai)


let string_of_Vernacexpr__module_signature string_of_a ms =
  match ms with
    | Vernacexpr.Enforce a -> "Vernacexpr.Enforce " ^ (string_of_a a)
    | Vernacexpr.Check al  -> "Vernacexpr.Check " ^ (string_of_list string_of_a al)


let string_of_Vernacexpr__comment = function
  | Vernacexpr.CommentConstr ce ->
      "Vernacexpr.CommentConstr " ^ (string_of_Constrexpr__constr_expr ce)
  | Vernacexpr.CommentString s -> "Vernacexpr.CommentString " ^ (string_of_string s)
  | Vernacexpr.CommentInt i -> "Vernacexpr.CommentInt " ^ (string_of_int i)


let string_of_Vernacexpr__register_kind = function
  | Vernacexpr.RegisterInline -> "Vernacexpr.RegisterInline"


let string_of_Vernacexpr__locatable = function
  | Vernacexpr.LocateAny r_obn ->
      "Vernacexpr.LocateAny " ^ (string_of_Misctypes__or_by_notation string_of_Libnames__reference r_obn)
  | Vernacexpr.LocateTerm r_obn ->
      "Vernacexpr.LocateTerm " ^ (string_of_Misctypes__or_by_notation string_of_Libnames__reference r_obn)
  | Vernacexpr.LocateLibrary r ->
      "Vernacexpr.LocateLibrary " ^ (string_of_Libnames__reference r)
  | Vernacexpr.LocateModule r ->
      "Vernacexpr.LocateModule " ^ (string_of_Libnames__reference r)
  | Vernacexpr.LocateOther (s, r) ->
      sprintf "Vernacexpr.LocateOther (%s, %s)" (string_of_string s) (string_of_Libnames__reference r)
  | Vernacexpr.LocateFile s ->
      "Vernacexpr.LocateFile " ^ (string_of_string s)

let string_of_Goptions__option_value = function
  | Goptions.BoolValue b       -> "Goptions.BoolValue " ^ (string_of_bool b)
  | Goptions.IntValue io       -> "Goptions.IntValue " ^ (string_of_option string_of_int io)
  | Goptions.StringValue s     -> "Goptions.StringValue " ^ (string_of_string s)
  | Goptions.StringOptValue so ->
      "Goptions.StringOptValue " ^ (string_of_option string_of_string so)


let string_of_Vernacexpr__hint_info_gen string_of_a {Vernacexpr.hint_priority; hint_pattern} =
  sprintf "Vernacexpr.hint_info_gen{Vernacexpr.hint_priority = %s; Vernacexpr.hint_pattern = %s}"
    (string_of_option string_of_int hint_priority) (string_of_option string_of_a hint_pattern)


let string_of_Vernacexpr__hint_info_expr = string_of_Vernacexpr__hint_info_gen string_of_Constrexpr__constr_expr


let string_of_Vernacexpr__reference_or_constr = function
  | Vernacexpr.HintsReference r -> "Vernacexpr.HintsReference " ^ (string_of_Libnames__reference r)
  | Vernacexpr.HintsConstr ce -> "Vernacexpr.HintsConstr " ^ (string_of_Constrexpr__constr_expr ce)


let string_of_Vernacexpr__hint_mode = function
  | Vernacexpr.ModeInput      -> "Vernacexpr.ModeInput"
  | Vernacexpr.ModeNoHeadEvar -> "Vernacexpr.ModeNoHeadEvar"
  | Vernacexpr.ModeOutput     -> "Vernacexpr.ModeOutput"


let string_of_Vernacexpr__hints_expr = function
  | Vernacexpr.HintsResolve l ->
      "Vernacexpr.HintsResolve " ^ (string_of_list (fun (hie, b, roc) ->
        sprintf "(%s, %s, %s)" (string_of_Vernacexpr__hint_info_expr hie)
          (string_of_bool b) (string_of_Vernacexpr__reference_or_constr roc)) l)

  | Vernacexpr.HintsImmediate rocl ->
      "Vernacexpr.HintsImmediate " ^ (string_of_list string_of_Vernacexpr__reference_or_constr rocl)
  | Vernacexpr.HintsUnfold rl ->
      "Vernacexpr.HintsUnfold " ^ (string_of_list string_of_Libnames__reference rl)
  | Vernacexpr.HintsTransparency (rl, b) ->
      sprintf "Vernacexpr.HintsTransparency (%s, %s)" (string_of_list string_of_Libnames__reference rl)
        (string_of_bool b)

  | Vernacexpr.HintsMode (r, hml) ->
      sprintf "Vernacexpr.HintsMode (%s, %s)" (string_of_Libnames__reference r)
        (string_of_list string_of_Vernacexpr__hint_mode hml)

  | Vernacexpr.HintsConstructors rl ->
      "Vernacexpr.HintsConstructors " ^ (string_of_list string_of_Libnames__reference rl)
  | Vernacexpr.HintsExtern (i, ceo, rga) ->
      sprintf "Vernacexpr.HintsExtern (%s, %s, %s)" (string_of_int i)
        (string_of_option string_of_Constrexpr__constr_expr ceo) (string_of_Genarg__raw_generic_argument rga)


let string_of_Vernacexpr__extend_name (s, i) =
  sprintf "(%s, %d)" (string_of_string s) i


let string_of_Vernacexpr__vernac_implicit_status = function
  | Vernacexpr.Implicit          -> "Vernacexpr.Implicit"
  | Vernacexpr.MaximallyImplicit -> "Vernacexpr.MaximallyImplicit"
  | Vernacexpr.NotImplicit       -> "Vernacexpr.NotImplicit"


let string_of_Vernacexpr__vernac_argument_status {Vernacexpr.name; recarg_like; notation_scope; implicit_status} =
  sprintf "Vernacexpr.vernac_argument_status{Vernacexpr.name = %s; Vernacexpr.recarg_like = %s; Vernacexpr.notation_scope = %s; Vernacexpr.implicit_status = %s}"
    (string_of_Names__Name__t name) (string_of_bool recarg_like) (string_of_option (string_of_CAst__t string_of_string) notation_scope)
      (string_of_Vernacexpr__vernac_implicit_status implicit_status)


let string_of_Vernacexpr__option_ref_value = function
  | Vernacexpr.StringRefValue s -> "Vernacexpr.StringRefValue " ^ (string_of_string s)
  | Vernacexpr.QualidRefValue r ->
      "Vernacexpr.QualidRefValue " ^ (string_of_Libnames__reference r)


let string_of_Vernacexpr__typeclass_constraint (nd, bk, ce) =
  sprintf "(%s, %s, %s)" (string_of_Constrexpr__name_decl nd)
    (string_of_Decl_kinds__binding_kind bk) (string_of_Constrexpr__constr_expr ce)


let string_of_Vernacexpr__simple_binder (lidl, ce) =
  sprintf "(%s, %s)" (string_of_list string_of_Names__lident lidl) (string_of_Constrexpr__constr_expr ce)


let string_of_Conv_oracle__level = function
  | Conv_oracle.Expand  -> "Conv_oracle.Expand"
  | Conv_oracle.Level i -> "Conv_oracle.Level " ^ (string_of_int i)
  | Conv_oracle.Opaque  -> "Conv_oracle.Opaque"


let string_of_Vernacexpr__goal_reference = function
    | Vernacexpr.OpenSubgoals -> "Vernacexpr.OpenSubgoals"
    | Vernacexpr.NthGoal i -> "Vernacexpr.NthGoal " ^ (string_of_int i)
    | Vernacexpr.GoalId id -> "Vernacexpr.GoalId " ^ (string_of_string id)


let string_of_Vernacexpr__search_about_item = function
  | Vernacexpr.SearchSubPattern ce -> "Vernacexpr.SearchSubPattern " ^ (string_of_Constrexpr__constr_expr ce)
  | Vernacexpr.SearchString (s, sno) ->
      sprintf "Vernacexpr.SearchString (%s, %s)" (string_of_string s) (string_of_option string_of_string sno)


let string_of_Vernacexpr__searchable = function
  | Vernacexpr.SearchPattern ce -> "Vernacexpr.SearchPattern " ^ (string_of_Constrexpr__constr_expr ce)
  | Vernacexpr.SearchRewrite ce -> "Vernacexpr.SearchRewrite " ^ (string_of_Constrexpr__constr_expr ce)
  | Vernacexpr.SearchHead ce -> "Vernacexpr.SearchHead " ^ (string_of_Constrexpr__constr_expr ce)
  | Vernacexpr.SearchAbout bsl ->
      "Vernacexpr.SearchAbout " ^ (string_of_list (fun (b, sai) ->
        sprintf "(%s, %s)" (string_of_bool b) (string_of_Vernacexpr__search_about_item sai)) bsl)


let string_of_Vernacexpr__showable = function
  | Vernacexpr.ShowGoal gr -> "Vernacexpr.ShowGoal " ^ (string_of_Vernacexpr__goal_reference gr)
  | Vernacexpr.ShowProof -> "Vernacexpr.ShowProof"
  | Vernacexpr.ShowScript -> "Vernacexpr.ShowScript"
  | Vernacexpr.ShowExistentials -> "Vernacexpr.ShowExistentials"
  | Vernacexpr.ShowUniverses -> "Vernacexpr.ShowUniverses"
  | Vernacexpr.ShowProofNames -> "Vernacexpr.ShowProofNames"
  | Vernacexpr.ShowIntros b -> "Vernacexpr.ShowIntros " ^ (string_of_bool b)
  | Vernacexpr.ShowMatch r -> "Vernacexpr.ShowMatch " ^ (string_of_Libnames__reference r)


let string_of_Vernacexpr__printable = function
  | Vernacexpr.PrintTables -> "Vernacexpr.PrintTables"
  | Vernacexpr.PrintFullContext -> "Vernacexpr.PrintFullContext"
  | Vernacexpr.PrintSectionContext r -> "Vernacexpr.PrintSectionContext " ^ (string_of_Libnames__reference r)
  | Vernacexpr.PrintInspect i -> "Vernacexpr.PrintInspect " ^ (string_of_int i)
  | Vernacexpr.PrintGrammar s -> "Vernacexpr.PrintGrammar " ^ (string_of_string s)
  | Vernacexpr.PrintLoadPath dro -> "Vernacexpr.PrintLoadPath " ^ (string_of_option string_of_DirPath__t dro)
  | Vernacexpr.PrintModules -> "Vernacexpr.PrintModules"
  | Vernacexpr.PrintModule r -> "Vernacexpr.PrintModule " ^ (string_of_Libnames__reference r)
  | Vernacexpr.PrintModuleType r -> "Vernacexpr.PrintModuleType " ^ (string_of_Libnames__reference r)
  | Vernacexpr.PrintNamespace d -> "Vernacexpr.PrintNamespace " ^ (string_of_DirPath__t d)
  | Vernacexpr.PrintMLLoadPath -> "Vernacexpr.PrintMLLoadPath"
  | Vernacexpr.PrintMLModules -> "Vernacexpr.PrintMLModules"
  | Vernacexpr.PrintDebugGC -> "Vernacexpr.PrintDebugGC"
  | Vernacexpr.PrintName (r_obn, unlo) ->
      sprintf "Vernacexpr.PrintName (%s, %s)"
        (string_of_Misctypes__or_by_notation string_of_Libnames__reference r_obn)
          (string_of_option (string_of_list string_of_Names__lname) unlo)
  | Vernacexpr.PrintGraph -> "Vernacexpr.PrintGraph"
  | Vernacexpr.PrintClasses -> "Vernacexpr.PrintClasses"
  | Vernacexpr.PrintTypeClasses -> "Vernacexpr.PrintTypeClasses"
  | Vernacexpr.PrintInstances r_obn ->
      "Vernacexpr.PrintInstances " ^ (string_of_Misctypes__or_by_notation string_of_Libnames__reference r_obn)
  | Vernacexpr.PrintCoercions -> "Vernacexpr.PrintCoercions"
  | Vernacexpr.PrintCoercionPaths (cr1, cr2) ->
      sprintf "Vernacexpr.PrintCoercionPaths (%s, %s)" (string_of_Vernacexpr__class_rawexpr cr1)
        (string_of_Vernacexpr__class_rawexpr cr2)
  | Vernacexpr.PrintCanonicalConversions -> "Vernacexpr.PrintCanonicalConversions"
  | Vernacexpr.PrintUniverses (b, so) ->
      sprintf "Vernacexpr.PrintUniverses (%s, %s)" (string_of_bool b) (string_of_option string_of_string so)
  | Vernacexpr.PrintHint r_obn ->
      "Vernacexpr.PrintHint " ^ (string_of_Misctypes__or_by_notation string_of_Libnames__reference r_obn)
  | Vernacexpr.PrintHintGoal -> "Vernacexpr.PrintHintGoal"
  | Vernacexpr.PrintHintDbName s -> "Vernacexpr.PrintHintDbName " ^ (string_of_string s)
  | Vernacexpr.PrintHintDb -> "Vernacexpr.PrintHintDb"
  | Vernacexpr.PrintScopes -> "Vernacexpr.PrintScopes"
  | Vernacexpr.PrintScope s -> "Vernacexpr.PrintScope " ^ (string_of_string s)
  | Vernacexpr.PrintVisibility so -> "Vernacexpr.PrintVisibility " ^ (string_of_option string_of_string so)
  | Vernacexpr.PrintAbout (r_obn, unlo, gsto) ->
      sprintf "Vernacexpr.PrintAbout (%s, %s, %s)" (string_of_Misctypes__or_by_notation string_of_Libnames__reference r_obn)
        (string_of_option (string_of_list string_of_Names__lname) unlo) (string_of_option string_of_Vernacexpr__goal_selector gsto)

  | Vernacexpr.PrintImplicit r_obn ->
      "Vernacexpr.PrintImplicit " ^ (string_of_Misctypes__or_by_notation string_of_Libnames__reference r_obn)
  | Vernacexpr.PrintAssumptions (b1, b2, r_obn) ->
      sprintf "Vernacexpr.PrintAssumptions (%s, %s, %s)" (string_of_bool b1) (string_of_bool b2)
        (string_of_Misctypes__or_by_notation string_of_Libnames__reference r_obn)

  | Vernacexpr.PrintStrategy r_obno ->
      "Vernacexpr.PrintStrategy " ^ (string_of_option (string_of_Misctypes__or_by_notation string_of_Libnames__reference) r_obno)


let string_of_Vernacexpr__search_restriction = function
  | Vernacexpr.SearchInside rl -> "Vernacexpr.SearchInside " ^  (string_of_list string_of_Libnames__reference rl)
  | Vernacexpr.SearchOutside rl -> "Vernacexpr.SearchOutside " ^  (string_of_list string_of_Libnames__reference rl)


let string_of_Vernacexpr__vernac_expr = function

  | Vernacexpr.VernacLoad (vf, s) ->
      sprintf "Vernacexpr.VernacLoad (%s, %s)" (string_of_bool vf) (string_of_string s)

  (* Syntax *)
  | Vernacexpr.VernacSyntaxExtension (b, (l, sl)) ->
      sprintf "Vernacexpr.VernacSyntaxExtension (%s, (%s, %s))" (string_of_bool b)
        (string_of_Names__lstring l) (string_of_list string_of_Vernacexpr__syntax_modifier sl)

  | Vernacexpr.VernacOpenCloseScope (b, sn) ->
    sprintf "Vernacexpr.VernacOpenCloseScope (%s, %s)" (string_of_bool b) (string_of_string sn)

  | Vernacexpr.VernacDelimiters (sn, s) ->
    sprintf "Vernacexpr.VernacDelimiters (%s, %s)" (string_of_string sn) (string_of_option string_of_string s)

  | Vernacexpr.VernacBindScope (sn, cl) ->
    sprintf "Vernacexpr.VernacBindScope (%s, %s)" (string_of_string sn) (string_of_list string_of_Vernacexpr__class_rawexpr cl)

  | Vernacexpr.VernacInfix ((ls, sl), ce, sno) ->
    sprintf "Vernacexpr.VernacInfix ((%s, %s), %s, %s)" (string_of_Names__lstring ls)
      (string_of_list string_of_Vernacexpr__syntax_modifier sl)
        (string_of_Constrexpr__constr_expr ce) (string_of_option string_of_string sno)

  | Vernacexpr.VernacNotation (ce, (ls, sl), sno) ->
      sprintf "Vernacexpr.VernacNotation (%s, (%s, %s), %s)" (string_of_Constrexpr__constr_expr ce)
        (string_of_Names__lstring ls) (string_of_list string_of_Vernacexpr__syntax_modifier sl)
          (string_of_option string_of_string sno)

  | Vernacexpr.VernacNotationAddFormat (s1, s2, s3) ->
      sprintf "Vernacexpr.VernacNotationAddFormat (%s, %s, %s)" (string_of_string s1) (string_of_string s2) (s3)

  (* Gallina *)
  | Vernacexpr.VernacDefinition ((d, dok), nd, de) ->
      sprintf "Vernacexpr.VernacDefinition ((%s, %s), %s, %s)" (string_of_Decl_kinds__discharge d)
        (string_of_Decl_kinds__definition_object_kind dok) (string_of_Constrexpr__name_decl nd)
          (string_of_Vernacexpr__definition_expr de)

  | Vernacexpr.VernacStartTheoremProof (tk, pel) ->
      sprintf "Vernacexpr.VernacStartTheoremProof (%s, %s)" (string_of_Decl_kinds__theorem_kind tk)
        (string_of_list string_of_Vernacexpr__proof_expr pel)

  | Vernacexpr.VernacEndProof pe ->
      "Vernacexpr.VernacEndProof " ^ (string_of_Vernacexpr__proof_end pe)

  | Vernacexpr.VernacExactProof ce ->
      "Vernacexpr.VernacExactProof " ^ (string_of_Constrexpr__constr_expr ce)

  | Vernacexpr.VernacAssumption ((d, aok), inl, wcl) ->
      sprintf "Vernacexpr.VernacAssumption ((%s, %s), %s, %s)" (string_of_Decl_kinds__discharge d)
        (string_of_Decl_kinds__assumption_object_kind aok) (string_of_Vernacexpr__inline inl)
          (string_of_list (string_of_Vernacexpr__with_coercion (fun (idl, ce) ->
            sprintf "(%s, %s)" (string_of_list string_of_Constrexpr__ident_decl idl) (string_of_Constrexpr__constr_expr ce))) wcl)

  | Vernacexpr.VernacInductive (cf, pf, f, l) ->
      sprintf "Vernacexpr.VernacInductive (%s, %s, %s, %s)" (string_of_Vernacexpr__cumulative_inductive_parsing_flag cf)
        (string_of_bool pf) (string_of_Declarations__recursivity_kind f)
          (string_of_list (fun (ie, dnl) -> sprintf "(%s, %s)"
            (string_of_Vernacexpr__inductive_expr ie)
              (string_of_list string_of_Vernacexpr__decl_notation dnl)) l)

  | Vernacexpr.VernacFixpoint (dis, fdll) ->
      sprintf "Vernacexpr.VernacFixpoint (%s, %s)" (string_of_Decl_kinds__discharge dis)
        (string_of_list (fun (fe, dnl) -> sprintf "(%s, %s)" (string_of_Vernacexpr__fixpoint_expr fe)
          (string_of_list string_of_Vernacexpr__decl_notation dnl)) fdll)

  | Vernacexpr.VernacCoFixpoint (dis, cdll) ->
      sprintf "Vernacexpr.VernacCoFixpoint (%s, %s)" (string_of_Decl_kinds__discharge dis)
        (string_of_list (fun (coe, dnl) -> sprintf "(%s, %s)" (string_of_Vernacexpr__cofixpoint_expr coe)
          (string_of_list string_of_Vernacexpr__decl_notation dnl)) cdll)

  | Vernacexpr.VernacScheme losl ->
      "Vernacexpr.VernacScheme " ^ (string_of_list (fun (lo, s) -> sprintf "(%s, %s)"
        (string_of_option string_of_Names__lident lo) (string_of_Vernacexpr__scheme s)) losl)

  | Vernacexpr.VernacCombinedScheme (lid, lidl) ->
      sprintf "Vernacexpr.VernacCombinedScheme (%s, %s)" (string_of_Names__lident lid)
        (string_of_list string_of_Names__lident lidl)

  | Vernacexpr.VernacUniverse lidl -> "Vernacexpr.VernacUniverse " ^ (string_of_list string_of_Names__lident lidl)
  | Vernacexpr.VernacConstraint gcl ->
      "Vernacexpr.VernacConstraint " ^ (string_of_list string_of_Glob_term__glob_constraint gcl)

  (* Gallina extensions *)
  | Vernacexpr.VernacBeginSection lid -> "Vernacexpr.VernacBeginSection " ^ (string_of_Names__lident lid)
  | Vernacexpr.VernacEndSegment lid -> "Vernacexpr.VernacEndSegment " ^ (string_of_Names__lident lid)
  | Vernacexpr.VernacRequire (ro, efo, rl) ->
      sprintf "Vernacexpr.VernacRequire (%s, %s, %s)" (string_of_option string_of_Libnames__reference ro)
        (string_of_option string_of_bool efo) (string_of_list string_of_Libnames__reference rl)

  | Vernacexpr.VernacImport (ef, rl) ->
      sprintf "Vernacexpr.VernacImport (%s, %s)" (string_of_bool ef)
        (string_of_list string_of_Libnames__reference rl)

  | Vernacexpr.VernacCanonical r_obn ->
      "Vernacexpr.VernacCanonical " ^ (string_of_Misctypes__or_by_notation string_of_Libnames__reference r_obn)
  | Vernacexpr.VernacCoercion (r_obn, cr1, cr2) ->
      sprintf "Vernacexpr.VernacCoercion (%s, %s, %s)"
        (string_of_Misctypes__or_by_notation string_of_Libnames__reference r_obn)
          (string_of_Vernacexpr__class_rawexpr cr1)  (string_of_Vernacexpr__class_rawexpr cr2)

  | Vernacexpr.VernacIdentityCoercion (lid, cr1, cr2) ->
      sprintf "Vernacexpr.VernacIdentityCoercion (%s, %s, %s)" (string_of_Names__lident lid)
        (string_of_Vernacexpr__class_rawexpr cr1)  (string_of_Vernacexpr__class_rawexpr cr2)

  | Vernacexpr.VernacNameSectionHypSet (lid, sse) ->
      sprintf "Vernacexpr.VernacNameSectionHypSet (%s, %s)" (string_of_Names__lident lid)
        (string_of_Vernacexpr__section_subset_expr sse)

    (* Type classes *)
  | Vernacexpr.VernacInstance (b, lbel, tc, bco, hie) ->
      sprintf "Vernacexpr.VernacInstance (%s, %s, %s, %s, %s)" (string_of_bool b)
        (string_of_list string_of_Constrexpr__local_binder_expr lbel) (string_of_Vernacexpr__typeclass_constraint tc)
          (string_of_option (fun (b, ce) -> sprintf "(%s, %s)" (string_of_bool b)
            ((string_of_Constrexpr__constr_expr ce))) bco) (string_of_Vernacexpr__hint_info_expr hie)

  | Vernacexpr.VernacContext lbel ->
      "Vernacexpr.VernacContext " ^ (string_of_list string_of_Constrexpr__local_binder_expr lbel)
  | Vernacexpr.VernacDeclareInstances rhl ->
      "Vernacexpr.VernacDeclareInstances " ^ (string_of_list (fun (r, hie) ->
        sprintf "(%s, %s)" (string_of_Libnames__reference r) (string_of_Vernacexpr__hint_info_expr hie)) rhl)

  | Vernacexpr.VernacDeclareClass r ->
      "Vernacexpr.VernacDeclareClass " ^ (string_of_Libnames__reference r)

  (* Modules and Module Types *)
  | Vernacexpr.VernacDeclareModule (bo, lid, mbl, mai) ->
      sprintf "Vernacexpr.VernacDeclareModule (%s, %s, %s, %s)" (string_of_option string_of_bool bo)
        (string_of_Names__lident lid) (string_of_list string_of_Vernacexpr__module_binder mbl)
          (string_of_Vernacexpr__module_ast_inl mai)

  | Vernacexpr.VernacDefineModule (bo, lid, mbl, ms, mail) ->
      sprintf "Vernacexpr.VernacDefineModule (%s, %s, %s, %s, %s)" (string_of_option string_of_bool bo)
        (string_of_Names__lident lid) (string_of_list string_of_Vernacexpr__module_binder mbl)
          (string_of_Vernacexpr__module_signature string_of_Vernacexpr__module_ast_inl ms)
            (string_of_list string_of_Vernacexpr__module_ast_inl mail)

  | Vernacexpr.VernacDeclareModuleType (lid, mbl, mail1, mail2) ->
      sprintf "Vernacexpr.VernacDeclareModuleType (%s, %s, %s, %s)" (string_of_Names__lident lid)
        (string_of_list string_of_Vernacexpr__module_binder mbl) (string_of_list string_of_Vernacexpr__module_ast_inl mail1)
          (string_of_list string_of_Vernacexpr__module_ast_inl mail2)

  | Vernacexpr.VernacInclude mail -> "Vernacexpr.VernacInclude " ^ (string_of_list string_of_Vernacexpr__module_ast_inl mail)

  (* Solving *)

  | Vernacexpr.VernacSolveExistential (i, ce) ->
      sprintf "Vernacexpr.VernacSolveExistential (%d, %s)" i (string_of_Constrexpr__constr_expr ce)

  (* Auxiliary file and library management *)
  | Vernacexpr.VernacAddLoadPath (rc, s, dto) ->
      sprintf "Vernacexpr.VernacAddLoadPath (%s, %s, %s)" (string_of_bool rc) (string_of_string s)
        (string_of_option string_of_DirPath__t dto)

  | Vernacexpr.VernacRemoveLoadPath s -> "Vernacexpr.VernacRemoveLoadPath " ^ (string_of_string s)
  | Vernacexpr.VernacAddMLPath (rf, s) ->
      sprintf "Vernacexpr.VernacAddMLPath (%s, %s)" (string_of_bool rf) (string_of_string s)
  | Vernacexpr.VernacDeclareMLModule sl ->
      "Vernacexpr.VernacDeclareMLModule " ^ (string_of_list string_of_string sl)
  | Vernacexpr.VernacChdir so ->
      "Vernacexpr.VernacChdir " ^ (string_of_option string_of_string so)

  (* State management *)
  | Vernacexpr.VernacWriteState s -> "Vernacexpr.VernacWriteState " ^ (string_of_string s)
  | Vernacexpr.VernacRestoreState s -> "Vernacexpr.VernacRestoreState " ^ (string_of_string s)

  (* Resetting *)
  | Vernacexpr.VernacResetName lid ->
      "Vernacexpr.VernacResetName " ^ (string_of_Names__lident lid)
  | Vernacexpr.VernacResetInitial -> "Vernacexpr.VernacResetInitial"
  | Vernacexpr.VernacBack i -> "Vernacexpr.VernacBack " ^ (string_of_int i)
  | Vernacexpr.VernacBackTo i -> "Vernacexpr.VernacBackTo " ^ (string_of_int i)

  (* Commands *)
  | Vernacexpr.VernacCreateHintDb (s, b) ->
      sprintf "Vernacexpr.VernacCreateHintDb (%s, %s)" (string_of_string s) (string_of_bool b)
  | Vernacexpr.VernacRemoveHints (sl, rl) ->
      sprintf "Vernacexpr.VernacRemoveHints (%s, %s)"
        (string_of_list string_of_string sl) (string_of_list string_of_Libnames__reference rl)

  | Vernacexpr.VernacHints (sl, he) ->
      sprintf "Vernacexpr.VernacHints (%s, %s)" (string_of_list string_of_string sl)
        (string_of_Vernacexpr__hints_expr he)

  | Vernacexpr.VernacDeclareImplicits (r_obn, ll) ->
      sprintf "Vernacexpr.VernacDeclareImplicits (%s, %s)" (string_of_Misctypes__or_by_notation string_of_Libnames__reference r_obn)
       (string_of_list (string_of_list (fun (e, b1, b2) ->
        sprintf "(%s, %s, %s)" (string_of_Constrexpr__explicitation e) (string_of_bool b1) (string_of_bool b2)
       )) ll)

  | Vernacexpr.VernacSyntacticDefinition (lid, (idl, ce), f) ->
      sprintf "Vernacexpr.VernacSyntacticDefinition (%s, (%s, %s), %s)" (string_of_Names__lident lid)
        (string_of_list string_of_string idl) (string_of_Constrexpr__constr_expr ce)
          (string_of_option string_of_Flags__compat_version f)

  | Vernacexpr.VernacArguments (r_obn, vas, tvll, io, l) ->
      sprintf "Vernacexpr.VernacArguments (%s, %s, %s, %s, %s)" (string_of_Misctypes__or_by_notation string_of_Libnames__reference r_obn)
        (string_of_list string_of_Vernacexpr__vernac_argument_status vas) (string_of_list (string_of_list (fun (nt, vis) -> sprintf "(%s, %s)"
          (string_of_Names__Name__t nt) (string_of_Vernacexpr__vernac_implicit_status vis))) tvll) (string_of_option string_of_int io)
            (string_of_list (fun e -> match e with
            | `ReductionDontExposeCase -> "`ReductionDontExposeCase"
            | `ReductionNeverUnfold    -> "`ReductionNeverUnfold"
            | `Rename                  -> "`Rename"
            | `ExtraScopes             -> "`ExtraScopes"
            | `Assert                  -> "`Assert"
            | `ClearImplicits          -> "`ClearImplicits"
            | `ClearScopes             -> "`ClearScopes"
            | `DefaultImplicits        -> "`DefaultImplicits"
            ) l)

  | Vernacexpr.VernacArgumentsScope (r_obn, scol) ->
      sprintf "VernacArgumentsScope (%s, %s)" (string_of_Misctypes__or_by_notation string_of_Libnames__reference r_obn)
       (string_of_list (string_of_option (fun s -> s)) scol)

  | Vernacexpr.VernacReserve sbl ->
      "Vernacexpr.VernacReserve " ^ (string_of_list string_of_Vernacexpr__simple_binder sbl)
  | Vernacexpr.VernacGeneralizable llo ->
      "Vernacexpr.VernacGeneralizable " ^ (string_of_option
        (string_of_list string_of_Names__lident) llo)

  | Vernacexpr.VernacSetOpacity (le, r_obnl) ->
      sprintf "Vernacexpr.VernacSetOpacity (%s, %s)" (string_of_Conv_oracle__level le)
        (string_of_list (string_of_Misctypes__or_by_notation string_of_Libnames__reference) r_obnl)

  | Vernacexpr.VernacSetStrategy l ->
      "Vernacexpr.VernacSetStrategy " ^ (string_of_list (fun (le, r_obnl) ->
        sprintf "(%s, %s)" (string_of_Conv_oracle__level le)
          (string_of_list (string_of_Misctypes__or_by_notation string_of_Libnames__reference) r_obnl)) l)

  | Vernacexpr.VernacUnsetOption (ef, on) ->
      sprintf "Vernacexpr.VernacUnsetOption (%s, %s)" (string_of_bool ef) (string_of_list string_of_string on)
  | Vernacexpr.VernacSetOption (ef, on, ov) ->
      sprintf "Vernacexpr.VernacSetOption (%s, %s, %s)" (string_of_bool ef)
        (string_of_list string_of_string on) (string_of_Goptions__option_value ov)

  | Vernacexpr.VernacAddOption (on, orvl) ->
      sprintf "Vernacexpr.VernacAddOption (%s, %s)" (string_of_list string_of_string on)
        (string_of_list string_of_Vernacexpr__option_ref_value orvl)

  | Vernacexpr.VernacRemoveOption (on, orvl) ->
      sprintf "Vernacexpr.VernacRemoveOption (%s, %s)" (string_of_list string_of_string on)
        (string_of_list string_of_Vernacexpr__option_ref_value orvl)

  | Vernacexpr.VernacMemOption (on, orvl) ->
      sprintf "Vernacexpr.VernacMemOption (%s, %s)" (string_of_list string_of_string on)
        (string_of_list string_of_Vernacexpr__option_ref_value orvl)

  | Vernacexpr.VernacPrintOption on -> "Vernacexpr.VernacPrintOption " ^ (string_of_list string_of_string on)
  | Vernacexpr.VernacCheckMayEval (rreo, gsto, ce) ->
      sprintf "Vernacexpr.VernacCheckMayEval (%s, %s, %s)" (string_of_option string_of_Genredexpr__raw_red_expr rreo)
        (string_of_option string_of_Vernacexpr__goal_selector gsto) (string_of_Constrexpr__constr_expr ce)

  | Vernacexpr.VernacGlobalCheck ce ->
      "Vernacexpr.VernacGlobalCheck " ^ (string_of_Constrexpr__constr_expr ce)
  | Vernacexpr.VernacDeclareReduction (s, rre) ->
      sprintf "Vernacexpr.VernacDeclareReduction (%s, %s)" (string_of_string s) (string_of_Genredexpr__raw_red_expr rre)
  | Vernacexpr.VernacPrint p ->
      "Vernacexpr.VernacPrint " ^ (string_of_Vernacexpr__printable p)
  | Vernacexpr.VernacSearch (sb, gsto, sr) ->
      sprintf "Vernacexpr.VernacSearch (%s, %s, %s)" (string_of_Vernacexpr__searchable sb)
        (string_of_option string_of_Vernacexpr__goal_selector gsto) (string_of_Vernacexpr__search_restriction sr)

  | Vernacexpr.VernacLocate loc -> "Vernacexpr.VernacLocate " ^ (string_of_Vernacexpr__locatable loc)
  | Vernacexpr.VernacRegister (lid, rk) ->
      sprintf "Vernacexpr.VernacRegister (%s, %s)" (string_of_Names__lident lid)
        (string_of_Vernacexpr__register_kind rk)

  | Vernacexpr.VernacComments cl ->
      "Vernacexpr.VernacComments " ^ (string_of_list string_of_Vernacexpr__comment cl)

  (* Proof management *)
  | Vernacexpr.VernacAbort lid -> "Vernacexpr.VernacAbort " ^ (string_of_option string_of_Names__lident lid)
  | Vernacexpr.VernacAbortAll -> "Vernacexpr.VernacAbortAll"
  | Vernacexpr.VernacRestart -> "Vernacexpr.VernacRestart"
  | Vernacexpr.VernacUndo i -> "Vernacexpr.VernacUndo " ^ (string_of_int i)
  | Vernacexpr.VernacUndoTo i -> "Vernacexpr.VernacUndoTo " ^ (string_of_int i)
  | Vernacexpr.VernacBacktrack (i1, i2, i3) -> sprintf "Vernacexpr.VernacBacktrack (%d, %d, %d)" i1 i2 i3
  | Vernacexpr.VernacFocus io -> "Vernacexpr.VernacFocus " ^ (string_of_option string_of_int io)
  | Vernacexpr.VernacUnfocus -> "Vernacexpr.VernacUnfocus"
  | Vernacexpr.VernacUnfocused -> "Vernacexpr.VernacUnfocused"
  | Vernacexpr.VernacBullet pbt -> "Vernacexpr.VernacBullet " ^ (stirng_of_Vernacexpr__bullet pbt)
  | Vernacexpr.VernacSubproof gsto ->
      "Vernacexpr.VernacSubproof " ^ (string_of_option string_of_Vernacexpr__goal_selector gsto)

  | Vernacexpr.VernacShow sh -> "Vernacexpr.VernacShow " ^ (string_of_Vernacexpr__showable sh)
  | Vernacexpr.VernacEndSubproof -> "Vernacexpr.VernacEndSubproof"
  | Vernacexpr.VernacCheckGuard -> "Vernacexpr.VernacCheckGuard"
  | Vernacexpr.VernacProof (rgao, sseo) ->
     sprintf "Vernacexpr.VernacProof (%s, %s)" (string_of_option string_of_Genarg__raw_generic_argument rgao)
      (string_of_option string_of_Vernacexpr__section_subset_expr sseo)

  | Vernacexpr.VernacProofMode s -> "Vernacexpr.VernacProofMode " ^ (string_of_string s)

  (* Toplevel control *)
  | Vernacexpr.VernacToplevelControl ex -> "Vernacexpr.VernacToplevelControl " ^ (Printexc.to_string ex)

  (* For extension *)
  | Vernacexpr.VernacExtend (en, rgal) ->
      sprintf "Vernacexpr.VernacExtend (%s, %s)" (string_of_Vernacexpr__extend_name en)
        (string_of_list string_of_Genarg__raw_generic_argument rgal)


let rec string_of_Vernacexpr__vernac_control = function
  | Vernacexpr.VernacExpr (f, v') ->
      sprintf "Vernacexpr.VernacExpr (%s, %s)" (string_of_list string_of_Vernacexpr__vernac_flag f) (string_of_Vernacexpr__vernac_expr v')
  | Vernacexpr.VernacTime (time, {CAst.v}) ->
      sprintf "Vernacexpr.VernacTime (%s, %s)" (string_of_bool time) (string_of_Vernacexpr__vernac_control v)
  | Vernacexpr.VernacRedirect (s, {CAst.v}) ->
      sprintf "Vernacexpr.VernacRedirect (%s, %s)" (string_of_string s) (string_of_Vernacexpr__vernac_control v)
  | Vernacexpr.VernacTimeout(n, v) ->
      sprintf "Vernacexpr.VernacTimeout (%s, %s)" (string_of_int n) (string_of_Vernacexpr__vernac_control v)
  | Vernacexpr.VernacFail v ->
      "Vernacexpr.VernacFail " ^ (string_of_Vernacexpr__vernac_control v)
*)

let vernac_expr_type = function
  | Vernacexpr.VernacLoad _ -> "VernacLoad"
  | Vernacexpr.VernacSyntaxExtension _ -> "VernacSyntaxExtension"
  | Vernacexpr.VernacOpenCloseScope _ -> "VernacOpenCloseScope"
  | Vernacexpr.VernacDelimiters _ -> "VernacDelimiters"
  | Vernacexpr.VernacBindScope _ -> "VernacBindScope"
  | Vernacexpr.VernacInfix _  -> "VernacInfix"
  | Vernacexpr.VernacNotation _ -> "VernacNotation"
  | Vernacexpr.VernacNotationAddFormat _ -> "VernacNotationAddFormat"
  | Vernacexpr.VernacDeclareCustomEntry _ -> "VernacDeclareCustomEntry"
  | Vernacexpr.VernacDefinition _ -> "VernacDefinition"
  | Vernacexpr.VernacStartTheoremProof _ -> "VernacStartTheoremProof"
  | Vernacexpr.VernacEndProof _ -> "VernacEndProof"
  | Vernacexpr.VernacExactProof _ -> "VernacExactProof"
  | Vernacexpr.VernacAssumption _ -> "VernacAssumption"
  | Vernacexpr.VernacInductive _ -> "VernacInductive"
  | Vernacexpr.VernacFixpoint _ -> "VernacFixpoint"
  | Vernacexpr.VernacCoFixpoint _ -> "VernacCoFixpoint"
  | Vernacexpr.VernacScheme _ -> "VernacScheme"
  | Vernacexpr.VernacCombinedScheme _ -> "VernacCombinedScheme"
  | Vernacexpr.VernacUniverse _ -> "VernacUniverse"
  | Vernacexpr.VernacConstraint _ -> "VernacConstraint"
  | Vernacexpr.VernacBeginSection _ -> "VernacBeginSection"
  | Vernacexpr.VernacEndSegment _ -> "VernacEndSegment"
  | Vernacexpr.VernacRequire _ -> "VernacRequire"
  | Vernacexpr.VernacImport _ -> "VernacImport"
  | Vernacexpr.VernacCanonical _ -> "VernacCanonical"
  | Vernacexpr.VernacCoercion _ -> "VernacCoercion"
  | Vernacexpr.VernacIdentityCoercion _ -> "VernacIdentityCoercion"
  | Vernacexpr.VernacNameSectionHypSet _ -> "VernacNameSectionHypSet"
  | Vernacexpr.VernacInstance _ -> "VernacInstance"
  | Vernacexpr.VernacContext _ -> "VernacContext"
  | Vernacexpr.VernacDeclareInstances _ -> "VernacDeclareInstances"
  | Vernacexpr.VernacDeclareClass _ -> "VernacDeclareClass"
  | Vernacexpr.VernacDeclareModule _ -> "VernacDeclareModule"
  | Vernacexpr.VernacDefineModule _ -> "VernacDefineModule"
  | Vernacexpr.VernacDeclareModuleType _ -> "VernacDeclareModuleType"
  | Vernacexpr.VernacInclude _ -> "VernacInclude"
  | Vernacexpr.VernacSolveExistential _ -> "VernacSolveExistential"
  | Vernacexpr.VernacAddLoadPath _ -> "VernacAddLoadPath"
  | Vernacexpr.VernacRemoveLoadPath _ -> "VernacRemoveLoadPath"
  | Vernacexpr.VernacAddMLPath _ -> "VernacAddMLPath"
  | Vernacexpr.VernacDeclareMLModule _ -> "VernacDeclareMLModule"
  | Vernacexpr.VernacChdir _ -> "VernacChdir"
  | Vernacexpr.VernacWriteState _ -> "VernacWriteState"
  | Vernacexpr.VernacRestoreState _ -> "VernacRestoreState"
  | Vernacexpr.VernacResetName _ -> "VernacResetName"
  | Vernacexpr.VernacResetInitial -> "VernacResetInitial"
  | Vernacexpr.VernacBack i -> "VernacBack"
  | Vernacexpr.VernacBackTo i -> "VernacBackTo"
  | Vernacexpr.VernacCreateHintDb _ -> "VernacCreateHintDb"
  | Vernacexpr.VernacRemoveHints _ -> "VernacRemoveHints"
  | Vernacexpr.VernacHints _ -> "VernacHints"
  | Vernacexpr.VernacSyntacticDefinition _ -> "VernacSyntacticDefinition"
  | Vernacexpr.VernacArguments _ -> "VernacArguments"
  | Vernacexpr.VernacReserve _ -> "VernacReserve"
  | Vernacexpr.VernacGeneralizable _ -> "VernacGeneralizable"
  | Vernacexpr.VernacSetOpacity _ -> "VernacSetOpacity"
  | Vernacexpr.VernacSetStrategy _ -> "VernacSetStrategy"
  | Vernacexpr.VernacUnsetOption _ -> "VernacUnsetOption"
  | Vernacexpr.VernacSetOption _ -> "VernacSetOption"
  | Vernacexpr.VernacAddOption _ -> "VernacAddOption"
  | Vernacexpr.VernacRemoveOption _ -> "VernacRemoveOption"
  | Vernacexpr.VernacMemOption _ -> "VernacMemOption"
  | Vernacexpr.VernacPrintOption _ -> "VernacPrintOption"
  | Vernacexpr.VernacCheckMayEval _ -> "VernacCheckMayEval"
  | Vernacexpr.VernacGlobalCheck _ -> "VernacGlobalCheck"
  | Vernacexpr.VernacDeclareReduction _ -> "VernacDeclareReduction"
  | Vernacexpr.VernacPrint _ -> "VernacPrint"
  | Vernacexpr.VernacSearch _ -> "VernacSearch"
  | Vernacexpr.VernacLocate _ -> "VernacLocate"
  | Vernacexpr.VernacRegister _ -> "VernacRegister"
  | Vernacexpr.VernacComments _ -> "VernacComments"
  | Vernacexpr.VernacAbort _ -> "VernacAbort"
  | Vernacexpr.VernacAbortAll -> "VernacAbortAll"
  | Vernacexpr.VernacRestart -> "VernacRestart"
  | Vernacexpr.VernacUndo _ -> "VernacUndo"
  | Vernacexpr.VernacUndoTo _ -> "VernacUndoTo"
  | Vernacexpr.VernacFocus _ -> "VernacFocus"
  | Vernacexpr.VernacUnfocus -> "VernacUnfocus"
  | Vernacexpr.VernacUnfocused -> "VernacUnfocused"
  | Vernacexpr.VernacBullet _ -> "VernacBullet"
  | Vernacexpr.VernacSubproof _ -> "VernacSubproof"
  | Vernacexpr.VernacShow _ -> "VernacShow"
  | Vernacexpr.VernacEndSubproof -> "VernacEndSubproof"
  | Vernacexpr.VernacCheckGuard -> "VernacCheckGuard"
  | Vernacexpr.VernacProof _ -> "VernacProof"
  | Vernacexpr.VernacProofMode _ -> "VernacProofMode"
  | Vernacexpr.VernacExtend _ -> "VernacExtend"
