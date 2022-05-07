open Assignment

let ty =  TypeDef(0, "ty")
let subst = TypeDef(0, "subst")
let ty_subst = Type("ty_subst", "subst -> ty -> ty")
let compose = Type("compose", "subst -> subst -> subst")
let unify = Type("unify", "(ty * ty) list -> subst")

let toi n = Toi(Dir, n)

let assignments =
  [toi 2, [ty; subst; ty_subst];
   toi 3, [ty; subst; compose];
   toi 4, [ty; subst; unify]]
