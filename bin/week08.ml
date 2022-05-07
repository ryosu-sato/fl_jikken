open Assignment

let ty =  TypeDef(0, "ty")
let subst = TypeDef(0, "subst")
let ty_subst = Type("ty_subst", "subst -> ty -> ty")
let compose = Type("compose", "subst -> subst -> subst")
let unify = Type("unify", "(ty * ty) list -> subst")

let assignments =
  [Toi 2, [ty; subst; ty_subst];
   Toi 3, [ty; subst; compose];
   Toi 4, [ty; subst; unify]]
