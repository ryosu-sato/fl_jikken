open Assignment

let may_be_included =
  ["constraintSolver.cmi";
   "constraintSolver.cmo";
   "tySyntax.cmi";
   "tySyntax.cmo"]

let assignments =
  [ToiDir 1, [Build(None, may_be_included)]]
