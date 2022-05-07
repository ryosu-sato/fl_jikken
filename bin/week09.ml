open Assignment

let build = Build(None, [])

let assignments =
  [ToiDir 1, [build];
   ToiDir 2, [build];
   ToiDir 3, [build];
   ToiDir 4, [build];
   HattenDir 1, [build];
   HattenDir 2, [build]]
