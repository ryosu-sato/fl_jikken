open Assignment

let assignments =
  [Toi 1, [Type("sum_to", "int -> int");
           Type("is_prime", "int -> bool");
           Type("gcd", "int -> int -> int")];
   Toi 2, [ValDef "twice";
           ValDef "repeat"];
   Toi 3, [Type("sum_to_fix", "int -> int");
           Type("is_prime_fix", "int -> bool");
           Type("gcd_fix", "int -> int -> int")];
   Toi 4, [ValDef "fold_left";
           ValDef "fold_right"];
   Toi 5, [Type("append", "'a list -> 'a list -> 'a list");
           Type("filter", "('a -> bool) -> 'a list -> 'a list")];
   Toi 6, [Type("append_left", "'a list -> 'a list -> 'a list");
           Type("filter_left", "('a -> bool) -> 'a list -> 'a list");
           Type("append_right", "'a list -> 'a list -> 'a list");
           Type("filter_right", "('a -> bool) -> 'a list -> 'a list")];
   Toi 7, [Type("perm", "'a list -> 'a list list")];
   Hatten 1, [ValDef "reverse";
              ValDef "reverse_right"];
   Hatten 2, [ValDef "fold_left_by_right";
              ValDef "fold_right_by_left"];
   Hatten 3, [ValDef "add";
              ValDef "mul";
              ValDef "sub"]]
