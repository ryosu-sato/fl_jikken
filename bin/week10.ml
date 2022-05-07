open Assignment

let assignments =
  [Toi 1, [Predicate("bloodrelative", 2)];
   Toi 2, [Predicate("mult", 3)];
   Toi 3, [Predicate("reverse", 2);
           Query("reverse([1,2,3],X),display(X).", ["[3,2,1]"]);
           Predicate("concat", 2);
           Query("concat([[1],[2,3]],X),display(X).", ["[1,2,3]"])];
   Toi 4, [Predicate("hamilton", 2)]]
