open Assignment

let assignments =
  [Toi 2, [Type("AbstStack.pop", "'a AbstStack.t -> 'a * 'a AbstStack.t");
           Type("AbstStack.push", "'a -> 'a AbstStack.t -> 'a AbstStack.t");
           Type("AbstStack.empty", "'a AbstStack.t");
           Type("AbstStack.size", "'a AbstStack.t -> int");
           Value("AbstStack.empty", "<abstr>")];
   Toi 3, [Value("let module M = AbstMultiset2(struct type t = int let compare _ _ = EQ end) in M.count 0 (M.remove 0 (M.add 0 M.empty))", "0")];
   Toi 4, [ModDef "MakeMap(struct type t = int let compare _ _ = EQ end)"];
   Toi 5, [ModDef "Matrix(struct type t = int let add (x:t) (y:t) = x let mul = add let unit = 0 let zero = 0 end)";
           ModDef "BoolMatrix";
           ModDef "TropMatrix"];
   Hatten 1, [Module("Eq", "EQ");
              Type("eval", "'a expr -> 'a value")];
   Hatten 2, [Module("Eq2", "EQ2")]]
