  $ compile () { ../src/main.exe $1; }

*** Lex exceptions ***
  $ echo "if 1 . 1 then 1" > input
  $ compile input
  Fatal error: exception Tokens.LexError("Undefined symbol: '.'")
  [2]

*** Gen expections ***
  $ echo "if 1 is 1 then 1" > input
  $ compile input
  Fatal error: exception Gn.Gen.GenError("Expected one of '<=', '<', '>', '>=' in condition.")
  [2]

*** Gen expections ***
  $ echo "let f x = f x - 1" > input
  $ compile input
  Fatal error: exception Gn.Gen.GenError("Function `f` is not found")
  [2]
