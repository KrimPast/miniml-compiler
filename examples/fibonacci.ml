open Main

let start = 
Function("_start", "a0",
  Sequence(LetI("a0", "x0", Add, 0),
    Sequence(Call "fibonacci",
      SingleNested(EndOfProgram)
    )
  )
);;

(* Returns a n-th number of Fibonacci starting from zero *)
let program = 
Function("fibonacci", "a0",
  Sequence(LetI("t1", "x0", Add, 0),
    Sequence(LetI("t2", "x0", Add, 1),
      While(
        Condition("a0", LESS_EQUAL, 0),
        Sequence(                         (* cycle body *)
          LetI("a0", "a0", Subtract, 1),
          Sequence(LetR("t3", "t2", Add, "t1"),
            Sequence(LetR("t1", "t2", Add, "x0"),
              SingleNested(LetR("t2", "t3", Add, "x0"))
            )
          )
        ),
        Sequence(                         (* after cycle *)
          LetR("a0", "t1", Add, "x0"),
          SingleNested(Return("a0"))
        )
      )
    )
  )
);;

print_string (parse_program start_context start ^ parse_program start_context program);;