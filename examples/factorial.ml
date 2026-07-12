open Main

let start = 
Function("_start", "a0",
  Sequence(LetI("a0", "x0", Add, 5),
    Sequence(Call "factorial",
      SingleNested(EndOfProgram)
    )
  )
)

let program = 
Function("factorial", "a0",
  If(Condition("a0", LESS_EQUAL, 1),
    Sequence(LetI("a0", "x0", Add, 1), (* then *)
             SingleNested(Return "a0")
    ),
    Sequence(LetI("a1", "a0", Add, -1),(* else *)
      Sequence(Putarg "a1",
        Sequence(Call "factorial",
          Sequence(LetI("a1", "a1", Add, 1),
            Sequence(
              LetR("a0", "a0", Multiply, "a1"),
              SingleNested(Return "a0")
            )
          )
        )
      )
    )
  )
);;

print_string (parse_program start_context start ^ parse_program start_context program);;