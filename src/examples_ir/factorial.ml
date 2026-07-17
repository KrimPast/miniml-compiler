(* SPDX-License-Identifier: LGPL-3.0-only *)
(* Copyright Nikita Egorov and Maksim Butyugov *)

(* open LibML.Ir
open LibML.Gen

let start = 
Function("_start", "a0",
  Sequence(LetI("a0", "x0", Add, 5),
    Sequence(
      Call "factorial",
      EndOfProgram
    )
  )
)

(* Returns n! = n * (n-1) * ... * 2 * 1 *)
let program = 
Function("factorial", "a0",
  If(Condition("a0", LESS_EQUAL, 1),
    Sequence(LetI("a0", "x0", Add, 1), (* then *)
             Return "a0"
    ),
    Sequence(LetI("a1", "a0", Add, -1),(* else *)
      Sequence(Putarg "a1",
        Sequence(Call "factorial",
          Sequence(LetI("a1", "a1", Add, 1),
            Sequence(
              LetR("a0", "a0", Multiply, "a1"),
              Return "a0"
            )
          )
        )
      )
    )
  )
);;

print_string (parse_program start_context start ^ parse_program start_context program);; *)