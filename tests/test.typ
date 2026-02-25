#import "/src/lib.typ" as exercism

#import "@preview/layout-ltd:0.1.0": layout-limiter
#show: layout-limiter.with(max-iterations: 3)

#show ref: exercism.show-ref
#show ref: set text(green)
#show link: set text(blue)

#let exercise = exercism.new(
  "exercise",
  supplement: [Exercise],
)

#exercise[Fermat's Last Theorem][
  Prove that $x^n + y^n = z^n$, where $n >= 3$, has no non-trivial solutions $x, y, z in ZZ$.
][
  The truly marvelous proof of this is unable to be contained within this small document.
] <fermat>

See @fermat.

#context exercism.questions("exercise", (
  body,
  supplement,
  number,
  title,
  solution,
) => {
  let title = if title != none [(#title)] else []
  block[
    *#supplement #number.* #title
    #body
    //#ref(solution)
    #link(solution)[Solution]
  ]
})

#exercise[
  Do you love Typst?
][
  Yes!
]

#context exercism.solutions("exercise", (body, _, number, _, question) => {
  block[
    *Exercise #number.*
    #body
    #ref(question)
    #link(question)[Question]
  ]
})

@fermat
