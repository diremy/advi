  type dimen =
    | Px of int
    | Pt of float
    | Pc of float
    | In of float
    | Bp of float
    | Cm of float
    | Mm of float
    | Dd of float
    | Cc of float
    | Sp of int

  val dimen_of_string : string -> dimen

  val normalize : dimen -> dimen
