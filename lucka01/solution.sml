fun readInts (infile : string) = let
  val ins = TextIO.openIn infile

  fun loop ins =
    case TextIO.scanStream( Int.scan StringCvt.DEC) ins of
      SOME int => int :: loop ins
    | NONE => []

  in
    loop ins before TextIO.closeIn ins
  end;

fun numBigger (x1::x2::xs) = (if x2 > x1 then 1 else 0) + numBigger (x2::xs)
  | numBigger _ = 0;

fun mergeThree (x1::x2::x3::xs) = (x1 + x2 + x3)::(mergeThree (x2::x3::xs))
  | mergeThree _ = [];
