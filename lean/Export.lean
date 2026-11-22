/- Prints the Lean-defined relation lists (run: lake env lean --run Export.lean <outdir>). -/
import Trs.Lists
open Trs

def writeList (path : String) (l : List String) : IO Unit :=
  IO.FS.writeFile path (String.intercalate "\n" l ++ "\n")

def main (args : List String) : IO Unit := do
  let d := args.headD "."
  for n in [2, 3, 4] do
    writeList s!"{d}/bool_zero_n{n}.txt" (Bool.zeroTerms n 3)
    writeList s!"{d}/bool_input_n{n}.txt" (Bool.inputTerms n 3)
  writeList s!"{d}/box_zero_s1.txt" (Box.mzeroTerms 1)
  writeList s!"{d}/box_atom_s1.txt" (Box.matomTerms 1)
  writeList s!"{d}/box_atomeq.txt" Box.matomEqs
  IO.println s!"box size<=2: terms {(Box.mtermsUpTo 2).length} zero {(Box.mzeroTerms 2).length} atom {(Box.matomTerms 2).length}"
