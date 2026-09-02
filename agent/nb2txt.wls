(* nb2txt.wls -- transcribe a notebook by the AUTHORSHIP rule (Vasil,
   2026-08-26): everything human-typed goes in COMPLETELY (headings, Text
   cells, Input/Code cells with comments and line breaks verbatim);
   everything kernel-generated (Output/Print/Echo/Message) is skipped by
   default -- outputs are recreated by evaluation, not read.

   Usage:
     wl nb2txt.wls <file.nb>        -> <file>.txt   (default: typed cells only)
     wl nb2txt.wls <file.nb> io     -> <file>.io.txt (adds generated cells,
                                       long ones trimmed to Short skeletons;
                                       the flag is dashless because
                                       wolframscript eats "-io" itself)

   Input cells are converted through the front end's own box-to-text export
   (ExportPacket "InputText"), the same translation the FE performs on copy
   -- comments and \[IndentingNewLine]s survive, unlike a ToExpression-based
   parse (the unbox-io.wls defect this tool supersedes). Text/heading cells
   go through "PlainText". One hidden FE serves the whole run (UsingFrontEnd
   around the loop, not per cell).

   Fingers-rule edge: generated content PASTED into an Input cell. Any typed
   cell whose text exceeds $typedCap characters is truncated with a loud
   marker -- by the rule it cannot have been typed.                          *)

$typedCap = 4000;      (* chars; genuinely typed cells sit far below this *)
$leafCap  = 120;       (* LeafCount above which a generated cell is trimmed *)

nbfile = $ScriptCommandLine[[2]];
withIO = MemberQ[$ScriptCommandLine, "io"] || MemberQ[$ScriptCommandLine, "-io"];
outfile = StringReplace[nbfile, ".nb" ~~ EndOfString -> If[withIO, ".io.txt", ".txt"]];

nb = Get[nbfile];

typedStyles = "Title" | "Chapter" | "Section" | "Subsection" | "Subsubsection" |
  "Text" | "Item" | "ItemNumbered" | "ItemParagraph" | "Input" | "Code";
genStyles = "Output" | "Print" | "Echo" | "Message";

labelOf[Cell[___, CellLabel -> l_String, ___]] := " " <> l;
labelOf[_] := "";

feText[cell_Cell, fmt_String] :=
  Quiet@Check[First@FrontEndExecute@FrontEnd`ExportPacket[cell, fmt],
    "NB2TXT-FAILED: " <> StringTake[ToString[First[cell], InputForm], UpTo[500]]];

capped[s_String] := If[StringLength[s] <= $typedCap, s,
  StringTake[s, $typedCap] <>
    "\n(* NB2TXT: TRUNCATED at " <> ToString[$typedCap] <>
    " chars of " <> ToString[StringLength[s]] <>
    " -- generated content in a typed cell? *)"];

genText[cell : Cell[c_, ___]] := Module[{held},
  held = Quiet@Check[
    ToExpression[c /. BoxData[b_] :> b /. TextData[t_] :> t,
        StandardForm, HoldComplete] /. HoldComplete[x___] :> HoldForm[{x}],
    $Failed];
  Which[
    held === $Failed,
      "UNPARSED-BOXES (panel/graphics?), skipped",
    LeafCount[held] > $leafCap,
      ToString[Short[held, 4], OutputForm] <>
        "\n(* NB2TXT: trimmed, LeafCount " <> ToString[LeafCount[held]] <> " *)",
    True,
      ToString[held, InputForm, PageWidth -> Infinity]]];

lines = {};
emit[s_String] := AppendTo[lines, s];

UsingFrontEnd@Do[
  With[{style = cell[[2]]},
    Which[
      MatchQ[style, "Title" | "Chapter" | "Section" | "Subsection" | "Subsubsection"],
        emit["\n==== [" <> style <> "] " <>
          StringTrim@feText[cell, "PlainText"] <> " ===="],
      MatchQ[style, "Text" | "Item" | "ItemNumbered" | "ItemParagraph"],
        emit["<text>\n" <> capped@feText[cell, "PlainText"] <> "\n</text>"],
      MatchQ[style, "Input" | "Code"],
        emit["<input" <> labelOf[cell] <> ">\n" <>
          capped@feText[cell, "InputText"] <> "\n</input>"],
      withIO && MatchQ[style, genStyles],
        emit["<" <> ToLowerCase[style] <> labelOf[cell] <> ">\n" <>
          genText[cell] <> "\n</" <> ToLowerCase[style] <> ">"]]],
  {cell, Cases[nb, Cell[_, _String, ___], Infinity]}];

WriteString[outfile, StringRiffle[lines, "\n"] <> "\n"];
Print["nb2txt: ", Length[lines], " cells -> ", outfile];
