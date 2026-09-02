(* nb-author.wls -- author an inputs-only notebook from typed sources.
   EXPERIMENTAL (agent kit). The deliverable notebook contains ONLY what a
   human would have typed -- headings, Text cells, Input cells -- so the
   user can open it, evaluate it, and tweak it. Kernel-generated output is
   never written into a notebook; it is recreated by evaluation.

   Usage:
     wl nb-author.wls <source> <target>

   <source> is either
     - a transcript .txt in the kit's exchange format (what nb2txt.wls
       emits and what a reaped sxk session produces): "==== [Style] Title
       ====" headings, <input>...</input> cells, <text>...</text> cells; or
     - a sources .wl file defining nbSections = {{title, {cellString, ...}},
       ...} (each cellString one whole cell, newlines included) and
       optionally nbInit (a cell string).

   <target> is either
     - an ENTRY DIRECTORY (.../Data/<alias>/<Entry>): writes <Entry>.nb
       inside, PREPENDS the standard init cell (Needs + position-derived
       identity + LocateData[]), and asserts
       the entry's .m is untouched by the build (md5 before == after); or
     - a bare .nb path: writes exactly the sourced cells, NO init cell
       added (roundtrip mode).

   Mechanism: each cell's full text is parsed by the front end's own
   parser (whole-cell, line breaks preserved in the boxes -- verified
   2026-08-26), then ^, / and Sqrt are lifted to 2D boxes. One hidden FE
   serves the whole build. The build never loads SolutionsX.              *)

naOut[s_] := WriteString[Streams["stdout"][[1]], s <> "\n"];

naArgs = Rest[$ScriptCommandLine];
If[Length[naArgs] < 2, naOut["usage: nb-author.wls <source.txt|source.wl> <entry-dir|target.nb>"]; Exit[1]];
{naSrc, naDst} = naArgs[[1 ;; 2]];

naInitDefault = "Needs[\"VasilDimitrov`SolutionsX`\"]
If[StringQ[#]&&#=!=$Alias,SetAlias[#,Permanent->False];SetCurator[False,Permanent->False]]&@Quiet@FileNameTake@DirectoryName[NotebookFileName[],2]
LocateData[]";

(* ---- source -> {init|None, {{title|None, style, {cellSpec...}}...}}
   cellSpec: {"Input"|"Text", string} ---- *)

naParseTxt[naFile_] := Module[{naLines, naSecs = {}, naCur = {None, "Section", {}}, naI = 1, naN, naBody, naTitle},
  naLines = StringSplit[ReadString[naFile], "\n"];
  naN = Length[naLines];
  While[naI <= naN,
    Which[
      StringMatchQ[naLines[[naI]], "==== [" ~~ __ ~~ "===="],
        AppendTo[naSecs, naCur];
        naTitle = StringCases[naLines[[naI]],
          "==== [" ~~ naS : Except["]"] .. ~~ "] " ~~ naT__ ~~ " ====" :> {naS, naT}];
        naCur = If[naTitle === {}, {None, "Section", {}},
          {naTitle[[1, 2]], naTitle[[1, 1]], {}}];
        naI++,
      StringMatchQ[naLines[[naI]], "<input" ~~ ___ ~~ ">"],
        naBody = {}; naI++;
        While[naI <= naN && naLines[[naI]] =!= "</input>",
          AppendTo[naBody, naLines[[naI]]]; naI++];
        naI++;
        naCur[[3]] = Append[naCur[[3]], {"Input", StringRiffle[naBody, "\n"]}],
      StringMatchQ[naLines[[naI]], "<text>"],
        naBody = {}; naI++;
        While[naI <= naN && naLines[[naI]] =!= "</text>",
          AppendTo[naBody, naLines[[naI]]]; naI++];
        naI++;
        naCur[[3]] = Append[naCur[[3]], {"Text", StringRiffle[naBody, "\n"]}],
      True, naI++]];
  AppendTo[naSecs, naCur];
  DeleteCases[naSecs, {_, _, {}}]];

naParseWl[naFile_] := (Get[naFile];
  If[! ValueQ[nbSections], naOut["FATAL: " <> naFile <> " defines no nbSections"]; Exit[1]];
  {None, "Section", {"Input", #} & /@ #[[2]]} & /@ nbSections);

(* ---- display-wrap guard (ENTRY MODE ONLY) ----
   nb2txt goes through the FE's ExportPacket["InputText"], which hard-wraps
   long expressions at display width and does NOT mark which newlines were
   typed. This tool parses each cell whole, line breaks preserved in the
   boxes -- so a transcript modelled on nb2txt output delivers a notebook
   full of hard breaks nobody typed. Tested 2026-08-26: the wrap cannot be
   suppressed at export (PageWidth has no effect) and cannot be undone
   afterwards (joining lines until they parse re-splits a product wrapped
   after a complete prefix). Detection is therefore the only guard, and the
   convention it enforces is one top-level expression per line.
   Bare-.nb mode is exempt: it is the exact inverse of nb2txt and MUST
   reproduce the wraps verbatim, which is what tests/roundtrip.sh checks. *)

naTopCount[naS_String] := Module[{naF, naR},
  naF = FileNameJoin[{$TemporaryDirectory, "nbauthor-topcount.wl"}];
  WriteString[naF, naS <> "\n"]; Close[naF];
  naR = Quiet@Check[Import[naF, "HeldExpressions"], $Failed];
  Quiet@DeleteFile[naF];
  If[ListQ[naR], Length[naR], $Failed]];

naWrapCheck[naSecs_] := Module[{naBad},
  naBad = Flatten[Function[naSec,
      Function[naC, Module[{naL, naN},
         If[naC[[1]] =!= "Input", Nothing,
          naL = Length@StringSplit[naC[[2]], "\n"];
          naN = naTopCount[naC[[2]]];
          If[IntegerQ[naN] && naL > naN, {naL, naN, First@StringSplit[naC[[2]], "\n"]}, Nothing]]]
        ] /@ naSec[[3]]] /@ naSecs, 1];
  If[naBad =!= {},
   naOut["nb-author: WARNING -- " <> ToString[Length[naBad]] <>
     " input cell(s) contain line breaks that are NOT top-level expression"];
   naOut["  boundaries. If this transcript was modelled on nb2txt output, those"];
   naOut["  are FRONT-END DISPLAY WRAPS, not typed breaks, and they are being"];
   naOut["  written into the notebook as hard breaks. Author one top-level"];
   naOut["  expression per line (agent/TRAPS.md: 'its input cells have line"];
   naOut["  breaks nobody typed')."];
   Do[naOut["    " <> ToString[naB[[1]]] <> " lines / " <> ToString[naB[[2]]] <>
       " expressions: " <> StringTake[naB[[3]], UpTo[60]]], {naB, naBad}]];
  Length[naBad]];

(* ---- text -> boxes, whole-cell, FE parser; 2D sugar ---- *)

naTwoD[naB_] := naB //. {
  RowBox[{naP_, "^", naQ_}] :> SuperscriptBox[naP, naQ],
  RowBox[{"Sqrt", "[", naP_, "]"}] :> SqrtBox[naP]};

naBoxes[naStr_String] := Module[{naR},
  naR = FrontEndExecute@FrontEnd`UndocumentedTestFEParserPacket[naStr, False];
  If[! MatchQ[naR, {BoxData[_], ___} | {{BoxData[_], ___}, ___}],
    naOut["FATAL: FE parser rejected a cell -- first 100 chars: " <>
      StringTake[naStr, UpTo[100]]]; Exit[1]];
  naTwoD@First@First@naR];

naCell[{"Input", naS_}] := Cell[BoxData[naBoxes[naS]], "Input"];
naCell[{"Text", naS_}]  := Cell[naS, "Text"];

(* ---- assemble ---- *)

naEntryMode = DirectoryQ[naDst];
naNbPath = If[naEntryMode,
  FileNameJoin[{naDst, FileNameTake[naDst] <> ".nb"}], naDst];
naMPath = If[naEntryMode,
  FileNameJoin[{naDst, FileNameTake[naDst] <> ".m"}],
  StringReplace[naNbPath, ".nb" ~~ EndOfString -> ".m"]];
naMd5Before = If[FileExistsQ[naMPath], FileHash[naMPath, "MD5"], None];

naSections = If[StringMatchQ[naSrc, __ ~~ ".txt"], naParseTxt[naSrc], naParseWl[naSrc]];

If[naEntryMode, naWrapCheck[naSections]];

UsingFrontEnd[
  naCells = Join[
    If[naEntryMode,
      {Cell[BoxData[naBoxes[If[ValueQ[nbInit], nbInit, naInitDefault]]],
         "Input", InitializationCell -> True]}, {}],
    Flatten[Function[naSec, If[naSec[[1]] === None,
        naCell /@ naSec[[3]],
        {Cell[CellGroupData[
           Join[{Cell[naSec[[1]], naSec[[2]]]}, naCell /@ naSec[[3]]], Open]]}]] /@
      naSections]];
];

Put[Notebook[naCells, StyleDefinitions -> "Default.nb"], naNbPath];

naMd5After = If[FileExistsQ[naMPath], FileHash[naMPath, "MD5"], None];
If[naMd5Before =!= naMd5After,
  naOut["FATAL: the .m changed under an inputs-only build -- " <> naMPath]; Exit[1]];

naOut["nb-author: wrote " <> naNbPath <> " (" <> ToString[FileByteCount[naNbPath]] <> " bytes)"];
naOut["  cells: " <> ToString[
  Tally[Cases[Notebook[naCells], Cell[_, naS_String, ___] :> naS, Infinity]]]];
If[naMd5Before =!= None, naOut["  .m md5 unchanged: " <> ToString[naMd5Before]]];
