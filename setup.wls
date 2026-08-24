(* setup.wls -- one-time setup for a fresh clone of SolutionsX.
   Run from the repo root:   wolframscript -file setup.wls
   Works on Wolfram Language 13.3+ on macOS, Linux and Windows.

   Does three things:
     1. builds the paclet from this clone and installs it on this kernel,
     2. points your SolutionsX configuration at this clone's Data directory,
     3. tells you the two next steps (SetAlias, GetData).
   It does NOT choose your alias: you do that yourself, once, in your first
   session -- SetAlias["<you>"] -- and it is remembered from then on. *)

If[$VersionNumber < 13.3,
  Print["SolutionsX needs Wolfram Language 13.3 or later (this is ", $Version, ")."];
  Exit[1]];

repo = DirectoryName[ExpandFileName[$InputFileName]];
Print["SolutionsX clone: ", repo];

(* xAct is a hard dependency; say so before a long build, not after *)
If[FindFile["xAct`xTensor`"] === $Failed,
  Print["WARNING: xAct does not seem to be installed (xAct`xTensor` not found)."];
  Print["         SolutionsX will not load without it: https://josmar493.dreamhosters.com"]];

Print["Building the paclet (this processes the documentation; give it a minute)..."];
Needs["PacletTools`"];
build = PacletBuild[repo];
If[FailureQ[build],
  Print["Build failed: ", build];
  Exit[1]];
archive = Last[build]["PacletArchive"];
installed = PacletInstall[archive, ForceVersionInstall -> True];
Print["Installed ", installed["Name"], " v", installed["Version"], " -> ", installed["Location"]];

(* point the user configuration at this clone's Data; preserve an existing
   alias if the user already has one *)
cfg = FileNameJoin[{$UserBaseDirectory, "ApplicationData", "SolutionsX", "config.wl"}];
existing = If[FileExistsQ[cfg], Quiet@Check[Get[cfg], <||>], <||>];
If[!AssociationQ[existing], existing = <||>];
existing["DataDirectory"] = FileNameJoin[{repo, "Data"}];
Quiet@CreateDirectory[DirectoryName[cfg], CreateIntermediateDirectories -> True];
Put[existing, cfg];
Print["Configuration written: ", cfg];
Print["  Data directory -> ", existing["DataDirectory"]];
Print[If[KeyExistsQ[existing, "Alias"],
  "  Alias          -> " <> existing["Alias"] <> " (kept)",
  "  Alias          -> not set yet"]];

Print[""];
Print["Next, in a notebook:"];
Print["  << VasilDimitrov`SolutionsX`"];
If[!KeyExistsQ[existing, "Alias"],
  Print["  SetAlias[\"<you>\"]        (* your entries will live under Data/<you>; remembered from now on *)"]];
Print["  GetData[\"Vasko/<name>\"]  (* read a shipped entry; save your own work with SaveData *)"];
