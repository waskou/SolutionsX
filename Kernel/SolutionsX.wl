(* ::Package:: *)

(* ::Section:: *)
(*Licensing & Begin Package*)


(* ::Input::Initialization:: *)
VasilDimitrov`SolutionsX`$FieldsXVersionExpected={"1.1.4",{2021,8,26}};
VasilDimitrov`SolutionsX`$xTeriorVersionExpected={"0.9.1",{2019,5,17}};
VasilDimitrov`SolutionsX`$Version={"2.0.0",{2026,8,24}};


(* ::Input::Initialization:: *)
(*Front-end loads are silent: every Print of the dependency cascade is
captured into $LoadLog (package -> its printout, split on xCore's dashed
bars) and the package's own banner goes through bannerPrint into the same
log. Loading records, display replays: the panel and the Data status are
shown by LocateData[], the display line of the initialization cell, which
reproduces the same output on every evaluation -- so the FE's yes/no
initialization double run is invisible by construction. Headless sessions
print verbatim, exactly as before. Messages are never captured, in either
mode -- real problems stay visible.*)
VasilDimitrov`SolutionsX`$LoadLog=<||>;
VasilDimitrov`SolutionsX`bannerPrint[bannerArgs___]:=If[TrueQ[$Notebooks],
VasilDimitrov`SolutionsX`$LoadLog["VasilDimitrov`SolutionsX`"]=
StringJoin[Lookup[VasilDimitrov`SolutionsX`$LoadLog,"VasilDimitrov`SolutionsX`",""],StringJoin[ToString/@{bannerArgs}],"\n"];,
Print[bannerArgs]];


(* ::Input::Initialization:: *)
If[Unevaluated[xAct`xCore`Private`$LastPackage]===xAct`xCore`Private`$LastPackage,xAct`xCore`Private`$LastPackage="VasilDimitrov`SolutionsX`"];


(* ::Input::Initialization:: *)
(*The xAct dependencies load and are version-checked BEFORE BeginPackage, so a
too-old stack aborts with the package context never opened. xAct's own guard
idiom, Throw@Message with no Catch, leaves a half-opened context and a
Throw::nocatch on top of the real message. The check itself is unchanged:
minimum by release date (dates outlive version strings -- two Invar 2.0.5
builds differ only in date), reported through xCore's General::versions.*)
If[TrueQ[$Notebooks],
Module[{lines={},blocks={},cur={},blockName},
Block[{Print=(AppendTo[lines,StringJoin[ToString/@{##}]];)&},
Map[Needs,{"xAct`FieldsX`","xAct`xTerior`","xAct`xCoba`","xAct`xTensor`","xAct`xCore`","xAct`Invar`","xAct`xPerm`","xAct`xPert`","xAct`xTras`"}]];
Do[
If[StringMatchQ[line,"-"..],
If[cur=!={},AppendTo[blocks,cur];cur={}],
AppendTo[cur,line]],
{line,lines}];
If[cur=!={},AppendTo[blocks,cur]];
Do[
blockName=SelectFirst[block,StringStartsQ[#,"Package "]&,None];
blockName=If[StringQ[blockName],First[StringCases[blockName,"Package "~~nn:Except[" "]..:>nn],"xAct"],"xAct"];
VasilDimitrov`SolutionsX`$LoadLog[blockName]=
StringJoin[Lookup[VasilDimitrov`SolutionsX`$LoadLog,blockName,""],StringRiffle[block,"\n"],"\n"];
,{block,blocks}];
],
Map[Needs,{"xAct`FieldsX`","xAct`xTerior`","xAct`xCoba`","xAct`xTensor`","xAct`xCore`","xAct`Invar`","xAct`xPerm`","xAct`xPert`","xAct`xTras`"}]];


(* ::Input::Initialization:: *)
If[Not@OrderedQ@Map[Last,{VasilDimitrov`SolutionsX`$FieldsXVersionExpected,xAct`FieldsX`$Version}],
Message[General::versions,"FieldsX",xAct`FieldsX`$Version,VasilDimitrov`SolutionsX`$FieldsXVersionExpected];
Print["SolutionsX not loaded: FieldsX ",First[xAct`FieldsX`$Version]," at ",FindFile["xAct`FieldsX`"]," is older than the required ",First[VasilDimitrov`SolutionsX`$FieldsXVersionExpected],". Update FieldsX and reload."];
Abort[]]


(* ::Input::Initialization:: *)
If[Not@OrderedQ@Map[Last,{VasilDimitrov`SolutionsX`$xTeriorVersionExpected,xAct`xTerior`$Version}],
Message[General::versions,"xTerior",xAct`xTerior`$Version,VasilDimitrov`SolutionsX`$xTeriorVersionExpected];
Print["SolutionsX not loaded: xTerior ",First[xAct`xTerior`$Version]," at ",FindFile["xAct`xTerior`"]," is older than the required ",First[VasilDimitrov`SolutionsX`$xTeriorVersionExpected],". Update xTerior and reload."];
Abort[]]


(* ::Input::Initialization:: *)
BeginPackage["VasilDimitrov`SolutionsX`",{"xAct`FieldsX`","xAct`xTerior`","xAct`xCoba`","xAct`xTensor`","xAct`xCore`","xAct`Invar`","xAct`xPerm`","xAct`xPert`","xAct`xTras`"}];


(* ::Input::Initialization:: *)
If[!TrueQ[$Notebooks],Print[xAct`xCore`Private`bars]];
bannerPrint["Package VasilDimitrov`SolutionsX` version ",$Version[[1]],", ",$Version[[2]]];
bannerPrint["Copyright \[Copyright] 2025 Vasil Dimitrov under the MIT License."];


(* ::Input::Initialization:: *)
(*DefConstantSymbol and ReportSet print through xAct's own Print calls, not
ours -- in a front end, shadow Print with bannerPrint so their "** ..."
lines land in the SolutionsX block of the load panel instead of in front of
it. The headless branch must NOT shadow: Block[{Print=Print}] would strip
the builtin (the localized symbol only carries a self-referential OwnValue)
and the lines would silently vanish.*)
Module[{settings},
settings[]:=(
DefConstantSymbol[gaugeDim,PrintAs->"\!\(\*SubscriptBox[\(d\), \(G\)]\)"];
DefConstantSymbol[internalDim,PrintAs->"\!\(\*SubscriptBox[\(d\), \(I\)]\)"];
ReportSet[$DefInfoQ,False];
ReportSet[$UndefInfoQ,False];
ReportSet[$CVVerbose,False];
ReportSet[$PrecomputeGammaMatrixProducts,False];);
If[TrueQ[$Notebooks],Block[{Print=bannerPrint},settings[]],settings[]];
];
Off[DefMetric::old];
bannerPrint["** Message DefMetric::old turned off"];
Off[PrintAsCharacter::argx];
bannerPrint["** Message PrintAsCharacter::argx turned off"];
Off[ToCanonical::cmods];
bannerPrint["** Message ToCanonical::cmods turned off"];
CommutativityOfProduct[CircleTimes]^="Commutative";
bannerPrint["** Changed CommutativityOfProduct[CircleTimes] to Commutative"];
(*xTensor 1.3.0's ScreenDollarIndices recurses into InformationData - the
output of ?symbol - and its traversal evaluates fragments of the held
definitions stored there, raising stray messages (Keys::invrl from Compute's
chainEntry iterator, for one). xTensor 1.2.0 screened the same panel
silently, so this reproduces the old observable behaviour: pass ?-output
through untouched, screen everything else exactly as before. Definitions
contain no runtime dollar-indices, so nothing is lost.*)
$PrePrint=If[Head[#]===InformationData,#,ScreenDollarIndices[#]]&;
bannerPrint["** $PrePrint wrapped to leave InformationData unscreened"];
(*The Grade guard. xTensor's Grade falls through to
"Grade[expr_,prod_?ProductQ]:=0" -- per its own source comment written for
NON-graded products -- so an undefined application (a named form w[], a
coordinate r[], a tensor-valued form A[I], a derivative head cd[-a][w[]])
is classified grade-0 and scalar-extracted by every graded product:
w[]\[Wedge]aa[] -> w[]*aa[]. That collapse is why stored metric
expressions used to need a HoldForm wrap across the GetData window,
before Load defines the entry's symbols. One rule, inserted AFTER every
typed Grade rule and BEFORE the catch-all (located by matching its shape,
identical on xTensor 1.2.0 and 1.3.0), gives every undefined APPLICATION
its true grade: unknown. Bare symbols keep the catch-all's 0 on purpose:
they cannot be graded objects in xAct's grammar, and xTerior's
graded-derivation registration (GradeOfDerivation, xTerior.m:371) grades
its bare formal vector symbol at every DefCovD/DefMetric and relies on
the 0 -- widening to bare symbols poisons the Koszul operator's stored
grade and its Leibniz rule (found 2026-08-21, lab/grade-guard).
Registered objects can never reach the rule -- typed rules dispatch
first; it fired zero times across the batteries, the census, xTerior's
195 documentation cells and the FieldsX Super-Yang-Mills computation --
and products of unknowns stay inert instead of collapsing. Same mechanism
FieldsX itself uses to extend Grade. Experimental record: lab/grade-guard
in the development project. Upstream report pending: X4.*)
General::gradeguard="xTensor's Grade catch-all was not found; the Grade guard is NOT installed and naked stored expressions may collapse (see lab/grade-guard in the development project).";
Module[{dv,idx,rule},
rule=HoldPattern[Grade[_[___],_?GradedProductQ]]:>Indeterminate;
$gradeGuardRule=rule;
dv=DownValues[Grade];
If[!MemberQ[dv,rule],
idx=SelectFirst[Range@Length@dv,
MatchQ[dv[[#]],Verbatim[RuleDelayed][_,0]]&&
!FreeQ[Extract[dv,{#,1}],ProductQ]&&
FreeQ[Extract[dv,{#,1}],GradedProductQ]&,$Failed];
If[idx===$Failed,
Message[General::gradeguard],
Unprotect[Grade];
DownValues[Grade]=Insert[dv,rule,idx];
Protect[Grade];
bannerPrint["** Grade guarded: undefined applications have Indeterminate grade"];
]]];


(* ::Input::Initialization:: *)
(*Suspend the guard for one evaluation. MakeArray on a gotten-but-not-loaded
record legitimately meets applications of not-yet-defined scalars when
entries share chart symbols (the "slightly illegal" workflow, 2026-08-24):
the guard grades them Indeterminate, and xTensor's Grade-of-Plus
homogeneity check then throws Validate::inhom where the pre-guard
catch-all's 0 let the array be built correctly. Inside this window the
catch-all semantics are restored verbatim and the caller owns the risk;
InheritedBlock copies Grade's definitions, so the removal is invisible to
everything outside and undone on exit. On the loaded path the guard rule
is unreachable (typed rules dispatch first, zero hits across every corpus,
lab/grade-guard), so suspending it there changes nothing.*)
Attributes[withoutGradeGuard]={HoldFirst};
withoutGradeGuard[body_]:=Internal`InheritedBlock[{Grade},
Unprotect[Grade];
DownValues[Grade]=Select[DownValues[Grade],#=!=$gradeGuardRule&];
body];


(* ::Input::Initialization:: *)
Off[General::shdw]
VasilDimitrov`SolutionsX`Disclaimer[]:=Print["This is the warranty and liability text of the MIT License:\n\nTHE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.\n\nIN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE."]
On[General::shdw]


(* ::Input::Initialization:: *)
If[xAct`xCore`Private`$LastPackage==="VasilDimitrov`SolutionsX`",
Unset[xAct`xCore`Private`$LastPackage];
If[!TrueQ[$Notebooks],Print[xAct`xCore`Private`bars]];
bannerPrint["These packages come with ABSOLUTELY NO WARRANTY; for details type Disclaimer[]. This is free software, and you are welcome to redistribute it under certain conditions. See the MIT License for details."];
If[!TrueQ[$Notebooks],Print[xAct`xCore`Private`bars]];
]


(* ::Section:: *)
(*Names*)


(* ::Input::Initialization:: *)
$DefaultDataDirectory = FileNameJoin[{ExpandFileName@URL@$LocalBase,"SolutionsX"}];
$DataDirectory=$DefaultDataDirectory;
$Apply={Map->Together,ParallelMap->Simplify};
$PrintColor;
$Alias;
Protect[$Alias];
$Curator=False;
Protect[$Curator];
Protect[$DefaultDataDirectory];
Protect[$DataDirectory];
(*completions are registered by addCompletion[] in the User configuration
section at the end of the package, once config and definitions exist*)


(* ::Input::Initialization:: *)
(*Changing the xAct undef behaviour from Remove to ClearAll*)
(*xAct`xTensor`Private`RemoveSymbol[xAct`xTensor`Private`symbol_]:=(Unprotect[xAct`xTensor`Private`symbol];ClearAll[xAct`xTensor`Private`symbol];);*)


(* ::Input::Initialization:: *)
$Objects={$manifold,$parameter,$metric,$frame,$spinStructure,$spinConnection,$bundle,$covd,$chart,$basis,$form,$tensor,$spinor,$function,$constant,$assumption,$rule,$set};
{Dimension,Index,Name,Routine,Chain,Value,$solution,$routine,$auto,$info,DefOf,UndefOf,KeysOf,PropKeysOf,ModKeysOf,AutoKeysOf,OptKeysOf,name,chart,$self};


(* ::Input::Initialization:: *)
enableParallelComputations="HoldForm[applyMaps[apply_, symbol_, valId_] := Module[{slot, ind, basis, dep, keys, values, assumptions = $Assumptions, applyValues, applyKeys}, basis = Map[If[Head[#1] === Times && First[#1] === -1, Times @@ Rest[#1], #1] & , valId, All]; slot = valId/basis; ind = {}; Do[AppendTo[ind, Flatten[(GetIndicesOfVBundle @@ #1 & ) /@ MapAt[VBundleOfBasis, Tally[basis[[ii]]], {All, 1}]]], {ii, 1, Length[basis]}]; dep = First[TensorValues[symbol, valId]]; keys = Keys[Last[TensorValues[symbol, valId]]]; values = Values[Last[TensorValues[symbol, valId]]]; applyValues = Values[apply]; applyKeys = Keys[apply]; Do[Which[applyKeys[[aa]] === Map, Monitor[Do[values[[ii]] = applyValues[[aa]][values[[ii]]]; , {ii, 1, Length[values]}]; , Row[{\"Applying \", $PrintColor[applyValues[[aa]]], \" to the independent values of \", Row[Riffle[Table[ToBases[symbol @@ (slot[[ii]]*ind[[ii]])], {ii, 1, Length[basis]}], \", \"]], \" \", ProgressIndicator[ii/Length[values], ImageSize -> {200, 20}], \" \", ii, \"/\", Length[values]}]], applyKeys[[aa]] === ParallelMap, values = ParallelMap[Block[{$Assumptions = assumptions}, applyValues[[aa]][#1]] & , values, Method -> Automatic, ProgressReporting -> True]; ], {aa, 1, Length[applyKeys]}]; Quiet[symbol /: TensorValues[symbol, valId] = FoldedRule[dep, Thread[keys -> values]]]; ]]";


(* ::Section:: *)
(*Usage messages*)


(* ::Input::Initialization:: *)
Begin["`Private`"];
usagerow[l_List]:=ToString[Row[l,""],StandardForm];
usagerows[arg:_List..]:=StringRiffle[Map[usagerow,{arg}],"\n"];
it[s_]:=Style[s,Italic];
PrependTo[$ContextPath,End[]];


(* ::Input::Initialization:: *)
$DefaultDataDirectory::usage = usagerow[{"$DefaultDataDirectory is the path used for data and notebooks when no other is set: the SolutionsX folder under $LocalBase."}];
$DataDirectory::usage = usagerows[
{"$DataDirectory is the path under which data and notebooks are stored and retrieved."},
{"Entries live in ",it["$DataDirectory"],"/",it["$Alias"],"/",it["name"],"/. Set it with SetDataDirectory."}];
$Alias::usage = usagerows[
{"$Alias is the sub-directory of $DataDirectory holding the current user's entries: their identity."},
{"SaveData and DeleteData act only under it; GetData and OpenData default to it but read any alias via \"alias/name\" or the Alias option. Set it once with SetAlias; it persists in the user configuration file."}];
$Curator::usage = usagerow[{"$Curator is True when this user curates the published Curated corpus: it unlocks working under the Curated alias. Set it with SetCurator; it persists in the user configuration file."}];
Alias::usage = usagerow[{"Alias is an option for GetData and OpenData that selects the alias to read from, equivalent to the \"alias/name\" form."}];
$Apply::usage= usagerows[
{"$Apply is the default value of the Apply option of Compute."},
{"It is a list of rules whose keys are Map or ParallelMap and whose values are applied to computed components; the default is {Map->Together, ParallelMap->Simplify}."}];
Gen::usage=usagerows[
{"Gen[",it["sol"],"[$function,",it["key"],"]] gives the rule sending the scalar function ",it["key"]," to its stored Expression, for use with /."},
{"Gen[",it["sol"],"[$form,",it["key"],"]] gives the rule sending the form's Symbol to its stored Expression."},
{"Gen[",it["sol"],"[$rule,",it["key"],"]] gives the stored Expression of the rule entry ",it["key"],"."},
{"Gen gives {} when the entry is absent, so it is safe to apply unconditionally."}];
(*The four SolutionsX-owned key names of the object system. Symbol and
Expression are System` symbols used as keys, so their key meaning lives on the
Object Keys tech note only, never in a ::usage on a builtin.*)
Dimension::usage=usagerow[{"Dimension is a key of $manifold and $bundle objects: the dimension, a positive integer."}];
Index::usage=usagerow[{"Index is a key of $manifold, $frame, $spinStructure and $bundle objects: the abstract indices to register, as a list of symbols."}];
Name::usage=usagerow[{"Name is a key of $manifold and $chart objects: a string from which SaveData composes the entry name."}];
$auto::usage=usagerow[{"$auto is a key of most objects: slots for the symbols xAct creates alongside the definition, each carrying a Routine (the Chain recipe Compute follows) and its computed Value."}];
ValidateObject::usage=usagerows[
{"ValidateObject[",it["object"],",",it["expr"],",",it["types"],"] checks the association ",it["expr"]," against the template of ",it["object"],", and returns it unchanged if it passes."},
{"ValidateObject[$solution,",it["sol"],",",it["types"],"] checks a whole solution, object type by object type."},
{it["types"]," is a list of key selectors, normally {PropKeysOf,OptKeysOf}. Failure issues a message and throws ",it["object"],"."}];
Unload::usage=usagerows[
{"Unload[",it["sol"],"] undefines every xAct object of ",it["sol"],", in reverse order of construction, keeping the stored association."},
{"Unload[",it["sol"],"[",it["object"],"],",it["key"],"] undefines a single entry; a list of keys is also accepted."},
{"Unload does not delete the entry: use DropFrom for that."}];
Load::usage=usagerows[
{"Load[",it["sol"],"] installs the UpValues of ",it["sol"],", then makes the xAct definitions of every entry it holds."},
{"Load[",it["sol"],"[",it["object"],"],",it["key"],"] loads a single entry; a list of keys is also accepted."},
{"Load is the step that turns an association returned by GetData into live xAct definitions."}];
IncludeTo::usage=usagerows[
{"IncludeTo[",it["sol"],"[",it["object"],"],",it["assoc"],"] validates ",it["assoc"],", stores it under ",it["object"],", and makes the corresponding xAct definition."},
{"IncludeTo[",it["sol"],"[",it["object"],"],{",it["assoc"],"..}] does the same for several entries in order."},
{"The keys of ",it["assoc"]," are those of Options[",it["object"],"]; missing keys take their default. Any entry already under the same key is unloaded first."}];
DropFrom::usage=usagerows[
{"DropFrom[",it["sol"],"[",it["object"],"],",it["key"],"] undefines the entry and removes it from ",it["sol"],"; a list of keys is also accepted."},
{"Unlike Unload, DropFrom forgets the stored association as well."}];
MergeWith::usage=usagerows[
{"MergeWith[",it["sol"],",",it["assoc"],"] includes and loads every entry of the solution association ",it["assoc"]," into ",it["sol"],"."},
{"It refuses, with a message, if the two share any symbol or any key."}];
Instantiate::usage=usagerows[
{"Instantiate[",it["sol"],"] clears ",it["sol"]," and makes it an empty solution: an association over $Objects, carrying the UpValues of Load, Unload, IncludeTo, DropFrom and MergeWith."},
{"Instantiate[",it["sol"],",",it["assoc"],"] does the same and then merges ",it["assoc"]," into it."}];
MakeArray::usage=usagerows[
{"MakeArray[",it["expr"],"] gives the component array of ",it["expr"],", which may be a line element written with CircleTimes or a differential form written with Wedge."},
{"The chart is deduced from the Diff's appearing in ",it["expr"],". It is the inverse of MakeMetric and MakeForm."}];
MakeMetric::usage=usagerow[{"MakeMetric[",it["chart"],"][",it["array"],"] gives the line element of the symmetric matrix ",it["array"]," in ",it["chart"],", as a sum of CircleTimes of coordinate differentials."}];
MakeForm::usage=usagerow[{"MakeForm[",it["chart"],"][",it["array"],"] gives the differential form of the totally antisymmetric ",it["array"]," in ",it["chart"],", as a sum of Wedge of coordinate differentials."}];
SaveData::usage=usagerows[
{"SaveData[",it["sol"],"] writes ",it["sol"]," and the evaluation notebook into ",it["$DataDirectory"],"/",it["$Alias"],"/",it["name"],"/, and echoes the destination."},
{"The name is built from the first manifold's Name and Dimension, the first metric's Signature and the first chart's Name: an entry with a chart is a Sol, one without is a Thr."},
{"Every save stamps ",it["sol"],"[$info]: SolutionsX version, the saving alias (LastEdit), date, and the Wolfram and xAct versions. The stamp is machine-written and invisible to validation."}];
GetData::usage=usagerows[
{"GetData[",it["name"],"] gives the stored solution association of the entry ",it["name"]," under the user's own alias, validated but not loaded."},
{"GetData[",it["\"alias/name\""],"] (or the option Alias->",it["\"alias\""],") reads the entry of another alias."},
{"GetData[] takes the alias and name from the directory containing the current notebook."},
{"Follow GetData with Load to make the definitions."}];
Resurrect::usage=usagerow[{"Resurrect[",it["symbol"],"] rebinds any symbol removed by an xAct Undef, by round-tripping ",it["symbol"]," through Compress and replacing Removed[",it["name"],"] by Symbol[",it["name"],"]."}];
Revive::usage=usagerow[{"Revive[",it["sol"],"] applies Resurrect throughout ",it["sol"],", to every symbol in the Global` context and to $Assumptions. It is called after each unload."}];
SetAlias::usage=usagerows[
{"SetAlias[",it["\"alias\""],"] sets $Alias, refreshes the argument completions, and records the alias in the user configuration file so later sessions restore it."},
{"SetAlias[",it["\"alias\""],", Permanent->False] sets it for this session only."}];
SetDataDirectory::usage=usagerows[
{"SetDataDirectory[",it["\"path\""],"] sets $DataDirectory, refreshes the argument completions, and records the path in the user configuration file so later sessions restore it."},
{"SetDataDirectory[",it["\"path\""],", Permanent->False] sets it for this session only."}];
SetCurator::usage=usagerows[
{"SetCurator[True] marks this user as the curator of the Curated corpus and records it in the user configuration file; SetCurator[False] removes the mark."},
{"SetCurator[",it["flag"],", Permanent->False] sets it for this session only."}];
CopyData::usage=usagerows[
{"CopyData[",it["\"alias/name\""],"] copies the entry ",it["name"]," of another alias into your own alias, notebook and all; the source is never touched."},
{"CopyData[",it["\"alias\""],"] copies every entry of that alias into yours, one by one."},
{"Names you already have are collected and confirmed one by one. Overwrite->Automatic (the default) asks in a front end and skips them headless; Overwrite->False always skips; Overwrite->True replaces silently."},
{"When the data tree has no Curated alias, CopyData[",it["\"Curated\""],"] copies from the curated corpus bundled inside the installed paclet."}];
CurateData::usage=usagerows[
{"CurateData[",it["\"name\""],"] publishes the entry ",it["name"]," of your own alias into the Curated corpus. Curator-only; enable with SetCurator[True]."},
{"An existing Curated entry of the same name is replaced only after confirmation; the Overwrite option works as in CopyData."}];
Overwrite::usage=usagerow[{"Overwrite is an option for CopyData and CurateData: Automatic (default) asks per existing entry in a front end and skips headless; True replaces silently; False always skips."}];
DeleteData::usage=usagerows[
{"DeleteData[",it["name"],"] permanently deletes the entry directory ",it["name"]," and all of its contents."},
{"DeleteData acts only under the user's own alias; entries of other aliases cannot be deleted."}];
OpenData::usage=usagerows[
{"OpenData[",it["name"],"] opens the notebook of the entry ",it["name"]," under the user's own alias."},
{"OpenData[",it["\"alias/name\""],"] (or the option Alias->",it["\"alias\""],") opens another alias's entry."}];
BasisOfVBundle::usage=usagerows[
{"BasisOfVBundle[",it["bundle"],"] gives the first basis or chart defined over ",it["bundle"],", or None if there is none."},
{"It issues a message and throws $Failed if ",it["bundle"]," is not a vector bundle."}];
ToBases::usage=usagerows[
{"ToBases[",it["expr"],"] rewrites every abstract index of ",it["expr"]," in the basis belonging to its vector bundle."},
{"The option Not->{",it["basis"],"..} leaves those bases untouched."}];
ToArray::usage=usagerows[
{"ToArray[",it["expr"],"] gives the explicit component array of ",it["expr"],", by taking it to bases, tracing dummies and resolving component values."},
{"The option Not->{",it["basis"],"..} leaves those bases untouched."}];
Compute::usage=usagerows[
{"Compute[",it["sol"],"[",it["object"],",",it["key"],",$auto,",it["autoKey"],"]] computes the components of ",it["autoKey"]," by walking its stored Chain, and stores them under Value."},
{"Compute[",it["sol"],"[",it["object"],",",it["key"],"]] does this for every automatic tensor of the entry."},
{"A Chain entry HoldForm[",it["expr"],"]->{",it["slots"],"} takes the components from ",it["expr"],"; an entry {",it["slots"],"}->{",it["slots"],"} raises or lowers indices with the metric. Nothing is computed if the Chain is unset."},
{"Options are Chain, which overrides and stores the recipe, Using, which chooses the metric of each bundle, and Apply, which defaults to $Apply."}];
EnableParallelComputations::usage=usagerow[{"EnableParallelComputations[] defines Global`applyMaps, letting Compute apply ParallelMap entries of Apply across parallel kernels."}];
SolutionsXHelp::usage=usagerow[{"SolutionsXHelp[] opens the SolutionsX guide page in the Documentation Center."}];
Welcome::usage=usagerows[
{"Welcome[] guides a first session: it asks where your data should live and what your alias is, records both in the user configuration, and offers the curated entries \[LongDash] individually selectable \[LongDash] to copy under your alias."},
{"Without a front end it prints the same steps as commands. Welcome[] never touches the curator flag and can be re-run at any time."}];
NewData::usage=usagerows[
{"NewData[] creates a notebook holding the initialization cell that loads SolutionsX."},
{"The cell carries no user identity and no absolute paths: LocateData[] finds the data root relative to the notebook's own position; $Alias always comes from the user configuration."}];
LocateData::usage=usagerows[
{"LocateData[] points $DataDirectory at the data tree containing the current notebook, for this session only, then displays the package load panel and the Data status."},
{"It relocates only when the notebook sits inside a Data entry directory (",it["\[Ellipsis]/alias/name/"],"); anywhere else it leaves the configured values untouched and just displays them. It is the display line of NewData's initialization cell: in a front end the package load itself prints nothing, and every run of the cell reproduces the same panel and status line."},
{"LocateData[",it["dir"],"] does the same for an explicit entry directory path."}];
ResumeAs::usage=usagerows[
{"ResumeAs[",it["sol"],"] restores the current notebook's entry into ",it["sol"]," (GetData[] followed by Load) when nothing has been loaded in this kernel yet, and rebuilds the Load banner of everything loaded so far otherwise."},
{"Put it at the top of a notebook section: after a kernel quit it resumes from disk; in a full top-to-bottom run it only re-shows the banner, so the same notebook supports both workflows."}];
RulesToChain::usage=usagerow[{"RulesToChain[",it["a"],"->",it["b"],"->",it["c"],"] gives {",it["a"],"->",it["b"],",",it["b"],"->",it["c"],"}, the form Compute expects for a raising or lowering ladder."}];
ExpandForms::usage=usagerow[{"ExpandForms[",it["sol"],"][",it["expr"],"] rewrites every differential form of ",it["sol"]," appearing in ",it["expr"]," as its companion tensor contracted with a wedge of coordinate differentials, and expands the exterior derivative of every degree-zero expression in the same sweep."}];
WedgeCoeff::usage=usagerows[
{"WedgeCoeff[",it["sol"],"][",it["expr"],"] extracts the antisymmetric component tensor of a form written in wedges of coordinate differentials, as the lowest-rank equivalent: a form of degree p above half the dimension d is Hodge-dualized and expanded first, and the extraction runs on the dual, giving the component tensor of the Hodge dual with d-p free indices."},
{"For p at most d/2 the result is the plain component tensor, so WedgeCoeff inverts ExpandForms: the companion tensor comes back exactly."},
{"Reconstructing a dualized form from its result costs the double-dual sign (-1)^(p(d-p)) sign(det g) \[LongDash] in Lorentzian signature the volume form extracts to minus its coefficient. That sign is mathematics, not a convention."},
{"The result is presented in the order of the manifold's Index list: the frees introduced by the extraction take the first names not already free in the expression, and ScreenDollarIndices renames the dummies to the following unused ones. If the registered list is exhausted mid-extraction, new indices are generated and registered, so the extraction always succeeds."},
{"Only ContractMetric is applied to the result; simplification is left to the caller."}];
ComputeDiffs::usage=usagerow[{"ComputeDiffs[",it["chart"],"] installs the rules that canonicalise, and annihilate repeated, wedge products of the coordinate differentials of ",it["chart"],"."}];


(* ::Section:: *)
(*Temporary public private names*)


(* ::Input::Initialization:: *)
{xActSymbols,optionIfSupported,conditionalKeysOf,evaluateInHeld,makeKey,include,load,unload,drop,makeSymbolPanel,hook,giveName,extractAllSymbols,myUndefMetric,transformAssociation,addCompletion,makeTensorFromForm,makeMetricArray,dcoeff,makeFormArray,toBasis,myPart,applyTensorSymmetries,includeIndependentValuesFromArray,includeIndependentValuesFromSlot,selector,computeFromArray,computeFromSlot,attachTVs,generatePermutations,overwriteOld,overwriteSame,saveNew,configFile,readConfig,writeConfig,setDataDir,curatedSource,curatorIdentity,welcomeAskDirectory,welcomeAskAlias,welcomeAskCurated,welcomeRecipe,printDataStatus,printLoadPanel,bannerPrint,resolveEntry,makeInfo,$loadedSymbols,withoutGradeGuard,$gradeGuardRule};


(* ::Section:: *)
(*Package Header*)


(* ::Text:: *)
(*Declare your public symbols here:*)


(* ::Input::Initialization:: *)
Begin["`Private`"];


(* ::Input::Initialization:: *)
template;


(* ::Section:: *)
(*Helpers*)


(* ::Input::Initialization:: *)
xActSymbols[]:=DeleteDuplicates[Select[Join[$Manifolds,$Parameters,$Metrics,$CovDs,$VBundles,$Charts,$Bases,$Tensors,$ScalarFunctions,$ConstantSymbols,$Mappings,$InertHeads,$AbstractIndices],Head[#]===Symbol&&!StringContainsQ[ToString[#],"xAct"]&]];


(* ::Input::Initialization:: *)
(*Mirror the loaded xAct. A Def* constructor may gain an option in a later
version: DefTensor has always taken Dagger, DefMetric only takes it from
xTensor 1.3.0 onwards. Offer a key only where the constructor actually accepts
it, so that the template, the verifier and the Def* call agree on any version.
Give a Rule for Options[...], a Symbol for OptKeysOf[...].*)
optionIfSupported[def_,opt_Rule]:=If[MemberQ[Keys@Options[def],First@opt],{opt},{}];
optionIfSupported[def_,key_Symbol]:=If[MemberQ[Keys@Options[def],key],{key},{}];


(* ::Input::Initialization:: *)
(*Evaluate is inert below level 1 of a held expression, so a $set record built
as HoldForm[IndexSet[lhs,Evaluate[%]]] stores Out[] literally and cannot be
replayed in a fresh session. IncludeTo therefore honours Evaluate markers at
include time: each one is evaluated in place and its value spliced into the
held record, leaving everything around it held.*)
evaluateInHeld[expr_]:=expr/.Verbatim[Evaluate][babaroga_]:>With[{talasam=babaroga},talasam/;True];


(* ::Input::Initialization:: *)
(*The read side of optionIfSupported. A stored record carries the complete
option sheet of the xAct that wrote it, so a version-conditional key may sit
in a record and be unknown to the loaded xAct, or be expected by the loaded
xAct and absent from the record. ValidateObject therefore compares key sets
modulo the keys listed here, and pattern-checks a conditional key only when
it is present - the property and modifier sectors stay exact, and the record
itself is never rewritten. When an xAct update adds or removes a Def* option:
wrap it in optionIfSupported in the type's Options and OptKeysOf, and list it
here with its pattern, next to the type's schema.*)
conditionalKeysOf[_]={};


(* ::Input::Initialization:: *)
Attributes[Resurrect]={HoldFirst};
Resurrect[symbol_]:=(
If[ValueQ[symbol],
symbol=Uncompress[Compress[symbol]]/.Removed[name_]:>Symbol[name];
]
)
transformAssociation[assoc_Association,transformFunction_]:=Module[{},
  Association[
    KeyValueMap[
      (
        transformFunction[#1]->
          If[AssociationQ[#2],
            transformAssociation[#2,transformFunction],
            transformFunction[#2]
          ]
      )&,
      assoc
    ]
  ]
]
Attributes[Revive]={HoldFirst};
Revive[sol_]:=(
sol=transformAssociation[sol,(Uncompress[Compress[#]]/.Removed[name_]:>Symbol[name])&];
(*sol=Uncompress[Compress[sol]]/.Removed[name_]:>Symbol[name]//Evaluate;*)
(Function[{talasam},Resurrect[talasam],HoldFirst]@@MakeExpression[#])&/@Names["Global`"<>"*"];
System`$Assumptions//Resurrect;
)


(* ::Input::Initialization:: *)
SetAlias::invalid="Argument of SetAlias should be a string";
SetAlias::curated="Only the curator works under the Curated alias -- enable with SetCurator[True]. Entries are published into Curated with CurateData.";
SetAlias::noidentity="The Curated role needs to know who you are -- set your own alias first with SetAlias[\"you\"].";
Options[SetAlias]={Permanent->True};
(*Curated is a role, never an identity: entering it is curator-only, always
session-only (the config keeps who you are), matched case-insensitively and
spelled canonically, and the identity behind the role is derived from the
configuration file -- SaveData stamps Author with it (design contract:
lab/curation-welcome/DESIGN.md in the development project).*)
SetAlias[alias_,OptionsPattern[]]:=Module[{c},
If[!StringQ[alias],
Message[SetAlias::invalid];
Throw[$Failed]];
If[StringMatchQ[alias,"curated",IgnoreCase->True],
If[!TrueQ[$Curator],
Message[SetAlias::curated];
Throw[$Failed]];
c=readConfig[];
c=If[AssociationQ[c],c["Alias"],$Failed];
If[!StringQ[c]||StringMatchQ[c,"curated",IgnoreCase->True],
Message[SetAlias::noidentity];
Throw[$Failed]];
curatorIdentity=c;
Unprotect[$Alias];
$Alias="Curated";
Protect[$Alias];
addCompletion[];
Echo[Row[{"Entering ",$PrintColor["Curated"]," as ",$PrintColor[curatorIdentity]," (this session)"}]];
,
Unprotect[$Alias];
$Alias=alias;
Protect[$Alias];
addCompletion[];
If[TrueQ[OptionValue[Permanent]],writeConfig[]];
Echo[Row[{"Alias set to ",$PrintColor[alias]}]];
]
]


(* ::Input::Initialization:: *)
SetDataDirectory::invalid="Argument of SetDataDirectory should be a (string)path";
(*setDataDir is the silent worker: LocateData uses it when it derives the
location from a notebook's position (the status line right after already
displays the result); the public SetDataDirectory echoes the change.*)
setDataDir[path_]:=(
Unprotect[$DataDirectory];
$DataDirectory=path;
Protect[$DataDirectory];
addCompletion[];
)
Options[SetDataDirectory]={Permanent->True};
SetDataDirectory[path_,OptionsPattern[]]:=Module[{},
If[StringQ[path],
setDataDir[path];
If[TrueQ[OptionValue[Permanent]],writeConfig[]];
Echo[Row[{"Data directory set to ",$PrintColor[path]}]];
,
Message[SetDataDirectory::invalid];
Throw[$Failed];
]
]


(* ::Input::Initialization:: *)
SetCurator::invalid="Argument of SetCurator should be True or False";
Options[SetCurator]={Permanent->True};
SetCurator[flag:(True|False),OptionsPattern[]]:=Module[{},
Unprotect[$Curator];
$Curator=flag;
Protect[$Curator];
If[TrueQ[OptionValue[Permanent]],writeConfig[]];
Echo[If[flag,"Curator enabled","Curator disabled"]];
]
SetCurator[_,OptionsPattern[]]:=(
Message[SetCurator::invalid];
Throw[$Failed];
)


(* ::Input::Initialization:: *)
(*The blind cross-alias fetch: from anybody, into your own alias, source
never touched. Slashless arguments can only be aliases (giveName entries
always contain "__"); bare entry names are refused with the qualified
spelling. Duplicates are collected and settled per item; a whole entry
directory is replaced or kept, never merged. Design contract:
lab/curation-welcome/DESIGN.md in the development project.*)
(*The bundled curated corpus: the installed paclet carries Data/Curated
(the Resource extension in PacletInfo), so a data tree without a Curated
alias -- a Paclet Repository install with no clone -- still has a curated
source. The tree's own Curated, when present, always wins: it may be
newer than the release snapshot inside the paclet.*)
curatedSource[]:=With[{tree=FileNameJoin[{$DataDirectory,"Curated"}]},
If[DirectoryQ[tree],tree,
With[{p=Quiet@Check[PacletObject["VasilDimitrov/SolutionsX"]["Location"],$Failed]},
If[StringQ[p]&&DirectoryQ[FileNameJoin[{p,"Data","Curated"}]],
FileNameJoin[{p,"Data","Curated"}],$Failed]]]];
CopyData::noalias="CopyData copies into your own alias -- set it first with SetAlias[\"you\"].";
CopyData::bare="\"`1`\" is a bare entry name, which resolves to your own alias -- copying from yourself is meaningless. Use CopyData[\"alias/name\"].";
CopyData::self="`1` is your own alias -- nothing to copy.";
CopyData::nosource="No alias `1` under `2`.";
CopyData::noentry="No entry `1` under alias `2`.";
Options[CopyData]={Overwrite->Automatic};
CopyData[spec_String,OptionsPattern[]]:=Module[
{ow,parts,srcAlias,names,srcDir,dstDir,fresh,dups,decision,decideAll=None,
copied={},replaced={},skipped={}},
If[!StringQ[$Alias],Message[CopyData::noalias];Throw[$Failed]];
If[!StringQ[$DataDirectory],Message[SaveData::baddirectory,$DefaultDataDirectory];Throw[$Failed]];
ow=OptionValue[Overwrite];
If[ow===Automatic,ow=If[TrueQ[$Notebooks],"ask",False]];
parts=StringSplit[spec,"/"];
Which[
Length[parts]==2,srcAlias=First[parts];names=Rest[parts],
StringContainsQ[spec,"__"],Message[CopyData::bare,spec];Throw[$Failed],
True,srcAlias=spec;names=All];
If[StringMatchQ[srcAlias,$Alias,IgnoreCase->True],
Message[CopyData::self,srcAlias];Throw[$Failed]];
srcDir=FileNameJoin[{$DataDirectory,srcAlias}];
If[!DirectoryQ[srcDir]&&StringMatchQ[srcAlias,"Curated",IgnoreCase->True],
srcDir=curatedSource[]];
If[!StringQ[srcDir]||!DirectoryQ[srcDir],Message[CopyData::nosource,srcAlias,$DataDirectory];Throw[$Failed]];
If[names===All,
names=FileNameTake/@Select[FileNames["*",srcDir],DirectoryQ];
If[names==={},
Echo[Row[{"Nothing to copy \[LongDash] alias ",$PrintColor[srcAlias]," is empty"}]];
Return[]];
,
If[!DirectoryQ[FileNameJoin[{srcDir,First[names]}]],
Message[CopyData::noentry,First[names],srcAlias];Throw[$Failed]];
];
dstDir=FileNameJoin[{$DataDirectory,$Alias}];
Quiet@CreateDirectory[dstDir,CreateIntermediateDirectories->True];
fresh=Select[names,!DirectoryQ[FileNameJoin[{dstDir,#}]]&];
dups=Complement[names,fresh];
Do[
CopyDirectory[FileNameJoin[{srcDir,n}],FileNameJoin[{dstDir,n}]];
AppendTo[copied,n];
Echo[Row[{"Copied ",$PrintColor[srcAlias<>"/"<>n]," to ",$PrintColor[$Alias<>"/"<>n]}]];
,{n,fresh}];
Do[
decision=Which[
decideAll==="all",True,
decideAll==="none",False,
ow===True,True,
ow===False,False,
True,
Switch[
ChoiceDialog[Row[{n," already exists under ",$Alias,". Overwrite it?"}],
{"Overwrite"->True,"Skip"->False,"Overwrite all"->"all","Skip all"->"none"}],
True,True,
False,False,
"all",decideAll="all";True,
"none",decideAll="none";False]];
If[decision,
DeleteDirectory[FileNameJoin[{dstDir,n}],DeleteContents->True];
CopyDirectory[FileNameJoin[{srcDir,n}],FileNameJoin[{dstDir,n}]];
AppendTo[replaced,n];
Echo[Row[{"Replaced ",$PrintColor[n]," from ",$PrintColor[srcAlias<>"/"<>n]}]];
,
AppendTo[skipped,n];
Echo[Row[{"Skipped ",$PrintColor[srcAlias<>"/"<>n]," (",$PrintColor[$Alias<>"/"<>n]," exists)"}]];
];
,{n,dups}];
addCompletion[];
Echo[Row[{"Data copied from ",$PrintColor[srcAlias]," to ",$PrintColor[$Alias],": ",
Length[copied]," copied, ",Length[replaced]," replaced, ",Length[skipped]," skipped"}]];
]


(* ::Input::Initialization:: *)
(*The publication gate: your own entry, copied verbatim into the literal
spelling Curated ($info untouched -- Author is provenance and curation is
publication, not authorship). Curator-only; the ::tracked message is the
careful explanation owed to anyone using it: the write is local until the
repository owner commits it. Design contract: lab/curation-welcome/DESIGN.md
in the development project.*)
CurateData::curator="CurateData publishes into the Curated corpus and is curator-only -- enable with SetCurator[True] if you are the curator.";
CurateData::noalias="Set your own alias first with SetAlias[\"you\"] -- CurateData publishes entries from your own alias.";
CurateData::role="You are working under the Curated alias -- CurateData publishes from your own alias; while in the role, save directly instead.";
CurateData::qualified="CurateData publishes from your own alias only -- CopyData[\"`1`\"] it to yourself first.";
CurateData::noentry="No entry `1` under your alias `2`.";
CurateData::tracked="Data/Curated is git-tracked: this write is local to your clone until the repository owner commits and pushes it. If you are not the owner, your curated copy stays local and may conflict with upstream updates.";
Options[CurateData]={Overwrite->Automatic};
CurateData[name_String,OptionsPattern[]]:=Module[{ow,srcDir,dst,decision},
If[!TrueQ[$Curator],Message[CurateData::curator];Throw[$Failed]];
If[!StringQ[$Alias],Message[CurateData::noalias];Throw[$Failed]];
If[$Alias==="Curated",Message[CurateData::role];Throw[$Failed]];
If[!StringQ[$DataDirectory],Message[SaveData::baddirectory,$DefaultDataDirectory];Throw[$Failed]];
If[StringContainsQ[name,"/"],Message[CurateData::qualified,name];Throw[$Failed]];
srcDir=FileNameJoin[{$DataDirectory,$Alias,name}];
If[!DirectoryQ[srcDir],Message[CurateData::noentry,name,$Alias];Throw[$Failed]];
Message[CurateData::tracked];
dst=FileNameJoin[{$DataDirectory,"Curated",name}];
Quiet@CreateDirectory[FileNameJoin[{$DataDirectory,"Curated"}],CreateIntermediateDirectories->True];
If[DirectoryQ[dst],
ow=OptionValue[Overwrite];
If[ow===Automatic,ow=If[TrueQ[$Notebooks],"ask",False]];
decision=Which[
ow===True,True,
ow===False,False,
True,ChoiceDialog[Row[{name," already exists in Curated. Replace it?"}],
{"Replace"->True,"Cancel"->False}]];
If[!TrueQ[decision],
Echo[Row[{"Kept existing ",$PrintColor["Curated/"<>name]}]];
Return[]];
DeleteDirectory[dst,DeleteContents->True];
];
CopyDirectory[srcDir,dst];
addCompletion[];
Echo[Row[{"Curated ",$PrintColor[$Alias<>"/"<>name]," \[Rule] ",$PrintColor["Curated/"<>name]}]];
]


(* ::Input::Initialization:: *)
(*Completion candidates: own bare names, plus alias/name composites for every
alias present under the data root -- prefix matching then gives two-stage
completion (type a letter for own entries, an alias for the rest).
DeleteData completes own names only: it refuses foreign entries.*)
addCompletion[]:=Module[{aliases={},own={},composites={}},
If[StringQ[$DataDirectory],
aliases=FileNameTake/@Select[FileNames["*",$DataDirectory],DirectoryQ];
composites=Flatten@Table[a<>"/"<>FileNameTake[e],{a,aliases},{e,Select[FileNames["*",FileNameJoin[{$DataDirectory,a}]],DirectoryQ]}];
If[StringQ[$Alias],
own=FileNameTake/@Select[FileNames["*",FileNameJoin[{$DataDirectory,$Alias}]],DirectoryQ]];
];
(FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#]]&)["SetAlias"->{Join[{""},aliases]}];
(FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#]]&)["GetData"->{Join[{""},own,composites]}];
(FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#]]&)["OpenData"->{Join[{""},own,composites]}];
(FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#]]&)["DeleteData"->{Join[{""},own]}];
(FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#]]&)["CurateData"->{Join[{""},own]}];
(FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#]]&)["CopyData"->{Join[{""},aliases,composites]}];
]


(* ::Input::Initialization:: *)
(*Clear, not ClearAll: ClearAll would wipe the ::usage set in the Usage messages
section above. Attributes and Options are re-established immediately below.*)
Clear[Gen]
Attributes[Gen]={HoldFirst};
Gen[sol_[$function,key_]]:=Module[{head,arg,val,rulee},
If[Head@sol[$function,key]===Missing,
Return[{}];
,
head=Head@sol[$function,key,Symbol];
arg=Level[sol[$function,key,Symbol],1];
val=HoldForm[Evaluate[sol[$function,key,Expression]]];rulee=Thread[(HoldPattern[#]&/@arg)->Table[SymbolJoin["xxx",ii],{ii,1,Length@arg}]];Return[(head->Function[Evaluate@ReleaseHold@(arg/.rulee),Evaluate@ReleaseHold[val/.rulee]])];
];
]
Gen[sol_[$form,key_]]:=Module[{},
If[Head@sol[$form,key]===Missing,
Return[{}];
,
Return[(sol[$form,key,Symbol]->sol[$form,key,Expression])];
];
]
Gen[sol_[$rule,key_]]:=Module[{},
If[Head@sol[$rule,key]===Missing,
Return[{}];
,
Return[sol[$rule,key,Expression]];
]
]


(* ::Input::Initialization:: *)
BasisOfVBundle::unknown="Unknown bundle `1`";
BasisOfVBundle::toomany="Multiple charts: `1` found under the same tangent bundle `2`";
BasisOfVBundle[bundle_]:=Module[{bases},
If[!VBundleQ[bundle],Message[BasisOfVBundle::unknown,bundle];Throw[$Failed]];
bases=Select[$Bases,VBundleOfBasis[#]===bundle&];
(*If[TangentBundleQ[bundle]&&Length@bases=!=1,Message[BasisOfVBundle::toomany,bases,bundle];Throw[$Failed]];*)
If[bases==={},
Return[None];
,
Return[First@bases];
];
]


(* ::Input::Initialization:: *)
ClearAll[toBasis]
toBasis[expr_,arg_]:=ToBasis[arg][expr]
Clear[ToBases](*not ClearAll: it would wipe the ::usage*)
Options[ToBases]={Not->{}};
ToBases[expr_,OptionsPattern[]]:=Module[{allBases,not=OptionValue[Not]},
(*allBases=BasisOfVBundle[#]&/@(VBundleOfIndex[#]&/@Map[If[Head[#]===Times&&First[#]===-1,Times@@Rest[#],#]&,(IndicesOf[AIndex][expr]/.IndexList->List),All]);*)
allBases=DeleteCases[None][BasisOfVBundle[#]&/@(VBundleOfIndex[#]&/@(Abs[#]&/@(IndicesOf[AIndex][expr]/.IndexList->List)/.Abs[talasam_]:>talasam))];
If[not=!={},
If[Length[not]===0,
allBases=DeleteCases[not][allBases];
,
Do[allBases=DeleteCases[not[[ii]]][allBases],{ii,1,Length@not}]
];
];
Fold[toBasis,expr,allBases]
]
Clear[ToArray](*not ClearAll: it would wipe the ::usage*)
Options[ToArray]={Not->{}};
ToArray[expr_,OptionsPattern[]]:=Module[{
not=OptionValue[Not]
},
ToValues@ToValues@ToValues@ComponentArray@TraceBasisDummy@ToBases[expr,Not->not]
]


(* ::Input::Initialization:: *)
EnableParallelComputations[]:=ReleaseHold@ToExpression@enableParallelComputations


(* ::Input::Initialization:: *)
RulesToChain[chain_]:=Module[{babaroga,orig,final},
orig={chain}/.Rule->babaroga//.babaroga[talasam__]:>talasam;
final={};
Do[
AppendTo[final,orig[[ii]]->orig[[ii+1]]]
,
{ii,1,Length@orig-1}
];
final
]


(* ::Input::Initialization:: *)
ComputeDiffs[chart_]:=Module[{
coord=ScalarsOfChart[chart],
basis,
rules
},
basis=Diff[#]&/@coord;
Wedge[args__]/;(
  With[{elems=Select[{args},MemberQ[basis,#]&]},
    Length[elems]>Length[DeleteDuplicates[elems]]
  ]
):=0;
rules=Table[Thread[(#/.List->Wedge)&/@Flatten[Permutations/@Subsets[basis,{ii}],1]->ToCanonical[(#/.List->Wedge)&/@Flatten[Permutations/@Subsets[basis,{ii}],1]]],{ii,1,Length@coord}];
Do[rules[[ii]]=Select[rules[[ii]],#[[1]]=!=#[[2]]&],{ii,1,Length@rules}];
Do[(#/.Rule->SetDelayed)&/@rules[[ii]],{ii,Reverse@Range[Length@rules]}];
]


(* ::Section:: *)
(*ValidateObject*)


(* ::Input::Initialization:: *)
ValidateObject::notassoc="The input `1` is not an association.";
ValidateObject::keysmismatch="The keys do not match. Missing keys: `1`. Extra keys: `2`.";
ValidateObject::badval="The value for key `1` (`2`) does not match the expected pattern: `3`.";
ValidateObject[object_,expr_,validationTypes_List]:=Module[{
keys=KeysOf[object],
conditionalKeys=Keys[conditionalKeysOf[object]],
validationKeys=Flatten[Join[#[object]&/@validationTypes]],
validationPatterns,
exprKeys
},
validationPatterns=Lookup[object[],validationKeys];
(*Check if expr is assoc*)
If[!AssociationQ[expr],
Message[ValidateObject::notassoc,expr];
Throw[object]
];
(*Check for the correct keys, modulo the version-conditional ones: a record
written under another xAct version may carry a conditional key this version
does not know, or lack one it expects. Everything else stays exact.*)
exprKeys=Keys[expr];
If[Sort[Complement[exprKeys,conditionalKeys]]=!=Sort[Complement[keys,conditionalKeys]],
With[{
missingKeys=SortBy[Complement[keys,exprKeys,conditionalKeys],Position[keys,#]&],
extraKeys=SortBy[Complement[exprKeys,keys,conditionalKeys],Position[exprKeys,#]&]
},
Message[ValidateObject::keysmismatch,missingKeys,extraKeys];
Throw[object]
]
];
(*Check keys which are called for validation; conditional keys are owned by
the loop below*)
Do[
Module[{pattern},
        pattern=validationPatterns[[Position[validationKeys,key][[1,1]]]];
        If[!MatchQ[expr[key],pattern],
            Message[ValidateObject::badval,key,expr[key],pattern];
            Throw[object]
        ]
    ]
,{key,DeleteCases[validationKeys,Alternatives@@conditionalKeys]}
];
(*A conditional key is validated against its declared pattern whenever it is
present, whether or not the loaded xAct knows it; an absent one is tolerated*)
Do[
If[KeyExistsQ[expr,First@rule]&&!MatchQ[expr[First@rule],Last@rule],
Message[ValidateObject::badval,First@rule,expr[First@rule],Last@rule];
Throw[object]
]
,{rule,conditionalKeysOf[object]}
];
(*Return if pass*)
expr
];
ValidateObject[$solution,expr_,validationTypes_List]:=Module[{
keys=$Objects,
exprKeys
},
(*Check if expr is assoc*)
If[!AssociationQ[expr],
Message[ValidateObject::notassoc,expr];
Throw[$solution]
];
(*Check for the correct keys. $info -- the machine-written stamp SaveData
adds (version, alias, date, xAct stack) -- is invisible to validation:
tolerated present or absent, content never inspected. Records predating it
load unchanged; future fields inside it cannot break older Kernels.*)
exprKeys=DeleteCases[Keys[expr],$info];
If[Sort[exprKeys]=!=Sort[keys],
With[{
missingKeys=SortBy[Complement[keys,exprKeys],Position[keys,#]&],
extraKeys=SortBy[Complement[exprKeys,keys],Position[exprKeys,#]&]
},
Message[ValidateObject::keysmismatch,missingKeys,extraKeys];
Throw[$solution]
]
];
(*Verify object*)
Do[
ValidateObject[key,#,validationTypes]&/@(Values@expr[key]);
,{key,keys}
];
(*Return if valid*)
expr
]


(* ::Section:: *)
(*Object constructors*)


(* ::Input::Initialization:: *)
myUndefMetric[met_]:=Module[{
covd=CovDOfMetric[met]
},
Quiet@UndefParameter[SymbolJoin["PerturbationParameter",met]];
Quiet@UndefTensor[SymbolJoin["Perturbation",met]];
Quiet@UndefMetric[met];
Quiet@UndefInertHead[SymbolJoin["CovD",covd]];
Quiet@UndefCovD[covd];
]


(* ::Input::Initialization:: *)
Options[$manifold]={
Symbol->_Symbol,
Dimension->(_Integer?Positive|dim),
Index->{__Symbol},
Name->(_String|name),
Master->_Symbol,
PrintAs->(_String|Identity),
ProtectNewSymbol:>(True|False),
DefInfo->{_String,_String}
};
$manifold/:DefOf[$manifold]=DefManifold;
$manifold/:UndefOf[$manifold]=(
(Quiet@Undef[#])&/@VisitorsOf[#];
UndefManifold[#]
)&;
$manifold/:KeysOf[$manifold]=Keys@Options[$manifold];
$manifold/:PropKeysOf[$manifold]={Symbol,Dimension,Index};
$manifold/:ModKeysOf[$manifold]={Name};
$manifold/:OptKeysOf[$manifold]={Master,PrintAs,ProtectNewSymbol,DefInfo};
$manifold/:$manifold[]:=Association[Options[$manifold]];
$manifold/:$manifold[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$manifold,Join[$manifold[],KeyDrop[Options[DefOf[$manifold]],Tangent],<|Name->name|>,Association[opts]],{PropKeysOf,OptKeysOf}]
]
$manifold/:makeKey[$manifold]=#[Symbol]&;


(* ::Input::Initialization:: *)
Options[$parameter]={
Symbol->_Symbol,
Master->_Symbol,
PrintAs->(_String|Identity),
ProtectNewSymbol:>(True|False),
DefInfo->{_String,_String}
};
$parameter/:DefOf[$parameter]=DefParameter;
$parameter/:UndefOf[$parameter]=UndefParameter;
$parameter/:KeysOf[$parameter]=Keys@Options[$parameter];
$parameter/:PropKeysOf[$parameter]={Symbol};
$parameter/:ModKeysOf[$parameter]={};
$parameter/:OptKeysOf[$parameter]={Master,PrintAs,ProtectNewSymbol,DefInfo};
$parameter/:$parameter[]:=Association[Options[$parameter]];
$parameter/:$parameter[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$parameter,Join[$parameter[],Association@Options[DefOf[$parameter]],Association[opts]],{PropKeysOf,OptKeysOf}]
]
$parameter/:makeKey[$parameter]=#[Symbol]&;


(* ::Input::Initialization:: *)
Options[$routine]={
Chain->{Rule[_,{___List}]...},
Apply->{Rule[Alternatives[Map,ParallelMap],_]...}
};
$routine/:KeysOf[$routine]=Keys@Options[$routine];
$routine/:PropKeysOf[$routine]={};
$routine/:ModKeysOf[$routine]={Chain,Apply};
$routine/:OptKeysOf[$routine]={};
$routine/:$routine[]:=Association[Options[$routine]];
$routine/:$routine[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$routine,Join[$routine[],Association[opts]],{PropKeysOf,OptKeysOf}]
]


(* ::Input::Initialization:: *)
Options[$auto]={
Routine->$routine[],
Value->_
};
$auto/:KeysOf[$auto]=Keys@Options[$auto];
$auto/:PropKeysOf[$auto]={};
$auto/:ModKeysOf[$auto]={Routine,Value};
$auto/:OptKeysOf[$auto]={};
$auto/:$auto[]:=Association[Options[$auto]];
$auto/:$auto[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$auto,Join[$auto[],Association[opts]],{PropKeysOf,OptKeysOf}]
]


(* ::Input::Initialization:: *)
Options[$metric]=Join[{
Signature->(-1|1),
Symbol->_Symbol[(_Symbol|-_Symbol),(_Symbol|-_Symbol)],
CovD->_Symbol,
SymbolOfCovD->{_String,_String},
Expression->_,
$auto-><|_Symbol->$auto[]...|>,
PrintAs->(_String|Identity),
FlatMetric->(True|False),
InducedFrom->({_Symbol,_Symbol}|Null),
ConformalTo->(_|Null),(*needs attention later on*)
OtherDependencies->{___Symbol},
WeightedWithBasis->(_Symbol|Null),
epsilonOrientationInBasis:>{_Symbol,(1|-1)},
Master->_Symbol,
ProtectNewSymbol:>(True|False),
DefInfo->{_String,_String},
DefMetricPerturbation->(True|False),
Torsion->(True|False),
Curvature->(True|False),
SymCovDQ->(True|False)
},
(*Kept apart from the list above because it is version-conditional*)
optionIfSupported[DefMetric,Dagger->(Real|Complex)]
];
$metric/:DefOf[$metric]=DefMetric;
$metric/:UndefOf[$metric]=myUndefMetric[#]&;
$metric/:KeysOf[$metric]=Keys@Options[$metric];
$metric/:PropKeysOf[$metric]={Signature,Symbol,CovD,SymbolOfCovD};
$metric/:ModKeysOf[$metric]={Expression,$auto};
$metric/:OptKeysOf[$metric]=Join[{PrintAs,FlatMetric,InducedFrom,ConformalTo,OtherDependencies,WeightedWithBasis,epsilonOrientationInBasis,Master,ProtectNewSymbol,DefInfo,DefMetricPerturbation,Torsion,Curvature,FromMetric,CurvatureRelations,ExtendedFrom,OrthogonalTo,ProjectedWith,SymCovDQ},optionIfSupported[DefMetric,Dagger]];
(*Version-conditional since xTensor 1.3.0, matching the optionIfSupported wrapping above*)
conditionalKeysOf[$metric]={Dagger->(Real|Complex)};
$metric/:$metric[]:=Association[Options[$metric]];
$metric/:$metric[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$metric,Join[$metric[],Association@Options[DefOf[$metric]],<|Torsion->False,Curvature->True,SymCovDQ->True|>,Association[opts]],{PropKeysOf,OptKeysOf}]
]
$metric/:makeKey[$metric]=Head[#[Symbol]]&;


(* ::Input::Initialization:: *)
Options[$frame]={
Symbol->_Symbol[-_Symbol,_Symbol],
Metric->_Symbol[-_Symbol,-_Symbol],
Index->{__Symbol},
Expression->_,
$auto-><|_Symbol->$auto[]...|>,
PrintAs->({_String,_String}|{}),
ProtectNewSymbol:>(True|False)
};
$frame/:DefOf[$frame]=DefFrameBundle;
$frame/:UndefOf[$frame]=UndefFrameBundle;
$frame/:KeysOf[$frame]=Keys@Options[$frame];
$frame/:PropKeysOf[$frame]={Symbol,Metric,Index};
$frame/:ModKeysOf[$frame]={Expression,$auto};(*$auto was in KeysOf but classed
nowhere -- every $auto-carrying sibling lists it here; found by the Object Keys
docs contract, 2026-08-24. ModKeysOf is only consumed for $manifold, so this is
metadata, not behaviour.*)
$frame/:OptKeysOf[$frame]={PrintAs,ProtectNewSymbol};
$frame/:$frame[]:=Association[Options[$frame]];
$frame/:$frame[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$frame,Join[$frame[],Association@Options[DefOf[$frame]],Association[opts]],{PropKeysOf,OptKeysOf}]
]
$frame/:makeKey[$frame]=Head[#[Symbol]]&;


(* ::Input::Initialization:: *)
Options[$spinStructure]={
Metric->_Symbol,
Index->{__Symbol},
$auto-><|_Symbol->$auto[]...|>
};
$spinStructure/:DefOf[$spinStructure]=DefSpinStructure;
$spinStructure/:UndefOf[$spinStructure]=UndefSpinStructure;
$spinStructure/:KeysOf[$spinStructure]=Keys@Options[$spinStructure];
$spinStructure/:PropKeysOf[$spinStructure]={Metric,Index};
$spinStructure/:ModKeysOf[$spinStructure]={$auto};
$spinStructure/:OptKeysOf[$spinStructure]={};
$spinStructure/:$spinStructure[]:=Association[Options[$spinStructure]];
$spinStructure/:$spinStructure[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$spinStructure,Join[$spinStructure[],Association@Options[DefOf[$spinStructure]],Association[opts]],{PropKeysOf,OptKeysOf}]
]
$spinStructure/:makeKey[$spinStructure]=#[Metric]&;


(* ::Input::Initialization:: *)
Options[$spinConnection]={
Symbol->_Symbol[-_Symbol,-_Symbol,-_Symbol],
CovD->_Symbol,
$auto-><|_Symbol->$auto[]...|>,
PrintAs->(_String|Identity),
CurvatureRelations->(True|False),
ProtectNewSymbol:>(True|False)
};
$spinConnection/:DefOf[$spinConnection]=DefSpinConnection;
$spinConnection/:UndefOf[$spinConnection]=UndefSpinConnection;
$spinConnection/:KeysOf[$spinConnection]=Keys@Options[$spinConnection];
$spinConnection/:PropKeysOf[$spinConnection]={Symbol,CovD};
$spinConnection/:ModKeysOf[$spinConnection]={$auto};
$spinConnection/:OptKeysOf[$spinConnection]={PrintAs,CurvatureRelations,ProtectNewSymbol};
$spinConnection/:$spinConnection[]:=Association[Options[$spinConnection]];
$spinConnection/:$spinConnection[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$spinConnection,Join[$spinConnection[],Association@Options[DefOf[$spinConnection]],Association[opts]],{PropKeysOf,OptKeysOf}]
]
$spinConnection/:makeKey[$spinConnection]=Head[#[Symbol]]&;


(* ::Input::Initialization:: *)
Options[$bundle]={
Symbol->_Symbol,
Manifold->_Symbol,
Dimension->(_Integer?Positive|gaugeDim|internalDim),
Index->{__Symbol},
Metric->(_Symbol|None),
InvariantTraceTensor->({3,Antisymmetric}|{3,Symmetric}|{_Integer?Positive}|None),
$auto-><|_Symbol->$auto[]...|>,
Dagger->(Real|Complex),
Master->_Symbol,
PrintAs->(_String|Identity),
ProtectNewSymbol:>(True|False),
DefInfo->{_String,_String}
};
$bundle/:DefOf[$bundle]={DefVBundle,DefVBundleWithMetric};
$bundle/:UndefOf[$bundle]=((Quiet@Undef[#])&/@VisitorsOf[#];UndefVBundle[#])&;
$bundle/:KeysOf[$bundle]=Keys@Options[$bundle];
$bundle/:PropKeysOf[$bundle]={Symbol,Manifold,Dimension,Index,Metric};
$bundle/:ModKeysOf[$bundle]={$auto,InvariantTraceTensor};
$bundle/:OptKeysOf[$bundle]={Dagger,Master,PrintAs,ProtectNewSymbol,DefInfo};
$bundle/:$bundle[]:=Association[Options[$bundle]];
$bundle/:$bundle[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$bundle,Join[$bundle[],Association@Options[DefVBundle],Association[opts]],{PropKeysOf,OptKeysOf}]
]
$bundle/:makeKey[$bundle]=#[Symbol]&;


(* ::Input::Initialization:: *)
Options[$covd]={
Symbol->_Symbol[(_Symbol|-_Symbol)],
VBundle->{___Symbol},
SymbolOfCovD->{_String,_String},
$auto-><|_Symbol->$auto[]...|>,
Torsion->(True|False),
Curvature->(True|False),
FromMetric->_Symbol,
CurvatureRelations->(True|False),
ExtendedFrom->_Symbol,
OtherDependencies->{___Symbol},
OrthogonalTo->{_Symbol[(_Symbol|-_Symbol)]...},
ProjectedWith->{_Symbol[(_Symbol|-_Symbol),(_Symbol|-_Symbol)]...},
WeightedWithBasis->(_Symbol|Null),
ProtectNewSymbol:>(True|False),
Master->_Symbol,
DefInfo->{_String,_String},
SymCovDQ->(True|False)
};
$covd/:DefOf[$covd]=DefCovD;
$covd/:UndefOf[$covd]=UndefCovD;
$covd/:KeysOf[$covd]=Keys@Options[$covd];
$covd/:PropKeysOf[$covd]={Symbol,VBundle,SymbolOfCovD};
$covd/:ModKeysOf[$covd]={$auto};
$covd/:OptKeysOf[$covd]={Torsion,Curvature,FromMetric,CurvatureRelations,ExtendedFrom,OtherDependencies,OrthogonalTo,ProjectedWith,WeightedWithBasis,ProtectNewSymbol,Master,DefInfo,SymCovDQ};
$covd/:$covd[]:=Association[Options[$covd]];
$covd/:$covd[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$covd,Join[$covd[],Association@Options[DefOf[$covd]],Association[opts]],{PropKeysOf,OptKeysOf}]
]
$covd/:makeKey[$covd]=Head[#[Symbol]]&;


(* ::Input::Initialization:: *)
Options[$chart]={
Symbol->_Symbol,
Manifold->_Symbol,
CNumbersOf->{__Integer},
ScalarsOfChart->{_Symbol[]..},
Name->(_String|chart),
ChartColor->_RGBColor,
FormatBasis->Automatic(*needs attention later on*),
ProtectNewSymbol:>(True|False),
Dagger->(Real|Complex),
MetricInBasis->{}(*needs attention later on*),
epsilonOrientationOfMetric:>{_Symbol,(1|-1)},
ExtendedCoordinateDerivatives->(True|False),
DefInfo->{_String,_String}
};
$chart/:DefOf[$chart]=DefChart;
$chart/:UndefOf[$chart]=UndefChart;
$chart/:KeysOf[$chart]=Keys@Options[$chart];
$chart/:PropKeysOf[$chart]={Symbol,Manifold,CNumbersOf,ScalarsOfChart};
$chart/:ModKeysOf[$chart]={Name};
$chart/:OptKeysOf[$chart]={ChartColor,FormatBasis,ProtectNewSymbol,Dagger,MetricInBasis,epsilonOrientationOfMetric,ExtendedCoordinateDerivatives,DefInfo};
$chart/:$chart[]:=Association[Options[$chart]];
$chart/:$chart[opts:OptionsPattern[]]:=Module[{},
	ValidateObject[$chart,Join[$chart[],Association@Options[DefOf[$chart]],<|Name->chart|>,Association[opts]],{PropKeysOf,OptKeysOf}]
]
$chart/:makeKey[$chart]=#[Symbol]&;


(* ::Input::Initialization:: *)
Options[$basis]={
Symbol->_Symbol,
VBundle->_Symbol,
CNumbersOf->{__Integer},
BasisColor->_RGBColor,
FormatBasis->Automatic(*needs attention later on*),
ProtectNewSymbol:>(True|False),
Dagger->(Real|Complex),
ExtendedFrom->Null(*needs attention later on*),
MetricInBasis->{}(*needs attention later on*),
BasisChange->Null(*needs attention later on*),
epsilonOrientationOfMetric:>{_Symbol,(1|-1)},
DependenciesOfBasis->{___Symbol},
DefInfo->{_String,_String},
Master->_Symbol
};
$basis/:DefOf[$basis]=DefBasis;
$basis/:UndefOf[$basis]=UndefBasis;
$basis/:KeysOf[$basis]=Keys@Options[$basis];
$basis/:PropKeysOf[$basis]={Symbol,VBundle,CNumbersOf};
$basis/:ModKeysOf[$basis]={};
$basis/:OptKeysOf[$basis]={BasisColor,FormatBasis,ProtectNewSymbol,Dagger,ExtendedFrom,MetricInBasis,BasisChange,epsilonOrientationOfMetric,DependenciesOfBasis,DefInfo,Master};
$basis/:$basis[]:=Association[Options[$basis]];
$basis/:$basis[opts:OptionsPattern[]]:=Module[{},
	ValidateObject[$basis,Join[$basis[],Association@Options[DefOf[$basis]],Association[opts]],{PropKeysOf,OptKeysOf}]
]
$basis/:makeKey[$basis]=#[Symbol]&;


(* ::Input::Initialization:: *)
Options[$tensor]={
Symbol->_Symbol[(_Symbol|-_Symbol)...],
Manifold->({__Symbol}|_Symbol),
Symmetry->(Symmetric[({__Symbol,__}|{-__Symbol,__})]|Antisymmetric[({__Symbol,__}|{-__Symbol,__})]|GenSet[(Cycles[{(__Integer?Positive|__Symbol),__}]|-Cycles[{(__Integer?Positive|__Symbol),__}])...]|None),
$auto-><|_Symbol->$auto[]...|>,
Dagger->(Real|Complex),
Master->_Symbol,
PrintAs->(_String|Identity),
VanishingQ->(True|False),
ForceSymmetries->(True|False),
WeightOfTensor->(Times[_Integer,_Symbol]|0),
GradeOfTensor->({Rule[_Symbol,_Integer?Positive]..}|0),
FrobeniusQ->(True|False),
OrthogonalTo->{_Symbol[(_Symbol|-_Symbol)]...},
ProjectedWith->{_Symbol[(_Symbol|-_Symbol),(_Symbol|-_Symbol)]...},
ProtectNewSymbol:>(True|False),
DefInfo->{_String,_String},
TensorID->_,
KillingVectorOf->_Symbol
};
$tensor/:DefOf[$tensor]=DefTensor;
$tensor/:UndefOf[$tensor]=UndefTensor;
$tensor/:KeysOf[$tensor]=Keys@Options[$tensor];
$tensor/:PropKeysOf[$tensor]={Symbol,Manifold,Symmetry};
$tensor/:ModKeysOf[$tensor]={$auto};
$tensor/:OptKeysOf[$tensor]={Dagger,Master,PrintAs,VanishingQ,ForceSymmetries,WeightOfTensor,GradeOfTensor,FrobeniusQ,OrthogonalTo,ProjectedWith,ProtectNewSymbol,DefInfo,TensorID,KillingVectorOf};
$tensor/:$tensor[]:=Association[Options[$tensor]];
$tensor/:$tensor[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$tensor,Join[$tensor[],Association@Options[DefOf[$tensor]],Association[opts]],{PropKeysOf,OptKeysOf}]
]
$tensor/:makeKey[$tensor]=Head[#[Symbol]]&;


(* ::Input::Initialization:: *)
Options[$form]={
Symbol->_Symbol[(_Symbol|-_Symbol)...],
Manifold->({__Symbol}|_Symbol),
Deg->_Integer?Positive,
Symmetry->(Symmetric[({__Symbol,__}|{-__Symbol,__})]|Antisymmetric[({__Symbol,__}|{-__Symbol,__})]|GenSet[(Cycles[{(__Integer?Positive|__Symbol),__}]|-Cycles[{(__Integer?Positive|__Symbol),__}])...]|None),
Expression->_,
$auto-><|_Symbol->$auto[]...|>,
Dagger->(Real|Complex),
Master->_Symbol,
PrintAs->(_String|Identity),
VanishingQ->(True|False),
ForceSymmetries->(True|False),
WeightOfTensor->(Times[_Integer,_Symbol]|0),
GradeOfTensor->({Rule[_Symbol,_Integer?Positive]..}|0),
FrobeniusQ->(True|False),
OrthogonalTo->{_Symbol[(_Symbol|-_Symbol)]...},
ProjectedWith->{_Symbol[(_Symbol|-_Symbol),(_Symbol|-_Symbol)]...},
ProtectNewSymbol:>(True|False),
DefInfo->{_String,_String},
TensorID->_,
KillingVectorOf->_Symbol
};
$form/:DefOf[$form]=DefDiffForm;
$form/:UndefOf[$form]=UndefDiffForm;
$form/:KeysOf[$form]=Keys@Options[$form];
$form/:PropKeysOf[$form]={Symbol,Manifold,Deg,Symmetry};
$form/:ModKeysOf[$form]={Expression,$auto};
$form/:OptKeysOf[$form]={Dagger,Master,PrintAs,VanishingQ,ForceSymmetries,WeightOfTensor,GradeOfTensor,FrobeniusQ,OrthogonalTo,ProjectedWith,ProtectNewSymbol,DefInfo,TensorID,KillingVectorOf};
$form/:$form[]:=Association[Options[$form]];
$form/:$form[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$form,Join[$form[],Association@Options[DefOf[$form]],Association[opts]],{PropKeysOf,OptKeysOf}]
]
$form/:makeKey[$form]=Head[#[Symbol]]&;


(* ::Input::Initialization:: *)
Options[$spinor]={
Symbol->_Symbol[(_Symbol|-_Symbol)...],
Manifold->({__Symbol}|_Symbol),
Symmetry->(Symmetric[({__Symbol,__}|{-__Symbol,__})]|Antisymmetric[({__Symbol,__}|{-__Symbol,__})]|GenSet[(Cycles[{(__Integer?Positive|__Symbol),__}]|-Cycles[{(__Integer?Positive|__Symbol),__}])...]|None),
$auto-><|_Symbol->$auto[]...|>,
GradeOfTensor->({Rule[_Symbol,_Integer?Positive]..}|0),
SpinorType->(Dirac|Majorana),
Conjugate->(True|False)
};
$spinor/:DefOf[$spinor]=DefSpinor;
$spinor/:UndefOf[$spinor]=UndefSpinor;
$spinor/:KeysOf[$spinor]=Keys@Options[$spinor];
$spinor/:PropKeysOf[$spinor]={Symbol,Manifold,Symmetry};
$spinor/:ModKeysOf[$spinor]={$auto};
$spinor/:OptKeysOf[$spinor]={GradeOfTensor,SpinorType,Conjugate};
$spinor/:$spinor[]:=Association[Options[$spinor]];
$spinor/:$spinor[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$spinor,Join[$spinor[],<|GradeOfTensor->0|>,Association@Options[DefOf[$spinor]],Association[opts]],{PropKeysOf,OptKeysOf}]
]
$spinor/:makeKey[$spinor]=Head[#[Symbol]]&;


(* ::Input::Initialization:: *)
Options[$function]={
Symbol->_Symbol[___],
Expression->_,
Dagger->(Real|Complex),
Master->_Symbol,
PrintAs->(_String|Identity),
ProtectNewSymbol:>(True|False),
DefInfo->{_String,_String},
Validate->(True|False)
};
$function/:DefOf[$function]=DefScalarFunction;
$function/:UndefOf[$function]=UndefScalarFunction;
$function/:KeysOf[$function]=Keys@Options[$function];
$function/:PropKeysOf[$function]={Symbol};
$function/:ModKeysOf[$function]={Expression};
$function/:OptKeysOf[$function]={Dagger,Master,PrintAs,ProtectNewSymbol,DefInfo,Validate};
$function/:$function[]:=Association[Options[$function]];
$function/:$function[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$function,Join[$function[],Association@Options[DefOf[$function]],Association[opts]],{PropKeysOf,OptKeysOf}]
]
$function/:makeKey[$function]=Head[#[Symbol]]&;


(* ::Input::Initialization:: *)
Options[$constant]={
Symbol->_Symbol,
Dagger->(Real|Complex),
Master->_Symbol,
PrintAs->(_String|Identity),
ProtectNewSymbol:>(True|False),
DefInfo->{_String,_String},
Validate->(True|False)
};
$constant/:DefOf[$constant]=DefConstantSymbol;
$constant/:UndefOf[$constant]=UndefConstantSymbol;
$constant/:KeysOf[$constant]=Keys@Options[$constant];
$constant/:PropKeysOf[$constant]={Symbol};
$constant/:ModKeysOf[$constant]={};
$constant/:OptKeysOf[$constant]={Dagger,Master,PrintAs,ProtectNewSymbol,DefInfo,Validate};
$constant/:$constant[]:=Association[Options[$constant]];
$constant/:$constant[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$constant,Join[$constant[],Association@Options[DefOf[$constant]],Association[opts]],{PropKeysOf,OptKeysOf}]
]
$constant/:makeKey[$constant]=#[Symbol]&;


(* ::Input::Initialization:: *)
Options[$assumption]={
Symbol->_Symbol,
Expression->_Equal|_Unequal|_Less|_LessEqual|_Greater|_GreaterEqual|
    _And|_Or|_Not|_Element|_Exists|_ForAll|
    _Inequality|_Implies|_Equivalent|
    _List?(AllTrue[#,MatchQ[#,
        _Equal|_Unequal|_Less|_LessEqual|_Greater|_GreaterEqual|
        _And|_Or|_Not|_Element|_Exists|_ForAll|
        _Inequality|_Implies|_Equivalent|_List?(AllTrue[#,MatchQ[#,#]&]&)]&]&)
};
$assumption/:KeysOf[$assumption]=Keys@Options[$assumption];
$assumption/:PropKeysOf[$assumption]={Symbol,Expression};
$assumption/:ModKeysOf[$assumption]={};
$assumption/:OptKeysOf[$assumption]={};
$assumption/:$assumption[]:=Association[Options[$assumption]];
$assumption/:$assumption[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$assumption,Join[$assumption[],Association[opts]],{PropKeysOf,OptKeysOf}]
]
$assumption/:makeKey[$assumption]=#[Symbol]&;


(* ::Input::Initialization:: *)
Options[$rule]={
Symbol->_Symbol,
Expression->_
};
$rule/:KeysOf[$rule]=Keys@Options[$rule];
$rule/:PropKeysOf[$rule]={Symbol,Expression};
$rule/:ModKeysOf[$rule]={};
$rule/:OptKeysOf[$rule]={};
$rule/:$rule[]:=Association[Options[$rule]];
$rule/:$rule[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$rule,Join[$rule[],Association[opts]],{PropKeysOf,OptKeysOf}]
]
$rule/:makeKey[$rule]=#[Symbol]&;


(* ::Input::Initialization:: *)
Options[$set]={
Symbol->_Symbol,
Expression->{_HoldForm..}
};
$set/:KeysOf[$set]=Keys@Options[$set];
$set/:PropKeysOf[$set]={Symbol,Expression};
$set/:ModKeysOf[$set]={};
$set/:OptKeysOf[$set]={};
$set/:$set[]:=Association[Options[$set]];
$set/:$set[opts:OptionsPattern[]]:=Module[{},
ValidateObject[$set,Join[$set[],Association[opts]],{PropKeysOf,OptKeysOf}]
]
$set/:makeKey[$set]=#[Symbol]&;


(* ::Section:: *)
(*Unload, Load, IncludeTo, DropFrom, MergeWith, Instantiate*)


(* ::Input::Initialization:: *)
Attributes[attachTVs]={HoldFirst};
attachTVs[sol_[object_,key_]]:=Module[{valIDs,keys,values,autoKeys},
autoKeys=Keys@sol[object,key,$auto];
Do[
If[sol[object,key,$auto,autoKey,Value]=!=$auto[][Value],
valIDs=Keys@sol[object,key,$auto,autoKey,Value];
keys=Last[Level[#,1]]&/@valIDs;
values=Values@sol[object,key,$auto,autoKey,Value];

TensorValIDs[autoKey]^=valIDs;

Do[
Evaluate@autoKey/:TensorValues[autoKey,keys[[ii]]]=values[[ii]]
,{ii,1,Length@keys}
]
]
,{autoKey,autoKeys}
]
]


(* ::Input::Initialization:: *)
Attributes[makeTensorFromForm]={HoldFirst};
makeTensorFromForm[sol_,assoc_]:=Module[{
symbol=makeKey[$form][assoc],
symmetry=assoc[Symmetry],
numInd=Length@Level[assoc[Symbol],1],
ind=Level[assoc[Symbol],1],
deg=assoc[Deg],
mani=assoc[Manifold],
ssymmetry
},
ssymmetry=Which[
Head[#]===Symmetric,Last@Level[JoinSGS[Symmetric[First@Level[Symmetric[ind],1],Cycles],Antisymmetric[Range[numInd+1,numInd+deg],Cycles]],1],
Head[#]===Antisymmetric,Last@Level[JoinSGS[Antisymmetric[First@Level[Antisymmetric[ind],1],Cycles],Antisymmetric[Range[numInd+1,numInd+deg],Cycles]],1],
Head[#]===GenSet,Last@Level[JoinSGS[StrongGenSet[ind,symmetry],Antisymmetric[Range[numInd+1,numInd+deg],Cycles]],1],
#===None,Last@Level[Antisymmetric[Range[numInd+1,numInd+deg],Cycles],1]
]&@symmetry;
$tensor[Symbol->SymbolJoin["T",symbol]@@(Join[ind,-Take[sol[$manifold,mani,Index],deg]]),Manifold->assoc[Manifold],Symmetry->ssymmetry]
]


(* ::Input::Initialization:: *)
generatePermutations[deg_Integer,basis_,seed_]:=Module[
  {permutation,result,raiseIndex,tmp},
  (* Initialize the permutation with all -1s *)
  permutation=ConstantArray[-1,deg]*ConstantArray[1,deg];
  result={};
  
  (* Function to raise the first -1 to 1 in the permutation *)
  raiseIndex[idx_]:=Module[{perm=permutation},
    perm[[idx]]=1;
    perm
  ];

  (* Generate the sequence of permutations *)
  result=Append[result,seed->Permutations[permutation]];
  Do[
    permutation=raiseIndex[i];
tmp=Last@Values@result;
    result=Append[result,tmp->Permutations[permutation]],
    {i,deg}
  ];
  
  result
]


(* ::Input::Initialization:: *)
Attributes[include]={HoldFirst};
include[sol_,object_,expr_]:=Module[{
assoc=ValidateObject[object,expr,{PropKeysOf,OptKeysOf}]
},
AppendTo[sol[object],<|makeKey[object][assoc]->assoc|>];

Which[
object===$metric&&assoc[InducedFrom]=!=Null,
Module[{symbol,index,mani,tensorAssoc},
symbol=assoc[InducedFrom][[2]];
(*index=Map[If[Head[#]===Times&&First[#]===-1,Times@@Rest[#],#]&,First@Level[assoc[Symbol],1],All];*)
index=Abs[First@Level[assoc[Symbol],1]]/.Abs[talasam_]:>talasam;
mani=SelectFirst[Keys[sol[$manifold]],MemberQ[sol[$manifold,#,Index],index]&];
tensorAssoc=$tensor[Symbol->symbol[index],Manifold->mani,Symmetry->None];
sol[$tensor]=Merge[{sol[$tensor],<|makeKey[$tensor][tensorAssoc]->tensorAssoc|>},Last];
],

object===$form,
sol[$tensor]=Merge[{sol[$tensor],<|SymbolJoin["T",makeKey[$form][assoc]]->makeTensorFromForm[sol,assoc]|>},Last]
];
]


(* ::Input::Initialization:: *)
Attributes[drop]={HoldFirst};
(*The key arrives as a NAME,not as a symbol:unload Removes the symbol and Revive then rebinds the stored key to a fresh symbol of the same name,so the symbol DropFrom was handed no longer matches the key held in sol.The name survives that;the symbol does not.*)
drop[sol_,object_,name_String]:=Module[{
keys=Select[Keys[sol[object]],Head[#]===Symbol&&SymbolName[#]===name&]
},
If[keys=!={},
sol[object]=KeyDrop[sol[object],keys];
,
Return[Missing["KeyAbsent",name]];
]
]


(* ::Input::Initialization:: *)
Attributes[load]={HoldFirst};
load[sol_,object_,key_]:=Module[{
assoc=Lookup[sol[object],key],
tensorSymbols,
attempt,
oldSymbols,
newSymbols,
loaded={}
},
If[AssociationQ[assoc],
ValidateObject[object,assoc,{PropKeysOf,OptKeysOf}];

(*preload tensors if needed*)
Which[
object===$metric&&assoc[InducedFrom]=!=Null&&!xTensorQ[assoc[InducedFrom][[2]]],
Module[{symbol=assoc[InducedFrom][[2]],tensorAssoc},
tensorAssoc=sol[$tensor,symbol];
oldSymbols=xActSymbols[];
attempt=Check[DefOf[$tensor]@@Join[DeleteCases[None][Lookup[tensorAssoc,PropKeysOf[$tensor]]],Normal@KeySelect[tensorAssoc,MemberQ[OptKeysOf[$tensor],#]&]],$Failed];newSymbols=xActSymbols[];If[attempt=!=$Failed,
loaded=Flatten@Append[loaded,Select[newSymbols,!MemberQ[oldSymbols,#]&]];
tensorSymbols=Select[loaded,xTensorQ[#]&];
If[sol[$tensor,symbol,$auto]===<|_Symbol->$auto[]...|>,
sol[$tensor,symbol,$auto]=AssociationThread[tensorSymbols->ConstantArray[$auto[],Length@tensorSymbols]];
,
sol[$tensor,symbol,$auto]=Merge[{sol[$tensor,symbol,$auto],AssociationThread[tensorSymbols->ConstantArray[$auto[],Length@tensorSymbols]]},First];
];
attachTVs[sol[$tensor,symbol]];
,Throw[attempt]
]
],

object===$form&&!xTensorQ[SymbolJoin["T",key]],
Module[{symbol=SymbolJoin["T",key],tensorAssoc},
tensorAssoc=sol[$tensor,symbol];
oldSymbols=xActSymbols[];attempt=Check[DefOf[$tensor]@@Join[DeleteCases[None][Lookup[tensorAssoc,PropKeysOf[$tensor]]],Normal@KeySelect[tensorAssoc,MemberQ[OptKeysOf[$tensor],#]&]],$Failed];newSymbols=xActSymbols[];
If[attempt=!=$Failed,
loaded=Flatten@Append[loaded,Select[newSymbols,!MemberQ[oldSymbols,#]&]];
tensorSymbols=Select[loaded,xTensorQ[#]&];
If[sol[$tensor,symbol,$auto]===<|_Symbol->$auto[]...|>,
sol[$tensor,symbol,$auto]=AssociationThread[tensorSymbols->ConstantArray[$auto[],Length@tensorSymbols]];
,
sol[$tensor,symbol,$auto]=Merge[{sol[$tensor,symbol,$auto],AssociationThread[tensorSymbols->ConstantArray[$auto[],Length@tensorSymbols]]},First];
];
attachTVs[sol[$tensor,symbol]];
,Throw[attempt]
]
]
];

(*attempt loading*)
oldSymbols=xActSymbols[];
Which[
object===$bundle,
If[sol[object,key,Metric]===None,
attempt=Check[DefOf[object][[1]]@@Join[DeleteCases[None][Lookup[assoc,PropKeysOf[object]]],Normal@KeySelect[assoc,MemberQ[OptKeysOf[object],#]&]],$Failed];If[sol[object,key,InvariantTraceTensor]=!=None,InvariantTraceTensor@@Join[{sol[object,key,Symbol]},sol[object,key,InvariantTraceTensor]];];
,
attempt=Check[DefOf[object][[2]]@@Join[DeleteCases[None][Lookup[assoc,PropKeysOf[object]]],Normal@KeySelect[assoc,MemberQ[OptKeysOf[object],#]&]],$Failed];If[sol[object,key,InvariantTraceTensor]=!=None,InvariantTraceTensor@@Join[{sol[object,key,Symbol]},sol[object,key,InvariantTraceTensor]];];
];,

object===$tensor&&xTensorQ[key],
attempt=$Failed;,

object===$function,
attempt=Check[DefOf[object]@@Join[DeleteCases[None][Head[#]&/@Lookup[assoc,PropKeysOf[object]]],Normal@KeySelect[assoc,MemberQ[OptKeysOf[object],#]&]],$Failed];
If[Level[sol[object,key,Symbol],1]=!={},
key[]:=sol[object,key,Symbol];
Derivative[talasam__][key][]:=Derivative[talasam][key]@@Level[sol[$function,key,Symbol],1];
];,

object===$assumption,
$Assumptions=sol[object,key,Expression];
loaded=Flatten@Append[loaded,HoldForm[$Assumptions]];,

object===$set,
ReleaseHold[#]&/@sol[object,key,Expression];,

object===$chart,
attempt=Check[DefOf[object]@@Join[DeleteCases[None][Lookup[assoc,PropKeysOf[object]]],Normal@KeySelect[assoc,MemberQ[OptKeysOf[object],#]&]],$Failed];
SymbolJoin["ChristoffelPD",key][__]:=0;
Do[ToBasis[key][SymbolJoin["Det",metric,key]];,{metric,MetricsOfVBundle@VBundleOfBasis@key}];
Do[Evaluate[SymbolJoin["Det",metric,key]]:=Evaluate[SymbolJoin["Det",metric]];,{metric,MetricsOfVBundle@VBundleOfBasis@key}];
Wedge[args__]/;(
With[{elems=Select[{args},MemberQ[Diff[#]&/@ScalarsOfChart[key],#]&]},
Length[elems]>Length[DeleteDuplicates[elems]]
]
):=0;,

object===$basis,
attempt=Check[DefOf[object]@@Join[DeleteCases[None][Lookup[assoc,PropKeysOf[object]]],Normal@KeySelect[assoc,MemberQ[OptKeysOf[object],#]&]],$Failed];
SymbolJoin["AChristoffelPD",key][__]:=0;,

True,
attempt=Check[DefOf[object]@@Join[DeleteCases[None][Lookup[assoc,PropKeysOf[object]]],Normal@KeySelect[assoc,MemberQ[OptKeysOf[object],#]&]],$Failed];
];
newSymbols=xActSymbols[];
If[attempt=!=$Failed,loaded=Flatten@Append[loaded,Select[newSymbols,!MemberQ[oldSymbols,#]&]];,Throw[attempt]];

(*special code for after loading a chart*)
(*If[object===$chart,
Module[{metrics=MetricsOfVBundle@VBundleOfBasis@key},
Do[
sol[$metric,metric,$auto]=Merge[{sol[$metric,metric,$auto],<|SymbolJoin["Det",metric,key]->$auto[]|>},First];
,{metric,metrics}
]
];
];*)

(*after loading populate auto tensors*)
If[MemberQ[{$metric,$frame,$spinStructure,$spinConnection,$bundle,$covd,$tensor,$form,$spinor},object],
(*make the tensorSymbols*)
Which[
object===$frame,
Module[{index,mani},
index=If[Head[First@Level[assoc[Symbol],1]]===Times,Level[First@Level[assoc[Symbol],1],1][[2]],First@Level[assoc[Symbol],1]];mani=SelectFirst[Keys[sol[$manifold]],MemberQ[sol[$manifold,#,Index],index]&];tensorSymbols=Join[{Coframe[mani]},Select[loaded,xTensorQ[#]&]];
];,

object===$form,
tensorSymbols=Select[loaded,(xTensorQ[#]&&GradeOfTensor[#,Wedge]=!=0)&];,

True,
tensorSymbols=Select[loaded,xTensorQ[#]&];
];
(*usual code*)
If[sol[object,key,$auto]===<|_Symbol->$auto[]...|>,
sol[object,key,$auto]=AssociationThread[tensorSymbols->ConstantArray[$auto[],Length@tensorSymbols]];
,
sol[object,key,$auto]=Merge[{sol[object,key,$auto],AssociationThread[tensorSymbols->ConstantArray[$auto[],Length@tensorSymbols]]},First];
];
(*further modify the tensor if form NEED TO RE-WRITE THIS*)
If[object===$form&&(*ChartsOfManifold@sol[object,key,Manifold]=!={}&&*)Level[sol[object,key,Symbol],1]==={},
Module[{(*basis=BasisOfVBundle@TangentBundleOfManifold@sol[object,key,Manifold]*)
deg=sol[object,key,Deg],
seed=HoldForm[MakeArray[$self[$form,key,Expression]/.Gen@$self[$rule,$form]]],
basis
},
If[sol[$tensor,SymbolJoin["T",key],$auto,SymbolJoin["T",key],Routine,Chain]===$routine[][Chain],
sol[$tensor,SymbolJoin["T",key],$auto,SymbolJoin["T",key],Routine,Chain]=generatePermutations[deg,basis,seed];
];
];
];

attachTVs[sol[object,key]];
];
(*every successful entry-load accumulates WHAT became live, in load order,
because the banner content only exists as these runtime diffs; never
reset -- only a kernel quit resets it. This list is the single record of
kernel liveness: ResumeAs decides on emptiness and rebuilds the Load
banner from the content.*)
$loadedSymbols=DeleteDuplicates@Join[$loadedSymbols,Select[loaded,#=!=$Failed&&Head[#]=!=Missing&]];
Return[loaded];
,
Return[assoc]
];
]


(* ::Input::Initialization:: *)
Attributes[unload]={HoldFirst};
unload[sol_,object_,key_]:=Module[{
assoc=Lookup[sol[object],key],
attempt,
oldSymbols,
newSymbols,
loaded={},
babaroga
},
If[AssociationQ[assoc],
oldSymbols=xActSymbols[];
Which[
object===$assumption,
If[sol[object,key,Value]===$Assumptions,
$Assumptions=True;
loaded=Flatten@Append[loaded,HoldForm[$Assumptions]];
];
,
object===$set,
Quiet@ReleaseHold[sol[$set,key,Expression]/.Set->babaroga/.SetDelayed->babaroga/.IndexSet->babaroga/.IndexSetDelayed->babaroga/.babaroga[talasam_,_]:>Unset[talasam]];,

True,
attempt=Check[UndefOf[object][makeKey[object][sol[object,key]]],$Failed];
];
Revive[sol];
newSymbols=xActSymbols[];
loaded=Flatten@Append[loaded,Select[oldSymbols//(Uncompress[Compress[#]]/.Removed[name_]:>Symbol[name])&,!MemberQ[newSymbols,#]&]];
If[attempt===$Failed,Throw[attempt]];
Return[loaded];
,
Return[assoc];
];
]


(* ::Input::Initialization:: *)
(*Everything the kernel has loaded, in load order -- accumulated by the
private load worker and never reset; a kernel quit is the reset. The
single record of kernel liveness: ResumeAs decides on emptiness and
rebuilds the Load banner from the content.*)
$loadedSymbols={};
makeSymbolPanel[symbolNames_List,heading_]:=DynamicModule[{
names=symbolNames,
info="",
showInfo=False,
currentSymbol=""
},
Panel[
Column[{
heading,
Button[
Style[#,FontFamily->"Source Code Pro",FontWeight->"SemiBold",Darker@Blue],
If[currentSymbol===#&&showInfo,
showInfo=False,
currentSymbol=#;
info=If[#===HoldForm[$Assumptions],$Assumptions,Information[#]];
showInfo=True
],ImageMargins->2,Appearance->"Frameless"]&/@names//Row[#," , "]&,
Dynamic[If[showInfo,info,"Info[...]"]]
},Spacings->1],ImageSize->Full]
]


(* ::Input::Initialization:: *)
Attributes[Unload]={HoldFirst};
Attributes[Load]={HoldFirst};
Attributes[IncludeTo]={HoldFirst};
Attributes[DropFrom]={HoldFirst};
Attributes[MergeWith]={HoldFirst};
MergeWith::duplicatesymbols="Symbols: `1` already appear in `2`";
MergeWith::duplicatekeys="Keys: `1` already appear in `2`[`3`]";
Attributes[hook]={HoldFirst};
Attributes[Instantiate]={HoldFirst};
hook[sol_]:=Module[{},
sol/:Unload[sol[object_],key_Symbol]:=Module[{
loaded={}
},
loaded=Flatten@Append[loaded,Catch@unload[sol,object,key]];
loaded=Select[loaded,#=!=$Failed&&Head[#]=!=Missing&];
If[loaded=!={},Return[makeSymbolPanel[loaded,"Unloaded:"]];];
];
sol/:Unload[sol[object_],keys_List/;AllTrue[keys,Head[#]===Symbol&]]:=Module[{
loaded={}
},
Do[
loaded=Flatten@Append[loaded,Catch@unload[sol,object,key]];
,{key,keys}
];
loaded=Select[loaded,#=!=$Failed&&Head[#]=!=Missing&];
If[loaded=!={},Return[makeSymbolPanel[loaded,"Unloaded:"]];];
];
sol/:Load[sol[object_],key_Symbol]:=Module[{
loaded={}
},
Quiet@Catch@Unload[sol[object],key];
loaded=Flatten@Append[loaded,Catch@load[sol,object,key]];
loaded=Select[loaded,#=!=$Failed&&Head[#]=!=Missing&];
If[loaded=!={},Return[makeSymbolPanel[loaded,"Loaded:"]];];
];
sol/:Load[sol[object_],keys_List/;AllTrue[keys,Head[#]===Symbol&]]:=Module[{
loaded={}
},
Quiet@Catch@Unload[sol[object],Reverse@keys];
Do[
loaded=Flatten@Append[loaded,Catch@load[sol,object,key]];
,{key,keys}
];
loaded=Select[loaded,#=!=$Failed&&Head[#]=!=Missing&];
If[loaded=!={},Return[makeSymbolPanel[loaded,"Loaded:"]];];
];
sol/:IncludeTo[sol[object_],expr_Association]:=Module[{
assoc=evaluateInHeld[object@@Normal[expr]],
key=makeKey[object][expr],
loaded={}
},
Quiet@Catch@Unload[sol[object],key];
include[sol,object,assoc];
loaded=Flatten@Append[loaded,Catch@load[sol,object,key]];
loaded=Select[loaded,#=!=$Failed&&Head[#]=!=Missing&];
If[loaded=!={},Return[makeSymbolPanel[loaded,"Loaded:"]];];
];
sol/:IncludeTo[sol[object_],exprs_List/;AllTrue[exprs,AssociationQ]]:=Module[{
assocs=evaluateInHeld[object@@Normal[#]]&/@exprs,
keys=makeKey[object][#]&/@exprs,
loaded={}
},
Quiet@Catch@Unload[sol[object],Reverse@keys];
Do[
include[sol,object,assocs[[ii]]];
loaded=Flatten@Append[loaded,Catch@load[sol,object,keys[[ii]]]];
,{ii,1,Length@keys}
];
loaded=Select[loaded,#=!=$Failed&&Head[#]=!=Missing&];
If[loaded=!={},Return[makeSymbolPanel[loaded,"Loaded:"]];];
];
sol/:DropFrom[sol[object_],key_Symbol]:=Module[{
loaded={},
name=SymbolName[key]
},
loaded=Flatten@Append[loaded,Quiet@Catch@unload[sol,object,key]];
drop[sol,object,name];
loaded=Select[loaded,#=!=$Failed&&Head[#]=!=Missing&];
If[loaded=!={},Return[makeSymbolPanel[loaded,"Unloaded:"]];];
];
(*The names are taken before the loop,not inside it:UndefOf[$manifold] undefines its visitors first,so one unload can Remove symbols still sitting later in keys.*)
sol/:DropFrom[sol[object_],keys_List/;AllTrue[keys,Head[#]===Symbol&]]:=Module[{
loaded={},
names=SymbolName/@keys
},
Do[
loaded=Flatten@Append[loaded,Quiet@Catch@unload[sol,object,keys[[ii]]]];
drop[sol,object,names[[ii]]];
,{ii,1,Length@keys}
];
loaded=Select[loaded,#=!=$Failed&&Head[#]=!=Missing&];
If[loaded=!={},Return[makeSymbolPanel[loaded,"Unloaded:"]];];
];
sol/:Unload[sol]:=Module[{
loaded={}
},
ValidateObject[$solution,sol,{PropKeysOf,OptKeysOf}];
Do[
loaded=Flatten@Append[loaded,Catch@unload[sol,object,key]];
,{object,Reverse@$Objects},{key,Reverse@Keys[sol[object]]}
];
loaded=Select[loaded,#=!=$Failed&&Head[#]=!=Missing&];
If[loaded=!={},Return[makeSymbolPanel[loaded,"Unloaded:"]];];
];
sol/:Load[sol]:=Module[{
loaded={},metrics
},
ValidateObject[$solution,sol,{PropKeysOf,OptKeysOf}];
(*Quiet@Unload[sol];*)
Do[
loaded=Flatten@Append[loaded,Catch@load[sol,object,key]];
,{object,$Objects},{key,Keys[sol[object]]}
];
loaded=Select[loaded,#=!=$Failed&&Head[#]=!=Missing&];
If[loaded=!={},
(*legacy tolerance: records saved before the Grade guard (2026-08-20)
store the metric Expression HoldForm-wrapped; ReleaseHold is the
identity on today's naked records*)
metrics=Keys@sol[$metric];
Do[
sol[$metric,metric,Expression]=ReleaseHold[sol[$metric,metric,Expression]];
,{metric,metrics}
];
Return[makeSymbolPanel[loaded,"Loaded:"]];
];
];
sol/:MergeWith[sol,assoc_]:=Module[{
validAssoc,
intersectionSymbols,
intersectionKeys,
assocs,
keys,
loaded={}
},
validAssoc=ValidateObject[$solution,assoc,{PropKeysOf,OptKeysOf}];
intersectionSymbols=Intersection[extractAllSymbols[sol],extractAllSymbols[validAssoc]];
If[intersectionSymbols=!={},Message[MergeWith::duplicatesymbols,intersectionSymbols,ToString[Unevaluated[sol]]];Throw[$Failed]];
Do[
intersectionKeys=Intersection[Keys@sol[obj],Keys@validAssoc[obj]];
If[intersectionKeys=!={},
Message[MergeWith::duplicatekeys,intersectionKeys,ToString[Unevaluated[sol]],obj];Throw[$Failed]
];
,{obj,$Objects}
];
Do[
assocs=obj@@Normal[#]&/@Values[validAssoc[obj]];
keys=makeKey[obj][#]&/@Values[validAssoc[obj]];
Do[
include[sol,obj,assocs[[ii]]];
loaded=Flatten@Append[loaded,Catch@load[sol,obj,keys[[ii]]]];
,{ii,1,Length@keys}
];
,{obj,$Objects}
];
loaded=Select[loaded,#=!=$Failed&&Head[#]=!=Missing&];
If[loaded=!={},Return[makeSymbolPanel[loaded,"Loaded:"]];];
];
]
Instantiate[sol_]:=Module[{},
ClearAll[Unevaluated@sol];
sol=AssociationThread[$Objects->ConstantArray[<||>,Length@$Objects]];
hook[sol];
]
Instantiate[sol_,assoc_]:=Module[{},
Instantiate[sol];
MergeWith[sol,assoc]
]
Load[sol_]:=Module[{},
ValidateObject[$solution,sol,{PropKeysOf,OptKeysOf}];
hook[sol];
Load@sol
]


(* ::Section:: *)
(*Metric and Forms: back and forth*)


(* ::Input::Initialization:: *)
Attributes[makeMetricArray]={HoldFirst};
makeMetricArray[chart_][expr_]:=Module[{
coord=ScalarsOfChart[chart],
dim,
coeff
},
dim=Length@coord;
coeff=Table[Diff[coord[[ii]]]\[CircleTimes]Diff[coord[[jj]]],{ii,1,dim},{jj,1,dim}];
Return[1/2 (Coefficient[expr,coeff]+Transpose@Coefficient[expr,coeff])]
]
makeMetricArray[sol_[$metric,key_]]:=Module[{
mani,
dim,
chart,
coord,
coeff,
expr
},
mani=MasterOf@VBundleOfMetric@key;
dim=DimOfVBundle@VBundleOfMetric@key;
chart=SelectFirst[$Charts,VBundleOfBasis[#]===VBundleOfMetric[key]&];
coord=ScalarsOfChart[chart];
coeff=Table[Diff[coord[[ii]]]\[CircleTimes]Diff[coord[[jj]]],{ii,1,dim},{jj,1,dim}];
expr=sol[$metric,key,Expression];
Return[1/2 (Coefficient[expr,coeff]+Transpose@Coefficient[expr,coeff])]
]


(* ::Input::Initialization:: *)
dcoeff[1,xc_]:=(Diff[#]&/@xc)
dcoeff[2,xc_]:=Map[Wedge[#,Diff[#]&/@xc]&,dcoeff[1,xc],{1}]
dcoeff[n_,xc_]:=If[EvenQ[n],Map[Wedge[#,Diff[#]&/@xc]&,dcoeff[n-1,xc],{n-1}],Map[Wedge[Diff[#]&/@xc,#]&,dcoeff[n-1,xc],{n-1}]]


(* ::Input::Initialization:: *)
Attributes[makeFormArray]={HoldFirst};
makeFormArray[chart_][expr_]:=Module[{
coord=ScalarsOfChart[chart],
dim,
coeff,
deg=Deg[expr],
ind,
tb
},
dim=Length@coord;
ind=Take[First@IndicesOfVBundle@VBundleOfBasis[chart],deg];
If[deg===1,
tb=Coefficient[expr,dcoeff[deg,coord]];
tb=CTensor[tb,ConstantArray[-chart,deg]];
Return[(tb@@(-ind[[#]]&/@Range[1,deg]))[[0,1]]]
];
If[deg>1,
Off[Coefficient::ivar];
tb=Coefficient[expr,dcoeff[deg,coord]];
On[Coefficient::ivar];
tb=CTensor[tb,ConstantArray[-chart,deg]];
Return[deg!Antisymmetrize[(tb@@(-ind[[#]]&/@Range[1,deg])),(-ind[[#]]&/@Range[1,deg])][[0,1]]]
];
]
makeFormArray[sol_[$form,key_]]:=Module[{
mani,
dim,
deg,
chart,
coord,
coeff,
expr,
tb,
ind
},
mani=sol[$form,key,Manifold];
dim=DimOfManifold[mani];
deg=sol[$form,key,Deg];
chart=SelectFirst[$Charts,MappingDomain[#]===mani&];
coord=ScalarsOfChart[chart];
expr=sol[$form,key,Expression];
ind=sol[$manifold,mani,Index];
If[deg===1,
tb=Coefficient[expr,dcoeff[deg,coord]];
tb=CTensor[tb,ConstantArray[-chart,deg]];
Return[(tb@@(-ind[[#]]&/@Range[1,deg]))[[0,1]]]
];
If[deg>1,
tb=Coefficient[expr,dcoeff[deg,coord]];
tb=CTensor[tb,ConstantArray[-chart,deg]];
Return[deg!Antisymmetrize[(tb@@(-ind[[#]]&/@Range[1,deg])),(-ind[[#]]&/@Range[1,deg])][[0,1]]]
];
]


(* ::Input::Initialization:: *)
(*Attributes[MakeArray]={HoldFirst};*)
(*MakeArray[chart_][expr_]:=Module[{
deg=Deg[expr]
},
Which[
deg===0,makeMetricArray[chart][expr],
deg>0,makeFormArray[chart][expr]
]
]*)
(*MakeArray[sol_[object_,key_]]:=Module[{},
Which[
object===$metric,Return[makeMetricArray[sol[object,key]]];,
object===$form,Return[makeFormArray[sol[object,key]]];,
object===$frame,Module[{chart=BasisOfVBundle@SelectFirst[HostsOf[key],VBundleQ[#]&]},
Return[makeFormArray[chart][#]&/@(sol[$frame,key,Expression])];
],
True,Message[MakeArray::invalidobject,{$metric,$form}];
]
]*)
MakeArray::invalidobject="Arrays can be made only from: `1`";
MakeArray::nodiffs="No Diff's found in `1`";
MakeArray::nochart="No chart could be extracted from `1`";
MakeArray[expr_]:=withoutGradeGuard@Module[{string=ToString[expr]},
If[StringContainsQ[string,"\[CircleTimes]"],(*its a metric*)
If[StringContainsQ[string,"Diff"],
Module[{maybeCoordinatesFound,maybeChartList},
maybeCoordinatesFound=(StringCases[string,"Diff["~~Shortest[___]~~"[], PD]"]//ToExpression//DeleteDuplicates)/.Diff[x_,PD]:>Identity[x];
maybeChartList=ChartOfScalar[#]&/@maybeCoordinatesFound//DeleteCases[Null];
If[maybeChartList=!={},
Return[makeMetricArray[First@maybeChartList][expr]]
,
Message[MakeArray::nochart,expr];Throw[$Failed];
];
]
,
Message[MakeArray::nodiffs,expr];Throw[$Failed];
];
,(*its a form*)
If[StringContainsQ[string,"Diff"],
Module[{maybeCoordinatesFound,maybeChartList},
maybeCoordinatesFound=(StringCases[string,"Diff["~~Shortest[___]~~"[], PD]"]//ToExpression//DeleteDuplicates)/.Diff[x_,PD]:>Identity[x];
maybeChartList=ChartOfScalar[#]&/@maybeCoordinatesFound//DeleteCases[Null];
If[maybeChartList=!={},
Return[makeFormArray[First@maybeChartList][expr]]
,
Message[MakeArray::nochart,expr];Throw[$Failed];
];
]
,
Message[MakeArray::nodiffs,expr];Throw[$Failed];
];
]
]


(* ::Input::Initialization:: *)
MakeMetric::invaliddimension="The Dimensions of the argument (`1`) should instead be `2`";
MakeMetric[chart_][array_]:=Module[{
coord
},
coord=ScalarsOfChart[chart];
If[Dimensions[array]=!=ConstantArray[Length@coord,2],Message[MakeMetric::invaliddimension,Dimensions[array],HoldForm[ConstantArray[DimOfVBundle@VBundleOfBasis@chart,2]]];Throw[$Failed]];
Sum[array[[ii,jj]]Diff[coord[[ii]]]\[CircleTimes]Diff[coord[[jj]]],{ii,1,Length@coord},{jj,1,Length@coord}]
]


(* ::Input::Initialization:: *)
MakeForm[chart_][array_]:=Module[{coord,dim,deg,indices,coeffs,forms},
coord=ScalarsOfChart[chart];
dim=Length@coord;
If[Length@DeleteDuplicates@Dimensions[array]=!=1&&First@DeleteDuplicates@Dimensions[array]=!=dim,Throw[MakeForm]];
deg=Length@Dimensions@array;
indices=Tuples[Range[dim],deg];
coeffs=Flatten[Table[array[[Sequence@@idx]],{idx,indices}]];
forms=Map[Wedge@@(Diff[coord[[#]]]&/@#)&,indices];
1/deg! Total[MapThread[Times,{coeffs,forms}]]
]


(* ::Input::Initialization:: *)
Attributes[ExpandForms]={HoldFirst};
ExpandForms[sol_][expr_]:=Module[{
keys=Keys@sol[$form],
form,names,deg,mani,ind,dxes,rule,
firstMani=First@Keys@sol[$manifold],
covd=If[Length@Keys@sol[$metric]>0,sol[$metric,First@Keys@sol[$metric],CovD],PD]
},
rule={};
Do[
form=sol[$form,key,Symbol];
(* the pattern names are the stored slot indices: registered indices of the right vbundle, so the rule's right side assembles cleanly; each slot binds whole (sign included), so any index instance matches *)
names=Level[form,1]/.-ii_Symbol:>ii;
deg=sol[$form,key,Deg];
mani=sol[$form,key,Manifold];
ind=sol[$manifold,mani,Index][[1;;deg]];
dxes=Table[dx[mani][ind[[ii]]],{ii,1,Length@ind}]/.List->Wedge;
AppendTo[rule,IndexRule[Head[form]@@(Pattern[#,Blank[]]&/@names),1/deg! SymbolJoin["T",Head@form]@@Join[names,-ind]dxes]]
,{key,keys}
];
(* Diff of any degree-zero expression expands in the same sweep; ambiguities resolve to the first manifold and first metric's CovD *)
AppendTo[rule,With[{ii=First@sol[$manifold,firstMani,Index]},IndexRuleDelayed[Diff[ee_],covd[-ii][ee]dx[firstMani][ii]/;Deg[ee]===0]]];
expr//.rule
]


(* ::Input::Initialization:: *)
WedgeCoeff::inhom="The expression is not a form of homogeneous integer degree (Deg gave `1`).";
WedgeCoeff::nometric="A metric is needed (to raise dx indices or to build the epsilon contraction) but `1` stores none.";
WedgeCoeff::wedge="A wedge monomial of `1` differential(s) found in an expression of degree `2`.";
WedgeCoeff::unexpanded="Content other than wedges of coordinate differentials remains; expand all forms and Hodge duals first (ExpandForms, ExpandHodgeDual).";
Attributes[WedgeCoeff]={HoldFirst};
WedgeCoeff[sol_][expr_]:=Module[{
mani=First@Keys@sol[$manifold],
indices,dim,met,deg,ex,avoid,avail,cand,targets,pair,finalize
},
indices=sol[$manifold,mani,Index];
dim=sol[$manifold,mani,Dimension];
deg=Catch[Deg[expr]];
If[!IntegerQ[deg]||deg<0||deg>dim,Message[WedgeCoeff::inhom,deg];Throw[$Failed]];
If[deg===0,Return[expr]];
met=If[Length@Keys@sol[$metric]>0,Head@sol[$metric,First@Keys@sol[$metric],Symbol],None];
(* the star trick, done as the star trick: a form above half the dimension is
   Hodge-dualized and expanded first, and the plain extraction then runs on the
   lower-degree dual. The result is the component tensor of the Hodge dual of
   expr; reconstructing expr from it costs the double-dual sign
   (-1)^(p(d-p)) sign(det g) -- that sign is mathematics, not a convention. *)
(* present the result in the order of the manifold's Index list: the frees
   introduced by the extraction take the first names not already free in the
   expression, and ScreenDollarIndices then renames every dummy to the first
   unused registered indices -- frees first, dummies later *)
finalize[res_,fr_List]:=Module[{rr=ReplaceDummies[res],new},
(* after ReplaceDummies the names in fr occur only as the extraction's frees,
   so a plain simultaneous substitution renames them exactly (ReplaceIndex
   silently ignores sums, so it is of no use here) *)
new=Select[indices,FreeQ[rr,#]||MemberQ[fr,#]&];
If[Length@new>=Length@fr,rr=rr/.Thread[fr->Take[new,Length@fr]]];
ScreenDollarIndices[rr]];
ex=If[2 deg>dim,deg=dim-deg;Hodge[met][expr],expr];
If[met===None&&!(FreeQ[ex,Hodge]&&FreeQ[ex,dx[mani][-_Symbol]]),
Message[WedgeCoeff::nometric,HoldForm[sol]];Throw[$Failed]];
If[!FreeQ[ex,Hodge],ex=ExpandHodgeDual[ex,dx[mani],met]];
If[deg===0,Return[finalize[ContractMetric[ex],{}]]];
ex=ReplaceDummies[ex,IndexList@@indices];
(* free indices of the result: the first registered tangent indices not present
   in the expression; when the registered list is exhausted, new indices are
   generated and registered (NewIndexIn: last user index plus a number), so the
   extraction always succeeds. The raise dummy needs no reservation:
   IndexRuleDelayed mints a fresh variant per application. *)
avoid=Select[indices,!FreeQ[ex,#]&];
avail=Select[indices,FreeQ[ex,#]&];
While[Length@avail<deg,
cand=GetIndicesOfVBundle[SymbolJoin["Tangent",mani],deg-Length@avail,Join[avail,avoid]];
Do[If[FreeQ[ex,ii],AppendTo[avail,ii],AppendTo[avoid,ii]],{ii,cand}]
];
targets=Take[avail,deg];
If[!FreeQ[ex,dx[mani][-_Symbol]],
ex=ex/.With[{kk=Last@indices,gsym=met},IndexRuleDelayed[dx[mani][-ii_Symbol],gsym[-ii,-kk]dx[mani][kk]]]];
pair[is_List]:=If[Length[is]=!=deg,Message[WedgeCoeff::wedge,Length[is],deg];Throw[$Failed],
Antisymmetrize[Times@@MapThread[delta[#1,-#2]&,{is,targets}],-targets]];
ex=ex/.{w_Wedge/;MatchQ[List@@w,{dx[mani][_]..}]:>pair[(List@@w)/.dx[mani][ii_]:>ii],
dx[mani][ii_]/;deg===1:>pair[{ii}]};
If[!FreeQ[ex,dx[mani]]||!FreeQ[ex,Wedge],Message[WedgeCoeff::unexpanded];Throw[$Failed]];
(* ex is the folded coefficient; deg! ex is the honest component tensor,
   inverting ExpandForms exactly *)
finalize[ContractMetric[deg! ex],targets]
]


(* ::Section:: *)
(*Computations*)


(* ::Input::Initialization:: *)
$PrintColor=Style[#,FontColor->Darker@Brown,FontFamily->"Source Code Pro Semibold"]&;


(* ::Input::Initialization:: *)
myPart[expr_,indices_List,bases_List]:=Module[{
rules=Thread[CNumbersOf[#]->Range[Length@CNumbersOf[#]]]&/@bases
},
expr[[Sequence@@Table[indices[[ii]]/.rules[[ii]],{ii,1,Length@indices}]]]
]


(* ::Input::Initialization:: *)
selector[symbol_,slot_,basis_]:=SelectFirst[Last[Level[#,1]]&/@TensorValIDs[symbol],MemberQ[#,slot*basis]&]


(* ::Input::Initialization:: *)
applyTensorSymmetries[autoKey_,slots_,indices_,bases_]:=Module[{all},
Do[
all=Flatten@ComponentArray@Fold[toBasis,autoKey@@(slot*indices),Reverse[bases]];
Monitor[
Do[
(*Pause[1];*)
ComponentValue[all[[ii]]];
,{ii,1,Length@all}
],
Row[{"Applying tensor symmetries to ",ToBases[autoKey@@(slot*indices)]," ",ProgressIndicator[ii/Length@all,ImageSize->{200,20}]," ",ii,"/",Length@all}]]
,{slot,slots}
]
]


(* ::Input::Initialization:: *)
includeIndependentValuesFromArray[symbol_,array_->slot_,ind_,basis_]:=Module[{keys,positions},
keys=Keys@Last@TensorValues[symbol,slot*basis];
positions=Map[#[[1]]&,#]&/@(Level[#,1]&/@keys);
$CVSimplify=Identity;
Monitor[Do[
(*Pause[0.1];*)
ComponentValue[keys[[ii]],myPart[array,positions[[ii]],basis]];
,
{ii,1,Length@keys}
],Row[{"Including independent values of ",ToBases[symbol@@(slot*ind)]," ",ProgressIndicator[ii/Length@keys,ImageSize->{200,20}]," ",ii,"/",Length@keys}]
];
$CVSimplify=Simplify;
]


(* ::Input::Initialization:: *)
includeIndependentValuesFromSlot[autoKey_,fromSlots_->slots_,indices_,bases_,using_]:=Module[{
fromSlot,
pos,
raiseQ,
keys,
values
},
Do[
fromSlot=SelectFirst[fromSlots,HammingDistance[#,slot]===1&];
pos=First@First@Position[fromSlot+slot,0];
raiseQ=Which[(fromSlot-slot)[[pos]]>0,False,(fromSlot-slot)[[pos]]<0,True];
keys=Keys@Last@TensorValues[autoKey,selector[autoKey,slot,bases]];
values=(using[VBundleOfIndex@indices[[pos]]])[#[[pos]],If[raiseQ,indices[[pos]],-indices[[pos]]]]autoKey@@ReplacePart[#,pos->If[raiseQ,-indices[[pos]],indices[[pos]]]]&/@keys;

$CVSimplify=Identity;
Monitor[Do[
(*Pause[0.1];*)
ComponentValue[keys[[ii]],ToArray[values[[ii]]]];
,
{ii,1,Length@keys}
],Row[{"Including independent values of ",ToBases[autoKey@@(slot*indices)]," ",ProgressIndicator[ii/Length@keys,ImageSize->{200,20}]," ",ii,"/",Length@keys}]
];
$CVSimplify=Simplify;
,{slot,slots}
]
]


(* ::Input::Initialization:: *)
Clear[Compute](*not ClearAll: it would wipe the ::usage*)
Attributes[Compute]={HoldFirst};
Options[Compute]={Using->Automatic,Chain->Automatic,Apply:>$Apply};
Compute[sol_[object_,key_,$auto,autoKey_],OptionsPattern[]]:=Module[{
bases,
indices,
usingOpt=OptionValue[Using],
using,
chainOpt=OptionValue[Chain],
chain,
applyOpt=OptionValue[Apply],
apply,
bundles
},
If[chainOpt===Automatic,
chain=sol[object,key,$auto,autoKey,Routine,Chain],
sol[object,key,$auto,autoKey,Routine,Chain]=chainOpt;chain=chainOpt;
];
(*the slot/index prefetch runs only for a set chain: an unset chain
computes nothing, and prefetching anyway made the entry-level
Compute[sol[object,key]] iterate into the metric's Labels-slotted
automatic tensors (SchoutenCC, EinsteinCC, Perturbation), where
IndicesOfVBundle[Labels] is empty -- xAct's "Tell JMM" debug line plus
First::nofirst on every metric (found 2026-08-24 in the WPR example)*)
If[chain=!=$routine[][Chain],
bundles=(Abs[SlotsOfTensor@autoKey]/.Abs[talasam_]:>talasam);
bases=BasisOfVBundle[#]&/@(Abs[SlotsOfTensor@autoKey]/.Abs[talasam_]:>talasam);
indices={};
Do[AppendTo[indices,(First@IndicesOfVBundle@bundles[[ii]])[[ii]]],{ii,1,Length@bases}];
If[usingOpt===Automatic,
using=<||>;
Do[If[Length@MetricsOfVBundle[bundle]=!=0,AppendTo[using,bundle->First@MetricsOfVBundle[bundle]]],{bundle,bundles}];
(*using=AssociationThread[bundles->(First[MetricsOfVBundle[#]]&/@bundles)];*)
,
using=usingOpt;
];
sol[object,key,$auto,autoKey,Routine,Apply]=applyOpt;
apply=applyOpt;

Do[
Which[
Head@Keys@chainEntry===HoldForm,
Module[{array=ReleaseHold[Keys@chainEntry/.$self->sol],scalar,time,nicePrint},
Module[{},
If[SlotsOfTensor@autoKey=!={},
time=AbsoluteTime[];
applyTensorSymmetries[autoKey,Values@chainEntry,indices,bases];
includeIndependentValuesFromArray[autoKey,array->First@Values@chainEntry,indices,bases];
Global`applyMaps[apply,autoKey,selector[autoKey,First@Values@chainEntry,bases]];
(*the pretty branch evaluates a doctored copy of the fed expression, which is
faithful only while nothing fires on abstract tensors; always-fire rewriter
heads (D, ReplaceAll, Gen) would half-execute and print nonsense (a stray 0),
so chains carrying them are shown verbatim as fed*)
nicePrint=If[StringContainsQ[ToString[Keys@chainEntry],{"MakeArray","Expression","DiagonalMatrix","Pauli"}]||!FreeQ[Keys@chainEntry,D|ReplaceAll|ReplaceRepeated|Gen],
Keys@chainEntry,
ScreenDollarIndices@MapAll[ToBases,Keys@chainEntry/.ToArray->Identity//ReleaseHold]
];
Echo[Row[{"Applied ",Row[Riffle[$PrintColor[#]&/@Values@apply," and "]]," to ",ToBases[autoKey@@(First@Values@chainEntry*indices)]," = ",nicePrint," in ",UnitConvert[Quantity[Round[AbsoluteTime[]-time],"Seconds"],MixedRadix["Minutes","Seconds"]]}]];
,
TensorValIDs[autoKey]^={ValID[autoKey,{}]};
time=AbsoluteTime[];
Monitor[
scalar=(Composition@@(Reverse@Values@apply))[array];
(*Do[
(*Pause[3];*)
Print["I am here"];
scalar=map@array;
,{map,Values@apply}
]*)
,Row[{"Applying ",$PrintColor@((Composition@@(Reverse@Values@apply)))," to ",ToBases[autoKey[]]}
]
];
(Evaluate@autoKey)/:TensorValues[(Evaluate@autoKey),{}]=FoldedRule[{},{(Evaluate@autoKey)[]->scalar}];
nicePrint=If[StringContainsQ[ToString[Keys@chainEntry],{"MakeArray","Expression","DiagonalMatrix","Pauli"}]||!FreeQ[Keys@chainEntry,D|ReplaceAll|ReplaceRepeated|Gen],Keys@chainEntry,MapAll[ToBases,Keys@chainEntry/.ToArray->Identity//ReleaseHold]];
Echo[Row[{"Applied ",Row[Riffle[$PrintColor[#]&/@Values@apply," and "]]," to ",ToBases[autoKey[]]," = ",nicePrint," in ",UnitConvert[Quantity[Round[AbsoluteTime[]-time],"Seconds"],MixedRadix["Minutes","Seconds"]]}]];
]
]
],

True,
Module[{allFromSlots=#&/@(Keys@chainEntry),allSlots=#&/@(Values@chainEntry),fromSlot,time},
time=AbsoluteTime[];
applyTensorSymmetries[autoKey,Values@chainEntry,indices,bases];
includeIndependentValuesFromSlot[autoKey,chainEntry,indices,bases,using];
Global`applyMaps[apply,autoKey,#]&/@(DeleteDuplicates[selector[autoKey,#,bases]&/@allSlots]);
Echo[Row[{"Applied ",Row[Riffle[$PrintColor[#]&/@Values@apply," and "]]," to ",Row[Riffle[Table[ToBases[autoKey@@(allSlots[[ii]]*indices)],{ii,1,Length@allSlots}],", "]]," obtained from ",Row[Riffle[Table[ToBases[autoKey@@(allFromSlots[[ii]]*indices)],{ii,1,Length@allFromSlots}],", "]]," using metrics ",Row[Riffle[Normal@using,", "]]," in ",UnitConvert[Quantity[Round[AbsoluteTime[]-time],"Seconds"],MixedRadix["Minutes","Seconds"]]}]];
]
]
,{chainEntry,chain}
];

Module[{values,valIDs},
values={};
valIDs=TensorValIDs[autoKey];
Do[
AppendTo[values,valID->TensorValues[autoKey,Last[Level[valID,1]]]]
,{valID,valIDs}
];
sol[object,key,$auto,autoKey,Value]=values;
(*special code for determinant*)
(*If[StringContainsQ[ToString[autoKey],"Det"],
Module[{maybeMetric,detSymbol,chart},
maybeMetric=ToExpression[First@StringSplit[ToString[autoKey],"Det"]];
If[MetricQ[maybeMetric],
chart=BasisOfVBundle@VBundleOfMetric@maybeMetric;
detSymbol=SymbolJoin["Det",maybeMetric,chart];
TensorValIDs[detSymbol]^={ValID[detSymbol,{}]};
Evaluate@detSymbol /: TensorValues[Evaluate@detSymbol,{}] =ToBasis[chart]@TensorValues[autoKey,{}];
sol[$metric,maybeMetric,$auto,detSymbol]=ToBasis[chart]@sol[$metric,maybeMetric,$auto,autoKey];
];
]
];*)
];

];
]
Compute[sol_[object_,key_],OptionsPattern[]]:=Module[{usingOpt=OptionValue[Using],applyOpt=OptionValue[Apply],autoKeys},
autoKeys=Keys@sol[object,key,$auto];
Do[
Compute[sol[object,key,$auto,autoKey],Using->usingOpt,Apply:>applyOpt]
,{autoKey,autoKeys}
]
]


(* ::Section:: *)
(*SaveData*)


(* ::Input::Initialization:: *)
giveName[sol_]:=Module[{
maniName,
metSignature,
signatureLetter,
dimension,
chartName
},
ValidateObject[$solution,sol,{PropKeysOf,OptKeysOf}];
ValidateObject[$manifold,(Values@sol[$manifold])[[1]],{ModKeysOf}];

maniName=ToString[(Values@sol[$manifold])[[1]][Name]];
dimension=ToString[(Values@sol[$manifold])[[1]][Dimension]];
metSignature=If[Length@(Values@sol[$metric])=!=0,(Values@sol[$metric])[[1]][Signature],None];
signatureLetter=Which[metSignature===1,"E",metSignature===-1,"L",metSignature===None,None];
chartName=If[Length@(Values@sol[$chart])=!=0,ToString[(Values@sol[$chart])[[1]][Name]],None];

Which[
signatureLetter===None&&chartName===None,Return[StringRiffle[{"Thr",dimension<>"d",maniName},"__"]],
signatureLetter=!=None&&chartName===None,Return[StringRiffle[{"Thr",dimension<>"d"<>signatureLetter,maniName},"__"]],
signatureLetter===None&&chartName=!=None,Return[StringRiffle[{"Sol",dimension<>"d",maniName,chartName},"__"]],
signatureLetter=!=None&&chartName=!=None,Return[StringRiffle[{"Sol",dimension<>"d"<>signatureLetter,maniName,chartName},"__"]]
];
]
(*saveNotebookCopy[path_]:=Module[{
obj,
tmp
},
obj=EvaluationNotebook[];
If[!FailureQ[Quiet[NotebookFileName[]]],
If[path===NotebookFileName[],
NotebookSave[]
,
tmp=NotebookFileName[];
NotebookSave[obj,tmp];
NotebookSave[obj,path];NotebookOpen[tmp];
NotebookOpen[path];
]
,
NotebookSave[obj,path];
]
]*)


(* ::Input::Initialization:: *)
SaveData::baddirectory="$DataDirectory should be a string. The default value is `1`.";
SaveData::badalias="No alias set. Run SetAlias[\"<you>\"] -- your entries are saved under Data/<alias>, and the choice is recorded in your configuration. Aliases already present here: `1`.";
Attributes[SaveData]={HoldFirst};
SaveData[sol_]:=Module[{
dir,
base,
metrics,
attemptSavenb,
currentnb,
allPresentnbs,
tmp
},
ValidateObject[$solution,sol,{PropKeysOf,OptKeysOf}];
ValidateObject[$manifold,(Values@sol[$manifold])[[1]],{ModKeysOf}];
If[!StringQ[$DataDirectory],Message[SaveData::baddirectory,$DefaultDataDirectory];Throw[SaveData]];
If[!StringQ[$Alias],Message[SaveData::badalias,Last[FileNameSplit[#]]&/@FileNames["*",$DataDirectory]];Throw[SaveData]];

sol[$info]=makeInfo[sol[$info]];

Quiet@CreateDirectory[$DataDirectory];
Quiet@CreateDirectory[FileNameJoin[{$DataDirectory,$Alias}]];

base=giveName[sol];

dir=FileNameJoin[{$DataDirectory,$Alias,base}];
attemptSavenb=FileNameTake[dir]<>".nb";
(*Headless, NotebookFileName[] does not return $Failed -- it comes back
unevaluated as NotebookFileName[$Failed] (checked on both 15.0.1 and 13.3.1),
so the currentnb===FileNameTake[$Failed] test below never fired and every
headless save fell through to the front-end branch. Normalise instead: a
notebook that has been saved to disk gives a string, everything else -- a
headless kernel, or a front-end notebook never saved -- gives None.*)
currentnb=Quiet@FileNameTake[NotebookFileName[]];
If[!StringQ[currentnb],currentnb=None];
allPresentnbs=FileNameTake[#]<>".nb"&/@FileNames[All,FileNameJoin[{$DataDirectory,$Alias}]];
If[currentnb===None,
If[MemberQ[allPresentnbs,attemptSavenb],
(*Nobody can answer a dialog without a front end: ChoiceDialog degrades to a
text prompt on stdin, and under wolframscript or any harness that blocks
forever. A headless caller asked for the record to be written and has no
notebook to reconcile, so take the branch a front end takes when the
evaluation notebook IS the entry notebook -- overwrite the .m in place.*)
If[!TrueQ[$Notebooks],
overwriteSame[sol,dir,base];
Return[];
];
If[ChoiceDialog[Row[{"Overwriting of existing data ",$PrintColor@FileNameTake[dir]," is attempted. Shall we proceed?"}]],
overwriteOld[sol,dir,base];
Return[];
,
Return[$Canceled]
];
,
saveNew[sol,dir,base];
Return[];
]
,
If[attemptSavenb===currentnb,
overwriteSame[sol,dir,base];
Return[];
,
If[MemberQ[allPresentnbs,attemptSavenb],
If[ChoiceDialog[Row[{"Overwriting of existing data ",$PrintColor@FileNameTake[dir]," is attempted. Shall we proceed?"}]],
overwriteOld[sol,dir,base];
Return[];
,
Return[$Canceled]
];
,
saveNew[sol,dir,base];
Return[];
]
];
]
]


(* ::Input::Initialization:: *)
(*Expressions are stored exactly as they live in the kernel -- naked; the
Grade guard makes the GetData window safe (2026-08-20, lab/grade-guard).
Records saved before that date carry the metric Expression HoldForm-wrapped
(the pre-guard hack); Load still releases them.*)
Attributes[overwriteOld]={HoldFirst};
overwriteOld[sol_,dir_,base_]:=Module[{tmp,stale},
tmp=EvaluationNotebook[];
stale=SelectFirst[Notebooks[],With[{name=Quiet@NotebookFileName[#]},StringQ[name]&&StringContainsQ[name,FileNameTake[dir]]]&];
If[!MissingQ[stale],NotebookClose[stale]];
DeleteData[FileNameTake[dir]];
Quiet@CreateDirectory[FileNameJoin[{$DataDirectory,$Alias,base}]];
Put[Compress@sol,FileNameJoin[{dir,base<>".m"}]];
addCompletion[];
Echo[Row[{"Saved ",$PrintColor[FileNameJoin[{$Alias,base}]]," under ",$PrintColor[$DataDirectory]," overwriting the existing entry"}]];
SetSelectedNotebook[tmp];
NotebookSave[EvaluationNotebook[],FileNameJoin[{dir,base<>".nb"}]];
]


(* ::Input::Initialization:: *)
Attributes[saveNew]={HoldFirst};
saveNew[sol_,dir_,base_]:=(
Quiet@CreateDirectory[FileNameJoin[{$DataDirectory,$Alias,base}]];
Put[Compress@sol,FileNameJoin[{dir,base<>".m"}]];
addCompletion[];
Echo[Row[{"Saved ",$PrintColor[FileNameJoin[{$Alias,base}]]," under ",$PrintColor[$DataDirectory]}]];
If[TrueQ[$Notebooks],NotebookSave[EvaluationNotebook[],FileNameJoin[{dir,base<>".nb"}]]];
)


(* ::Input::Initialization:: *)
Attributes[overwriteSame]={HoldFirst};
overwriteSame[sol_,dir_,base_]:=(
Quiet@CreateDirectory[FileNameJoin[{$DataDirectory,$Alias,base}]];
Put[Compress@sol,FileNameJoin[{dir,base<>".m"}]];
addCompletion[];
Echo[Row[{"Saved ",$PrintColor[FileNameJoin[{$Alias,base}]]," under ",$PrintColor[$DataDirectory]}]];
If[TrueQ[$Notebooks],NotebookSave[]];
)


(* ::Section:: *)
(*GetData*)


(* ::Input::Initialization:: *)
extractAllSymbols[expr_]:=Module[{symbols={},contexts},
  (* Define the contexts to exclude *)
  contexts=Join[{"System`","VasilDimitrov`SolutionsX`"},Contexts["xAct`*"]];
  
  (* Define a local function to recursively traverse the expression *)
  recTraverse[expression_]:=If[
Head@expression=!=HoldForm&&Head@expression=!=Function,
Which[
    (* If expression is a symbol and not in the excluded contexts, add it to the list *)
    Head[expression]===Symbol&&!MemberQ[contexts,Context[expression]],
    AppendTo[symbols,expression],
    
    (* If expression is an association, recursively traverse its keys and values *)
    AssociationQ[expression],
    (recTraverse/@Keys[expression];recTraverse/@Values[expression]),
    
    (* If expression is a list or any other expression, recursively traverse its elements *)
    ListQ[expression]||Head[expression]===List,
    recTraverse/@expression,
    
    (* If expression is a function application, check the head and arguments *)
    Head[expression]=!=Symbol&&!AtomQ[expression],
    (recTraverse[Head[expression]];recTraverse/@List@@expression)
  ];
];
  
  (* Start the recursive traversal *)
  recTraverse[expr];
  
  (* Return the unique symbols found *)
  DeleteDuplicates[symbols]
]
(*The Label machinery, retired together with the Version and Backup
options on 2026-08-17 (SY6). GetData's Label->"x" suffixed every defined
symbol of a retrieved solution with x, so a second copy could be loaded
beside the first; constants, functions and chart scalars were deliberately
left shared between the copies. Kept below in full in case the feature is
revived. To revive: uncomment the three helpers, add splitStringByList,
makeSymbolRule and replaceInAssociation back to the name list at the top
of the package, give GetData the option Label->None again, and restore its
label branch:
  tmpAssoc=Uncompress@Get@retrievePath;
  ValidateObject[$solution,tmpAssoc,{PropKeysOf,OptKeysOf}];
  tmpAssoc=replaceInAssociation[tmpAssoc,makeSymbolRule[tmpAssoc,label,{}]];
  Return@ValidateObject[$solution,tmpAssoc,{PropKeysOf,OptKeysOf}]
Open questions, never resolved while the feature was live: what Label does
to $auto recipes and stored $self references, and that splitStringByList
silently skips a symbol whose name it cannot segment, so that symbol would
collide with the original copy on Load.*)
(*
splitStringByList[testString_,list_]:=Module[
  {n=StringLength[testString],splitPositions,segments={},pos=1},
  
  (* Create a set for quick membership testing *)
  listSet=AssociationThread[list->True];
  
  (* Function to find the longest match from the current position *)
  longestMatch[start_]:=Module[{end=n,segment},
    While[end>=start,
      segment=StringTake[testString,{start,end}];
      If[KeyExistsQ[listSet,segment],Return[segment]];
      end--
    ];
    None
  ];
  
  (* Iterate over the string to find and collect segments *)
  While[pos<=n,
    With[{segment=longestMatch[pos]},
      If[segment===None,Return[{}]];(* If no match is found at any point, return empty list *)
      AppendTo[segments,segment];
      pos+=StringLength[segment];
    ]
  ];
  
  segments
]
makeSymbolRule[sol_Association,label_,excluded_List]:=Module[{
allSymbols,
defSymbols,
allNames,
defNames,
splitNames,
splittedSymbols,
rulee,
customExcluded
},
(*allSymbols=Select[extractAllSymbols[sol],!MemberQ[excluded,#]&];
defSymbols=Select[extractAllSymbols[Table[sol[obj,key,#]&/@PropKeysOf[obj],{obj,$Objects},{key,Keys@sol[obj]}]],!MemberQ[excluded,#]&];
allNames=ToString[#]&/@allSymbols;
defNames=ToString[#]&/@defSymbols;*)
allSymbols=Select[extractAllSymbols[sol],!MemberQ[excluded,#]&];
customExcluded=Join[
extractAllSymbols[Table[sol[obj,key,#]&/@PropKeysOf[obj],{obj,{$constant,$function}},{key,Keys@sol[obj]}]],
extractAllSymbols[sol[$chart,#,ScalarsOfChart]&/@Keys@sol[$chart]]
]//Flatten//DeleteDuplicates;
defSymbols=Select[extractAllSymbols[Table[sol[obj,key,#]&/@PropKeysOf[obj],{obj,$Objects},{key,Keys@sol[obj]}]],!MemberQ[customExcluded,#]&];
allNames=ToString[#]&/@allSymbols;
defNames=ToString[#]&/@defSymbols;
splitNames=Join[Select[Flatten[Names[#]&/@((#<>"*")&/@Select[Contexts["xAct`*"],StringFreeQ[#,"Private"]&])],StringFreeQ[#,"xAct"]&],defNames,{"Det"}];
splittedSymbols=Map[If[StringQ[#],Symbol[#],#]&,DeleteCases[{}][(splitStringByList[#,splitNames]&/@allNames)],{0,Infinity}];
rulee=Thread[defSymbols->(SymbolJoin[#,label]&/@defSymbols)];
Return@Thread[Symbol[StringJoin[#]]&/@Map[If[Head[#]===Symbol,ToString[#],#]&,(splittedSymbols),{0,Infinity}]->(Symbol[StringJoin[#]]&/@Map[If[Head[#]===Symbol,ToString[#],#]&,(splittedSymbols/.rulee),{0,Infinity}])]
]
replaceInAssociation[assoc_Association,rulee_]:=Module[{},
  Association[
    KeyValueMap[
      (ReplaceAll[#1,rulee]->
        If[AssociationQ[#2],replaceInAssociation[#2,rulee],ReplaceAll[#2,rulee]]
      )&,assoc
    ]
  ]
]
*)


(* ::Input::Initialization:: *)
GetData::missingname="name(`1`) is missing from the names: `2`, found under alias: `3`";
GetData::missingfile="No stored .m file was found in the entry directory `1`.";
GetData::noalias="No alias set. Run SetAlias[\"<you>\"] (recorded in your configuration), or read another user's entry as \"alias/name\".";
Options[GetData]={Alias->Automatic};
GetData[name_String,OptionsPattern[]]:=Module[{
alias,entry,currentNames,mFiles,retrievePath
},
{alias,entry}=resolveEntry[name,OptionValue[Alias]];
If[!StringQ[alias],Message[GetData::noalias];Throw[GetData]];
currentNames=Last[FileNameSplit[#]]&/@FileNames["*",FileNameJoin[{$DataDirectory,alias}]];
If[!MemberQ[currentNames,entry],
Message[GetData::missingname,entry,currentNames,alias];Throw[GetData];
];
mFiles=FileNames["*.m",FileNameJoin[{$DataDirectory,alias,entry}]];
retrievePath=First[mFiles,Missing["NotFound"]];
If[Head@retrievePath===Missing,Message[GetData::missingfile,FileNameJoin[{$DataDirectory,alias,entry}]]; Throw[GetData]];
Return@ValidateObject[$solution,Uncompress@Get@retrievePath,{PropKeysOf,OptKeysOf}]
]
GetData[]:=With[{split=FileNameSplit[NotebookFileName[]]},GetData[split[[-3]]<>"/"<>split[[-2]]]]


(* ::Input::Initialization:: *)
DeleteData::missingname="name(`1`) is missing from the names: `2`, found under alias: `3`";
DeleteData::foreign="DeleteData only deletes entries under your own alias (`1`); `2` belongs to `3`.";
DeleteData[name_String]:=Module[{
alias,entry,currentNames
},
If[!StringQ[$Alias],Message[GetData::noalias];Throw[$Failed]];
{alias,entry}=resolveEntry[name,Automatic];
If[alias=!=$Alias,Message[DeleteData::foreign,$Alias,entry,alias];Throw[$Failed]];
currentNames=Last[FileNameSplit[#]]&/@FileNames["*",FileNameJoin[{$DataDirectory,$Alias}]];
If[!MemberQ[currentNames,entry],
Message[DeleteData::missingname,entry,currentNames,$Alias];Throw[$Failed];
];
DeleteDirectory[FileNameJoin[{$DataDirectory,$Alias,entry}],DeleteContents->True];
addCompletion[];
]


(* ::Input::Initialization:: *)
OpenData::missingname="name(`1`) is missing from the names: `2`, found under alias: `3`";
OpenData::nofe="OpenData requires a front end to open the entry notebook.";
Options[OpenData]={Alias->Automatic};
OpenData[name_String,OptionsPattern[]]:=Module[{
alias,entry,currentNames
},
If[!TrueQ[$Notebooks],Message[OpenData::nofe];Throw[$Failed]];
{alias,entry}=resolveEntry[name,OptionValue[Alias]];
If[!StringQ[alias],Message[GetData::noalias];Throw[$Failed]];
currentNames=Last[FileNameSplit[#]]&/@FileNames["*",FileNameJoin[{$DataDirectory,alias}]];
If[!MemberQ[currentNames,entry],
Message[OpenData::missingname,entry,currentNames,alias];Throw[$Failed];
];
NotebookOpen[FileNameJoin[{$DataDirectory,alias,entry,entry<>".nb"}]]
]


(* ::Input::Initialization:: *)
(*The init cell carries no identity and no absolute paths: LocateData[] derives
the location from the notebook's own position, session-only; $Alias always
comes from the user configuration.*)
NewData::nofe="NewData requires a front end to create a notebook.";
NewData[]:=Module[{nb,code,cellExpr},
If[!TrueQ[$Notebooks],Message[NewData::nofe];Throw[$Failed]];
nb=NotebookCreate[];
code=StringJoin[
"Needs[\"VasilDimitrov`SolutionsX`\"]\n",
"LocateData[]\n",
"EnableParallelComputations[]"
];
cellExpr=Cell[TextData@{code},"Input",InitializationCell->True];
NotebookWrite[nb,cellExpr];
SelectionMove[nb,Before,Notebook]; (*Optional:deselect the cell*)
]


(* ::Input::Initialization:: *)
(*The newcomer wizard (2026-08-24; X7 record in lab/repo-maintenance;
dialogs streamlined same day after Vasil's Jem test: short buttons with
the paths and the clone advice in the prose, a checkbox picker for the
curated entries, and the tour notebook removed as valueless once the
picker existed). The dialogs write the configuration through
SetAlias/SetDataDirectory with the default Permanent->True: this is the
user's own machine and being remembered is the point; $Curator is never
touched in either direction (writeConfig snapshots it, so a curator
re-running Welcome keeps the flag and a newcomer never gains it).
Cancelling a dialog skips it and everything after it. Headless prints
the same steps as commands. All names selected is copied as the one
bulk CopyData["Curated"], so the echo protocol matches the documented
bulk path.*)
Welcome[]:=Module[{dir,alias,sel},
If[!TrueQ[$Notebooks],welcomeRecipe[];Return[]];
dir=welcomeAskDirectory[];
If[!StringQ[dir],Return[]];
alias=welcomeAskAlias[];
If[!StringQ[alias],Return[]];
If[dir=!=$DataDirectory,SetDataDirectory[dir]];
If[alias=!=$Alias,SetAlias[alias]];
sel=welcomeAskCurated[];
Which[
sel===All,Catch[CopyData["Curated"]],
ListQ[sel]&&sel=!={},Scan[Function[n,Catch[CopyData["Curated/"<>n]]],sel]];
Echo["Ready \[LongDash] NewData[] opens a fresh working notebook; OpenData[\"name\"] opens a copied entry; GetData[\"name\"] then Load makes it live"];
]

(*The xTras idiom (xTrasHelp, xCore.m): one verb to the guide page.*)
SolutionsXHelp[]:=If[TrueQ[$Notebooks],
SystemOpen["paclet:VasilDimitrov/SolutionsX/guide/SolutionsX"],
Print["The SolutionsX guide: paclet:VasilDimitrov/SolutionsX/guide/SolutionsX \[LongDash] open in a front end, or see https://github.com/waskou/SolutionsX"]]

welcomeAskDirectory[]:=Module[{current,isDefault,choice},
current=If[StringQ[$DataDirectory],$DataDirectory,$DefaultDataDirectory];
isDefault=current===$DefaultDataDirectory;
choice=ChoiceDialog[
Column[{
Style["Where should your data live?",Bold],
"Entries are stored under <directory>/<alias>/.",
"If you cloned the SolutionsX repository, the best choice is the clone's own Data folder.",
Style[If[isDefault,"Default:  ","Current:  "]<>current,Italic,GrayLevel[0.35],11]
},Spacings->1],
{"Choose folder\[Ellipsis]"->"browse",
If[isDefault,"Use default","Keep current"]->current},
WindowTitle->"SolutionsX \[LongDash] data directory"];
Which[
choice==="browse",
With[{d=SystemDialogInput["Directory",current]},
If[StringQ[d],d,$Canceled]],
StringQ[choice],choice,
True,$Canceled]]

welcomeAskAlias[]:=Module[{choice,a},
If[StringQ[$Alias],
choice=ChoiceDialog[
Row[{"Your alias is ",$Alias,". Everything you save lives under Data/",$Alias,"."}],
{"Keep "<>$Alias->$Alias,"Change\[Ellipsis]"->"change"},
WindowTitle->"SolutionsX \[LongDash] alias"];
If[choice=!="change",Return[choice]]];
a=InputString["Choose your alias -- your entries will be saved under Data/<alias>:"];
If[StringQ[a]&&StringLength[a]>0,a,$Canceled]]

welcomeRecipe[]:=(
Print["SolutionsX first steps:"];
Print["  SetAlias[\"you\"]            -- your entries live under Data/you; recorded in your configuration"];
Print["  SetDataDirectory[\"path\"]   -- optional; the default is "<>$DefaultDataDirectory];
Print["  CopyData[\"Curated\"]        -- copy the curated entries under your alias"];
Print["  sol=GetData[\"name\"]; Load[sol]   -- make a stored entry live; see the SolutionsX guide for more"];)

(*The curated picker: every entry of the curated source (tree or the
paclet bundle, via curatedSource) as a vertical checkbox list, all
preselected -- the recommendation is the default -- with All/None and
one Copy. Returns All when everything stays selected (Welcome then
takes the documented bulk path), the selected names otherwise, {} on
Skip or a closed window. An absent corpus is said out loud, never a
silent no-op.*)
welcomeAskCurated[]:=Module[{src,names,res},
src=curatedSource[];
names=If[StringQ[src],FileNameTake/@Select[FileNames["*",src],DirectoryQ],{}];
If[names==={},
Echo["No curated corpus found \[LongDash] neither the data tree nor the installed paclet carries one"];
Return[{}]];
res=DialogInput[DynamicModule[{sel=names},
Column[{
Style["Copy curated entries under your alias?",Bold],
"Recommended: they are the package's worked examples.",
CheckboxBar[Dynamic[sel],names,Appearance->"Vertical"],
Row[{Button["All",sel=names],"  ",Button["None",sel={}]}],
Row[{DefaultButton["Copy selected",DialogReturn[sel]],"  ",CancelButton["Skip",DialogReturn[{}]]}]
},Spacings->1]],WindowTitle->"SolutionsX \[LongDash] curated data"];
Which[
!ListQ[res],{},
Sort[res]===Sort[names],All,
True,res]]


(* ::Section:: *)
(*User configuration*)


(* ::Input::Initialization:: *)
(*Identity ($Alias) and default location ($DataDirectory) are user state, kept
in one inspectable file outside any repository and shared by every kernel
generation via $UserBaseDirectory. Notebooks carry neither: an entry notebook
derives location from its own position (see NewData), identity always comes
from here. The config is written by SetAlias/SetDataDirectory (Permanent->True,
the default) and read once at package load.*)
configFile[]:=FileNameJoin[{$UserBaseDirectory,"ApplicationData","SolutionsX","config.wl"}];
SetAlias::badconfig="Ignoring the malformed configuration file at `1`.";
readConfig[]:=If[FileExistsQ[configFile[]],
With[{c=Quiet@Check[Get[configFile[]],$Failed]},
If[AssociationQ[c],c,Message[SetAlias::badconfig,configFile[]];$Failed]],
$Failed];
writeConfig[]:=Module[{assoc=<||>},
If[StringQ[$DataDirectory],assoc["DataDirectory"]=$DataDirectory];
(*the Curated role must never leak into the config as an identity: while
in the role, any permanent write records the identity behind it*)
Which[
StringQ[$Alias]&&$Alias=!="Curated",assoc["Alias"]=$Alias,
$Alias==="Curated"&&StringQ[curatorIdentity],assoc["Alias"]=curatorIdentity];
If[TrueQ[$Curator],assoc["Curator"]=True];
Quiet@CreateDirectory[DirectoryName[configFile[]],CreateIntermediateDirectories->True];
Put[assoc,configFile[]];
]


(* ::Input::Initialization:: *)
(*The resume idiom: at the top of a notebook section, ResumeAs[sol] restores
the entry from disk into a fresh kernel; on a live kernel it rebuilds the
Load banner from $loadedSymbols -- everything load made live, whether via a
full-record Load, IncludeTo-style authoring, or a hand load -- so the same
panel appears either way and a full top-to-bottom run is indistinguishable
from a resume after a kernel quit. Load stays pure -- it always replays the
record exactly; only the DECISION to call it is state-aware.*)
Attributes[ResumeAs]={HoldFirst};
ResumeAs[sol_]:=If[$loadedSymbols==={},
sol=GetData[];Load[sol],
makeSymbolPanel[$loadedSymbols,"Loaded:"]]


(* ::Input::Initialization:: *)
(*The display line of NewData's initialization cell. When the current
notebook sits inside a Data entry directory (.../root/alias/name/), point
$DataDirectory at root/Data for this session; when it sits at a SolutionsX
repo root (the unmistakable signature: PacletInfo.wl next to a Data
subdirectory -- the Welcome wizard's position), point it at <dir>/Data;
anywhere else the configured values stand. Session-only so opening a
notebook from another data tree never rewrites the user's configuration.
It then always shows the load panel and the Data status: in a front end
the package load prints nothing, so this is the single display point, and
every evaluation reproduces the same output -- the FE's yes/no
initialization double run cannot be seen.*)
LocateData[]:=With[{d=Quiet@Check[DirectoryName[NotebookFileName[]],$Failed]},
If[StringQ[d],LocateData[d],printLoadPanel[];printDataStatus[];]]
LocateData[dir_String]:=(
Which[
StringMatchQ[FileNameTake[dir],("Thr__"|"Sol__")~~___],
setDataDir[FileNameJoin[Drop[FileNameSplit[dir],-2]]],
FileExistsQ[FileNameJoin[{dir,"PacletInfo.wl"}]]&&DirectoryQ[FileNameJoin[{dir,"Data"}]],
setDataDir[FileNameJoin[{dir,"Data"}]]];
printLoadPanel[];
printDataStatus[];)


(* ::Input::Initialization:: *)
(*The $info stamp SaveData writes into every record: machine-generated,
machine-trusted, invisible to validation. SolutionsXVersion and LastEdit are
the contact-and-compatibility surface; LastSave and the full xAct $Version
pairs (version string AND build date -- two Invar 2.0.5 builds differ only
by date) are what reverse-engineering a compatibility question actually
needs, since the xAct stack, not SolutionsX, is what has broken record
compatibility so far (the Dagger lesson).*)
(*Author is provenance and survives every path: set once at a record's
first save, preserved verbatim ever after (old records inherit it from
their LastEdit). Under the Curated role it is the identity behind the
role, never "Curated".*)
makeInfo[old_:<||>]:=<|
"SolutionsXVersion"->$Version[[1]],
"Author"->Which[
AssociationQ[old]&&StringQ[old["Author"]],old["Author"],
AssociationQ[old]&&StringQ[old["LastEdit"]],old["LastEdit"],
$Alias==="Curated"&&StringQ[curatorIdentity],curatorIdentity,
True,$Alias],
"LastEdit"->$Alias,
"LastSave"->DateString[],
"Wolfram"->First[StringSplit[System`$Version," "]],
"xAct"-><|
"FieldsX"->xAct`FieldsX`$Version,
"xTerior"->xAct`xTerior`$Version,
"xCoba"->xAct`xCoba`$Version,
"xTensor"->xAct`xTensor`$Version,
"xCore"->xAct`xCore`$Version,
"Invar"->xAct`Invar`$Version,
"xPerm"->xAct`xPerm`$Version,
"xPert"->xAct`xPert`$Version,
"xTras"->xAct`xTras`$Version|>
|>


(* ::Input::Initialization:: *)
(*"alias/name" and the Alias option resolve to {alias, entry}; a bare name is
the user's own. Entry names never contain "/", so the split is unambiguous.*)
resolveEntry[name_String,aliasOption_]:=Which[
StringQ[aliasOption],{aliasOption,name},
StringContainsQ[name,"/"],With[{p=StringSplit[name,"/"]},{First[p],StringRiffle[Rest[p],"/"]}],
True,{$Alias,name}]


(* ::Input::Initialization:: *)
(*The captured banner blocks, one clickable name each -- the mirror of
makeSymbolPanel, with the block text in place of Information.*)
printLoadPanel[]:=If[TrueQ[$Notebooks]&&Length[$LoadLog]>0,
Print@DynamicModule[{info="",showInfo=False,current=""},
Panel[
Column[{
"Loaded:",
Button[
Style[#,FontFamily->"Source Code Pro",FontWeight->"SemiBold",Darker@Blue],
If[current===#&&showInfo,
showInfo=False,
current=#;
info=Style[$LoadLog[#],FontFamily->"Source Code Pro",FontSize->11];
showInfo=True
],ImageMargins->2,Appearance->"Frameless"]&/@Keys[$LoadLog]//Row[#," , "]&,
Dynamic[If[showInfo,info,"Info[...]"]]
},Spacings->1],ImageSize->Full]]];


(* ::Input::Initialization:: *)
(*The Data status line, in the Compute Echo convention: inferred state must
be displayed, and an Echo is immutable once out -- like a Print, never
rewritten. In a front end an unset alias renders as a house-style
frameless link, "--> choose alias <--", that asks for one; the resulting
SetAlias (like any SetAlias/SetDataDirectory, programmatic or not) echoes
its own confirmation line, so the change is recorded by a fresh Echo
below the old status line. Headless prints the same line at the end of
the load.*)
printDataStatus[]:=Module[{dirPart,aliasPart},
dirPart=If[StringQ[$DataDirectory],$PrintColor[$DataDirectory],Style["not set \[LongDash] SetDataDirectory[\"path\"]",Italic]];
aliasPart=Which[
StringQ[$Alias],$PrintColor[$Alias],
TrueQ[$Notebooks],Button[
Style["--> choose alias <--",FontFamily->"Source Code Pro",FontWeight->"SemiBold",Darker@Blue],
Module[{a=InputString["Choose your alias -- your entries will be saved under Data/<alias>:"]},
If[StringQ[a]&&StringLength[a]>0,SetAlias[a]]],Method->"Queued",Appearance->"Frameless"],
True,Style["not set \[LongDash] SetAlias[\"you\"]",Italic]];
Echo[Row[Join[{"Data directory ",dirPart," with alias ",aliasPart},
If[TrueQ[$Curator],{" ",Style["[curator]",Italic]},{}],
If[!StringQ[$Alias],{" ",Style["\[LongDash] Welcome[] gives a guided start",Italic]},{}]]]];
]


(* ::Input::Initialization:: *)
Module[{c=readConfig[]},
If[AssociationQ[c],
Unprotect[$DataDirectory,$Alias,$Curator];
If[StringQ[c["DataDirectory"]],$DataDirectory=c["DataDirectory"]];
If[StringQ[c["Alias"]],$Alias=c["Alias"]];
If[TrueQ[c["Curator"]],$Curator=True];
Protect[$DataDirectory,$Alias,$Curator];
];
];
addCompletion[];
If[!TrueQ[$Notebooks],printDataStatus[]];


(* ::Section:: *)
(*Package Footer*)


(* ::Input::Initialization:: *)
End[];
EndPackage[];
