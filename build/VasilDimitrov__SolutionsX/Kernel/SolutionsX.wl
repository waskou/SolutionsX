(* ::Package:: *)

(* ::Section::Closed:: *)
(*Licensing*)


(* ::Input::Initialization:: *)
VasilDimitrov`SolutionsX`$FieldsXVersionExpected={"1.1.3",{2021,5,27}};
VasilDimitrov`SolutionsX`$Version={"0.0.1",Date[][[1;;3]]};


(* ::Input::Initialization:: *)
If[Unevaluated[xAct`xCore`Private`$LastPackage]===xAct`xCore`Private`$LastPackage,xAct`xCore`Private`$LastPackage="VasilDimitrov`SolutionsX`"];


(* ::Input::Initialization:: *)
If[!MemberQ[$Packages,"VasilDimitrov`SolutionsX`"],
Block[{Print},
BeginPackage["VasilDimitrov`SolutionsX`",{"xAct`FieldsX`","xAct`xTerior`","xAct`xCoba`","xAct`xTensor`","xAct`xCore`","xAct`Invar`","xAct`xPerm`","xAct`xPert`","xAct`xTras`"}];
];
];


(* ::Input::Initialization:: *)
Block[{Print},
Print[xAct`xCore`Private`bars];Print["Package VasilDimitrov`SolutionsX`  version ",VasilDimitrov`SolutionsX`$Version[[1]],", ",VasilDimitrov`SolutionsX`$Version[[2]]];Print["Copyright (C) 2023, Vasil Dimitrov, under the General Public License."];
]


(* ::Input::Initialization:: *)
Off[General::shdw]
xAct`xForm`Disclaimer[]:=Print["These are points 11 and 12 of the General Public License:\n\nBECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW. EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM `AS IS\.b4 WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU. SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.\n\nIN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGES."]
On[General::shdw]


(* ::Input::Initialization:: *)
Block[{Print},
If[xAct`xCore`Private`$LastPackage==="VasilDimitrov`SolutionsX`",
Unset[xAct`xCore`Private`$LastPackage];
Print[xAct`xCore`Private`bars];Print["These packages come with ABSOLUTELY NO WARRANTY; for details type Disclaimer[]. This is free software, and you are welcome to redistribute it under certain conditions. See the General Public License for details."];
Print[xAct`xCore`Private`bars];
]
]


(* ::Input::Initialization:: *)
Unprotect[Get];
<<VasilDimitrov`SolutionsX` :=Null
Protect[Get];


(* ::Section::Closed:: *)
(*Non-standard setup*)


(* ::Input::Initialization:: *)
$Overwrite=True;
$Verbose=False;
$QuietEcho=False;
$Load=False;
$Success;


(* ::Input::Initialization:: *)
$DefInfoQ=$UndefInfoQ=$CVVerbose=$Verbose;
$PrecomputeGammaMatrixProducts=False;
Off[Throw::nocatch];
Off[DefMetric::old];
Off[Undef::unknown];
Off[UndefCovD::unknown];
Off[PrintAsCharacter::argx];
Off[Definition::ssle];
Off[ToCanonical::cmods];


(* ::Input::Initialization:: *)
$context="Global`";
$Letters=<|
"latin"->CharacterRange["a","z"],
"Latin"->{"A","B","F","G","H","J","L","M","P","Q","R","S","T","U","V","W","X","Y","Z"},
"greek"->{"\[Alpha]","\[Beta]","\[Gamma]","\[Delta]","\[Mu]","\[Nu]","\[Rho]","\[Sigma]"},
"script"->{"\[ScriptA]","\[ScriptB]","\[ScriptC]","\[ScriptD]","\[ScriptE]","\[ScriptF]","\[ScriptG]","\[ScriptH]","\[ScriptI]","\[ScriptJ]","\[ScriptK]","\[ScriptL]","\[ScriptM]","\[ScriptN]","\[ScriptO]","\[ScriptP]" ,"\[ScriptQ]","\[ScriptR]","\[ScriptS]","\[ScriptT]","\[ScriptU]","\[ScriptV]","\[ScriptW]","\[ScriptX]","\[ScriptY]","\[ScriptZ]"},
"Script"->{"\[ScriptCapitalA]","\[ScriptCapitalB]","\[ScriptCapitalC]","\[ScriptCapitalD]","\[ScriptCapitalE]","\[ScriptCapitalF]","\[ScriptCapitalG]","\[ScriptCapitalH]","\[ScriptCapitalI]","\[ScriptCapitalJ]","\[ScriptCapitalK]","\[ScriptCapitalL]","\[ScriptCapitalM]","\[ScriptCapitalN]","\[ScriptCapitalO]","\[ScriptCapitalP]","\[ScriptCapitalQ]","\[ScriptCapitalR]","\[ScriptCapitalS]","\[ScriptCapitalT]","\[ScriptCapitalU]","\[ScriptCapitalV]","\[ScriptCapitalW]","\[ScriptCapitalX]","\[ScriptCapitalY]","\[ScriptCapitalZ]"},
"gothic"->{"\[GothicA]","\[GothicB]","\[GothicC]","\[GothicD]","\[GothicE]","\[GothicF]","\[GothicG]","\[GothicH]","\[GothicI]","\[GothicJ]","\[GothicK]","\[GothicL]","\[GothicM]","\[GothicN]","\[GothicO]","\[GothicP]","\[GothicQ]","\[GothicR]","\[GothicS]","\[GothicT]","\[GothicU]","\[GothicV]","\[GothicW]","\[GothicX]","\[GothicY]","\[GothicZ]"},
"Gothic"->{"\[GothicCapitalA]","\[GothicCapitalB]","\[GothicCapitalC]","\[GothicCapitalD]","\[GothicCapitalE]","\[GothicCapitalF]","\[GothicCapitalG]","\[GothicCapitalH]","\[GothicCapitalI]","\[GothicCapitalJ]","\[GothicCapitalK]","\[GothicCapitalL]","\[GothicCapitalM]","\[GothicCapitalN]","\[GothicCapitalO]","\[GothicCapitalP]","\[GothicCapitalQ]","\[GothicCapitalR]","\[GothicCapitalS]","\[GothicCapitalT]","\[GothicCapitalU]","\[GothicCapitalV]","\[GothicCapitalW]","\[GothicCapitalX]","\[GothicCapitalY]","\[GothicCapitalZ]"},
"struck"->{"\[DoubleStruckA]","\[DoubleStruckB]","\[DoubleStruckC]","\[DoubleStruckE]","\[DoubleStruckF]","\[DoubleStruckG]","\[DoubleStruckH]","\[DoubleStruckI]","\[DoubleStruckJ]","\[DoubleStruckK]","\[DoubleStruckL]","\[DoubleStruckM]","\[DoubleStruckN]","\[DoubleStruckO]","\[DoubleStruckP]","\[DoubleStruckQ]","\[DoubleStruckR]","\[DoubleStruckS]","\[DoubleStruckT]","\[DoubleStruckU]","\[DoubleStruckV]","\[DoubleStruckW]","\[DoubleStruckY]","\[DoubleStruckZ]"},
"Struck"->{"\[DoubleStruckCapitalA]","\[DoubleStruckCapitalB]","\[DoubleStruckCapitalC]","\[DoubleStruckCapitalD]","\[DoubleStruckCapitalE]","\[DoubleStruckCapitalF]","\[DoubleStruckCapitalG]","\[DoubleStruckCapitalH]","\[DoubleStruckCapitalI]","\[DoubleStruckCapitalJ]","\[DoubleStruckCapitalK]","\[DoubleStruckCapitalL]","\[DoubleStruckCapitalM]","\[DoubleStruckCapitalN]","\[DoubleStruckCapitalO]","\[DoubleStruckCapitalP]","\[DoubleStruckCapitalQ]","\[DoubleStruckCapitalR]","\[DoubleStruckCapitalS]","\[DoubleStruckCapitalT]","\[DoubleStruckCapitalU]","\[DoubleStruckCapitalV]","\[DoubleStruckCapitalX]","\[DoubleStruckCapitalW]","\[DoubleStruckCapitalY]","\[DoubleStruckCapitalZ]"}
|>;
$Objects={$info,$manifold,$covd,$autoTensor,$metric,$bundle,$frame,$spin,$chart,$basis,$form,$tensor,$spinor,$function,$constant,$parameter,$assumption,$rule,$equation};
$Properties={PropKeysOf,KeysOf,OptKeysOf,AutoKeysOf,MatchOf};
$properties={propKeysOf,keysOf,optKeysOf,autoKeysOf,matchOf};
$Methods={WeakValidate,StrongValidate,StrongerValidate,MakeLoad,Uniformize};
$methods={weakValidate,strongValidate,strongerValidate,makeLoad,uniformize};
$Pattern;
$Log={{"Begin"}};


(* ::Input::Initialization:: *)
$DefaultDirectory=FileNameJoin[{ExpandFileName@URL@$LocalBase,"SolutionsX"}];
Protect[$DefaultDirectory];


(* ::Input::Initialization:: *)
$Directory:=Evaluate@$DefaultDirectory;
Quiet[CreateDirectory[$Directory]];
If[(FileNameSplit[#][[-1]]&/@FileNames[All,$Directory])=!={},(FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#]]&)["Retrieve"->{Join[{""},(FileNameSplit[#][[-1]]&/@FileNames[All,$Directory])]}]];


(* ::Input::Initialization:: *)
$Map=Together;
$ParallelMap=Simplify;
$PrintColor=Style[#,FontColor->Darker@Brown,FontFamily->"Source Code Pro Semibold"]&;


(* ::Section::Closed:: *)
(*Usage messages*)


(* ::Input::Initialization:: *)
Instantiate::usage=
"Instantiate[sol, Curved->(<|Dimension->integer, SignDet->(1|-1), CNumber->{integers}, Coord->{symbols[]}|>|False), Frame->(True|False), Spin->(True|False), Lie->(<|Dimension->integer,CNumber->{integers}|>|False), $info->{association}, $metric->{associations}, $covd->{associations}, $form->{associations}, $tensor->{associations}, $spinor->{associations}, $function->{associations}, $constant->{associations}, $parameter->{associations}, $assumption->{associations}, $rule->{associations}, $equation->{associations}, Load->True, Verbose->False] sets the variable sol to a proper $solution association (only the first four options are strictly needed, the rest can be included later on) and by default loads the xAct environment";
Load::usage=
"Load[sol[$object], key, Verbose->$Verbose, Overwrite->$Overwrite] loads the $object stored in the association sol[$object, key] (by default it overwrides it)
Load[sol[$object], {keys}, options] does the same for a list of keys
Load[sol[$object], options] does the same for all keys
Load[sol, options] loads the entire solutions sol";
Unload::usage=
"Unload[sol[$object], key, Verbose->$Verbose, Overwrite->$Overwrite] unloads the $object stored in the association sol[$object, key]
Unload[sol[$object], {keys}, options] does the same for a list of keys
Unload[sol[$object], options] does the same for all keys
Unload[sol, options] loads the entire solutions sol";
IncludeTo::usage=
"IncludeTo[sol[$object], association, Load->True, Verbose->False] makes a proper $object out of the possibly incomplete association, stores it in sol and (by default) loads it
IncludeTo[sol[$object], {associations}, options] does the same for a list of associations";
DropFrom::usage=
"DropFrom[sol[$object], key, Verbose->False] drops the $object specified by key and unloads it
DropFrom[sol[$object], {keys}, options] does the same for a list of keys";
Compute::usage=
"Compute[sol[$object, keyList]] computes the $object stored in sol[$object, keyList] using sol[$object, keyList, Routine]";
Store::usage=
"Store[sol] creates a unique solution folder with name generated from sol[$info, firstKey], stores sol as a binary file (keeping up to five versions) and saves the current notebook with that unique name in the same folder";
Retrieve::usage=
"Retrieve[string] fetches the last association stored in the unique folder string in $Directory
Retrieve[string, -n] does the same with the last, except fetching one of the previous five versions";


(* ::Input::Initialization:: *)
Overwrite;
Seed::usage=
"a key in Routine";
Chain::usage=
"a key in Routine";


(* ::Input::Initialization:: *)
PropKeysOf::usage=
"PropKeysOf[$object] returns the property keys of the $object";
KeysOf::usage=
"KeysOf[$object] returns all keys of the $object";
OptKeysOf::usage=
"OptKeysOf[$object] returns the option keys of the $object";
AutoKeysOf::usage=
"AutoKeysOf[$object] returns the automatically generated keys of the $object";
MatchOf::usage=
"MatchOf[$object] returns the canonical match form of the $object used for validation purposes";

WeakValidate::usage=
"WeakValidate[$object, association] returns the association if it passes as a weakly validated $object, meaning that the general form of the association is correct (possibly missing some key-value pairs are left unspecified)";
StrongValidate::usage=
"StrongValidate[$object, association] returns the association if it passes as a strongly validated $object, meaning that PropKeysOf[$object] are all valid";
StrongerValidate::usage=
"StrongerValidate[$object, association] returns the association if it passes as a strongest validated $object, meaning that PropKeysOf[$object] and AutoKeysOf[$object] are all valid";
MakeLoad::usage=
"MakeLoad[$object, association] attempt to generate a list of arguments for the respective xAct function that will load that $object";
Uniformize::usage=
"Uniformize[$object, association] brings the association to the canonical form for the $object";


(* ::Input::Initialization:: *)
Letters;
MakeSymbol;
Resurrect;
Revive;
ds2Q;
formDegreeQ;
MakeArray;
MakeForm;
ParallelMapSimplify;


(* ::Input::Initialization:: *)
ToBases::usage=
"ToBases[sol][expr] performs the usual xAct function ToBasis[expr] for all bases found in the $solution sol";
ToArray::usage=
"ToArray[sol][expr] converts a differential form, metric or xAct object with indices into an ordinary Mathematica array. To do so it needs as first argument a loaded $solution sol";
GenFlatGamma::usage=
"GenFlatGamma[sol, n] generates an n-index flat gamma matrix array with all upper indices using Toine's conventions";
GenFlatMetric::usage=
"GenFlatMetric[sol] generates a diagonal metric (-+...+) or (+...+) depending on sol[$metric, firstMetric, SignDet]";
SolutionNames::usage=
"SolutionNames[] returns all uniques solution names in $Directory";
UpdateAutocomplete::usage=
"UpadateAutocomplete[] updats the autocompletion of string argument of Retrieve";
GenFunctionRule::usage=
"GenFunctionRule[sol[$function, key]] makes a Mathematica Function type rule out of the $function stored in sol[$function, key]";
FirstBasisOfVBundle;


(* ::Input::Initialization:: *)
(*obj:*)$manifold;
Symbol;
Dimension;
Index;


(* ::Input::Initialization:: *)
(*obj:*)$autoTensor;
Symbol;
Routine;
Dependency;
Value;


(* ::Input::Initialization:: *)
(*obj:*)$metric;
SignDet;
Symbol;
CovD;
Expr;
Routine;
Dependency;
Value;


(* ::Input::Initialization:: *)
(*obj:*)$bundle;
Symbol;
Manifold;
Dimension;
Metric;


(* ::Input::Initialization:: *)
(*obj:*)$covd;
Symbol;
VBundle;


(* ::Input::Initialization:: *)
(*obj:*)$frame;
Symbol;
Metric;
Index;
Expr;
Vielbein;
MetricTensor;


(* ::Input::Initialization:: *)
(*obj:*)$spin;
Metric;
Index;
Connection;
CovD;


(* ::Input::Initialization:: *)
(*obj:*)$chart;
Manifold;
CNumber;
Coord;


(* ::Input::Initialization:: *)
(*obj:*)$basis;
VBundle;
CNumber;
Coord;


(* ::Input::Initialization:: *)
(*obj:*)$form;
Symbol;
Manifold;
Degree;
Tensor;
Expr;


(* ::Input::Initialization:: *)
(*obj:*)$tensor;
Symbol;
Manifold;
Symmetry;
Grassmann;Even;Odd;
Routine;
Dependency;
Value;


(* ::Input::Initialization:: *)
(*obj:*)$spinor;
Symbol;
Manifold;
Symmetry;
Grassmann;Even;Odd;
Routine;
Dependency;
Value;
bar;


(* ::Input::Initialization:: *)
(*obj:*)$function;
Symbol;
Expr;


(* ::Input::Initialization:: *)
(*obj:*)$constant;
Symbol;
Expr;


(* ::Input::Initialization:: *)
(*obj:*)$parameter;
Symbol;


(* ::Input::Initialization:: *)
(*obj:*)$assumption;
Symbol;
Value;


(* ::Input::Initialization:: *)
(*obj:*)$info;
Symbol;
CoordSystem;
Dimension;
Signature;
Version;
Author;
Comment;
Timestamp;


(* ::Input::Initialization:: *)
(*obj:*)$rule;
Symbol;
Value;


(* ::Input::Initialization:: *)
(*obj:*)$equation;
Symbol;
Value;


(* ::Input::Initialization:: *)
$solution;


(* ::Input::Initialization:: *)
Instantiate;
Lie;
Spin;
Curved;


(* ::Input::Initialization:: *)
$array;


(* ::Input::Initialization:: *)
$override=True;
Protect[$override];


SayHello;


(* ::Section::Closed:: *)
(*Begin private*)


Begin["`Private`"];


(* ::Section::Closed:: *)
(*Helpers*)


(* ::Input::Initialization:: *)
Letters::notin="`1` is not among the supported letter: `2`, to be used as indices";


(* ::Input::Initialization:: *)
Attributes[Resurrect]={HoldFirst};
Resurrect[symbol_]:=(
If[ValueQ[symbol],
symbol=Uncompress[Compress[symbol]]/.Removed[name_]:>Symbol[name]//Evaluate;
]
)


(* ::Input::Initialization:: *)
Attributes[Revive]={HoldFirst};
Revive[sol_]:=(
Unprotect[sol];
sol=Uncompress[Compress[sol]]/.Removed[name_]:>Symbol[name]//Evaluate;
Protected[sol];
(Function[{x},Resurrect[x],HoldFirst]@@MakeExpression[#])&/@Names["Global`"<>"*"];
System`$Assumptions//Resurrect;
)


(* ::Input::Initialization:: *)
MakeSymbol[name__]:=If[MemberQ[{name},None],None,Symbol[StringJoin[$context,Sequence@@(ToString/@{name})]]]


(* ::Input::Initialization:: *)
AppendKeepOrder[assoc1_,assoc2_,order_]:=AssociationThread[order->Normal@(Append[assoc1,assoc2][#]&/@order)]
AppendKeepOrder[assoc1_,assoc2_]:=AssociationThread[Keys@assoc1->Normal@(Append[assoc1,assoc2][#]&/@Keys@assoc1)]


(* ::Input::Initialization:: *)
metricExprQ[expr_]:=Module[{
Expr=Expand@expr,
tmp
},
If[Head@Expr===Plus,tmp=Level[Expr,1],tmp={Expr}];
(MatchQ[#,(_ Diff[_Symbol[]]\[CircleTimes]Diff[_Symbol[]]|Diff[_Symbol[]]\[CircleTimes]Diff[_Symbol[]])]&/@tmp//DeleteDuplicates)==={True}
]
metricExprQ[expr_,coord_]:=Module[{
Expr=Expand@expr,
tmp
},
If[Head@Expr===Plus,tmp=Level[Expr,1],tmp={Expr}];
(MatchQ[#,(_ Diff[_?(MemberQ[coord,#]&)]\[CircleTimes]Diff[_?(MemberQ[coord,#]&)]|Diff[_?(MemberQ[coord,#]&)]\[CircleTimes]Diff[_?(MemberQ[coord,#]&)])]&/@tmp//DeleteDuplicates)==={True}
]


(* ::Input::Initialization:: *)
$QuietEcho/:Set[$QuietEcho,True]:=($Pre=QuietEcho;Return@True;)
$QuietEcho/:Set[$QuietEcho,False]:=($Pre=.;Return@False;)


(* ::Input::Initialization:: *)
Letters[from_String,num_Integer]:=Module[{From,pos},
From=Select[Values@$Letters,MemberQ[#,from]&];
If[From==={},
Message[Letters::notin,from,Flatten@Values@$Letters],
From=First@From;
pos=First@First@Position[From,from];
If[num-(Length@From-pos)>0,
Return@(MakeSymbol/@Join[From[[pos;;-1]],From[[1;;num-(Length@From-pos)-1]]]),
Return@(MakeSymbol/@From[[pos;;pos+num-1]])
]
];
]
Letters[from_String]:=Module[{From,pos},
From=Select[Values@$Letters,MemberQ[#,from]&];
If[From==={},
Message[Letters::notin,from,Flatten@Values@$Letters],
From=First@From;
pos=First@First@Position[From,from];
Return@(MakeSymbol/@From[[pos;;-1]])
]
]
Letters[from_String,upDown_List]:=upDown*Letters[from,Length@upDown]
Letters[]:=MakeSymbol/@Flatten[Letters[#]&/@{"a","A","\[Alpha]","\[ScriptA]","\[ScriptCapitalA]","\[GothicA]","\[GothicCapitalA]","\[DoubleStruckA]","\[DoubleStruckCapitalA]"}]


(* ::Input::Initialization:: *)
$Pattern:=<|
"indexListSame"->_List?((SubsetQ[Letters["a"],#]||SubsetQ[Letters["A"],#]||SubsetQ[Letters["\[Alpha]"],#]||SubsetQ[Letters["\[ScriptA]"],#]||SubsetQ[Letters["\[ScriptCapitalA]"],#]||SubsetQ[Letters["\[GothicA]"],#]||SubsetQ[Letters["\[GothicCapitalA]"],#]||SubsetQ[Letters["\[DoubleStruckA]"],#]||SubsetQ[Letters["\[DoubleStruckCapitalA]"],#])&&(CountDistinct[#]===Length[#])&),
"indexListAny"->{_?(MemberQ[Letters[],#]&)..},
"upDownIndex"->(-_?(MemberQ[Letters[],#]&)|_?(MemberQ[Letters[],#]&))
|>


(* ::Input::Initialization:: *)
FirstIndex[expr_]:=Module[{x},ToString[First@Level[expr,1]/.Times[-1,x_]:>x]]


(* ::Input::Initialization:: *)
AddCompletion=FE`Evaluate[FEPrivate`AddSpecialArgCompletion[#]]&;


(* ::Input::Initialization:: *)
Attributes[GenFunctionRule]={HoldFirst};
GenFunctionRule[sol_[$function,key_]]:=Module[{head,arg,expr},
head=Head@sol[$function,key,Symbol];
arg=Level[sol[$function,key,Symbol],1]/.Thread[sol[$chart,sol[$chart][[1]][Symbol],Coord]->(MakeSymbol["xxxx",#]&/@sol[$chart,sol[$chart][[1]][Symbol],CNumber])];
expr=sol[$function,key,Expr]/.Thread[sol[$chart,sol[$chart][[1]][Symbol],Coord]->(MakeSymbol["xxxx",#]&/@sol[$chart,sol[$chart][[1]][Symbol],CNumber])];
Return@(head->Function[Evaluate@arg,Evaluate@expr])
]


(* ::Section:: *)
(*Constructors*)


(* ::Subsection::Closed:: *)
(*Messages*)


(* ::Input::Initialization:: *)
WeakValidate::assoc="`1` should be an association";
WeakValidate::keys="The keys `1` should be the same as the keys `2`";
WeakValidate::value="The value of key `1` of a `2` should be the same as `3` or match with its ReleaseHold";


(* ::Input::Initialization:: *)
StrongValidate::value="The value of key `1` of a `2` should match with `3`";


(* ::Subsection::Closed:: *)
(*Helpers*)


(* ::Input::Initialization:: *)
keysOf[object_]:=Keys@Options[object]
propKeysOf[object_]:=Identity[object]
autoKeysOf[object_]:=Identity[object]
optKeysOf[object_]:=Select[KeysOf[object],!MemberQ[Join[PropKeysOf[object],AutoKeysOf[object]],#]&]
matchOf[object_]:=KeyDrop[Association@Options[object],OptKeysOf[object]]


(* ::Input::Initialization:: *)
weakValidate[object_,assoc_]:=(
If[!AssociationQ[assoc],Message[WeakValidate::assoc,assoc];Throw@$Failed];
If[Keys@assoc=!=KeysOf[object],Message[WeakValidate::keys,Keys@assoc,KeysOf[object]];Throw@$Failed];
If[!(MatchQ[assoc[#],MatchOf[object][#]]||SameQ[assoc[#],MatchOf[object][#]]),
Message[WeakValidate::value,#,object,MatchOf[object][#]];
Throw@$Failed]&/@Join[PropKeysOf[object],AutoKeysOf[object]];
Return[assoc];
)
strongValidate[object_,assoc_]:=(
WeakValidate[object,assoc];
If[!(MatchQ[assoc[#],MatchOf[object][#]]),Message[StrongValidate::value,#,object,MatchOf[object][#]];Throw@$Failed]&/@PropKeysOf[object];
Return[assoc];
)
strongerValidate[object_,assoc_]:=(
StrongValidate[object,assoc];
If[!(MatchQ[assoc[#],MatchOf[object][#]]),Message[StrongValidate::value,#,object,MatchOf[object][#]];Throw@$Failed]&/@AutoKeysOf[object];
Return[assoc];
)
makeLoad[object_,assoc_]:=Module[{tmp=Uniformize[object,assoc]},
Return@DeleteCases[Odd]@DeleteCases[Even]@DeleteCases[None]@(Join[tmp[#]&/@PropKeysOf[object],(#->tmp[#]&)/@OptKeysOf[object]]);
]
uniformize[object_,assoc_]:=StrongValidate[object,assoc]


(* ::Input::Initialization:: *)
(*Include property methods like $manifold/:KeysOf[$manifold]:=keysOf[$manifold]*)
Apply[(#3/:#1[#3]:=#2[#3])&,#]&/@Flatten[Table[{$Properties[[ii]],$properties[[ii]],$Objects[[jj]]},{ii,1,Length@$Properties},{jj,1,Length@$Objects}],1];
(*Include methods like $manifold/:StrongValidate[$manifold,assoc_]:=strongValidate[$manifold,assoc]*)
Apply[(#3/:#1[#3,assoc_]:=#2[#3,assoc])&,#]&/@Flatten[Table[{$Methods[[ii]],$methods[[ii]],$Objects[[jj]]},{ii,1,Length@$Methods},{jj,1,Length@$Objects}],1];
(*Overload the methods as $manifold/:StrongValidate[$manifold[arg___]]:=StrongValidate[$manifold,$manifold[arg]]*)
(Attributes[#]={HoldFirst})&/@$Methods;
Apply[(#2/:#1[#2[arg___]]:=#1[#2,#2[arg]])&,#]&/@Flatten[Table[{$Methods[[ii]],$Objects[[jj]]},{ii,1,Length@$Methods},{jj,1,Length@$Objects}],1];
(*Make the construstors*)
(#[OptionsPattern[]]:=WeakValidate[#,AssociationThread[(KeysOf[#])->First@Table[OptionValue[#,ii],{ii,{KeysOf@#}}]]])&/@$Objects;


(* ::Input::Initialization:: *)
applyToComposite[method_,assoc_]:=<|
$info->AssociationThread[(Keys@assoc[$info])->(method[$info,#]&/@Values@assoc[$info])],
$manifold->AssociationThread[(Keys@assoc[$manifold])->(method[$manifold,#]&/@Values@assoc[$manifold])],
$metric->AssociationThread[(Keys@assoc[$metric])->(method[$metric,#]&/@Values@assoc[$metric])],
$frame->AssociationThread[(Keys@assoc[$frame])->(method[$frame,#]&/@Values@assoc[$frame])],
$spin->AssociationThread[(Keys@assoc[$spin])->(method[$spin,#]&/@Values@assoc[$spin])],
$bundle->AssociationThread[(Keys@assoc[$bundle])->(method[$bundle,#]&/@Values@assoc[$bundle])],
$covd->AssociationThread[(Keys@assoc[$covd])->(method[$covd,#]&/@Values@assoc[$covd])],
$chart->AssociationThread[(Keys@assoc[$chart])->(method[$chart,#]&/@Values@assoc[$chart])],
$basis->AssociationThread[(Keys@assoc[$basis])->(method[$basis,#]&/@Values@assoc[$basis])],
$form->AssociationThread[(Keys@assoc[$form])->(method[$form,#]&/@Values@assoc[$form])],
$tensor->AssociationThread[(Keys@assoc[$tensor])->(method[$tensor,#]&/@Values@assoc[$tensor])],
$spinor->AssociationThread[(Keys@assoc[$spinor])->(method[$spinor,#]&/@Values@assoc[$spinor])],
$function->AssociationThread[(Keys@assoc[$function])->(method[$function,#]&/@Values@assoc[$function])],
$constant->AssociationThread[(Keys@assoc[$constant])->(method[$constant,#]&/@Values@assoc[$constant])],
$parameter->AssociationThread[(Keys@assoc[$parameter])->(method[$parameter,#]&/@Values@assoc[$parameter])],
$assumption->AssociationThread[(Keys@assoc[$assumption])->(method[$assumption,#]&/@Values@assoc[$assumption])],
$rule->AssociationThread[(Keys@assoc[$rule])->(method[$rule,#]&/@Values@assoc[$rule])],
$equation->AssociationThread[(Keys@assoc[$equation])->(method[$equation,#]&/@Values@assoc[$equation])]
|>


(* ::Input::Initialization:: *)
extractIdentifier[value_]:=(
If[MatchQ[value,_Symbol[___]],Return@Head@value];
If[MatchQ[value,(_Symbol|_String)],Return@value];
)


(* ::Subsection::Closed:: *)
(*Unit constructors*)


(* ::Input::Initialization:: *)
Options[$manifold]=Join[{
Symbol->_Symbol,
Dimension->_Integer?(#>0&),
Index->$Pattern["indexListSame"],
VBundle->_Symbol
},Normal@KeyDrop[Options[DefManifold],Tangent]];
(*unique property methods*)
$manifold/:PropKeysOf[$manifold]:={Symbol,Dimension,Index}
$manifold/:AutoKeysOf[$manifold]:={VBundle}
(*unique methods*)
$manifold/:Uniformize[$manifold,assoc_]:=Module[{tmp=StrongValidate[$manifold,assoc]},
tmp[VBundle]=MakeSymbol[Tangent,assoc[Symbol]];
Return@StrongValidate[$manifold,tmp]
]


(* ::Input::Initialization:: *)
Options[$autoTensor]={
Symbol->_Symbol,
Routine->_,
Dependency->_,
Value->_
};
(*unique property methods*)
$autoTensor/:PropKeysOf[$autoTensor]:={Symbol};
$autoTensor/:AutoKeysOf[$autoTensor]:={Routine,Dependency,Value};
(*unique methods*)


(* ::Input::Initialization:: *)
Options[$metric]=Join[{
SignDet->(1|-1),
Symbol->_Symbol[-$Pattern["upDownIndex"],-$Pattern["upDownIndex"]],
CovD->_Symbol,
Manifold->_Symbol,
(*Expr->(_ Diff[_Symbol[]]\[CircleTimes]Diff[_Symbol[]]|Diff[_Symbol[]]\[CircleTimes]Diff[_Symbol[]]|_?((#===Plus)&)[(_ Diff[_Symbol[]]\[CircleTimes]Diff[_Symbol[]]|Diff[_Symbol[]]\[CircleTimes]Diff[_Symbol[]])..]),*)
Expr->_,
Routine->_,
Dependency->_,
Value->_,
epsilon->$autoTensor[],
Christoffel->$autoTensor[],
Riemann->$autoTensor[],
Ricci->$autoTensor[],
RicciScalar->$autoTensor[],
Einstein->$autoTensor[],
Weyl->$autoTensor[],
TFRicci->$autoTensor[],
Kretschmann->$autoTensor[],
SymRiemann->$autoTensor[],
Schouten->$autoTensor[],
Det->$autoTensor[],
ExtrinsicK->($autoTensor[]|None),
Acceleration->($autoTensor[]|None)
},Join[
Normal@KeyDrop[Options[DefCovD],{Torsion,FromMetric,ExtendedFrom,OtherDependencies,OrthogonalTo,ProjectedWith,WeightedWithBasis,ProtectNewSymbol,Master,DefInfo}],Options[DefMetric]
]
];
(*unique property methods*)
$metric/:PropKeysOf[$metric]:={SignDet,Symbol,CovD,Manifold}
$metric/:AutoKeysOf[$metric]:={Expr,Routine,Dependency,Value,epsilon,Christoffel,Riemann,Ricci,RicciScalar,Einstein,Weyl,TFRicci,Kretschmann,SymRiemann,Schouten,Det,ExtrinsicK,Acceleration}
(*unique methods*)
$metric/:Uniformize[$metric,assoc_]:=Module[{tmp=StrongValidate[$metric,assoc],table,checkNoneList},
If[assoc[InducedFrom]===Null,
tmp[ExtrinsicK]=None;
tmp[Acceleration]=None;,
tmp[ExtrinsicK]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[ExtrinsicK,assoc[InducedFrom][[1]]]];
tmp[Acceleration]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Acceleration,assoc[InducedFrom][[2]]]];
];
(tmp[#]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[#,Head@assoc[Symbol]]])&/@{epsilon,Det};
(tmp[#]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[#,assoc[CovD]]])&/@{Christoffel,Riemann,Ricci,RicciScalar,Einstein,Weyl,TFRicci,Kretschmann,SymRiemann,Schouten};
Return@StrongValidate[$metric,tmp];
];
$metric/:MakeLoad[$metric,assoc_]:=Module[{tmp=Uniformize[$metric,assoc]},
Return@DeleteCases[Odd]@DeleteCases[Even]@DeleteCases[None]@(Join[tmp[#]&/@{SignDet,Symbol,CovD},(#->tmp[#]&)/@OptKeysOf[$metric]]);
]


(* ::Input::Initialization:: *)
Options[$bundle]=Join[{
Symbol->_Symbol,
Manifold->_Symbol,
Dimension->_Integer?(#>0&),
Index->$Pattern["indexListSame"],
Metric->(_Symbol|None),
Inv->$autoTensor[]
},Normal@KeyDrop[Options[DefVBundle],Tangent]];
(*unique property methods*)
$bundle/:PropKeysOf[$bundle]:={Symbol,Manifold,Dimension,Index,Metric}
$bundle/:AutoKeysOf[$bundle]:={Inv}
(*unique methods*)
$bundle/:Uniformize[$bundle,assoc_]:=Module[{tmp=assoc},
If[tmp[Metric]===MatchOf[$bundle][Metric],
tmp[Metric]=None
];
tmp[Inv]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Inv,assoc[Symbol],"f"]];
Return@StrongValidate[$bundle,tmp];
]


(* ::Input::Initialization:: *)
Options[$covd]=Join[{
Symbol->_Symbol[-$Pattern["upDownIndex"]],
VBundle->{_Symbol...},
Manifold->_Symbol,
Torsion->($autoTensor[]|True|False|None),
Christoffel->$autoTensor[],
Riemann->$autoTensor[],
Ricci->$autoTensor[],
AChristoffel->($autoTensor[]|None),
FRiemann->($autoTensor[]|None)
},Normal@KeyDrop[Options[DefCovD],Torsion]];
(*unique property methods*)
$covd/:PropKeysOf[$covd]:={Symbol,VBundle,Manifold}
$covd/:AutoKeysOf[$covd]:={Torsion,Christoffel,Riemann,Ricci,AChristoffel,FRiemann}
(*unique methods*)
$covd/:Uniformize[$covd,assoc_]:=Module[{tmp=assoc},
If[tmp[VBundle]===MatchOf[$covd][VBundle],
tmp[VBundle]={}
];
tmp=StrongValidate[$covd,tmp];

If[tmp[Torsion]===MatchOf[$covd][Torsion]||tmp[Torsion]===False,
tmp[Torsion]=None,
tmp[Torsion]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Torsion,Head@assoc[Symbol]]]
];
If[tmp[$bundle]==={},
tmp[AChristoffel]=None;
tmp[FRiemann]=None;,
tmp[AChristoffel]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[AChristoffel,Head@assoc[Symbol]]];
tmp[FRiemann]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[FRiemann,Head@assoc[Symbol]]];
];
tmp[Christoffel]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Christoffel,Head@assoc[Symbol]]];
tmp[Riemann]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Riemann,Head@assoc[Symbol]]];
tmp[Ricci]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Ricci,Head@assoc[Symbol]]];
Return@StrongValidate[$covd,tmp];
]
$covd/:MakeLoad[$covd,assoc_]:=Module[{tmp=Uniformize[$covd,assoc]},
Return@Append[
DeleteCases[Odd]@DeleteCases[Even]@DeleteCases[None]@(Join[tmp[#]&/@{Symbol,VBundle},(#->tmp[#]&)/@OptKeysOf[$covd]]),
Torsion->If[assoc[Torsion]===True,True,False]
]
]


(* ::Input::Initialization:: *)
Options[$frame]=Join[{
Symbol->_Symbol[-$Pattern["upDownIndex"],$Pattern["upDownIndex"]],
Metric->_Symbol[-$Pattern["upDownIndex"],-$Pattern["upDownIndex"]],
Index->$Pattern["indexListSame"],
Manifold->_Symbol,
VBundle->_Symbol,
Expr->_,
Vielbein->$autoTensor[],
MetricTensor->$autoTensor[],
Det->$autoTensor[],
epsilon->$autoTensor[]
},Options[DefFrameBundle]];
(*unique property methods*)
$frame/:PropKeysOf[$frame]:={Symbol,Metric,Index,Manifold}
$frame/:AutoKeysOf[$frame]:={VBundle,Expr,Vielbein,MetricTensor,Det,epsilon}
(*unique methods*)
$frame/:Uniformize[$frame,assoc_]:=Module[{tmp=StrongValidate[$frame,assoc]},
tmp[VBundle]=MakeSymbol[Frame,tmp[Manifold]];
tmp[Vielbein]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Head@assoc[Symbol]]];
tmp[MetricTensor]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Head@assoc[Metric]]];
tmp[Det]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Det,Head@assoc[Symbol]]];
tmp[epsilon]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[epsilon,Head@assoc[Metric]]];
Return@StrongValidate[$frame,tmp];
]
$frame/:MakeLoad[$frame,assoc_]:=Module[{tmp=Uniformize[$frame,assoc]},
Return@DeleteCases[Odd]@DeleteCases[Even]@DeleteCases[None]@(Join[tmp[#]&/@{Symbol,Metric,Index},(#->tmp[#]&)/@OptKeysOf[$frame]]);
]


(* ::Input::Initialization:: *)
Options[$spin]=Join[{
Symbol->_Symbol[-$Pattern["upDownIndex"],-$Pattern["upDownIndex"],-$Pattern["upDownIndex"]],
Index->$Pattern["indexListSame"],
Metric->{_Symbol,_Symbol},
CovD->_Symbol,
Manifold->_Symbol,
VBundle->_Symbol,
Routine->_,
Dependency->_,
Value->_,
Gamma-><|
x_Symbol-><|
1->$autoTensor[],
2->($autoTensor[]|None),
3->($autoTensor[]|None),
4->($autoTensor[]|None),
5->($autoTensor[]|None),
6->($autoTensor[]|None),
7->($autoTensor[]|None),
8->($autoTensor[]|None),
9->($autoTensor[]|None),
10->($autoTensor[]|None),
11->($autoTensor[]|None)
|>,
y_Symbol-><|
1->$autoTensor[],
2->($autoTensor[]|None),
3->($autoTensor[]|None),
4->($autoTensor[]|None),
5->($autoTensor[]|None),
6->($autoTensor[]|None),
7->($autoTensor[]|None),
8->($autoTensor[]|None),
9->($autoTensor[]|None),
10->($autoTensor[]|None),
11->($autoTensor[]|None)
|>
|>,
Riemann->$autoTensor[],
Ricci->$autoTensor[],
RicciScalar->$autoTensor[],
AChristoffel->$autoTensor[],
FRiemann->$autoTensor[]
},Options[DefSpinConnection]];
(*unique property methods*)
$spin/:PropKeysOf[$spin]:={Symbol,Index,Metric,CovD,Manifold}
$spin/:AutoKeysOf[$spin]:={VBundle,Routine,Dependency,Value,Gamma,Riemann,Ricci,RicciScalar,AChristoffel,FRiemann}
(*unique methods*)
$spin/:Uniformize[$spin,assoc_]:=Module[{tmp=StrongValidate[$spin,assoc]},
tmp[VBundle]=MakeSymbol["Spin",tmp[Manifold]];
tmp[AChristoffel]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[AChristoffel,assoc[CovD]]];
tmp[FRiemann]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[FRiemann,assoc[CovD]]];
tmp[Riemann]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Riemann,Head@assoc[Symbol]]];
tmp[Ricci]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Ricci,Head@assoc[Symbol]]];
tmp[RicciScalar]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[RicciScalar,Head@assoc[Symbol]]];
tmp[Gamma]=<|
assoc[Metric][[1]]-><|
1->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[1]],1]],
2->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[1]],2]],
3->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[1]],3]],
4->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[1]],4]],
5->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[1]],5]],
6->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[1]],6]],
7->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[1]],7]],
8->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[1]],8]],
9->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[1]],9]],
10->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[1]],10]],
11->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[1]],11]]
|>,
assoc[Metric][[2]]-><|
1->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[2]],1]],
2->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[2]],2]],
3->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[2]],3]],
4->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[2]],4]],
5->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[2]],5]],
6->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[2]],6]],
7->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[2]],7]],
8->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[2]],8]],
9->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[2]],9]],
10->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[2]],10]],
11->AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[Gamma,assoc[Metric][[2]],11]]
|>
|>;
Return@StrongValidate[$spin,tmp];
]
$spin/:MakeLoad[$spin,assoc_]:=Module[{tmp=Uniformize[$spin,assoc]},
Return@{{tmp[Metric][[1]],tmp[Index]},Join[{tmp[Symbol],tmp[CovD]},(#->tmp[#]&)/@OptKeysOf[$spin]]}
]


(* ::Input::Initialization:: *)
Options[$chart]=Join[{
Symbol->_Symbol,
Manifold->_Symbol,
CNumber->{_Integer..},
Coord->{_Symbol[]..}
},Options[DefChart]];
(*unique property methods*)
$chart/:PropKeysOf[$chart]:={Symbol,Manifold,CNumber,Coord};
$chart/:AutoKeysOf[$chart]:={}
(*unique methods*)


(* ::Input::Initialization:: *)
Options[$basis]=Join[{
Symbol->_Symbol,
VBundle->_Symbol,
CNumber->{_Integer..}
},Options[DefBasis]];
(*unique property methods*)
$basis/:PropKeysOf[$basis]:={Symbol,VBundle,CNumber};
$basis/:AutoKeysOf[$basis]:={}
(*unique methods*)


(* ::Input::Initialization:: *)
Options[$tensor]=Join[{
Symbol->(_Symbol[$Pattern["upDownIndex"]...]|0),
Manifold->_Symbol,
Symmetry->(_Antisymmetric|_Symmetric|_StrongGenSet|None),
Grassmann->(Even|Odd),
Routine->_,
Dependency->_,
Value->_
},Normal@KeyDrop[Options[DefTensor],GradeOfTensor]];
(*unique property methods*)
$tensor/:PropKeysOf[$tensor]:={Symbol,Manifold,Symmetry,Grassmann};
$tensor/:AutoKeysOf[$tensor]:={Routine,Dependency,Value};
(*unique methods*)
$tensor/:Uniformize[$tensor,assoc_]:=Module[{tmp=assoc},
If[tmp[Grassmann]===MatchOf[$tensor][Grassmann],
tmp[Grassmann]=Even;
];
If[tmp[Symmetry]===MatchOf[$tensor][Symmetry],
tmp[Symmetry]=None;
];
Return@StrongValidate[$tensor,tmp];
]


(* ::Input::Initialization:: *)
Options[$form]=Join[{
Symbol->(_Symbol[$Pattern["upDownIndex"]...]|0),
Manifold->_Symbol,
Degree->_Integer,
Tensor->(_Symbol|None),
Expr->_
},Options[DefDiffForm]];
(*unique property methods*)
$form/:PropKeysOf[$form]:={Symbol,Manifold,Degree,Tensor};
$form/:AutoKeysOf[$form]:={Expr};
(*unique methods*)
$form/:Uniformize[$form,assoc_]:=Module[{tmp=assoc},
If[tmp[Tensor]===MatchOf[$form][Tensor],
tmp[Tensor]=None;
];
Return@StrongValidate[$form,tmp];
]
$form/:MakeLoad[$form,assoc_]:=Module[{tmp=Uniformize[$form,assoc]},
Return@Join[tmp[#]&/@{Symbol,Manifold,Degree},(#->tmp[#]&)/@OptKeysOf[$form]]
]


(* ::Input::Initialization:: *)
Options[$spinor]=Join[{
Symbol->(_Symbol[$Pattern["upDownIndex"]...]|0),
Manifold->_Symbol,
Symmetry->(_Antisymmetric|_Symmetric|_StrongGenSet|None),
Grassmann->(Even|Odd),
Routine->_,
Dependency->_,
Value->_,
bar->$autoTensor[]
},Options[DefSpinor]];
(*unique property methods*)
$spinor/:PropKeysOf[$spinor]:={Symbol,Manifold,Symmetry,Grassmann};
$spinor/:AutoKeysOf[$spinor]:={Routine,Dependency,Value,bar};
(*unique methods*)
$spinor/:Uniformize[$spinor,assoc_]:=Module[{tmp=assoc},
If[tmp[Grassmann]===MatchOf[$spinor][Grassmann],
tmp[Grassmann]=Even;
];
If[tmp[Symmetry]===MatchOf[$spinor][Symmetry],
tmp[Symmetry]=None;
];
tmp[bar]=AppendKeepOrder[$autoTensor[],Symbol->MakeSymbol[bar,Head@assoc[Symbol]]];
Return@StrongValidate[$spinor,tmp];
]


(* ::Input::Initialization:: *)
Options[$function]=Join[{
Symbol->_Symbol[___],
Expr->_
},Options[DefScalarFunction]];
(*unique property methods*)
$function/:PropKeysOf[$function]:={Symbol};
$function/:AutoKeysOf[$function]:={Expr};
(*unique methods*)
$function/:MakeLoad[$function,assoc_]:=Module[{tmp=Uniformize[$function,assoc]},
Return@Join[{Head@tmp[Symbol]},(#->tmp[#]&)/@OptKeysOf[$function]]
]


(* ::Input::Initialization:: *)
Options[$constant]=Join[{
Symbol->_Symbol,
Expr->_
},Options[DefConstantSymbol]];
(*unique property methods*)
$constant/:PropKeysOf[$constant]:={Symbol};
$constant/:AutoKeysOf[$constant]:={Expr};
(*unique methods*)


(* ::Input::Initialization:: *)
Options[$parameter]=Join[{
Symbol->_Symbol
},Options[DefParameter]];
(*unique property methods*)
$parameter/:PropKeysOf[$parameter]:={Symbol};
$parameter/:AutoKeysOf[$parameter]:={};
(*unique methods*)


(* ::Input::Initialization:: *)
Options[$assumption]={
Symbol->_String,
Value->{(_Less|_LessEqual|_Greater|_GreaterEqual|_Inequality|_Element)...}
};
(*unique property methods*)
$assumption/:PropKeysOf[$assumption]:={Symbol};
$assumption/:AutoKeysOf[$assumption]:={Value};
(*unique methods*)
$assumption/:Uniformize[$assumption,assoc_]:=Module[{tmp=assoc},
If[tmp[Value]===MatchOf[$assumption][Value],
tmp[Value]={};
];
Return@StrongValidate[$assumption,tmp];
]
$assumption/:MakeLoad[$assumption,assoc_]:=Module[{tmp=Uniformize[$assumption,assoc]},
Return@(assoc[Value])
]


(* ::Input::Initialization:: *)
Options[$info]={
Symbol->_String,
Dimension->_Integer,
Signature->_String,
CoordSystem->_String,
Version->_String,
Author->_String,
Comment->_String,
Timestamp->_String
};
(*unique property methods*)
$info/:PropKeysOf[$info]:={Symbol,CoordSystem,Version,Comment};
$info/:AutoKeysOf[$info]:={Dimension,Signature,Timestamp,Author};
(*unique methods*)
$info/:Uniformize[$info,assoc_]:=Module[{tmp=assoc,id},
If[tmp[Version]===MatchOf[$info][Version],
tmp[Version]="Original";
];
If[tmp[Comment]===MatchOf[$info][Comment],
tmp[Comment]="None";
];
tmp[Timestamp]=DateString["ISODateTime"];
SeedRandom[$MachineID];
id=ToString@RandomInteger[{10000,99999}];
SeedRandom[];
tmp[Author]=StringJoin[$Username,"-",$MachineName,"-",id];
Return@StrongValidate[$info,tmp];
]


(* ::Input::Initialization:: *)
Options[$rule]={
Symbol->_String,
Value->_
};
(*unique property methods*)
$rule/:PropKeysOf[$rule]:={Symbol,Value};
$rule/:AutoKeysOf[$rule]:={};
(*unique methods*)


(* ::Input::Initialization:: *)
Options[$equation]={
Symbol->_Symbol,
Value->_
};
(*unique property methods*)
$equation/:PropKeysOf[$equation]:={Symbol,Value};
$equation/:AutoKeysOf[$equation]:={};
(*unique methods*)


(* ::Subsection::Closed:: *)
(*Composite constructors*)


(* ::Input::Initialization:: *)
$solution::assoc="The value of key `1` should be an association";
$solution::liassoc="The value of key `1` should be a list of associations";
Options[$solution]={
$info->{$info[]},
$manifold->{$manifold[]},
$metric->{$metric[]...},
$frame->{$frame[]},
$spin->{$spin[]},
$bundle->{$bundle[]...},
$covd->{$covd[]...},
$chart->{$chart[]...},
$basis->{$basis[]...},
$form->{$form[]...},
$tensor->{$tensor[]...},
$spinor->{$spinor[]...},
$function->{$function[]...},
$constant->{$constant[]...},
$parameter->{$parameter[]...},
$assumption->{$assumption[]...},
$rule->{$rule[]...},
$equation->{$equation[]...}
};
$solution/:KeysOf[$solution]:=keysOf[$solution]
$solution/:PropKeysOf[$solution]:=keysOf[$solution]
$solution/:AutoKeysOf[$solution]:={}
$solution/:OptKeysOf[$solution]:=optKeysOf[$solution]
$solution/:MatchOf[$solution]:=matchOf[$solution]
$solution[OptionsPattern[]]:=Module[{tmp=Association@Options[$solution],tmp2},
Function[{obj},If[OptionValue[obj]===MatchOf[$solution][obj],
tmp[obj]=<||>,
If[MatchQ[OptionValue[obj],{_Association...}],
tmp2=Apply[obj,#]&/@(Normal[ReleaseHold[#]]&/@OptionValue[obj]);
tmp[obj]=AssociationThread[(extractIdentifier[#[Symbol]]&/@tmp2)->tmp2],
Message[$solution::liassoc,obj];Throw@$Failed
]
]]/@PropKeysOf[$solution];
Return[tmp]
]
WeakValidate[$solution,assoc_]:=Module[{},
If[!AssociationQ[assoc],Message[WeakValidate::assoc,assoc];Throw@$Failed];
If[Keys@assoc=!=KeysOf[$solution],Message[WeakValidate::keys,Keys@assoc,KeysOf[$solution]];Throw@$Failed];
Return@applyToComposite[WeakValidate,assoc];
]
StrongValidate[$solution,assoc_]:=Module[{},WeakValidate[$solution,assoc];Return@applyToComposite[StrongValidate,assoc];]
StrongerValidate[$solution,assoc_]:=Module[{},StrongValidate[$solution,assoc];Return@applyToComposite[StrongerValidate,assoc];]
Uniformize[$solution,assoc_]:=Module[{tmp=assoc},
tmp=applyToComposite[Uniformize,tmp];
If[tmp[$form]=!=<||>,
If[tmp[$form,#,Tensor]=!=None,
tmp[$tensor]=Append[
tmp[$tensor],
tmp[$form,#,Tensor]->$tensor[
Symbol->tmp[$form,#,Tensor]@@Join[-tmp[$manifold,tmp[$form,#,Manifold],Index][[1;;tmp[$form,#,Degree]]],Level[tmp[$form,#,Symbol],1]],
Manifold->tmp[$form,#,Manifold],
Symmetry->Antisymmetric[Range[1,tmp[$form,#,Degree]]],
Grassmann->Even,
Routine-><|
Seed->Hold@ToArray[$solution][$solution[$form,#,Expr]],
Chain->
Which[
tmp[$form,#,Degree]===1,{
Seed->Permutations[{-1}],
Permutations[{-1}]->Permutations[{1}]
},
tmp[$form,#,Degree]===2,{
Seed->Permutations[{-1,-1}],
Permutations[{-1,-1}]->Permutations[{1,-1}],
Permutations[{1,-1}]->Permutations[{1,1}]
},
tmp[$form,#,Degree]===3,{
Seed->Permutations[{-1,-1,-1}],
Permutations[{-1,-1,-1}]->Permutations[{1,-1,-1}],
Permutations[{1,-1,-1}]->Permutations[{1,1,-1}],
Permutations[{1,1,-1}]->Permutations[{1,1,1}]
},
tmp[$form,#,Degree]===4,{
Seed->Permutations[{-1,-1,-1,-1}],
Permutations[{-1,-1,-1,-1}]->Permutations[{1,-1,-1,-1}],
Permutations[{1,-1,-1,-1}]->Permutations[{1,1,-1,-1}],
Permutations[{1,1,-1,-1}]->Permutations[{1,1,1,-1}],
Permutations[{1,1,1,-1}]->Permutations[{1,1,1,1}]
},
tmp[$form,#,Degree]=!=1||tmp[$form,#,Degree]=!=2||tmp[$form,#,Degree]=!=3||tmp[$form,#,Degree]=!=4,_
],
Map:>$Map,ParallelMap:>$ParallelMap
|>
]
]
]&/@Keys@tmp[$form]
];
If[tmp[$metric]=!=<||>,
If[tmp[$metric,#,InducedFrom]=!=Null,
tmp[$tensor]=Append[
tmp[$tensor],
tmp[$metric,#,InducedFrom][[2]]->$tensor[
Symbol->tmp[$metric,#,InducedFrom][[2]][-(tmp[$manifold,tmp[$metric,#,Manifold],Index])[[1]]],
Manifold->tmp[$metric,#,Manifold],
Symmetry->None,
Grassmann->Even
]
]
]&/@Keys@tmp[$metric]
];
Return@StrongValidate[$solution,tmp]
]
($solution/:#[$solution[arg___]]:=#[$solution,$solution[arg]])&/@$Methods;


(* ::Section:: *)
(*Load/Unload*)


(* ::Subsection::Closed:: *)
(*Helpers*)


(* ::Input::Initialization:: *)
Attributes[AttachTVs]={HoldFirst};
AttachTVs[sol_[keyList__]]:=Module[{
symbol=extractIdentifier@sol[Sequence[keyList,Symbol]],
value=sol[Sequence[keyList,Value]]
},
If[value=!=_,
TensorValIDs[symbol]^=Keys@value;
(Evaluate@symbol/:TensorValues[symbol,Keys@#]=Values[#])&/@Thread[Reverse@(Level[#,1][[2]]&/@Keys@value)->Reverse@Values[value]];
]
]


(* ::Subsection::Closed:: *)
(*Single*)


(* ::Input::Initialization:: *)
mapAtKeyEveryWhere[assoc_,fun_,key_]:=MapAt[fun,assoc,Append[Key[key]]/@Position[assoc,KeyValuePattern[key->_]]];


(* ::Input::Initialization:: *)
Attributes[Load]={HoldFirst};
Options[Load]={Verbose:>$Verbose,Overwrite:>$Overwrite};
Attributes[Unload]={HoldFirst};
Options[Unload]={Verbose:>$Verbose};


(* ::Input::Initialization:: *)
Load[sol_[$manifold],key_Symbol,OptionsPattern[]]:=Module[{tmp,log=$Log},
$DefInfoQ=$CVVerbose=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
Check[DefManifold@@MakeLoad[$manifold,sol[$manifold,key]],Throw@$Failed];
$DefInfoQ=$CVVerbose=$Verbose;
AppendTo[$Log,{Style["Loaded",Darker@Green],ToString@$manifold,ToString@sol[$manifold,key,Symbol]}];
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[$manifold]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Unload[sol_[$manifold],key_Symbol,OptionsPattern[]]:=Module[{tmp,keyStr=ToString@key,log=$Log},
$UndefInfoQ=OptionValue[Verbose];
Check[UndefTensor@#,Throw@$Failed]&/@VisitorsOf[sol[$manifold,key,Symbol]];
Check[UndefManifold@sol[$manifold,key,Symbol],Throw@$Failed];
(*sol=Map[Evaluate,Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],All];*)
sol=mapAtKeyEveryWhere[Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],Evaluate,Symbol];
$UndefInfoQ=$Verbose;
AppendTo[$Log,{Style["Unloaded",Red],ToString@$manifold,keyStr}];
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[$manifold]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Load[sol_[$tensor],key_Symbol,OptionsPattern[]]:=Module[{tmp,log=$Log},
$DefInfoQ=$CVVerbose=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
Check[If[sol[$tensor,key,Grassmann]===Even,DefEvenTensor,DefOddTensor]@@MakeLoad[$tensor,sol[$tensor,key]],Throw@$Failed];
AttachTVs[sol[$tensor,key]];
$DefInfoQ=$CVVerbose=$Verbose;
AppendTo[$Log,{Style["Loaded",Darker@Green],ToString@$tensor,ToString@key}];
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[$tensor]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Unload[sol_[$tensor],key_Symbol,OptionsPattern[]]:=Module[{tmp,keyStr=ToString@key,log=$Log},
$UndefInfoQ=OptionValue[Verbose];
Check[UndefTensor@key,Throw@$Failed];
(*sol=Map[Evaluate,Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],All];*)
sol=mapAtKeyEveryWhere[Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],Evaluate,Symbol];
$UndefInfoQ=$Verbose;
AppendTo[$Log,{Style["Unloaded",Red],ToString@$tensor,keyStr}];
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[$tensor]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Load[sol_[$metric],key_Symbol,OptionsPattern[]]:=Module[{tmp,log=$Log},
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
If[sol[$metric,key,InducedFrom]=!=Null,
QuietEcho@Load[sol[$tensor],sol[$metric,key,InducedFrom][[2]],Verbose->OptionValue[Verbose]];
];
$DefInfoQ=$CVVerbose=OptionValue[Verbose];
Check[DefMetric@@MakeLoad[$metric,sol[$metric,key]],Throw@$Failed];
AttachTVs[sol[$metric,key]];
AttachTVs[sol[$metric,key,#]]&/@(DeleteCases[Null]@(If[MatchQ[Values@#,MatchOf[$autoTensor]],Keys@#]&/@Normal@sol[$metric,key]));
$DefInfoQ=$CVVerbose=$Verbose;
AppendTo[$Log,{Style["Loaded",Darker@Green],ToString@$metric,ToString@key}];
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[$metric]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Unload[sol_[$metric],key_Symbol,OptionsPattern[]]:=Module[{tmp,keyStr=ToString@key,log=$Log,spinKey=First@Keys@sol[$spin]},
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
If[sol[$metric,key,InducedFrom]=!=Null,
QuietEcho@Unload[sol[$tensor],sol[$metric,key,InducedFrom][[2]],Verbose->OptionValue[Verbose]];
];
$UndefInfoQ=OptionValue[Verbose];
Check[UndefTensor@MakeSymbol[Perturbation,key],Throw@$Failed];
Check[UndefParameter@MakeSymbol[PerturbationParameter,key],Throw@$Failed];
Check[UndefMetric@key,Throw@$Failed];
(*sol=Map[Evaluate,Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],All];*)
sol=mapAtKeyEveryWhere[Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],Evaluate,Symbol];
If[CovDQ[sol[$metric,MakeSymbol[keyStr],CovD]],Check[UndefCovD[sol[$metric,MakeSymbol[keyStr],CovD]],Throw@$Failed]];
(*sol=Map[Evaluate,Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],All];*)
sol=mapAtKeyEveryWhere[Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],Evaluate,Symbol];
If[InertHeadQ[MakeSymbol[CovD,sol[$metric,MakeSymbol[keyStr],CovD]]],Check[UndefInertHead[MakeSymbol[CovD,sol[$metric,MakeSymbol[keyStr],CovD]]],Throw@$Failed]];
(*sol=Map[Evaluate,Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],All];*)
sol=mapAtKeyEveryWhere[Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],Evaluate,Symbol];
sol[$spin,spinKey,Gamma]=Association[Uncompress@Compress@Normal@sol[$spin,spinKey,Gamma]/.Removed[x_]:>MakeSymbol[x]];
$UndefInfoQ=$Verbose;
AppendTo[$Log,{Style["Unloaded",Red],ToString@$metric,keyStr}];
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[$metric]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Load[sol_[$frame],key_Symbol,OptionsPattern[]]:=Module[{tmp,log=$Log},
$DefInfoQ=$CVVerbose=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
Check[DefFrameBundle@@MakeLoad[$frame,sol[$frame,key]],Throw@$Failed];
AttachTVs[sol[$frame,key,#]]&/@(DeleteCases[Null]@(If[MatchQ[Values@#,MatchOf[$autoTensor]],Keys@#]&/@Normal@sol[$frame,key]));
$DefInfoQ=$CVVerbose=$Verbose;
AppendTo[$Log,{Style["Loaded",Darker@Green],ToString@$frame,ToString@Head@sol[$frame,key,Symbol]}];
AppendTo[$Log,{Style["Loaded",Darker@Green],ToString@$metric,ToString@Head@sol[$frame,key,Metric]}];
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[$frame]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Unload[sol_[$frame],key_Symbol,OptionsPattern[]]:=Module[{tmp,keyStr=ToString@key,log=$Log,metricKeyStr=ToString@Head@sol[$frame,key,Metric]},
$UndefInfoQ=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
Check[UndefFrameBundle@(Head@sol[$frame,key,Symbol]),Throw@$Failed];
(*sol=Map[Evaluate,Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],All];*)
sol=mapAtKeyEveryWhere[Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],Evaluate,Symbol];
$UndefInfoQ=$Verbose;
AppendTo[$Log,{Style["Unloaded",Red],ToString@$frame,keyStr}];
AppendTo[$Log,{Style["Unloaded",Red],ToString@$metric,metricKeyStr}];
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[$frame]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Load[sol_[$spin],key_Symbol,OptionsPattern[]]:=Module[{tmp,log=$Log},
$DefInfoQ=$CVVerbose=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
Check[DefSpinStructure@@MakeLoad[$spin,sol[$spin,key]][[1]],Throw@$Failed];
Check[DefSpinConnection@@MakeLoad[$spin,sol[$spin,key]][[2]],Throw@$Failed];
AttachTVs[sol[$spin,key]];
AttachTVs[sol[$spin,key,#]]&/@(DeleteCases[Null]@(If[MatchQ[Values@#,MatchOf[$autoTensor]],Keys@#]&/@Normal@sol[$spin,key]));
AttachTVs[sol[$spin,key,Gamma,extractIdentifier@sol[$metric][[1]][Symbol],#]]&/@Keys@sol[$spin,key,Gamma,extractIdentifier@sol[$metric][[1]][Symbol]];
AttachTVs[sol[$spin,key,Gamma,extractIdentifier@sol[$frame][[1]][MetricTensor,Symbol],#]]&/@Keys@sol[$spin,key,Gamma,extractIdentifier@sol[$frame][[1]][MetricTensor,Symbol]];
$DefInfoQ=$CVVerbose=$Verbose;
AppendTo[$Log,{Style["Loaded",Darker@Green],ToString@$spin,ToString@Head@sol[$spin,key,Symbol]}];
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[$spin]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Unload[sol_[$spin],key_Symbol,OptionsPattern[]]:=Module[{tmp,keyStr=ToString@key,log=$Log},
$UndefInfoQ=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
Check[UndefSpinConnection@(Head@sol[$spin,key,Symbol]),Throw@$Failed];
Check[UndefSpinStructure@(sol[$spin,key,Metric][[1]]),Throw@$Failed];
(*sol=Map[Evaluate,Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],All];*)
sol=mapAtKeyEveryWhere[Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],Evaluate,Symbol];
$UndefInfoQ=$Verbose;
AppendTo[$Log,{Style["Unloaded",Red],ToString@$spin,keyStr}];
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[$spin]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Load[sol_[$covd],key_Symbol,OptionsPattern[]]:=Module[{tmp,log=$Log},
$DefInfoQ=$CVVerbose=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
Check[DefCovD@@MakeLoad[$covd,sol[$covd,key]],Throw@$Failed];
AttachTVs[sol[$covd,key,#]]&/@(DeleteCases[Null]@(If[MatchQ[Values@#,MatchOf[$autoTensor]],Keys@#]&/@Normal@sol[$covd,key]));
$DefInfoQ=$CVVerbose=$Verbose;
AppendTo[$Log,{Style["Loaded",Darker@Green],ToString@$covd,ToString@key}];
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[$covd]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Unload[sol_[$covd],key_Symbol,OptionsPattern[]]:=Module[{tmp,keyStr=ToString@key,log=$Log},
$UndefInfoQ=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
Check[UndefCovD@key,Throw@$Failed];
(*sol=Map[Evaluate,Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],All];*)
sol=mapAtKeyEveryWhere[Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],Evaluate,Symbol];
$UndefInfoQ=$Verbose;
AppendTo[$Log,{Style["Unloaded",Red],ToString@$covd,keyStr}];
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[$covd]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Load[sol_[$bundle],key_Symbol,OptionsPattern[]]:=Module[{tmp,log=$Log},
$DefInfoQ=$CVVerbose=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
Check[If[sol[$bundle,key,Metric]=!=None,DefVBundleWithMetric,DefVBundle]@@MakeLoad[$bundle,sol[$bundle,key]],Throw@$Failed];
If[sol[$bundle,key,Metric]=!=None,Check[InvariantTraceTensor[sol[$bundle,key,Symbol],3,Antisymmetric],Throw@$Failed];];
AttachTVs[sol[$bundle,key,#]]&/@(DeleteCases[Null]@(If[MatchQ[Values@#,MatchOf[$autoTensor]],Keys@#]&/@Normal@sol[$bundle,key]));
$DefInfoQ=$CVVerbose=$Verbose;
AppendTo[$Log,{Style["Loaded",Darker@Green],ToString@$bundle,ToString@key}];
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[$bundle]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Unload[sol_[$bundle],key_Symbol,OptionsPattern[]]:=Module[{tmp,keyStr=ToString@key,log=$Log},
$UndefInfoQ=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
If[xTensorQ[sol[$bundle,key,Inv,Symbol]],Check[UndefTensor@sol[$bundle,key,Inv,Symbol],Throw@$Failed];];
Check[UndefVBundle@key,Throw@$Failed];
(*sol=Map[Evaluate,Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],All];*)
sol=mapAtKeyEveryWhere[Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],Evaluate,Symbol];
$UndefInfoQ=$Verbose;
AppendTo[$Log,{Style["Unloaded",Red],ToString@$bundle,keyStr}];
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[$bundle]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Load[sol_[$chart],key_Symbol,OptionsPattern[]]:=Module[{tmp,log=$Log},
$DefInfoQ=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
Check[DefChart@@MakeLoad[$chart,sol[$chart,key]],Throw@$Failed];
MakeSymbol["Christoffel","PD",sol[$chart,key,Symbol]][__]:=0;
ToBasis[key][MakeSymbol["Det",extractIdentifier@sol[$metric][[1]][Symbol]][]];
$DefInfoQ=$Verbose;
AppendTo[$Log,{Style["Loaded",Darker@Green],ToString@$chart,ToString@key}];
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[$chart]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Unload[sol_[$chart],key_Symbol,OptionsPattern[]]:=Module[{tmp,keyStr=ToString@key,log=$Log},
$UndefInfoQ=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
MakeSymbol["Christoffel","PD",sol[$chart,key,Symbol]][__]=.;
Check[UndefChart@key,Throw@$Failed];
(*sol=Map[Evaluate,Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],All];*)
sol=mapAtKeyEveryWhere[Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],Evaluate,Symbol];
$UndefInfoQ=$Verbose;
AppendTo[$Log,{Style["Unloaded",Red],ToString@$chart,keyStr}];
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[$chart]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Load[sol_[$basis],key_Symbol,OptionsPattern[]]:=Module[{tmp,log=$Log},
$DefInfoQ=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
Check[DefBasis@@MakeLoad[$basis,sol[$basis,key]],Throw@$Failed];
MakeSymbol["A","Christoffel","PD",sol[$basis,key,Symbol]][__]:=0;
$DefInfoQ=$Verbose;
AppendTo[$Log,{Style["Loaded",Darker@Green],ToString@$basis,ToString@key}];
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[$basis]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Unload[sol_[$basis],key_Symbol,OptionsPattern[]]:=Module[{tmp,keyStr=ToString@key,log=$Log},
$UndefInfoQ=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
MakeSymbol["A","Christoffel","PD",sol[$basis,key,Symbol]][__]=.;
Check[UndefBasis@key,Throw@$Failed];
(*sol=Map[Evaluate,Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],All];*)
sol=mapAtKeyEveryWhere[Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],Evaluate,Symbol];
$UndefInfoQ=$Verbose;
AppendTo[$Log,{Style["Unloaded",Red],ToString@$basis,keyStr}];
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[$basis]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Load[sol_[$form],key_Symbol,OptionsPattern[]]:=Module[{tmp,log=$Log},
$DefInfoQ=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
Check[DefDiffForm@@MakeLoad[$form,sol[$form,key]],Throw@$Failed];
AppendTo[$Log,{Style["Loaded",Darker@Green],ToString@$form,ToString@key}];
$DefInfoQ=$Verbose;
If[sol[$form,key,Tensor]=!=None,
QuietEcho@Load[sol[$tensor],sol[$form,key,Tensor],Verbose->OptionValue[Verbose]];
];
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[$form]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Unload[sol_[$form],key_Symbol,OptionsPattern[]]:=Module[{tmp,keyStr=ToString@key,log=$Log},
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
If[sol[$form,key,Tensor]=!=None,
QuietEcho@Unload[sol[$tensor],sol[$form,key,Tensor],Verbose->OptionValue[Verbose]];
];
$UndefInfoQ=OptionValue[Verbose];
Check[UndefDiffForm@key,Throw@$Failed];
(*sol=Map[Evaluate,Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],All];*)
sol=mapAtKeyEveryWhere[Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],Evaluate,Symbol];
$UndefInfoQ=$Verbose;
AppendTo[$Log,{Style["Unloaded",Red],ToString@$form,keyStr}];
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[$form]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Load[sol_[$spinor],key_Symbol,OptionsPattern[]]:=Module[{tmp,log=$Log},
$DefInfoQ=$CVVerbose=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
Check[If[sol[$spinor,key,Grassmann]===Even,DefEvenSpinor,DefOddSpinor]@@MakeLoad[$spinor,sol[$spinor,key]],Throw@$Failed];
AttachTVs[sol[$spinor,key]];
AttachTVs[sol[$spinor,key,#]]&/@(DeleteCases[Null]@(If[MatchQ[Values@#,MatchOf[$autoTensor]],Keys@#]&/@Normal@sol[$spinor,key]));
$DefInfoQ=$CVVerbose=$Verbose;
AppendTo[$Log,{Style["Loaded",Darker@Green],ToString@$spinor,ToString@key}];
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[$spinor]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Unload[sol_[$spinor],key_Symbol,OptionsPattern[]]:=Module[{tmp,keyStr=ToString@key,log=$Log},
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
$UndefInfoQ=OptionValue[Verbose];
Check[UndefSpinor@key,Throw@$Failed];
(*sol=Map[Evaluate,Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],All];*)
sol=mapAtKeyEveryWhere[Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],Evaluate,Symbol];
$UndefInfoQ=$Verbose;
AppendTo[$Log,{Style["Unloaded",Red],ToString@$spinor,keyStr}];
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[$spinor]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Load[sol_[$function],key_Symbol,OptionsPattern[]]:=Module[{tmp,log=$Log},
$DefInfoQ=$CVVerbose=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
Check[DefScalarFunction@@MakeLoad[$function,sol[$function,key]],Throw@$Failed];
key[]:=sol[$function,key,Symbol];
$DefInfoQ=$CVVerbose=$Verbose;
AppendTo[$Log,{Style["Loaded",Darker@Green],ToString@$function,ToString@key}];
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[$function]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Unload[sol_[$function],key_Symbol,OptionsPattern[]]:=Module[{tmp,keyStr=ToString@key,log=$Log},
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
$UndefInfoQ=OptionValue[Verbose];
Check[UndefScalarFunction@key,Throw@$Failed];
(*sol=Map[Evaluate,Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],All];*)
sol=mapAtKeyEveryWhere[Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],Evaluate,Symbol];
$UndefInfoQ=$Verbose;
AppendTo[$Log,{Style["Unloaded",Red],ToString@$function,keyStr}];
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[$function]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Load[sol_[$constant],key_Symbol,OptionsPattern[]]:=Module[{tmp,log=$Log},
$DefInfoQ=$CVVerbose=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
Check[DefConstantSymbol@@MakeLoad[$constant,sol[$constant,key]],Throw@$Failed];
$DefInfoQ=$CVVerbose=$Verbose;
AppendTo[$Log,{Style["Loaded",Darker@Green],ToString@$constant,ToString@key}];
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[$constant]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Unload[sol_[$constant],key_Symbol,OptionsPattern[]]:=Module[{tmp,keyStr=ToString@key,log=$Log},
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
$UndefInfoQ=OptionValue[Verbose];
Check[UndefConstantSymbol@key,Throw@$Failed];
(*sol=Map[Evaluate,Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],All];*)
sol=mapAtKeyEveryWhere[Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],Evaluate,Symbol];
$UndefInfoQ=$Verbose;
AppendTo[$Log,{Style["Unloaded",Red],ToString@$constant,keyStr}];
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[$constant]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Load[sol_[$parameter],key_Symbol,OptionsPattern[]]:=Module[{tmp,log=$Log},
$DefInfoQ=$CVVerbose=OptionValue[Verbose];
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
Check[DefParameter@@MakeLoad[$parameter,sol[$parameter,key]],Throw@$Failed];
$DefInfoQ=$CVVerbose=$Verbose;
AppendTo[$Log,{Style["Loaded",Darker@Green],ToString@$parameter,ToString@key}];
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[$parameter]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Unload[sol_[$parameter],key_Symbol,OptionsPattern[]]:=Module[{tmp,keyStr=ToString@key,log=$Log},
(*If[OptionValue[Overwrite]===True,Catch@Quiet@Unload[sol[$manifold]];];*)
$UndefInfoQ=OptionValue[Verbose];
Check[UndefParameter@key,Throw@$Failed];
(*sol=Map[Evaluate,Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],All];*)
sol=mapAtKeyEveryWhere[Map[Association,Uncompress@Compress@Map[Normal,sol,1]/.Removed[x_]:>MakeSymbol[x],1],Evaluate,Symbol];
$UndefInfoQ=$Verbose;
AppendTo[$Log,{Style["Unloaded",Red],ToString@$parameter,keyStr}];
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[$parameter]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Load[sol_[$assumption],key_String,OptionsPattern[]]:=Module[{log=$Log},
System`$Assumptions=And@@DeleteDuplicates@Join[Level[System`$Assumptions,1],MakeLoad[$assumption,sol[$assumption,key]]];
If[OptionValue[Verbose]===True,Print["$Assumptions = ",System`$Assumptions]];
AppendTo[$Log,{Style["Loaded",Darker@Green],ToString@$assumption,"\""<>key<>"\""}];
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[$assumption]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Unload[sol_[$assumption],key_String,OptionsPattern[]]:=Module[{log=$Log},
System`$Assumptions=And@@Select[If[!MatchQ[System`$Assumptions,(_Less|_LessEqual|_Greater|_GreaterEqual|_Inequality|_Element)],Level[System`$Assumptions,1],{System`$Assumptions}],!MemberQ[sol[$assumption,key,Value],#]&];
If[OptionValue[Verbose]===True,Print["$Assumptions = ",System`$Assumptions]];
AppendTo[$Log,{Style["Unloaded",Red],ToString@$assumption,"\""<>key<>"\""}];
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[$assumption]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Load[sol_[$equation],key_Symbol,OptionsPattern[]]:=Module[{log=$Log,
symbol,
expr,
ind,
indClean,
pattern
},
symbol=sol[$equation,key,Symbol];
expr=Level[sol[$equation,key,Value],{2}][[1]];
ind=IndicesOf[Free][expr]/.IndexList->List;
indClean=ind/.{-x_:>x};
pattern=Pattern[#,Blank[]]&/@indClean;
Check[IndexSetDelayed[Evaluate[symbol@@pattern],Evaluate[expr/.Thread[indClean->ind]]],Throw@$Failed];
AppendTo[$Log,{Style["Loaded",Darker@Green],ToString@$equation,ToString@key}];
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[$equation]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Unload[sol_[$equation],key_Symbol,OptionsPattern[]]:=Module[{log=$Log,keyStr=ToString@key},
Check[Clear[Evaluate[sol[$equation,key,Symbol]]],Throw@$Failed];
AppendTo[$Log,{Style["Unloaded",Red],ToString@$equation,keyStr}];
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[$equation]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Subsection::Closed:: *)
(*Multiple at once*)


(* ::Input::Initialization:: *)
Function[{obj},
Load[sol_[obj],keys_List,OptionsPattern[]]:=Module[{log=$Log},
QuietEcho[Load[sol[obj],#,Verbose->OptionValue[Verbose]]]&/@keys;
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[obj]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]
]/@KeysOf[$solution];
Load[sol_[$tensor],keys_List,OptionsPattern[]]:=Module[{log=$Log,notAuto},
notAuto=Select[
keys,!MemberQ[DeleteCases[Null]@Join[If[sol[$metric,#,InducedFrom]=!=Null,sol[$metric,#,InducedFrom][[2]]]&/@Keys@sol[$metric],If[sol[$form,#,Tensor]=!=None,sol[$form,#,Tensor]]&/@Keys@sol[$form]],#]&
];
QuietEcho[Load[sol[$tensor],#,Verbose->OptionValue[Verbose]]]&/@notAuto;
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol[$tensor]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Function[{obj},Load[sol_[obj],OptionsPattern[]]:=Load[sol[obj],Keys@sol[obj],Verbose->OptionValue[Verbose]]
]/@KeysOf[$solution];
Load[sol_[$tensor],OptionsPattern[]]:=Load[sol[$tensor],Keys@sol[$tensor],Verbose->OptionValue[Verbose]]


(* ::Input::Initialization:: *)
Function[{obj},
Unload[sol_[obj],keys_List,OptionsPattern[]]:=Module[{log=$Log},
QuietEcho[Unload[sol[obj],#,Verbose->OptionValue[Verbose]]]&/@(Reverse@keys);
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[obj]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]
]/@KeysOf[$solution];
Unload[sol_[$tensor],keys_List,OptionsPattern[]]:=Module[{log=$Log,notAuto},
notAuto=Select[
keys,!MemberQ[DeleteCases[Null]@Join[If[sol[$metric,#,InducedFrom]=!=Null,sol[$metric,#,InducedFrom][[2]]]&/@Keys@sol[$metric],If[sol[$form,#,Tensor]=!=None,sol[$form,#,Tensor]]&/@Keys@sol[$form]],#]&
];
QuietEcho[Unload[sol[$tensor],#,Verbose->OptionValue[Verbose]]]&/@notAuto;
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol[$tensor]),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Function[{obj},Unload[sol_[obj],OptionsPattern[]]:=Unload[sol[obj],Keys@sol[obj],Verbose->OptionValue[Verbose]]
]/@KeysOf[$solution];
Unload[sol_[$tensor],OptionsPattern[]]:=Unload[sol[$tensor],Keys@sol[$tensor],Verbose->OptionValue[Verbose]]


(* ::Input::Initialization:: *)
Load[sol_,OptionsPattern[]]:=Module[{log=$Log},
QuietEcho[Load[sol[#],Verbose->OptionValue[Verbose]]]&/@(Keys@sol);
Echo[TableForm@{{Style["Loaded",Darker@Green],ToString@Unevaluated@(sol),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
Unload[sol_,OptionsPattern[]]:=Module[{log=$Log},
QuietEcho[Unload[sol[#],Verbose->OptionValue[Verbose]]]&/@(Reverse@(Keys@sol));
Echo[TableForm@{{Style["Unloaded",Red],ToString@Unevaluated@(sol),{#[[-1]]&/@$Log[[-(Length@$Log-Length@log);;-1]]}}}];
]


(* ::Input::Initialization:: *)
(*Load[sol_,OptionsPattern[]]:=(Load[sol[#],Verbose->OptionValue[Verbose]]&/@Keys@sol;Return[Null])
Unload[sol_,OptionsPattern[]]:=(Unload[sol[#],Verbose->OptionValue[Verbose]]&/@(Reverse@Keys@sol);Return[Null])*)


(* ::Section:: *)
(*Computations*)


(* ::Subsection::Closed:: *)
(*Helpers*)


(* ::Input::Initialization:: *)
Attributes[ds2Q]={HoldFirst};
ds2Q[sol_,expr_]:=Module[{
Expr=Expand@expr,
coord=sol[$chart][[1]][Coord],
tmp
},
If[Head@Expr===Plus,tmp=Level[Expr,1],tmp={Expr}];
(MatchQ[#,(_ Diff[_?(MemberQ[coord,#]&)]\[CircleTimes]Diff[_?(MemberQ[coord,#]&)]|Diff[_?(MemberQ[coord,#]&)]\[CircleTimes]Diff[_?(MemberQ[coord,#]&)])]&/@tmp//DeleteDuplicates)==={True}
]
(*ds2Q[expr_]:=ds2Q[expr,$solution[$info,$coordinate]]*)

Attributes[formDegreeQ]={HoldFirst};
formDegreeQ[sol_,expr_]:=Module[{
Expr=Expand@expr,
coord=sol[$chart][[1]][Coord],
tmp,
deg,
dpattern
},
deg=Deg[expr];
(*If[Catch@Deg[expr]===Null,Abort[];,deg=Deg[expr]];*)
dpattern=Wedge@@Table[Diff[_?(MemberQ[coord,#]&)],{ii,1,deg}];
If[Head@Expr===Plus,tmp=Level[Expr,1],tmp={Expr}];
If[(MatchQ[#,(dpattern|_ dpattern)]&/@tmp//DeleteDuplicates)==={True},
Return[deg],
Return["Not a form"]f
]
]
(*formDegreeQ[expr_]:=formDegreeQ[expr,Solution[$info,$coordinate]]*)

dcoeff[1,xc_]:=(Diff[#]&/@xc)
dcoeff[2,xc_]:=Map[Wedge[#,Diff[#]&/@xc]&,dcoeff[1,xc],{1}]
dcoeff[n_,xc_]:=If[EvenQ[n],Map[Wedge[#,Diff[#]&/@xc]&,dcoeff[n-1,xc],{n-1}],Map[Wedge[Diff[#]&/@xc,#]&,dcoeff[n-1,xc],{n-1}]]


(* ::Input::Initialization:: *)
Attributes[MakeArray]={HoldFirst};
MakeArray[sol_,expr_]:=Module[{
xc=sol[$chart][[1]][Coord],
dim=Length@sol[$chart][[1]][Coord],
curved=sol[$chart][[1]][Symbol],
ind=sol[$manifold][[1]][Index],
formDegree,
tb
},
If[ds2Q[sol,expr],
tb=Table[Diff[xc[[ii]]]\[CircleTimes]Diff[xc[[jj]]],{ii,1,dim},{jj,1,dim}];
Return[1/2 (Coefficient[expr,tb]+Transpose@Coefficient[expr,tb])];
(*Return[Table[(1/2(Normal@CoefficientArrays[expr,tb\[LeftDoubleBracket]ii\[RightDoubleBracket] ]\[LeftDoubleBracket]2\[RightDoubleBracket]+Normal@CoefficientArrays[expr,Transpose[tb]\[LeftDoubleBracket]ii\[RightDoubleBracket]]\[LeftDoubleBracket]2\[RightDoubleBracket])),{ii,1,dim}]];*)
,
formDegree=formDegreeQ[sol,expr];
If[formDegree===1,
tb=Coefficient[expr,dcoeff[formDegree,xc]];
tb=CTensor[tb,ConstantArray[-curved,formDegree]];
Return[(tb@@(-ind[[#]]&/@Range[1,formDegree]))[[0,1]]]
];
If[formDegree>1,
tb=Coefficient[expr,dcoeff[formDegree,xc]];
tb=CTensor[tb,ConstantArray[-curved,formDegree]];
Return[formDegree!Antisymmetrize[(tb@@(-ind[[#]]&/@Range[1,formDegree])),(-ind[[#]]&/@Range[1,formDegree])][[0,1]]]
];
];
]
MakeArray[sol_,expr_,coord_]:=Module[{
xc=coord,
dim=Length@coord,
curved=sol[$chart][[1]][Symbol],
ind=sol[$manifold][[1]][Index],
formDegree,
tb
},
If[ds2Q[sol,expr],
tb=Table[Diff[xc[[ii]]]\[CircleTimes]Diff[xc[[jj]]],{ii,1,dim},{jj,1,dim}];
Return[1/2 (Coefficient[expr,tb]+Transpose@Coefficient[expr,tb])];
(*Return[Table[(1/2(Normal@CoefficientArrays[expr,tb\[LeftDoubleBracket]ii\[RightDoubleBracket] ]\[LeftDoubleBracket]2\[RightDoubleBracket]+Normal@CoefficientArrays[expr,Transpose[tb]\[LeftDoubleBracket]ii\[RightDoubleBracket]]\[LeftDoubleBracket]2\[RightDoubleBracket])),{ii,1,dim}]];*)
,
formDegree=formDegreeQ[sol,expr];
If[formDegree===1,
tb=Coefficient[expr,dcoeff[formDegree,xc]];
tb=CTensor[tb,ConstantArray[-curved,formDegree]];
Return[(tb@@(-ind[[#]]&/@Range[1,formDegree]))[[0,1]]]
];
If[formDegree>1,
tb=Coefficient[expr,dcoeff[formDegree,xc]];
tb=CTensor[tb,ConstantArray[-curved,formDegree]];
Return[formDegree!Antisymmetrize[(tb@@(-ind[[#]]&/@Range[1,formDegree])),(-ind[[#]]&/@Range[1,formDegree])][[0,1]]]
];
];
]


(* ::Input::Initialization:: *)
Attributes[ToBases]={HoldFirst};
ToBases[sol_][expr_]:=Module[{bas},
bas=Join[{sol[$chart][[1]][Symbol]},sol[$basis,#,Symbol]&/@Keys@sol[$basis],{sol[$chart][[1]][Symbol]},sol[$basis,#,Symbol]&/@Keys@sol[$basis]];
Return[(Composition@@(ToBasis/@bas))[expr]];
]


(* ::Input::Initialization:: *)
Attributes[ToArray]={HoldFirst};
ToArray[sol_][expr_]/;((ds2Q[sol,expr]===True||formDegreeQ[sol,expr]=!=0)&&Head@sol===Association):=MakeArray[sol,expr]
(*ToArray[sol_][expr_]/;(Head@expr===List&&(formDegreeQ[sol,#]&/@expr)===ConstantArray[0,Length@expr]&&Head@sol===Association):=Identity[expr]*)
ToArray[sol_][expr_]/;(Head@expr===List&&(formDegreeQ[sol,#]&/@expr)=!=ConstantArray[0,Length@expr]&&Head@sol===Association):=MakeArray[sol,#]&/@expr
(*ToArray[sol_][expr_]/;(Head@sol===Association&&Head@expr=!=List):=ToValues[ToValues[ToValues[ComponentArray[TraceBasisDummy[ToBases[sol][expr]]]]]]*)
ToArray[sol_][expr_]/;(Head@sol===Association):=ToValues[ToValues[ToValues[ComponentArray[TraceBasisDummy[ToBases[sol][expr]]]]]]


(* ::Input::Initialization:: *)
MakeForm::incompatible="The dimensions of the supplied array are incompatible with the number of the supplied coordinates";
Attributes[MakeForm]={HoldFirst};
MakeForm[sol_,array_]:=Module[{
degree,
xc=sol[$chart][[1]][Coord]
},
If[(Dimensions@array//DeleteDuplicates)=!={Length@xc},
Catch@Throw@Message[MakeForm::incompatible];Abort[];
,
degree=Length@Dimensions@array;
Return[1/degree! Total@Flatten[array*(dcoeff[degree,xc]//Simplification)]];
];
]


(* ::Input::Initialization:: *)
ParallelMapSimplify[symbol_,valID_,parallelSimplify_]:=(
symbol/:TensorValues[symbol,valID]=FoldedRule[
TensorValues[symbol,valID][[1]],
ParallelMap[parallelSimplify,TensorValues[symbol,valID][[2]],Method->Automatic,ProgressReporting->True]
];
)


(* ::Input::Initialization:: *)
ParallelMapSimplify[symbol_,valID_,parallelSimplify_,assump_]:=(
symbol/:TensorValues[symbol,valID]=FoldedRule[
TensorValues[symbol,valID][[1]],
ParallelMap[parallelSimplify[#1,Assumptions->assump]&,TensorValues[symbol,valID][[2]],Method->Automatic,ProgressReporting->True]
];
)


(* ::Subsection::Closed:: *)
(*Gamma matrices and flat metric*)


(* ::Input::Initialization:: *)
Attributes[GenFlatGamma]={HoldFirst};
GenFlatGamma[sol_,int_Integer]/;(Head@sol===Association):=Module[{
dim=sol[$manifold][[1]][Dimension],
signature=sol[$metric][[1]][SignDet],
cnumbers=sol[$chart,sol[$chart][[1]][Symbol],CNumber],
(*cnumbers=CNumbersOf[sol[$chart][[1]][Symbol]],*)
repDim,
max,
gammaU
},
repDim=2^Floor[dim/2];
max=repDim/2;

If[int>dim,Print["error"];Abort[];];

If[max===1&&dim===2,
gammaU=AssociationThread[
cnumbers->
{
I^signature PauliMatrix@1,
PauliMatrix@2}
];
Return[gammaU[int]]
];

If[max===1&&dim===3,
gammaU=AssociationThread[
cnumbers->
{
I^signature PauliMatrix@1,
PauliMatrix@2,
PauliMatrix@3}
];
Return[gammaU[int]]
];

If[max===2&&dim===4,
gammaU=AssociationThread[
cnumbers->
{
I^signature KroneckerProduct@@{PauliMatrix@1,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@1},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@2}
}
];
Return[gammaU[int]]
];

If[max===2&&dim===5,
gammaU=AssociationThread[
cnumbers->
{
I^signature KroneckerProduct@@{PauliMatrix@1,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@1},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3}
}
];
Return[gammaU[int]]
];

If[max===3&&dim===6,
gammaU=AssociationThread[
cnumbers->
{
I^signature KroneckerProduct@@{PauliMatrix@1,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@1,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@1},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@2}
}
];
Return[gammaU[int]]
];

If[max===3&&dim===7,
gammaU=AssociationThread[
cnumbers->
{
I^signature KroneckerProduct@@{PauliMatrix@1,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@1,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@1},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@3}
}
];
Return[gammaU[int]]
];

If[max===4&&dim===8,
gammaU=AssociationThread[
cnumbers->
{
I^signature KroneckerProduct@@{PauliMatrix@1,IdentityMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@2,IdentityMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@1,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@1,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@1},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@2}
}
];
Return[gammaU[int]]
];

If[max===4&&dim===9,
gammaU=AssociationThread[
cnumbers->
{
I^signature KroneckerProduct@@{PauliMatrix@1,IdentityMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@2,IdentityMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@1,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@1,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@1},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@3}
}
];
Return[gammaU[int]]
];

If[max===5&&dim===10,
gammaU=AssociationThread[
cnumbers->
{
I^signature KroneckerProduct@@{PauliMatrix@1,IdentityMatrix@2,IdentityMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@2,IdentityMatrix@2,IdentityMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@1,IdentityMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@2,IdentityMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@1,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@1,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@1},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@2}
}
];
Return[gammaU[int]]
];

If[max===5&&dim===11,
gammaU=AssociationThread[
cnumbers->
{
I^signature KroneckerProduct@@{PauliMatrix@1,IdentityMatrix@2,IdentityMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@2,IdentityMatrix@2,IdentityMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@1,IdentityMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@2,IdentityMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@1,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@2,IdentityMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@1,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@2,IdentityMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@1},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@2},
KroneckerProduct@@{PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@3,PauliMatrix@3}
}
];
Return[gammaU[int]]
];
]
GenFlatGamma[sol_,{int__Integer}]/;(Head@sol===Association):=GenFlatGamma[sol,#]&/@{int}


(* ::Input::Initialization:: *)
GenFlatMetric[sol_]/;(Head@sol===Association):=DiagonalMatrix[Join[{sol[$metric,MakeSymbol["gg"],SignDet]},ConstantArray[1,sol[$manifold,MakeSymbol["man"],Dimension]-1]]]


(* ::Subsection::Closed:: *)
(*Compute*)


(* ::Input::Initialization:: *)
ClearAll[func];
Attributes[func]={HoldFirst};
func[sol_,symbol_,Crule_,fromValIdList_,map_]:=Module[{
goodDown,
changedIndexPos,
changedCIndex,
changedCIndexNumber,
changedCIndexBasis,
raiseOrLower,
dummyCIndex,
metric
},
(*goodDown=First@First@Position[Table[Length@Position[Values[(#/.List->Rule)&/@Values@Crule]+fromValIdList[[\[Alpha]\[Alpha]]],0],{\[Alpha]\[Alpha],1,Length@fromValIdList}],1];
changedIndexPos=First@First@Position[Values[(#/.List->Rule)&/@((IndicesOf[CIndex][#]/.IndexList->List)&@(Keys@Crule))]+fromValIdList[[goodDown]],0];
changedCIndex=(IndicesOf[CIndex][Keys@Crule]/.IndexList->List)[[changedIndexPos]];
dummyCIndex={IndicesOfVBundle[VBundleOfBasis[changedCIndex[[2]]]][[1]][[-1]],changedCIndex[[2]]};
metric=First@Select[$Metrics,(VBundleOfMetric[#]===VBundleOfBasis[changedCIndex[[2]]])&];*)

goodDown=First@First@Position[Table[Length@Position[Values[(#/.List->Rule)&/@Values@Crule]+fromValIdList[[\[Alpha]\[Alpha]]],0],{\[Alpha]\[Alpha],1,Length@fromValIdList}],1];
changedIndexPos=First@First@Position[Values[(#/.List->Rule)&/@((IndicesOf[CIndex][#]/.IndexList->List)&@(Keys@Crule))]+fromValIdList[[goodDown]],0];
changedCIndexNumber=(IndicesOf[CIndex][Keys@Crule]/.IndexList->List)[[changedIndexPos]][[1]];
changedCIndexBasis=(IndicesOf[CIndex][Keys@Crule]/.IndexList->List)[[changedIndexPos]][[2]];
raiseOrLower=If[changedCIndexBasis===(changedCIndexBasis/.-x_:>x),1,-1];
dummyCIndex={raiseOrLower IndicesOfVBundle[VBundleOfBasis[changedCIndexBasis]][[1,-1]],changedCIndexBasis};
changedCIndex={changedCIndexNumber,changedCIndexBasis};
metric=First@Select[$Metrics,(VBundleOfMetric[#]===VBundleOfBasis[changedCIndex[[2]]])&];

(*ComponentValue[Keys@Crule,map@ToValues@ToValues@ToValues@TraceBasisDummy@ToBases[sol]@(metric[changedCIndex,dummyCIndex]symbol@@Thread[ReplacePart[(#[[1]]&/@(Values@Crule)),changedIndexPos->-dummyCIndex[[1]]]->ReplacePart[(#[[2]]&/@(Values@Crule)),changedIndexPos->-dummyCIndex[[2]]]]/.Rule->List)
];*)

ComponentValue[Keys@Crule,map@ToValues@TraceBasisDummy@(metric[changedCIndex,dummyCIndex]symbol@@ReplacePart[Values@Crule,changedIndexPos->-dummyCIndex])];
]
FirstBasisOfVBundle[bundle_]:=Select[$Bases,(VBundleOfBasis[#]===(bundle/.-x_:>x))&][[1]]


(* ::Input::Initialization:: *)
Attributes[ExprSeedCompute]={HoldFirst};
ExprSeedCompute[sol_[keyList__],chainEntry_]:=Module[{
symbol,
seed,
length,
replace,
seedPositions,
seedValId,
indices,
seedIndices,
map,
parallelMap,
assumptions,
tmpli,
at
},
(*prepare*)
symbol=extractIdentifier@sol[Sequence[keyList,Symbol]];
(*seed=(Keys@chainEntry)/.Seed->sol[Sequence[keyList,Routine,Seed]]/.$solution->sol//ReleaseHold;*)
seed=Level[(Keys@chainEntry)/.Seed->sol[Sequence[keyList,Routine,Seed]]//ReleaseHold,1][[1]];
map=sol[Sequence[keyList,Routine,Map]];
parallelMap=sol[Sequence[keyList,Routine,ParallelMap]];
assumptions=System`$Assumptions;
seedPositions=(Values@chainEntry)[[1]];
seedValId={seedPositions (FirstBasisOfVBundle[#]&/@SlotsOfTensor[symbol])};
(*indices=Table[(IndicesOfVBundle[#][[1]]&/@SlotsOfTensor[symbol])[[1,ii]],{ii,Range[1,Length@SlotsOfTensor[symbol]]}];*)
indices=Table[(IndicesOfVBundle[#][[1]]&/@SlotsOfTensor[symbol])[[ii,ii]],{ii,1,Length@SlotsOfTensor[symbol]}];
seedIndices=seedPositions indices;

(*for nice printing*)
length=IndicesOf[Dummy][seed]/.IndexList->List/.-x_:>x//DeleteDuplicates//Length;
replace=IndexList@@Flatten[(Reverse[IndicesOfVBundle[#][[1]]])[[1;;length]]&/@$VBundles];
seed=ReplaceDummies[seed,replace];

(*If[TensorValues[symbol]=!=FoldedRule[{},{}],Block[{Print},DeleteTensorValues[symbol]]];*)

at=AbsoluteTime[];

(*apply tensor symmetries*)
$CVSimplify=Identity;
tmpli=Flatten@ComponentArray@ToBases[sol]@(symbol@@seedIndices);
Monitor[
Do[
ComponentValue[tmpli[[\[Alpha]\[Alpha]]]];,
{\[Alpha]\[Alpha],1,Length@tmpli}
],
Row[{ProgressIndicator[\[Alpha]\[Alpha],{1,Length@tmpli},ImageSize->{300,5}]," Applying tensor symmetries to ",symbol@@seedIndices}]
];
$CVSimplify=Simplify;
sol[keyList,Value]=Thread[TensorValIDs[symbol]->(TensorValues[symbol,#]&/@(Level[#,1][[2]]&/@TensorValIDs[symbol]))];

(*apply map*)
$CVSimplify=map;
tmpli=Normal@((IndicesOf[CIndex][#]/.IndexList->List)&/@(Association@TensorValues[symbol,seedValId][[2]]));
Monitor[
Do[
ComponentValue[Keys@tmpli[[\[Alpha]\[Alpha]]],map@ToValues@ToValues@ToValues@TraceBasisDummy@ToBases[sol]@(seed/.Thread[(IndicesOf[Free][seed]/.IndexList->List)->Values@tmpli[[\[Alpha]\[Alpha]]]])];,
{\[Alpha]\[Alpha],1,Length@tmpli}
],
Row[{ProgressIndicator[\[Alpha]\[Alpha],{1,Length@tmpli},ImageSize->{300,5}]," Applying ",map," to ",seed}]
];
$CVSimplify=Simplify;
sol[keyList,Value]=Thread[TensorValIDs[symbol]->(TensorValues[symbol,#]&/@(Level[#,1][[2]]&/@TensorValIDs[symbol]))];

(*apply parallel map*)
If[parallelMap===Simplify||parallelMap===FullSimplify,
ParallelMapSimplify[symbol,seedValId,parallelMap,assumptions];
,
ParallelMapSimplify[symbol,seedValId,parallelMap];
];
sol[keyList,Value]=Thread[TensorValIDs[symbol]->(TensorValues[symbol,#]&/@(Level[#,1][[2]]&/@TensorValIDs[symbol]))];

(*echo*)
Echo[Row[{"Applied ",$PrintColor@map," and ",$PrintColor@parallelMap," to ",ToBases[sol][symbol@@seedIndices]==ReplaceDummies[ToBases[sol][seed],replace]," in ",UnitConvert[Quantity[Round[AbsoluteTime[]-at],"Seconds"],MixedRadix["Minutes","Seconds"]]}]];
]


(* ::Input::Initialization:: *)
Attributes[MetricSeedCompute]={HoldFirst};
MetricSeedCompute[sol_[keyList__],chainEntry_]:=Module[{
symbol,
seed,
length,
replace,
seedPositions,
seedValId,
indices,
seedIndices,
map,
parallelMap,
assumptions,
tmpli,
at
},
(*prepare*)
symbol=extractIdentifier@sol[Sequence[keyList,Symbol]];
seed=(Keys@chainEntry)/.Seed->sol[Sequence[keyList,Routine,Seed]]/.$solution->sol//ReleaseHold;
(*seed=Level[(Keys@chainEntry)/.Seed->sol[Sequence[keyList,Routine,Seed]]//ReleaseHold,1][[1]]/.$solution->sol;*)
map=sol[Sequence[keyList,Routine,Map]];
parallelMap=sol[Sequence[keyList,Routine,ParallelMap]];
assumptions=System`$Assumptions;
seedPositions=(Values@chainEntry)[[1]];
seedValId={seedPositions (FirstBasisOfVBundle[#]&/@SlotsOfTensor[symbol])};
(*indices=Table[(IndicesOfVBundle[#][[1]]&/@SlotsOfTensor[symbol])[[1,ii]],{ii,Range[1,Length@SlotsOfTensor[symbol]]}];*)
indices=Table[(IndicesOfVBundle[#][[1]]&/@SlotsOfTensor[symbol])[[ii,ii]],{ii,1,Length@SlotsOfTensor[symbol]}];
seedIndices=seedPositions indices;
(*If[TensorValues[symbol]=!=FoldedRule[{},{}],Block[{Print},DeleteTensorValues[symbol]]];*)

at=AbsoluteTime[];

(*apply map*)
$CVSimplify=map;
Monitor[
MetricInBasis[symbol,First@DeleteDuplicates@seedValId[[1]],seed];,
Row[{ProgressIndicator[Appearance->"Percolate"],"Applying ",map," to ",symbol@@seedIndices}]
];
$CVSimplify=Simplify;
sol[keyList,Value]=Thread[TensorValIDs[symbol]->(TensorValues[symbol,#]&/@(Level[#,1][[2]]&/@TensorValIDs[symbol]))];

(*apply parallel map*)
If[parallelMap===Simplify||parallelMap===FullSimplify,
ParallelMapSimplify[symbol,seedValId,parallelMap,assumptions];
,
ParallelMapSimplify[symbol,seedValId,parallelMap];
];
sol[keyList,Value]=Thread[TensorValIDs[symbol]->(TensorValues[symbol,#]&/@(Level[#,1][[2]]&/@TensorValIDs[symbol]))];

(*echo*)
Echo[Row[{"Applied ",$PrintColor@map," and ",$PrintColor@parallelMap," to ",ToBases[sol][symbol@@seedIndices]==((Keys@chainEntry)/.Seed->sol[Sequence[keyList,Routine,Seed]])," in ",UnitConvert[Quantity[Round[AbsoluteTime[]-at],"Seconds"],MixedRadix["Minutes","Seconds"]]}]];
]


(* ::Input::Initialization:: *)
Attributes[ScalarSeedCompute]={HoldFirst};
ScalarSeedCompute[sol_[keyList__],chainEntry_]:=Module[{
symbol,
seed,
length,
replace,
seedPositions,
seedValId,
indices,
seedIndices,
map,
parallelMap,
assumptions,
tmpli,
at
},
(*prepare*)
symbol=extractIdentifier@sol[Sequence[keyList,Symbol]];
seed=(Keys@chainEntry)/.Seed->sol[Sequence[keyList,Routine,Seed]]/.$solution->sol//ReleaseHold;
(*seed=Level[(Keys@chainEntry)/.Seed->sol[Sequence[keyList,Routine,Seed]]//ReleaseHold,1][[1]]/.$solution->sol;*)
map=sol[Sequence[keyList,Routine,Map]];
parallelMap=sol[Sequence[keyList,Routine,ParallelMap]];
assumptions=System`$Assumptions;
seedPositions=(Values@chainEntry)[[1]];
seedValId={seedPositions (FirstBasisOfVBundle[#]&/@SlotsOfTensor[symbol])};
(*indices=Table[(IndicesOfVBundle[#][[1]]&/@SlotsOfTensor[symbol])[[1,ii]],{ii,Range[1,Length@SlotsOfTensor[symbol]]}];*)
indices=Table[(IndicesOfVBundle[#][[1]]&/@SlotsOfTensor[symbol])[[ii,ii]],{ii,1,Length@SlotsOfTensor[symbol]}];
seedIndices=seedPositions indices;
(*If[TensorValues[symbol]=!=FoldedRule[{},{}],Block[{Print},DeleteTensorValues[symbol]]];*)

at=AbsoluteTime[];

(*apply map*)
$CVSimplify=map;
TensorValIDs[symbol]^={ValID[symbol,{}]};
(Evaluate@symbol)/:TensorValues[(Evaluate@symbol),{}]=FoldedRule[{},{(Evaluate@symbol)[]->map@seed}];
$CVSimplify=Simplify;
sol[keyList,Value]=Thread[TensorValIDs[symbol]->(TensorValues[symbol,#]&/@(Level[#,1][[2]]&/@TensorValIDs[symbol]))];

(*apply parallel map*)
If[parallelMap=!=Identity,
Evaluate@symbol/:TensorValues[symbol,{}]=FoldedRule[TensorValues[symbol][[1]],Map[parallelMap,TensorValues[symbol][[2]]]];
];
sol[keyList,Value]=Thread[TensorValIDs[symbol]->(TensorValues[symbol,#]&/@(Level[#,1][[2]]&/@TensorValIDs[symbol]))];

(*echo*)
Echo[Row[{"Applied ",$PrintColor@map," and ",$PrintColor@parallelMap," to ",ToBases[sol][symbol@@seedIndices]==((Keys@chainEntry)/.Seed->sol[Sequence[keyList,Routine,Seed]])," in ",UnitConvert[Quantity[Round[AbsoluteTime[]-at],"Seconds"],MixedRadix["Minutes","Seconds"]]}]];
]


(* ::Input::Initialization:: *)
ClearAll[ArraySeedCompute];
Attributes[ArraySeedCompute]={HoldFirst};
ArraySeedCompute[sol_[keyList__],chainEntry_]:=Module[{
symbol,
seed,
length,
replace,
seedPositions,
seedValId,
indices,
seedIndices,
map,
parallelMap,
assumptions,
tmpli,
at,
bas,
ili
},
(*prepare*)
symbol=extractIdentifier@sol[Sequence[keyList,Symbol]];
seed=(Keys@chainEntry)/.Seed->sol[Sequence[keyList,Routine,Seed]]/.$solution->sol//ReleaseHold;
map=sol[Sequence[keyList,Routine,Map]];
parallelMap=sol[Sequence[keyList,Routine,ParallelMap]];
assumptions=System`$Assumptions;
seedPositions=(Values@chainEntry)[[1]];
bas=(FirstBasisOfVBundle[#]&/@SlotsOfTensor[symbol]);
seedValId={seedPositions bas};
(*indices=Table[(IndicesOfVBundle[#][[1]]&/@SlotsOfTensor[symbol])[[1,ii]],{ii,Range[1,Length@SlotsOfTensor[symbol]]}];*)
indices=Table[(IndicesOfVBundle[#][[1]]&/@SlotsOfTensor[symbol])[[ii,ii]],{ii,1,Length@SlotsOfTensor[symbol]}];
seedIndices=seedPositions indices;
(*If[TensorValues[symbol]=!=FoldedRule[{},{}],Block[{Print},DeleteTensorValues[symbol]]];*)

at=AbsoluteTime[];

(*apply tensor symmetries*)
$CVSimplify=Identity;
tmpli=Flatten@ComponentArray@ToBases[sol]@(symbol@@seedIndices);
Monitor[
Do[
ComponentValue[tmpli[[\[Alpha]\[Alpha]]]];,
{\[Alpha]\[Alpha],1,Length@tmpli}
],
Row[{ProgressIndicator[\[Alpha]\[Alpha],{1,Length@tmpli},ImageSize->{300,5}]," Applying tensor symmetries to ",symbol@@seedIndices}]
];
$CVSimplify=Simplify;
sol[keyList,Value]=Thread[TensorValIDs[symbol]->(TensorValues[symbol,#]&/@(Level[#,1][[2]]&/@TensorValIDs[symbol]))];

(*apply map*)
$CVSimplify=map;
ili=Table[(Composition@@Table[DeleteCases[bas[[ii]]],{ii,1,Length@bas}])@(Composition@@Table[DeleteCases[-bas[[ii]]],{ii,1,Length@bas}])@Flatten@Level[Keys@TensorValues[symbol][[2]][[\[Alpha]\[Alpha]]],1],{\[Alpha]\[Alpha],1,Length@Keys@TensorValues[symbol][[2]]}];
(*tmpli=Normal@((IndicesOf[CIndex][#]/.IndexList->List)&/@(Association@TensorValues[symbol,seedValId][[2]]));*)
Monitor[
Do[ComponentValue[
symbol@@Table[{ili[[\[Alpha]\[Alpha],ii]],seedPositions[[ii]]bas[[ii]]},{ii,1,Length@bas}],
Part@@Join[{seed},Table[(Flatten@Position[CNumbersOf[bas[[ii]]],ili[[\[Alpha]\[Alpha],ii]]])[[1]],{ii,1,Length@bas}]]
],{\[Alpha]\[Alpha],1,Length@ili}
],
Row[{ProgressIndicator[\[Alpha]\[Alpha],{1,Length@ili},ImageSize->{300,5}]," Applying ",map," to ",symbol@@seedIndices}]
];
$CVSimplify=Simplify;
sol[keyList,Value]=Thread[TensorValIDs[symbol]->(TensorValues[symbol,#]&/@(Level[#,1][[2]]&/@TensorValIDs[symbol]))];

(*apply parallel map*)
If[parallelMap===Simplify||parallelMap===FullSimplify,
ParallelMapSimplify[symbol,seedValId,parallelMap,assumptions];
,
ParallelMapSimplify[symbol,seedValId,parallelMap];
];
sol[keyList,Value]=Thread[TensorValIDs[symbol]->(TensorValues[symbol,#]&/@(Level[#,1][[2]]&/@TensorValIDs[symbol]))];

(*echo*)
Echo[Row[{"Applied ",$PrintColor@map," and ",$PrintColor@parallelMap," to ",ToBases[sol][symbol@@seedIndices]==((Keys@chainEntry)/.Seed->sol[Sequence[keyList,Routine,Seed]])," in ",UnitConvert[Quantity[Round[AbsoluteTime[]-at],"Seconds"],MixedRadix["Minutes","Seconds"]]}]];
]


(* ::Input::Initialization:: *)
Attributes[RLCompute]={HoldFirst};
RLCompute[sol_[keyList__],chainEntry_]:=Module[{
symbol,
seed,
length,
replace,
seedPositions,
seedValId,
indices,
seedIndices,
map,
parallelMap,
assumptions,
tmpli,
at,

fromPositionsList,
targetPositionsList,
targetIndicesList,
fromValIdList,
currentTV,
toValIdList,
tbm
},
symbol=extractIdentifier@sol[Sequence[keyList,Symbol]];
map=sol[Sequence[keyList,Routine,Map]];
parallelMap=sol[Sequence[keyList,Routine,ParallelMap]];
assumptions=System`$Assumptions;
(*indices=Table[(IndicesOfVBundle[#][[1]]&/@SlotsOfTensor[symbol])[[1,ii]],{ii,Range[1,Length@SlotsOfTensor[symbol]]}];*)
indices=Table[(IndicesOfVBundle[#][[1]]&/@SlotsOfTensor[symbol])[[ii,ii]],{ii,1,Length@SlotsOfTensor[symbol]}];

(*If[TensorValues[symbol]=!=FoldedRule[{},{}],Block[{Print},DeleteTensorValues[symbol]]];*)

fromPositionsList=Keys@chainEntry;
targetPositionsList=Values@chainEntry;
targetIndicesList=Table[targetPositionsList[[\[Alpha]\[Alpha]]] Table[(IndicesOfVBundle[#][[1]]&/@SlotsOfTensor[symbol])[[ii,ii]],{ii,Range[1,Length@SlotsOfTensor[symbol]]}],{\[Alpha]\[Alpha],1,Length@targetPositionsList}];
(*targetIndicesList=Table[targetPositionsList[[\[Alpha]\[Alpha]]] indices,{\[Alpha]\[Alpha],1,Length@targetPositionsList}];*)
(*targetIndicesList=Table[targetPositionsList[[\[Alpha]\[Alpha]]] (FirstBasisOfVBundle[#]&/@SlotsOfTensor[symbol]),{\[Alpha]\[Alpha],1,Length@targetPositionsList}];*)
fromValIdList=Table[fromPositionsList[[\[Alpha]\[Alpha]]] (FirstBasisOfVBundle[#]&/@SlotsOfTensor[symbol]),{\[Alpha]\[Alpha],1,Length@fromPositionsList}];

at=AbsoluteTime[];

currentTV=TensorValIDs[symbol];

(*apply tensor symmetries*)
$CVSimplify=Identity;
tmpli=Flatten@(((ComponentArray@ToBases[sol]@(symbol@@#))&)/@targetIndicesList);
Monitor[
Do[
ComponentValue[tmpli[[\[Alpha]\[Alpha]]]];,
{\[Alpha]\[Alpha],1,Length@tmpli}
],
Row[{ProgressIndicator[\[Alpha]\[Alpha],{1,Length@tmpli},ImageSize->{300,5}]," Applying tensor symmetries to ",Row[((symbol@@#)&/@targetIndicesList)," "]}]
];
$CVSimplify=Simplify;
sol[keyList,Value]=Thread[TensorValIDs[symbol]->(TensorValues[symbol,#]&/@(Level[#,1][[2]]&/@TensorValIDs[symbol]))];

toValIdList=Level[#,1][[2]]&/@(Select[TensorValIDs[symbol],!MemberQ[currentTV,#]&]);

(*apply map*)
$CVSimplify=map;
Do[
tbm=Normal@((IndicesOf[CIndex][#]/.IndexList->List)&/@Association[TensorValues[symbol,toValIdList[[\[Beta]\[Beta]]]][[2]]]);
Monitor[
Do[
(*If[\[Alpha]\[Alpha]===1,Echo[{symbol,tbm[[\[Alpha]\[Alpha]]],fromValIdList,map}];];*)
func[sol,symbol,tbm[[\[Alpha]\[Alpha]]],fromValIdList,map];,
{\[Alpha]\[Alpha],1,Length@tbm}
],
Row[{ProgressIndicator[\[Alpha]\[Alpha],{1,Length@tmpli},ImageSize->{300,5}]," Applying ",map," to ",Row[((symbol@@(indices #))&/@(toValIdList[[\[Beta]\[Beta]]]/.Thread[Variables@toValIdList[[\[Beta]\[Beta]]]->ConstantArray[1,Length@Variables@toValIdList[[\[Beta]\[Beta]]]]]))," "]}]
];
$CVSimplify=Simplify;
sol[keyList,Value]=Thread[TensorValIDs[symbol]->(TensorValues[symbol,#]&/@(Level[#,1][[2]]&/@TensorValIDs[symbol]))];

(*apply parallel map*)
If[TrueQ[(ToString[parallelMap]==ToString[Simplify]||ToString[parallelMap]==ToString[FullSimplify])],
ParallelMapSimplify[symbol,toValIdList[[\[Beta]\[Beta]]],parallelMap,assumptions];
,
ParallelMapSimplify[symbol,toValIdList[[\[Beta]\[Beta]]],parallelMap];
];
,
{\[Beta]\[Beta],1,Length@toValIdList}
];
sol[keyList,Value]=Thread[TensorValIDs[symbol]->(TensorValues[symbol,#]&/@(Level[#,1][[2]]&/@TensorValIDs[symbol]))];

(*echo*)
Echo[Row[{"Applied ",$PrintColor@map," and ",$PrintColor@parallelMap," to ",Row[ToBases[sol]/@((symbol@@#)&/@targetIndicesList)," "]," in ",UnitConvert[Quantity[Round[AbsoluteTime[]-at],"Seconds"],MixedRadix["Minutes","Seconds"]]}]];
]


(* ::Input::Initialization:: *)
Attributes[chainCompute]={HoldFirst};
chainCompute[sol_[keyList__],chainEntry_]:=Module[{
symbol,
protoSeed,
protoChainEntry
},
symbol=extractIdentifier@sol[keyList,Symbol];
(*seed=(Keys@chainEntry)/.Seed->sol[Sequence[keyList,Routine,Seed]]/.$solution->sol//ReleaseHold;*)
protoSeed=Level[(Keys@chainEntry)/.Seed->sol[Sequence[keyList,Routine,Seed]]//ReleaseHold,1][[1]];

Which[
MatchQ[Keys@chainEntry,{{(-1|1)..}..}],RLCompute[sol[keyList],chainEntry],
!MatchQ[Keys@chainEntry,{{(-1|1)..}..}],
Which[
MetricQ[symbol],MetricSeedCompute[sol[keyList],chainEntry],
xTensorQ[symbol]&&SlotsOfTensor[symbol]==={},ScalarSeedCompute[sol[keyList],chainEntry],
xTensorQ[symbol]&&SlotsOfTensor[symbol]=!={},
Which[
IndicesOf[Free][protoSeed]===IndexList[],ArraySeedCompute[sol[keyList],chainEntry],
IndicesOf[Free][protoSeed]=!=IndexList[],ExprSeedCompute[sol[keyList],chainEntry]
]
]
]
]


(* ::Input::Initialization:: *)
Attributes[Compute]={HoldFirst};
Compute[sol_[keyList__]]:=Module[{
symbol
},
symbol=extractIdentifier@sol[keyList,Symbol];
If[TensorValues[symbol]=!=FoldedRule[{},{}],Block[{Print},DeleteTensorValues[symbol]]];
Do[
chainCompute[sol[keyList],chainEntry],
{chainEntry,sol[keyList,Routine,Chain]}
]
]


(* ::Section:: *)
(*Setup*)


(* ::Subsection::Closed:: *)
(*Instantiate*)


(* ::Input::Initialization:: *)
Instantiate::value="Key `1` should point to a match with `2`";
Attributes[Instantiate]={HoldFirst};
Options[Instantiate]={
Curved->(<|Dimension->_Integer?(#>0&),SignDet->(1|-1),CNumber->{_Integer..},Coord->{_Symbol[]..}|>|False),
Frame->(True|False),
Spin->(True|False),
Lie->(<|Dimension->_Integer?(#>0&),CNumber->{_Integer..}|>|False),
$info->{$info[]},
(*$manifold->{$manifold[]},*)
$metric->{$metric[]...},
(*$frame->{$frame[]},*)
(*$spin->{$spin[]},*)
(*$bundle->{$bundle[]...},*)
$covd->{$covd[]...},
(*$chart->{$chart[]...},*)
(*$basis->{$basis[]...},*)
$form->{$form[]...},
$tensor->{$tensor[]...},
$spinor->{$spinor[]...},
$function->{$function[]...},
$constant->{$constant[]...},
$parameter->{$parameter[]...},
$assumption->{$assumption[]...},
$rule->{$rule[]...},
$equation->{$equation[]...},
Load->True,
Verbose->False
};
Instantiate[sol_,OptionsPattern[]]:=Module[{
autoSolution=Association[Rule[#,OptionValue[#]]&/@Keys@Options@Instantiate],
tmp=$solution[],
tmp2
},
If[SameQ[OptionValue[#],(Association@Options[Instantiate])[#]],autoSolution[#]=False]&/@{Curved,Frame,Spin,Lie};
If[SameQ[OptionValue[#],(Association@Options[Instantiate])[#]],autoSolution[#]={}]&/@{$info,$metric,$covd,$form,$tensor,$spinor,$function,$constant,$parameter,$assumption,$rule,$equation};
Function[{obj},If[OptionValue[obj]===MatchOf[$solution][obj],
autoSolution[obj]=<||>,
If[MatchQ[OptionValue[obj],{_Association...}],
tmp2=Apply[obj,#]&/@(Normal[ReleaseHold[#]]&/@OptionValue[obj]);
autoSolution[obj]=AssociationThread[(extractIdentifier[#[Symbol]]&/@tmp2)->tmp2],
Message[$solution::liassoc,obj];Throw@$Failed
]
]]/@{$info,$metric,$covd,$form,$tensor,$spinor,$function,$constant,$parameter,$assumption,$rule,$equation};
If[!MatchQ[autoSolution[#],(Association@Options[Instantiate])[#]],
Message[Instantiate::value,#,(Association@Options[Instantiate])[#]];Throw@$Failed
]&/@{Curved,Frame,Spin,Lie};

If[ValueQ[sol],(*QuietEcho@*)Quiet@Catch@Unload[sol,Verbose->OptionValue[Verbose]];];
autoSolution=Map[Evaluate,Map[Association,Uncompress@Compress@Map[Normal,autoSolution,1]/.Removed[x_]:>MakeSymbol[x],1],All];
(*autoSolution=mapAtKeyEveryWhere[Map[Association,Uncompress@Compress@Map[Normal,autoSolution,1]/.Removed[x_]:>MakeSymbol[x],1],Evaluate,Symbol];*)
(*autoSolution=Map[If[Head[#]=!=Hold,Evaluate[#]]&,Map[Association,Uncompress@Compress@Map[Normal,autoSolution,1]/.Removed[x_]:>MakeSymbol[x],1],All];*)

If[autoSolution[Curved]=!=Association[Evaluate[False]],
IncludeTo[tmp[$manifold],<|Symbol->MakeSymbol["man"],Dimension->autoSolution[Curved,Dimension],Index->Letters["\[ScriptA]"]|>,Load->False];
IncludeTo[tmp[$metric],<|Symbol->MakeSymbol["gg"][-MakeSymbol["\[ScriptA]"],-MakeSymbol["\[ScriptB]"]],Manifold->MakeSymbol["man"],SignDet->autoSolution[Curved,SignDet],CovD->MakeSymbol["cd"],SymCovDQ->True,PrintAs->"g"|>,Load->False];
IncludeTo[tmp[$chart],
<|Symbol->MakeSymbol["curved"],Manifold->MakeSymbol["man"],CNumber->autoSolution[Curved,CNumber],Coord->autoSolution[Curved,Coord],ChartColor->Red|>,Load->False
];
];
If[autoSolution[Frame]=!=Association[Evaluate[False]],
IncludeTo[tmp[$frame],
<|Symbol->MakeSymbol["ee"][-MakeSymbol["\[ScriptA]"],MakeSymbol["\[GothicA]"]],Manifold->MakeSymbol["man"],Metric->MakeSymbol["\[Eta]\[Eta]"][-MakeSymbol["\[GothicA]"],-MakeSymbol["\[GothicB]"]],Index->Letters["\[GothicA]"],PrintAs->{"e","\[Eta]"}|>,Load->False
];
IncludeTo[tmp[$basis],
<|Symbol->MakeSymbol["flat"],VBundle->MakeSymbol["Frameman"],CNumber->autoSolution[Curved,CNumber],BasisColor->Blue|>,Load->False
];
];
If[autoSolution[Spin]=!=Association[Evaluate[False]],
IncludeTo[tmp[$spin],
<|Symbol->MakeSymbol["\[Omega]\[Omega]"][-MakeSymbol["\[ScriptA]"],-MakeSymbol["\[GothicA]"],-MakeSymbol["\[GothicB]"]],Manifold->MakeSymbol["man"],Index->Letters["\[DoubleStruckCapitalA]"],Metric->{MakeSymbol["gg"],MakeSymbol["\[Eta]\[Eta]"]},CovD->MakeSymbol["cd"],PrintAs->"\[Omega]"|>,Load->False
];
IncludeTo[tmp[$basis],
<|Symbol->MakeSymbol["spin"],VBundle->MakeSymbol["Spinman"],CNumber->Range[1,2^Floor[autoSolution[Curved,Dimension]/2]],BasisColor->Darker@Green|>,Load->False
];
];
If[autoSolution[Lie]=!=Association[Evaluate[False]],
IncludeTo[tmp[$bundle],
<|Symbol->MakeSymbol["Lieman"],Manifold->MakeSymbol["man"],Dimension->autoSolution[Lie,Dimension],Index->Letters["\[ScriptCapitalA]"],Metric->MakeSymbol["GG"],PrintAs->"\[DoubleStruckCapitalL]man"|>,Load->False
];
IncludeTo[tmp[$covd],
<|Symbol->MakeSymbol["DD"][-MakeSymbol["\[ScriptA]"]],Manifold->MakeSymbol["man"],VBundle->{MakeSymbol["Lieman"],MakeSymbol["Spinman"]},SymbolOfCovD->{";","D"}|>,Load->False
];
IncludeTo[tmp[$basis],
<|Symbol->MakeSymbol["lie"],VBundle->MakeSymbol["Lieman"],CNumber->autoSolution[Lie,CNumber],BasisColor->Brown|>,Load->False
];
];
If[autoSolution[#]=!={},IncludeTo[tmp[#],Values@autoSolution[#],Load->False]]&/@{$info,$metric,$covd,$form,$tensor,$spinor,$function,$constant,$parameter,$assumption,$rule,$equation};
tmp=Uniformize[$solution,tmp];

tmp[$info,tmp[$info][[1]][Symbol],Dimension]=tmp[$manifold][[1]][Dimension];
tmp[$info,tmp[$info][[1]][Symbol],Signature]=Which[
tmp[$metric][[1]][SignDet]===1,"E",
tmp[$metric][[1]][SignDet]===-1,"L"
];
tmp[$metric,MakeSymbol["gg"],Det,Symbol]=MakeSymbol["Det",MakeSymbol["gg"],MakeSymbol["curved"]];

tmp[$metric,MakeSymbol["gg"],Routine]=<|
Seed->Hold@ToArray[$solution][$solution[$metric,MakeSymbol["gg"],Expr]],
Chain->{
Seed->Permutations[{-1,-1}],
Inverse@Seed->Permutations[{1,1}]
},
Map:>$Map,ParallelMap:>$ParallelMap
|>;
tmp[$metric,MakeSymbol["gg"],Det,Routine]=<|
Seed->Hold@ToArray[$solution][Det@ToArray[$solution]@MakeSymbol["gg"][-MakeSymbol["\[ScriptA]"],-MakeSymbol["\[ScriptB]"]]],
Chain->{
Seed->Permutations[{}]
},
Map:>$Map,ParallelMap:>$ParallelMap
|>;
tmp[$metric,MakeSymbol["gg"],Christoffel,Routine]=<|
Seed->Hold@ToArray[$solution][ChristoffelToGradMetric@MakeSymbol["Christoffelcd"][-MakeSymbol["\[ScriptA]"],-MakeSymbol["\[ScriptB]"],-MakeSymbol["\[ScriptC]"]]],
Chain->{
Seed->Permutations[{-1,-1,-1}],
Permutations[{-1,-1,-1}]->Permutations[{1,-1,-1}],
Permutations[{1,-1,-1}]->Permutations[{1,1,-1}],
Permutations[{1,1,-1}]->Permutations[{1,1,1}]
},
Map:>$Map,ParallelMap:>$ParallelMap
|>;
tmp[$metric,MakeSymbol["gg"],Ricci,Routine]=<|
Seed->Hold@ToArray[$solution][RiemannToChristoffel@MakeSymbol["Riccicd"][-MakeSymbol["\[ScriptA]"],-MakeSymbol["\[ScriptB]"]]],
Chain->{
Seed->Permutations[{-1,-1}],
Permutations[{-1,-1}]->Permutations[{1,-1}],
Permutations[{1,-1}]->Permutations[{1,1}]
},
Map:>$Map,ParallelMap:>$ParallelMap
|>;
tmp[$metric,MakeSymbol["gg"],RicciScalar,Routine]=<|
Seed->Hold@ToArray[$solution][Tr@ToArray[$solution]@MakeSymbol["Riccicd"][MakeSymbol["\[ScriptA]"],-MakeSymbol["\[ScriptB]"]]],
Chain->{
Seed->Permutations[{}]
},
Map:>$Map,ParallelMap:>$ParallelMap
|>;
tmp[$metric,MakeSymbol["gg"],Riemann,Routine]=<|
Seed->Hold@ToArray[$solution][RiemannToChristoffel@MakeSymbol["Riemanncd"][-MakeSymbol["\[ScriptA]"],-MakeSymbol["\[ScriptB]"],-MakeSymbol["\[ScriptC]"],-MakeSymbol["\[ScriptD]"]]],
Chain->{
Seed->Permutations[{-1,-1,-1,-1}],
Permutations[{-1,-1,-1,-1}]->Permutations[{1,-1,-1,-1}],
Permutations[{1,-1,-1,-1}]->Permutations[{1,1,-1,-1}],
Permutations[{1,1,-1,-1}]->Permutations[{1,1,1,-1}],
Permutations[{1,1,1,-1}]->Permutations[{1,1,1,1}]
},
Map:>$Map,ParallelMap:>$ParallelMap
|>;
tmp[$frame,MakeSymbol["ee"],MetricTensor,Routine]=<|
Seed->Hold@ToArray[$solution][GenFlatMetric[$solution]],
Chain->{
Seed->Permutations[{-1,-1}],
Inverse@Seed->Permutations[{1,1}]
},
Map->Identity,ParallelMap->Identity
|>;
tmp[$frame,MakeSymbol["ee"],Vielbein,Routine]=<|
Seed->Hold@ToArray[$solution][Transpose@ToArray[$solution]@$solution[$frame,MakeSymbol["ee"],Expr]],
Chain->{
Seed->{{-1,1}},
{{-1,1}}->{{1,1},{-1,-1}},
{{1,1},{-1,-1}}->{{1,-1}}
},
Map:>$Map,ParallelMap:>$ParallelMap
|>;
tmp[$spin,MakeSymbol["\[Omega]\[Omega]"],Routine]=<|
Seed->Hold@ToArray[$solution][(MakeSymbol["ee"][-MakeSymbol["\[ScriptZ]"],-MakeSymbol["\[GothicA]"]](PD[-MakeSymbol["\[ScriptA]"]][MakeSymbol["ee"][MakeSymbol["\[ScriptZ]"],-MakeSymbol["\[GothicB]"]]]+MakeSymbol["Christoffelcd"][MakeSymbol["\[ScriptZ]"],-MakeSymbol["\[ScriptY]"],-MakeSymbol["\[ScriptA]"]]MakeSymbol["ee"][MakeSymbol["\[ScriptY]"],-MakeSymbol["\[GothicB]"]]))],
Chain->{
Seed->Permutations[{-1,-1,-1}]
},
Map:>$Map,ParallelMap:>$ParallelMap
|>;
tmp[$spin,MakeSymbol["\[Omega]\[Omega]"],Gamma,MakeSymbol["\[Eta]\[Eta]"],1,Routine]=<|
Seed->Hold@ToArray[$solution][GenFlatGamma[$solution,$solution[$chart,MakeSymbol["curved"],CNumber]]],
Chain->{
Seed->{{1,-1,1}},
{{1,-1,1}}->{{-1,-1,1}}
},
Map->Identity,ParallelMap->Identity
|>;
tmp[$spin,MakeSymbol["\[Omega]\[Omega]"],Gamma,MakeSymbol["\[Eta]\[Eta]"],2,Routine]=<|
Seed->Hold@ToArray[$solution][SplitGammaMatrix@MakeSymbol["Gamma\[Eta]\[Eta]2"][MakeSymbol["\[GothicA]"],MakeSymbol["\[GothicB]"],-MakeSymbol["\[DoubleStruckCapitalA]"],MakeSymbol["\[DoubleStruckCapitalB]"]]],
Chain->{
Seed->{{1,1,-1,1}}
},
Map->Identity,ParallelMap->Identity
|>;
tmp[$spin,MakeSymbol["\[Omega]\[Omega]"],Gamma,MakeSymbol["\[Eta]\[Eta]"],3,Routine]=<|
Seed->Hold@ToArray[$solution][SplitGammaMatrix@MakeSymbol["Gamma\[Eta]\[Eta]3"][MakeSymbol["\[GothicA]"],MakeSymbol["\[GothicB]"],MakeSymbol["\[GothicC]"],-MakeSymbol["\[DoubleStruckCapitalA]"],MakeSymbol["\[DoubleStruckCapitalB]"]]],
Chain->{
Seed->{{1,1,1,-1,1}}
},
Map->Identity,ParallelMap->Identity
|>;
tmp[$spin,MakeSymbol["\[Omega]\[Omega]"],Gamma,MakeSymbol["gg"],1,Routine]=<|
Seed->Hold@ToArray[$solution][(MakeSymbol["ee"][MakeSymbol["\[ScriptA]"],-MakeSymbol["\[GothicZ]"]] MakeSymbol["Gamma\[Eta]\[Eta]1"][MakeSymbol["\[GothicZ]"],-MakeSymbol["\[DoubleStruckCapitalA]"],MakeSymbol["\[DoubleStruckCapitalB]"]])],
Chain->{
Seed->{{1,-1,1}},
{{1,-1,1}}->{{-1,-1,1}}
},
Map:>$Map,ParallelMap:>$ParallelMap
|>;
tmp[$spin,MakeSymbol["\[Omega]\[Omega]"],Gamma,MakeSymbol["gg"],3,Routine]=<|
Seed->Hold@ToArray[$solution][SplitGammaMatrix@MakeSymbol["Gammagg3"][-MakeSymbol["\[ScriptA]"],-MakeSymbol["\[ScriptB]"],-MakeSymbol["\[ScriptC]"],-MakeSymbol["\[DoubleStruckCapitalA]"],MakeSymbol["\[DoubleStruckCapitalB]"]]],
Chain->{
Seed->{{-1,-1,-1,-1,1}}
},
Map:>$Map,ParallelMap:>$ParallelMap
|>;

sol=StrongValidate[$solution,tmp];
If[OptionValue[Load]===True,
(*Unload[sol,Verbose->OptionValue[Verbose]];*)
Load[sol,Verbose->OptionValue[Verbose]];
];
]


(* ::Subsection::Closed:: *)
(*IncludeTo*)


(* ::Input::Initialization:: *)
Attributes[IncludeTo]={HoldFirst};
Options[IncludeTo]={Load->True,Verbose->False};
IncludeTo[sol_[obj_],assoc_Association,OptionsPattern[]]:=Module[{tmp,key,keyStr},
tmp=obj@@(Normal@assoc);
StrongValidate[$solution,sol];
If[KeyExistsQ[tmp,Manifold],tmp[Manifold]=sol[$manifold][[1]][Symbol]];
tmp=Uniformize[obj,tmp];
key=extractIdentifier[tmp[Symbol]];
keyStr=ToString@key;
sol[obj]=Append[sol[obj],key->tmp];
(*sol=Uniformize[$solution,sol];*)
If[OptionValue[Load]===True,
(*QuietEcho@*)Quiet@Catch@Unload[sol[obj],key,Verbose->OptionValue[Verbose]];
key=MakeSymbol[keyStr];
Load[sol[obj],key,Verbose->OptionValue[Verbose]];,
Return@sol[obj,key];
];
]
IncludeTo[sol_[obj_],list_List,OptionsPattern[]]:=Module[{keys,keysStr},
IncludeTo[sol[obj],#,Load->False]&/@list;
If[OptionValue[Load]===True,
keys=extractIdentifier[#[Symbol]]&/@list;
keysStr=ToString/@keys;
(*QuietEcho@*)Quiet@Catch@Unload[sol[obj],keys,Verbose->OptionValue[Verbose]];
keys=MakeSymbol/@keysStr;
Load[sol[obj],keys,Verbose->OptionValue[Verbose]];,
Return@sol[obj];
]
]

IncludeTo[sol_[$assumption],assoc_Association,OptionsPattern[]]:=Module[{tmp,key,keyStr},
tmp=$assumption@@(Normal@assoc);
StrongValidate[$solution,sol];
If[KeyExistsQ[tmp,Manifold],tmp[Manifold]=sol[$manifold][[1]][Symbol]];
tmp=Uniformize[$assumption,tmp];
key=extractIdentifier[tmp[Symbol]];
sol[$assumption]=Append[sol[$assumption],key->tmp];
(*sol=Uniformize[$solution,sol];*)
If[OptionValue[Load]===True,
(*QuietEcho@*)Quiet@Catch@Unload[sol[$assumption],key,Verbose->OptionValue[Verbose]];
Load[sol[$assumption],key,Verbose->OptionValue[Verbose]];,
Return@sol[$assumption,key];
];
]
IncludeTo[sol_[$assumption],list_List,OptionsPattern[]]:=Module[{keys,keysStr},
IncludeTo[sol[$assumption],#,Load->False]&/@list;
If[OptionValue[Load]===True,
keys=extractIdentifier[#[Symbol]]&/@list;
(*QuietEcho@*)Quiet@Catch@Unload[sol[$assumption],keys,Verbose->OptionValue[Verbose]];
Load[sol[$assumption],keys,Verbose->OptionValue[Verbose]];,
Return@sol[$assumption];
]
]

IncludeTo[sol_[$form],assoc_Association,OptionsPattern[]]:=Module[{tmp,key,keyStr},
tmp=$form@@(Normal@assoc);
StrongValidate[$solution,sol];
If[KeyExistsQ[tmp,Manifold],tmp[Manifold]=sol[$manifold][[1]][Symbol]];
tmp=Uniformize[$form,tmp];
key=extractIdentifier[tmp[Symbol]];
keyStr=ToString@key;
sol[$form]=Append[sol[$form],key->tmp];
(*sol=Uniformize[$solution,sol];*)
If[tmp[Tensor]=!=None,
sol[$tensor]=Append[
sol[$tensor],
sol[$form,#,Tensor]->$tensor[
Symbol->sol[$form,#,Tensor]@@Join[-sol[$manifold,sol[$form,#,Manifold],Index][[1;;sol[$form,#,Degree]]],Level[sol[$form,#,Symbol],1]],
Manifold->sol[$form,#,Manifold],
Symmetry->Antisymmetric[Range[1,sol[$form,#,Degree]]],
Grassmann->Even,
Routine-><|
Seed->Hold@ToArray[$solution][$solution[$form,#,Expr]],
Chain->
Which[
sol[$form,#,Degree]===1,{
Seed->Permutations[{-1}],
Permutations[{-1}]->Permutations[{1}]
},
sol[$form,#,Degree]===2,{
Seed->Permutations[{-1,-1}],
Permutations[{-1,-1}]->Permutations[{1,-1}],
Permutations[{1,-1}]->Permutations[{1,1}]
},
sol[$form,#,Degree]===3,{
Seed->Permutations[{-1,-1,-1}],
Permutations[{-1,-1,-1}]->Permutations[{1,-1,-1}],
Permutations[{1,-1,-1}]->Permutations[{1,1,-1}],
Permutations[{1,1,-1}]->Permutations[{1,1,1}]
},
sol[$form,#,Degree]===4,{
Seed->Permutations[{-1,-1,-1,-1}],
Permutations[{-1,-1,-1,-1}]->Permutations[{1,-1,-1,-1}],
Permutations[{1,-1,-1,-1}]->Permutations[{1,1,-1,-1}],
Permutations[{1,1,-1,-1}]->Permutations[{1,1,1,-1}],
Permutations[{1,1,1,-1}]->Permutations[{1,1,1,1}]
},
sol[$form,#,Degree]=!=1||sol[$form,#,Degree]=!=2||sol[$form,#,Degree]=!=3||sol[$form,#,Degree]=!=4,_
],
Map:>$Map,ParallelMap:>$ParallelMap
|>
]
]
]&/@{key};
If[OptionValue[Load]===True,
(*QuietEcho@*)Quiet@Catch@Unload[sol[$form],key,Verbose->OptionValue[Verbose]];
key=MakeSymbol[keyStr];
Load[sol[$form],key,Verbose->OptionValue[Verbose]];,
Return@sol[$form,key];
];
]
IncludeTo[sol_[$form],list_List,OptionsPattern[]]:=Module[{keys,keysStr},
IncludeTo[sol[$form],#,Load->False]&/@list;
If[OptionValue[Load]===True,
keys=extractIdentifier[#[Symbol]]&/@list;
keysStr=ToString/@keys;
(*QuietEcho@*)Quiet@Catch@Unload[sol[$form],keys,Verbose->OptionValue[Verbose]];
keys=MakeSymbol/@keysStr;
Load[sol[$form],keys,Verbose->OptionValue[Verbose]];,
Return@sol[$form];
]
]

IncludeTo[sol_[$equation],assoc_Association,OptionsPattern[]]:=Module[{tmp,key,keyStr,def},
tmp=$equation@@(Normal@assoc);
def=Definition[Evaluate@tmp[Symbol]];
StrongValidate[$solution,sol];
If[KeyExistsQ[tmp,Manifold],tmp[Manifold]=sol[$manifold][[1]][Symbol]];
tmp=Uniformize[$equation,tmp];
key=extractIdentifier[tmp[Symbol]];
keyStr=ToString@key;
sol[$equation]=Append[sol[$equation],key->tmp];
(*sol=Uniformize[$solution,sol];*)
If[OptionValue[Load]===True,
(*QuietEcho@*)If[ToString@def=!=ToString@Null,Quiet@Catch@Unload[sol[$equation],key,Verbose->OptionValue[Verbose]]];
key=MakeSymbol[keyStr];
Load[sol[$equation],key,Verbose->OptionValue[Verbose]];,
Return@sol[$equation,key];
];
]
IncludeTo[sol_[$equation],list_List,OptionsPattern[]]:=Module[{keys,keysStr,def},
def=ToString[Definition[Evaluate@#[Symbol]]]&/@list;
IncludeTo[sol[$equation],#,Load->False]&/@list;
If[OptionValue[Load]===True,
keys=extractIdentifier[#[Symbol]]&/@list;
keysStr=ToString/@keys;
(*QuietEcho@*)If[!MatchQ[def,{ToString@Null..}],Quiet@Catch@Unload[sol[$equation],keys,Verbose->OptionValue[Verbose]]];
keys=MakeSymbol/@keysStr;
Load[sol[$equation],keys,Verbose->OptionValue[Verbose]];,
Return@sol[$equation];
]
]

IncludeTo[sol_[$info],assoc_Association]:=Module[{tmp},
tmp=$info@@(Normal@assoc);
tmp[Dimension]=sol[$manifold][[1]][Dimension];
tmp[Signature]=Which[
sol[$metric][[1]][SignDet]===1,"E",
sol[$metric][[1]][SignDet]===-1,"L"
];
sol[$info]=<|extractIdentifier@tmp[Symbol]->Uniformize[$info,tmp]|>;
]

IncludeTo[sol_[$rule],assoc_Association,OptionsPattern[]]:=Module[{tmp,key,keyStr},
tmp=$rule@@(Normal@assoc);
StrongValidate[$solution,sol];
If[KeyExistsQ[tmp,Manifold],tmp[Manifold]=sol[$manifold][[1]][Symbol]];
tmp=Uniformize[$rule,tmp];
key=extractIdentifier[tmp[Symbol]];
sol[$rule]=Append[sol[$rule],key->tmp];
]
(*sol=Uniformize[$solution,sol];*)
(*If[OptionValue[Load]===True,
(*QuietEcho@*)Quiet@Catch@Unload[sol[$assumption],key,Verbose->OptionValue[Verbose]];
Load[sol[$assumption],key,Verbose->OptionValue[Verbose]];,
Return@sol[$assumption,key];
];*)
IncludeTo[sol_[$rule],list_List,OptionsPattern[]]:=Module[{keys,keysStr},
IncludeTo[sol[$rule],#,Load->False]&/@list;
]
(*If[OptionValue[Load]===True,
keys=extractIdentifier[#[Symbol]]&/@list;
(*QuietEcho@*)Quiet@Catch@Unload[sol[$assumption],keys,Verbose->OptionValue[Verbose]];
Load[sol[$assumption],keys,Verbose->OptionValue[Verbose]];,
Return@sol[$assumption];
]*)


(* ::Subsection::Closed:: *)
(*DropFrom*)


(* ::Input::Initialization:: *)
Attributes[DropFrom]={HoldFirst};
Options[DropFrom]={Verbose->False};
DropFrom[sol_[obj_],key_Symbol,OptionsPattern[]]:=Module[{tmp,keyStr=ToString@key},
Unload[sol[obj],key,Verbose->OptionValue[Verbose]];
sol[obj]=KeyDrop[sol[obj],MakeSymbol[keyStr]];
]
DropFrom[sol_[$assumption],key_String,OptionsPattern[]]:=Module[{tmp},
Unload[sol[$assumption],key,Verbose->OptionValue[Verbose]];
sol[$assumption]=KeyDrop[sol[$assumption],key];
]
DropFrom[sol_[obj_],list_List,OptionsPattern[]]:=Module[{tmp,keysStr},
keysStr=ToString/@list;
Unload[sol[obj],list,Verbose->OptionValue[Verbose]];
sol[obj]=KeyDrop[sol[obj],MakeSymbol/@keysStr];
]
DropFrom[sol_[$assumption],list_List,OptionsPattern[]]:=Module[{tmp},
Unload[sol[$assumption],list,Verbose->OptionValue[Verbose]];
sol[$assumption]=KeyDrop[sol[$assumption],list];
]
DropFrom[sol_[$info],key_String]:=sol[$info]=KeyDrop[sol[$info],key];


(* ::Section:: *)
(*Save*)


(* ::Subsection::Closed:: *)
(*Helpers*)


(* ::Input::Initialization:: *)
setDirectory[path_String]:=(
$Directory:=Evaluate@path;
Quiet[CreateDirectory[$Directory]];
Return[$Directory];
)
$Directory/:Set[$Directory,path_]:=setDirectory[path]


(* ::Input::Initialization:: *)
SaveNotebookCopy[path_]:=Module[{
obj,
tmp
},
obj=EvaluationNotebook[];
If[!FailureQ[Quiet[NotebookFileName[]]],
tmp=NotebookFileName[];
NotebookSave[obj,tmp];
NotebookSave[obj,path];
NotebookOpen[tmp];
NotebookOpen[path];
,
NotebookSave[obj,path];
]
]


(* ::Input::Initialization:: *)
Attributes[GenName]={HoldFirst};
GenName[sol_]:=Module[{info=sol[$info][[1]]},
Return@StringJoin[info[Symbol],"_",ToString@info[Dimension],"d",info[Signature],"_",info[CoordSystem],"_",info[Version],"_",info[Author]]
]


(* ::Input::Initialization:: *)
SolutionNames[]:=FileNameSplit[#][[-1]]&/@FileNames[All,$Directory]


(* ::Input::Initialization:: *)
UpdateAutocomplete[]:=If[SolutionNames[]=!={},
AddCompletion["Retrieve"->{Join[{""},SolutionNames[]]}]
]


(* ::Subsection::Closed:: *)
(*Store*)


(* ::Input::Initialization:: *)
Attributes[Store]={HoldFirst};
Store[sol_]:=Module[{
directoryPath,
notebookPath,
dataPath,
currentDataFiles,
id
},
SeedRandom[$MachineID];
id=ToString@RandomInteger[{10000,99999}];
SeedRandom[];
sol[$info,sol[$info][[1]][Symbol],Timestamp]=DateString["ISODateTime"];
sol[$info,sol[$info][[1]][Symbol],Author]=StringJoin[$Username,"-",$MachineName,"-",id];
sol[$info,sol[$info][[1]][Symbol],Dimension]=sol[$manifold][[1]][Dimension];
sol[$info,sol[$info][[1]][Symbol],Signature]=Which[
sol[$metric][[1]][SignDet]===1,"E",
sol[$metric][[1]][SignDet]===-1,"L"
];

directoryPath=FileNameJoin[{$Directory,GenName[sol]}];
Quiet[CreateDirectory[directoryPath]];
notebookPath=FileNameJoin[{directoryPath,StringJoin[GenName[sol],".nb"]}];
dataPath=FileNameJoin[{directoryPath,StringJoin[sol[$info][[1]][Timestamp],".m"]}];

currentDataFiles=Select[FileNames[All,directoryPath],(FileExtension[#]==="m")&];
While[Length@currentDataFiles>4,DeleteFile[currentDataFiles[[1]]];currentDataFiles=Select[FileNames[All,directoryPath],(FileExtension[#]==="m")&];];

Put[Compress[sol],dataPath];
UpdateAutocomplete[];
SaveNotebookCopy[notebookPath];
]


(* ::Subsection::Closed:: *)
(*Retrieve*)


(* ::Input::Initialization:: *)
Retrieve[string_,int_]:=Module[{
directoryPath
},
directoryPath=FileNameJoin[{$Directory,string}];
Return@Uncompress@Get[Select[FileNames[All,directoryPath],(FileExtension[#]==="m")&][[int-1]]]
]
Retrieve[string_]:=Retrieve[string,0]


(* ::Section::Closed:: *)
(*Definitions*)


(* ::Text:: *)
(*Define your public and private symbols here:*)


SayHello[name_? StringQ] := "Hello " <> name <> "! Its a good day to live!";


(* ::Section:: *)
(*Protect*)


(* ::Section::Closed:: *)
(*Package Footer*)


End[];
EndPackage[];
