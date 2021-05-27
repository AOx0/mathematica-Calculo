#!/usr/bin/env wolframscript
(* ::Package:: *)

BiggerFrom[n1_, n2_] := (
	If[n1>n2,
		Return[{n2, n1}],
		Return[{n1, n2}]
	]
)

(* Area entre 2 curvas: f y g *)
AreaEntre[f_, g_] := (
Clear[solutions, x1, x2, x, reorder, te];
    solutions = Flatten[Values@Solve[f == g]] ;
    x1 = solutions[[1]];
    x2 = solutions[[2]];
  If[\!\(
\*SubsuperscriptBox[\(\[Integral]\), \(x1\), \(x2\)]\(\((Together[\ \(-\((f - \ g)\)\)])\) \[DifferentialD]x\)\) < 0, AreaEntre[g, f], 

    Print["Area entre dos curvas"];
    Print["y = ", TraditionalForm[f], "\ny = ", TraditionalForm[g]];
    Print["------------"];
    
    
    Print[TraditionalForm[f], " = ", TraditionalForm[g]];
    Print[TraditionalForm[Together[f - g]], " = 0"];
    Print["x = ", Flatten[Values@Solve[f == g]]];
    Print["x = ", N[Flatten[Values@Solve[f == g]]]];
    solutions =   N[Flatten[Values@Solve[f == g]]];
    
    x1 = Query[1][solutions]/._Missing->0;
    x2 = Query[2][solutions]/._Missing->0;
    
    reorder = BiggerFrom[x1, x2];
    
    x1 = reorder[[1]];
    x2 = reorder[[2]];
    
    Print["------------"];
    
    
    Print[StringForm["\!\(\*SubsuperscriptBox[\(\[Integral]\), \(``\), \(``\)]\)(``)\[DifferentialD]x", x1, x2, TraditionalForm[Together[-(f - g)]]]];
    te = \[Integral](Together[-(f - g)]) \[DifferentialD]x ;
    Print["= ", \[Integral]( Together[-(f - g)]) \[DifferentialD]x , 
        StringForm["\!\(\*SubsuperscriptBox[\(|\), \(\(``\)\(\\\\n\)\), \(``\)]\)", x1, x2]
    ];
    Print[ With[{t = te}, HoldForm[t ]/. x -> x2], "-(", With[{t = te}, HoldForm[t ]/. x -> x1], ")"];
    Print[ te /. x -> x2, "-(", te /. x -> x1, ")"];
    Print["------------"];
    
    
    Print["Area = ", \!\(
\*SubsuperscriptBox[\(\[Integral]\), \(x1\), \(x2\)]\(\((Together[\ \(-\((f - \ g)\)\)])\) \[DifferentialD]x\)\), 
        " \!\(\*SuperscriptBox[\(u\), \(2\)]\)"];
    Print["Area = ", N[\!\(
\*SubsuperscriptBox[\(\[Integral]\), \(x1\), \(x2\)]\(\((Together[\ \(-\((f - \ g)\)\)])\) \[DifferentialD]x\)\)], 
        " \!\(\*SuperscriptBox[\(u\), \(2\)]\)"
    ];
    ];
)
