#!/usr/bin/env wolframscript
(* ::Package:: *)

BiggerFrom[n1_, n2_] := (
	If[n1>n2,
		Return[{n2, n1}],
		Return[{n1, n2}]
	]
)

(* Utilidad *)
Utilidad[I_, C_] := (
Clear[solutions, i1, i2, x, reorder, expr1, expr2, expression];
	solutions = N[Flatten[Values@Solve[I == C]]];
    i1 = Query[1][solutions]/._Missing->0;
    i2 = Query[2][solutions]/._Missing->0;
    reorder = BiggerFrom[i1, i2];
    i1 = reorder[[1]];
    i2 = reorder[[2]];
    
	If [\!\(
\*SubsuperscriptBox[\(\[Integral]\), \(i1\), \(i2\)]\(\((Together[\ \(-\((I - \ C)\)\)])\) \[DifferentialD]t\)\) < 0,
	Utilidad[C, I],   
    Print["U = I - C"];
    Print["I = ", TraditionalForm[I]];
    Print["C = ", TraditionalForm[C]];
    
    Print["\n", TraditionalForm[I==C]];
    Print[TraditionalForm[Simplify[Together[-(I-C)]]], " = 0"];
    Print["t = ", Flatten[Values@Solve[I == C]]];
    Print["t = ", N[Flatten[Values@Solve[I == C]]]];
    solutions = N[Flatten[Values@Solve[I == C]]];
    
    
    i1 = Query[1][solutions]/._Missing->0;
    i2 = Query[2][solutions]/._Missing->0;
    
    reorder = BiggerFrom[i1, i2];
    
    i1 = reorder[[1]];
    i2 = reorder[[2]];
    
    Print[StringForm[" \!\(\*SubsuperscriptBox[\(\[Integral]\), \(``\), \(``\)]\)(``)\[DifferentialD]x",i1, i2, With[{x1=I, x2=C},HoldForm[-(I-C)]]]];
    Print[StringForm[" \!\(\*SubsuperscriptBox[\(\[Integral]\), \(``\), \(``\)]\)(``)\[DifferentialD]x",i1, i2, TraditionalForm[Simplify[Together[-(I-C)]]]]];
    Print[StringForm[" `` |\*GridBox[{
{``},
{``}
}]", TraditionalForm[\[Integral]( Together[-(I-C)]) \[DifferentialD]t],  i2, i1]];
    expression = \[Integral]( Together[-(I-C)]) \[DifferentialD]t;
    expr1 = With[{xx = expression},HoldForm[xx ]/.t-> (i1)] ;
    expr2 = With[{xx = expression},HoldForm[xx ]/.t-> (i2)] ;
    
    Print[ TraditionalForm[expr2 - (expr1)]];
    Print[With[{x1 = ReleaseHold[expr1],x2 = ReleaseHold[expr2] }, HoldForm[(x2-x1)]]];
    Print[With[{x1 =N[ ReleaseHold[expr1]],x2 = N[ReleaseHold[expr2] ]},HoldForm[(x2-x1)]]];
    Print["Utilidad =  $", N[(ReleaseHold[expr2] - ReleaseHold[expr1])]];
    ]
    
)
