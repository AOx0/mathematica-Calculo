#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Volumen en intervalo *)
Volumen[f_, intervalo_] := (
    Clear[i1, i2, expression, expr1, expr2];
    i1 = intervalo[[1]];
    i2 = intervalo[[2]];
    Print[StringForm[
        "Area bajo `` en el intervalo de ``",
            Style[StringForm["y= ``", TraditionalForm[f]], Bold],
            Style[StringForm["[``,``]", i1, i2], Bold]
        ]
    ];
    Print[StringForm[" \!\(\*SubsuperscriptBox[\(\[Integral]\), \(``\), \(``\)]\)``\[DifferentialD]x",i1, i2, \[Pi] TraditionalForm[f] ^2]];
    Print[StringForm[" \!\(\*SubsuperscriptBox[\(\[Integral]\), \(``\), \(``\)]\)``\[DifferentialD]x",i1, i2, \[Pi] TraditionalForm[Expand[f ^2]]]];
    Print[StringForm[" `` |\*GridBox[{
{``},
{``}
}]", \[Pi] TraditionalForm[\[Integral](Expand[f ^2])\[DifferentialD]x],  i2, i1]];
    expression := \[Integral](Expand[f ^2])\[DifferentialD]x;
    expr1 = With[{xx = expression},HoldForm[xx ]/.x-> (i1)] ;
    expr2 = With[{xx = expression},HoldForm[xx ]/.x-> (i2)] ;
    Print[ \[Pi] TraditionalForm[expr2- (expr1)]];
    Print[With[{x1 = ReleaseHold[expr1],x2 = ReleaseHold[expr2] }, HoldForm[\[Pi] (x2-x1)]]];
    Print[With[{x1 =N[ ReleaseHold[expr1]],x2 = N[ReleaseHold[expr2] ]},HoldForm[\[Pi] (x2-x1)]]];
    Print["Volumen = ", N[\[Pi] (ReleaseHold[expr2] - ReleaseHold[expr1])], " \!\(\*SuperscriptBox[\(u\), \(3\)]\)"]
);
