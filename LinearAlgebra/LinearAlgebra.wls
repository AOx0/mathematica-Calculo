#!/usr/bin/env wolframscript
(* ::Package:: *)

Unprotect[CustomMatrix];
Remove["Global`*"];


Z = x |-> (r = {}; Do[r = Join[r, {0}];, x ]; Return[r];);
r2 = v |-> Arrow[{{0,0},v}];
rp = v |-> Point[v];


vag = list |-> (
	listTemp = {}; Do[listTemp = Join[listTemp, {r2[{v[[1]], v[[2]]}]}],{v, list}];
	Graphics[Join[{Red, Thick}, listTemp],Axes->True, PlotRange->{{-10,10},{-10,10}}]
);

vpgc = {list, saxis, prange} |-> (
	listTemp = {}; Do[listTemp = Join[listTemp, {rp[{v[[1]], v[[2]]}]} ],{v, list}];
	Graphics[Join[{Red, Thick}, listTemp],Axes->saxis, PlotRange->prange]
);

vpg = list |-> (
	listTemp = {}; Do[listTemp = Join[listTemp, {rp[{v[[1]], v[[2]]}]} ],{v, list}];
	Graphics[Join[{Red, Thick}, listTemp],Axes->True, PlotRange->{{-10,10},{-10,10}}]
);

MToV = {V, matrix}|-> ((xel |-> matrix . xel)/@V);

Protect[CustomMatrix];
Options[VPS] = {CustomMatrix -> {{1,0},{0,1}}};
VPG[list_, opts : OptionsPattern[{Graphics, VPS}]] := (
	Module[{sa, pr, ma, list2},
	ma = OptionValue[CustomMatrix];
	sa = OptionValue[Axes];
	pr = OptionValue[PlotRange];
	list2 = list;
	If[ma != {{1,0},{0,1}}, list2 = MToV[list, ma]];
	vpgc[list2, sa, pr]]
)


PointsInRange = {x, Yr} |-> (
	L = {};
	Do[L = Join[{{x, y}}, L], {y, Yr[[1]], Yr[[2]]}];
	L
);

PointGrid = {x, y} |-> (
	LA = {};
	Do[LA = Join[PointsInRange[xx, {x,y}], LA],{xx, x, y}];
	LA
);


VPG[PointGrid[-10, 10], CustomMatrix->  { {1/3, -1/3},{1/3, 2/3}}]


Manipulate[
	VPG[PointGrid[-10, 10],
	CustomMatrix ->  { {1+(-(2/3)x),0+(-(1/3)x)},{0+(1/3 x),1+(-(1/3)x)}} ,
	PlotRange -> {{-10,10},{-10,10}}], {x, 0, 1, 0.05}
]
