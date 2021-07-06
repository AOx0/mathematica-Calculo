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

vpgc = {list, hLi, saxis, prange} |-> (
	listTemp = {}; Do[listTemp = Join[listTemp, {rp[{v[[1]], v[[2]]}]} ],{v, list}];
	nL = Join[{Red, Thick}, listTemp];
	If[hLi != {}, 
		listTemp2 = {}; Do[listTemp2 = Join[listTemp2, {rp[{v[[1]], v[[2]]}]} ],{v, hLi}];
		hL = Join[{Blue, Thick}, listTemp2];
	];
	Graphics[ If[hLi != {}, Join[nL, hL], nL] , Axes->saxis, PlotRange->prange]
);

vpg = list |-> (
	listTemp = {}; Do[listTemp = Join[listTemp, {rp[{v[[1]], v[[2]]}]} ],{v, list}];
	Graphics[Join[{Red, Thick}, listTemp],Axes->True, PlotRange->{{-10,10},{-10,10}}]
);

MToV = {V, matrix}|-> ((xel |-> matrix . xel)/@V);

Quiet[CustomMatrix=CustomMatrix; HighlightV=HighlightV];
Protect[CustomMatrix, HighlightV];
Options[VPG] = {CustomMatrix -> {{1,0},{0,1}}, HighlightV -> {}};
VPG[list_, opts : OptionsPattern[{Graphics, VPG}]] := (
	Module[{sa, pr, ma, list2, hList},
	ma = OptionValue[CustomMatrix];
	sa = OptionValue[Axes];
	pr = OptionValue[PlotRange];
	list2 = list;
	hList = OptionValue[HighlightV];
	If[ma != {{1,0},{0,1}}, list2 = MToV[list, ma]];
	If[ma != {{1,0},{0,1}}, hList = MToV[hList, ma]];
	vpgc[list2, hList, sa, pr]]
)


PointsInRangeX = {x, Yr} |-> (
	L = {};
	Do[L = Join[{{x, y}}, L], {y, Yr[[1]], Yr[[2]]}];
	L
);

PointsInRangeY = {y, Xr} |-> (
	L = {};
	Do[L = Join[{{x, y}}, L], {x, Xr[[1]], Xr[[2]]}];
	L
);

PointGrid = {x, y} |-> (
	LA = {};
	Do[LA = Join[PointsInRangeX[xx, {x,y}], LA],{xx, x, y}];
	LA
);

NormalAxes = {x, y} |->  Join[PointsInRangeX[0, {x,y}],PointsInRangeY[0, {x,y}] ];


Manipulate[
	VPG[
		PointGrid[-10, 10],
		
		HighlightV -> NormalAxes[-10,10],
		CustomMatrix->  { {1+(-(2/3)Transformation),0+(-(1/3)Transformation)},{0+(1/3 Transformation),1+(-(1/3)Transformation)}} ,
		PlotRange-> {{-10,10},{-10,10}}
	
	],{Transformation, 0, 1, 0.05}
]
