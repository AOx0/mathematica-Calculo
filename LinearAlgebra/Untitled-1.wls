#!/usr/bin/env wolframscript
(* ::Package:: *)

PathIn = triangle |-> Module[{biggest, level=2, sum=0, index=1},
	biggest = {ToExpression[triangle[[1]][[1]]]};
	Do[
		If[
			ToExpression[triangle[[level]][[index]]] > ToExpression[triangle[[level]][[index+1]]],
			AppendTo[biggest, ToExpression[triangle[[level]][[index]]]],
			AppendTo[biggest, ToExpression[triangle[[level]][[index+1]]]];
			index += 1
		];
		level += 1
	,{l, 1, Length[triangle]-1}];
	biggest
];

GetTriangle = {url, from, to} |->Module[{w, v={}},
	w = Import[url];
	w = StringCases[w,from~~x:Shortest[___]~~to:>x][[1]];
	Do[v = Join[v,{ StringSplit[x," "]}], {x, StringSplit[w, "\n"]}];
	v = DeleteCases[v, {}];
	v
];
