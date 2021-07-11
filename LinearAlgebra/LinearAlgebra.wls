#!/usr/bin/env wolframscript
(* ::Package:: *)

Get[StringJoin[SetDirectory[NotebookDirectory[]], "/MainScripts.wl"]]


Export["mantest.mp4",
Manipulate[
	VPG[
		PointGrid[-10, 10],
		
		HighlightV -> NormalAxes[-10,10],
		Matrix->  { {1+(-(2/3)Transformation),0+(-(1/3)Transformation)},{0+(1/3 Transformation),1+(-(1/3)Transformation)}} ,
		PlotRange-> {{-3,3},{-3,3}}
	
	],{Transformation, 0, 1, 0.05}
]
]
