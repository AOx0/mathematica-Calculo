#!/usr/bin/env wolframscript
(* ::Package:: *)

Par := f |-> (f[2] == f[-2])
Impar := f |-> (f[-2] == -f[2])
Tipo := f |-> Which[Par[f], "Par", Impar[f], "Impar", !Par[f] || !Impar[f], "Nope"]


Graphics[{Thick,Red,Line[{{-15/2,0.2},{10,0.2}}],{PointSize[Large],Point[{-15/2,0.2}]},White,PointSize[Medium],(*Point[{-4,0.2}]*)},Axes->{True,False},PlotRange->{{-10,10},{0,0}}]


Simplify[(Sqrt[10-3x]+Sqrt[2x+15])/(x^2-9)]


Graphics[{Thick,Red,(*Line[{{10/3,0.2},{-10,0.2}}]*) ,{PointSize[Large],Point[{3,0.2}]}, {PointSize[Large],Point[{-3,0.2}]},White,PointSize[Medium],Point[{-3,0.2}], Point[{3,0.2}]},Axes->{True,False},PlotRange->{{-10,10},{0,0}}]


Graphics[{Thick,Red,(*Line[{{10/3,0.2},{-10,0.2}}]*) ,{PointSize[Large],Point[{3,0}]},(*White,PointSize[Medium],Point[{-3,0.2}], Point[{3,0.2}]*)},Axes->{True,False},PlotRange->{{1,5},{0,0}}]


Tipo[x|->x^2]
