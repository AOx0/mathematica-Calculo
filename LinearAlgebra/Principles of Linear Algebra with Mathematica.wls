#!/usr/bin/env wolframscript
(* ::Package:: *)

Remove["Global`*"]


(* ::Title:: *)
(*Principles of Linear Algebra with Mathematica*)


(* ::Subtitle:: *)
(*Chapter 1: Basics of Mathematica*)


(* ::Text:: *)
(*The very basics of Mathematica very quickly.*)


(* ::Subsubtitle::Closed:: *)
(*Basic Arithmetic*)


(* ::Input:: *)
(*1+1*)


(* ::Input:: *)
(*N[1/3, 9]*)


(* ::Input:: *)
(*Sum[\[Pi]^k, {k, 2, 10, 2}]*)


(* ::Input:: *)
(*N[Sum[\[Pi]^k, {k, 2, 10, 2}], 10]*)


(* ::Input:: *)
(*N[Sum[\[Pi]^k, {k, 2, 10, 2}]]*)


(* ::Input:: *)
(*f = 5x - 2y + 7z==15;*)
(*ContourPlot3D[Evaluate[f],{x,-7,7}, {y,-7,7}, {z,-7,7}, Mesh->None, ContourStyle->Red]*)


(* ::Text:: *)
(*ContourPlot solves the given equation such that evaluating it with x and y the expression is true.*)
(*For example, with the equation x + y == 10, when x = 10 and y = 0 the expression evaluates to True. Because 10 + 0 == 10 evaluates to True. In the graph below it is more than clear:*)


(* ::Input:: *)
(*ContourPlot[x+y ==10, {x, -20,20}, {y, -20, 20},GridLines->Full]*)


(* ::Input:: *)
(*DensityPlot[x+y,{x,-5,5},{y,-5,5}]*)


(* ::Subsubtitle::Closed:: *)
(*Basic Algebra*)


(* ::Input:: *)
(*NRoots[x^2==255, x]*)


(* ::Input:: *)
(*x^2==255 /. {ToRules[NRoots[x^2==255, x]]}*)


(* ::Input:: *)
(*PolynomialQuotientRemainder[2x,x,x]*)


(* ::Input:: *)
(*(2x)/x*)


(* ::Input:: *)
(*Print["Jajaja"]; Print["Adios"]; Print["Lol"]*)


(* ::Subsubtitle::Closed:: *)
(*Matrices & Lists*)


(* ::Input:: *)
(*L1 = {2, 4, 1,2,4,5, a};*)
(*L2 = {a, b, c};*)
(*Union[L1, L2] (* 'Joins' every element once *)*)
(*Intersection[L1, L2] (* Elements that are present in both lists *)*)
(*Complement[L1, L2] (* All elements of the first list that do not exist in the second one *)*)
(*Complement[L2, L1] (* The order of the arguments matters *)*)
(*Join[L1, L2] (* 'Joins' every element, repetiton is allowed *)*)
(**)


(* ::Input:: *)
(*M = {{1,2,3},{4,5,6},{7,8,9}};*)
(*MatrixForm[M]*)


(* ::Input:: *)
(*K = {{-1,-1,-1},{-1,-1,-1},{-1,-1,-1}};*)
(*(L = M + K) // MatrixForm*)


(* ::Input:: *)
(*Det[M] == Det[K]==Det[L]*)


(* ::Input:: *)
(*Inverse[{{-5, 7, 1},{0,-4,6},{2,-1,9}}] // MatrixForm*)


(* ::Input:: *)
(*Transpose[M] // MatrixForm*)


(* ::Subsubtitle::Closed:: *)
(*Expressions vs Functions*)


(* ::Input:: *)
(*f[x_] = 5x;*)
(*g = 5x;*)
(*h = x |->5x;*)
(*StringJoin[ToString[h[[0]]]," ",  ToString[h[[1]]]," ",  ToString[h[[2]]]]== (StringJoin[ToString[h[[0]]]," ",  ToString[h[[1]]]," ",  ToString[h[[2]]]])/. h -> f*)
(*f[1] == (g/.x-> 1) == f@1  == h[1] == h@1 == h[[2]]/. x->1*)


(* ::Text:: *)
(*To achieve (f \[SmallCircle] g)(x) we write f@f[x] since  (f \[SmallCircle] g)(x) = f(g(x))*)


(* ::Input:: *)
(*f@f[x]*)


(* ::Text:: *)
(*Mathematica supports piecewise function declaration. Piecewise[{{Result, Condition}, {Result, Condition}, ...}]. When no condition evaluates to True the function returns 0 *)


(* ::Input:: *)
(*f[x_] = Piecewise[{{5x, x>0},{-5x, x<0}}];*)
(*DiscretePlot[f[x], {x, -10,10}]*)


(* ::Input:: *)
(**)


(* ::Subsubtitle::Closed:: *)
(*Plotting*)


(* ::Input:: *)
(*f = x |-> x Sin[x^2 E^x];*)
(*Plot[f[x], {x, -1,2}, PlotStyle->Red, PlotRange->{-2,2}]*)


(* ::Input:: *)
(*f = x |-> x Sin[x^2 E^x];*)
(*g = x |->E^(1/2 Sin[3x]);*)
(*Plot[{f[x], g[x]}, {x, -1,2}, PlotStyle->{Directive[Red, Thickness[0.007]], Directive[Blue, Thickness[0.01]]}, PlotRange->{-2,2}]*)
(**)


(* ::Input:: *)
(*ParametricPlot[{{Sin[2t], Sin[3t]}, {Sin[t], Cos[t]}}, {t,0,2\[Pi]}, PlotStyle->{Directive[Blue, Thick], Directive[Red, Thick]}]*)


(* ::Input:: *)
(*ContourPlot[{x^2+y^2==1, x^4+y^4==1}, {x, -1, 1}, {y, -1, 1}]*)


(* ::Input:: *)
(*ContourPlot[{(x-5)^2/49+(y-10)^2/144==1,(x-5)^2/49-(y-10)^2/144==1}, {x, -10, 25}, {y, -10, 25}]*)


(* ::Input:: *)
(*ContourPlot[{(x-5)^2/49+(y-10)^2/144==1,(x-5)^2/49-(y-10)^2/144==1}, {x, -10, 25}, {y, -10, 25}, ContourStyle->{Red, Blue}]*)


(* ::Subsubtitle::Closed:: *)
(*Animating*)


(* ::Input:: *)
(*f = x |->Sin[x];*)
(*Animate[Plot[f[x], {x, 0, A}], {A, 0.01, 4\[Pi]}, AnimationRunning->False]*)


(* ::Input:: *)
(*f = x |->Sin[x];*)
(*Animate[Plot[f[x], {x, 0, A}, PlotRange->{{0,4\[Pi]}, {-1.01,1.01}}], {A, 0.01, 4\[Pi]}, AnimationRunning->False]*)


(* ::Input:: *)
(*f = x |->Sin[x];*)
(*g = x |->Sin[x];*)
(*Animate[*)
(*{*)
(*Plot[f[x], {x, 0, A}, PlotRange->{{0,4\[Pi]}, {-1.01,1.01}}],*)
(*Plot[f[x+A], {x, 0, 4\[Pi]}, PlotRange->{{0,4\[Pi]}, {-1.01,1.01}}]*)
(*}, {A, 0.01, 4\[Pi]}, AnimationRunning->False]*)


(* ::Input:: *)
(*ellipses := (x-10Sin[A])^2/4.+(y-10Cos[A])^2/25.==1;*)
(* Animate[*)
(*  ContourPlot[Evaluate[(ellipses) /. A->B], {x,-15,15}, {y, -15, 15}, PlotPoints->100, Epilog->{ Red, Thick, Circle[{0,0}, 10]}], *)
(*{B, 0, 2\[Pi]}, AnimationRate->.15, AnimationRunning->False*)
(*  ]*)


(* ::Input:: *)
(*f= Sin[x+y]Cos[x-y]+3;*)
(*g = Sin[x y]+e^-(x^2+y^2);*)
(*Plot3D[{f, g}, {x,-3,3}, {y, -2, 2}]*)


(* ::Input:: *)
(*f[x_,y_]:= Sin[x+y]Cos[x-y]+3;*)
(*g = Sin[x y]+e^-(x^2+y^2);*)
(*Animate[*)
(*Plot3D[{f[x, y], g}, {x,-A,A}, {y, -A+1, A-1}],*)
(*{A, -3, 3}*)
(*]*)


(* ::Subtitle:: *)
(*Chapter 2. fvfvfv*)
