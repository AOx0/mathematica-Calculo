#!/usr/bin/env wolframscript
(* ::Package:: *)

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


(* ::Subtitle:: *)
(*Chapter 2. fvfvfv*)
