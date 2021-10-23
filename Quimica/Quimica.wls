#!/usr/bin/env wolframscript
(* ::Package:: *)

Unprotect[e];
e = <||>;
Do[
e@Symbol[ToString[ k[[1]]]]=QuantityMagnitude[k[[2]]], 
{k, EntityValue[EntityClass["Element",All],{"AtomicSymbol","AtomicMass"}]}
];
Protect[e];
Avogadro := 6.023*10^23;
Moles = Symbol["Moles"];
Moleculas = Symbol["Moleculas"];

SymbolQ = Function[s,
  MatchQ[Unevaluated@s, t_Symbol /; AtomQ[Unevaluated@t]], 
  HoldAllComplete
];
  
MolesMoleculas = {moles, moleculas} |-> Module[{solveFor},
	solveFor = Which[SymbolQ[moles], "moles", SymbolQ[moleculas], "moleculas"];
    Print[StringReplace[ToString[TraditionalForm[Solve[ Symbol["moleculas"] ==  Symbol["moles"] * Symbol["avogadro"]/1 ,  Symbol[solveFor]][[1]][[1]]]], {"moleculas"..-> ToString[ToString[TraditionalForm[moleculas]] " moleculas"], "moles" ..-> ToString[ToString[TraditionalForm[moles]] " moles"] }]];
	Solve[ moleculas == moles * Avogadro/1 ][[1]][[1]]
];


MolesMoleculas[Moles,2.709 10^24]


MolesMoleculas[Evaluate[1/16],Moleculas]
MolesMoleculas[Evaluate[1/32],Moleculas]
MolesMoleculas[Evaluate[1/48],Moleculas]


(*
(
mol rl)/(indice rl) * (peso re)/(indice re)=g que si se usaron r.e. 
g originales - que si se usaron  = r.e exedente

*)
