#!/usr/bin/env wolframscript
(* ::Package:: *)

Remove["Global`*"]
Intersec[f_] := ( 
Clear[PosX, PosY];
(*
    f'' se iguala a 0.
   Con esto se obtienen todas las posiciones en x donde hay una intersecci\[OAcute]n
*)
Print[StringForm["Intersecciones de ``", Style[TraditionalForm@f[[2]], Bold]]];
Print[StringForm["f' = ``",TraditionalForm[f'[[2]]]]];
Print[StringForm["f'' = ``",TraditionalForm[f''[[2]]]]];
Print["x = ", Values@Solve[f''[x]==0]];
PosX= Flatten@Values@Solve[f''[x] == 0];
PosY = {};

Do[
AppendTo[PosY, f[z]];
Print[StringForm["     ``", With[{xx=TraditionalForm[f]}, HoldForm[xx]/.x-> z]]];
, {z, PosX}];

PosY = Flatten@PosY;
Print[Transpose[{PosX, PosY}]];
)

Intersec[x |->E^-x^2]
Pendiente[f_, en_]:=( 
m = f'[en]/en;
Print[m]
)
Pendiente[x|-> x^2, 5]

Ley de senos

Sin[A]/a = Sin[B]/b = Sin[C]/c

Pendiente:

m = (y2-y1)/(x2-x1)
