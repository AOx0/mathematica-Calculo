#!/usr/bin/env wolframscript
(* ::Package:: *)

(* ::Text:: *)
(**)


(* ::Title:: *)
(*Termodin\[AAcute]mica*)


(* ::Text:: *)
(*Ejemplo: Energ\[IAcute]a solar. En las celdas solares hay problemas porque.*)
(*La energ\[IAcute]a final del sistema es la suma de las distintas energ\[IAcute]as que tengamos. *)
(**)
(*Las dos energ\[IAcute]as que existen:*)
(*	- Cin\[EAcute]tica: *)
(*	- Potencial: Por altura*)
(*	*)
(*\[CapitalDelta]E = Q + W*)
(*Q = \[CapitalDelta]E - W |-> Entlap\[IAcute]a *)
(*Donde Q es calor y W es trabajo*)
(**)
(*Hay tres tipos de sistemas:*)
(*	- Abierto: Se pierde masa y energ\[IAcute]a*)
(*	- Cerrado: Energ\[IAcute]a. *)
(*	- Abi\[AAcute]tico: Aislado *)
(**)
(*Ley cero (equilibrio): Cuando pones dos cuerpos de distintas temperaturas llegaran a un equilibrio.*)
(**)
(*En el refri la cerveza est\[AAcute] fr\[IAcute]a. La sacamos y se calienta. Si abrimos la serveza pierde el gas que tiene la misma.*)
(**)
(*Endot\[EAcute]rmica: El valor de Q es positivo. Absorbe calor*)
(*Exot\[EAcute]rmica: El valor de Q es negativo. Saca calor*)
(**)
(*Trabajo positivo: Cuando hay un globo y se infla, el globo empuja los alrededores. Ese trabajo que hace el sistema del globo a sus alrededores*)
(*Trabajo negativo: Se est\[AAcute] aplastando y los alrededores aplican un trabajo al sistema.*)
(**)
(*Calor: Transferencia de energ\[IAcute]a (interna) debido a la diferencia de T.*)
(**)
(*Energ\[IAcute]a interna de un sistema se asocia con sus \[AAcute]tomos y mol\[EAcute]culas.*)
(*Q = H. calor*)
(**)
(**)
(*Cada compuesto tiene diferente energ\[IAcute]a de formaci\[OAcute]n y eso es la entalp\[IAcute]a de formaci\[OAcute]n (Subscript[\[CapitalDelta]H, f])*)
(**)
(*Subscript[\[CapitalDelta]H, f] = Entalp\[IAcute]a de formaci\[OAcute]n*)
(*Subscript[\[CapitalDelta]H, R] = \[Sum]nSubscript[\[CapitalDelta]H, f productos] -  \[Sum]nSubscript[\[CapitalDelta]H, f reactivos]*)
(**)
(*aA + bB |-> cC + dD*)
(*Subscript[\[CapitalDelta]H, R] = (Subscript[cH, fC]+Subscript[dH, fD]) - (Subscript[aH, fA]+Subscript[bH, fB])*)
(**)
(**)
(*1 kcal = 4.84 J*)


(* ::Subtitle:: *)
(*1. NaOH(s) + HCl(l) |-> NaCl(solucion) + H2O(l)*)


(* ::Text:: *)
(*1. C\[AAcute]clulo entalap\[IAcute]as. El estado es importante.*)
(**)
(*NaOH(s) + HCl(l) |-> NaCl(solucion) + H2O(l)*)
(*Subscript[\[CapitalDelta]H, f]NaCl = -97.302*)
(*Subscript[\[CapitalDelta]H, f]NaOH = -101.99*)
(*Subscript[\[CapitalDelta]H, f]HCl = -40.020*)
(*Subscript[\[CapitalDelta]H, f]H20 = -68.3174*)


\!\(TraditionalForm\`
\*SubscriptBox[\(\[CapitalDelta]H\), \(R\)]\) := (-68.3174 - 97.302) -  (-101.99 - 40.020 )
\!\(TraditionalForm\`
\*SubscriptBox[\(\[CapitalDelta]H\), \(R\)]\) 


(* ::Subtitle:: *)
(*2. CaO(s) + H2O(l) |-> Ca(OH)2 (s)*)


(* ::Text:: *)
(*2. CaO(s) + H2O(l) |-> Ca(OH)2 (s)*)
(*Subscript[\[CapitalDelta]H, f]CaO = -151.9*)
(*Subscript[\[CapitalDelta]H, f]H2O = -68.3174*)
(*Subscript[\[CapitalDelta]H, f]Ca(OH)2 = -235.8*)
(**)
(*Subscript[\[CapitalDelta]H, R] = \[Sum]nSubscript[\[CapitalDelta]H, f productos] -  \[Sum]nSubscript[\[CapitalDelta]H, f reactivos]*)


Subscript[\[CapitalDelta]H, R] := -235.8 - (-151.9 - 68.3174)
Subscript[\[CapitalDelta]H, R]


(* ::Text:: *)
(*Como di\[OAcute] Q = -15.5826 kcal/mol es negativo es Exot\[EAcute]rmica.*)


(* ::Subtitle:: *)
(*Otra forma*)


(* ::Text:: *)
(*Subscript[\[CapitalDelta]H, R]  =  \[Sum]Energia enlace reactivos -  \[Sum]Energia enlace productos*)


(* ::Subtitle:: *)
(*2C6H14 + 19O2 |-> 12CO2 + 14H20 *)


(* ::Text:: *)
(*H-C = 411 kJ/mol * numero que hay ahi (14) * 2 (coeficiente)*)
(*C-C = 345.6 * 5 (que hay) * 2 (coeficiente)*)
(**)
(*O-O = 493.59 * 1 * 19*)
(**)
(*O-C = 798.9 * 2 * 12 *)
(**)
(*O-H = 458.8 * 2 * 14*)
(**)


\!\(TraditionalForm\`
\*SubscriptBox[\(\[CapitalDelta]H\), \(R\)]\) := (411*14*2+345.6*5*2+493.59*1*19)-(798.9*2*12+458.8*2*14)
Subscript[\[CapitalDelta]H, R]


(* ::Subtitle:: *)
(*2C6H14(l) + 19O2(g) |-> 12CO2 (g)+ 14H20 (l)*)


(* ::Text:: *)
(*Subscript[\[CapitalDelta]H, f]O2 = 0 KJ/mol*)
(*Subscript[\[CapitalDelta]H, f]CO2 = -393.509 KJ/mol*)
(*Subscript[\[CapitalDelta]H, f]H2O = -285.83  KJ/mol*)
(*Subscript[\[CapitalDelta]H, f]C6H4 = -167.2 KJ/mol*)
(**)
(*Subscript[\[CapitalDelta]H, R] = \[Sum]nSubscript[\[CapitalDelta]H, f productos] -  \[Sum]nSubscript[\[CapitalDelta]H, f reactivos]*)


\!\(TraditionalForm\`
\*SubscriptBox[\(\[CapitalDelta]H\), \(R\)]\) := (12 * -393.509 + 14 * -285.83 + 19 * 0)-(2*-167.2 )
Subscript[\[CapitalDelta]H, R]


(* ::Text:: *)
(*Experimento exot\[EAcute]rmico y endot\[EAcute]rmico. *)
