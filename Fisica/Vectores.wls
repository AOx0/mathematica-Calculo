#!/usr/bin/env wolframscript
(* ::Package:: *)

Magnitud := x |-> (
Sqrt[(x[[1]])^2 + (x[[2]])^2+(x[[3]])^2]
);
ProductoPunto := {x, y }|-> (
(x[[1]]y[[1]] + x[[2]] y[[2]] + x[[3]] y[[3]])
);
Unitario := x |-> (
{x[[1]]/Magnitud[x], x[[2]]/Magnitud[x], x[[3]]/Magnitud[x]}
);
Angulo := {x, y} |-> (
N@ArcCos[ProductoPunto[x, y]/(Magnitud[x] Magnitud[y])]/Degree
);


Magnitud[{-3/2, 1/2, 4/3}]


Unitario[{-3, 2,2}]


Angulo[{1,3,5/2},{-1,2,1/2}]


ProductoPunto[{1,3,5/2},{-1,2,1/2}]
