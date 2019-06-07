module Helpers

open System.Numerics
open FSharp.Data.UnitSystems.SI.UnitSymbols

type Ray = { o:Vector3; dir:Vector3 }
let epsilon = 1e-3f
let epsilonVec = Vector3.One * epsilon;

[<Measure>] type eV // electron volts
[<Measure>] type barn // barns
[<Measure>] type amg // Amagats

let JoulesPerEV = 1.6021766208E-19<J/eV>
let M2PerBarn = 1e-28<m^2/barn>
let InvM3PerAmg = 2.6867811e25<m^-3/amg>
