module Helpers

open System.Numerics

type Ray = { o:Vector3; dir:Vector3 }
let epsilon = 1e-5f
