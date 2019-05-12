module AABB

open System.Numerics
open Helpers

type AABB =
    { min:Vector3; max:Vector3 }
    static member (+) (b0 : AABB, b1 : AABB) =
        { min=Vector3.Min(b0.min,b1.min); max=Vector3.Max(b0.max,b1.max) }

let contains b0 (b1 : AABB) = b0 = b0 + b1

let intersects_aabb { o=o; dir=dir } { min=b0; max=b1 } =
    let inv = Vector3.One / dir // TODO: precompute
    let mins = (b0 - o) * inv
    let maxs = (b1 - o) * inv

    let v_max (v3 : Vector3) = max v3.X (max v3.Y v3.Z)
    let v_min (v3 : Vector3) = min v3.X (min v3.Y v3.Z)

    let tmin = v_max (Vector3.Min(mins,maxs))
    let tmax = v_min (Vector3.Max(mins,maxs))

    tmax >= tmin && (tmax > 0.0f || tmin > 0.0f)

let contains_point { min=min; max=max } (point : Vector3) =
    min.X < point.X && point.X < max.X && min.Y < point.Y && point.Y < max.Y && min.Z < point.Z && point.Z < max.Z
