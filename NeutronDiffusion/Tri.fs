module Tri

open System.Numerics
open Helpers
open AABB

type Tri = { tri:Vector3*Vector3*Vector3; index:int }

let intersect_tri { o=o; dir=dir } { tri=(va,vb,vc); index=index } =
    let ab = vb - va
    let ac = vc - va
    let pvec = Vector3.Cross(dir, ac)
    let det = Vector3.Dot(ab, pvec)
    let invDet = 1.0f / det
    let tvec = o - va
    let u = Vector3.Dot(tvec, pvec) * invDet
    let qvec = Vector3.Cross(tvec, ab)
    let v = Vector3.Dot(dir, qvec) * invDet
    let t = Vector3.Dot(ac, qvec) * invDet
    if abs det < epsilon then
        None
    elif u < 0.0f || u > 1.0f then
        None
    elif v < 0.0f || u + v > 1.0f then
        None
    elif t < 0.0f then
        None
    else
        Some (t,index)
    
let build_aabb_tri { tri=(va,vb,vc) } =
    let epsilon_vec = Vector3.One * epsilon;
    { min=Vector3.Min(va,Vector3.Min(vb,vc)) - epsilon_vec; max=Vector3.Max(va,Vector3.Max(vb,vc)) + epsilon_vec }

let get_centroid { tri=(a,b,c) } = (a + b + c) / 3.0f

let intersect_tris tris ray =
    match Array.choose (intersect_tri ray) tris with
    | [||] -> None
    | arr -> Array.minBy fst arr |> Some

let get_aabb tris =
    let aabbs = Array.map build_aabb_tri tris
    (Array.reduce (+) aabbs, aabbs)
