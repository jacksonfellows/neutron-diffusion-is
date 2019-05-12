module BVH

open System.Numerics
open AABB
open Tri

type BVHStructure =
    | BVHNode of AABB * BVHStructure array
    | BVHLeaf of AABB * Tri list // TODO: should this be an int array?

type Octree =
    | OctreeDummy of AABB
    | OctreeNode of AABB * Octree array
    | OctreeLeaf of AABB * int list

let get_oct_aabb octree =
    match octree with
    | OctreeDummy aabb -> aabb
    | OctreeNode (aabb,_) -> aabb
    | OctreeLeaf (aabb,_) -> aabb

let get_bvh_aabb bvh =
    match bvh with
    | BVHNode (aabb,_) -> aabb
    | BVHLeaf (aabb,_) -> aabb

let oct_split_aabb { min=min; max=max} =
    let center = (min + max) / 2.0f
    [|
        { min=Vector3(min.X,min.Y,min.Z); max=Vector3(center.X,center.Y,center.Z) }
        { min=Vector3(center.X,min.Y,min.Z); max=Vector3(max.X,center.Y,center.Z) }
        { min=Vector3(min.X,center.Y,min.Z); max=Vector3(center.X,max.Y,center.Z) }
        { min=Vector3(center.X,center.Y,min.Z); max=Vector3(max.X,max.Y,center.Z) }

        { min=Vector3(min.X,min.Y,center.Z); max=Vector3(center.X,center.Y,max.Z) }
        { min=Vector3(center.X,min.Y,center.Z); max=Vector3(max.X,center.Y,max.Z) }
        { min=Vector3(min.X,center.Y,center.Z); max=Vector3(center.X,max.Y,max.Z) }
        { min=Vector3(center.X,center.Y,center.Z); max=Vector3(max.X,max.Y,max.Z) }
    |]

let create_empty_tree aabb =
    let octs = oct_split_aabb aabb |> Array.map OctreeDummy
    OctreeNode (aabb,octs)

let is_dummy octree =
    match octree with
    | OctreeDummy _ -> true
    | _ -> false

let build_bvh tris =
    let centroids = Array.map get_centroid tris
    let max_depth = 16 // TODO: fine-tune

    let insert octree index =
        let rec insert' octree index depth =
            match octree with
            | OctreeNode (aabb,children) ->
                let new_children = Array.map (fun child -> if contains_point (get_oct_aabb child) centroids.[index] then insert' child index (depth+1) else child) children
                OctreeNode (aabb,new_children)
            | OctreeLeaf (aabb,indices) ->
                let new_indices = index :: indices
                if depth < max_depth then
                    List.fold (fun tree i -> insert' tree i (depth+1)) (create_empty_tree aabb) new_indices
                else OctreeLeaf (aabb,new_indices)
            | OctreeDummy aabb -> OctreeLeaf (aabb,[index])
        insert' octree index 0

    let (aabb,aabbs) = get_aabb tris
    let octree = List.fold insert (create_empty_tree aabb) [0..tris.Length - 1]

    let rec build_bvh_from_octree octree =
        match octree with
        | OctreeNode (_,children) ->
            let bvhs = Array.filter (is_dummy >> not) children |> Array.map build_bvh_from_octree
            let new_aabb = Array.map get_bvh_aabb bvhs |> Array.reduce (+)
            BVHNode (new_aabb,bvhs)
        | OctreeLeaf (_,indices) ->
            let new_aabb = List.map (fun i -> aabbs.[i]) indices |> List.reduce (+)
            let new_tris = List.map (fun i -> tris.[i]) indices
            BVHLeaf (new_aabb,new_tris)

    build_bvh_from_octree octree

let rec intersect_bvh ray bvh =
    match bvh with
    | BVHNode (aabb,children) ->
        if intersects_aabb ray aabb then
            match Array.choose (intersect_bvh ray) children with
            | [||] -> None
            | arr -> Array.minBy fst arr |> Some
        else None
    | BVHLeaf (aabb,tris) ->
        if intersects_aabb ray aabb then
            match List.choose (intersect_tri ray) tris with
            | [] -> None
            | lst -> List.minBy fst lst |> Some
        else None
