// Learn more about F# at http://fsharp.org

// open System
// open FsAlg.Generic

// type Ray = { Origin: Vector<float>; Dir: Vector<float> }
// 
// let randomDir() =
//     let theta = rnd.NextDouble() * 2.0 * Math.PI
//     let phi = rnd.NextDouble() * 2.0 - 1.0 |> acos
//     vector [| cos theta * sin phi; sin theta * sin phi; cos phi |]

let rnd = System.Random()

// calculate a rolling mean and variance of a sequence
let calc_mean_variance xs =
    let step (m, s, k) x =
        let m' = m + (x - m) / k
        let s' = s + (x - m) * (x - m')
        m', s', k + 1.0
    let m, s, n2 = Seq.fold step (0.0, 0.0, 1.0) xs
    m, s / (n2 - 2.0)

let path_length sigma () =
    (-1.0 / sigma) * log (rnd.NextDouble())

let sim sigma samples =
    // I found that Array is faster than List is faster than Seq
    Array.init samples (fun _ -> path_length sigma ()) |> calc_mean_variance

let week2 () =
    printfn "Macroscopic Cross Section,Inverse Cross Section,Mean Free Path,Variance,Standard Deviation,Coefficient of Variation"
    let samples = 1000
    for pwr in -5.0..5.0 do
        for per in 0.1..0.1..0.9 do
            let sigma = per * 10.0**pwr
            let mean,variance = sim sigma samples
            let std_dev = sqrt variance
            let cv = std_dev / mean
            printfn "%f,%f,%f,%f,%f,%f" sigma (1.0/sigma) mean variance std_dev cv

open System.IO
open QuantumConcepts.Formats
open JeremyAnsel.Media

let stl_vert_to_obj (stl_vert : StereoLithography.Vertex) : WavefrontObj.ObjVertex =
    WavefrontObj.ObjVertex (stl_vert.X, stl_vert.Y, stl_vert.Z)

let stl_file_to_obj (stl : StereoLithography.STLDocument) : WavefrontObj.ObjFile =
    let obj = WavefrontObj.ObjFile ()
    // TODO: avoid exporting duplicate vertices
    Seq.iteri (fun f_n (facet : StereoLithography.Facet) ->
        let face = WavefrontObj.ObjFace ()
        Seq.iteri (fun v_off (vertex : StereoLithography.Vertex) ->
            stl_vert_to_obj vertex |> obj.Vertices.Add
            WavefrontObj.ObjTriplet (f_n * 3 + v_off + 1,0,0) |> face.Vertices.Add
        ) facet.Vertices
        obj.Faces.Add face
    ) stl.Facets
    obj

[<EntryPoint>]
let main argv =
    let model = File.OpenRead "baseparaffinblock.stl"
              |> StereoLithography.STLDocument.Read
              |> stl_file_to_obj

    let mat_lib_name = "output.mtl"
    model.MaterialLibraries.Add mat_lib_name

    let texture_file_name = "demo_texture2.png"
    let mat_map = WavefrontObj.ObjMaterialMap texture_file_name

    let mat_name = "textured_material"
    let mat = WavefrontObj.ObjMaterial mat_name
    mat.DiffuseMap <- mat_map

    let mat_file = WavefrontObj.ObjMaterialFile ()
    mat_file.Materials.Add mat

    // TODO: can they not be 1 or 0?
    let tex_coords = [(0.01f,0.01f);(0.01f,0.99f);(0.99f,0.01f);(0.99f,0.99f)]
    for x,y in tex_coords do
        WavefrontObj.ObjVector3 (x,y,0.0f) |> model.TextureVertices.Add

    for face in model.Faces do
        face.MaterialName <- mat_name
        let offset = if model.Vertices.[face.Vertices.[0].Vertex].Position.Y < 10.0f then 1 else 2
        for t_i in [0..2] do // assume that each face has 3 vertices
            let mutable triplet = face.Vertices.[t_i]
            triplet.Texture <- t_i + offset
            face.Vertices.[t_i] <- triplet

    File.OpenWrite mat_lib_name |> mat_file.WriteTo
    File.OpenWrite "output.obj" |> model.WriteTo

    0 // return an integer exit code
