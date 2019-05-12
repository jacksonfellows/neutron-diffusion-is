// Learn more about F# at http://fsharp.org

open System
// open FsAlg.Generic

// type Ray = { Origin: Vector<float>; Dir: Vector<float> }

let rnd = System.Random ()

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

let add_texture_to_model (model : WavefrontObj.ObjFile) mat_lib_name texture_file_name mat_name =
    model.MaterialLibraries.Add mat_lib_name
    let mat_map = WavefrontObj.ObjMaterialMap texture_file_name
    let mat = WavefrontObj.ObjMaterial mat_name
    mat.DiffuseMap <- mat_map
    let mat_file = WavefrontObj.ObjMaterialFile ()
    mat_file.Materials.Add mat
    for face in model.Faces do
        face.MaterialName <- mat_name
    mat_file

let week3 () =
    let model = File.OpenRead "baseparaffinblock.stl"
              |> StereoLithography.STLDocument.Read
              |> stl_file_to_obj

    // TODO: can they not be 1 or 0?
    let tex_coords = [(0.01f,0.01f);(0.01f,0.99f);(0.99f,0.01f);(0.99f,0.99f)]
    for x,y in tex_coords do
        WavefrontObj.ObjVector3 (x,y,0.0f) |> model.TextureVertices.Add

    for face in model.Faces do
        let offset = if model.Vertices.[face.Vertices.[0].Vertex-1].Position.Y < 10.0f then 1 else 2
        for t_i in [0..2] do // assume that each face has 3 vertices
            let mutable triplet = face.Vertices.[t_i]
            triplet.Texture <- t_i + offset
            face.Vertices.[t_i] <- triplet

    let mat_lib_name = "output.mtl"
    let texture_file_name = "demo_texture2.png"
    let mat_name = "textured_material"
    let mat_file = add_texture_to_model model mat_lib_name texture_file_name mat_name

    File.OpenWrite mat_lib_name |> mat_file.WriteTo
    File.OpenWrite "output.obj" |> model.WriteTo

open System.Numerics
open System.Drawing

type Texture =
    { bitmap:Bitmap
      width:int
      height:int
      get_coords:int -> Vector2*Vector2*Vector2 }
    interface IDisposable with
        member this.Dispose () = this.bitmap.Dispose ()

let set_tex_coord (texture : Texture) (uv : Vector2) color =
    texture.bitmap.SetPixel (uv.X*(float32 (texture.width-1)) |> int, (1.0f - uv.Y)*(float32 (texture.height-1)) |> int, color)

let add_texture (model : WavefrontObj.ObjFile) (r_width,r_height) cols : Texture =
    let num_tris = model.Faces.Count
    let num_rects = num_tris / 2
    // TODO: how should I choose to setup my grid?
    let rows = float num_rects / float cols |> ceil |> int
    let tex_width = r_width * cols
    let tex_height = r_height * rows

    let tex_coords = [|
        for row in 0..rows do
            for col in 0..cols do
                let u = float32 (col*r_width) / float32 tex_width
                let v = 1.0f - float32 (row*r_height) / float32 tex_height
                yield Vector2(u,v)
    |]

    let get_tex_coord_indices tri_i =
        let (rect_i,o) = Math.DivRem (tri_i,2)
        let (row,col) = Math.DivRem (rect_i,cols)
        let at (r,c) = (cols+1) * r + c
        let ia = at (row,col)
        let ib = if o = 0 then at (row+1,col) else at (row+1,col+1)
        let ic = if o = 0 then at (row+1,col+1) else at (row,col+1)
        // TODO: remove checks if they are slowing things down
        if ia = ib || ia = ic || ib = ic then
            failwith "texture indices are the same"
        if ia < 0 || ib < 0 || ic < 0 then
            failwith "texture indices are negative"
        (ia,ib,ic)

    let get_tex_coords tri_i =
        let (ia,ib,ic) = get_tex_coord_indices tri_i
        (tex_coords.[ia], tex_coords.[ib], tex_coords.[ic])

    let texture : Bitmap = new Bitmap(tex_width, tex_height)

    printfn "Num tris: %d" num_tris
    printfn "Rows: %d" rows
    printfn "Cols: %d" cols

    Seq.iter (fun (tex_coord : Vector2) ->
        model.TextureVertices.Add(WavefrontObj.ObjVector3(tex_coord.X,tex_coord.Y))
    ) tex_coords

    // set up the texture coords
    Seq.iteri (fun tri_i (face : WavefrontObj.ObjFace) ->
        let (ia,ib,ic) = get_tex_coord_indices tri_i
        let set_texture vi ti =
            let mutable triplet = face.Vertices.[vi]
            triplet.Texture <- ti+1
            face.Vertices.[vi] <- triplet
        set_texture 0 ia
        set_texture 1 ib
        set_texture 2 ic
    ) model.Faces

    { bitmap=texture; width=tex_width; height=tex_height; get_coords=get_tex_coords }

let get_uv_coords ((va,vb,vc) : Vector3*Vector3*Vector3) ((ta,tb,tc) : Vector2*Vector2*Vector2) (point : Vector3) : Vector2 =
    if va = vb || va = vc || vb = vc then
        failwith "triangle vertices are the same!"

    // vectors from point to vertices
    let fa = va - point
    let fb = vb - point
    let fc = vc - point
    // areas and factors
    let a = Vector3.Cross(va-vb, va-vc).Length() // main area
    let a1 = Vector3.Cross(fb, fc).Length() / a
    let a2 = Vector3.Cross(fc, fa).Length() / a
    let a3 = Vector3.Cross(fa, fb).Length() / a
    // get final uv coord
    ta * a1 + tb * a2 + tc * a3

let week4 () =
    let model = File.OpenRead "baseparaffinblock.stl"
              |> StereoLithography.STLDocument.Read
              |> stl_file_to_obj

    let vertices = [| for vertex in model.Vertices do yield Vector3(vertex.Position.X,vertex.Position.Y,vertex.Position.Z) |]
    let texture_file_name = "texture.bmp"
    let mat_lib_name = "week4.mtl"
    let mat_name = "textured_material"

    using (add_texture model (100,100) 20) (fun texture ->
        // mark the center of each face
        Seq.iteri (fun tri_i (face : WavefrontObj.ObjFace) ->
            let (ta,tb,tc) as tex_coords = texture.get_coords tri_i
            let [va;vb;vc] = List.init 3 (fun i -> vertices.[face.Vertices.[i].Vertex-1])
            let tri = (va,vb,vc)

            let center = (va+vb+vc) / 3.0f
            set_tex_coord texture (get_uv_coords tri tex_coords center) Color.White

            set_tex_coord texture ta Color.Red
            set_tex_coord texture tb Color.Red
            set_tex_coord texture tc Color.Red
        ) model.Faces

        // In Haskell, I would curry and flip texture.bitmap.Save
        File.OpenWrite texture_file_name |> fun stream -> texture.bitmap.Save (stream,Imaging.ImageFormat.Bmp)
    )

    let mat_file = add_texture_to_model model mat_lib_name texture_file_name mat_name

    File.OpenWrite mat_lib_name |> mat_file.WriteTo
    File.OpenWrite "week4.obj" |> model.WriteTo

open Helpers
open AABB
open Tri
open BVH

let build_tri (vertices : Vector3 array) (face : WavefrontObj.ObjFace) index =
    let [va;vb;vc] = List.init 3 (fun i -> vertices.[face.Vertices.[i].Vertex-1])
    let tri = (va,vb,vc)
    // TODO: I may need to add/subtract eps from the AABB
    { tri=tri; index=index }

let random_dir () =
    // TODO: is this generator biased?
    let theta = rnd.NextDouble() * 2.0 * Math.PI |> float32
    let phi = rnd.NextDouble() * 2.0 - 1.0 |> acos |> float32
    Vector3 (cos theta * sin phi, sin theta * sin phi, cos phi)

// TESTS:

let rec nodes_contain_children bvh =
    match bvh with
    | BVHNode (aabb,children) ->
        [||] = Array.filter (get_bvh_aabb >> contains aabb >> not) children &&
            [||] = Array.filter (nodes_contain_children >> not) children
    | BVHLeaf (aabb,tris) ->
        [] = List.filter (build_aabb_tri >> contains aabb >> not) tris

let rec get_all_tris bvh =
    match bvh with
    | BVHNode (_,children) -> Array.fold (fun tris child -> get_all_tris child @ tris) [] children
    | BVHLeaf (_,tris) -> tris

[<EntryPoint>]
let main argv =
    // let model = File.OpenRead "baseparaffinblock.stl"
    let model = File.OpenRead "sphere.stl"
              |> StereoLithography.STLDocument.Read
              |> stl_file_to_obj

    let texture_file_name = "texture_week5.bmp"
    let mat_lib_name = "week5.mtl"
    let mat_name = "textured_material"

    let vertices = [| for vertex in model.Vertices do yield Vector3(vertex.Position.X,vertex.Position.Y,vertex.Position.Z) |]
    let tris = [| for face,i in Seq.zip model.Faces {0..model.Faces.Count} do yield build_tri vertices face i |]

    let bvh = build_bvh tris

    // printfn "%A" bvh
    // printfn "nodes contain children: %b" (nodes_contain_children bvh)
    // let all_tris = get_all_tris bvh
    // printfn "# tris: %d, # bvh_tris: %d" tris.Length all_tris.Length
    // let sorter { index=i } = i
    // List.sortBy sorter (Array.toList tris) = List.sortBy sorter all_tris
    //     |> printfn "tris are the same: %b"

    using (add_texture model (10,10) 20) (fun texture ->
        for _ in [1..100000] do
            let o = Vector3.Zero + Vector3.UnitZ * 20.0f
            let dir = random_dir ()
            let ray = { o=o; dir=dir }
            // let tris_int = intersect_tris tris ray
            let bvh_int = intersect_bvh ray bvh
            // if tris_int <> bvh_int then
                // printfn "%A <> %A with ray %A" tris_int bvh_int ray
            // match intersect_tris tris ray with
            // match intersect_bvh ray bvh with
            match bvh_int with
            | Some (dist,i) ->
                let point = o + dir * dist
                let uv = get_uv_coords tris.[i].tri (texture.get_coords i) point
                set_tex_coord texture uv Color.Blue
            | None -> ()
        File.OpenWrite texture_file_name |> fun stream -> texture.bitmap.Save (stream,Imaging.ImageFormat.Bmp)
    )

    let mat_file = add_texture_to_model model mat_lib_name texture_file_name mat_name

    File.OpenWrite mat_lib_name |> mat_file.WriteTo
    File.OpenWrite "week5.obj" |> model.WriteTo
    0 // return an integer exit code
