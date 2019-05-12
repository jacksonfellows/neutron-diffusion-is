// Learn more about F# at http://fsharp.org

open System
// open FsAlg.Generic

// type Ray = { Origin: Vector<float>; Dir: Vector<float> }

let rnd = System.Random ()

// calculate a rolling mean and variance of a sequence
let calcMeanVariance xs =
    let step (m, s, k) x =
        let m' = m + (x - m) / k
        let s' = s + (x - m) * (x - m')
        m', s', k + 1.0
    let m, s, n2 = Seq.fold step (0.0, 0.0, 1.0) xs
    m, s / (n2 - 2.0)

let pathLength sigma () =
    (-1.0 / sigma) * log (rnd.NextDouble())

let sim sigma samples =
    // I found that Array is faster than List is faster than Seq
    Array.init samples (fun _ -> pathLength sigma ()) |> calcMeanVariance

let week2 () =
    printfn "Macroscopic Cross Section,Inverse Cross Section,Mean Free Path,Variance,Standard Deviation,Coefficient of Variation"
    let samples = 1000
    for pwr in -5.0..5.0 do
        for per in 0.1..0.1..0.9 do
            let sigma = per * 10.0**pwr
            let mean,variance = sim sigma samples
            let stdDev = sqrt variance
            let cv = stdDev / mean
            printfn "%f,%f,%f,%f,%f,%f" sigma (1.0/sigma) mean variance stdDev cv

open System.IO
open QuantumConcepts.Formats
open JeremyAnsel.Media

let stlVertToObj (stlVert : StereoLithography.Vertex) : WavefrontObj.ObjVertex =
    WavefrontObj.ObjVertex (stlVert.X, stlVert.Y, stlVert.Z)

let stlFileToObj (stl : StereoLithography.STLDocument) : WavefrontObj.ObjFile =
    let obj = WavefrontObj.ObjFile ()
    // TODO: avoid exporting duplicate vertices
    Seq.iteri (fun fN (facet : StereoLithography.Facet) ->
        let face = WavefrontObj.ObjFace ()
        Seq.iteri (fun vOff (vertex : StereoLithography.Vertex) ->
            stlVertToObj vertex |> obj.Vertices.Add
            WavefrontObj.ObjTriplet (fN * 3 + vOff + 1,0,0) |> face.Vertices.Add
        ) facet.Vertices
        obj.Faces.Add face
    ) stl.Facets
    obj

let addTextureToModel (model : WavefrontObj.ObjFile) matLibName textureFileName matName =
    model.MaterialLibraries.Add matLibName
    let matMap = WavefrontObj.ObjMaterialMap textureFileName
    let mat = WavefrontObj.ObjMaterial matName
    mat.DiffuseMap <- matMap
    let matFile = WavefrontObj.ObjMaterialFile ()
    matFile.Materials.Add mat
    for face in model.Faces do
        face.MaterialName <- matName
    matFile

let week3 () =
    let model = File.OpenRead "baseparaffinblock.stl"
              |> StereoLithography.STLDocument.Read
              |> stlFileToObj

    // TODO: can they not be 1 or 0?
    let texCoords = [(0.01f,0.01f);(0.01f,0.99f);(0.99f,0.01f);(0.99f,0.99f)]
    for x,y in texCoords do
        WavefrontObj.ObjVector3 (x,y,0.0f) |> model.TextureVertices.Add

    for face in model.Faces do
        let offset = if model.Vertices.[face.Vertices.[0].Vertex-1].Position.Y < 10.0f then 1 else 2
        for tI in [0..2] do // assume that each face has 3 vertices
            let mutable triplet = face.Vertices.[tI]
            triplet.Texture <- tI + offset
            face.Vertices.[tI] <- triplet

    let matLibName = "output.mtl"
    let textureFileName = "demoTexture2.png"
    let matName = "texturedMaterial"
    let matFile = addTextureToModel model matLibName textureFileName matName

    File.OpenWrite matLibName |> matFile.WriteTo
    File.OpenWrite "output.obj" |> model.WriteTo

open System.Numerics
open System.Drawing

type Texture =
    { bitmap:Bitmap
      width:int
      height:int
      getCoords:int -> Vector2*Vector2*Vector2 }
    interface IDisposable with
        member this.Dispose () = this.bitmap.Dispose ()

let setTexCoord (texture : Texture) (uv : Vector2) color =
    texture.bitmap.SetPixel (uv.X*(float32 (texture.width-1)) |> int, (1.0f - uv.Y)*(float32 (texture.height-1)) |> int, color)

let addTexture (model : WavefrontObj.ObjFile) (rWidth,rHeight) cols : Texture =
    let numTris = model.Faces.Count
    let numRects = numTris / 2
    // TODO: how should I choose to setup my grid?
    let rows = float numRects / float cols |> ceil |> int
    let texWidth = rWidth * cols
    let texHeight = rHeight * rows

    let texCoords = [|
        for row in 0..rows do
            for col in 0..cols do
                let u = float32 (col*rWidth) / float32 texWidth
                let v = 1.0f - float32 (row*rHeight) / float32 texHeight
                yield Vector2(u,v)
    |]

    let getTexCoordIndices triI =
        let (rectI,o) = Math.DivRem (triI,2)
        let (row,col) = Math.DivRem (rectI,cols)
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

    let getTexCoords triI =
        let (ia,ib,ic) = getTexCoordIndices triI
        (texCoords.[ia], texCoords.[ib], texCoords.[ic])

    let texture : Bitmap = new Bitmap(texWidth, texHeight)

    printfn "Num tris: %d" numTris
    printfn "Rows: %d" rows
    printfn "Cols: %d" cols

    Seq.iter (fun (texCoord : Vector2) ->
        model.TextureVertices.Add(WavefrontObj.ObjVector3(texCoord.X,texCoord.Y))
    ) texCoords

    // set up the texture coords
    Seq.iteri (fun triI (face : WavefrontObj.ObjFace) ->
        let (ia,ib,ic) = getTexCoordIndices triI
        let setTexture vi ti =
            let mutable triplet = face.Vertices.[vi]
            triplet.Texture <- ti+1
            face.Vertices.[vi] <- triplet
        setTexture 0 ia
        setTexture 1 ib
        setTexture 2 ic
    ) model.Faces

    { bitmap=texture; width=texWidth; height=texHeight; getCoords=getTexCoords }

let getUvCoords ((va,vb,vc) : Vector3*Vector3*Vector3) ((ta,tb,tc) : Vector2*Vector2*Vector2) (point : Vector3) : Vector2 =
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
              |> stlFileToObj

    let vertices = [| for vertex in model.Vertices do yield Vector3(vertex.Position.X,vertex.Position.Y,vertex.Position.Z) |]
    let textureFileName = "texture.bmp"
    let matLibName = "week4.mtl"
    let matName = "texturedMaterial"

    using (addTexture model (100,100) 20) (fun texture ->
        // mark the center of each face
        Seq.iteri (fun triI (face : WavefrontObj.ObjFace) ->
            let (ta,tb,tc) as texCoords = texture.getCoords triI
            let [va;vb;vc] = List.init 3 (fun i -> vertices.[face.Vertices.[i].Vertex-1])
            let tri = (va,vb,vc)

            let center = (va+vb+vc) / 3.0f
            setTexCoord texture (getUvCoords tri texCoords center) Color.White

            setTexCoord texture ta Color.Red
            setTexCoord texture tb Color.Red
            setTexCoord texture tc Color.Red
        ) model.Faces

        // In Haskell, I would curry and flip texture.bitmap.Save
        File.OpenWrite textureFileName |> fun stream -> texture.bitmap.Save (stream,Imaging.ImageFormat.Bmp)
    )

    let matFile = addTextureToModel model matLibName textureFileName matName

    File.OpenWrite matLibName |> matFile.WriteTo
    File.OpenWrite "week4.obj" |> model.WriteTo

open Helpers
open AABB
open Tri
open BVH

let buildTri (vertices : Vector3 array) (face : WavefrontObj.ObjFace) index =
    let [va;vb;vc] = List.init 3 (fun i -> vertices.[face.Vertices.[i].Vertex-1])
    let tri = (va,vb,vc)
    // TODO: I may need to add/subtract eps from the AABB
    { tri=tri; index=index }

let randomDir () =
    // TODO: is this generator biased?
    let theta = rnd.NextDouble() * 2.0 * Math.PI |> float32
    let phi = rnd.NextDouble() * 2.0 - 1.0 |> acos |> float32
    Vector3 (cos theta * sin phi, sin theta * sin phi, cos phi)

// TESTS:

let rec nodesContainChildren bvh =
    match bvh with
    | BVHNode (aabb,children) ->
        [||] = Array.filter (getBvhAabb >> contains aabb >> not) children &&
            [||] = Array.filter (nodesContainChildren >> not) children
    | BVHLeaf (aabb,tris) ->
        [] = List.filter (buildAabbTri >> contains aabb >> not) tris

let rec getAllTris bvh =
    match bvh with
    | BVHNode (_,children) -> Array.fold (fun tris child -> getAllTris child @ tris) [] children
    | BVHLeaf (_,tris) -> tris

[<EntryPoint>]
let main argv =
    // let model = File.OpenRead "baseparaffinblock.stl"
    let model = File.OpenRead "sphere.stl"
              |> StereoLithography.STLDocument.Read
              |> stlFileToObj

    let textureFileName = "textureWeek5.bmp"
    let matLibName = "week5.mtl"
    let matName = "texturedMaterial"

    let vertices = [| for vertex in model.Vertices do yield Vector3(vertex.Position.X,vertex.Position.Y,vertex.Position.Z) |]
    let tris = [| for face,i in Seq.zip model.Faces {0..model.Faces.Count} do yield buildTri vertices face i |]

    let bvh = buildBvh tris

    // printfn "%A" bvh
    // printfn "nodes contain children: %b" (nodesContainChildren bvh)
    // let allTris = getAllTris bvh
    // printfn "# tris: %d, # bvhTris: %d" tris.Length allTris.Length
    // let sorter { index=i } = i
    // List.sortBy sorter (Array.toList tris) = List.sortBy sorter allTris
    //     |> printfn "tris are the same: %b"

    using (addTexture model (10,10) 20) (fun texture ->
        for _ in [1..100000] do
            let o = Vector3.Zero + Vector3.UnitZ * 20.0f
            let dir = randomDir ()
            let ray = { o=o; dir=dir }
            // let trisInt = intersectTris tris ray
            let bvhInt = intersectBvh ray bvh
            // if trisInt <> bvhInt then
                // printfn "%A <> %A with ray %A" trisInt bvhInt ray
            // match intersectTris tris ray with
            // match intersectBvh ray bvh with
            match bvhInt with
            | Some (dist,i) ->
                let point = o + dir * dist
                let uv = getUvCoords tris.[i].tri (texture.getCoords i) point
                setTexCoord texture uv Color.Blue
            | None -> ()
        File.OpenWrite textureFileName |> fun stream -> texture.bitmap.Save (stream,Imaging.ImageFormat.Bmp)
    )

    let matFile = addTextureToModel model matLibName textureFileName matName

    File.OpenWrite matLibName |> matFile.WriteTo
    File.OpenWrite "week5.obj" |> model.WriteTo
    0 // return an integer exit code
