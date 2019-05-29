open System
open System.Numerics
open Helpers
open Scene
open CrossSections
open FSharp.Data.UnitSystems.SI.UnitSymbols // lets me see unit symbols in editor

let rnd = System.Random ()

let randomDir () =
    // TODO: is this generator biased?
    let theta = rnd.NextDouble() * 2.0 * Math.PI |> float32
    let phi = rnd.NextDouble() * 2.0 - 1.0 |> acos |> float32
    Vector3 (cos theta * sin phi, sin theta * sin phi, cos phi)

let pathLength (sigma : float</m>) () : float<m> =
    (-1.0 / sigma) * log (rnd.NextDouble())

[<EntryPoint>]
let main argv =
    let neutrons = int argv.[0]

    // TODO: figure out which cross section I need
    let crossSections = readCrossSections "2631"

    // TODO: change energy as I collide with things
    let energy = 2.45e6<eV> // is this the starting energy?

    let scene = buildScene "test_scene.json" "week7_3"
    for _ in [1..neutrons] do
        // assuming that I start outside of all objects in the scene
        let mutable pastDir = randomDir()
        let mutable intersection = scene.intersectScene { o=Vector3.Zero; dir=pastDir }

        while intersection <> None do
            intersection <- Option.bind (fun { point=point; distance=distance; dest=dest } ->
                match dest with
                | None -> // we are outside of all objects -> no need to look at cross sections
                    scene.intersectScene { o=point; dir=pastDir }
                | Some (material,obj) ->
                    let totalMicro = (crossSections.getTotal energy).Value
                    let totalMacro = totalMicro * InvM3PerAmg * material.number_density * M2PerBarn
                    let distanceTraveled = pathLength totalMacro ()
                    if distanceTraveled < distance then // we are doing something!
                        let elasticMicro = (crossSections.getElastic energy).Value
                        let elasticMacro = elasticMicro * InvM3PerAmg * material.number_density * M2PerBarn
                        if (elasticMacro / elasticMacro) > rnd.NextDouble() then // elastic scattering
                            pastDir <- randomDir() // TODO: actually elastic scatter
                            scene.intersectObj { o=point; dir=pastDir } obj
                        else
                            None
                    else // just keep going
                        scene.intersectObj { o=point; dir=pastDir } obj
            ) intersection
    scene.write()
    0