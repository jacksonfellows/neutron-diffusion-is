open System
open System.Numerics
open Helpers
open Scene
open CrossSections
open FSharp.Data.UnitSystems.SI.UnitSymbols // lets me see unit symbols in editor
open System.Collections.Generic
open XPlot.Plotly
open MathNet.Numerics.Distributions

let rnd = System.Random ()
let normalRnd = Normal ()

let randomDir () =
    // TODO: is this generator biased?
    let theta = rnd.NextDouble() * 2.0 * Math.PI |> float32
    let phi = rnd.NextDouble() * 2.0 - 1.0 |> acos |> float32
    Vector3 (cos theta * sin phi, sin theta * sin phi, cos phi)

let pathLength (sigma : float</m>) () : float<m> =
    (-1.0 / sigma) * log (rnd.NextDouble())

let boltzmann = 8.61733333353e-5<eV/K>
let roomTemp = 293.0<K>
let neutronMass = 1.008664f // u
let protonMass = 1.007276f // u

let elasticScatter (neutronDir : Vector3) (neutronEnergy : float<eV>) =
    let protonEnergy = normalRnd.Sample() * (boltzmann * roomTemp / 2.0)
    let protonDir = randomDir()

    let neutron = (float32 neutronEnergy) * neutronDir
    let proton = (float32 protonEnergy) * protonDir

    let referenceFrame = (neutronMass * neutron + proton * protonMass) * (1.0f / (neutronMass + protonMass))
    let neutronRef = neutron - referenceFrame
    let scatteredRef = neutronRef.Length() * randomDir()
    let scattered = scatteredRef + referenceFrame

    Vector3.Normalize(scattered), 1.0<eV> * float (scattered.Length())

[<EntryPoint>]
let main argv =
    let neutrons = int argv.[0]

    let scene = buildScene "test_scene.json" "week8"

    let crossSectionsMap : Map<string,CrossSection> =
        Seq.map (fun material -> material.mat) scene.materials
        |> Seq.distinct
        |> Seq.map (fun mat -> mat,readCrossSections mat)
        |> Map.ofSeq

    // printfn "cross sections map: %A" crossSectionsMap

    // TODO: change energy as I collide with things
    let startingEnergy = 2.45e6<eV> // is this the starting energy?

    let escaped = new List<float>()
    let absorbed = new List<float>()

    for _ in [1..neutrons] do
        // assuming that I start outside of all objects in the scene
        let mutable pastDir = randomDir()
        let mutable intersection = scene.intersectScene { o=Vector3.Zero; dir=pastDir }
        let mutable justAbsorbed = false
        let mutable energy = startingEnergy

        while intersection <> None do
            intersection <- Option.bind (fun { point=point; distance=distance; dest=dest } ->
                match dest with
                | None -> // we are outside of all objects -> no need to look at cross sections
                    scene.intersectScene { o=point; dir=pastDir }
                | Some (material,obj) ->
                    let crossSections = (crossSectionsMap.TryFind material.mat).Value
                    let totalMicro = (crossSections.getTotal energy).Value
                    let totalMacro = totalMicro * InvM3PerAmg * material.number_density * M2PerBarn
                    let distanceTraveled = pathLength totalMacro ()
                    if distanceTraveled < distance then // we are doing something!
                        let elasticMicro = (crossSections.getElastic energy).Value
                        let elasticMacro = elasticMicro * InvM3PerAmg * material.number_density * M2PerBarn
                        if (elasticMacro / totalMacro) > rnd.NextDouble() then // elastic scattering
                            let newDir,newEnergy = elasticScatter pastDir energy
                            pastDir <- newDir
                            energy <- newEnergy
                            scene.intersectObj { o=point; dir=pastDir } obj
                        else // absorbed
                            absorbed.Add(float energy)
                            justAbsorbed <- true
                            None
                    else // just keep going
                        scene.intersectObj { o=point; dir=pastDir } obj
            ) intersection

        if not justAbsorbed then
            escaped.Add(float energy)
    scene.write()

    printfn "%d neutrons escaped" escaped.Count
    printfn "%d neutrons were absorbed" absorbed.Count

    let escapedData =
        Histogram(
            x = escaped,
            name = "escaped",
            opacity = 0.75,
            marker = Marker(
                color = "red"
            )
        )

    let absorbedData =
        Histogram(
            x = absorbed,
            name = "absorbed",
            opacity = 0.75,
            marker = Marker(
                color = "blue"
            )
        )

    let layout =
        Layout(
            barmode = "stack",
            title = "Energies of Escaped and Absorbed Neutrons",
            xaxis = Xaxis(
                title = "Energy (eV)",
                ``type`` = "log",
                autorange = true
            ),
            yaxis = Yaxis(
                title = "Count",
                ``type`` = "log",
                autorange = true
            )
        )

    [escapedData; absorbedData]
    |> Chart.Plot
    |> Chart.WithLayout layout
    |> Chart.Show

    0