open System
open System.IO
open System.Numerics

open FSharp.Data.UnitSystems.SI.UnitSymbols // lets me see unit symbols in editor
open MathNet.Numerics.Distributions

open Arguments
open Constants
open CrossSections
open Scene

let rnd = System.Random ()
let normalRnd = Normal ()

let randomDir () =
    // TODO: is this generator biased?
    let theta = rnd.NextDouble() * 2.0 * Math.PI |> float32
    let phi = rnd.NextDouble() * 2.0 - 1.0 |> acos |> float32
    Vector3 (cos theta * sin phi, sin theta * sin phi, cos phi)

let pathLength (sigma : float</m>) () : float<m> =
    (-1.0 / sigma) * log (rnd.NextDouble())

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
    let settings = readSettings argv

    if not (Directory.Exists(settings.outputDir)) then
        printfn "creating directory %s for output" settings.outputDir
        Directory.CreateDirectory(settings.outputDir) |> ignore

    let buildOutputName suffix = Path.Combine(settings.outputDir,settings.outputName + suffix)

    printfn "Building scene..."
    let scene = buildScene settings.sceneFile buildOutputName
    printfn "...done"

    let crossSectionsMap : Map<string,CrossSection> =
        Seq.map (fun material -> material.mat) scene.materials
        |> Seq.distinct
        |> Seq.map (fun mat -> mat,readCrossSections mat)
        |> Map.ofSeq

    // printfn "cross sections map: %A" crossSectionsMap

    // TODO: change energy as I collide with things
    let startingEnergy = 2.45e6<eV> // is this the starting energy?

    let mutable numEscaped = 0
    let mutable numAbsorbed = 0

    use logFile = new StreamWriter(buildOutputName ".log")
    // Event IDs: 1 = Elastic Scattering, 2 = Absorption, 3 = Escape
    fprintfn logFile "Neutron #,Event #,Event ID,Pos,In Dir,Out Dir,In Energy (eV),Out Energy (eV)"

    let showV (v3 : Vector3) =
        sprintf "%f %f %f" v3.X v3.Y v3.Z

    printfn "Scattering..."
    for neutronNum in [1..settings.neutrons] do
        // printfn "#%d" i
        // assuming that I start outside of all objects in the scene
        let mutable eventNum = 0
        let mutable pastDir = randomDir()
        let mutable intersection = scene.intersectScene { o=Vector3.Zero; dir=pastDir }
        let mutable justAbsorbed = false
        let mutable energy = startingEnergy

        let logLine = fprintfn logFile "%d,%d,%d,%s,%s,%s,%s,%s"

        while intersection <> None do
            intersection <- Option.bind (fun { point=point; distance=distance; dest=dest } ->
                let logLineSA eventType newDir newEnergy =
                    logLine neutronNum eventNum eventType (showV point) (showV pastDir) newDir (string energy) newEnergy
                match dest with
                | None -> // we are outside of all objects -> no need to look at cross sections
                    scene.intersectScene { o=point; dir=pastDir }
                | Some (material,obj) ->
                    let crossSections = (crossSectionsMap.TryFind material.mat).Value
                    let totalMicro = (crossSections.getTotal energy).Value
                    let totalMacro = totalMicro * InvM3PerAmg * material.number_density * M2PerBarn
                    let distanceTraveled = pathLength totalMacro ()
                    if distanceTraveled < distance then // we are doing something!
                        eventNum <- eventNum + 1

                        let elasticMicro = (crossSections.getElastic energy).Value
                        let elasticMacro = elasticMicro * InvM3PerAmg * material.number_density * M2PerBarn
                        if (elasticMacro / totalMacro) > rnd.NextDouble() then // elastic scattering
                            let newDir,newEnergy = elasticScatter pastDir energy
                            logLineSA 1 (showV newDir) (string newEnergy)
                            pastDir <- newDir
                            energy <- newEnergy
                            scene.intersectObj { o=point; dir=pastDir } obj
                        else // absorbed
                            numAbsorbed <- numAbsorbed + 1
                            justAbsorbed <- true
                            logLineSA 2 "NA" "NA"
                            None
                    else // just keep going
                        scene.intersectObj { o=point; dir=pastDir } obj
            ) intersection

        if not justAbsorbed then
            numEscaped <- numEscaped + 1
            logLine neutronNum eventNum 3 "NA" "NA" (showV pastDir) "NA" (string energy)

    printfn "...done"

    printfn "Writing scene..."
    scene.write()
    printfn "...done"

    printfn "%d neutrons escaped" numEscaped
    printfn "%d neutrons were absorbed" numAbsorbed

    0
