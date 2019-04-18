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

[<EntryPoint>]
let main argv =
    0 // return an integer exit code
