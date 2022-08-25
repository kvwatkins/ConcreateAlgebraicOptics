module Program = 
    open Prelude.Concrete
    open Domain
    open FSharpPlus

    // Classification functions
    let measurementDistance (xs : Measurements) (ys: Measurements) : float =
        let diff a b = (a - b) * 2.0
        (zipWith diff xs.getMeasurements ys.getMeasurements)
        |> (sqrt << sum)

    let classify (flowers : Flower list) (measurements : Measurements)  =
        match flowers with
        | [] -> None
        | flowers -> 
            let flower = Seq.minBy (fun (f : Flower) -> measurementDistance f.flowerMeasurements measurements) flowers
            Some({flowerSpecies = flower.flowerSpecies; flowerMeasurements = measurements})
    
    // Algebraic Lens carrying the algebra for the product of the list monad 
    let flowerLens = AlgebraicLens ((fun (f: Flower) -> f.flowerMeasurements), classify)

    // Sample Data
    let versicolor = {flowerSpecies = Versicolor; flowerMeasurements = { getMeasurements = [2;3;4;2] } }
    let setosa = {flowerSpecies = Setsoa; flowerMeasurements = { getMeasurements = [5;4;3;2.5] } }
    let flowers = [versicolor; setosa]

    // Viewing the Measurements of a given flower sample set
    let test = flowerLens.view (flowers.[0])

    // Classifying a new flower based off of sample data
    let test2 = flowerLens.classify flowers { getMeasurements = [1.9; 3.2; 3.8; 2]}

    printfn "%A" test   // { getMeasurements = [2.0; 3.0; 4.0; 2.0] }
    printfn "%A" test2  // Some { flowerSpecies = Versicolor flowerMeasurements = { getMeasurements = [1.9; 3.2; 3.8; 2.0] } }
