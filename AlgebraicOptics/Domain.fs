module Domain
    open FSharpPlus
    open Prelude.Concrete 

    type Species = Setsoa | Versicolor | Virginica
    type Measurements = { getMeasurements : float list}
    type Flower = { flowerSpecies : Species; flowerMeasurements: Measurements}

    let zipWith (f : 'a -> 'b -> 'c) (a: 'a list) (b : 'b list) =
        zip a b
        |> fold (fun (state: 'c list) (a: 'a,  b: 'b) -> [f a b] @ state) []
