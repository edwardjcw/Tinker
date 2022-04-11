open System.Runtime.CompilerServices
open Tinker
open System 

[<Extension>]
type List<'T> =
    [<Extension>]
    static member OfOption opt =
        match opt with
        | Some x -> [x]
        | None -> []


//AHundredPrisoners.run()
//TwentyOneGame.run()
//BenfordsLaw.run()
//BarnsleyFern.run 720 720
//Poker.run()

//Monadster.Test.run()

Some 5 |> List.OfOption |> printfn "%A"