module Tinker.CodeWars

module Division =
    let rec gcd ab =
        match ab with
        | 0,b -> b
        | a,0 -> a
        | a,b -> gcd (b,(a%b))    

    let lcd numbers =
        if numbers |> List.isEmpty then 0
        else
            numbers
            |> List.reduce (fun a b -> a*b/(gcd (a,b)))
            
    let commonDenominator = lcd
    let commonNumerator cd (n,d) = (cd/d) * n // normalize numerator
    let convertFracts (ls : (int * int) list) =
        let reducedFractions =
          ls
          |> List.map (fun (n,d) ->
              let gcd' = gcd (n,d)
              (n/gcd',d/gcd'))
        let denominator = reducedFractions |> List.map snd |> commonDenominator
        let numerators = reducedFractions |> List.map (commonNumerator denominator)
        numerators
        |> List.map (fun n -> (n,denominator))
        
module Perimeter =
    open System.Numerics
    let perimeter (n: BigInteger): BigInteger =
        Seq.unfold (fun (a,b) -> Some (a, (b, a+b))) (1I,1I)
        |> Seq.take (int n+1)
        |> Seq.reduce (+)
        |> fun s -> s*4I
        
module PrimeGap =
    let gap(g: int) (m: int) (n: int): int[] =
        let isPrime n =
            if n = 2 || n = 3 then true
            elif n <= 1 || n%2 = 0 || n%3 = 0 then false
            else
                Seq.initInfinite (fun i -> (6 * i) + 5)
                |> Seq.takeWhile (fun i -> i * i <= n)
                |> Seq.exists (fun i -> n%i = 0 || n%(i+2) = 0)
                |> not
        seq {m .. n}
        |> Seq.filter isPrime // primes
        |> Seq.pairwise 
        |> Seq.tryFind (fun (a,b) -> b-a = g) // try find gap
        |> function | Some (a,b) -> [|a;b|] | None -> [||]
            
module WeirdPrimeGenerator =
    let rec gcd ab =
        match ab with
        | 0,b -> b
        | a,0 -> a
        | a,b -> gcd (b,(a%b))    
    
    let primeRecRelations =
        Seq.unfold (fun (n,r) ->
            match n with
            | 1 -> Some(7, (n+1,7))
            | _ ->
                let r' = r + gcd(n,r)
                Some(r', (n+1,r'))) (1,0)
    
    let subtractions = seq {
        yield 1
        yield! primeRecRelations
               |> Seq.pairwise
               |> Seq.map (fun (a,b) -> b-a)
    }
    
    let countOnes(n: int): int =
        subtractions |> Seq.take n |> Seq.filter (fun e -> e=1) |> Seq.length

    let maxPn(n: int): int =
        subtractions |> Seq.filter (fun e -> e <> 1) |> Seq.distinct |> Seq.take n |> Seq.max

    let anOverAverage(n: int): int =
        Seq.zip3 (seq {1 .. n}) primeRecRelations subtractions
        |> Seq.take n
        |> Seq.filter (fun (_,_,s) -> s <> 1)
        |> Seq.map (fun (i,p,_) -> float p/(float i))
        |> Seq.average
        |> int
        
    
    