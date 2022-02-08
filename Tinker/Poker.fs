﻿module Tinker.Poker

let PLAYER_DELIMITER = "  "
let HAND_DELIMITER = " "

type Value =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
    
type Suit =
    | Diamond
    | Heart
    | Spade
    | Club 

type Card = {value:Value; suit:Suit}

type HandRank =
    | Empty
    | HighCard of int
    | Pair of int
    | TwoPairs of int * int
    | ThreeOfAKind of int   
    | Straight of int
    | Flush of int * int * int * int * int
    | FullHouse of int
    | FourOfAKind of int
    | StraightFlush of int

type Player = {name:string; hand:Card list; rank:HandRank list}

let emptyHand = Empty

type ComparableHandRank =
    | Greater
    | LessThan
    | Equal

let compareHandRanks left right =
    let comparison left' right' = if left' = right' then Equal else if left' > right' then Greater else LessThan
    let rec comparisonMultiple t = 
        match t with
        | [] -> Equal
        | (left', right')::r when left' = right' -> comparisonMultiple r
        | (left', right')::_ -> comparison left' right'
        
    match left, right with
    | StraightFlush li, StraightFlush ri -> comparison li ri
    | StraightFlush _, _ -> Greater
    | FourOfAKind li, FourOfAKind ri -> comparison li ri
    | FourOfAKind _, _ -> Greater
    | FullHouse li, FullHouse ri -> comparison li ri
    | FullHouse _, _ -> Greater
    | Flush (la, lb, lc, ld, le), Flush (ra, rb, rc, rd, re) ->
        let orderedValues (a, b, c, d, e) = [a; b; c; d; e] |> List.sortDescending
        let orderedLi = (la, lb, lc, ld, le) |> orderedValues
        let orderedRi = (ra, rb, rc, rd, re) |> orderedValues
        List.zip orderedLi orderedRi |> comparisonMultiple
    | Flush _, _ -> Greater
    | Straight li, Straight ri -> comparison li ri
    | Straight _, _ -> Greater
    | ThreeOfAKind li, ThreeOfAKind ri -> comparison li ri
    | ThreeOfAKind _, _ -> Greater
    | TwoPairs (la, lb), TwoPairs (ra, rb) ->
        let orderedValues (a, b) = [a; b] |> List.sortDescending
        let orderedLi = (la, lb) |> orderedValues
        let orderedRi = (ra, rb) |> orderedValues
        List.zip orderedLi orderedRi |> comparisonMultiple
    | TwoPairs _, _ -> Greater
    | Pair li, Pair ri -> comparison li ri
    | Pair _, _ -> Greater
    | HighCard li, HighCard ri -> comparison li ri
    | HighCard _, _ -> Greater
    | _ -> Equal

let orderedHandRanks handRanks =
    let comparator left right =
        match compareHandRanks left right with
        | Greater -> 1
        | Equal -> 0
        | LessThan -> -1
    handRanks
    |> List.sortWith comparator
    
let nextConsecutiveCard card =
    match card with
    | {value=Two; suit=suit} -> Some {value=Three; suit=suit}
    | {value=Three; suit=suit} -> Some {value=Four; suit=suit}
    | {value=Four; suit=suit} -> Some {value=Five; suit=suit}
    | {value=Five; suit=suit} -> Some {value=Six; suit=suit}
    | {value=Six; suit=suit} -> Some {value=Seven; suit=suit}
    | {value=Seven; suit=suit} -> Some {value=Eight; suit=suit}
    | {value=Eight; suit=suit} -> Some {value=Nine; suit=suit}
    | {value=Nine; suit=suit} -> Some {value=Ten; suit=suit}
    | {value=Ten; suit=suit} -> Some {value=Jack; suit=suit}
    | {value=Jack; suit=suit} -> Some {value=Queen; suit=suit}
    | {value=Queen; suit=suit} -> Some {value=King; suit=suit}
    | {value=King; suit=suit} -> Some {value=Ace; suit=suit}
    | {value=Ace; suit=suit} -> None
    
let sameSuit suit cards =
    cards
    |> List.forall (fun {value=_; suit=suit'} -> suit' = suit)

let toCardRank {value=value; suit=suit} =
    match value with
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | Ten -> 10
    | Jack -> 11
    | Queen -> 12
    | King -> 13
    | Ace -> 14    

let highestCardRank cards =
    cards
    |> List.map toCardRank
    |> List.sortDescending
    |> List.head

let ofAKind cards =
    let toOfAKind (count, cards') =
        match count, cards' with
        | 4, card::_ -> FourOfAKind (toCardRank card)
        | 3, card::_ -> ThreeOfAKind (toCardRank card)
        | 2, card::_ -> Pair (toCardRank card)
        | 1, card::_ -> HighCard (toCardRank card)
        | _, _ -> failwith "problem determining ofAKind"
    
    let toTwoPairsOrFullHouse handRanks = 
        match orderedHandRanks handRanks with
        | [ThreeOfAKind ti; Pair _] -> [FullHouse ti]
        | [Pair pi; Pair pi'; HighCard hi] -> [TwoPairs (pi, pi'); HighCard hi]
        | other -> other
        
    cards
    |> List.groupBy (fun card -> card.value)
    |> List.map snd
    |> List.map (fun cards' -> (cards'.Length, cards'))
    |> List.map toOfAKind
    |> toTwoPairsOrFullHouse

let straight cards =
    let isAStraight =
        cards
        |> List.sortBy toCardRank
        |> List.pairwise
        |> List.forall (fun (lowerCard, higherCard) -> toCardRank lowerCard + 1 = toCardRank higherCard)
    if isAStraight then cards |> highestCardRank |> Straight else Empty 
    
// start here: let flush    ... and then for straight flush, it's whether straight and flush can apply    

let toCard (rawCard : string) =
    let value =
        match rawCard.[0] with
        | '2' -> Two
        | '3' -> Three
        | '4' -> Four
        | '5' -> Five
        | '6' -> Six
        | '7' -> Seven
        | '8' -> Eight
        | '9' -> Nine
        | 'T' -> Ten
        | 'J' -> Jack
        | 'Q' -> Queen
        | 'K' -> King
        | 'A' -> Ace
        | _ -> failwith "No value"
    let suit =
        match rawCard.[1] with
        | 'D' -> Diamond
        | 'H' -> Heart
        | 'S' -> Spade
        | 'C' -> Club
        | _ -> failwith "No suit"
    {value=value; suit=suit}

let toHand (h : string) =
    h.Split HAND_DELIMITER
    |> Array.map toCard
    |> Array.toList

let toPlayers (s : string) =
    s.Split PLAYER_DELIMITER
    |> Array.map (fun rawPlayer -> rawPlayer.Split ":")
    |> Array.map (fun rawPlayer -> (rawPlayer.[0], rawPlayer.[1].Trim()))
    |> Array.map (fun (name, rawHand) -> {name=name; hand=toHand rawHand; rank=[emptyHand]})


    
let toPlayerRank {name=name; hand=hand; rank=_} =

    let pair = // this just looks for pair, so active pattern is overkill
        hand
        |> List.groupBy (fun card -> card.value)
        |> List.filter (fun (_, cards) -> cards.Length = 2)
        |> List.map (fun (_, cards) -> cards |> List.head)
        |> List.map toCardRank
    let rank (p : int list) = if p.Length = 0 then 0 else p |> List.maxBy id
    {name=name; hand=hand; rank=[Pair (rank pair)]}
    
let game (rawInput : string) =
    let rankedPlayers =
        rawInput
        |> toPlayers
        |> Array.map toPlayerRank
             
    "Tie"
(*    |> Array.fold (fun (winnerName, winnerRank) {name=name; hand=_; rank=rank} ->
                     if winnerRank = rank then ("Tie", rank)
                     else if winnerRank > rank then (winnerName, winnerRank)
                     else (name, rank))
                     ("No players, no winners", 0)
    |> fst*)
    
let simpleTest rawInput =
    let blackSide = [{value=Two;suit=Heart}
                     {value=Three;suit=Diamond}
                     {value=Five;suit=Spade}
                     {value=Nine;suit=Club}
                     {value=King;suit=Diamond}]
    
    let expected = {name="Black"; hand=blackSide; rank=[emptyHand]}
    let actual = (toPlayers rawInput).[0]
    expected = actual

let simpleTest2 rawInput =
    let expected = "Black"
    let actual = (game rawInput)
    expected = actual

let simpleOrderedHandRankTest =
    let expected1 = [StraightFlush 10]
    let actual1 = orderedHandRanks expected1
    let result1 = expected1 = actual1
    
    let expected2 = [Flush (9, 14, 10, 8, 2)]
    let actual2 = orderedHandRanks expected2 
    let result2 = expected2 = actual2
    
    let expected3 = [Straight 10]
    let actual3 = orderedHandRanks expected3
    let result3 = expected3 = actual3
    
    let expected4 = [FourOfAKind 2; HighCard 5]
    let actual4 = orderedHandRanks [HighCard 5; FourOfAKind 2]
    let result4 = expected4 = actual4
    
    let expected5 = [TwoPairs (10, 11); HighCard 5]
    let actual5 = orderedHandRanks [HighCard 5; TwoPairs (10, 11)]
    let result5 = expected5 = actual5
    
    result1 && result2 && result3 && result4 && result5
    
let samples =
    [
     "Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C AH"
     "Black: 2H 4S 4C 2D 4H  White: 2S 8S AS QS 3S"
     "Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C KH"
     "Black: 2H 3D 5S 9C KD  White: 2D 3H 5C 9S KH"
     "Black: AH AC 2H 2C 3D  White: 5H 5C 6S 6C 7H"]

let run () =         
    printfn $"{(simpleTest samples.[0])}"
    //printfn $"{(simpleTest2 samples.[4])}"
    printfn $"{simpleOrderedHandRankTest}"