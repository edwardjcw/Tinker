module Tinker.Poker

let PLAYER_DELIMITER = "  "
let HAND_DELIMITER = " "
let HANDS_ARE_EQUAL = 5

module GameTypes =
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

module Rank =
    open GameTypes
    type ComparableHandRank =
        | Greater
        | LessThan
        | Equal

    // ranks two cards, determining if the left card is higher than, equal to, or less than the right
    let compareHandRanks left right =
        let comparison left' right' = if left' = right' then Equal else if left' > right' then Greater else LessThan
        let rec comparisonMultiple t = 
            match t with
            | [] -> Equal
            | (left', right')::r when left' = right' -> comparisonMultiple r
            | (left', right')::_ -> comparison left' right'
            
        match left, right with
        | StraightFlush li, StraightFlush ri -> comparison li ri
        | _, StraightFlush _ -> LessThan
        | StraightFlush _, _ -> Greater
        | FourOfAKind li, FourOfAKind ri -> comparison li ri
        | _, FourOfAKind _ -> LessThan
        | FourOfAKind _, _ -> Greater
        | FullHouse li, FullHouse ri -> comparison li ri
        | _, FullHouse _ -> LessThan 
        | FullHouse _, _ -> Greater
        | Flush (la, lb, lc, ld, le), Flush (ra, rb, rc, rd, re) ->
            let orderedValues (a, b, c, d, e) = [a; b; c; d; e] |> List.sortDescending
            let orderedLi = (la, lb, lc, ld, le) |> orderedValues
            let orderedRi = (ra, rb, rc, rd, re) |> orderedValues
            List.zip orderedLi orderedRi |> comparisonMultiple
        | _, Flush _ -> LessThan
        | Flush _, _ -> Greater
        | Straight li, Straight ri -> comparison li ri
        | _, Straight _ -> LessThan
        | Straight _, _ -> Greater
        | ThreeOfAKind li, ThreeOfAKind ri -> comparison li ri
        | _, ThreeOfAKind _ -> LessThan 
        | ThreeOfAKind _, _ -> Greater
        | TwoPairs (la, lb), TwoPairs (ra, rb) ->
            let orderedValues (a, b) = [a; b] |> List.sortDescending
            let orderedLi = (la, lb) |> orderedValues
            let orderedRi = (ra, rb) |> orderedValues
            List.zip orderedLi orderedRi |> comparisonMultiple
        | _, TwoPairs _ -> LessThan
        | TwoPairs _, _ -> Greater
        | Pair li, Pair ri -> comparison li ri
        | _, Pair _ -> LessThan 
        | Pair _, _ -> Greater
        | HighCard li, HighCard ri -> comparison li ri
        | _, HighCard _ -> LessThan 
        | HighCard _, _ -> Greater
        | _ -> Equal

    // sorts given cards based on rank
    let orderedHandRanks handRanks =
        let comparator left right =
            match compareHandRanks left right with
            | Greater -> 1
            | Equal -> 0
            | LessThan -> -1
        handRanks
        |> List.sortWith comparator
        
    // determines if all the suits match in a list of cards
    let sameSuit suit cards =
        cards
        |> List.forall (fun {value=_; suit=suit'} -> suit' = suit)

    // the value of a given card
    let toCardRank {value=value; suit=_} =
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

    // retrieves the card the ranks the highest in a list of cards
    let highestCardRank cards =
        cards
        |> List.map toCardRank
        |> List.sortDescending
        |> List.head

    // transforms a card list to Four or Three of a Kind, Pair, Two Pairs, Full House or High Card
    let ofAKind cards =
        let toOfAKind (count, cards') =
            match count, cards' with
            | 4, card::_ -> FourOfAKind (toCardRank card)
            | 3, card::_ -> ThreeOfAKind (toCardRank card)
            | 2, card::_ -> Pair (toCardRank card)
            | 1, card::_ -> HighCard (toCardRank card)
            | _, _ -> failwith "problem determining ofAKind"
        
        let toTwoPairsOrFullHouse handRanks = 
            match orderedHandRanks handRanks |> List.rev with
            | [ThreeOfAKind ti; Pair _] -> [FullHouse ti]
            | [Pair pi; Pair pi'; HighCard hi] -> [TwoPairs (pi, pi'); HighCard hi]
            | other -> other
            
        cards
        |> List.groupBy (fun card -> card.value)
        |> List.map snd
        |> List.map (fun cards' -> (cards'.Length, cards'))
        |> List.map toOfAKind
        |> toTwoPairsOrFullHouse

    // transforms a card list to a straight or empty if it's not
    let straight cards =
        let isAStraight =
            cards
            |> List.sortBy toCardRank
            |> List.pairwise
            |> List.forall (fun (lowerCard, higherCard) -> toCardRank lowerCard + 1 = toCardRank higherCard)
        if isAStraight then cards |> highestCardRank |> Straight else Empty 
         
    // transforms a card list to a flush or empty if it's not
    let flush cards =
        let isFlush =
            let suitToUse = cards |> List.head |> fun {value=_; suit=suit} -> suit
            cards
            |> List.forall (fun {value=_; suit=suit} -> suit = suitToUse)
        if isFlush then
            cards
            |> List.sortBy toCardRank
            |> List.map toCardRank
            |> function
                | [a; b; c; d; e] -> Flush (a,b,c,d,e)
                | _ -> failwith "Flush can't be created"
        else Empty
        
    // transforms a card list to a straight flush or empty if it's not one
    let straightFlush cards =
        let straight' = cards |> straight
        let flush' = cards |> flush
        let straightRank = function | Straight s -> s | _ -> failwith "Can't get straight value for straight flush"
        if straight' = Empty || flush' = Empty then Empty
        else straightRank straight' |> StraightFlush

    // transforms a player to include the categorized hand (Flush, Pairs, etc)
    let toPlayerRank {name=name; hand=hand; rank=_} =
        let straightFlush' = hand |> straightFlush
        let flush' = hand |> flush
        let straight' = hand |> straight
        let ofAKind' = hand |> ofAKind
        if straightFlush' <> Empty then {name=name; hand=hand; rank=[straightFlush']}
        else if flush' <> Empty then {name=name; hand=hand; rank=[flush']}
        else if straight' <> Empty then {name=name; hand=hand; rank=[straight']}
        else {name=name; hand=hand; rank=ofAKind'}
    
module Parser =
    open GameTypes
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

    // creates player from interpreting string (rank is not determined yet)
    let toPlayers (s : string) =
        s.Split PLAYER_DELIMITER
        |> Array.map (fun rawPlayer -> rawPlayer.Split ":")
        |> Array.map (fun rawPlayer -> (rawPlayer.[0], rawPlayer.[1].Trim()))
        |> Array.map (fun (name, rawHand) -> {name=name; hand=toHand rawHand; rank=[emptyHand]})

module Game =
    let play (rawInput : string) =
        // from raw input to list of players with hands ranked (Flush, Pairs, etc)
        let rankedPlayers =
            rawInput
            |> Parser.toPlayers
            |> Array.map Rank.toPlayerRank
            |> Array.toList
        
        // given a list of players, tries to determine the best ranked card among them 
        let bestHand =
            rankedPlayers
            |> List.collect (fun {name=_; hand=_; rank=rank} -> rank)
            |> List.distinct
            |> Rank.orderedHandRanks
            |> List.rev
            |> fun handRanks' -> if handRanks'.Length = HANDS_ARE_EQUAL then [] else handRanks'
            |> List.tryHead

        // if there's no best hand, then it's a tie. otherwise, find out who had the best hand
        match bestHand with
        | None -> "Tie"
        | Some bestHand' -> 
            rankedPlayers
            |> List.filter (fun {name=_; hand=_; rank=rank} -> rank |> (List.contains bestHand'))
            |> List.head
            |> fun {name=name; hand=_; rank=_} -> name 

module Tests =
    open GameTypes
    let simpleTest rawInput =
        let blackSide = [{value=Two;suit=Heart}
                         {value=Three;suit=Diamond}
                         {value=Five;suit=Spade}
                         {value=Nine;suit=Club}
                         {value=King;suit=Diamond}]
        
        let expected = {name="Black"; hand=blackSide; rank=[emptyHand]}
        let actual = (Parser.toPlayers rawInput).[0]
        expected = actual

    let simpleTestWinner winner rawInput =
        let actual = (Game.play rawInput)
        winner = actual 

    let simpleOrderedHandRankTest () =
        let expected1 = [StraightFlush 10]
        let actual1 = Rank.orderedHandRanks expected1
        let result1 = expected1 = actual1
        
        let expected2 = [Flush (9, 14, 10, 8, 2)]
        let actual2 = Rank.orderedHandRanks expected2 
        let result2 = expected2 = actual2
        
        let expected3 = [Straight 10]
        let actual3 = Rank.orderedHandRanks expected3
        let result3 = expected3 = actual3
        
        let expected4 = [FourOfAKind 2; HighCard 5]
        let actual4 = Rank.orderedHandRanks [HighCard 5; FourOfAKind 2] |> List.rev
        let result4 = expected4 = actual4
        
        let expected5 = [TwoPairs (10, 11); HighCard 5]
        let actual5 = Rank.orderedHandRanks [HighCard 5; TwoPairs (10, 11)] |> List.rev
        let result5 = expected5 = actual5

        result1 && result2 && result3 && result4 && result5

    let simpleOrderedHandRankTest2 () =
        let expected = [TwoPairs (13, 6); TwoPairs (12, 10); HighCard 14; HighCard 11]
        let actual = Rank.orderedHandRanks expected |> List.rev
        let result = expected = actual
        result
        
    let samples =
        [
         "Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C AH"
         "Black: 2H 4S 4C 2D 4H  White: 2S 8S AS QS 3S"
         "Black: 2H 3D 5S 9C KD  White: 2C 3H 4S 8C KH"
         "Black: 2H 3D 5S 9C KD  White: 2D 3H 5C 9S KH"
         "Black: AH AC 2H 2C 3D  White: 5H 5C 6S 6C 7H"]

    let runTests () =
        let white = "White"
        let black = "Black"
        let tie = "Tie"
        printfn $"proper conversion: {(simpleTest samples.[0])}"
        printfn $"high card: {(simpleTestWinner white samples.[0])}"
        printfn $"full house beats flush: {(simpleTestWinner black samples.[1])}"
        printfn $"next highest card: {(simpleTestWinner black samples.[2])}"
        printfn $"tie: {(simpleTestWinner tie samples.[3])}"
        printfn $"higher two pairs wins: {(simpleTestWinner black samples.[4])}" 
        printfn $"ordering works: {simpleOrderedHandRankTest ()}"
        printfn $"ordering works2: {simpleOrderedHandRankTest2 ()}"        

let run () = Tests.runTests ()
