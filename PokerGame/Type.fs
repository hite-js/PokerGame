module Type

type Suit =
    |Club
    |Heart
    |Spade
    |Diamond

type Rank =
    |Two
    |Three
    |Four
    |Five
    |Six
    |Seven
    |Eight
    |Nine
    |Ten
    |Jack
    |Queen
    |King
    |Ace

 type GameState =
    |CheckRound
    |BettingRound
    |ShowdownRound


type Card = Rank * Suit

type PokerHand = 
    |HighCard of Rank List
    |OnePair of pair:Rank * kickers:Rank List
    |TwoPair of p1:Rank * p2:Rank * kickers:Rank
    |ThreeKind of threekind:Rank
    |Straight of Rank List
    |Flush
    |FullHouse of p1:Rank * p2:Rank 
    |FourKind of fourkind:Rank
    |StraightFlush
    |RoyalFlush

type Hand = Card * Card * Card * Card * Card

type Player = 
    { id: uint16; 
    Money: uint16; 
    Hand: Hand}

type PokerGame = 
    { GameState: GameState;
    pot: uint16;
    playerList: Player List;
    prevBet: uint16;
    }

let printCard (card:Card) =
    let rank,suit = card
    match suit with
    |Club ->
        printf "%A %s " rank "♣"
    |Heart ->
        printf "%A %s " rank "♥"
    |Spade ->
        printf "%A %s " rank "♠"
    |Diamond ->
        printf "%A %s " rank "♦" 

let reverseList list =
    match list with
    | [] -> []
    | _ -> list |> List.rev

let handToList (hand:Hand) =
    match hand with
    |(c1,c2,c3,c4,c5) -> [c1;c2;c3;c4;c5]

let getCardValue (value:Rank) =
    match value with
    |Ace -> 1
    |Two -> 2
    |Three -> 3
    |Four -> 4
    |Five -> 5
    |Six -> 6
    |Seven -> 7
    |Eight -> 8
    |Nine -> 9
    |Ten -> 10
    |Jack -> 11
    |Queen -> 12
    |King -> 13
    

let rec printHand (hand:Card List) (index:uint16)=
    match hand with
    |[] -> ()
    |hd::tl -> printf "%i- " index
               printCard hd
               printf "\n"
               printHand tl (index+1us)

let merge a b = a @ b |> List.distinct

let sortedHandByValue (hand:Hand) =
    let handList  = handToList hand
    let sortedList = List.sortBy (fun (x : Card) -> getCardValue(fst(x))) handList
    let sortedHand:Hand = sortedList.[0],sortedList.[1],sortedList.[2],sortedList.[3],sortedList.[4]
    sortedHand

let rec removeRankFromList (rank:Rank) (rankList: Rank List) = 
    match rankList with
    | h::tl when h = rank -> tl
    | h::tl -> h :: (removeRankFromList rank tl)
    | [] -> []


let isNextValue (val1 : Rank) val2 = int val2 = getCardValue val1 + 1

let isStraight hand = 
   let listHand = handToList hand
   let listSeq = List.toSeq listHand
   let pair = Seq.pairwise listSeq
   let bool = Seq.forall (fun ((v1, _), (v2, _)) -> isNextValue v1 (getCardValue v2)) pair
   match bool with
   |true -> true
   |false -> match listHand |> List.map (fun c -> fst(c)) with
             | [ Ace; Ten; Jack; Queen; King ] -> true
             | _ -> false

let isFlush hand = 
   hand
   |> handToList
   |> List.toSeq
   |> Seq.pairwise
   |> Seq.forall (fun ((_, s1), (_, s2)) -> s1 = s2)

let getValueCounts hand = 
   hand
   |> handToList
   |> List.toSeq
   |> Seq.countBy (fun (v, _) -> v)
   |> Seq.map (fun (_, count) -> count)
   |> Seq.toList
   |> List.sort
   |> List.rev

type Occurrences = Occurrences of (Rank * int) list

let getKickers (Occurrences occurrences) =
    occurrences
    |> List.filter (snd >> (=) 1)
    |> List.map fst
    |> List.sortByDescending getCardValue 

let getOccurrences (hand:Hand): Occurrences  =
    let handList = handToList hand
    handList
    |> List.map (fun c -> fst(c))
    |> List.countBy id
    |> List.sortByDescending snd
    |> Occurrences

let getOnePair (Occurrences occurrences as wrapped) =
    match occurrences with
    | (pair,2)::tail when tail |> List.map snd = [1;1;1] -> OnePair(pair, getKickers wrapped)
    |_-> failwith "Not a valid hand"

let getTwoPair (Occurrences occurrences) =
    match occurrences with
    | [ (p1,2); (p2,2); (kicker,1) ] -> TwoPair(p1, p2, kicker)
    | _ -> failwith "Not a valid hand"

let getThreeKind (Occurrences occurrences as wrapped) =
    match occurrences with
    | (threekind,3)::_ -> ThreeKind(threekind)
    | _ -> failwith "Not a valid hand"

let getFullHouse (Occurrences occurrences) =
    match occurrences with
    | [ (p1,3); (p2,2) ] -> FullHouse(p1, p2)
    | _ -> failwith "Not a valid hand"

let getFourKind (Occurrences occurrences as wrapped) =
    occurrences
    |> List.head
    |> fun (value, number) ->
        if number = 4 then
             FourKind(value)
        else failwith "Not a vlid hand"


let scoreHand hand = 
   let isStraight = isStraight hand
   let isFlush = isFlush hand
   let valueCounts = getValueCounts hand
   match (isStraight, isFlush, valueCounts) with
   | (true, true, _) -> 
      let ((value, _), _, _, _, _) = hand
      if value = Rank.Ace then RoyalFlush
      else StraightFlush
   | (false, true,_) -> Flush
   | (true, false,_) -> let ((c1,_),(c2,_),(c3,_),(c4,_),(c5,_)) = hand
                        Straight([c1;c2;c3;c4;c5])
   | (false, false,[4;1]) -> let occurrences = getOccurrences hand
                             getFourKind(occurrences)
   | (false, false,[3;2]) -> let occurrences = getOccurrences hand
                             getFullHouse(occurrences)
   | (false, false,[3;1;1]) -> let occurrences = getOccurrences hand
                               getThreeKind(occurrences)
   | (false, false,[2;2;1]) -> let occurrences = getOccurrences hand
                               getTwoPair(occurrences)
   | (false, false,[2;1;1;1]) -> let occurrences = getOccurrences hand
                                 getOnePair(occurrences)
   | (false, false,_) -> let ((c1,_),(c2,_),(c3,_),(c4,_),(c5,_)) = hand
                         HighCard([c1;c2;c3;c4;c5])

let rec getHighestCard (p1:Player) (p2:Player) (k1:Rank List) (k2:Rank List) =
    if k1.Length > 0 then
        match (k1,k2) with
        |k1,k2 when k1.Head > k2.Head -> Some p1
        |k1,k2 when k1.Head < k2.Head -> Some p2
        |_-> let newk1 = removeRankFromList k1.Head k1
             let newk2 = removeRankFromList k2.Head k2
             getHighestCard p1 p2 newk1 newk2
    else None

let isAce (p1:Player) (p2:Player) (k1:Rank List) (k2:Rank List) =
    match (k1,k2) with
    |k1,k2 when k1.Head = Ace && k2.Head = Ace -> let newk1 = removeRankFromList k1.Head k1 
                                                  let newk2 = removeRankFromList k2.Head k2 
                                                  let winner = getHighestCard p1 p2 (List.rev newk1) (List.rev newk2)
                                                  winner
    |_-> let winner = getHighestCard p1 p2 (List.rev k1) (List.rev k2)
         winner

let getWinner (p1:Player) (p2:Player) =
    let p1SortedHand = sortedHandByValue p1.Hand
    let p2SortedHand = sortedHandByValue p2.Hand
    let p1Hand = scoreHand p1SortedHand
    printf "\nScore: %A" p1Hand
    let p2Hand = scoreHand p2SortedHand
    printf "\nScore: %A" p2Hand
    match (p1Hand, p2Hand) with
    |HighCard k1, HighCard k2 -> isAce p1 p2 k1 k2
    |OnePair(pair1,k1), OnePair(pair2,k2) -> match pair1,pair2 with
                                             |pair1,pair2 when pair1 > pair2 -> Some p1
                                             |pair1,pair2 when pair1 > pair2 -> Some p2
                                             |_-> isAce p1 p2 k1 k2
    |TwoPair(pair1,pair2,k1), TwoPair(secondp1,secondp2,k2) -> match pair1,secondp1 with
                                                               |pair1,secondp1 when pair1 > secondp1 -> Some p1
                                                               |pair1,secondp1 when pair1 < secondp1 -> Some p2
                                                               |_-> match pair2,secondp2 with
                                                                    |pair2,secondp2 when pair2 > secondp2 -> Some p1
                                                                    |pair2,secondp2 when pair2 < secondp2 -> Some p2
                                                                    |_-> isAce p1 p2 [k1] [k2]
    |ThreeKind(threeKind),ThreeKind(threeKind2) -> match threeKind, threeKind2 with
                                                         |threeKind, threeKind2 when threeKind > threeKind2 -> Some p1
                                                         |threeKind, threeKind2 when threeKind < threeKind2 -> Some p2
                                                         |_-> None
    |Straight k1, Straight k2 -> isAce p1 p2 k1 k2
    |FullHouse(pair1,pair2), FullHouse(secondp1,secondp2) -> match pair1,secondp1 with
                                                             |pair1,secondp1 when pair1 > secondp1 -> Some p1
                                                             |pair1,secondp1 when pair1 < secondp1 -> Some p2
                                                             |_-> match pair2,secondp2 with
                                                                  |pair2,secondp2 when pair2 > secondp2 -> Some p1
                                                                  |pair2,secondp2 when pair2 < secondp2 -> Some p2
                                                                  |_-> None
    |FourKind fk1, FourKind fk2 -> match fk1, fk2 with
                                   |fk1, fk2 when fk1 > fk2 -> Some p1
                                   |fk1, fk2 when fk1 < fk2 -> Some p2
                                   |_-> None
    |_-> None