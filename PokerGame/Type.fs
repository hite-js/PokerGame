module Type

type Suit =
    |Club
    |Heart
    |Spade
    |Diamond

type Rank =
    |Ace
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

 type GameState =
    |CheckRound
    |BettingRound
    |ShowdownRound

type Card = Rank * Suit

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

let rec printHand (hand:Card List) (index:uint16)=
    match hand with
    |[] -> ()
    |hd::tl -> printf "%i- " index
               printCard hd
               printf "\n"
               printHand tl (index+1us)

let merge a b = a @ b |> List.distinct