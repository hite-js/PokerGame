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
        printf "\n-%A %s " rank "♣"
    |Heart ->
        printf "\n-%A %s " rank "♥"
    |Spade ->
        printf "\n-%A %s " rank "♠"
    |Diamond ->
        printf "\n-%A %s " rank "♦" 

let reverseList list =
    match list with
    | [] -> []
    | _ -> list |> List.rev

