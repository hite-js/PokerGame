module Deck
open Type

let DeckOfCards = List.toArray [
    for s in [Club; Diamond; Heart; Spade] do
        for r in [Ace; King; Queen; Jack; Ten; Nine; Eight; Seven; Six; Five; Four; Three; Two] do
            yield Card(r,s)
] 

let shuffle cards =
    let rand = new System.Random()
    cards
        |> Array.map (fun c -> (rand.Next(), c))
        |> Array.sortBy fst
        |> Array.map snd

let removeTopCard (deck:Card[]) =
    deck |> Array.filter ((<>)deck.[0]);;

let rec drawCardFromDeck (cardAmounts:uint16) (deck:Card[]) =
    match cardAmounts with
        |cardAmounts when cardAmounts < 1us ->
            deck
        |_ ->
            let index = cardAmounts - 1us
            let newDeck = removeTopCard deck
            drawCardFromDeck (cardAmounts - 1us) (newDeck)

        
