open Type
open Deck
open Player
open Rounds

open System
let rec getInt () =
    let (parsed,input) = Console.ReadLine () |> UInt16.TryParse
    if parsed
    then
        input
    else
        printfn "ERROR: The input was not a valid integer"
        getInt ()
 


[<EntryPoint>]
let main argv =
    Console.WriteLine("-------------------POKER GAME-------------------")
    Console.WriteLine("|                                              |")
    Console.WriteLine("|                                              |")
    Console.WriteLine("|                  by Hitesh                   |")
    Console.WriteLine("|                                              |")
    Console.WriteLine("|                                              |")
    Console.WriteLine("-------------------POKER GAME-------------------")

    printf "1. Play\n"
    printf "2. Quit\n"
    printf "> "

    let choice = getInt ()
    match choice with
        |1us -> printf ""
        |2us -> Environment.Exit 55
        |_ -> printf "Invalid input"
    //player set
    let startDeck = shuffle DeckOfCards
    let playerList: Player list = []
    let (playerList,deck) = setPlayers "\nPlease specify the number of players (Min:2,Max:5)" playerList startDeck
    //ante set
    let ante = setAnte "\nPlease specify the ante: "
    let newPlayerList = takeAnte playerList ante 0
    printPlayerDetails newPlayerList
    printf "\n\n\n\n\n"
    //game set
    let pokerGame:PokerGame = {GameState = CheckRound; pot = (ante * uint16 newPlayerList.Length); playerList = newPlayerList;prevBet = 0us}
    
    let firstRound = bettingRound pokerGame.playerList pokerGame
    printf "\nFirstRound Stats:\n"
    printf "\n%i" firstRound.pot
    printf "\n%i" firstRound.prevBet
    printf "\n%A" firstRound.playerList
    let swappingRound = swappingRound firstRound.playerList firstRound deck
    printf "\nSwapround Stats:\n"
    printf "\n%A" swappingRound.playerList
    let secondRound = bettingRound swappingRound.playerList swappingRound
    printf "\nSecondRound Stats:\n"
    printf "\n%i" secondRound.pot
    printf "\n%i" secondRound.prevBet
    printf "\n%A" secondRound.playerList
    0