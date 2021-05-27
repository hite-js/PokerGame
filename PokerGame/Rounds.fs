module Rounds
open Player
open Type
open System
open Deck

let rec getAmount () =
    let (parsed,input) = Console.ReadLine () |> UInt16.TryParse
    if parsed
    then
        input
    else
        printfn "ERROR: The input was not a valid integer"
        getAmount ()

let rec getPlayerChoice (error:string) (pokerGame:PokerGame) =
    printf "%s" error
    match pokerGame.GameState with
    |CheckRound ->
        printf "\n1- Check"
        printf "\n2- Fold"
        printf "\n3- Bet"
        printf "\n> "
        let (parsed,input) = Console.ReadLine () |> UInt16.TryParse
        match (parsed,input) with
        |(false, input) ->
            getPlayerChoice "[ERROR]Invalid Character" pokerGame
        |(true,input)->
            match input with
            |input when input > 0us && input < 4us -> input
            |_-> getPlayerChoice "[ERROR]Invalid choice please select from the following" pokerGame
    |BettingRound ->
        printf "\n1- Call"
        printf "\n2- Raise"
        printf "\n3- Fold"
        printf "\n> "
        let (parsed,input) = Console.ReadLine () |> UInt16.TryParse
        match (parsed,input) with
        |(false, input) ->
            getPlayerChoice "[ERROR]Invalid Character" pokerGame
        |(true,input)->
            match input with
            |input when input > 0us && input < 4us -> input
            |_-> getPlayerChoice "[ERROR]Invalid choice please select from the following" pokerGame
    |_-> getPlayerChoice "[ERROR]Invalid choice please select from the following" pokerGame

let rec playerBet (error:string) (player:Player) (pokerGame:PokerGame) (amount:uint16) =
    printf "%s" error
    let pMoney = player.Money
    match pMoney with
      |pMoney when pMoney < amount -> playerBet "[ERROR]You don't have enough money!" player pokerGame amount
      |_->
         let newPlayerList = reducePlayerMoneyByAmount(player,pokerGame.playerList,amount)
         let newPot = pokerGame.pot + amount
         let newPokerGame = {pokerGame with playerList = newPlayerList; pot = newPot;GameState = BettingRound;prevBet = amount}
         newPokerGame

let playerFold (player:Player) (pokerGame:PokerGame) =
    let newPlayerList = removePlayerFromList player pokerGame.playerList
    let newPokerGame = {pokerGame with playerList = newPlayerList}
    newPokerGame


let rec bettingRound (playerList:Player List) (pokerGame:PokerGame) =
    match playerList with
    | [] -> pokerGame
    | head::tail ->
        printf "\n--------------------\n"
        printf "|   PLAYER %i TURN  |\n" head.id
        printf "--------------------\n"
        getPlayerDetail head
        printf "\n\nPrevious Bet: %i\n" pokerGame.prevBet
        printf "Total pot money: %i\n" pokerGame.pot
        let choice = getPlayerChoice "\nPlease select from the following option:\n" pokerGame
        match pokerGame.GameState with
        |CheckRound ->
            match choice with
            |1us -> printf "Player %i has checked\n" head.id // CHECK IS OPTIONAL AND IT WILL ONLY PRINT IF THE USER CAN DO IT
                    bettingRound tail pokerGame
            |2us -> let newPokerGame = playerFold head pokerGame                        //FOLD -> Removes players from list
                    bettingRound tail newPokerGame
            |3us -> printf "Please specify bet amount or type 0 to go back:\n"
                    printf "> "
                    let amount = getAmount ()
                    match amount with
                    |amount when amount > 0us ->
                        match amount with
                        |amount when amount <= head.Money ->
                            let betPokerGame = playerBet "" head pokerGame amount          //BET -> First Bet
                            bettingRound tail betPokerGame
                        |_-> printf "\n[ERROR]You don't have enough money!"
                             let newList = head::tail
                             bettingRound newList pokerGame
                    |_-> let newList = head::tail 
                         bettingRound newList pokerGame
            |_ -> printf "[ERROR]Invalid choice"
                  bettingRound tail pokerGame
        |_->
            match choice with
            |1us -> let pMoney = head.Money            //CALL
                    match pMoney with
                    |pMoney when pMoney < pokerGame.prevBet ->
                        printf "\n[ERROR]You don't have enough money to call!"
                        let newList = head::tail
                        bettingRound newList pokerGame
                    |_-> let newPokerGame = playerBet "" head pokerGame pokerGame.prevBet
                         bettingRound tail newPokerGame
            |2us -> printf "Please specify the raise amount or type 0 to go back:\n"        //RAISE
                    printf "> "
                    let amount = getAmount ()
                    match amount with
                    |amount when amount > 0us ->
                        match amount with
                        |amount when amount <= head.Money ->
                            match amount with
                            |amount when amount > pokerGame.prevBet ->
                                let betPokerGame = playerBet "" head pokerGame amount
                                bettingRound tail betPokerGame
                            |_-> printf "\n[ERROR]The raise needs to be higher than the previous bet!"
                                 let newList = head::tail
                                 bettingRound newList pokerGame
                        |_-> printf "\n[ERROR]You don't have enough money!"
                             let newList = head::tail
                             bettingRound newList pokerGame
                    |_-> let newList = head::tail 
                         bettingRound newList pokerGame
            |3us -> let newPokerGame = playerFold head pokerGame            //FOLD
                    bettingRound tail newPokerGame
            |_ -> printf "[ERROR]Invalid choice"
                  bettingRound tail pokerGame
        

let rec swapCard (handList:Card List) (amount:uint16) =
    match amount with
    |0us -> handList
    |_-> printf "Select the card you want to swap: "
         let cardIndex = getAmount ()
         match cardIndex with 
         |cardIndex when cardIndex > 0us && cardIndex < (uint16 handList.Length + 1us) ->
            let card = handList.[int cardIndex - 1]
            let newHand = removeCardFromHand card handList
            printHand newHand 1us
            swapCard newHand (amount - 1us)
         |_-> printf "[ERROR]Invalid card"
              swapCard handList amount



//        --swapping round--
let rec swappingRound (playerList:Player List) (pokerGame:PokerGame) (deck:Card[]) =
    let sortedPlayerList = List.sortBy (fun (x : Player) -> x.id) playerList
    match sortedPlayerList with
    |[] -> pokerGame
    |head::tail ->
        printf "\n--------------------\n"
        printf "|   PLAYER %i TURN  |\n" head.id
        printf "--------------------\n"
        let player = head
        let hand = handToList player.Hand
        printHand hand 1us
        printf "\nHow many cards do you want to swap? "
        printf "\n> "
        let cardSwapAmount = getAmount ()
        match cardSwapAmount with
        |cardSwapAmount when cardSwapAmount >= 0us && cardSwapAmount < 4us ->
            let oldCards = swapCard hand cardSwapAmount
            printf "%A" oldCards
            let newCards = Array.toList (drawCardFromDeck cardSwapAmount deck)
            let newHand = merge oldCards newCards
            let newPlayerHand:Hand = newHand.[0],newHand.[1],newHand.[2],newHand.[3],newHand.[4]
            let newPlayer = {player with Hand = newPlayerHand}
            let newPlayerList = newPlayer :: removePlayerFromList player pokerGame.playerList
            let newPokerGame = {pokerGame with playerList = newPlayerList}
            swappingRound tail newPokerGame deck
        |_-> printf "[ERROR]Invalid amount(Min:0, Max:3)"
             let newList = head::tail
             swappingRound newList pokerGame deck