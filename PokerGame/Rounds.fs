module Rounds
open Player
open Type
open System

let rec getBetAmount () =
    let (parsed,input) = Console.ReadLine () |> UInt16.TryParse
    if parsed
    then
        input
    else
        printfn "ERROR: The input was not a valid integer"
        getBetAmount ()

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

let rec firstBet (error:string) (player:Player) (pokerGame:PokerGame) (amount:uint16) =
    printf "%s" error
    let pMoney = player.Money
    match pMoney with
      |pMoney when pMoney < amount -> firstBet "[ERROR]You don't have enough money!" player pokerGame amount
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
        let choice = getPlayerChoice "\nPlease select from the following option:\n" pokerGame
        match pokerGame.GameState with
        |CheckRound ->
            match choice with
            |1us -> printf "Player %i has checked. Next player turn" head.id // CHECK IS OPTIONAL AND IT WILL ONLY PRINT IF THE USER CAN DO IT
                    bettingRound tail pokerGame
            |2us -> let newPokerGame = playerFold head pokerGame                        //FOLD -> Removes players from list
                    bettingRound tail newPokerGame
            |3us -> printf "Please specify bet amount or type 0 to go back:\n"
                    printf "> "
                    let amount = getBetAmount ()
                    match amount with
                    |amount when amount > 0us ->
                        match amount with
                        |amount when amount <= head.Money ->
                            let betPokerGame = firstBet "" head pokerGame amount          //BET -> First Bet
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
            |1us -> printf "Call"
                    bettingRound tail pokerGame
            |2us -> printf "Raise"
                    bettingRound tail pokerGame
            |3us -> let newPokerGame = playerFold head pokerGame
                    bettingRound tail newPokerGame
            |_ -> printf "[ERROR]Invalid choice"
                  bettingRound tail pokerGame
        

