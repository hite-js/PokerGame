module Player
open Type
open Deck
open System

let rec createPlayer (money:uint16) (deck:Card[]) (iter:uint16) (playerList:Player List) =
    match iter with
        |iter when iter >= 1us ->
            let hand:Hand = deck.[0],deck.[1],deck.[2],deck.[3],deck.[4]
            let newDeck = drawCardFromDeck 5us deck
            let player:Player = {
                id = iter;
                Money = money;
                Hand = hand;
            }
            let playerList = player :: playerList
            createPlayer 1000us newDeck (iter - 1us) playerList
        |0us -> printf "Players have been created"
                (playerList,deck)
        |_-> printf "Invalid Input: %d" iter
             (playerList,deck)

let rec removePlayerByIndex (index:int) (playerList: Player List) =
    match index, playerList with
    | 0, x::xs -> xs
    | index, x::xs -> x::removePlayerByIndex (index - 1) xs
    | index, [] -> failwith "Index out of range"

let rec removePlayerFromList (player:Player) (playerList: Player List) = 
    match playerList with
    | h::tl when h = player -> tl
    | h::tl -> h :: (removePlayerFromList player tl)
    | [] -> []

let rec takeAnte (playerList: Player List) (ante:uint16) (id:int) =
    match id with
    |id when id < playerList.Length ->
        let player = playerList.[id]
        let newMoney = player.Money - uint16 ante
        let newPlayer = {player with Money = newMoney}
        let newList = removePlayerByIndex (int id) playerList
        let updatedPlayerList = newPlayer :: newList
        takeAnte updatedPlayerList ante (id + 1)
    |id when id >= playerList.Length ->
        playerList
    |_-> printf "ERROR: Invalid ID"
         playerList

let rec setPlayers (error:string) (playerList: Player List) (deck:Card[]) =
    printf "%s" error
    printf"\n> "
    let (parsed,input) = Console.ReadLine () |> UInt16.TryParse
    match (parsed,input) with
    |(false, input) ->
        setPlayers "[ERROR]Invalid character" playerList deck
    |(true,input)->
        match input with
        |input when input > 1us && input < 6us ->
            let (playerList,deck) = createPlayer 1000us deck input playerList
            let reversedList = reverseList playerList
            (reversedList,deck)
        |_->
            setPlayers "[ERROR]Invalid number of players (Min:2, Max:5)" playerList deck

let getPlayerDetail (player:Player) =
    printf "\n------Player[%i]------" player.id
    printf "\n[Money] - %i" player.Money
    printf "\n[Hand]" 
    let playerHand = player.Hand
    match playerHand with 
    |(c1,c2,c3,c4,c5) ->
        printCard c1
        printCard c2
        printCard c3
        printCard c4
        printCard c5
        
let printPlayerDetails (playerList: Player List) =
    playerList |> List.iter (fun player ->
        match player with 
        |(player) -> getPlayerDetail player )

let reducePlayerMoneyByAmount (player:Player, playerList: Player List, amount:uint16) =
    let newMoney = player.Money - amount
    let newPlayer = {player with Money = newMoney}
    let playerIndex = playerList |> List.findIndex ((=) player)
    let newList = removePlayerByIndex playerIndex playerList
    let newPlayerList = newPlayer :: newList
    newPlayerList

let rec setAnte (error:string) =
    printf "%s" error
    printf "\n1- £10"
    printf "\n2- £20"
    printf "\n3- £30"
    printf "\n4- £50"
    printf"\n> "
    let (parsed,input) = Console.ReadLine () |> UInt16.TryParse
    match (parsed,input) with
    |(false, input) ->
        setAnte "[ERROR]Invalid Character"
    |(true,input)->
        match input with
        |input when input = 1us -> 10us
        |input when input = 2us -> 20us
        |input when input = 3us -> 30us
        |input when input = 4us -> 50us
        |_->
            setAnte "[ERROR]Invalid ante. Please select from the list"
