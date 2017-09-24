type Rate = decimal

type Currency = Currency of string 

type ExchangeRate = {
    source: Currency
    destination: Currency
    rate: Rate
}

type RateByCurrencies = Map<(Currency*Currency), Rate>

type TreeNode =
    | Root of Currency
    | Child of Currency * TreeNode




let euro = Currency "EUR"
let dollar = Currency "USD"
let yen = Currency "YEN"
let yuan = Currency "YUAN"

let exchangeRates = [
    { source = euro; destination = dollar; rate = 1.33m }
    { source = yen; destination = dollar; rate = 20.61m }
]




let getTreePath node =
    let rec getTreePathRec currencies n =
        match n with
        | Root c -> c :: currencies
        | Child (c,parent) -> getTreePathRec (c::currencies) parent
    
    getTreePathRec [] node


// let createRateByCurrencies exchangeRates =
//     exchangeRates
//     |> List.collect (fun e ->
//         [ ((e.source, e.destination), e.rate)
//           ((e.destination, e.source), 1m / e.rate)
//         ])
//     |> Map.ofList
// let rateByCurrencies = createRateByCurrencies exchangeRates

let getRatio
    (rateByCurrencies : Map<(Currency*Currency), Rate>)
    (currencyPath : Currency list) =
    
    currencyPath
    |> List.zip (currencyPath |> List.skip 1)
    |> List.sumBy (fun t -> rateByCurrencies.[t])

let shortestPath
    (source : Currency)
    (destination : Currency)
    rateByCurrencies =

    let sourceDestTuples =
        rateByCurrencies
        |> Map.toList
        |> List.map (fun (key, _) -> key)

    let rec shortestPathRec src dest remainingCurrencies nodes =
        let possibleCurrencies =
            sourceDestTuples
            |> List.filter (
                fun (cSource,cDest) ->
                    cSource = source &&
                    remainingCurrencies |> List.exists cDest)
            |> List.map snd


    let remainingCurrencies =
        rateByCurrencies
        |> Map.toList
        |> List.collect (fun ((c1,c2), _) -> [c1;c2])
        |> List.distinct
