open System

let factorizations n =
    let rec factorize start n current acc =
        match n with
        | 1 -> [current] @ acc
        | _ ->
            [start .. n]
            |> List.filter (fun x -> n % x = 0)
            |> List.fold (fun a x -> 
                match x with
                | 1 -> a
                | _ -> factorize x (n / x) (x::current) a) acc
    
    match n with
    | n when n < 2 -> [[]]
    | _ -> factorize 2 n [] []

[<EntryPoint>]
let main args = 
    let result = factorizations 24
    printfn "%A" result
    0