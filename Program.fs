let digitalRoot n =
    let rec sumDigits m = 
        match m with
        | 0 -> 0
        | _ -> m % 10 + sumDigits (m / 10)
    
    let rec dr m = 
        match m < 10 with
        | true -> m
        | false -> dr (sumDigits m)
    
    dr n

let mdrs n =
    let rec findFactors acc d n =
        match d * d > n, n % d = 0 with
        | true, _ -> acc
        | false, true -> findFactors (d::(n/d)::acc) (d+1) n
        | false, false -> findFactors acc (d+1) n
    
    let factors n = 
        match n with
        | 1 -> []
        | _ -> findFactors [] 2 n |> List.filter (fun x -> x < n) |> List.distinct
    
    let memo = System.Collections.Generic.Dictionary<int, int>()
    
    let rec compute n =
        match memo.ContainsKey(n) with
        | true -> memo.[n]
        | false ->
            let fs = factors n
            let result =
                match List.isEmpty fs with
                | true -> digitalRoot n
                | false ->
                    let maxFromFactors = 
                        fs |> List.map (fun d -> compute d + compute (n/d)) |> List.max
                    max (digitalRoot n) maxFromFactors
            memo.[n] <- result
            result
    
    compute n

let result =
    [2..999999]
    |> List.sumBy mdrs

printfn "%d" result