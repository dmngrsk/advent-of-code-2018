open System
open System.IO
open System.Text.RegularExpressions

type Entry =
    | Arrival of int * DateTime
    | Asleep of DateTime
    | Awake of DateTime

let extractDate (m:Match) =
    let group = (fun (i:int) -> (int m.Groups.[i].Value))
    new DateTime(group 1, group 2, group 3, group 4, group 5, 00, DateTimeKind.Unspecified)

let guardIdFromStr msg = 
    (int (Regex.Match(msg, "Guard #(\\d+)").Groups.[1].Value))

let parseEntry str = 
    let m = Regex.Match(str, "\\[(\\d+)\\-(\\d+)\\-(\\d+) (\\d+):(\\d+)\\] (.+)")
    let date = (extractDate m)

    match (m.Groups.[6].Value) with
    | "falls asleep" -> Asleep date
    | "wakes up" -> Awake date
    | msg -> Arrival ((guardIdFromStr msg), date)

let date e =
    match e with
    | Arrival (_, dt) -> dt
    | Asleep (dt) -> dt
    | Awake (dt) -> dt

let isArrival e =
    match e with
    | Arrival (_, _) -> true
    | _ -> false

let rec partitionGuards es =
    seq {
        match (es |> Seq.tail |> Seq.tryFindIndex isArrival) with
        | Some i ->
            let cnt = i + 1
            yield es |> Seq.take cnt
            yield! es |> Seq.skip cnt |> partitionGuards
        | _ ->
            yield es
    }

let minutes (e1, e2) =
    match e1, e2 with
    | Asleep(t1), Awake(t2) -> seq { t1.Minute .. (t2.Minute - 1) }
    | _ -> Seq.empty

let calculateSleepingMinutes es =
    es
    |> Seq.collect id
    |> Seq.pairwise
    |> Seq.collect minutes

let guardIdFromSeq es =
    match (es |> Seq.head) with
    | Arrival (gid, _) -> gid
    | _ -> failwith "Not an Arrival entry."
    
let maxByMinutesCount mins = 
    mins
    |> Seq.groupBy id
    |> Seq.map (fun (m, ms) -> m, (Seq.length ms))
    |> Seq.maxBy (fun (_, ms) -> ms)

let findOutput input =
    let guardsWithSleepingMinutes =
        input
        |> Seq.map parseEntry
        |> Seq.sortBy date
        |> partitionGuards
        |> Seq.groupBy guardIdFromSeq
        |> Seq.map (fun (gid, es) -> (gid, calculateSleepingMinutes es))
        |> Seq.filter (fun (_, ms) -> not (Seq.isEmpty ms))

    let (frequentGuardId, frequentMins) =
        guardsWithSleepingMinutes
        |> Seq.maxBy (fun (_, ms) -> ms |> maxByMinutesCount |> snd)

    let frequentMinute = frequentMins |> maxByMinutesCount |> fst
    frequentGuardId * frequentMinute

let input = File.ReadAllLines("./input.txt")
printfn "%A" (findOutput input)