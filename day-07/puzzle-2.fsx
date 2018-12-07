open System.IO
open System.Text.RegularExpressions

let flip f a b = f b a
let neg f a = not (f a)

let parseLine str = 
    let regexMatch = Regex.Match(str, "Step ([A-Z]) must be finished before step ([A-Z]) can begin.")
    let group = (fun (i:int) -> char (regexMatch.Groups.[i].Value))
    (group 1, group 2)

let makeStepsList steps = steps |> List.map fst |> List.append (List.map snd steps) |> List.distinct
let appendStopConditionSteps steps = List.append steps (steps |> makeStepsList |> List.map (fun x -> x, '~'))

let makeTimesMap steps = steps |> makeStepsList |> List.map (fun c -> c, ((int c) - (int 'A') + 61)) |> Map.ofList
let updateTimesMap steps (times:Map<char,int>) = 
    times 
    |> Map.toSeq 
    |> Seq.map (fun (k, t) -> if List.contains k steps then k, (t - 1) else k, t)
    |> Map.ofSeq

let findIndependentSteps maxWorkers workers steps = 
    let requiredSteps = steps |> Seq.map fst |> Set.ofSeq
    let executedSteps = steps |> Seq.map snd |> Set.ofSeq
    let independentSteps = executedSteps |> Set.difference requiredSteps |> Set.filter (neg ((flip List.contains) workers)) |> Set.toList |> List.sortBy id
    let independentStepsLength = List.length independentSteps
    independentSteps |> List.take (min (maxWorkers - independentStepsLength) independentStepsLength) |> List.ofSeq
    
let rec findStepOrder acc maxWorkers workers (times:Map<char, int>) steps =
    match steps with
    | [] -> acc
    | _ ->
        let currentSteps = List.append workers (findIndependentSteps maxWorkers workers steps)
        let completeSteps = currentSteps |> Seq.filter (fun s -> times.[s] = 1) |> List.ofSeq
        let ws = List.filter (neg ((flip List.contains) completeSteps)) currentSteps
        let ts = updateTimesMap currentSteps times
        let ss = List.filter (fun (req, _) -> not (List.contains req completeSteps)) steps
        findStepOrder (acc + 1) maxWorkers ws ts ss

let input = File.ReadAllLines("./input.txt")
let steps = input |> List.ofSeq |> List.map parseLine |> appendStopConditionSteps
printfn "%d" (steps |> findStepOrder 0 5 [] (makeTimesMap steps))