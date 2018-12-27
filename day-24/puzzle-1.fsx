open System
open System.IO
open System.Text.RegularExpressions

type Fraction = 
    | ImmuneSystem
    | Infection

type UnitGroup = {
    Id:int;
    Fraction:Fraction;
    Count:int;
    Health:int;
    Weaknesses:list<string>;
    Immunities:list<string>;
    Damage:int;
    DamageType:string;
    Initiative:int
}

let trimBy (tr:array<char>) (s:string) = s.Trim(tr)
let splitBy (spl:string) (s:string) = s.Split([| spl |], StringSplitOptions.None)
let substr n (s:string) = s.Substring(n)

let parseWeaknessesAndImmunities (line:string) =
    let parseAttackTypes o s = s |> substr o |> splitBy ", " |> List.ofArray
    let sentences = line |> trimBy [| '('; ' '; ')' |] |> splitBy "; " |> List.ofArray |> List.sortBy id
    match sentences with
    | [ "" ] -> [], []
    | [ im ] when im.StartsWith("immune to ") -> [], parseAttackTypes 10 im
    | [ wk ] when wk.StartsWith("weak to ") -> parseAttackTypes 8 wk, []
    | [ im; wk ] -> parseAttackTypes 8 wk, parseAttackTypes 10 im
    | _ -> failwith "Failed to parse the unit."

let parseLine i f line = 
    let regexMatch = Regex.Match(line, "^(\\d+) units each with (\\d+) hit points(\\D*) with an attack that does (\\d+) (\\D+) damage at initiative (\\d+)$")
    let group f = (fun (i:int) -> (f regexMatch.Groups.[i].Value))
    let wks, ims = parseWeaknessesAndImmunities (group id 3)
    {
        Id = i; 
        Fraction = f; 
        Count = group int 1;
        Health = group int 2; 
        Weaknesses = wks; 
        Immunities = ims;
        Damage = group int 4;
        DamageType = group id 5;
        Initiative = group int 6
    }

let parse input =
    let ims, ifs = 
        input 
        |> List.ofArray 
        |> List.splitAt (Array.findIndex (fun s -> s = "") input)
    let immuneSystemGroups = ims |> List.tail |> List.mapi (fun i l -> parseLine i Fraction.ImmuneSystem l)
    let infectionGroups = ifs |> List.skip 2 |> List.mapi (fun i l -> parseLine (i + List.length immuneSystemGroups) Fraction.Infection l)
    List.append immuneSystemGroups infectionGroups

let getEffectivePower ug = ug.Damage * ug.Count

let getDamageOutput ug ug' =
    match ug.DamageType with
    | dt when List.contains dt ug'.Immunities -> 0
    | dt when List.contains dt ug'.Weaknesses -> (getEffectivePower ug) * 2
    | _ -> getEffectivePower ug

let findMostDamagedUnit ug ugs =
    match ugs with
    | [] -> (ug, None, 0)
    | _ ->
        let ug', dmg =
            ugs
            |> List.map (fun ug' -> ug', getDamageOutput ug ug')
            |> List.sortBy (fun (ug', dmg) -> -dmg, -getEffectivePower ug', -ug'.Initiative)
            |> List.head
        match dmg with
        | 0 -> (ug, None, 0)
        | dmg -> (ug, Some ug', dmg)

let getUnitGroupSelection ugs ug =
    let enemyUgs = List.filter (fun ug' -> ug'.Fraction <> ug.Fraction) ugs
    let ug, ug', dmg = findMostDamagedUnit ug enemyUgs
    match ug' with
    | None -> (ug.Id, None, ug.Initiative), ugs
    | Some ug' -> (ug.Id, Some ug'.Id, ug.Initiative), List.filter (fun ug'' -> ug'' <> ug') ugs

let attackUnitGroup ug ug' =
    let dmg = getDamageOutput ug ug'
    let newCount = max 0 (ug'.Count - dmg / ug'.Health)
    {
        Id = ug'.Id;
        Fraction = ug'.Fraction;
        Count = newCount;
        Health = ug'.Health;
        Weaknesses = ug'.Weaknesses;
        Immunities = ug'.Immunities;
        Damage = ug'.Damage;
        DamageType = ug'.DamageType;
        Initiative = ug'.Initiative
    }

let attack ugs (id, id', _) =
    match id' with
    | None -> ugs
    | Some id' ->
        let attacker = List.tryFind (fun ug -> ug.Id = id) ugs
        let defender = List.tryFind (fun ug -> ug.Id = id') ugs
        match attacker, defender with
        | _, None -> ugs
        | None, _ -> ugs
        | Some attacker, Some defender ->
            let defender' = attackUnitGroup attacker defender
            let ugs' = List.filter (fun ug -> ug.Id <> defender'.Id) ugs
            match defender'.Count with
            | 0 -> ugs'
            | _ -> List.append ugs' (List.singleton defender')

let fight ugs =
    let ugs' = List.sortBy (fun ug -> -getEffectivePower ug, -ug.Initiative) ugs
    let selections = List.mapFold getUnitGroupSelection ugs ugs' |> fst |> List.sortBy (fun (_, _, i) -> -i)
    List.fold attack ugs selections

let getWinningFraction ugs =
    match ugs |> List.map (fun ug -> ug.Fraction) |> List.distinct with
    | [ f ] -> Some f
    | _ -> None

let rec findOutcome ugs =
    let ugs' = fight ugs
    match getWinningFraction ugs' with
    | Some _ -> List.sumBy (fun ug -> ug.Count) ugs'
    | None when ugs = ugs' -> -1
    | None -> findOutcome ugs'

let input = File.ReadAllLines("./input.txt")
printfn "%A" (input |> parse |> findOutcome)