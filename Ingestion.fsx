#load "Domain.fsx"
#r "nuget:FSharp.Data"

open Domain
open FSharp.Data

let [<Literal>] SummerPath = __SOURCE_DIRECTORY__ + "/summer.csv"
let [<Literal>] WinterPath = __SOURCE_DIRECTORY__ + "/winter.csv"

type OlympicData = CsvProvider<SummerPath>

module Option =
    let ofString (x:string) =
        if System.String.IsNullOrWhiteSpace x then None
        else Some x

type ErrorDetail =
    | UnknownGender of string
    | UnknownMedal of string

type ParsingError = { RowNumber:int; ErrorDetails : ErrorDetail }

#r "nuget:FsToolkit.ErrorHandling"
open FsToolkit.ErrorHandling

let parseRow season index (row:OlympicData.Row) =
    let extractName (name:string) =
        match name with
        | "Pending" ->
            None
        | name ->
            match name.IndexOf ',' with
            | -1 ->
                Some { FirstName = None; LastName = name.Trim() }
            | index ->
                Some { FirstName = Some (name.[0..index-1].Trim()); LastName = name.[index + 1..].Trim() }

    let gender =
        match row.Gender with
        | "Men" -> Ok Male
        | "Women" -> Ok Female
        | gender -> Error { RowNumber = index; ErrorDetails = UnknownGender gender }

    let medal =
        match row.Medal with
        | "Gold" -> Ok Gold
        | "Silver" -> Ok Silver
        | "Bronze" -> Ok Bronze
        | medal -> Error { RowNumber = index; ErrorDetails = UnknownMedal medal }

    match gender, medal with
    | Ok gender, Ok medal ->
        Ok
            {
                Id =
                    {
                        Season = season
                        City = City row.City
                        Year = row.Year
                    }
                EventCategory =
                    {
                        Sport = Sport row.Sport
                        Discipline = Discipline row.Discipline
                        Event = Event row.Event
                    }
                Athlete =
                    {
                        Name =
                            row.Athlete
                            |> Option.ofString
                            |> Option.bind extractName
                        Country =
                            row.Country
                            |> Option.ofString
                            |> Option.map Country
                        Gender = gender
                    }
                Medal = medal
            }
    | Error unknownGender, Error unknownMedal ->
        Error [ unknownGender; unknownMedal ]
    | Error unknownGender, _ ->
        Error [ unknownGender ]
    | _, Error unknownMedal ->
        Error [ unknownMedal ]


let parseRowR season index (row:OlympicData.Row) = validation {
    let extractName (name:string) =
        match name with
        | "Pending" ->
            None
        | name ->
            match name.IndexOf ',' with
            | -1 ->
                Some { FirstName = None; LastName = name.Trim() }
            | index ->
                Some { FirstName = Some (name.[0..index-1].Trim()); LastName = name.[index + 1..].Trim() }

    let! gender =
        match row.Gender with
        | "Men" -> Ok Male
        | "Women" -> Ok Female
        | gender -> Error { RowNumber = index; ErrorDetails = UnknownGender gender }

    and! medal =
        match row.Medal with
        | "Gold" -> Ok Gold
        | "Silver" -> Ok Silver
        | "Bronze" -> Ok Bronze
        | medal -> Error { RowNumber = index; ErrorDetails = UnknownMedal medal }

    return
        {
            Id =
                {
                    Season = season
                    City = City row.City
                    Year = row.Year
                }
            EventCategory =
                {
                    Sport = Sport row.Sport
                    Discipline = Discipline row.Discipline
                    Event = Event row.Event
                }
            Athlete =
                {
                    Name =
                        row.Athlete
                        |> Option.ofString
                        |> Option.bind extractName
                    Country =
                        row.Country
                        |> Option.ofString
                        |> Option.map Country
                    Gender = gender
                }
            Medal = medal
        }
}














let olympicRows = [|
    yield! OlympicData.GetSample().Rows |> Seq.mapi (fun index row -> parseRowR Summer (index + 2) row)
    yield! OlympicData.Load(WinterPath).Rows |> Seq.mapi (fun index row -> parseRowR Winter (index + 2) row)
|]

olympicRows
|> Array.toList
|> List.sequenceValidationA

(*

CSV -> Rich Domain
SQL -> Rich Domain


*)


