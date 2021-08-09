#load "Domain.fsx"
#r "nuget:FSharp.Data"
#r "nuget:FsToolkit.ErrorHandling"

open FsToolkit.ErrorHandling
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

type ParsingError = { RowNumber:int; Season : Season; ErrorDetails : ErrorDetail }

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
        | gender -> Error { RowNumber = index; Season = season; ErrorDetails = UnknownGender gender }

    let medal =
        match row.Medal with
        | "Gold" -> Ok Gold
        | "Silver" -> Ok Silver
        | "Bronze" -> Ok Bronze
        | medal -> Error { RowNumber = index; Season = season; ErrorDetails = UnknownMedal medal }

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
        | gender -> Error { RowNumber = index; Season = season; ErrorDetails = UnknownGender gender }

    and! medal =
        match row.Medal with
        | "Gold" -> Ok Gold
        | "Silver" -> Ok Silver
        | "Bronze" -> Ok Bronze
        | medal -> Error { RowNumber = index; Season = season; ErrorDetails = UnknownMedal medal }

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









let olympicRows = [
    yield! OlympicData.GetSample().Rows |> Seq.mapi (fun index row -> parseRowR Summer (index + 2) row)
    yield! OlympicData.Load(WinterPath).Rows |> Seq.mapi (fun index row -> parseRowR Winter (index + 2) row)
]

let successes =
    match List.sequenceValidationA olympicRows with
    | Ok results -> results
    | Error _ -> []

let whoWon city year event gender results =
    results
    |> List.filter(fun r ->
        r.Id.City = city
        && r.Athlete.Gender = gender
        && r.Medal = Gold
        && r.Id.Year = year
        && r.EventCategory.Event = event)

successes
|> whoWon (City "London") (2012) (Event "Hockey") Male



// Structure not clearly defined - exploratory phase
// Overkill - not worth the effort at this point

let results =
    successes
    |> List.map (fun r -> {| Name = r.Athlete.Name; Event = r.EventCategory.Event; Id = r.Id; Medal = r.Medal |})





let crossJoinResult =
    let olympicsGames =
        [ "Lake Placid", 1932
          "Sochi", 2014
          "St.Moritz", 1928 ]

    let events =
        [ "Ice Hockey"; "Slalom"; "Giant Slalom" ]

    let allGamesAndEvents = [
        for game in olympicsGames do
        for event in events do
            game, event
    ]

    [
        for ((city, year), event) in allGamesAndEvents do
            successes |> whoWon (City city) year (Event event) Female
    ]

let successesForCountry =
    successes
    |> List.choose(fun r -> r.Athlete.Country |> Option.map (fun (Country c) -> {| Id = r.Id; Country = c; EventCategory = r.EventCategory; Medal = r.Medal |}))
    |> List.distinct
    |> List.sortBy (fun r -> r.Country, r.Id.Year)

#r "nuget: Plotly.NET, 2.0.0-preview.6"
open Plotly.NET

// Simple example - calculate over last n years
let calculateData currentYear age =
    successesForCountry
    |> List.filter (fun r -> r.Medal = Gold)
    |> List.filter (fun r -> currentYear - r.Id.Year > age)
    |> List.countBy (fun r -> r.Country)
    |> List.sortByDescending snd
    |> List.take 10

calculateData 2021 20
|> Chart.Column
|> Chart.Show

// More complex example - get running totals to see progress

/// This function will count up rows denoted by year and present running totals for each year
let runningTotal rowsForCountry =
    let byCountry = rowsForCountry |> List.countBy id

    byCountry
    |> List.scan (fun count (year, rows) -> count + rows) 0
    |> List.skip 1
    |> List.zip byCountry
    |> List.map(fun (a, b) -> {| Year = a |> fst |> snd; Medals = b |})

// This groups up by country and then gets running totals
let runningTotals =
    successesForCountry
    |> List.map(fun r -> r.Country, r.Id.Year)
    |> List.groupBy fst
    |> List.map (fun (key, rows) -> key, rows |> runningTotal)

// Get top ten based on total medals over all time
let topTenCountries =
    runningTotals
    |> List.sortByDescending(fun (_, years) -> years |> List.last |> fun x -> x.Medals)
    |> List.take 10

// Create a line series for each country
let series = [
    for country, rows in topTenCountries do
        Chart.Line (rows |> List.map(fun r -> string r.Year, r.Medals), country)
]

// Combine all series into a single chart
series
|> Chart.Combine
|> Chart.Show

// //let topTenCountries = ??
//     //let scoreMedal = ??

// (*

//     1   COUNTRY     SCORE
//     2   COUNTRY     SCORE


// //let bestEventForCountries

//     COUNTRY     EVENT
//     COUNTRY     EVENT

// //athletes per year by gender

//     YEAR    # MALE EVENTS  # FEMALE EVENTS
//     YEAR    # MALE EVENTS  # FEMALE EVENTS

// *)





// (*

// CSV -> Rich Domain
// SQL -> Rich Domain

// *)


