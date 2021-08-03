type Sport = Sport of string
type Discipline = Discipline of string
type Event = Event of string
type FullName = FullName of string
type Country = Country of string
type Gender = Male | Female
type Season = Summer | Winter

type Athlete =
    {
        Name : FullName option
        Country : Country option
        Gender : Gender
    }

type GamesIdentifier =
    {
        Season : Season
        City : string
        Year : int
    }

type EventCategory =
    {
        Sport : Sport
        Discipline : Discipline
        Event : Event
    }

type Medal = Gold | Silver | Bronze

type OlympicResult =
    {
        Id : GamesIdentifier
        EventCategory : EventCategory
        Athlete : Athlete
        Medal : Medal
    }

#r "nuget:FSharp.Data"

open FSharp.Data

let [<Literal>] Sample = __SOURCE_DIRECTORY__ + "/summer.csv"
type Summer = CsvProvider<Sample>
Summer.GetSample().Rows |> Seq.toArray