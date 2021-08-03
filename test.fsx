type Sport = Sport of string
type Discipline = Discipline of string
type Event = Event of string
type FullName = FullName of string
type Country = Country of string
type Gender = Male | Female

type Season = Summer | Winter

type GamesIdentifier =
    {
        Season : Season
        City : string
        Year : int
    }

type GymnasticsDiscipline =
    | ArtisticG

type SportDu =
    | Gymnastics of GymnasticsDiscipline
    | Athletics

type GymasticsEventIndividual =
    | AllAround
    | AllRound

type GymnasticsEvent =
    | Individual of GymasticsEventIndividual option
    | ParallelBars

type AthleticsEvent =
    | Athletics of string

type AllEvents =
    | Athletics of AthleticsEvent
    | Gymnastics of GymnasticsEvent

type SportRecord =
    {
        Sport : Sport
        Discipline : Discipline
        Event : AllEvents
    }

let sr =
    {
        Sport = Sport "Gymnastics"
        Discipline = Discipline "Artistic G."
        Event = Gymnastics (Individual (Some AllRound))
    }

match sr.Event with
| Gymnastics (Individual None) ->
    0
| Gymnastics (Individual (Some AllRound)) ->
    10
| Gymnastics (Individual (Some AllAround)) ->
    0
| Gymnastics (ParallelBars _) ->
    0
| Athletics _ ->
    10

type OlympicResult =
    {
        Id : GamesIdentifier

        Sport : Sport
        Discipline : Discipline
        Event : string

        Athlete : FullName
        Country : Country
        Gender : Gender
    }