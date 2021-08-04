type Sport = Sport of string
type Discipline = Discipline of string
type Event = Event of string
type FullName = { FirstName : string option; LastName : string }
type Country = Country of string
type Gender = Male | Female
type Season = Summer | Winter
type City = City of string

type Athlete =
    {
        Name : FullName option
        Country : Country option
        Gender : Gender
    }

type GamesIdentifier =
    {
        Season : Season
        City : City
        Year : int
    }

type EventCategory =
    {
        Sport : Sport
        Discipline : Discipline
        Event : Event
    }

type Medal = Gold | Silver | Bronze

type OlympicScore =
    {
        Id : GamesIdentifier
        EventCategory : EventCategory
        Athlete : Athlete
        Medal : Medal
    }