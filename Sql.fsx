#load "Olympics.Dal.fsx"
      "Domain.fsx"
open Microsoft.Data.SqlClient
open Domain

let connectionString = "Server=localhost\\sqlexpress;Initial Catalog=Olympics;Integrated Security=true"

let conn = new SqlConnection (connectionString)
let cmd = new SqlCommand("SELECT * FROM dbo.Athlete", conn)
let reader = cmd.ExecuteReader()
let r = DataAccess.dbo.HydraReader(reader)
let athletes =
    [
        while reader.NextResult() do
            {
                Name =
                    r.Athlete.LastName.Read()
                    |> Option.map(fun lastName ->
                        {
                            FirstName = r.Athlete.FirstName.Read()
                            LastName = lastName
                        }
                    )
                Country =
                    r.Athlete.Country.Read()
                    |> Option.map Country
                Gender = failwith "Not Implemented"
            }

    ]