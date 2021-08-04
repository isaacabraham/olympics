CREATE TABLE [dbo].[Athlete]
(
	[Id] UNIQUEIDENTIFIER NOT NULL PRIMARY KEY, 
    [FirstName] NVARCHAR(50) NULL, 
    [LastName] NVARCHAR(100) NULL, 
    [Gender] CHAR(1) NOT NULL, 
    [Country] NVARCHAR(50) NULL
)
