USE [StudentSuccess]
GO

/****** Object:  Table [dbo].[StudentCertification]    Script Date: 5/24/2017 4:54:28 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

SET ANSI_PADDING ON
GO

CREATE TABLE [dbo].[StudentCertification](
	[student_number] [varbinary](8000) NULL,
	[IDType] [int] NULL,
	[AnSession] [smallint] NOT NULL,
	[program] [varchar](5) NOT NULL,
	[division] [varchar](10) NULL
) ON [PRIMARY]

GO

SET ANSI_PADDING OFF
GO


