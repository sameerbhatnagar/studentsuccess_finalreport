USE [StudentSuccess]
GO

/****** Object:  View [dbo].[vwStudentCertification]    Script Date: 5/24/2017 4:55:13 PM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO

CREATE view [dbo].[vwStudentCertification]
as
select HASHBYTES('SHA2_256',  CONVERT(nvarchar(4000),e.numero)) student_number
	,se.IDTypeSanction IDType , se.AnSession, p.Numero program
	, case when se.idUniteOrg in (265,555,984,1011) then uo.Numero  end  division
from [ClaraReportProdLink].[CLARA_DAW_PROD].[Sanctions].[SanctionEtudiant] se
inner join [ClaraReportProdLink].[CLARA_DAW_PROD].[Programmes].[Programme] p
on p.[IDProgramme]=se.IDProgramme
inner join [ClaraReportProdLink].[CLARA_DAW_PROD].[Reference].[UniteOrg] uo
on uo.idUniteOrg=se.idUniteOrg
inner join [ClaraReportProdLink].[CLARA_DAW_PROD].Etudiants.Etudiant e
on e.idEtudiant=se.idEtudiant
inner join dbo.Etudiant e1
on e1.student_number = HASHBYTES('SHA2_256',  CONVERT(nvarchar(4000),e.numero)) 
GO


