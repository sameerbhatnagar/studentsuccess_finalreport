create view [dbo].[vwAdmission]
as
select  
a.idAdmission
,HASHBYTES('SHA2_256',  CONVERT(nvarchar(4000),e.numero)) student_number,
a.spe, a.speAdmission, a.ansessionDebut, a.ansessionFin,a.DateHeureRetrait Withdrawal_date, a.TypeAdmission,a.NoTour,a.reforme,a.cohorte,a.population--,a.DateHeure,Indicateur
,a.IndicateurReadmission,es.program
from [ClaraReportProdLink].[CLARA_DAW_PROD].[Admissions].[Admission] a
inner join [ClaraReportProdLink].[CLARA_DAW_PROD].Etudiants.Etudiant e
on e.IDEtudiant=a.IDEtudiant

inner join (select  es.idAdmission, p.numero program,es.ansession
					, ROW_NUMBER() over (partition by es.idAdmission  order by es.ansession desc) ID
			from   [ClaraReportProdLink].[CLARA_DAW_PROD].[Etudiants].[EtudiantSession] es
			inner join [ClaraReportProdLink].[CLARA_DAW_PROD].[Programmes].[Programme] p
			on p.idProgramme=es.idProgramme
			where es.etat=1
			and es.[IDUniteOrg]=265
			and es.ansession>20050	  
			) es
on es.idAdmission=a.idAdmission
and es.ID=1
where a.[IDUniteOrg]=265
and a.ansessionDebut >20050
GO


create view [dbo].[vwCours]
as
select g.[IDGroupe],c.numero course, g.numero section,c.[PonderationTheo],c.[PonderationLab],c.[PonderationPersonnel],c.[Credite],c.[SeuilPassageNoteFinale]
,g.[NbEtudiantsPremierJourClasse],g.[NbEtudiantsAuRecensement],g.[MoyenneGroupeEvaluation]
from [ClaraReportProdLink].[CLARA_DAW_PROD].[BanqueCours].[Cours] c
inner join [ClaraReportProdLink].[CLARA_DAW_PROD].[Groupes].[Groupe] g
on g.[IDCours]=c.[IDCours]
where exists (select 1 from [ClaraReportProdLink].[CLARA_DAW_PROD].[Inscriptions].[Inscription] i
			  inner join [ClaraReportProdLink].[CLARA_DAW_PROD].[Etudiants].[EtudiantSession] es
			  on es.IDEtudiantSession=i.IDEtudiantSession
			  where es.ansession >20050
			  and es.[IDUniteOrg]=265
			  and i.[IDGroupe]=g.[IDGroupe])
GO



create view [dbo].[vwCoursSecondaire]
as
select  [IDCoursSecondaire],[Numero],[NumeroValidationPrealable],[Titre],[NbUnites],[Seuil],[TypeMoyenneSpecifique],[PointsBonis],[DomaineFormation],[SecteurEnseignement]
from [ClaraReportProdLink].[CLARA_DAW_PROD].[Reference].[CoursSecondaire]
GO

create view [dbo].[vwEtudesPrecedentes]
as
select ea.idEtudeAnterieure,
	 HASHBYTES('SHA2_256',  CONVERT(nvarchar(4000),e.numero)) student_number,
     ea.[EtatEtudeSecondaire],
	 ea.[EtatEtudeSecondaireAdulte],
	 ea.[EtatEtudeCollegial],
	 ea.[EtatEtudeUniversitaire]
from [ClaraReportProdLink].[CLARA_DAW_PROD].[Etudiants].[EtudeAnterieure] ea
inner join [ClaraReportProdLink].[CLARA_DAW_PROD].Etudiants.Etudiant e
on e.IDEtudiant=ea.IDEtudiant
where exists (select 1 from [dbo].[vwEtudiant] et
              where et.student_number = HASHBYTES('SHA2_256',  CONVERT(nvarchar(4000),e.numero)))
GO


create view [dbo].[vwEtudiant]
as
select  HASHBYTES('SHA2_256',  CONVERT(nvarchar(4000),e.numero)) student_number
		,e.DateNaissance
		,e.Sexe
		,e.LangueMaternelle
		,isNull(cp.NomANG,e.VilleNaissance) birth_place--e.CodeLieuNaissance
		,e.IndicateurDeficienceFonctionnelleMajeure
		,e.CoteR
		,ca.CodePostal
		
from [ClaraReportProdLink].[CLARA_DAW_PROD].Etudiants.Etudiant e
left join  [ClaraReportProdLink].[CLARA_DAW_PROD].Etudiants.EtudiantAdresse ee
ON ee.IDEtudiant = e.IDEtudiant AND ee.TypeAdresse = 1 
left join [ClaraReportProdLink].[CLARA_DAW_PROD].Coordonnees.Adresse  ca 
ON ca.IDAdresse = ee.IDAdresse
left join [ClaraReportProdLink].[CLARA_DAW_PROD].[Coordonnees].[Pays] cp
on cp.IDPays=e.CodeLieuNaissance
where  exists (select 1 from  [ClaraReportProdLink].[CLARA_DAW_PROD].Etudiants.EtudiantSession es
				where es.IDEtudiant=e.IDEtudiant
				and es.AnSession > 20050
				and es.Etat=1
				and es.IDUniteOrg =265)
GO


CREATE view [dbo].[vwEtudiantCoursSecondaire]
as
select  
ecs.IDEtudiantCoursSecondaire
,HASHBYTES('SHA2_256',  CONVERT(nvarchar(4000),e.numero)) student_number
, ecs.[IDCoursSecondaire],ecs.[Resultat],ecs.[CodeRemarque],ecs.[IndicateurReussi],ecs.Etat,ecs.[Annee],ecs.session
,ecs.[RangCinquieme],ecs.[RangCentile],ecs.[MoyenneGroupe],ecs.[RegimePedagogiqueCours],ecs.[TypeFormation],ecs.[Origine],ecs.[IndicateurSupprime]
,o.Titre School, o.type School_Type
from [ClaraReportProdLink].[CLARA_DAW_PROD].[Etudiants].[EtudiantCoursSecondaire] ecs
inner join  [ClaraReportProdLink].[CLARA_DAW_PROD].[Reference].[Organisme] o
on o.IDOrganisme=ecs.IDOrganisme
inner join [ClaraReportProdLink].[CLARA_DAW_PROD].Etudiants.Etudiant e
on e.IDEtudiant=ecs.IDEtudiant
and exists (select 1 from [ClaraReportProdLink].[CLARA_DAW_PROD].[Etudiants].[EtudiantSession] es
			where es.etat=1
			and es.[IDUniteOrg]=265
			and es.ansession>20050
			and es.IDEtudiant=ecs.IDEtudiant)
GO


CREATE view [dbo].[vwEtudiantSession]
as
select es.[IDEtudiantSession]
,HASHBYTES('SHA2_256',  CONVERT(nvarchar(4000),e.numero)) student_number
,p.numero program,es.ansession
,es.Etat,es.SPE,es.[TypeFrequentation],es.[IndicateurCommandite],es.[IndicateurFinissant],es.[IndicateurFinissantCalcule],es.[Moyenne]
,uo.Numero division
from [ClaraReportProdLink].[CLARA_DAW_PROD].[Etudiants].[EtudiantSession] es
inner join [ClaraReportProdLink].[CLARA_DAW_PROD].[Programmes].[Programme] p
on p.[IDProgramme]=es.IDProgramme
inner join [ClaraReportProdLink].[CLARA_DAW_PROD].Etudiants.Etudiant e
on e.IDEtudiant=es.IDEtudiant
inner join [ClaraReportProdLink].[CLARA_DAW_PROD].[Reference].[UniteOrg] uo
on uo.IDUniteOrg=es.IDUniteOrg
where es.ansession >20050
and es.[IDUniteOrg]=265
GO



CREATE view [dbo].[vwInscription]
as
select i.[IDInscription],
i.IDEtudiantSession, i.[IDGroupe],i.[TypeRAF],i.[ModeEnseignementDistance],i.[Ponderation],i.[NbAbsences],i.[Langue],i.[MoyenneGroupeEvaluation]
,i.Etat,i.[IndicateurCoursSuivi],i.[IndicateurCoursInscrit],i.IndicateurSupprime, i.DateHeureSynchronisationMinistere
,i.[MoyenneNotesRetenuesCoteR],i.[CodeRemarque],i.Note,i.[CoteR]
,uo.Numero division
from [ClaraReportProdLink].[CLARA_DAW_PROD].[Inscriptions].[Inscription] i
inner join  [ClaraReportProdLink].[CLARA_DAW_PROD].[Etudiants].[EtudiantSession] es
on es.IDEtudiantSession=i.IDEtudiantSession
inner join [ClaraReportProdLink].[CLARA_DAW_PROD].[Reference].[UniteOrg] uo
on uo.IDUniteOrg=i.IDUniteOrg
where es.ansession >20050
and es.[IDUniteOrg]=265

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

create view [dbo].[vwEvaluationEtudiant]
as
select
HASHBYTES('SHA2_256',  CONVERT(nvarchar(4000),e.numero)) student_number
, ee.AnSession,ee.IdGroupe
, cmi.titre result
from [ClaraReportProdLink].[CLARA_DAW_PROD].[EvaluationMiSession].[EvaluationEtudiant]  ee
left join [ClaraReportProdLink].[CLARA_DAW_PROD].[Reference].[CoteMiSession] cmi
on cmi.idCoteMiSession=ee.idCoteMiSession
inner join [ClaraReportProdLink].[CLARA_DAW_PROD].Etudiants.Etudiant e
on e.IDEtudiant=ee.IDEtudiant
where  exists (select 1 from  [ClaraReportProdLink].[CLARA_DAW_PROD].Etudiants.EtudiantSession es
                           where es.IDEtudiant=e.IDEtudiant
                           and es.AnSession > 20050
                           and es.Etat=1
                           and es.IDUniteOrg =265)

