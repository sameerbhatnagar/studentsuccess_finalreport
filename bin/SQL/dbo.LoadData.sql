ALTER procedure [dbo].[LoadData]
------------------------------------------
-- A.Likht 2016-June-10
--  output files
----------------------------------------------
-- A.Likht 2016- March-30

-- step 1  delete data from all tables
-- step 2  insert data from view to tables 
--  a. [dbo].[Etudiant]
--  b. [dbo].[Admission]
--  c. [dbo].[EtudiantSession]
--  d. [dbo].[Cours]
--  e. [dbo].[EtudesPrecedentes]
--  f. [dbo].[Inscription]
--  g. [dbo].[CoursSecondaire]
--  h. [dbo].[EtudiantCoursSecondaire]
--------------------------------------------
as
begin
	declare @str varchar(200)

--------------------------------------------
-- step 1  delete data from all tables
--------------------------------------------

   delete from [dbo].[EtudesPrecedentes]
   delete from [dbo].[Admission]
   delete from [dbo].[EtudiantCoursSecondaire]
   delete from [dbo].[CoursSecondaire]
   delete from [dbo].[Inscription]
   delete from [dbo].[Cours]
   delete from [dbo].[EtudiantSession]
   delete from [dbo].[Etudiant]
   delete from [dbo].[CertificationType]
   delete from [dbo].[EvaluationEtudiant]
--------------------------------------------
-- step 2 insert data from view to tables
--------------------------------------------   


	insert into [dbo].[CertificationType]
	select * 
	from [ClaraReportProdLink].[CLARA_DAW_PROD].Reference.TypeSanction


	set @str='bcp  "select * from StudentSuccess.dbo.CertificationType" queryout  E:\StudentSuccess\CertificationType.txt  -T -f E:\StudentSuccess\CertificationType.fmt'  
	exec xp_cmdshell @str

	insert into dbo.Etudiant
	select * from [dbo].[vwEtudiant]

	set @str='bcp  "select * from StudentSuccess.dbo.Etudiant" queryout  E:\StudentSuccess\Etudiant.txt  -T -f E:\StudentSuccess\Etudiant.fmt'  
	exec xp_cmdshell @str

	insert into [dbo].[Admission]
	select va.* from dbo.vwAdmission va
	inner join [dbo].[Etudiant] a
	on a.student_number=va.student_number

	set @str='bcp  "select * from StudentSuccess.dbo.vwAdmission" queryout  E:\StudentSuccess\Admission.txt  -T -f E:\StudentSuccess\Admission.fmt'  
	exec xp_cmdshell @str

	insert into dbo.EtudiantSession
	select ve.* from [dbo].[vwEtudiantSession] ve
	inner join [dbo].[Etudiant] e
	on e.student_number=ve.student_number

	
    	set @str='bcp  "select * from StudentSuccess.dbo.vwEtudiantSession" queryout  E:\StudentSuccess\EtudiantSession.txt  -T -f E:\StudentSuccess\EtudiantSession.fmt'  
	exec xp_cmdshell @str

	insert into dbo.Cours
	select * from dbo.vwCours

	
	set @str='bcp  "select * from StudentSuccess.dbo.vwCours" queryout  E:\StudentSuccess\Cours.txt  -T -f E:\StudentSuccess\Cours.fmt'  
	exec xp_cmdshell @str

	insert into [dbo].[EtudesPrecedentes]
	select ve.*
	from [dbo].vwEtudesPrecedentes ve
	inner join [dbo].[Etudiant] e
	on e.student_number=ve.student_number

	
	set @str='bcp  "select * from StudentSuccess.dbo.vwEtudesPrecedentes" queryout  E:\StudentSuccess\EtudesPrecedentes.txt  -T -f E:\StudentSuccess\EtudesPrecedentes.fmt'  
	exec xp_cmdshell @str

	insert into [dbo].[Inscription]
	select vi.* from [dbo].[vwInscription] vi
	inner join [dbo].[EtudiantSession] es
	on es.IDEtudiantSession=vi.IDEtudiantSession
	inner join [dbo].[Cours] c
	on c.IDGroupe=vi.IDGroupe

	
	set @str='bcp  "select * from StudentSuccess.dbo.Inscription" queryout  E:\StudentSuccess\Inscription.txt  -T -f E:\StudentSuccess\Inscription.fmt'  
	exec xp_cmdshell @str

	insert into dbo.CoursSecondaire
	select * from [dbo].[vwCoursSecondaire]

	
	set @str='bcp  "select * from StudentSuccess.dbo.vwCoursSecondaire" queryout  E:\StudentSuccess\CoursSecondaire.txt  -T -f E:\StudentSuccess\CoursSecondaire.fmt'  
	exec xp_cmdshell @str

	insert into dbo.EtudiantCoursSecondaire
	select ve.* from [dbo].[vwEtudiantCoursSecondaire]  ve
	inner join [dbo].[Etudiant] e
	on e.student_number=ve.student_number
	inner join [dbo].[CoursSecondaire] cs
	on cs.[IDCoursSecondaire]=ve.IDCoursSecondaire

	
	set @str='bcp  "select * from StudentSuccess.dbo.EtudiantCoursSecondaire" queryout  E:\StudentSuccess\EtudiantCoursSecondaire.txt  -T -f E:\StudentSuccess\EtudiantCoursSecondaire.fmt'  
	exec xp_cmdshell @str


	set @str='bcp  "select * from StudentSuccess.dbo.vwStudentCertification" queryout  E:\StudentSuccess\StudentCertification.txt  -T -f E:\StudentSuccess\StudentCertification.fmt'  
	exec xp_cmdshell @str
 


	insert into dbo.EvaluationEtudiant
    	select * from  dbo.vwEvaluationEtudiant

	
	set @str='bcp  "select * from StudentSuccess.dbo.EvaluationEtudiant" queryout  E:\StudentSuccess\EvaluationEtudiant.txt  -T -f E:\StudentSuccess\EvaluationEtudiant.fmt'  
	exec xp_cmdshell @str

	set @str='E:\StudentSuccess\copy_txtfiles.bat'
	exec xp_cmdshell @str

end


GO


