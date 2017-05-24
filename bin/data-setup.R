## ---- load-data ----
rm(list=ls())

path.to.data.directory<-"~/Desktop/StudentSuccessClean/"
NOW = 20171
load(paste0(path.to.data.directory,'student_success_Triple.RData'))
earliest.ansession<-20103

library(data.table)
library(magrittr)
library(knitr)
library(ggplot2)
subdir<-paste0(getwd())
'%NI%' <- function(x,y)!('%in%'(x,y))

## ---- choose-college ----
colleges<-c('Dawson','Vanier','JAC')
for (college in colleges){
admission<-get(paste0('admission.',college))
etudiant_session<-get(paste0('etudiant_session.',college))
inscription<-get(paste0('inscription.',college))
cours<-get(paste0('cours.',college))
certification_type<-get(paste0('certification_type.',college))
student_certification<-get(paste0('student_certification.',college))
evaluation_etudiant<-get(paste0('evaluation_etudiant.',college))
etudiant_cours_secondaire<-get(paste0('etudiant_cours_secondaire.',college))
etudiant<- get(paste0('etudiant.',college))


# keep admission records for only those who started or ended after Fall 2010
admission.recent<-admission[ansessionDebut>=earliest.ansession,
                            .(student_number,ansessionDebut,program,population,speAdmission)]
student_certification.recent<-student_certification[ansession>=earliest.ansession]
setkey(student_certification.recent,student_number,ansession)
student_certification.recent<-student_certification.recent %>% unique()

# remove session IDs from inscription that have only NA's as Note (unless current semester!)
current.sessions<-etudiant_session[ansession==NOW,IDEtudiantSession]
setkey(inscription,IDEtudiantSession)
inscription[,num.na:=sum(is.na(Note)),by=IDEtudiantSession]
inscription[,num.courses:=.N,by=IDEtudiantSession]
inscription.clean.old<-inscription[IDEtudiantSession %NI% current.sessions][num.courses!=num.na]
inscription.current<-inscription[IDEtudiantSession %in% current.sessions]
inscription.clean<-rbind(inscription.clean.old,inscription.current)
rm(current.sessions,inscription.clean.old,inscription.current)

# there are still inscription records which have the same IDGroupe and IDEtudiantSession
# ( half of which will be Note=NA and IndicateurSupprime=1, while the other half have
# Note = some real number and IndicateurSupprime=0)
# inscription.clean$IndicateurSupprime %>% table(useNA='always')
# table(inscription.clean$IndicateurSupprime,is.na(inscription.clean$Note),useNA = 'always')
# table(inscription.clean$IndicateurSupprime,inscription.clean$IndicateurCoursSuivi)
d<-inscription.clean[IndicateurSupprime==1][is.na(Note),IDInscription]
setkey(inscription.clean,IDInscription)
inscription.clean<-inscription.clean[IDInscription %NI% d]

setkey(inscription.clean,IDEtudiantSession,IDGroupe)
d<-inscription.clean[inscription.clean %>% duplicated() %>% which()]
d<-inscription.clean[IDEtudiantSession %in% d$IDEtudiantSession][IndicateurSupprime==1,IDInscription]
inscription.clean<-inscription.clean[IDInscription %NI% d]

# keep only session ids which are for recent students and have a course attached
sessions<-etudiant_session[student_number %in% admission.recent$student_number
                           ][ansession>=earliest.ansession,
                             .(IDEtudiantSession,student_number,program,ansession)]
sessions<-sessions[IDEtudiantSession %in% inscription.clean$IDEtudiantSession]


setkey(sessions,student_number,ansession)
# x<-sessions[sessions %>% duplicated() %>% which()]
# sessions[student_number==x$student_number[1]]$IDEtudiantSession
# inscription.clean[IDEtudiantSession==491578]
# inscription.clean[IDEtudiantSession==471795]
sessions<-unique(sessions)

# label terms
setkey(sessions,student_number,ansession)
sessions[,term:=seq(.N),by=student_number]
setnames(sessions,'program','profile.code')

## ---- map profile.code to program.code----
admits<-admission.recent[,.(student_number,program)]
grads<-student_certification.recent[,.(student_number,program)]
admits %>% setkey('student_number')
grads %>% setkey('student_number')
mappings<-grads[admits]
mappings %>% setnames('i.program','profile.code')
mappings<-mappings[!is.na(program)]
mappings %>% setkey('program','profile.code')
mappings[,cat:=paste(program,profile.code,sep='-')]
mappings<-mappings$cat %>% table() %>% sort(decreasing=T) %>% data.table()
mappings %>% setnames('.','code')
mappings[,c('program','profile'):=tstrsplit(code,'-')]
transitions<-mappings[substr(profile,1,1)=='0'][,program:='transition']
mappings<-mappings[!substr(profile,1,1)=='0']

mappings<-mappings[substr(profile,1,3)==substr(program,1,3),]
mappings %>% setkey('program','profile')
mappings<-mappings %>% unique()

changeups<-mappings[substr(profile,1,3)=='570'|substr(profile,1,3)=='410'][substr(profile,1,4)==substr(program,1,4)]
mappings<-rbind(mappings[!substr(profile,1,3)=='570'|substr(profile,1,3)=='410'],changeups)
mappings<-mappings[order(N,decreasing = T)][N>max(N)*0.01][,.(program,profile)]
mappings<-rbind(mappings,transitions[,.(program,profile)])
new_profiles=data.table(profile=c('500G1','500G2','500G3','500G4','500G5','500G6'),program=rep('500A1',6))
mappings<-rbind(mappings,new_profiles)
new_profiles=data.table(profile=c('200B5','300AD','410C0','500AE','500AF'),program=c('200B0','300A0','410C0','500A1','500A1'))
mappings<-rbind(mappings,new_profiles)


setkey(mappings,profile)
mappings<-mappings[!duplicated(mappings)]
setkey(sessions,'profile.code')
sessions<-mappings[sessions]

## ---- label-students ----
sessions %>% setkey(student_number,ansession)
sessions %>% duplicated() %>% which()

last.term<-sessions[,.SD[.N],by=student_number]
setkey(last.term,student_number,ansession)
sessions<-sessions[!last.term]
last.term.minus.1<-sessions[,.SD[.N],by=student_number]

finishers<-last.term[student_certification.recent,nomatch=0
                     ][,.(student_number,IDEtudiantSession,program,ansession,term)]
finishers[,program:=paste0(program,'-grad')]
finishers %>% setkey(student_number,ansession)
finishers[,profile:='-']

current<-last.term[ansession==NOW]
current[,program:=paste0(program,'-current')]
current %>% setkey(student_number,ansession)
current %>% duplicated() %>% which

# students who, in their last session record, do not appear in certification table,
# nor are they current students, must have dropped out
quitters<-last.term[student_number %NI% current$student_number][student_number %NI% finishers$student_number]
# unless they went certified from another college
other_colleges=colleges[colleges!=college]

o.g<-get(paste0('student_certification.',other_colleges[1]))$student_number
quitters<-quitters[!intersect(quitters,o.g)]

o.g<-get(paste0('student_certification.',other_colleges[2]))$student_number
quitters<-quitters[!intersect(quitters,o.g)]

quitters[,program:=paste0(program,'-out')]
quitters %>% setkey(student_number,ansession)
quitters %>% duplicated() %>% which

sessions<-rbind(sessions,finishers,current,quitters)
setkey(sessions,student_number,ansession)
d<-sessions[sessions %>% duplicated() %>% which()]
# sessions[student_number == d[23]$student_number]
sessions<-unique(sessions,fromLast = T)

sessions %>% setkey(student_number,ansession)
sessions %>% duplicated() %>% which()
sessions.cast<-sessions[,.(student_number,program,term)] %>% dcast.data.table(student_number ~ term,value.var='program')

# sessions %>% duplicated() %>% which()
# sessions[,.SD[.N],by=student_number][,program] %>% table()

## ---- build-transcripts-for-all-students ----
groups<-cours[,.(IDGroupe,course,section)]
setkey(groups,IDGroupe)
setkey(inscription.clean,IDGroupe)
courses<-groups[inscription.clean][,.(IDInscription,IDEtudiantSession,IDGroupe,course,Note,MoyenneGroupeEvaluation,section,CoteR)]

setkey(courses,IDEtudiantSession,IDGroupe)
# d<-courses[courses %>% duplicated() %>% which()]

setkey(sessions,IDEtudiantSession)
setkey(courses,IDEtudiantSession)

courses<-sessions[courses,nomatch=0]
setkey(courses,student_number,ansession)

setkey(evaluation_etudiant,student_number,IDGroupe)
setkey(courses,student_number,IDGroupe)

# d<-courses[courses %>% duplicated() %>% which()]
# courses[student_number==d$student_number[1]]

courses<-evaluation_etudiant[,.(student_number,IDGroupe,result)][courses]
setkey(courses,student_number,ansession,course)
# d<-courses[courses %>% duplicated() %>% which()]
# courses[student_number==d[2]$student_number]
courses<-unique(courses,fromLast = T)

## ---- build-model-matrix-to-predict-drop-out-following-term ----
last.term.minus.1 %>% setkey(student_number)
quitters %>% setkey(student_number)

last.term.minus.1.quitters<-last.term.minus.1[quitters][,.(student_number,ansession)]
last.term.minus.1.quitters[,quit.next.term:=1]
setkey(last.term.minus.1.quitters,student_number,ansession)
setkey(courses,student_number,ansession)
courses<-last.term.minus.1.quitters[courses]
courses[is.na(quit.next.term),quit.next.term:=0]
setkey(courses,student_number,ansession)

# table(courses$quit.next.term,courses$program)

courses[,course.dept:=substr(course,1,3)]
courses[,c('prog','current-out'):=tstrsplit(program,'-')]

# append demographic data
demo<-etudiant[,.(student_number,Sexe,birth_place,LangueMaternelle)]
demo %>% setkey('student_number')
courses %>% setkey('student_number')
courses<-demo[courses]

students_last_session<-rbind(finishers,quitters,current)
students_last_session<-demo[students_last_session]
students_last_session[,c('program','status'):=tstrsplit(program,'-')]



save(courses,
     file = paste0('bin/data/course_records_',college,'.Rdata'))
save(students_last_session,
     file = paste0('bin/data/labelled_students_',college,'.Rdata'))
}
rm(list=ls())


