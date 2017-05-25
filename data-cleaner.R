rm(list=ls())
library(data.table)
library(magrittr)
library(stringr)

# path.to.data.directory <- '~/Documents/science_program/onGoingEval/CLARA_database_mirror/DawsonCollege/'
path.to.data.directory <- 'C:/Work/JAC/PAREA/Parea Grant 2015-2018/Data/2017/Dawson/24-05-2017/'

## ---- etudiant ----
clara.table<-'Etudiant'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

lines<-paste0(path.to.data.directory,clara.table,'.txt') %>% readLines()
bad.rows <- which(stringr::str_count(lines,',') == 8)
for (i in 1:length(bad.rows)){
  chartorem<-gregexpr(pattern =',',lines[bad.rows[i]])[[1]][5]
  halfstr<-substr(lines[bad.rows[i]],0,chartorem-1)
  halfstr2<-substr(lines[bad.rows[i]],chartorem+1,stringr::str_length(lines[bad.rows[i]]))
  lines[bad.rows[i]]<-stringr::str_c(halfstr,halfstr2,sep="")
}
li<-paste(lines,collapse='\n')
write(li,paste0(path.to.data.directory,'EtudiantClean1.txt'),ncolumns=8)

etudiant <-fread(paste0(path.to.data.directory,'EtudiantClean1','.txt'),sep = ',',col.names = fmt$V7)

## ---- admission ----
clara.table<-'Admission'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

admission <-fread(paste0(path.to.data.directory,clara.table,'.txt'),sep = ',',
                  col.names = fmt$V7)

## ---- etudiant-session ----
clara.table<-'EtudiantSession'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

etudiant_session <-fread(paste0(path.to.data.directory,clara.table,'.txt'),sep = ',',
                        col.names = fmt$V7,drop=nrow(fmt))
#etudiant_session <-fread(paste0(path.to.data.directory,clara.table,'.txt'),sep = ',',
 #                        col.names = fmt$V7)

## ---- cours ----
clara.table<-'Cours'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

cours <-fread(paste0(path.to.data.directory,clara.table,'.txt'),sep = ',',
              col.names = fmt$V7)

## ---- inscription ---- 
clara.table<-'Inscription'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

lines<-paste0(path.to.data.directory,clara.table,'.txt') %>% readLines()
li<-paste(lines[seq(1,length(lines),2)],collapse='\n')
write(li,paste0(path.to.data.directory,'Inscription1Clean','.txt'),ncolumns=18)

inscription <-fread(paste0(path.to.data.directory,clara.table,'.txt'),sep = ',',
                    col.names = fmt$V7,drop = nrow(fmt))

#inscription <-fread(paste0(path.to.data.directory,clara.table,'1Clean.txt'),sep = ',',
#                    col.names = fmt$V7)

## ---- student-certification ----
clara.table<-'StudentCertification'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

student_certification <-fread(paste0(path.to.data.directory,clara.table,'.txt'),sep = ',',
                              col.names = fmt$V7)
setnames(student_certification,'AnSession','ansession')

## ---- certification-type ----
clara.table<-'CertificationType'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

certification_type <-fread(paste0(path.to.data.directory,clara.table,'.txt'),sep = ',',
                           col.names = fmt$V7)

## ---- evaluation-etudiant ----
clara.table<-'EvaluationEtudiant'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

evaluation_etudiant <-fread(paste0(path.to.data.directory,clara.table,'.txt'),sep = ',',
                            col.names = fmt$V7)
setnames(evaluation_etudiant,'AnSession','ansession')
setnames(evaluation_etudiant,'IdGroupe','IDGroupe')

## ---- etudiant-cours-secondaire ----
clara.table<-'EtudiantCoursSecondaire'
fmt<-fread(paste0(path.to.data.directory,clara.table,'.fmt'))

lines<-paste0(path.to.data.directory,clara.table,'.txt') %>% readLines()
bad.rows <- which(stringr::str_count(lines,',') >= 18)
for (i in 1:length(bad.rows)){
  count<-stringr::str_count(lines[bad.rows[i]],',')
  if(count==18){
    chartorem<-gregexpr(pattern =',',lines[bad.rows[i]])[[1]][17]
    halfstr<-substr(lines[bad.rows[i]],0,chartorem-1)
    halfstr2<-substr(lines[bad.rows[i]],chartorem+1,stringr::str_length(lines[bad.rows[i]]))
    lines[bad.rows[i]]<-stringr::str_c(halfstr,halfstr2,sep="")
  }
  else if(count==19){
    chartorem<-gregexpr(pattern =',',lines[bad.rows[i]])[[1]][17]
    chartorem2<-gregexpr(pattern =',',lines[bad.rows[i]])[[1]][18]
    thirdstr<-substr(lines[bad.rows[i]],0,chartorem-1)
    thirdstr2<-substr(lines[bad.rows[i]],chartorem+1,chartorem2-2)
    thirdstr3<-substr(lines[bad.rows[i]],chartorem2+1,stringr::str_length(lines[bad.rows[i]]))
    lines[bad.rows[i]]<-stringr::str_c(thirdstr,thirdstr2,thirdstr3,sep="")
  }
}
li<-paste(lines,collapse='\n')

write(li,paste0(path.to.data.directory,'EtudiantCoursSecondaireClean1','.txt'),ncolumns=18)
etudiant_cours_secondaire <-fread(paste0(path.to.data.directory,'EtudiantCoursSecondaireClean1','.txt'),sep = ',',
                                  col.names = fmt$V7)

rm(clara.table,fmt,bad.rows,chartorem,chartorem2,count,halfstr,halfstr2,thirdstr,thirdstr2,thirdstr3,li,lines,i)

save.image(file = paste0(path.to.data.directory,'student_success_Dawson-25-05-17.RData'))