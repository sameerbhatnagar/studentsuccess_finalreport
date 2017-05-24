## ---- comparisons-setup ----
rm(list=ls())
library(data.table)
library(magrittr)
load("bin/data/course_records_Dawson.Rdata")
load("bin/data/labelled_students.Rdata")
na=courses[,sum(is.na(result)|result=='Not applicable'),by=.(student_number,ansession)]
setnames(na,'V1','num_na')
pass=courses[,sum(result=='Passing',na.rm = T),by=.(student_number,ansession)]
setnames(pass,'V1','num_pass')
risk=courses[,sum(result=='At risk',na.rm = T),by=.(student_number,ansession)]
setnames(risk,'V1','num_at_risk')
fail=courses[,sum(result=='Failing',na.rm = T),by=.(student_number,ansession)]
setnames(fail,'V1','num_failing')
c=courses[,.N,by=.(student_number,ansession)]
setnames(c,'N','num_courses')

setkey(risk,student_number,ansession)
setkey(na,student_number,ansession)
setkey(pass,student_number,ansession)
setkey(fail,student_number,ansession)

setkey(c,student_number,ansession)
midterms=pass[risk][na][fail][c]

setkey(students_last_session,student_number,ansession)
last_term=midterms[students_last_session][status!='current']
last_term$birth_place<-ifelse(last_term$birth_place=='Quebec','Quebec','other')
last_term$status01<-ifelse(last_term$status=='out',1,0)

## ---- model-last-term-status ----
fit=glm(status01 ~ num_pass + num_at_risk + num_failing + num_courses +
          Sexe + birth_place + LangueMaternelle,
        data = last_term,
        family = 'binomial')
fit %>% summary() %>% pander::pander()
# predict(fit,newdata = last_term[ansession==20161])


## ---- simulate-ss-module ----
n=2
t=table(last_term$num_at_risk+last_term$num_failing>n,last_term$status)
recall=t[2,2]/sum(t[,2])
precision=(t[1,1]+t[2,2])/sum(t)
false_pos = t[2,1]
recall
precision
false_pos


n=2
t=table(last_term[term==3]$num_at_risk+last_term[term==3]$num_failing>n,last_term[term==3]$status)
recall=t[2,2]/sum(t[,2])
precision=(t[1,1]+t[2,2])/sum(t)
false_pos = t[2,1]
recall
precision
false_pos
