## ---- conted-setup ----
rm(list = ls())
library(ggplot2)
library(data.table)
library(magrittr)
load("bin/data/course_records_plus_Dawson.Rdata")
load("bin/data/labelled_students_plus_Dawson.Rdata")
'%NI%' <- function(x,y)!('%in%'(x,y))


## ---- dept-level-sizes-over-time ----
dept_sizes <-courses[section>3000 & CoteR=='E',.N,by=.(ansession,course.dept)]
leave_out <- c('990','520','570','311','242','365','388','607','608','510','410','243','582','574')
ggplot(data = dept_sizes[course.dept %NI% leave_out], aes(x=factor(ansession),y=N))+
  geom_point(aes(color=course.dept))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~course.dept)+
  theme(legend.position = 'none')+
  labs(x='ansession',y='Seats')



## ---- dept-level-sizes-2016 ----
dept_sizes <-courses[section>3000 & CoteR=='E' & as.integer(ansession/10)==2016,.N,by=course.dept]
ggplot(data = courses[section>3000 & CoteR=='E' & as.integer(ansession/10)==2016], aes(course.dept))+
  geom_bar()+
  labs(x='Department',y='Seats in 2016 ContEd')

## ---- big-dept-seat-dist-by-course ----
keep_dept <- c('201','345','602','603')
c2016<-courses[section>3000 & CoteR=='E'
        ][as.integer(ansession/10)==2016
          ][course.dept %in% keep_dept]#,.N,by=.(ansession,course)]
c2016[,course.dept:=substr(course,1,3)]
ggplot(data = c2016,aes(course))+
  geom_bar(aes(fill=course.dept))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(y='Seats in 2016 ContEd')

## ---- lab-dept-seat-dist-by-course-winter ----
keep_dept <- c('101','202','203','420')
clab<-courses[section>3000 & CoteR=='E'
                 ][course.dept %in% keep_dept]#,.N,by=.(ansession,course)]
clab[,course.dept:=substr(course,1,3)]
ggplot(data = clab[ansession>20111][ansession%%10==1],aes(course))+
  geom_bar(aes(fill=course.dept))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~ansession)+
  labs(y='Seats in Winter Term')

## ---- lab-dept-seat-dist-by-course-fall ----
ggplot(data = clab[ansession>20103][ansession%%10==3],aes(course))+
  geom_bar(aes(fill=course.dept))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~ansession)+
  labs(y='Seats in Fall Term')

## ---- lab-dept-seat-dist-by-course-summer ----
ggplot(data = clab[ansession%%10==2],aes(course))+
  geom_bar(aes(fill=course.dept))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~ansession)+
  labs(y='Seats in Summer Term')

## ---- Sexe-demographics-over-time ----
courses[,age:=as.integer(courses$ansession/10)-courses$DateNaissance %>% substr(1,4) %>% as.integer()]

demo<-courses[section>3000 & CoteR=='E',.(mean(age),mean(Note,na.rm = T),.N),by=.(course,ansession,Sexe)]
setnames(demo,'V1','mean_age')
setnames(demo,'V2','mean_grade')

demo[,course.dept:=substr(course,1,3)]
demo<-demo[,.(mean(mean_age),mean(mean_grade,na.rm = T),mean(N)),by=.(course.dept,ansession,Sexe)]
setnames(demo,'V1','mean_age')
setnames(demo,'V2','mean_grade')
setnames(demo,'V3','mean_N')

science<-c(keep_dept,'201','603')
ggplot(data=demo[ansession%%10!=2][ansession>20133][ansession<20171][!is.na(mean_grade)][course.dept %in% science],
       aes(x=mean_age,y=mean_grade))+
  geom_point(aes(color=course.dept,shape=Sexe,size=mean_N))+facet_wrap(~ansession)


## ---- Sexe-demographics-over-time-summer ----
ggplot(data=demo[ansession%%10==2][!is.na(mean_grade)][course.dept %in% science],
       aes(x=mean_age,y=mean_grade))+
  geom_point(aes(color=course.dept,shape=Sexe,size=mean_N))+facet_wrap(~ansession)

## ---- Sexe-demographics ----
demo<-courses[section>3000 & CoteR=='E',.(mean(age),mean(Note,na.rm = T),.N),by=.(course,Sexe)]
setnames(demo,'V1','mean_age')
setnames(demo,'V2','mean_grade')

demo[,course.dept:=substr(course,1,3)]
demo<-demo[,.(mean(mean_age),mean(mean_grade,na.rm = T),mean(N)),by=.(course.dept,Sexe)]
setnames(demo,'V1','mean_age')
setnames(demo,'V2','mean_grade')
setnames(demo,'V3','mean_N')

day_avg<-courses[section<3000 & CoteR=='D',mean(Note,na.rm = T),by=.(Sexe,course.dept)]
setnames(day_avg,'V1','day_avg_note')

science<-c(keep_dept,'201','603')
p<-ggplot(data=demo[!is.na(mean_grade)][course.dept %in% science],
       aes(x=mean_age,y=mean_grade))+
  geom_point(aes(color=course.dept,shape=Sexe,size=mean_N))
p

## ---- Sexe-demographics-day-mean ----
p+geom_hline(data = day_avg[course.dept %in% science],
             mapping = aes(yintercept = day_avg_note,color=course.dept,linetype=Sexe))


## ---- birth-place-demographics-over-time ----
courses[,birth_place:=ifelse(birth_place=='Quebec','Quebec','other')]
demo<-courses[section>3000 & CoteR=='E',.(mean(age),mean(Note,na.rm = T),.N),by=.(course,ansession,birth_place)]
setnames(demo,'V1','mean_age')
setnames(demo,'V2','mean_grade')

demo[,course.dept:=substr(course,1,3)]
demo<-demo[,.(mean(mean_age),mean(mean_grade,na.rm = T),mean(N)),by=.(course.dept,ansession,birth_place)]
setnames(demo,'V1','mean_age')
setnames(demo,'V2','mean_grade')
setnames(demo,'V3','mean_N')

science<-c(keep_dept,'201','603')
ggplot(data=demo[ansession%%10!=2][ansession>20133][ansession<20171][!is.na(mean_grade)][course.dept %in% science],
       aes(x=mean_age,y=mean_grade))+
  geom_point(aes(color=course.dept,shape=birth_place,size=mean_N))+facet_wrap(~ansession)

## ---- birth-place-demographics-over-time-summer ----
ggplot(data=demo[ansession%%10==2][!is.na(mean_grade)][course.dept %in% science],
       aes(x=mean_age,y=mean_grade))+
  geom_point(aes(color=course.dept,shape=birth_place,size=mean_N))+facet_wrap(~ansession)


## ---- birth-place-demographics ----
demo<-courses[section>3000 & CoteR=='E',.(mean(age),mean(Note,na.rm = T),.N),by=.(course,birth_place)]
setnames(demo,'V1','mean_age')
setnames(demo,'V2','mean_grade')

demo[,course.dept:=substr(course,1,3)]
demo<-demo[,.(mean(mean_age),mean(mean_grade,na.rm = T),mean(N)),by=.(course.dept,birth_place)]
setnames(demo,'V1','mean_age')
setnames(demo,'V2','mean_grade')
setnames(demo,'V3','mean_N')

day_avg<-courses[section<3000 & CoteR=='D',mean(Note,na.rm = T),by=.(birth_place,course.dept)]
setnames(day_avg,'V1','day_avg_note')

science<-c(keep_dept,'201','603')
p<-ggplot(data=demo[!is.na(mean_grade)][course.dept %in% science],
          aes(x=mean_age,y=mean_grade))+
  geom_point(aes(color=course.dept,shape=birth_place,size=mean_N))
p

## ---- birth-place-demographics-day-mean ----
p+geom_hline(data = day_avg[course.dept %in% science],
             mapping = aes(yintercept = day_avg_note,color=course.dept,linetype=birth_place))


## ---- langue-demographics-over-time ----
courses[,LangueMaternelle:=ifelse(LangueMaternelle=='AT','AU',LangueMaternelle)]

demo<-courses[section>3000 & CoteR=='E',.(mean(age),mean(Note,na.rm = T),.N),by=.(course,ansession,LangueMaternelle)]
setnames(demo,'V1','mean_age')
setnames(demo,'V2','mean_grade')

demo[,course.dept:=substr(course,1,3)]
demo<-demo[,.(mean(mean_age),mean(mean_grade,na.rm = T),mean(N)),by=.(course.dept,ansession,LangueMaternelle)]
setnames(demo,'V1','mean_age')
setnames(demo,'V2','mean_grade')
setnames(demo,'V3','mean_N')

science<-c(keep_dept,'201','603')
ggplot(data=demo[ansession%%10!=2][ansession>20133][ansession<20171][!is.na(mean_grade)][course.dept %in% science],
       aes(x=mean_age,y=mean_grade))+
  geom_point(aes(color=course.dept,shape=LangueMaternelle,size=mean_N))+facet_wrap(~ansession)

## ---- langue-demographics-over-time-summer ----
ggplot(data=demo[ansession%%10==2][!is.na(mean_grade)][course.dept %in% science],
       aes(x=mean_age,y=mean_grade))+
  geom_point(aes(color=course.dept,shape=LangueMaternelle,size=mean_N))+facet_wrap(~ansession)


## ---- langue-demographics ----
demo<-courses[section>3000 & CoteR=='E',.(mean(age),mean(Note,na.rm = T),.N),by=.(course,LangueMaternelle)]
setnames(demo,'V1','mean_age')
setnames(demo,'V2','mean_grade')

demo[,course.dept:=substr(course,1,3)]
demo<-demo[,.(mean(mean_age),mean(mean_grade,na.rm = T),mean(N)),by=.(course.dept,LangueMaternelle)]
setnames(demo,'V1','mean_age')
setnames(demo,'V2','mean_grade')
setnames(demo,'V3','mean_N')

day_avg<-courses[section<3000 & CoteR=='D',mean(Note,na.rm = T),by=.(LangueMaternelle,course.dept)]
setnames(day_avg,'V1','day_avg_note')

science<-c(keep_dept,'201','603')
p<-ggplot(data=demo[!is.na(mean_grade)][course.dept %in% science],
          aes(x=mean_age,y=mean_grade))+
  geom_point(aes(color=course.dept,shape=LangueMaternelle,size=mean_N))
p

## ---- langue-demographics-day-mean ----
p+geom_hline(data = day_avg[course.dept %in% science],
             mapping = aes(yintercept = day_avg_note,color=course.dept,linetype=LangueMaternelle))



##---- day-conted-same-students ----
load(Sys.getenv('R_path_to_data_directory'))

evening_pass_rates_student<-courses[Note>0][section>3000 & CoteR=='E',
        .(.N,sum(Note>60,na.rm=T),mean(Note,na.rm=T),Sexe,LangueMaternelle), by=.(student_number,course.dept)]
setnames(evening_pass_rates_student,"V2",'num_passed_conted')
setnames(evening_pass_rates_student,"V3",'mean_grade_conted')
setnames(evening_pass_rates_student,"N",'N_conted')
evening_pass_rates_student[,frac_passed_conted:=num_passed_conted/N_conted]

day_pass_rates_student<-courses[Note>0][student_number %in% evening_pass_rates_student$student_number
        ][section <3000 & CoteR=='D',
          .(.N,sum(Note>60,na.rm = T),mean(Note,na.rm=T),Sexe,LangueMaternelle),by=.(student_number,course.dept)]
setnames(day_pass_rates_student,"V2",'num_passed_day')
setnames(day_pass_rates_student,"V3",'mean_grade_day')
setnames(day_pass_rates_student,"N",'N_day')
day_pass_rates_student[,frac_passed_day:=num_passed_day/N_day]

setkey(evening_pass_rates_student,'student_number','course.dept')
setkey(day_pass_rates_student,'student_number','course.dept')
d<-evening_pass_rates_student[day_pass_rates_student,nomatch=0]

hsavg<-etudiant_cours_secondaire[student_number %in% d$student_number,mean(Resultat,na.rm=T),by=student_number]
setnames(hsavg,'V1','hs_avg')
hsavg$hs_avg_c<-cut(hsavg$hs_avg,breaks = seq(70,91,5))
setkey(hsavg,'student_number')
d<-d[hsavg][!is.na(hs_avg_c)]

ggplot(data = d[course.dept %in% science][course.dept!='420'],
       aes(x=mean_grade_conted,y=mean_grade_day))+
  geom_point(aes(color=LangueMaternelle,shape=Sexe),size=1)+
  geom_abline(slope=1,linetype='dashed',color='grey70')+
  facet_grid(hs_avg_c~course.dept)
