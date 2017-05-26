## ---- conted-setup ----
rm(list = ls())
library(ggplot2)
library(data.table)
load("bin/data/course_records_Dawson_plus.Rdata")
load("bin/data/labelled_students_Dawson_plus.Rdata")
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


## ---- Sexe-dept-ansession ----
c_s<-courses[section>3000 & CoteR=='E',.N,by=.(course.dept,ansession,Sexe)]
c_s[,course.dept:=substr(course,1,3)]
ggplot(data = c_s[course.dept %NI% leave_out], aes(x=factor(ansession),y=N,group=Sexe))+
  geom_line(aes(color=Sexe))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~course.dept)+
  theme(legend.position = 'none')+
  labs(x='ansession',y='Seats')



## ---- num-courses-per-student ----
# disagregated by success rates
