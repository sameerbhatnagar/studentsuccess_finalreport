## ---- packages ----
library(data.table)
library(magrittr)
library(ggplot2)
library(stringr)
library(pROC)
library(caret)

## ---- demographics-dawson ----
load("bin/data/labelled_students_Dawson.Rdata")
with(students_last_session,table(Sexe,LangueMaternelle)) %>% prop.table() %>%
  round(2) %>% addmargins( )%>% pander::pander()

## ---- demographics-jac ----
load("bin/data/labelled_students_JAC.Rdata")
with(students_last_session,table(Sexe,LangueMaternelle)) %>% prop.table() %>%
  round(2) %>% addmargins( )%>% pander::pander()

## ---- demographics-vanier ----
load("bin/data/labelled_students_Vanier.Rdata")
with(students_last_session,table(Sexe,LangueMaternelle))[,2:4] %>% prop.table() %>%
  round(2) %>% addmargins( )%>% pander::pander()


## ---- descriptive-stats-setup ----
rm(list=ls())
load("bin/data/labelled_students_Dawson.Rdata")
load("bin/data/course_records_Dawson.Rdata")
#Look to find the grades of the students who dropped as a total average

#<<<<<<< HEAD
ave<-courses[,mean(Note,na.rm=T),by=.(student_number)]
setnames(ave,'V1','Average_all_terms')
students_last_session<-merge(ave,students_last_session)

c1<-courses[,mean(Note),by=c("term","student_number")]
c2<-c1[,status:=students_last_session$status[which(student_number == students_last_session$student_number)],by=student_number]
fcount<-courses[,sum(Note<60),by=c("term","student_number")]
setnames(fcount,'V1','F Count')
c2<-merge(fcount,c2)

#Then compare the grade histograms for students who dropped vs students who graduated. Then look at only the ones
#who dropped, but look across time. Are they really different?

## ---- Average comp ----
hist(students_last_session$Average[which(students_last_session$status=="out")],col=rgb(1,0,0,0.5),main="Average Total Grade",ylim=c(0,3500))
hist(students_last_session$Average[which(students_last_session$status=="grad")],col=rgb(0,0,1,0.5),add=T)
legend("topleft",c("Drop-Outs","Graduates"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))
box(which="plot")
#=======
ave<-courses[,mean(Note),by=.(student_number)]
setnames(ave,'V1','Average_all_courses')
setkey(students_last_session,student_number)
setkey(ave,student_number)
students_last_session<-ave[students_last_session]

avg_grade_per_term<-courses[,mean(Note,na.rm=TRUE),by=.(student_number,term)]
setnames(avg_grade_per_term,"V1","avg_grade")
student_term_status<-courses[,.(student_number,term,`current-out`)]
setnames(student_term_status,"current-out","status")
student_term_status[is.na(status),status:='student']

setkey(student_term_status,student_number,term)
student_term_status<-student_term_status %>% unique(by=c('student_number','term'))
setkey(avg_grade_per_term,student_number,term)
avg_grade_per_term<-avg_grade_per_term[student_term_status]

current_students<-avg_grade_per_term[status=='current',student_number] %>% unique()
avg_grade_per_term<-avg_grade_per_term[!(student_number %in% current_students)]
setkey(avg_grade_per_term,'student_number','term')

avg_grade_last_term<-avg_grade_per_term[,.SD[.N],by=student_number]
#Then compare the grade histograms for students who dropped vs students who graduated. Then look at only the ones
#who dropped, but look across time. Are they really different?

## edit by sameer: because data.table has such a major difference in speed between finding
## the last entry of each group, versus the N-x entry of each group,
# we successively remove the 'last term' when trying to find the N-x term
# (the difference in speed is prohibitive as we re-build the entire project everyday)
# so here is for term N-1
setkey(avg_grade_last_term,student_number,term)
setkey(avg_grade_per_term,student_number,term)
tmp<-avg_grade_per_term[!avg_grade_last_term]
avg_grade_last_term_minus1<-tmp[,.SD[.N], by=student_number]
setkey(avg_grade_last_term_minus1,student_number)
setkey(students_last_session,student_number)
avg_grade_last_term_minus1<-students_last_session[avg_grade_last_term_minus1][,.(student_number,avg_grade,term,status,i.status)]

## and here for N-2
tmp_remove<- tmp[,.SD[.N], by=student_number]
setkey(tmp_remove,student_number,term)
setkey(tmp,student_number,term)
tmp2<-tmp[!tmp_remove]

setkey(tmp2,student_number,term)
avg_grade_last_term_minus2<-tmp2[,.SD[.N], by=student_number]
setkey(avg_grade_last_term_minus2,student_number)
setkey(students_last_session,student_number)
avg_grade_last_term_minus2<-students_last_session[avg_grade_last_term_minus2][,.(student_number,avg_grade,term,status,i.status)]


## ---- Average-comp ----
# hist(students_last_session$Average[which(students_last_session$status=="out")],col=rgb(1,0,0,0.5),main="Average Total Grade",ylim=c(0,3500))
# hist(students_last_session$Average[which(students_last_session$status=="grad")],col=rgb(0,0,1,0.5),add=T)
ggplot(data = students_last_session[!is.na(Average_all_courses)], aes(x=Average_all_courses, group=status))+
  geom_histogram(binwidth=5,center=2.5,aes(fill=status))+
  scale_fill_manual(values = c('green','red'))+
  labs(x='Average grade of all courses taken before final semester at college',
       y='Number of Students')+
  ggtitle('Comparing Overall Academic Performance of Students who Graduate, and those who Do Not')

## ---- Average-comp-ttest ----
# t.test(students_last_session$Average[which(students_last_session$status == "grad")],
       # students_last_session$Average[which(students_last_session$status == "out")])
t.test(Average_all_courses ~ status,data=students_last_session)
#>>>>>>> 09d5dad94bfceabd1acdaa6e46b0bbf82e89c757

#Nice to see that the majority of kids who drop have a college average above 60%. Ie they are passing!!!
#Clearly they are different in terms of the average overall grade during their stay.

#Follow up question is whether they had different grades leading up to their last semester or not?
#In other words, let's check the if their grades at the last semester and their grades on their N-1 semesters are diff.
#imagine a plot where you see histograms of 2 colors for DO sem, then another graph for DO-1, then DO-2 and
#the grades keep getting closer

##---- Last-semester-comp ----
# hist(c2[,.SD[.N],by=c("student_number")][status=="out"]$V1,xlab="Grade",col=rgb(1,0,0,0.5),main="Average semester grades on drop-out or graduation semester",ylim=c(0,3500))
# hist(c2[,.SD[.N],by=c("student_number")][status=="grad"]$V1,col=rgb(0,0,1,0.5),add=T)
# legend("topleft",c("Drop-Outs","Graduates"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))
# box(which="plot")

ggplot(data=avg_grade_last_term[!is.na(avg_grade)],aes(x=avg_grade,group=status))+
  geom_histogram(binwidth=5,center=2.5,aes(fill=status))+
  scale_fill_manual(values = c('green','red'))+
  labs(x='Average grade of courses taken during final semester at college',
       y='Number of Students')+
  ggtitle('Comparing Academic Performance of Students who Graduate, and those who Do Notm in Their Final Semester')

## ---- Last-semester-comp-ttest ----
# t.test(c2[,.SD[.N],by=c("student_number")][status=="out"]$V1, c2[,.SD[.N],by=c("student_number")][status=="grad"]$V1)
t.test(avg_grade ~ status,data=avg_grade_last_term)

## ---- N-1-semester-comp ----
# hist(c2[,.SD[.N-1],by=c("student_number")][status=="out"]$V1,xlab="Grade",col=rgb(1,0,0,0.5),main="Average semester grades in 1 semester before drop-out or graduation semester",ylim=c(0,3500))
# hist(c2[,.SD[.N-1],by=c("student_number")][status=="grad"]$V1,col=rgb(0,0,1,0.5),add=T)
# legend("topleft",c("Drop-Outs","Graduates"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))
# box(which="plot")
ggplot(data = avg_grade_last_term_minus1[status!='current'],aes(x=avg_grade, group=status))+
  geom_histogram(binwidth=5,center=2.5,aes(fill=status))+
  scale_fill_manual(values = c('green','red'))+
  labs(x='Average grade of courses taken in the semester just before final semester at college',
       y='Number of Students')+
  ggtitle('Comparing Academic Performance of Students who Graduate, and those who Do Not, in their Second to Last Semester')


## ---- N-1-semester-comp-ttest ----
# t.test(c2[,.SD[.N-1],by=c("student_number")][status=="out"]$V1, c2[,.SD[.N-1],by=c("student_number")][status=="grad"]$V1)
t.test(avg_grade ~ status,data=avg_grade_last_term_minus1)


## ---- N-2-semester-comp ----
# hist(c2[,.SD[.N-2],by=c("student_number")][status=="out"]$V1,xlab="Grade",col=rgb(1,0,0,0.5),main="Average semester grades in 2 semesters before drop-out or graduation semester",ylim=c(0,3500))
# hist(c2[,.SD[.N-2],by=c("student_number")][status=="grad"]$V1,col=rgb(0,0,1,0.5),add=T)
# legend("topleft",c("Drop-Outs","Graduates"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))
# box(which="plot")
ggplot(data = avg_grade_last_term_minus2[status!='current'],aes(x=avg_grade, group=status))+
  geom_histogram(binwidth=5,center=2.5,aes(fill=status))+
  scale_fill_manual(values = c('green','red'))+
  labs(x='Average grade of courses taken two semesters just before final semester at college',
       y='Number of Students')+
  ggtitle('Comparing Academic Performance of Students who Graduate, and those who Do Not, two semesters before their last')


## ---- N-2-semester-comp-ttest ----
# t.test(c2[,.SD[.N-2],by=c("student_number")][status=="out"]$V1, c2[,.SD[.N-2],by=c("student_number")][status=="grad"]$V1)
t.test(avg_grade ~ status,data=avg_grade_last_term_minus2)

## ---- third-semester-attrition-by-program ----
third_term<-courses[term==3]
setnames(third_term,'current-out','status')
third_term[is.na(status),status:='student']
third_term_avg<-third_term[,mean(Note,na.rm = T),by=.(student_number,program)]
setnames(third_term_avg,'V1','avg_grade')
third_term_prog<-third_term[,.(student_number,program,status)] %>% unique(by=c('student_number','program'))
setkey(third_term_avg,'student_number','program')
setkey(third_term_prog,'student_number','program')
third_term_avg<-third_term_prog[third_term_avg]
third_term_avg[status%in%c('grad','current','student'),status:='Not-drop-out']
prog_list<-third_term_avg$program %>% table() %>% sort() %>% tail(13) %>% names()
prog_list<-prog_list[!grepl('out',prog_list)]
third_term_avg[,c('p','s'):=tstrsplit(program,'-')]

ggplot(data = third_term_avg[status!='current'][p%in%prog_list],aes(x=avg_grade, group=status))+
  geom_histogram(binwidth=5,center=2.5,aes(fill=status))+
  scale_fill_manual(values = c('green','red'))+
  facet_wrap(~p)+
  labs(x='Average grade of courses taken in third term',
       y='Number of Students')+
  ggtitle('Comparing Academic Performance of Students who Graduate, and those who Do Not, in their third term of study')


## ---- failed-courses ----
num_failed_per_term<-courses[,.(.N,sum(Note<60,na.rm=TRUE)),by=.(student_number,term)]
setnames(num_failed_per_term,"V2","num_failed")
setnames(num_failed_per_term,'N',"num_courses")
student_term_status<-courses[,.(student_number,term,`current-out`)]
setnames(student_term_status,"current-out","status")
student_term_status[is.na(status),status:='student']

setkey(student_term_status,student_number,term)
student_term_status<-student_term_status %>% unique(by=c('student_number','term'))
setkey(num_failed_per_term,student_number,term)
num_failed_per_term<-num_failed_per_term[student_term_status]

current_students<-num_failed_per_term[status=='current',student_number] %>% unique()
num_failed_per_term<-num_failed_per_term[!(student_number %in% current_students)]
setkey(num_failed_per_term,'student_number','term')
num_failed_per_term[status%in%c('grad','current','student'),status:='Not-drop-out']

num_failed_third_term<-num_failed_per_term[,.SD[3],by=student_number]
with(num_failed_third_term,table(status,num_failed)) %>% pander::pander()

## ---- prop-failed-courses----
ggplot(data = num_failed_third_term[!is.na(status)], aes(y=num_failed/num_courses,x=status))+
  geom_boxplot(aes(fill=status),notch=T)+
  geom_jitter(width=0.1,alpha=1/10)+
  scale_fill_manual(values = c('green','red'))+
  labs(y='Fraction of Courses Failed in Third Term',
       x='Third Term Attrition Status')+
  ggtitle('Comparing Fraction courses failed, for Students who Graduate, and those who Do Not, in their third term of study')


## ---- Class fail comp ----
t.test(c2[,.SD[.N],by=c("student_number")][status=="out"]$`F Count`, c2[,.SD[.N],by=c("student_number")][status=="grad"]$`F Count`)
t.test(c2[,.SD[.N-1],by=c("student_number")][status=="out"]$`F Count`, c2[,.SD[.N-1],by=c("student_number")][status=="grad"]$`F Count`)
t.test(c2[,.SD[.N-2],by=c("student_number")][status=="out"]$`F Count`, c2[,.SD[.N-2],by=c("student_number")][status=="grad"]$`F Count`)
#I am imagining a nice bar graph 2 colors of average classes failed in each term (x axis)
ggplot(c2,aes(x=`F Count`,group=status)) + geom_histogram(bins=8,aes(color=status))
#Kind of like this one but prettier and more informative. Can't tell shit on this one.

#Next step will be to look for students who have great MSE the whole way through and who haven't failed a class.
#Let's figure out the people who leave after X semesters for reasons that aren't academic. How big is that fraction?

## ---- Optimal-Failing-MSE ----
t1<-courses[,sum(result=="Failing",na.rm=T),by=c("term","student_number")]
t2<-t1[,status:=students_last_session$status[which(student_number == students_last_session$student_number)],by=student_number]
fcount<-courses[,sum(result=="Failing",na.rm=T),by=c("term","student_number")]
setnames(fcount,'V1','F Count')
t2<-merge(fcount,t2)

#This is the right plot, but needs work (ggplot) to make it look nice.
hist(t2$V1[which(t2$status=="out")],col=rgb(1,0,0,0.5),main="Number of Failed MSEs",ylim=c(0,20500))
hist(t2$V1[which(t2$status=="grad")],col=rgb(0,0,1,0.5),add=T)
legend("topright",c("Drop-Outs","Graduates"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))
box(which="plot")
t.test(t2$V1[which(t2$status=="out")],t2$V1[which(t2$status=="grad")])
#There is a large difference in mean number of Failing MSEs.

#Let's work backwords to see how the N-x semesters look withtrespect to their MSEs. Copying the code from above

MSETot<-courses[,sum(result=="Failing",na.rm=T),by=.(student_number)]
setnames(MSETot,'V1','Failing MSE-Total')
setkey(students_last_session,student_number)
setkey(MSETot,student_number)
students_last_session<-MSETot[students_last_session]

num_fail_per_term<-courses[,sum(result=="Failing",na.rm=T),by=.(student_number,term)]
setnames(num_fail_per_term,"V1","MSE_term_F")
student_term_status<-courses[,.(student_number,term,`current-out`)]
setnames(student_term_status,"current-out","status")
student_term_status[is.na(status),status:='student']

setkey(student_term_status,student_number,term)
student_term_status<-student_term_status %>% unique(by=c('student_number','term'))
setkey(num_fail_per_term,student_number,term)
num_fail_per_term<-num_fail_per_term[student_term_status]

current_students<-num_fail_per_term[status=='current',student_number] %>% unique()
num_fail_per_term<-num_fail_per_term[!(student_number %in% current_students)]
setkey(num_fail_per_term,'student_number','term')

num_fail_last_term<-num_fail_per_term[,.SD[.N],by=student_number]
#Then compare the grade histograms for students who dropped vs students who graduated. Then look at only the ones
#who dropped, but look across time. Are they really different?
t.test(num_fail_last_term$MSE_term_F[which(num_fail_last_term$status=="out")],num_fail_last_term$MSE_term_F[which(num_fail_last_term$status=="grad")])
#Huge difference here for the last semester:
hist(num_fail_last_term$MSE_term_F[which(num_fail_last_term$status=="out")],col=rgb(1,0,0,0.5),main="Number of Failed MSEs in last term",ylim=c(0,20500))
hist(num_fail_last_term$MSE_term_F[which(num_fail_last_term$status=="grad")],col=rgb(0,0,1,0.5),add=T)
legend("topright",c("Drop-Outs","Graduates"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))
box(which="plot")


setkey(num_fail_last_term,student_number,term)
setkey(num_fail_per_term,student_number,term)
tmp<-num_fail_per_term[!num_fail_last_term]
num_fail_last_term_minus1<-tmp[,.SD[.N], by=student_number]
setkey(num_fail_last_term_minus1,student_number)
setkey(students_last_session,student_number)
num_fail_last_term_minus1<-students_last_session[num_fail_last_term_minus1][,.(student_number,MSE_term_F,term,status,i.status)]
t.test(num_fail_last_term_minus1$MSE_term_F[which(num_fail_last_term_minus1$status=="out")],num_fail_last_term_minus1$MSE_term_F[which(num_fail_last_term_minus1$status=="grad")])

## and here for N-2
tmp_remove<- tmp[,.SD[.N], by=student_number]
setkey(tmp_remove,student_number,term)
setkey(tmp,student_number,term)
tmp2<-tmp[!tmp_remove]

setkey(tmp2,student_number,term)
num_fail_last_term_minus2<-tmp2[,.SD[.N], by=student_number]
setkey(num_fail_last_term_minus2,student_number)
setkey(students_last_session,student_number)
num_fail_last_term_minus2<-students_last_session[num_fail_last_term_minus2][,.(student_number,MSE_term_F,term,status,i.status)]
t.test(num_fail_last_term_minus2$MSE_term_F[which(num_fail_last_term_minus2$status=="out")],num_fail_last_term_minus2$MSE_term_F[which(num_fail_last_term_minus2$status=="grad")])

#Grad = 0, DO = 1
num_fail_last_term$status<-factor(num_fail_last_term$status,labels=c(0,1))
num_fail_last_term_minus1$status<-factor(num_fail_last_term_minus1$status,labels=c(0,1))
num_fail_last_term_minus2$status<-factor(num_fail_last_term_minus2$status,labels=c(0,1))

#Building a model to predict drop outs. Add more parameters than just MSE.
DOfunc<- glm(status ~ MSE_term_F, num_fail_last_term,family="binomial")
summary(DOfunc)
predict(DOfunc, type="response")
num_fail_last_term_Pred <- num_fail_last_term
num_fail_last_term_Pred$Pred <- predict(DOfunc,num_fail_last_term_Pred,type="response")
rocCur<-roc(num_fail_last_term$status,num_fail_last_term_Pred$Pred)
auc(rocCur)
#0.708

#confusionMatrix(as.integer(num_fail_last_term_Pred$Pred>0.6),num_fail_last_term$status)


F1arr<-array(data=NA,dim=5)
xaxis<-c(seq(0,4,1))
temp<-confusionMatrix(as.integer(num_fail_last_term$MSE_term_F==0),num_fail_last_term$status)
F1arr[1]<-temp$overall[1]
temp<-confusionMatrix(as.integer(num_fail_last_term$MSE_term_F==1),num_fail_last_term$status)
F1arr[2]<-temp$overall[1]
temp<-confusionMatrix(as.integer(num_fail_last_term$MSE_term_F==2),num_fail_last_term$status)
F1arr[3]<-temp$overall[1]
temp<-confusionMatrix(as.integer(num_fail_last_term$MSE_term_F==3),num_fail_last_term$status)
F1arr[4]<-temp$overall[1]
temp<-confusionMatrix(as.integer(num_fail_last_term$MSE_term_F>4),num_fail_last_term$status)
F1arr[5]<-temp$overall[1]



F1arr2<-array(data=NA,dim=5)
temp<-confusionMatrix(as.integer(num_fail_last_term_minus1$MSE_term_F==0),num_fail_last_term_minus1$status)
F1arr2[1]<-temp$overall[1]
temp<-confusionMatrix(as.integer(num_fail_last_term_minus1$MSE_term_F==1),num_fail_last_term_minus1$status)
F1arr2[2]<-temp$overall[1]
temp<-confusionMatrix(as.integer(num_fail_last_term_minus1$MSE_term_F==2),num_fail_last_term_minus1$status)
F1arr2[3]<-temp$overall[1]
temp<-confusionMatrix(as.integer(num_fail_last_term_minus1$MSE_term_F==3),num_fail_last_term_minus1$status)
F1arr2[4]<-temp$overall[1]
temp<-confusionMatrix(as.integer(num_fail_last_term_minus1$MSE_term_F>4),num_fail_last_term_minus1$status)
F1arr2[5]<-temp$overall[1]

F1arr3<-array(data=NA,dim=5)
temp<-confusionMatrix(as.integer(num_fail_last_term_minus2$MSE_term_F==0),num_fail_last_term_minus2$status)
F1arr3[1]<-temp$overall[1]
temp<-confusionMatrix(as.integer(num_fail_last_term_minus2$MSE_term_F==1),num_fail_last_term_minus2$status)
F1arr3[2]<-temp$overall[1]
temp<-confusionMatrix(as.integer(num_fail_last_term_minus2$MSE_term_F==2),num_fail_last_term_minus2$status)
F1arr3[3]<-temp$overall[1]
temp<-confusionMatrix(as.integer(num_fail_last_term_minus2$MSE_term_F==3),num_fail_last_term_minus2$status)
F1arr3[4]<-temp$overall[1]
temp<-confusionMatrix(as.integer(num_fail_last_term_minus2$MSE_term_F>4),num_fail_last_term_minus2$status)
F1arr3[5]<-temp$overall[1]

## ---- Optimal-Failing-MSE-prop----
plot(xaxis, F1arr,xlab='# Failing MSEs in Last Term',ylab='F1 Score')
plot(xaxis, F1arr2,xlab='# Failing MSEs in Before Last Term',ylab='F1 Score')
plot(xaxis, F1arr3,xlab='# Failing MSEs 2 Terms before DO',ylab='F1 Score')


#Could also look at grades that students ended up getting after receiving a failing MSE. Based on past performance
#is this a natural path for them.

#Next step is to merge
#Try to merge all MSE tables for last semester, minus 1 and minus 2 to see if you can get trends.
#For example, when starting last semester, do the grades and MSE from previous semesters help predict the DO rate
# of semester n+1. There is probably additional predictive power to just the MSE of the current semester.
#Simply stacking the MSEs should lead to an increase predictability ratio. Obviously adding grades should help.


#Merging all the avg semester grades with the number of failing MSEs for that term.
setkey(num_fail_last_term,student_number)
setkey(avg_grade_last_term,student_number)
avg_grade_MSE_last_term<-merge(num_fail_last_term,avg_grade_last_term)

setkey(num_fail_last_term_minus1,student_number)
setkey(avg_grade_last_term_minus1,student_number)
avg_grade_MSE_last_term_minus1<-merge(num_fail_last_term_minus1,avg_grade_last_term_minus1)

setkey(num_fail_last_term_minus2,student_number)
setkey(avg_grade_last_term_minus2,student_number)
avg_grade_MSE_last_term_minus2<-merge(num_fail_last_term_minus2,avg_grade_last_term_minus2)


#Looking for a confusion matrix with the F1 score that is high based on past grades and MSEs.


num_fail_last_2terms<-merge(avg_grade_MSE_last_term,avg_grade_MSE_last_term_minus1,all.x=FALSE,all.y=FALSE)
#num_fail_last_2terms$MSE_doub<-factor(num_fail_last_2terms$MSE_term_F.x >0 | num_fail_last_2terms$avg_grade.y<70,labels=c(0,1))
num_fail_last_2terms$MSE_doub<-factor(num_fail_last_2terms$MSE_term_F.y >1,labels=c(0,1))
tmp<-confusionMatrix(num_fail_last_2terms$MSE_doub,num_fail_last_2terms$status.x.x)
tmp$byClass

setkey(avg_grade_MSE_last_term_minus2,student_number)

num_fail_last_f2<-merge(avg_grade_MSE_last_term_minus1,avg_grade_MSE_last_term_minus2,all.x=F,all.y=F)
num_fail_last_f2$MSE_doub<-factor(num_fail_last_f2$MSE_term_F.x >0 | num_fail_last_f2$avg_grade.y<70,labels=c(0,1))
tmp<-confusionMatrix(num_fail_last_f2$MSE_doub,num_fail_last_f2$status.x.x)
tmp$byClass
tmp

#Build the model with time series information, then predict, then confusion Matrix to compare the JAC way vs the
#purely academic data way. Can still add the demographic admissions data and such.

#Need to start moving now to the prediction of a student vs time of stay. So Day 1 prediction, then after the first
#batch of MSEs, then after first batch of final grades. We would be predicting 2 things: quit next term and quit ever.

#Could also add all the other schools.


