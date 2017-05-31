## ---- descriptive-stats-setup ----
rm(list=ls())
library(data.table)
library(magrittr)
library(ggplot2)
load("bin/data/course_records_Dawson.Rdata")
load("bin/data/labelled_students_Dawson.Rdata")

#Look to find the grades of the students who dropped as a total average

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

t.test(students_last_session$Average[which(students_last_session$status == "grad")],
       students_last_session$Average[which(students_last_session$status == "out")])
#Nice to see that the majority of kids who drop have a college average above 60%. Ie they are passing!!!
#Clearly they are different in terms of the average overall grade during their stay.

#Follow up question is whether they had different grades leading up to their last semester or not?
#In other words, let's check the if their grades at the last semester and their grades on their N-1 semesters are diff.
#imagine a plot where you see histograms of 2 colors for DO sem, then another graph for DO-1, then DO-2 and
#the grades keep getting closer

##---- Last semester comp ----
hist(c2[,.SD[.N],by=c("student_number")][status=="out"]$V1,xlab="Grade",col=rgb(1,0,0,0.5),main="Average semester grades on drop-out or graduation semester",ylim=c(0,3500))
hist(c2[,.SD[.N],by=c("student_number")][status=="grad"]$V1,col=rgb(0,0,1,0.5),add=T)
legend("topleft",c("Drop-Outs","Graduates"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))
box(which="plot")

t.test(c2[,.SD[.N],by=c("student_number")][status=="out"]$V1, c2[,.SD[.N],by=c("student_number")][status=="grad"]$V1)

## ---- N-1 semester comp ----
hist(c2[,.SD[.N-1],by=c("student_number")][status=="out"]$V1,xlab="Grade",col=rgb(1,0,0,0.5),main="Average semester grades in 1 semester before drop-out or graduation semester",ylim=c(0,3500))
hist(c2[,.SD[.N-1],by=c("student_number")][status=="grad"]$V1,col=rgb(0,0,1,0.5),add=T)
legend("topleft",c("Drop-Outs","Graduates"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))
box(which="plot")

t.test(c2[,.SD[.N-1],by=c("student_number")][status=="out"]$V1, c2[,.SD[.N-1],by=c("student_number")][status=="grad"]$V1)

## ---- N-2 semester comp ----
hist(c2[,.SD[.N-2],by=c("student_number")][status=="out"]$V1,xlab="Grade",col=rgb(1,0,0,0.5),main="Average semester grades in 2 semesters before drop-out or graduation semester",ylim=c(0,3500))
hist(c2[,.SD[.N-2],by=c("student_number")][status=="grad"]$V1,col=rgb(0,0,1,0.5),add=T)
legend("topleft",c("Drop-Outs","Graduates"),fill=c(rgb(1,0,0,0.5),rgb(0,0,1,0.5)))
box(which="plot")

t.test(c2[,.SD[.N-2],by=c("student_number")][status=="out"]$V1, c2[,.SD[.N-2],by=c("student_number")][status=="grad"]$V1)

## ---- Class fail comp ----
t.test(c2[,.SD[.N],by=c("student_number")][status=="out"]$`F Count`, c2[,.SD[.N],by=c("student_number")][status=="grad"]$`F Count`)
t.test(c2[,.SD[.N-1],by=c("student_number")][status=="out"]$`F Count`, c2[,.SD[.N-1],by=c("student_number")][status=="grad"]$`F Count`)
t.test(c2[,.SD[.N-2],by=c("student_number")][status=="out"]$`F Count`, c2[,.SD[.N-2],by=c("student_number")][status=="grad"]$`F Count`)
#I am imagining a nice bar graph 2 colors of average classes failed in each term (x axis)
ggplot(c2,aes(x=`F Count`,group=status)) + geom_histogram(bins=8,aes(color=status))
#Kind of like this one but prettier and more informative. Can't tell shit on this one.

#Next step will be to look for students who have great MSE the whole way through and who haven't failed a class.
#Let's figure out the people who leave after X semesters for reasons that aren't academic. How big is that fraction?

## ----
