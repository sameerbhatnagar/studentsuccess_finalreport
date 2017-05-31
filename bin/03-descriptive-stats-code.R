## ---- descriptive-stats-setup ----
rm(list=ls())
library(data.table)
library(magrittr)
library(ggplot2)
load("bin/data/course_records_Dawson.Rdata")
load("bin/data/labelled_students_Dawson.Rdata")

#Look to find the grades of the students who dropped as a total average

ave<-courses[,mean(Note),by=.(student_number)]
setnames(ave,'V1','Average')
students_last_session<-merge(ave,students_last_session)

#Then compare the grade histograms for students who dropped vs students who graduated. Then look at only the ones
#who dropped, but look across time. Are they really different?

t.test(students_last_session$Average[which(students_last_session$status == "grad")],
       students_last_session$Average[which(students_last_session$status == "out")])

hist(students_last_session$Average[which(students_last_session$status=="out")])
#Nice to see that the majority of kids who drop have a college average above 60%. Ie they are passing!!!
#Clearly they are different in terms of the average overall grade during their stay.

#Follow up question is whether they had different grades leading up to their last semester or not?
#In other words, let's check the if their grades at the last semester and their grades on their N-1 semesters are diff.
#imagine a plot where you see histograms of 2 colors for DO sem, then another graph for DO-1, then DO-2 and
#the grades keep getting closer.

c1<-courses[,mean(Note),by=c("term","student_number")]
c2<-c1[,status:=students_last_session$status[which(student_number == students_last_session$student_number)],by=student_number]

#add a column to c1 that is the final status of that student. Get it from looking it up in the students table

hist(c2[,.SD[.N],by=c("student_number")][status=="out"]$V1)

t.test(c2[,.SD[.N],by=c("student_number")][status=="out"]$V1, c2[,.SD[.N],by=c("student_number")][status=="grad"]$V1)

#Next step will be to look for students who have great MSE the whole way through and who haven't failed a class.
#Let's figure out the people who leave after X semesters for reasons that aren't academic. How big is that fraction?



## ----
