## ---- descriptive-stats-setup ----
rm(list=ls())
library(data.table)
library(magrittr)
load("bin/data/course_records_Dawson.Rdata")
load("bin/data/labelled_students_Dawson.Rdata")

#Look to find the grades of the students who dropped as a total average

ave<-courses[,mean(Note),by=.(student_number)]
setnames(ave,'V1','Average')
DO<-courses[,.N,by=.(student_number)]

#Then compare the grade histograms for students who dropped vs students who graduated. Then look at only the ones
#who dropped, but look across time. Are they really different?



## ----
