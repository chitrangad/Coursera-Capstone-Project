setwd("C:\\Users\\swapn\\Documents\\Coursera Capstone Project\\final\\en_US\\") 
filelist <- list.files(getwd(),full.names = FALSE)

con1 <- file("en_US.twitter.txt", "r") 
twitter<- readLines(con1) 
close(con1)
summary(nchar(twitter))

con2 <- file("en_US.news.txt", "r") 
news<- readLines(con2) 
close(con2)
summary(nchar(news))

con3 <- file("en_US.blogs.txt", "r") 
blogs<- readLines(con3) 
close(con3)
summary(nchar(blogs))

love<-length(grep("love",twitter))
hate<-length(grep("hate",twitter))
fraction<-love/hate
bio<-grep("biostats", twitter,value=T)
length(grep("A computer once beat me at chess, but it was no match for me at kickboxing", twitter))
