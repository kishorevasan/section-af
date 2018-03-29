#install relevant packages 
install.packages("wordcloud")
install.packages("ggplot2")
install.packages("waffle")
install.packages("tm")

#load relevant libraries
library(ggplot2)
library(waffle)
library(wordcloud)
library(tm)
library(plyr)

#LOADING DATA
t <- read.csv("Section AF - Survey.csv")
View(t)
colnames(t)
t <- t[,5:10]
colnames(t)<- c("gender","school","infostatus","rexp","gitexp","datascience")

#ORGANIZING DATA

#comnination of gender and infostatus
t1 <- t[,c("gender","infostatus")]
t1 <- as.data.frame(table(t1))

#combination of gender, infostatus and year of school
t2 <- t[,c("gender","infostatus","school")]
t2 <- as.data.frame(count(t2))

#r exp 
t3 <- as.data.frame(table(t$rexp))
t3<- t3%>% mutate(Group = factor(Var1),cumulative = cumsum(Freq),midpoint = cumulative - Freq/2,label = round(Freq/sum(Freq),2))
colnames(t3) <- c("rExp","Freq","Group","Cumulative","Midpoint","Label")

#git exp
t4 <- as.data.frame(table(t$gitexp))
t5<- t4$Freq
names(t5) <- t4$Var1


#VISUALIZATIONSS!

#stacked bar plot
ggplot(data = t)+
  geom_bar(aes(x= gender,fill= infostatus))

#grouped bar plot
ggplot(data = t1,aes(gender,Freq))+
  geom_bar(aes(fill=infostatus),position="dodge",stat="identity")

#facet wrap plot
ggplot(data = t2,aes(gender,freq))+
  geom_bar(aes(fill=infostatus),position="dodge",stat="identity")+
  facet_grid(school ~.)

#pie chart
ggplot(t3,aes(x = factor(1),y = Freq,fill= rExp))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y",start = 0)

#pie chart with percentage values
ggplot(t3, aes(x = 1, weight = Freq, fill = rExp)) +
  geom_bar(width = 1, position = "stack") +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.3, y = Midpoint, label = Label),size = 5) 


#waffle chart
waffle(t5,rows = 8,title= "Git Experience")

#word cloud
myCorpus = Corpus(VectorSource(t$datascience))

myCorpus = tm_map(myCorpus, tolower)
myCorpus = tm_map(myCorpus, removePunctuation)
myCorpus = tm_map(myCorpus, removeNumbers)
myCorpus = tm_map(myCorpus, removeWords, stopwords("english"))

myDTM = TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))

m = as.matrix(myDTM)

v = sort(rowSums(m), decreasing = TRUE)
wordcloud(names(v),v,min.freq = 1)
