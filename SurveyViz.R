library(ggplot2)
t <- read.csv("Blank Quiz.csv")
View(t)

ggplot(data = t)+
  geom_bar(aes(x= Gender,fill= Informatics.))
