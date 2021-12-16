library(ggplot2)


#load the dataset

titanic <- read.csv("titanic.csv")

head(titanic)

#generate bar charts
par(mfrow=c(3,1))
gender_counts <- table(titanic$Sex)
barplot(height=gender_counts, main="barplot of gender", col=c(1,2))

class_counts <- table(titanic$Pclass)
barplot(height=class_counts, main="barplot of ticket class", col=c(3,4,5))

survival_counts <- table(titanic$Survived)
barplot(height=survival_counts, main="barplot of survival", col=c(6,7))

#histogram of passenger age
par(mfrow=c(2,2))
par(mar=c(0.5,4.5,0.5,0.5))
boxplot(titanic$Age~titanic$Survived, xlab="Age", ylab="Survived", col=c("red","green"), 
        horizontal=TRUE, axes=FALSE)
legend("topright", legend=c("yes","no"),col=c("green","red"),pt.cex=(2),pch=15)
plot(0, type="n",xlab="",ylab="", axes=FALSE)
par(mar=c(4.5,4.5,0.5,0.5))
hist(titanic$Age, xlab="Age", ylab="Count of Age", main="")
par(mar=c(4.5,0.5,0.5,0.5))
boxplot(titanic$Age~titanic$Pclass, xlab="Age", ylab="Ticket Class", horizontal=TRUE)

#histogram of travel fare
par(mfrow=c(1,1))
par(mar=c(4,4,4,4))
hist(titanic$Fare, xlab="Travel Fare", col="light blue")
#table of people who did not pay
dnp <- sum(titanic$Fare == 0)
paid <- length(titanic$Fare) - dnp
tablepaid <- as.table(rbind(c(dnp,paid)))
dimnames(tablepaid) <- list(c("Number of People"),c("Did not Pay","Paid"))
tablepaid

#family size per ticket class
titanic$famsize <- titanic$SibSp+titanic$Parch+1
famsiz <- as.data.frame(titanic$famsize)
ggplot(famsiz)+
  geom_bar(aes(x=titanic$Pclass))

#stacked bar charts
ggplot(titanic) + aes(x=Pclass, fill=Sex)+
  geom_bar()+
  facet_grid(~Survived)

#violin chart of survival related to age and gender
ggplot(titanic) +
  aes(y= Age, x= Sex, fill = Sex) +
  geom_violin() +
  facet_grid(~Survived)

#violin of survival rate related to age and ticket class
ggplot(titanic) +
  aes(y=Age, x=Pclass, fill = Sex) +
  geom_violin()+
  facet_grid(~Survived)
