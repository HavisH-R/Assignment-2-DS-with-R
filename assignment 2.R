library(tidyverse)
library(rvest)
library(dplyr)
library(imager)
library(stringr)
library(MASS)

#1
data("iris")
setosa <- iris[which(iris$Species == "setosa"),]
versicolor <- iris[which(iris$Species == "versicolor"),]
virginica <- iris[which(iris$Species == "virginica"),]
par(mfrow = c(1,3))
boxplot(setosa$Sepal.Length,setosa$Sepal.Width,setosa$Petal.Length,setosa$Petal.Width,
        names = c("Sepal Length", "Sepal Width","Petal Length","Petal Width"),
        main = "Boxplot of Setosa species ")
boxplot(versicolor$Sepal.Length,versicolor$Sepal.Width,versicolor$Petal.Length,versicolor$Petal.Width,
        names = c("Sepal Length", "Sepal Width","Petal Length","Petal Width"),
        main = "Boxplot of Versicolor species")

boxplot(virginica$Sepal.Length,virginica$Sepal.Width,virginica$Petal.Length,virginica$Petal.Width,
        names = c("Sepal Length", "Sepal Width","Petal Length","Petal Width"),
        main = "Boxplot of Virginica species")

plot(iris$Sepal.Length, iris$Petal.Length,
     xlab = "Sepal Length", ylab = "Petal Length",main = "ScatterPlot of Sepal Length and Petal Length", col = iris$Species,pch=16)


legend("topright", legend = c("Setosa", "versicolor","virginica"), pch =16,col=c(1,2,3))



#2
  flip <- function(image){
  col.mat <- as.array(image[, ,1, ])
  
  dims <- dim(col.mat)
  rot <- array(0, dim = dims)
  
  for(i in 1:dims[1])
  {
    for(j in 1:dims[2])
    {
      rot[i, j, ] <- col.mat[dims[1] - i + 1, j, ]
    }
  }
  return(as.cimg(rot))
  
  }
  dog<-load.image("dog.jpeg")
  plot(flip(dog))
  


#3
ships
data <- ships[,c(1,5)]
data <- data[c(8,16,24,32,40),]
data[,2] <- 0
for (i in 1:3) {
  data[i, 2] <- sum(ships[8*(i-1)+1:8*i, ]$incidents)
}
data[4,2] <- sum(ships[25:32, ]$incidents)
data[5,2] <- sum(ships[33:40, ]$incidents)
plot(data$type,data$incidents, xlab = "Type of Ship", ylab= "Total accident caused by the Ship",pch = 16)


# I agree with your hypothesis as B has the largest value 





#4
data<-read_html("https://stats.stackexchange.com/questions?tab=Votes")
title <- data %>% html_elements(".s-post-summary--content-title a") %>% html_text()
views <-  data %>% html_elements(".s-post-summary--stats-item.is-supernova") %>% html_attr("title") %>% str_remove_all(" views")%>% as.numeric
answers <-  data %>% html_elements(".s-post-summary--stats-item-number") %>% html_text()  
for(i in 1:15){
  answers[i] <- answers[3*i - 1]
}
answers <- answers[1:15]
votes <-  data %>% html_elements(".s-post-summary--stats-item-number") %>% html_text()   
for(i in 1:15){
  votes[i] <- votes[3*i - 2]
}
votes <- votes[1:15]
table <- data.frame("The title of the questions" = title,"The number of views" = views,"The number of answers" = answers,"The number of votes" = votes)  






#5
daystaken <- function(){
  a <- c(0,100)
  days <- 0
  while(1){
    days <- days+1
    k <- sample(x = 1:2, size = 1,prob = a)
    if(k == 1) return(days)
    else{ a[1] <- a[1]+1
    a[2] <- a[2] -1
    }
  }
}
count <- numeric(length = 1000)
for(i in 1:1000){
  count[i] <- daystaken()
}
ans <- mean(count)
print(ans)        

