library(readxl)
library(class)
library(gmodels)
library(ggvis)
library(ggplot2)

osmancik <- read_excel("Rice_Osmancik_Cammeo_Dataset.xlsx",sheet=1)
set.seed(42)
rows <- sample(nrow(osmancik))
osmancik <- osmancik[rows, ] #shuffling the dataset since they are ordered (first cammeo then osmancik)
table(osmancik$CLASS)#cammeo = 1630  osmancik = 2180
#osmancik$CLASS <- factor(osmancik$CLASS, levels = c("Osmancik", "Cammeo"), labels = c("0", "1"))

cor(osmancik$MAJORAXIS, osmancik$PERIMETER) #the correlation between perimeter and major axis is the biggest, %97.1
#scatterplot
osmancik %>% ggvis(~MAJORAXIS, ~PERIMETER, fill = ~CLASS) %>% layer_points()

#boxplot
ggplot(osmancik) +
  aes(x = PERIMETER, y = MAJORAXIS) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

osmancik_n <- as.data.frame(lapply(osmancik[1:7], normalize))

index<-createDataPartition(osmancik$CLASS, p= .9, list=FALSE)
osmancik_train<-osmancik[index,]
osmancik_test<-osmancik[-index,]

osmancik_train_labels <- osmancik[index, 8]
osmancik_test_labels <- osmancik[-index, 8] 

trial_sum <- numeric(20)
trial_n <- numeric(20)

#knn with cross validation on 100 samples and k=1:20
for(i in 1:100){
  
  osmancik_sample <- sample(1:nrow(osmancik), size=nrow(osmancik)*0.9)
  os_train <- osmancik[osmancik_sample,]
  os_test <- osmancik[-osmancik_sample,]
  test_size <- nrow(os_test)
  
  for(j in 1:20){
    predict <- knn(os_train[,-8], os_test[,-8], os_train$CLASS, k=j)
    trial_sum[j] <- trial_sum[j] + sum(predict==os_test$CLASS)
    trial_n[j] <- trial_n[j] + test_size
  
  }
}

plot(trial_sum *100 / trial_n, type="l", ylab="Accuracy (%)",xlab="K",main="Accuracy for OSMANCIK With Varying K (100 Samples)")
print(100*(1-trial_sum / trial_n)) #accuracy values
