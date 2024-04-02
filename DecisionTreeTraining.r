library(readxl)
library(tree) # Contains the "tree" function
osmancik <- as.data.frame(read_excel("/data/Rice_Osmancik_Cammeo_Dataset.xlsx",sheet=1))

osmancik$CLASS <- factor(osmancik$CLASS, levels = c("Osmancik", "Cammeo"), labels = c("0", "1"))


apply(osmancik, 2, function(x) sum(is.na(x))) #check null values
# Use 75% of samples for training, the rest for testing
# the indices are saved in the "sub" vector
sub <- sample(1 : nrow(osmancik),round(0.75 * nrow(osmancik))) 
osmancik.tr <- tree(CLASS ~ ., data = osmancik, subset = sub)

set.seed(579642) 
sub <- sample(1 : nrow(osmancik),round(0.75 * nrow(osmancik))) 
osmancik.tr <- tree(CLASS ~ ., data = osmancik, subset = sub)

acc_train <- numeric()
acc_test <- numeric()

#Cross-validation version - Construct a new DT for different partitions of the samples - 100 times
for (i in 1:100){
  sub <- sample(1 : nrow(osmancik),round(0.75 * nrow(osmancik))) 
  osmancik.tr <- tree(CLASS ~ ., data = osmancik, subset = sub)
  pred <- predict(osmancik.tr, osmancik[sub,], type = "class")
  #accuracy at training set 
  acc_train <- c(acc_train,sum(pred == osmancik[sub,"CLASS"]) / nrow(osmancik[sub,]))
 
  predict <- predict(osmancik.tr, osmancik[-sub,], type = "class")
  #accuracy at test set
  acc_test <- c(acc_test,sum((predict == osmancik[-sub,"CLASS"])) / nrow(osmancik[-sub,]))

}

max(acc_train)
plot(1-acc_train, type="l", ylab="Error Rate", xlab="Iterations", main="Error Rate for Rice With Different Subsets of Data")
