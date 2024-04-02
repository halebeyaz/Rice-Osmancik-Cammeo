  library(readxl)
  library(neuralnet)
  #reading the excel file
  osmancik <- as.data.frame(read_excel("/data/Rice_Osmancik_Cammeo_Dataset.xlsx",sheet=1))
 
  #normalize function
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x))) 
  }
  
  #normalize the dataset and convert the target to numeric 
  osmancik_n <- as.data.frame(lapply(osmancik[1:7], normalize)) 
  osmancik_n$CLASS = as.numeric(as.factor(osmancik$CLASS)) -1 
  

  # check if there is any null value in the dataset. 
  apply(osmancik_n, 2, function(x) sum(is.na(x)))
  
  set.seed(1815850)
  #get 75% sample for training, and 25% sample for testing. 
  index <- sample(1 : nrow(osmancik_n),round(0.75 * nrow(osmancik_n))) 
  x_train <- osmancik_n[index, ] 
  x_test <- osmancik_n[-index, ] 
  
  #create the formula for neuralnet.
  n <- names(x_train) 
  f <- as.formula(paste("CLASS ~",
                        paste(n[!n %in% "CLASS"], 
                              collapse = " + "))) 
  #create the model.
  nn <- neuralnet(f,
                  data = x_train,
                  hidden = c(5,3,2),  err.fct = "ce", threshold = 0.34,
                  act.fct = "logistic", linear.output = FALSE,lifesign = "full") 

    #plot the structure of neural network
  plot(nn)
  #test the model with test set
  nn.results <- compute(nn, x_test[,1:7])
  results <- data.frame(actual = x_test$CLASS, prediction = nn.results$net.result)
  
  #convert to categorical value of the prediction
  results$prediction <- ifelse(results$prediction>0.5, 1, 0)
  #confusion matrix
  cf <- table(results)
  #accuracy
  acc <- sum((results$prediction == x_test$CLASS)) / nrow(results) 
