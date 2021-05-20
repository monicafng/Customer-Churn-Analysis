# Import libraries
library(e1071)
library(car)
library(reshape)
library(caret)
library(tidyverse)
library(caTools)
library(gains)
library(pROC)
library(MASS)

# Load the data
telco <- read.csv("Telco-Customer-Churn.csv")

######## DATA EXPLORATION ########
str(telco)
summary(telco)

# Descriptive statistics for continuous numerical variables
telco %>% select(tenure, MonthlyCharges, TotalCharges) %>% describe()

# Tenure count and Churn rate
ggplot(telco, aes(x=tenure)) + geom_histogram(binwidth=0.8, aes(fill=factor(Churn)))

# Compare churn rate for female vs. male
ggplot(telco, aes(gender)) + geom_bar(aes(fill=Churn), position='dodge')

# Boxplot for tenure vs. Churn
ggplot(telco, aes(x=factor(Churn), y=tenure)) + geom_boxplot(aes(fill=factor(Churn)))

# Boxplot for MonthlyCharges vs. Churn
ggplot(telco, aes(x=factor(Churn), y=MonthlyCharges)) + geom_boxplot(aes(fill=factor(Churn)))

# Boxplot for TotalCharges vs. Churn
ggplot(telco, aes(x=factor(Churn), y=TotalCharges)) + geom_boxplot(aes(fill=factor(Churn)))

######## DATA PREPROCESSING ########
# Check missing values
library(Amelia)
missmap(telco, y.at = c(1), y.labels = c(''),
        col = c('yellow','black'))
sum(is.na(telco))

# Remove rows with missing data (only 0.1%)
telco <- telco[complete.cases(telco),]

# Convert variables to dummies
telco$Male <- ifelse(telco$gender == 'Male', 1, 0)
telco$Partner <- ifelse(telco$Partner == 'Yes', 1, 0)
telco$Dependents <- ifelse(telco$Dependents == 'Yes', 1, 0)
# if customer has one-year contract -> Contract1yr == 1
# if customer has two-year contract -> Contract2yr == 1
# else customer has month-to-month contract
telco$Contract1yr <- ifelse(telco$Contract == 'One year', 1, 0)
telco$Contract2yr <- ifelse(telco$Contract == 'Two year', 1, 0)
telco$PaperlessBilling <- ifelse(telco$PaperlessBilling == 'Yes', 1, 0)
# for payment method, if customer pays by electronic check then Payment_echeck == 1,
# if customer pays by mailed check then Payment_check == 1,
# if customer pays by bank transfer then Payment_banktransfer == 1,
# else customer pays by credit card
telco$Payment_echeck <- ifelse(telco$PaymentMethod == 'Electronic check', 1, 0)
telco$Payment_check <- ifelse(telco$PaymentMethod == 'Mailed check', 1, 0)
telco$Payment_banktransfer <- ifelse(telco$PaymentMethod == 'Bank transfer (automatic)', 1, 0)
telco$PhoneService <- ifelse(telco$PhoneService == 'Yes', 1, 0) 
telco$MultipleLines <- ifelse(telco$MultipleLines == 'Yes', 1, 0)
telco$DSL <- ifelse(telco$InternetService == 'DSL', 1, 0)
telco$FiberOptic <- ifelse(telco$InternetService == 'Fiber optic', 1, 0)
telco$InternetService <- ifelse(telco$InternetService == 'No', 0, 1)
telco$OnlineSecurity <- ifelse(telco$OnlineSecurity == 'Yes', 1, 0)
telco$OnlineBackup <- ifelse(telco$OnlineBackup == 'Yes', 1, 0)
telco$DeviceProtection <- ifelse(telco$DeviceProtection == 'Yes', 1, 0)
telco$TechSupport <- ifelse(telco$TechSupport == 'Yes', 1, 0)
telco$StreamingTV <- ifelse(telco$StreamingTV == 'Yes', 1, 0)
telco$StreamingMovies <- ifelse(telco$StreamingMovies == 'Yes', 1, 0)
telco$Churn <- ifelse(telco$Churn == 'Yes', 1, 0)

######## DATA AND DIMENSION REDUCTION ########
# Selecting features for model prediction
telco <- telco %>% 
  dplyr::select(Male, SeniorCitizen, Partner, Dependents,
                tenure, PhoneService, MultipleLines, InternetService,
                DSL, OnlineSecurity, OnlineBackup, DeviceProtection,
                TechSupport, StreamingTV, StreamingMovies,
                PaperlessBilling, MonthlyCharges, TotalCharges, 
                Contract1yr, Contract2yr, Payment_echeck,
                Payment_check, Payment_banktransfer, Churn)

# Dimension reduction based on logistic regression and AIC
split <- sample.split(telco$Churn, SplitRatio = 0.8)
train <- subset(telco, split == TRUE)
test <- subset(telco, split == FALSE)

# Initial model
model <- glm(Churn ~ ., family = "binomial", data = train)
summary(model)

preds <- predict(model, test[, -(dim(telco)[2])])
preds <- ifelse(preds >= 0.5, 1, 0)

confusionMatrix(factor(preds, levels = c(1, 0)),
                factor(test$Churn, levels = c(1, 0)))

# AIC
telco.glm <- stepAIC(model, trace=FALSE)
telco.glm$anova

#Final Model based on AIC:
#  Churn ~ SeniorCitizen + Dependents + tenure + MultipleLines + 
#  InternetService + DSL + OnlineSecurity + TechSupport + StreamingTV + 
#  StreamingMovies + PaperlessBilling + MonthlyCharges + TotalCharges + 
#  Contract1yr + Contract2yr + Payment_echeck

df <- telco %>% 
  dplyr::select(SeniorCitizen, Dependents, tenure, MultipleLines, 
                InternetService, DSL, OnlineSecurity, TechSupport, 
                StreamingTV, StreamingMovies, PaperlessBilling, MonthlyCharges, 
                TotalCharges, Contract1yr, Contract2yr, Payment_echeck,
                Churn)

# Check structure of data before modeling
str(df)

######## PARTITION THE DATA ########
# train test split
split <- sample.split(df$Churn, SplitRatio = 0.8)
df.train <- subset(df, split == TRUE)
df.test <- subset(df, split == FALSE)

# Create empty lists to store results
train_results <- list()
valid_results <- list()

train_actual <- list()
valid_actual <- list()

train_probs <- list()
valid_probs <- list()

#Create 10 folds
folds <- cut(seq(1,nrow(df.train)),breaks=10,labels=FALSE)

#Perform 10 fold cross validation
for(i in 1:10){
  validIndexes <- which(folds==i,arr.ind=TRUE)
  
  valid.df <- df.train[validIndexes, ]
  train.df <- df.train[-validIndexes, ]
  
  # logistic regression
  model <- glm(Churn ~ ., family = "binomial", data = train.df)
  
  # get predictions
  train_prob <- predict(model, train.df[, -(dim(df)[2])])
  train_result <- ifelse(train_prob >= 0.5, 1, 0)
  valid_prob <- predict(model, valid.df[, -(dim(df)[2])])
  valid_result <- ifelse(valid_prob >= 0.5, 1, 0)
  
  train_actual <- c(train_actual, train.df[, (dim(df)[2])])
  valid_actual <- c(valid_actual, valid.df[, (dim(df)[2])])
  
  train_results <- c(train_results, train_result) 
  valid_results <- c(valid_results, valid_result)
  
  train_probs <- c(train_probs, train_prob)
  valid_probs <- c(valid_probs, valid_prob)
}

# Model Evaluation
train_cm <- confusionMatrix(factor(train_results, levels = c(1, 0)),
                            factor(train_actual, levels = c(1, 0)))

valid_cm <- confusionMatrix(factor(valid_results, levels = c(1, 0)),
                            factor(valid_actual, levels = c(1, 0)))

Accuracy <- train_cm$overall[[1]]
Sensitivity <- train_cm$byClass[[1]]
Specificity <- train_cm$byClass[[2]]
Precision <- train_cm$byClass[[5]]
FDR <- train_cm$table[3]/(train_cm$table[1]+train_cm$table[3])
FOR <- train_cm$table[2]/(train_cm$table[2]+train_cm$table[4])

print(paste0('Accuracy: ', Accuracy))
print(paste0('Sensitivity: ', Sensitivity))
print(paste0('Specificity: ', Specificity))
print(paste0('Precision: ', Precision))
print(paste0('FDR: ', FDR))
print(paste0('FOR: ', FOR))

######## MODEL DEPLOYMENT ########
split <- sample.split(df$Churn, SplitRatio = 0.8)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)

model <- glm(Churn ~ ., family = "binomial", data = train)
summary(model)

preds <- predict(model, test[, -(dim(df)[2])])
preds <- ifelse(preds >= 0.5, 1, 0)

confusionMatrix(factor(preds, levels = c(1, 0)),
                factor(test$Churn, levels = c(1, 0)))
