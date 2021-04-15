# Import libraries
library(psych)
library(tidyverse)
library(ggplot2)

# Load the data
telco <- read.csv("Telco-Customer-Churn.csv")

######## DATA EXPLORATION ########
View(telco)
str(telco)
summary(telco)

# Descriptive statistics for continuous numerical variables
telco %>% select(tenure, MonthlyCharges, TotalCharges) %>% describe()

# Histogram for tenure
ggplot(telco, aes(x=tenure)) + geom_histogram(binwidth=0.8, aes(fill=..count..))

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
str(telco)

# Omit NA data (only 0.1%)
telco <- na.omit(telco)
str(telco)

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
# for payment method:
# 'Electronic check' and 'Mailed check' -> Check
# 'Bank transfer' and 'Credit card' -> Automatic payment
# hence, AutoPayment -> if customer uses automatic payment then '1' else '0' for check payment
telco$AutoPayment <- ifelse(telco$PaymentMethod %in% c('Electronic check', 'Mailed check'), 0, 1)
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

categorical.variables <- c('SeniorCitizen','Partner','Dependents','PhoneService',
                           'MultipleLines','InternetService','OnlineSecurity',
                           'OnlineBackup','DeviceProtection','TechSupport',
                           'StreamingTV','StreamingMovies','PaperlessBilling',
                           'Churn','Male','Contract1yr','Contract2yr',
                           'AutoPayment','DSL','FiberOptic')
telco[,categorical.variables] <- lapply(telco[,categorical.variables], factor)

df <- telco %>% select(Male, SeniorCitizen, Partner, Dependents,
                       tenure, PhoneService, MultipleLines, InternetService,
                       DSL, FiberOptic, OnlineSecurity, OnlineBackup, 
                       DeviceProtection, TechSupport, StreamingTV, StreamingMovies,
                       Contract1yr, Contract2yr, PaperlessBilling, AutoPayment,
                       MonthlyCharges, TotalCharges, Churn)

str(df)

######## DATA AND DIMENSION REDUCTION ########


######## PARTITION THE DATA ########
# train test split
library(caret)
inTrain <- createDataPartition(y=df$Churn, p=0.75, list=FALSE)
df.train <- df[inTrain,]
df.test <- df[-inTrain,]

prop.table(table(df$Churn)) # imbalanced classification

#library(ROSE) # synthetic data generation
#df.rose <- ROSE(Churn ~ ., data = df.train, seed = 1)$data
#table(df.rose$Churn)
#prop.table(table(df.rose$Churn))

# k-fold cross validation
train.control <- trainControl(method="cv", number=10, savePredictions=TRUE)
# train the model
model <- train(Churn ~ ., data=df.train, method="glm", trControl=train.control)
print(model)

pred <- model$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1, 0)

eachfold <- pred %>% 
  group_by(Resample) %>% 
  summarise_at(vars(equal),
               list(Accuracy=mean))

eachfold
ggplot(data=eachfold, aes(x=Resample, y=Accuracy, group=1)) + geom_boxplot(color="maroon") + geom_point()

# get prediction on test.df
model <- glm(Churn ~ ., family="binomial", data = df.train)
summary(model)

test.pred <- predict(model, newdata=df.test[, -23], type='response')
test.pred <- ifelse(test.pred > 0.5, 1, 0)
table(df.test$Churn, test.pred)








