telco <- read.csv("Telco-Customer-Churn.csv")

View(telco)
str(telco)
summary(telco)

library(ggplot2)

ggplot(telco, aes(gender)) + geom_bar(aes(fill = Churn))

ggplot(telco, aes(tenure, TotalCharges)) + geom_point(aes(color=Churn))

telco$Male <- ifelse(telco$gender == 'Male', 1, 0)
telco$Male <- factor(telco$Male)
telco$SeniorCitizen <- factor(telco$SeniorCitizen)
telco$Partner <- ifelse(telco$Partner == 'Yes', 1, 0)
telco$Partner <- factor(telco$Partner)
telco$Dependents <- ifelse(telco$Dependents == 'Yes', 1, 0)
telco$Dependents <- factor(telco$Dependents)
telco$PaperlessBilling <- ifelse(telco$PaperlessBilling == 'Yes', 1, 0)
telco$PaperlessBilling <- factor(telco$PaperlessBilling)
telco$PhoneService <- ifelse(telco$PhoneService == 'Yes', 1, 0)                                                        
telco$PhoneService <- factor(telco$PhoneService)
telco$Churn <- ifelse(telco$Churn == 'Yes', 1, 0)
telco$Churn <- factor(telco$Churn)


categorical <- c('MultipleLines','InternetService','OnlineSecurity','OnlineBackup',
                 'DeviceProtection','TechSupport','StreamingTV','StreamingMovies',
                 'Contract', 'PaymentMethod')
telco[,categorical] <- lapply(telco[,categorical], factor)

table(telco$InternetService)
table(telco$MultipleLines)
table(telco$OnlineSecurity)
table(telco$DeviceProtection)
table(telco$TechSupport)
table(telco$StreamingTV)
table(telco$StreamingMovies)

ggplot(telco, aes(x=TotalCharges)) + geom_histogram(aes(fill=Churn, bins = 50),
                                                    position = 'dodge')

str(telco)

library(dplyr)

df <- telco %>% select(customerID, Male, SeniorCitizen, Partner, Dependents,
                       tenure, PhoneService, MultipleLines, InternetService,
                       OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport,
                       StreamingTV, StreamingMovies, Contract, PaperlessBilling,
                       PaymentMethod, MonthlyCharges, TotalCharges, Churn)

str(df)

# Check missing values
library(Amelia)
missmap(df, y.at = c(1), y.labels = c(''),
        col = c('yellow','black'))
sum(is.na(df))
str(df)

# Omit NA data (only 0.1%)
df <- na.omit(df)
str(df)

library(psych)
describe(telco)



# Creating New Columns
df$MultipleLines <- ifelse(df$MultipleLines == 'Yes', 1, 0)
df$MultipleLines <- factor(df$MultipleLines)
df$DSL <- ifelse(df$InternetService == 'DSL', 1, 0)
df$DSL <- factor(df$DSL)
df$FiberOptic <- ifelse(df$InternetService == 'Fiber optic', 1, 0)
df$FiberOptic <- factor(df$FiberOptic)
df$InternetService <- ifelse(df$InternetService == 'No', 0, 1)
df$InternetService <- factor(df$InternetService)
df$OnlineSecurity <- ifelse(df$OnlineSecurity == 'Yes', 1, 0)
df$OnlineSecurity <- factor(df$OnlineSecurity)
df$OnlineBackup <- ifelse(df$OnlineBackup == 'Yes', 1, 0)
df$OnlineBackup <- factor(df$OnlineBackup)
df$DeviceProtection <- ifelse(df$DeviceProtection == 'Yes', 1, 0)
df$DeviceProtection <- factor(df$DeviceProtection)
df$TechSupport <- ifelse(df$TechSupport == 'Yes', 1, 0)
df$TechSupport <- factor(df$TechSupport)
df$StreamingTV <- ifelse(df$StreamingTV == 'Yes', 1, 0)
df$StreamingTV <- factor(df$StreamingTV)
df$StreamingMovies <- ifelse(df$StreamingMovies == 'Yes', 1, 0)
df$StreamingMovies <- factor(df$StreamingMovies)

df$Contract1yr <- ifelse(df$Contract == 'One year', 1, 0)
df$Contract1yr <- factor(df$Contract1yr)
df$Contract2yr <- ifelse(df$Contract == 'Two year', 1, 0)
df$Contract2yr <- factor(df$Contract2yr)
df$AutoPayment <- ifelse(df$PaymentMethod %in% c('Electronic check', 'Mailed check'), 0, 1)
                           
                           

str(df)
colnames(df)

df <- df %>% select(Male, SeniorCitizen, Partner,
                    Dependents, tenure, Contract1yr, Contract2yr,
                    PaperlessBilling, AutoPayment, MonthlyCharges,
                    TotalCharges, PhoneService, MultipleLines,
                    InternetService, DSL, FiberOptic, OnlineSecurity,
                    OnlineBackup, DeviceProtection, TechSupport,
                    StreamingTV, StreamingMovies, Churn)

df$techaddons <- ifelse(df$OnlineSecurity == 1|df$OnlineBackup== 1 | 
                             df$DeviceProtection== 1 |df$TechSupport== 1,1,0)
df$streamingaddon <- ifelse(df$StreamingTV==1|df$StreamingMovies == 1,1,0)

df <- df %>% select(Male, SeniorCitizen, Partner,
                    Dependents, tenure, Contract1yr, Contract2yr,
                    PaperlessBilling, AutoPayment, MonthlyCharges,
                    TotalCharges, PhoneService, MultipleLines,
                    InternetService, DSL, FiberOptic, techaddons,
                    streamingaddon, Churn)

str(df)

df$AutoPayment <- factor(df$AutoPayment)
df$techaddons <- factor(df$techaddons)
df$streamingaddon <- factor(df$streamingaddon)

str(df)

# Building a model
library(caTools)

set.seed(101)
split <- sample.split(df$Churn, SplitRatio = 0.7)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)

model <- glm(Churn ~ .,
             family = binomial(link="logit"),
             data = train[, -1])

summary(model)

test$predictedChurn <- predict(model, newdata = test[, -19], type = 'response')
test$predictedChurn <- ifelse(test$predictedChurn > 0.5, 1, 0)
table(test$Churn, test$predictedChurn)

print(paste('Accuracy =',round(((1398+303)/(1398+151+258+303)), 2)))








