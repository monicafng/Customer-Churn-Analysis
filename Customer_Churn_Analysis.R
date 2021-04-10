setwd("/Users/monicakamanfengchen/iCloud/*MSBA CPP*/SPRING 2021/GBA6210/Slides/Project/")
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

str(telco)

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

# Building a model
library(caTools)

set.seed(101)
split <- sample.split(df$Churn, SplitRatio = 0.7)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)

model <- glm(Churn ~ ., family = binomial(link="logit"),
             data = train[, -1])

summary(model)

test$predictedChurn <- predict(model, newdata = test[, c(-1, -21)], type = 'response')
test$predictedChurn <- ifelse(test$predictedChurn > 0.5, 1, 0)
table(test$Churn, test$predictedChurn)

print(paste('Accuracy =',round(((1397+324)/(1397+152+237+324)), 2)))

