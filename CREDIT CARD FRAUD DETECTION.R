#Importing the dataset from the respective folder
credit_card <- read.csv('C:\\Users\\hp\\Documents\\CREDIT CARD DATA SET\\creditcard.csv')

# Glance at the structure of the dataset
str(credit_card)

#Convert Class to a factor variable
credit_card$Class <- factor(credit_card$Class, levels = c(0,1))

#Get the summary of the data
summary(credit_card)

#Count the missing values
sum(is.na(credit_card))

#--------------------------------------------------------------------------------------------------------------------------------

#get the distribution of fraud and legit transactions in the dataset
table(credit_card$Class)

#get the distribution of fraud and legit transactions in the dataset
prop.table(table(credit_card$Class))


#Pie chart of credit card transactions
labels <- c("legit", "fraud")
labels <- paste(labels, round(100*prop.table(table(credit_card$Class)), 2))
labels <- paste0(labels, "%")

pie(table(credit_card$Class), labels, col = c("orange", "red"),
      main = "Pie chart of credit card transactions")


#------------------------------------------------------------------------------------------------------------------------------------

#No model Predictions
predictions <- rep.int(0, nrow(credit_card))
predictions <- factor(predictions, levels = c(0,1))

#install.packages('caret')
library(caret)
confusionMatrix(data = predictions, reference = credit_card$Class)



#-----------------------------------------------------------------------------------------------------------------------------------
library(dplyr)

set.seed(1)
credit_card <-credit_card %>% sample_frac(0.1)

table(credit_card$Class)

library(ggplot2)

ggplot(data = credit_card, aes(x = V1 , y = V2, col = Class)) +
  geom_point() +
  theme_bw()+
  scale_color_manual(values = c('dodgerblue2' , 'red'))
#-------------------------------------------------------------------------------------------------------------------------------
#Creating training and Test sets for Fraud Detection Model

#install.packages('caTools')
library(caTools)

set.seed(123)

data_sample = sample.split(credit_card$Class,SplitRatio = 0.80)

train_data = subset(credit_card,data_sample==TRUE)

test_data = subset(credit_card,data_sample==FALSE)

dim(train_data)
dim(test_data)

#-----------------------------------------------------------------------------------------------------------------------------------
#Random Over-Sampling(ROS)

table(train_data$Class)

n_legit <- 22750
new_frac_legit <-0.50
new_n_total <- n_legit/new_frac_legit # = 22750/0.50

#install.packages('ROSE')
library(ROSE)
oversampling_result <- ovun.sample(Class ~ . ,
                                   data = train_data,
                                   method = "over",
                                   N = new_n_total,
                                   seed = 2019)

oversampled_credit <- oversampling_result$data

table(oversampled_credit$Class)


ggplot(data = oversampled_credit, aes(x = V1 , y = V2 , col = Class)) +
  geom_point(position = position_jitter(width = 0.2)) +
  theme_bw()+
  scale_color_manual(values = c('dodgerblue2' , 'red'))

#-----------------------------------------------------------------------------------------------------------------------------

#Random Under-Sampling(RUS)

table(train_data$Class)

n_legit <- 35
new_frac_legit <-0.50
new_n_total <- n_legit/new_frac_legit # = 35/0.50

#install.packages('ROSE')
library(ROSE)
undersampling_result <- ovun.sample(Class ~ . ,
                                   data = train_data,
                                   method = "under",
                                   N = new_n_total,
                                   seed = 2019)
undersampled_credit <- undersampling_result$data

table(undersampled_credit$Class)
ggplot(data = undersampled_credit, aes(x = V1 , y = V2 , col = Class)) +
  geom_point(position = position_jitter(width = 0.2)) +
  theme_bw()+
  scale_color_manual(values = c('dodgerblue2' , 'red'))



#-------------------------------------------------------------------------------------------------------------------------------
#ROS and RUS

n_new <-nrow(train_data) # = 22785
fraction_fraud_new <- 0.50

sampling_result <- ovun.sample(Class ~ . ,
                                    data = train_data,
                                    method = "both",
                                    N = n_new,
                                    p = fraction_fraud_new,
                                    seed = 2019)

sampled_credit <- sampling_result$data

table(sampled_credit$Class)
ggplot(data = sampled_credit, aes(x = V1 , y = V2 , col = Class)) +
  geom_point(position = position_jitter(width = 0.2)) +
  theme_bw()+
  scale_color_manual(values = c('dodgerblue2' , 'red'))




#-----------------------------------------------------------------------------------------------------------------------------
#Using SMOTE to balance the dataset

#install.packages("smotefamily")
library(smotefamily)

table(train_data$Class)

# Set the number of fraud and legitimate cases, and desire percentage of legitimate cases
n0 <- 22750
n1 <- 35
r0 <- 0.6

#Calculate the value for the dup_size parametre of SMOTE
ntimes <- ((1 - r0) / r0) * (n0 / n1) - 1

smote_output = SMOTE(X = train_data[ , -c(1, 31)],
                     target = train_data$Class,
                     K = 5,
                     dup_size = ntimes)


credit_smote <- smote_output$data

colnames(credit_smote)[30] <- "Class"

prop.table(table(credit_smote$Class))

#Class distribution for orignal dataset
ggplot(train_data, aes(x = V1, y = V2, color = Class)) +
  geom_point() +
  scale_color_manual(values = c('dodgerblue2', 'red'))

#Class distribution for over-sampled dataset using SMOTE
ggplot(credit_smote, aes(x = V1, y = V2, color = Class)) +
  geom_point() +
  scale_color_manual(values = c('dodgerblue2', 'red'))

                
#----------------------------------------------------------------------------------------------------------------------------

