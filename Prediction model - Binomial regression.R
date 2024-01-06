##installations
install.packages('caret')


#read in data
data=read.csv(file="C:/Users/tmasu/OneDrive/Documents/Projects/Analysis/Credit Risk analysis for extending bank loans/02 Analysis/bankloans.csv")

##check and clean data
#remove N/A
clean_data = data[complete.cases(data),]
num_rows = nrow(clean_data)


##split data
#use 70% of dataset as training set and 30% as test set
set.seed(1)
sample <- sample(c(TRUE, FALSE), num_rows, replace=TRUE, prob=c(0.7,0.3))
train_data  <- clean_data[sample, ]
test_data   <- clean_data[!sample, ]

?anova()

#binomial glm
#all variables
attach(train_data)
model_all = glm(default ~ age + ed + employ + address + income + debtinc + creddebt+othdebt, family = "binomial")
summary(model_all)

#significant variables
#previous model showed that the age, education level, income and other debt coefficients are not significant at a 5% level
model_sign = glm(default ~ employ + address + debtinc + creddebt, family = "binomial")
summary(model_sign)

#predict results
test_data_used=test_data[,-9]
prediction_sign = predict.glm(model_sign,newdata=test_data,type="response")
prediction_sign_adj=ifelse(prediction_sign<0.5,0,1)


##deeper/proper test for accuracy
#confusion matrix
expected=factor(test_data$default)
predicted=factor(prediction_sign_adj)

library(caret)
(confusion_matrix=confusionMatrix(data=predicted,reference=expected))

#combine data set with predicted results 
data_results=cbind(test_data,prediction_sign,prediction_sign_adj)



