library(tidyverse)
library(class)
library(gmodels)
library(caret)
library(e1071)

setwd("F:/projects/breast_cancer")
bc<- read.csv("breast_cancer.csv",stringsAsFactors = F)
str(bc)

# Type column is the outcome to be predicted
table(bc$type)  # count
round(prop.table(table(bc$type))*100, digits=1)  # gives percentage

# Visualize the frequency of the type of tumours
ggplot(data=bc) + geom_bar(aes(x=type,y=..count..,fill=type)) + geom_text(stat='count',aes(x=type,label=..count..),hjust=-0.5) + coord_flip()

# We don't want the ID of the patient, so we can remove it
bc<- bc[,-1]

# Closer look of summary of three features
summary(bc[c("radius_mean","area_mean","smoothness_mean")])
# after seeing these, we see that area_mean range is large, so it will have more impact on the classifier
# so need to rescale the features
# Creating a normalize function
normal<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

# Apply the function to the numeric features
bc_n<- as.data.frame(lapply(bc[2:31],normal))
summary(bc_n$area_mean)

# Training and Test datasets bifurcation
train<- bc_n[1:469,]
test<- bc_n[470:569,]

# Seperate the type as labels
train_labels<-bc[1:469,1]
test_labels<- bc[470:569,1]

# KNN Classification Model Prediction
test_pred<- knn(train=train,test=test,cl=train_labels,k=21)
summary(test_pred)

# Model Performance Evaluation
CrossTable(x=test_labels,y=test_pred,prop.chisq=FALSE)
confusionMatrix(test_pred,test_labels,positive="M")  # 98% accuracy
#--------------------------------------------------------------------------


# Improving model performance
# Two ways: using z-score for rescaling and different k values
# Z-score rescaling
bc_z<- as.data.frame(scale(bc[-1]))
summary(bc_z$area_mean)  # z values > 3 or< -3 indicates extremely rare values
# Training and Test datasets bifurcation
train<- bc_z[1:469,]
test<- bc_z[470:569,]

# Seperate the type as labels
train_labels<-bc[1:469,1]
test_labels<- bc[470:569,1]

# KNN Classification Model Prediction
test_pred<- knn(train=train,test=test,cl=train_labels,k=21)
summary(test_pred)

# Model Performance Evaluation
CrossTable(x=test_labels,y=test_pred,prop.chisq=FALSE)
confusionMatrix(test_pred,test_labels,positive="M")


# Check for different k values
test_pred<- knn(train=train,test=test,cl=train_labels,k=1)   # 6 percent incorrectly classified
CrossTable(x=test_labels,y=test_pred,prop.chisq=FALSE)
