library(dplyr)
library(ggplot2)
library(tidyverse)


data <- read_csv("C:/Users/tolua/Downloads/Placement_Data_Full_Class.csv")
#call the data to view the table
view(data)
#Check data structure, details
str(data)
summary(data)

#Check top5 and bottom5 data
head(data)
tail(data)
#Dimension of the data
dim(data)
#Check for NAs values
sum(is.na(data))
data[is.na(data)] <- 0
#Renaming the variables
data <- rename(data, Gender = gender,
               SecEducationPercent = ssc_p,
               SecBoardofEducation = ssc_b,
               HigherSecEduPercent = hsc_p,
               HigherSecBoardofEdu = hsc_b,
               HigherSecSpecial = hsc_s,
               DegreePercent = degree_p,
               DegreeType = degree_t,
               WorkExp = workex,
               EmpTestPercent = etest_p,
               MBAspec = specialisation,
               MBApercent = mba_p,
               Status = status,
               Salary = salary)
# Change char variables into factor
data$Gender <- as.factor(data$Gender)
data$SecBoardofEducation <- as.factor(data$SecBoardofEducation)
data$HigherSecBoardofEdu <- as.factor(data$HigherSecBoardofEdu)
data$HigherSecSpecial <- as.factor(data$HigherSecSpecial)
data$DegreeType <- as.factor(data$DegreeType)
data$WorkExp <- as.factor(data$WorkExp)
data$MBAspec <- as.factor(data$MBAspec)
data$Status <- as.factor(data$Status)

attribute <- names(data)
attribute

data_summary <- data %>% summarise_all(~ sum(is.na(.)))
data_summary

data %>% group_by(Status) %>% count()

#Observe the correlation between the numeric values excluding Salary
data_corr <- select_if(data, is.numeric) %>% select(-Salary, -sl_no)
corr <- round(cor(data_corr),2)
corr

install.packages("ggcorrplot")
library(ggcorrplot)

p.mat <- cor_pmat(data_corr)
p.mat

#visualize the correlation matrix
ggcorrplot(corr, method = "square", 
           ggtheme = ggthemes::theme_few, 
           outline.col = "black",
           lab = TRUE,
           lab_size = 5,
           digits = 2,
           type = "lower",
           legend = "",
           tl.cex = 12)

#The distribution of score at each level of education
#Histogram plot for MBApercent
data %>%
  ggplot( aes(x=MBApercent)) +
  geom_histogram(fill="#69b3a2", color="black", binwidth = 5, alpha=0.8)+
  coord_cartesian(xlim=c(30,100),
                  ylim=c(0,70))+
  labs(x = "MBA Percentage Score",
       y = "Number of students")
#Histogram plot for EmpTestPercent
data %>%
  ggplot( aes(x=EmpTestPercent)) +
  geom_histogram(fill="#69b3a2", color="black", binwidth = 5, alpha=0.8)+
  coord_cartesian(xlim=c(30,100),
                  ylim=c(0,70))+
  labs(x = "Employment Test Score",
       y = "Number of students")
#Histogram plot for DegreePercent
data %>%
  ggplot( aes(x=DegreePercent)) +
  geom_histogram(fill="#69b3a2", color="black", binwidth =5, alpha=0.8)+
  coord_cartesian(xlim=c(30,100),
                  ylim=c(0,70))+
  labs(x = "Degree (University) Score",
       y = "Number of students")
#Higher Secondary       
data %>%
  ggplot( aes(x=HigherSecEduPercent)) +
  geom_histogram(fill="#69b3a2", color="black", binwidth =5, alpha=0.8)+
  coord_cartesian(xlim=c(30,100),
                  ylim=c(0,70))+
  labs(x = "Higher Secondary Score",
       y = "Number of students")       
#Secondary
data %>%
  ggplot( aes(x=SecEducationPercent)) +
  geom_histogram(fill="#69b3a2", color="black", binwidth =5, alpha=0.8)+
  coord_cartesian(xlim=c(30,100),
                  ylim=c(0,70))+
  labs(x = "Secondary Score",
       y = "Number of students")

#Gender inference on performance
install.packages('rstatix')
library(rstatix)

library(ggthemes)

t_test_degree <- data%>%t_test(DegreePercent ~ Gender)%>%add_significance()

data %>% 
  ggplot(aes(DegreePercent, fill = Gender, col = Gender))+
  geom_density(alpha = 0.3, lwd = 1, show.legend = FALSE)+
  geom_rug()+
  labs(title = paste("Females have a significantly higher score 
  (", t_test_degree$p.signif, ") than males at the degree level", sep = ""),
       col = "Gender",
       x = "Degree (University) Score",
       y = "Density")+
  theme_few()+
  theme(legend.position = "right")                  

install.packages('kableExtra')
library(kableExtra)

t_test_degree%>%
  kbl()%>%
  kable_paper("hover", full_width = F)                   

#Gender inference on MBA                   

t_test_mba <- data%>%t_test(MBApercent ~ Gender)%>%add_significance()

data %>% 
  ggplot(aes(MBApercent, fill = Gender, col = Gender))+
  geom_density(alpha = 0.3, lwd = 1, show.legend = FALSE)+
  geom_rug()+
  labs(title = paste("Females have a significantly higher score 
  (", t_test_mba$p.signif, ") than males at the MBA level", sep = ""),
       col = "Gender",
       x = "MBA Percentage Score",
       y = "Density")+
  theme_few()+
  theme(legend.position = "right")                  


t_test_mba%>%
  kbl()%>%
  kable_paper("hover", full_width = F)   
#############  

t_test_emptest <- data%>%t_test(EmpTestPercent ~ Gender)%>%add_significance()

data %>% 
  ggplot(aes(EmpTestPercent, fill = Gender, col = Gender))+
  geom_density(alpha = 0.3, lwd = 1, show.legend = FALSE)+
  geom_rug()+
  labs(title = paste("Females have a significantly higher score 
  (", t_test_emptest$p.signif, ") than males at the Employee Test", sep = ""),
       col = "Gender",
       x = "Employee Percentage Score",
       y = "Density")+
  theme_few()+
  theme(legend.position = "right")                  


t_test_emptest%>%
  kbl()%>%
  kable_paper("hover", full_width = F)   
#######################  

t_test_se <- data%>%t_test(SecEducationPercent ~ Gender)%>%add_significance()

data %>% 
  ggplot(aes(SecEducationPercent, fill = Gender, col = Gender))+
  geom_density(alpha = 0.3, lwd = 1, show.legend = FALSE)+
  geom_rug()+
  labs(title = paste("Females have a significantly higher score 
  (", t_test_hse$p.signif, ") than males at the Secondary Education", sep = ""),
       col = "Gender",
       x = "Secondary Education Score",
       y = "Density")+
  theme_few()+
  theme(legend.position = "right")                  


t_test_se%>%
  kbl()%>%
  kable_paper("hover", full_width = F)  

#Academic performance impact on the chances of recieving an offer  
t_test <- data%>%
  t_test(SecEducationPercent ~ Status)%>%
  add_significance() 

data %>%
  ggplot( aes(x=SecEducationPercent, fill=Status, color=Status)) +
  geom_density(alpha=0.5)+
  labs(x = "Secondary Score")+
  theme_few()

t_test%>%
  kbl()%>%
  kable_paper("hover", full_width = F) 

#Higher Secondary                   

t_test_hsc <- data%>%
  t_test(HigherSecEduPercent ~ Status)%>%
  add_significance() 

data %>%
  ggplot( aes(x=HigherSecEduPercent, fill=Status, color=Status)) +
  geom_density(alpha=0.5)+
  labs(x = "Higher Secondary Score")+
  theme_few()

t_test_hsc%>%
  kbl()%>%
  kable_paper("hover", full_width = F)                    

#University                   

t_test_uni <- data%>%
  t_test(DegreePercent ~ Status)%>%
  add_significance() 

data %>%
  ggplot( aes(x=DegreePercent, fill=Status, color=Status)) +
  geom_density(alpha=0.5)+
  labs(x = "Degree (University) Score")+
  theme_few()

t_test_uni%>%
  kbl()%>%
  kable_paper("hover", full_width = F)  


#MBA                   

t_test_mba <- data%>%
  t_test(MBApercent ~ Status)%>%
  add_significance() 

data %>%
  ggplot( aes(x=MBApercent, fill=Status, color=Status)) +
  geom_density(alpha=0.5)+
  labs(x = "MBA Percentage Score")+
  theme_few()

t_test_mba%>%
  kbl()%>%
  kable_paper("hover", full_width = F) 

#EMP TEST                   

t_test_emp <- data%>%
  t_test(EmpTestPercent ~ Status)%>%
  add_significance() 

data %>%
  ggplot( aes(x=EmpTestPercent, fill=Status, color=Status)) +
  geom_density(alpha=0.5)+
  labs(x = "Employment Test Score")+
  theme_few()

t_test_emp%>%
  kbl()%>%
  kable_paper("hover", full_width = F)   

#Overall education percentile

data %>%
  mutate(lower_education = (SecEducationPercent+HigherSecEduPercent)/2,
         higher_education = (DegreePercent+MBApercent)/2,
         all_education = (SecEducationPercent+HigherSecEduPercent+DegreePercent+MBApercent)/4,
         percentile_lower = round(rank(lower_education)/n()*100,2),
         percentile_higher = round(rank(higher_education)/n()*100,2),
         percentile_all_education = round(rank(all_education)/n()*100,2))%>%
  ggplot(aes(percentile_lower, percentile_higher, col = Status))+
  geom_point(aes(col = Status), size = 4)+
  geom_vline(xintercept =50, lty = 2)+
  geom_hline(yintercept =50, lty = 2)+
  labs(x = "Lower education percentile",
       y = "Higher education percentile",
       col = "Status")+
  theme_few()+
  theme(legend.position = "top")

#Splitting the data
#clean the model data and drop the salary feature
data_mod <- data %>%
  select(-Salary)%>%
  mutate(Status = as.factor(make.names(Status)))

# Stratified Split
install.packages("caret")
library(caret)

set.seed(100)  # good idea to set the random seed for reproducibility
partition <- createDataPartition(data_mod$Status, p = 0.7, list=FALSE)

train_data <- data_mod[partition,]
test_data <- data_mod[-partition,]

view(test_data)

ggplot(data_mod, aes(Status)) + geom_bar(fill = "orange", color = "white")
ggplot(test_data, aes(Status)) + geom_bar(fill = "green", color = "white")  
ggplot(train_data, aes(Status)) + geom_bar(fill = "blue", color = "white")

y_train <- train_data$sl_no
y_test <- test_data$sl_no

colnames(train_data)

train_data <- select (train_data, -c(sl_no))  
test_data <- select (test_data, -c(sl_no))  

colnames(train_data)  

#scaling the data
scaled_centered <- preProcess(train_data, method=c('center', 'scale', 'nzv'))

train_data <- predict(scaled_centered, newdata = train_data)
test_data <- predict(scaled_centered, newdata = test_data)

#Subset Selection
install.packages('Leaps')
library(leaps)
#Forward
datafit.fwd <- regsubsets(Status ~., data = train_data, nvmax=13, method = 'forward')
fwd.summary <- summary(datafit.fwd)
names(fwd.summary)
fwd.summary$rss

par(mfrow = c(1,2))
plot(fwd.summary$rss, xlab = 'Number of variables',
     ylab = 'RSS', type = 'l')
plot(fwd.summary$adjr2, xlab = 'Number of variables',
     ylab = 'Adjusted RSq', type = 'l')  

coef(datafit.fwd, 7)
#Backward
datafit.bkd <- regsubsets(Status ~., data = train_data, nvmax=13, method = 'backward')
summary(datafit.bkd)
coef(datafit.bkd, 7)

#Forward
fwd.summary <- summary(datafit.fwd)
fwd.summary$adjr2
#Backward
bkd.summary <- summary(datafit.bkd)
bkd.summary$adjr2
#Best Subset
full.summary <- summary(datafit.full)
full.summary$adjr2

par(mfrow = c(1,2))
plot(bkd.summary$rss, xlab = 'Number of variables',
     ylab = 'RSS', type = 'l')
plot(bkd.summary$adjr2, xlab = 'Number of variables',
     ylab = 'Adjusted RSq', type = 'l') 

which.max(fwd.summary$adjr2)  


#Best Subset Selection
datafit.full <- regsubsets(Status ~., data = train_data, nvmax=13)
summary(datafit.full)
coef(datafit.full, 7)
full.summary <- summary(datafit.full)
full.summary$rss


#Resampling
#10-fold Cross Validation 
set.seed(10)
ctrl <- trainControl(method = 'cv', number =10, savePredictions = TRUE)

#Generalized Linear Model
glm_model <- train(Status ~ ., data=train_data, method='glm', trControl=ctrl, tuneLength=5)
#Summary
summary(glm_model)
names(glm_model)
#Accuracy
glm_model$results
#add prediction column to test dataset
test_data$glm <- predict(glm_model, newdata=test_data)
view(test_data)
#get probabilities
head(predict(glm_model, newdata = test_data, type = 'prob'))
glm_preds <- predict(glm_model, newdata = test_data)
glm_cm <- confusionMatrix(glm_preds, test_data$Status)
glm_cm

pred <-glm_model$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1,0)

eachfold <- pred %>% group_by(Resample) %>% summarise_at(vars(equal), list(Accuracy = mean))
eachfold

#Visualize the folds using boxplot
ggplot(data=eachfold, aes(x=Resample, y=Accuracy, group=1)) +
  geom_boxplot(color="maroon") +
  geom_point() +
  labs(title =paste("GLM model fold's accuracy in boxplot")) +
  theme_minimal()

#Draw the confudion Matrix
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#40B0A6')
  text(195, 440, 'Not Placed', cex=1.2)
  rect(250, 430, 340, 370, col='#DC3220')
  text(295, 440, 'Placed', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#DC3220')
  rect(250, 305, 340, 365, col='#40B0A6')
  text(140, 400, 'Not Placed', cex=1.2, srt=90)
  text(140, 335, 'Placed', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.5, font=2)
  text(10, 63, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.5, font=2)
  text(30, 63, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.5, font=2)
  text(50, 63, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.5, font=2)
  text(70, 63, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.5, font=2)
  text(90, 63, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 40, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.2)
  text(70, 40, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.2) }

draw_confusion_matrix(glm_cm)  



#Linear Discrimeanant Analysis
lda_model <- train(Status ~ ., data = train_data, method = 'lda', trControl = ctrl, tuneLength=5)
#Summary
summary(lda_model)
#names(lda_model)
lda_model$results
test_data$lda <- predict(lda_model, newdata = test_data)
view(test_data)
head(predict(lda_model, newdata=test_data, type='prob'))
lda_prob <- predict(lda_model, newdata=test_data, type='prob')*100
lda_pred <- predict(lda_model, newdata = test_data)
lda_cm <- confusionMatrix(lda_pred, test_data$Status)
lda_cm


pred <-lda2_model$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1,0)

eachfold <- pred %>% group_by(Resample) %>% summarise_at(vars(equal), list(Accuracy = mean))
eachfold

#Visualize the folds using boxplot
ggplot(data=eachfold, aes(x=Resample, y=Accuracy, group=1)) +
  geom_boxplot(color="maroon") +
  geom_point() +
  labs(title =paste("LDA model fold's accuracy in boxplot")) +
  theme_minimal()

#Support Vector Machine: Linear
svm_model <- train(Status ~ ., data = train_data, method = 'svmLinear', trControl = ctrl, tuneLength=5)
#Summary
summary(svm_model)
#names(svm_model)
svm_model$results
test_data$svm <- predict(svm_model, newdata = test_data)
view(test_data)
head(predict(svm_model, newdata=test_data, type='prob'))
svm_prob <- predict(svm_model, newdata=test_data, type='prob')*100
svm_pred <- predict(svm_model, newdata = test_data)
svm_cm <- confusionMatrix(svm_pred, test_data$Status)
svm_cm

pred <-svm_model$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1,0)

eachfold <- pred %>% group_by(Resample) %>% summarise_at(vars(equal), list(Accuracy = mean))
eachfold

#Visualize the folds using boxplot
ggplot(data=eachfold, aes(x=Resample, y=Accuracy, group=1)) +
  geom_boxplot(color="maroon") +
  geom_point() +
  labs(title =paste("SVMLinear model fold's accuracy in boxplot")) +
  theme_minimal()

#KNN
knn_model <- train(Status ~ ., data = train_data, method = 'knn', trControl = ctrl, tuneLength=5)
#Summary
names(knn_model)
knn_model$results
test_data$knn <- predict(knn_model, newdata = test_data)
view(test_data)
head(predict(knn_model, newdata=test_data, type='prob'))
knn_prob <- predict(knn_model, newdata=test_data, type='prob')*100
knn_pred <- predict(knn_model, newdata = test_data)
knn_cm <- confusionMatrix(knn_pred, test_data$Status)
knn_cm

pred <-knn_model$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1,0)

eachfold <- pred %>% group_by(Resample) %>% summarise_at(vars(equal), list(Accuracy = mean))
eachfold

#Visualize the folds using boxplot
ggplot(data=eachfold, aes(x=Resample, y=Accuracy, group=1)) +
  geom_boxplot(color="maroon") +
  geom_point() +
  labs(title =paste("KNN model fold's accuracy in boxplot")) +
  theme_minimal()

#Logistics Regression
log_model <- train(Status ~ ., data=train_data, method='glm', trControl=ctrl, tuneLength=5, family = binomial)
#Summary
summary(log_model)
names(log_model)
#Accuracy
log_model$results
#add prediction column to test dataset
test_data$log <- predict(log_model, newdata=test_data)
view(test_data)
#get probabilities
head(predict(log_model, newdata = test_data, type = 'prob'))
log_preds <- predict(log_model, newdata = test_data)
log_cm <- confusionMatrix(log_preds, test_data$Status)
log_cm

pred <-log_model$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1,0)

eachfold <- pred %>% group_by(Resample) %>% summarise_at(vars(equal), list(Accuracy = mean))
eachfold

#Visualize the folds using boxplot
ggplot(data=eachfold, aes(x=Resample, y=Accuracy, group=1)) +
  geom_boxplot(color="maroon") +
  geom_point() +
  labs(title =paste("Logistics Regression model fold's accuracy in boxplot")) +
  theme_minimal()

#Quadratic Linear Discriminant
qda_model <- train(Status ~ ., data=train_data, method='qda', trControl=ctrl, tuneLength=5)
#Summary
summary(qda_model)
#names(qda_model)
#Accuracy
qda_model$resample
#add prediction column to test dataset
test_data$qda <- predict(qda_model, newdata=test_data)
view(test_data)
#get probabilities
head(predict(qda_model, newdata = test_data, type = 'prob'))
qda_preds <- predict(qda_model, newdata = test_data)
qda_cm <- confusionMatrix(qda_preds, test_data$Status)
qda_cm

install.packages('randomForest')
library(randomForest)

#Random Forest-Ranger
rf_model <- train(Status ~ ., data=train_data, method='ranger', trControl=ctrl, tuneLength=5)
#Summary
summary(rf_model)
#names(rf_model)
#Accuracy
rf_model$resample
#add prediction column to test dataset
test_data$rf <- predict(rf_model, newdata=test_data)
view(test_data)
#get probabilities
#head(predict(rf_model, newdata = test_data, type = 'prob'))
rf_preds <- predict(rf_model, newdata = test_data)
rf_cm <- confusionMatrix(rf_preds, test_data$Status)
rf_cm

install.packages('gbm')
library(gbm)

#Boosting
gbm_model <- train(Status ~ ., data=train_data, method='gbm', trControl=ctrl, tuneLength=5)
#Summary
summary(gbm_model)
names(gbm_model)
#Accuracy
gbm_model$results
#add prediction column to test dataset
test_data$gbm <- predict(gbm_model, newdata=test_data)
view(test_data)
#get probabilities
#head(predict(gbm_model, newdata = test_data, type = 'prob'))
gbm_preds <- predict(gbm_model, newdata = test_data)
gbm_cm <- confusionMatrix(gbm_preds, test_data$Status)
gbm_cm

models <- resamples(list(GBoost = gbm_model, RandomF = rf_model,KNN = knn_model,
                         QDA = qda_model,SVM = svm_model,LDA = lda_model,LogREG = log_model,
                         GLM = glm_model))

#Scaling
scales <- list(x=list(relation="free"), y=list(relation="free"))

#draw a boxplot to compare models
bwplot(models, scales=scales)

summary(models)

tidy(compare_models(knn_model, lda_model))%>%
  kbl(caption = "Comparing the statistics of KNN and LDA models")%>%
  kable_paper("hover", full_width = F)



par(pty = "s")

#create the ROC
roc(test_data$Status,
    predict(knn_model, newdata = test_data, type = "prob")$Placed,
    plot = TRUE,
    legacy.axes = TRUE,
    percent = TRUE,
    xlab = "False Positive Percentage",
    ylab = "True Positive Percentage",
    lwd = 4,
    col = "#00AFBB",
    print.auc = TRUE)

#add 2nd ROC line
plot.roc(test_data$Status,
         predict(lda_model, newdata = test_data, type = "prob")$Placed,
         percent = TRUE,
         lwd = 4,
         col = "#E69F00",
         print.auc = TRUE,
         add=TRUE,
         print.auc.y=40)


legend("bottomright",
       legend = c("KNN", "LDA"),
       col=c("#00AFBB", "#E69F00"),
       lwd = 4)
