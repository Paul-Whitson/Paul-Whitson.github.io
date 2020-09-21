#Data Mining Course Project
#Feb 11, 2020

#Read in training and test data sets:
train <- read.csv("MIR_Fruit_purees_train.csv")
test <- read.csv("MIR_Fruit_purees_test.csv")


#Initialize vectors and data frame to store means, standard deviations, 
#and normalized training and test data:

train_means <- numeric(length=235)
train_sds <- numeric(length = 235)

#create training data set
train_normalized <- data.frame(matrix(nrow = 689, ncol = 237))
colnames(train_normalized) <- colnames(train)
train_normalized[,236:237] <- train[,236:237]

#create test data set
test_normalized <- data.frame(matrix(nrow = 294, ncol = 237))
colnames(test_normalized) <- colnames(test)
test_normalized[,236:237] <- test[,236:237]


#Normalize training set and store means and standard deviations
for (i in 1:235) {
  train_means[i] <- mean(train[,i])
  train_sds[i] <- sd(train[,i])
  train_normalized[,i] <- ((train[,i]-train_means[i])/train_sds[i])
}

#Normalize test set using parameters from training set
for (i in 1:235) {
  test_normalized[,i] <- ((test[,i]-train_means[i])/train_sds[i])
}

#run principal components on normalized training data
pca_train <- princomp(train_normalized[,1:235])

#Create scree plot:
pca_train_totvar <- sum(pca_train$sdev^2) #total variance of all principal components
pca_train_cumul_var_pct <- cumsum(pca_train$sdev^2)/pca_train_totvar #calculate cumulative % of total variance

library(ggplot2)

par(mfrow=c(1,1))
par(mar =c(1,1,1,1))
scree_df <- data.frame(pca_train_cumul_var_pct)
names(scree_df) <- c("Variance_Accounted_For")
head(scree_df)

plot(pca_train_cumul_var_pct[1:20], type = "l", 
     main = "Scree Plot for PCA on Normalized Training Set",
     xlab = "Principal Component", ylab = "Cumulative Fraction of Total Variance") +
  grid(col = "lightgray") + abline(h = 1.0)

689+294
#graph first 3 principal components:
library(plotly)
 
#note that the "strawberry" data points tend to cluster in an ellipsoid near the center of the data

#use loadings from the PCA analysis performed on the training data to convert the normalized TEST data into principal components:
test_pca <- predict(pca_train, test_normalized[,1:235])
#add the columns with the sample identification and the index from the original data set:
test_pca <- data.frame(cbind(test_pca, test[,236:237]))

plot_ly(x = test_pca[,1], y = test_pca[,2], 
        z = test_pca[,3], color = as.factor(test_pca[,236]), colors = c("blue", "red"),
        opacity = 0.7, size = 30)

sd(test_pca[,20])

write.csv(test_pca, "Test_Data_as_PCA.csv")

head(pca_train_cumul_var_pct, 20)

head(train)[1:5,1:5]

#**************************************************************
#Fit Partial Least Squares (PLS) with Binary Logistic Regression on the normalized training data:

install.packages("plsRglm")
library(plsRglm)
colnames(train_normalized)[230:237]

#Fit PLS model with 3 components
pls_model_3 <- plsRglm(strawberry~.,
                    data = train_normalized[,1:236],
                    modele = "pls-glm-logistic",
                    nt = 3)

summary(pls_model_3)
str(pls_model_3)

pls_3_predictions <- (predict(pls_model_3, type = "response") >= 0.5)

#Graph 3 components with predicted values:
plot_ly(x = pls_model_3$tt[,1], y = pls_model_3$tt[,2],
        z = pls_model_3$tt[,3], 
        color = as.factor(pls_3_predictions), size = 10)


predict(pls_model_3, type = "response")

xtabs(data = data.frame(cbind("Actual" = train_normalized$strawberry,
                              "Predicted" = pls_3_predictions)))

#Performance of 3-component model on training data:

#False Negatives: 
37/(37+407) #8.3%

#False positives:
21/(21+224) #8.6%

#Performance of 3-component model on test data:
remove(pls_3_test_predictions)
pls_3_test_predictions <- predict(pls_model_3,
                                  newdata = test_normalized[,1:235],
                                  type = "response")

pls_3_test_predictions <- (pls_3_test_predictions >= 0.5)
xtabs(data = data.frame(cbind("Actual" = test_normalized$strawberry,
                              "Predicted" = pls_3_test_predictions)))

#False neg
15/(15+173) #8.0% false neg

#False pos
12/(12+94) #11.3% False positives

#**********************************************
#10-Component Model
pls_model_10 <- plsRglm(strawberry~.,
                       data = train_normalized[,1:236],
                       modele = "pls-glm-logistic",
                       nt = 10)

pls_10_pred_train <- (predict(pls_model_10, type = "response") >= 0.5)

xtabs(data = data.frame(cbind("Actual" = train_normalized$strawberry,
                              "Predicted" = pls_10_pred_train)))

#False negs:
6/(6+438) #1.35%
#False positives:
4/(4+241) #1.6%%

#Performance on Test Data
pls_10_test_predictions <- predict(pls_model_10,
                                  newdata = test_normalized[,1:235],
                                  type = "response")

pls_10_test_predictions <- (pls_10_test_predictions >= 0.5)

write.csv(pls_10_test_predictions, "PLS_10_Test_Predictions.csv")

xtabs(data = data.frame(cbind("Actual" = test_normalized$strawberry,
                              "Predicted" = pls_10_test_predictions)))

#False Neg
4/(4+184) #2.1%
#False Pos
5/(5+101) #4.7%


#*******************************
#20-Component Model
pls_model_20 <- plsRglm(strawberry~.,
                        data = train_normalized[,1:236],
                        modele = "pls-glm-logistic",
                        nt = 20)

pls_20_pred_train <- (predict(pls_model_20, type = "response") >= 0.5)

xtabs(data = data.frame(cbind("Actual" = train_normalized$strawberry,
                              "Predicted" = pls_20_pred_train)))
#0% False positives
#0% False negatives

#on training data

pls_20_pred_test <- predict(pls_model_20,
                            newdata = test_normalized[,1:235],
                            type = "response")

pls_20_pred_test <- (pls_20_pred_test >= 0.5)

xtabs(data = data.frame(cbind("Actual" = test_normalized$strawberry,
                              "Predicted" = pls_20_pred_test)))
#False neg
3/(3+185) #1.6%
#False positives
1/(1+186) #0.5%

#Visualizations of Raw Data
head(train)[1:5,230:237]

train_strawberry <- train$strawberry
train_index <- train$index
tr_graph <- train[,-c(236,237)]

dim(tr_graph)
head(tr_graph)[,230:235]

Wavelengths <- colnames(tr_graph)
head(Wavelengths)
Wavelengths <- str_remove(Wavelengths, "X")
Wavelengths <- as.numeric(Wavelengths)
str(Wavelengths)

tr_graph_t <- t(tr_graph)
Wavelengths <- as.vector(Wavelengths, mode = "numeric")
tr_graph_t$Wavelengths <- Wavelengths
str(tr_graph_t)

tr_graph_t$Wavelength <- as.numeric(rownames(tr_graph_t))

write.csv()


head(tr_graph_t)[1:5,1:5]
dim(train_t)
names(train_t)<- c(1:689)
train_melt <- melt(train_t)  
library(reshape2)  
dim(train_melt)
head(train_melt)  
names(train_melt) <- c("Wavelength", "Sample", "Reading")
  
library(ggplot2)
ggplot(data = train_melt[1:2756,],
       aes(x = as.factor(Wavelength), y=Reading, col = as.factor(Sample))) +
  geom_line() 
  
max(train_melt$Reading)
which(train_melt$Reading == 628)

#some issues: Wavelength column also contains "strawberry" text, as 
#well as "index"

train_melt2 <- train_melt[1:6280,]
ggplot(data = train_melt2[1:235,],
       aes(x=c(1:235), y=Reading, col = as.factor(Sample))) +
  geom_line()


#Graphical Data Exploration
head(train)
dim(train)

data_graph = train
train_index = train[237]
strawberry_train = train[236]
data_graph = train[-c(236:237)]
Wavenumber = colnames(data_graph)
Wavenumber = as.vector(as.numeric(sub("X","", Wavenumber)))
length(Wavenumber)
dim(Wavenumber)
Wavenumber
str(Wavenumber)

train_index = t(train_index)
dim(train_index)

#transpose
data_graph = t(data_graph)
colnames(data_graph)<- train_index
dim(data_graph)
data_graph[,236] <- Wavenumber
colnames(data_graph)[236] <- "Wavenumber"
head(data_graph)[1:5, 230:236]

library(reshape2)
library(dplyr)

data_melt <- melt(data_graph, id.vars = Wavenumber, variable.name = "Sample_Num", value.name = "Absorbance")

head(data_melt)
data_melt[,1] <- as.numeric(sub("X", "", data_melt[,1]))
colnames(data_melt)[1] <- "Wavenumber"
colnames(data_melt)[2] <- "Sample_Num"
dim(data_melt)
161915/235

library(ggplot2)

2350*25

ggplot(data=data_melt[1:58750,], aes(x=Wavenumber, y=Absorbance, color = as.factor(Sample_Num))) +
  theme_bw() +
  geom_line(show.legend = FALSE, size = 0.15)

dim(data_melt)
data_melt <- data_melt[(data_melt[,1] != data_melt[,3]),]

head(data_graph)

245+106#351 pure strawberry
444+188#632 non-strawberry
351+632
head(train)[1:5, 230:237]
table(train$strawberry)
table(test$strawberry)
write.csv(pls_20_pred_test, "PLS_20_Test_Predictions.csv")
