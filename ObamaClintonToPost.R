########################################################
### Case: 2008 Democratic Primaries - Clinton vs. Obama
########################################################
source("DataAnalyticsFunctions.R")

# read data into R
election_data <- read.csv("ElectionDataAlone.csv")

# Next use the function summary to inspect the data
summary(election_data)

##############################################
# Cleaning up the data
# Write a function that replaces NAs with the mean of the non-missing data 
# in the column. This function can be called for different data sets to 
# impute the data.
impute_data <- function(vec, mn) {
  ifelse(is.na(vec), mn, vec)
}
# Find the means for all the numeric columns. 
# The function sapply automatically runs the mean function 
# (specified as second argument) on the columns 10 through 41. 
# The means are then saved in the vector named train_data_mean. 
# We use the argument na.rm=TRUE to ask the function to ignore NA entries.
data_mean <- sapply(election_data[,10:41],mean, na.rm=TRUE)

# Run this command to look at means for all the columns we found by running the sapply function
(data_mean)

# Impute the missing data. Loop through all the rows and 
# for each column call the function impute_train_data.
for(i in 10:41) {
  election_data[,i]<-impute_data(election_data[,i],data_mean[i-9])
}
# Run the summary function again. Now you see that no demographic/county columns have NA entries.
summary(election_data)

# Create two separate data sets from the data in electionData.
election_data$ElectionDate <- as.Date(election_data$ElectionDate, format="%m/%d/%Y")
election_data_train <- election_data[election_data$ElectionDate < as.Date("2/19/2008", format="%m/%d/%Y"), ]
election_data_test <- election_data[election_data$ElectionDate >= as.Date("2/19/2008", format="%m/%d/%Y"), ]

install.packages("ggplot2")
library(ggplot2)

max_perc = 60
d <- data.frame(Percent=rep(0,max_perc), Likelihood=rep(0,max_perc))
for (i in 1:max_perc){
  da <- subset(election_data_train, subset=Black>=i)
  d[i,1] = i
  d[i,2] = mean(da$Obama_wins)
}
ggplot() +
  geom_line(d, mapping=aes(x=Percent, y=Likelihood)) +
  ggtitle('Obama Winning Likelihood Based on Black Percentage')



# If you want to write these data sets back out into spreadsheets, 
# use the following "write" commands in R.
# write.csv(electionDataTrain, "electionDataTrain.csv")
# write.csv(electionDataTest, "electionDataTest.csv")

##########################################################
### End of Data Cleaning up
##########################################################
#
# Create some possible variables that might be of interest.
# (things we might like to predict in a regression using the demographic information). 
# These variables directly become a part of our data set election_data_train. You can use the command names(election_data_train) to see that these are added as columns in our data set.
election_data_train$Obama_margin <- election_data_train$Obama - election_data_train$Clinton
election_data_train$Obama_margin_percent <- 100*election_data_train$Obama_margin/election_data_train$TotalVote
election_data_train$Obama_wins <- ifelse(election_data_train$Obama_margin >0, 1,0)
names(election_data_train)
###
### Based on the data, to account for the size of possible delegates on each county
### we will work with election_data_train$Obama_margin_percent to be the target out models.
###

###
### Question 1: Provide a visualization (very flexible format, 
### it does not need to be related to the election)
### 

max_perc = 82.8
d <- data.frame(Percent=rep(0,max_perc), Likelihood=rep(0,max_perc))
for (i in 1:max_perc){
  da <- subset(election_data_train, subset=Black>=i)
  d[i,1] = i
  d[i,2] = mean(da$Obama_wins)
}
ggplot() +
  geom_line(d, mapping=aes(x=Percent, y=Likelihood)) +
  ggtitle('Obama Winning Likelihood Based on Percentage of Black Population') + xlab("Percentage of Black Population") + ylab("Likelihood of Obama Winning")





###
### Question 2: Prediction. No additional script beyond the cleaning up the data
### provided above. (Hint: FIPS variable bahaves as an identifier for each observation
### so you might not want to include it in a linear regression.)
###

###
### Question 3: Keep in mind that unsupervised learning 
### is used to explore the data. Feel free to consider only a subset of the 
### demographic variables. 
###

#Applying PCA
installpkg("plfm")
library(plfm)

Labels_Data<- labels(election_data_train[1,])
Labels_Data

Data_PCA<- subset(election_data_train, select =  - c(County, State,FIPS, Region,ElectionDate,
                                                     ElectionType,TotalVote,
      
                                                    
                                                    Clinton,Obama,Obama_wins,Obama_margin_percent,Obama_margin))
Data_PCA
pca.electionData<- prcomp(Data_PCA, scale=TRUE)

ncol(Data_PCA)
ncol(election_data_train)
pca.electionData


##Importance of components
summary(pca.electionData)

library(ggplot2)
plot(pca.electionData,main="PCA: Variance of the Votes Explained by Factors", xlab = "Factors")
mtext(side=1, "Factors Affecting the Votes",  line=1, font=2)


##Drawing a biplot

install.packages("ggfortify")
library(ggfortify)
autoplot(stats::prcomp(Data_PCA, scale=TRUE), label = FALSE, loadings.label = TRUE, main = "BiPlot : Variables of PC1 and PC2")


#PC score of each vote
prediction_Election<-predict(pca.electionData)
prediction_Election
#Picking the first four PC scores to interpret the votes
#PC1 Vs PC2
plot(prediction_Election[,1:2], pch=21,  main="PC1 and PC2 Impact on Votes")


plot(pca.electionData$rotation[,1:4], bg="black", cex=1, pch=21,  main="Loadings Plot")
text(pca.electionData$rotation[,1:4],             
     labels=rownames(pca.electionData$rotation))



#PC3 Vs PC4
plot(prediction_Election[,3:4], pch=21,  main="PC3 and PC4 Impact on Votes")




##calculating variance explained by each principal component
Var_Electiondata<-pca.electionData$sdev^2 / sum(pca.electionData$sdev^2)
Var_Electiondata
##Creating a scree plot:  plot that displays the total variance explained by each principal component – to visualize the results of PCA
Var_Electiondata_X<-Var_Electiondata[1:15]
Var_Electiondata_X

qplot (c(1:15),Var_Electiondata_X) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Total Variance in the Dataset Explained by Each Principal Component") +
  ylim(0, 0.3)


#### trying out the 2dPCA plot from the 30 feature dataset
election_data_train$Obama_wins
install.packages("factoextra")

library("factoextra")

Who_Won <- c()
for (i in 1:1737) {
  
  if (election_data_train$Obama_wins[i]>0) {
    a = "Obama"
  }
  else  {
    a = "Clinton"
  }
  Who_Won<-append(Who_Won,a)
}

Who_Won

election_data_train_Viz<-data.frame(election_data_train,Who_Won)
head(election_data_train_Viz)

pca.electionData
fviz_pca_ind(pca.electionData, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = election_data_train_Viz$Who_Won, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Candidate") +
  ggtitle("2D PCA-plot from 32 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))

##Interpreting the four factors by looking at loadings (AKA
##correlation of each factor with the original feature)
loadings <- pca.electionData$rotation[,1:10]
loadings

### For each factor lets display the top features that 
### are responsible for 3/4 of the squared norm of the loadings

#PC1
L1<-loadings[order(abs(loadings[,1]), decreasing=TRUE)[1:27],1]
loadingfit <- lapply(1:27, function(k) ( t(L1[1:k])%*%L1[1:k] - 3/4 )^2)
L1[1:which.min(loadingfit)]

#PC2
L2<-loadings[order(abs(loadings[,2]), decreasing=TRUE)[1:27],2]
loadingfit <- lapply(1:27, function(k) ( t(L2[1:k])%*%L2[1:k] - 3/4 )^2)
L2[1:which.min(loadingfit)]

#PC3

L3<-loadings[order(abs(loadings[,3]), decreasing=TRUE)[1:27],3]
loadingfit <- lapply(1:27, function(k) ( t(L3[1:k])%*%L3[1:k] - 3/4 )^2)
L3[1:which.min(loadingfit)]


#PC4

L4<-loadings[order(abs(loadings[,4]), decreasing=TRUE)[1:27],4]
loadingfit <- lapply(1:27, function(k) ( t(L4[1:k])%*%L4[1:k] - 3/4 )^2)
L4[1:which.min(loadingfit)]


L4
###
### Question 4(a) impact of changing hispanic demographic
###
#### Model with 1771 controls to measure the impact of 10% larger Hispanic demographic
y <- election_data_train$Obama_margin_percent
x <- model.matrix( Obama_margin_percent ~ .-Hispanic-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )
d <- election_data_train$Hispanic
####
#### Feel free to compare/contrast your results with the following simple regression model
#### 
HispanicSimple <- glm( Obama_margin_percent ~ Hispanic, data = election_data_train )
summary(HispanicSimple)

####
### Question 4(b) impact of changing black demographic
####
#### Model with 1771 controls to measure the impact of 10% larger Black demographic
y <- election_data_train$Obama_margin_percent
x <- model.matrix( Obama_margin_percent ~ .-Black-Obama_wins-Obama_margin-FIPS-ElectionDate-TotalVote-Clinton-Obama, data = election_data_train )
d <- election_data_train$Black
####
#### Feel free to compare/contrast your results with the following simple regression model
#### 
BlackSimple <- glm( Obama_margin_percent ~ Black, data = election_data_train )
summary(BlackSimple)
####


####
#### Question 5: No additional R code. Keep in mind that you can build upon your previous 
#### analysis or develop new analysis.
####

cols_to_drop <- c("County", "State", "Region", "FIPS", "ElectionDate", "ElectionType", 
                  "Obama_wins", "TotalVote", "Clinton", "Obama", "Obama_margin")
# Our new dataset without the columns specified above. 
train_data <- election_data_train[,!(names(election_data_train) %in% cols_to_drop)]

install.packages("randomForest")
library(randomForest)

### Clearly Random Forest is the winner. Fitting the model using Random Forest
RF_Final <- randomForest(Obama_margin_percent ~ ., data = train_data, importance = TRUE)

### Predicting spread using Random Forest

test_data <- election_data_test[, !(names(election_data_test) %in% names(election_data_test[, c(1:9)]))]

predict_test <- predict(RF_Final, newdata = test_data)
predict_test<-data.frame(predict_test)

predict_test<-data.frame(test_data,predict_test)


Who_Won <- c()
for (i in 1:1131) {
  
  if (predict_test$predict_test[i]>0) {
    a = "0"
  }
  else  {
    a = "1"
  }
  Who_Won<-append(Who_Won,a)
}

Who_Won

predict_test<-cbind(predict_test,Who_Won)
predict_test<-cbind(predict_test,election_data_test$County, election_data_test$State)

hist(predict_test$predict_test)

ClintonWins<-aggregate(as.numeric(predict_test$Who_Won),list(predict_test$`election_data_test$State`),sum)
Total<- aggregate(predict_test$`election_data_test$County`,list(predict_test$`election_data_test$State`),length)
Statewise<-data.frame(ClintonWins,Total)
names(Statewise)[1]<-paste("State")
names(Statewise)[2]<-paste("Clinton_Wins")
names(Statewise)[3]<-paste("State")
names(Statewise)[4]<-paste("Total_County")
b<-c(Statewise$Total_County-Statewise$Clinton_Wins)
Statewise<-cbind(Statewise,b)
names(Statewise)[5]<-paste("Obama_Wins")


##library(dplyr)

##predict_test2<-filter(predict_test)
