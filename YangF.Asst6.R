# QAI Assignment 6

# Problem 2
# b)
election_data <- read.csv("C:\\Users\\Funing\\Desktop\\Summer 2022\\QAI & Stats\\QAI\\Election.csv")
summary(election_data)
dim(election_data)
head(election_data)

# adding columns for logged values of the variables
election_data$logged_bush2000 <- log(election_data$Bush2000) # to the top
election_data$logged_buchanan2000 <- log(election_data$Buchanan2000)


# c)
no_palmbeach <- election_data[-c(50),]
splineoutput <- smooth.spline(x=no_palmbeach$Bush2000,y=no_palmbeach$Buchanan2000)

# a scatterplot of the 2 variables
plot(no_palmbeach$Bush2000,no_palmbeach$Buchanan2000,ylim=c(0,4000))
points(splineoutput$x,splineoutput$y,type="l")
#plot the particular palm beach point
points(election_data[c(50),]$Bush2000,election_data[c(50),]$Buchanan2000) 

# predict number of votes for Buchanan
predict(splineoutput, election_data[c(50),]$Bush2000)


# d) Choose one or two other variables that you think could be useful for predicting votes
# for Buchanan. Importantly, remember that you can (and often must!) manipulate the
# information provided in the columns in the data set to generate the predictors that you
# believe will be most interesting. Which variables are you going to investigate?

# variables I'll investigate: gender ratio between in each state
election_data$gender_ratio <- election_data$females/election_data$males

# predict votes for Buchannan based on gender ratio
no_palmbeach_d <- election_data[election_data$County!="Palm Beach",]
splineobject <- smooth.spline(x=no_palmbeach_d$gender_ratio,y=no_palmbeach_d$Buchanan2000)
predict(splineobject,election_data[election_data$County=="Palm Beach",]$gender_ratio)

# e) Excluding Palm Beach, create a regression tree that predicts the number of votes for
# Buchanan based on the predictors you chose. Display your tree model graphically.
# What does your tree model predict for the number of votes for Buchanan in Palm
# Beach County?

# outcome variable: election_data$Buchanan2000
# dataset: 
library(partykit)
#buchanan_factors <- as.factor(no_palmbeach_d$Buchanan2000)
tree<-ctree(Buchanan2000~gender_ratio, data=no_palmbeach_d)
plot(tree)
predict(tree,new=election_data[election_data$County=="Palm Beach",]) 


# f) Create a parametric linear regression model that predicts the number of votes for
# Buchanan based only on the number of votes for Bush. Consider whether it is
# appropriate to transform. Add the predictions and prediction bands from this model to
# the plot you created in 2c

# check assumptions: linerity, normality, equal variance
# add the new columns of logged values
# election_data$logged_bush2000 <- log(election_data$Bush2000) # to the top
# election_data$logged_buchanan2000 <- log(election_data$Buchanan2000)
lm_model <- lm(logged_buchanan2000~logged_bush2000,data=no_palmbeach_d)
prediction_f <- predict(lm_model,new=election_data[election_data$County=="Palm Beach",],interval="prediction")
exp(prediction_f) # transform back through taking the exp. 
points(no_palmbeach_d$Bush2000,prediction_f[,1],type="l",col='red')



# g) Create a multiple regression model that predicts the number of votes for Buchanan
#based on the number of votes for Bush and at least one other predictor
# residual v. fitted values plot
lm_model_multi <- lm(logged_buchanan2000~logged_bush2000+gender_ratio,data=no_palmbeach_d)
prediction_g <- predict(lm_model_multi,new=election_data[election_data$County=="Palm Beach",],interval="prediction")
exp(prediction_g)



# Problem 3
# a)
anes <- read.csv(file = "C:\\Users\\Funing\\Desktop\\Summer 2022\\QAI & Stats\\QAI\\anes.csv")
#Consider the variable vote16, the variable ftfeminists, and the other variable 
#you cleaned for assignment 4. For each of these variables, specify the number of
#missing values and briefly explain which kind of missingness you think occurs, if any
#table.isNa
table(anes$vote16)
anes$vote16[anes$vote16==9] <- "NA"
table(anes$vote16) #1 NA
anes$ftfeminists[anes$ftfeminists==999] <- "NA"
table(anes$ftfeminists) #37 NA's


# b)
anes$irritated[anes$irritated==9] <- "NA"
set.seed(22)
anes$feministclean <- anes$ftfeminists
anes$feministclean[anes$irritated==1 & anes$ftfeminists =="NA"]<-
  sample(anes$ftfeminists[anes$irritated==1 & anes$ftfeminists!="NA"], 
         length(anes$ftfeminists[anes$ftfeminists =="NA" & anes$irritated==1]),
         replace=TRUE)
anes$feministclean[anes$irritated==2 & anes$ftfeminists =="NA"]<-
  sample(anes$ftfeminists[anes$irritated==2 & anes$ftfeminists!="NA"], 
         length(anes$ftfeminists[anes$ftfeminists =="NA" & anes$irritated==2]),
         replace=TRUE)
anes$feministclean[anes$irritated==3 & anes$ftfeminists =="NA"]<-
  sample(anes$ftfeminists[anes$irritated==3 & anes$ftfeminists!="NA"], 
         length(anes$ftfeminists[anes$ftfeminists =="NA" & anes$irritated==3]),
         replace=TRUE)
anes$feministclean[anes$irritated==4 & anes$ftfeminists =="NA"]<-
  sample(anes$ftfeminists[anes$irritated==4 & anes$ftfeminists!="NA"], 
         length(anes$ftfeminists[anes$ftfeminists =="NA" & anes$irritated==4]),
         replace=TRUE)
anes$feministclean[anes$irritated==5 & anes$ftfeminists =="NA"]<-
  sample(anes$ftfeminists[anes$irritated==5 & anes$ftfeminists!="NA"], 
         length(anes$ftfeminists[anes$ftfeminists =="NA" & anes$irritated==5]),
         replace=TRUE)

table(anes$feministclean)


# c) strategy: mean imputation
anes$valid <- anes$ftfeminists[anes$ftfeminists!="NA"]
mean(anes$valid)

anes$valid <- anes$ftfeministsanes$valid[anes$ftfeminists=="NA"]<-mean(anes$ftfeminists[anes$ftfeminists!="NA"])

anes$valid <- anes$ftfeminists
anes$valid[anes$valid=="NA"]<-999
anes$valid[anes$ftfeminists==999]<-mean(anes$ftfeminists[anes$ftfeminists!=999])

table(anes$valid)


# d) Create a visualization that uses whichever imputed version of ftfeminists you prefer
# to explore a question or relationship that interests you. If use variables besides
# ftfeminists, make sure to clean and/or impute as needed. Briefly explain what you
# wanted to learn and what you did learn from the visualization.





