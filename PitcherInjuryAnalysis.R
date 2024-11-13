######clear your working directory
rm(list=ls())

#####Installing packages and libraries
install.packages("lubridate")
install.packages("gtsummary")
library(lubridate)
library(ggplot2)
library(sandwich)
library(readr)
library(readxl)
library(tidyr)
library(dplyr)
library(gtsummary)
library(caret)
library(klaR)
library(gains)
library(pROC)
library(rpart)
library(rpart.plot)
library(forecast)


##Bring in pitcher stats and player injury data
InjuryHistory <- read_excel("99-2017InjuryData.xlsx")
PitcherStats <- read_excel("99-2016PitcherStats.xlsx")


############### Data Management ###############
############### Data Management ###############
############### Data Management ###############



##Rename variables in PitcherStats for merging with injury data
PitcherStats <- rename(PitcherStats, Player = Name)
PitcherStats <- rename(PitcherStats, TeamAbv = Team)


##Change injury data to date type
InjuryHistory$Date <- as.Date(InjuryHistory$Date, 
                              format = "%Y-%m-%d")


##Create new variable called season in injury data to coincide with season 
##in the pitcher stats dataframe
InjuryHistory$Season <- year(InjuryHistory$Date)


##Merge pitcher stats and injury history dataframes by player and season
Pitchers <- merge(PitcherStats,
                  InjuryHistory, by= c("Player","Season"), all.x=TRUE, all.y=FALSE)


##Get count of how many NA vales exist for pitches variable in Pitchers
##This is the count of how many pitches were thrown in that season. 
sum(is.na(Pitchers$Pitches))


##Creating new column called injury location by searching the text in
##the injury type column for keywords related to location of injury. 
Pitchers$InjuryLocation <- ifelse(grepl("elbow",Pitchers$Injury_Type),"elbow",
                                  ifelse(grepl("shoulder",Pitchers$Injury_Type),"shoulder",
                                         ifelse(grepl("abdominal",Pitchers$Injury_Type),"abdominal",
                                                ifelse(grepl("foot",Pitchers$Injury_Type),"foot",
                                                       ifelse(grepl("groin",Pitchers$Injury_Type),"groin",
                                                              ifelse(grepl("ankle",Pitchers$Injury_Type),"ankle",
                                                                     ifelse(grepl("finger",Pitchers$Injury_Type),"hand",
                                                                            ifelse(grepl("bicep",Pitchers$Injury_Type),"arm",
                                                                                   ifelse(grepl("leg",Pitchers$Injury_Type),"leg",
                                                                                          ifelse(grepl("hand",Pitchers$Injury_Type),"hand",
                                                                                                 ifelse(grepl("blister",Pitchers$Injury_Type),"hand",
                                                                                                        ifelse(grepl("hip",Pitchers$Injury_Type),"hip",
                                                                                                               ifelse(grepl("knee",Pitchers$Injury_Type),"knee",
                                                                                                                      ifelse(grepl("wrist",Pitchers$Injury_Type),"wrist",
                                                                                                                             ifelse(grepl("heel",Pitchers$Injury_Type),"foot",
                                                                                                                                    ifelse(grepl("shin",Pitchers$Injury_Type),"shin",
                                                                                                                                           ifelse(grepl("thumb",Pitchers$Injury_Type),"hand",
                                                                                                                                                  ifelse(grepl("brachialis",Pitchers$Injury_Type),"elbow",
                                                                                                                                                         ifelse(grepl("toe",Pitchers$Injury_Type),"foot",
                                                                                                                                                                ifelse(grepl("calf",Pitchers$Injury_Type),"leg",
                                                                                                                                                                       ifelse(grepl("forearm",Pitchers$Injury_Type),"arm",
                                                                                                                                                                              ifelse(grepl("leg",Pitchers$Injury_Type),"leg",
                                                                                                                                                                                     ifelse(grepl("oblique",Pitchers$Injury_Type),"oblique",
                                                                                                                                                                                            ifelse(grepl("rib",Pitchers$Injury_Type),"ribs",
                                                                                                                                                                                                   ifelse(grepl("rotator cuff",Pitchers$Injury_Type),"shoulder",
                                                                                                                                                                                                          ifelse(grepl("back",Pitchers$Injury_Type),"back",
                                                                                                                                                                                                                 ifelse(grepl("neck",Pitchers$Injury_Type),"neck",
                                                                                                                                                                                                                        ifelse(grepl("cervical",Pitchers$Injury_Type),"neck",
                                                                                                                                                                                                                               ifelse(grepl("hamstring",Pitchers$Injury_Type),"leg",
                                                                                                                                                                                                                                      ifelse(grepl("concussion",Pitchers$Injury_Type),"head", "other"))
                                                                                                                                                                                                                        ))))))))))))))))))))))))))))



##Creating new column called injury severity by searching the text in
##the injury type column for keywords related to severity of injury.                                         
Pitchers$InjurySeverity <- ifelse(grepl("strain",Pitchers$Injury_Type),"strain",
                                  ifelse(grepl("sprain",Pitchers$Injury_Type),"sprain",
                                         ifelse(grepl("fracture",Pitchers$Injury_Type),"fracture",
                                                ifelse(grepl("torn",Pitchers$Injury_Type),"tear",
                                                       ifelse(grepl("pull",Pitchers$Injury_Type),"pull",
                                                              ifelse(grepl("bruise",Pitchers$Injury_Type),"bruise",
                                                                     ifelse(grepl("blister",Pitchers$Injury_Type),"blister",
                                                                            ifelse(grepl("tendinitis",Pitchers$Injury_Type),"tendinits",
                                                                                   ifelse(grepl("broke",Pitchers$Injury_Type),"fracture",
                                                                                          ifelse(grepl("disloc",Pitchers$Injury_Type),"dislocation",
                                                                                                 ifelse(grepl("herniat",Pitchers$Injury_Type),"herniation",
                                                                                                        ifelse(grepl("infec",Pitchers$Injury_Type),"infection",
                                                                                                               ifelse(grepl("inflam",Pitchers$Injury_Type),"inflamation",
                                                                                                                      ifelse(grepl("irrit",Pitchers$Injury_Type),"irritation",
                                                                                                                             ifelse(grepl("lacerat",Pitchers$Injury_Type),"laceration",
                                                                                                                                    ifelse(grepl("imping",Pitchers$Injury_Type),"impingement",
                                                                                                                                           ifelse(grepl("spasm",Pitchers$Injury_Type),"spasms",
                                                                                                                                                  ifelse(grepl("tendin",Pitchers$Injury_Type),"tendinitis",
                                                                                                                                                         ifelse(grepl("surgery",Pitchers$Injury_Type),"surgery",
                                                                                                                                                                ifelse(grepl("rupture",Pitchers$Injury_Type),"rupture",
                                                                                                                                                                       ifelse(grepl("separ",Pitchers$Injury_Type),"separation",
                                                                                                                                                                              ifelse(grepl("sore",Pitchers$Injury_Type),"soreness",
                                                                                                                                                                                     ifelse(grepl("contusion",Pitchers$Injury_Type),"bruise",
                                                                                                                                                                                            ifelse(grepl("tight",Pitchers$Injury_Type),"tightness", "other"
                                                                                                                                                                                            ))))))))))))))))))))))))

##Getting a count of how many observations of injury location and severity
##could not be reconsiled due to wording in injury type field. 
length(which(Pitchers$InjuryLocation =="other"))                       
length(which(Pitchers$InjurySeverity == "other"))                                            

##Getting a count of how many pitchers in the pitchers dataframe did not
##sustain an injury in a season. 
sum(is.na(Pitchers$Injury))

##Creating dataframe that includes just the players and/or years a player
#####did not sustain an injury
NoInjury <- Pitchers[is.na(Pitchers$Injury),]

##Creating a dataframe of complete cases of injury location for
##pitchers who sustained an injury 
InjuredPitchers <- subset(Pitchers, InjuryLocation != "other")

##Getting a count of how many observations in the Injured Pitchers dataframe
##I could not pull the injury severity for. 
length(which(InjuredPitchers$InjurySeverity == "other"))                                           

##Merging dataframes of players that did and did not sustain an injury in 
##a particular season. 
CompletePitchers <- merge(NoInjury, InjuredPitchers, all.x = TRUE, all.y = TRUE)                           


##Adding column to indicate whether a player sustained an injury or not
##and changing it to a factor. 
CompletePitchers$Injured <- ifelse(is.na(CompletePitchers$Injury),0,1)

CompletePitchers$Injured <- as.factor(CompletePitchers$Injured)

##Getting clear picture of range of values of different variables for 
##binning
summary(CompletePitchers)


##Testing range of values binning by quartiles for age. 
quantileTest <- cut(CompletePitchers$Age,
                    breaks=c(quantile(CompletePitchers$Age, probs = seq(0, 1, by = 0.25))))

summary(quantileTest)


##Adding column for binned age ranges
CompletePitchers$AgeRange <- cut(CompletePitchers$Age,
                                 breaks=c(quantile(CompletePitchers$Age, probs = seq(0, 1, by = 0.25))),
                                 labels = FALSE)


##Adding column for pitches per inning pitched
CompletePitchers$PitchesPerIP <- CompletePitchers$Pitches/CompletePitchers$IP

##Adding column for pitches per game
CompletePitchers$PitchesPerG <- CompletePitchers$Pitches/CompletePitchers$G

##Adding column for pitches per batter faced
CompletePitchers$PitchesPerBatter <- CompletePitchers$Pitches/CompletePitchers$TBF


##Getting idea of fastball velocity range
summary(CompletePitchers$vFA)

##Adding column for binned fastball velocity ranges
CompletePitchers$vFARange <- cut(CompletePitchers$vFA,
                                 breaks=c(quantile(CompletePitchers$vFA, probs = seq(0, 1, by = 0.25),
                                                   na.rm = TRUE)),labels = FALSE)

##Adding column for binned pitches per game ranges
CompletePitchers$PitchesPerGRange <- cut(CompletePitchers$PitchesPerG,
                                         breaks=c(quantile(CompletePitchers$PitchesPerG, probs = seq(0, 1, by = 0.25),
                                                           na.rm = TRUE)),labels = FALSE)

##Adding column for binned pitches ranges
CompletePitchers$PitchesRange <- cut(CompletePitchers$Pitches,
                                     breaks=c(quantile(CompletePitchers$Pitches, probs = seq(0, 1, by = 0.25),
                                                       na.rm = TRUE)),labels = FALSE)

##Adding column for binned games ranges
CompletePitchers$GRange <- cut(CompletePitchers$G,
                               breaks=c(quantile(CompletePitchers$G, probs = seq(0, 1, by = 0.25),
                                                 na.rm = TRUE)),labels = FALSE)

##Adding column for binned pitches per inning ranges
CompletePitchers$PitchesPerIPRange <- cut(CompletePitchers$PitchesPerIP,
                                          breaks=c(quantile(CompletePitchers$PitchesPerIP, probs = seq(0, 1, by = 0.25),
                                                            na.rm = TRUE)),labels = FALSE)



############### Regression Analysis ###############
############### Regression Analysis ###############
############### Regression Analysis ###############


sum(is.na(CompletePitchers$DL_length))


##Turn off scientific notation
options(scipen = 999)


##Linear Model 1
linearModel1 <- lm(DL_length ~ Age + vFA + G + IP + Pitches + FApercent + 
                     PitchesPerIP + PitchesPerG, data=CompletePitchers)
summary(linearModel1)

tbl_regression(linearModel1, intercept = TRUE)

##Linear Model 2
linearModel2 <- lm(as.numeric(Injured) ~ Age + vFA + G + IP + Pitches + 
                     FApercent + PitchesPerIP + PitchesPerG, data=CompletePitchers)
summary(linearModel2)

##Generate linear regression output table
tbl_regression(linearModel2, intercept = TRUE)

#produce residual vs. fitted plot and add horizontal line at 0
res <- resid(linearModel2)
plot(fitted(linearModel2), res)

abline(0,0)

##Linear Model 3
linearModel3 <- lm(as.numeric(Injured) ~ Age + G + Pitches + 
                     PitchesPerG, data=CompletePitchers)
summary(linearModel3)

##Linear Model 4
linearModel4 <- lm(as.numeric(Injured) ~ Age + Pitches + Age*Pitches,
                   data=CompletePitchers)
summary(linearModel4)

tbl_regression(linearModel4, intercept = TRUE)

##Linear Model 5
linearModel5 <- lm(as.numeric(Injured) ~ Age + PitchesPerG + Age*PitchesPerG,
                   data=CompletePitchers)
summary(linearModel5)

##Linear Model 6
linearModel6 <- lm(as.numeric(Injured) ~ Age + PitchesPerG,
                   data=CompletePitchers)
summary(linearModel6)

##Linear Model 7
linearModel7 <- lm(as.numeric(Injured) ~ Age + FApercent + FSpercent + 
                     SIpercent + SLpercent + CUpercent + CHpercent,
                   data=CompletePitchers)
summary(linearModel7)

##Linear Model 8
linearModel8 <- lm(as.numeric(Injured) ~ Age + FSpercent + 
                     SLpercent + CUpercent, data=CompletePitchers)
summary(linearModel8)

##Linear Model 9
linearModel9 <- lm(as.numeric(Injured) ~ FSpercent + 
                     CUpercent, data=CompletePitchers)
summary(linearModel9)

##Turn scientific notation back on
options(scipen = 0)




############### Naive Bayes Model ###############
############### Naive Bayes Model ###############
############### Naive Bayes Model ###############



##Create new dataframe for Naive Bayes that includes just the target variable
##and the predictors identified from logistical regression. 
nBayes <- na.omit(CompletePitchers [,c('Injured','AgeRange','GRange',
                                       'PitchesRange','PitchesPerGRange')])
summary(nBayes)


set.seed(1)
nBayesMyIndex<- createDataPartition(nBayes$Injured, p=0.6, list=FALSE)
nBayesTrainSet <- nBayes[nBayesMyIndex,]
nBayesValidationSet <- nBayes[-nBayesMyIndex,]

#k-fold cross validation
myCtrl <- trainControl(method="cv", number=10)

##train the model
set.seed(1)
nb_fit <- train(Injured ~., data = nBayesTrainSet, method = "nb", trControl = myCtrl)
nb_fit

##validate the model
nb_class <- predict(nb_fit, newdata = nBayesValidationSet)
confusionMatrix(nb_class, nBayesValidationSet$Injured, positive = '1')


##create the gains table
nb_class_prob <- predict(nb_fit, newdata = nBayesValidationSet, type = 'prob')
nBayesValidationSet$Injured <- as.numeric(nBayesValidationSet$Injured)
gains_table <- gains(nBayesValidationSet$Injured, nb_class_prob[,2])
gains_table


##cumulative lift chart
plot(c(0, gains_table$cume.pct.of.total*sum(nBayesValidationSet$Injured)) ~ c(0, gains_table$cume.obs), 
     xlab = '# of cases', 
     ylab = "Cumulative", 
     type = "l")
lines(c(0, sum(nBayesValidationSet$Injured))~c(0, dim(nBayesValidationSet)[1]), col="red", lty=2)


##decile-wise lift chart
barplot(gains_table$mean.resp/mean(nBayesValidationSet$Injured), 
        names.arg=gains_table$depth, 
        xlab="Percentile", 
        ylab="Lift", 
        ylim=c(0,1.5), 
        main="Decile-Wise Lift Chart")


##receiver operator curve
roc_object <- roc(nBayesValidationSet$Injured, 
                  nb_class_prob[,2])
plot.roc(roc_object)
auc(roc_object)



############### Regression Tree Model ###############
############### Regression Tree Model ###############
############### Regression Tree Model ###############



regTree <- na.omit(CompletePitchers [,c('Age','DL_length','Pitches',
                                        'PitchesPerG', 'InjuryLocation','InjurySeverity')])

##Changing injury location and injury severity from character to factor type
regTree$InjuryLocation <- as.factor(regTree$InjuryLocation)
regTree$InjurySeverity <- as.factor(regTree$InjurySeverity)


##partition the data set
set.seed(1)
regTreeMyIndex <- createDataPartition(regTree$DL_length, p=0.7, list=FALSE)
regTreeTrainSet <- regTree[regTreeMyIndex,]
regTreeValidationSet <- regTree[-regTreeMyIndex,]

##Default tree
set.seed(1)
default_tree <- rpart(DL_length ~., data = regTreeTrainSet, method = "anova")
summary(default_tree)

##Visualize the tree
prp(default_tree, type = 1, extra = 1, under = TRUE)

##Create the full tree
set.seed(1)
full_tree <- rpart(DL_length ~ ., data = regTreeTrainSet, method = "anova", cp = 0, minsplit = 2, minbucket = 1)
prp(full_tree, type = 1, extra = 1, under = TRUE)

##Identify the value of 'cp'
printcp(full_tree)

##Set cp 
pruned_tree <- prune(full_tree, cp = 0.1)
prp(pruned_tree, type = 1, extra = 1, under = TRUE)

##Predict the average injured list length of the observations in the 
##validation set
predicted_value <- predict(pruned_tree, regTreeValidationSet)

##Evaluate performance
accuracy(predicted_value, regTreeValidationSet$DL_length)



############### Data Visualization ###############
############### Data Visualization ###############
############### Data Visualization ###############



RevenuePercent <- read_excel("Player Revenue Percent.xlsx")

RevenuePercent$Year <- as.character(RevenuePercent$Year)


##Stacked bar chart showing split of revenue between MLB teams and players
ggplot(data=RevenuePercent, aes(fill=Category, y=RevenueSplit, x=Year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_y_continuous(breaks = seq(1, 12, by = 2)) +
  scale_fill_manual(values = c("lightblue", "blue")) +
  labs(title = "MLB Revenue Split", x = "Year", y = "Revenue (in Billions)") +
  geom_text(aes(label=ifelse(is.na(PlayerPercent),"" , paste(PlayerPercent, 
                                                             "%"))), hjust=.5, vjust = -3 ,color="black", size=3, check_overlap = TRUE) 




