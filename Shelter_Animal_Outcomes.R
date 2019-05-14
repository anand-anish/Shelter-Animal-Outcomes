## Author : Anish Anand
## Written on December,2016

#clearing environment
rm(list=ls())

#setting work directory
setwd("C:\\Users\\Anish Anand\\Desktop\\kaggle\\animal_challenge")

#installing and extracting packages
library(lubridate)
library(rpart)
library(stringr)
library(xgboost)
library(ggplot2)
library(dplyr)

#uploading the training and testing files
train<-read.csv("C:\\Users\\Anish Anand\\Desktop\\kaggle\\animal_challenge\\train.csv")
test<-read.csv("C:\\Users\\Anish Anand\\Desktop\\kaggle\\animal_challenge\\test.csv")
test_ID <- test$ID

#basic exploratory analysis of the data we are dealing with
summary(train)
str(train)
names(train)
names(test)
#merging the datasets
test$AnimalID <- NA
test$ID <- NULL #rememebr that at the end while dividing we have to divide the total dataset in (1:26729)and(26730:sum(train+test))
test$OutcomeType<-NA
test$OutcomeSubtype<-NA

combi<-rbind(train,test)

#creating new variables
combi$HasName<-ifelse(as.character(combi$Name)=="",0,1);
combi$name_len <- sapply(as.character(combi$Name),FUN=nchar);  #nchar() function is used to count no. of characters

combi$DateTime2<-as.character(combi$DateTime)

for (i in 1:nrow(combi)){
  combi$Year[i]=strsplit(combi$DateTime2[i],split='[-: ]')[[1]][1]
  combi$Month[i]=strsplit(combi$DateTime2[i],split='[-: ]')[[1]][2]
  combi$Day[i]=strsplit(combi$DateTime2[i],split='[-: ]')[[1]][3]
  combi$Hour[i]=strsplit(combi$DateTime2[i],split='[-: ]')[[1]][4]
}
combi$DateTime2 <- NULL 

#####aliter for above method to seperate date time#####
#combi$DateTime <- as.POSIXct(test$DateTime)
#combi$year <- year(combi$DateTime)
#combi$month <- month(combi$DateTime)
#combi$wday <- wday(combi$DateTime)
#combi$hour <- hour(combi$DateTime)
######################################################
combi$Weekday<-wday(combi$DateTime) #using lubridate package

# Binning Variables(since Time of day may also be useful)
combi$Hour<-as.numeric(combi$Hour)
combi$TimeofDay <- ifelse(combi$Hour > 05 & combi$Hour <=10, 'morning',
                          ifelse(combi$Hour > 10 & combi$Hour <= 15, 'midday',
                                 ifelse(combi$Hour > 15 & combi$Hour <=20, 'lateday', 'night')))
# Put factor levels into the order we want
combi$TimeofDay <- factor(combi$TimeofDay,levels = c('morning', 'midday', 'lateday', 'night'))

#animalID 
combi$AnimalID<-as.character(combi$AnimalID)
combi$AnimalID<- gsub("[A-Z]","",combi$AnimalID) #since all columns contained A,hence not of use.We shall keep the numeric ids
combi$AnimalID<-as.integer(combi$AnimalID)

####age upon outcomes######
# Write a function to convert age outcome to numeric age in days
convert <- function(x){
  split <- strsplit(as.character(x), split=" ")
  value <- split[[1]][2]
  if (grepl("year", value)){         
    days <- 356
  } else if (grepl("month", value)){ 
    days <- 30
  } else if (grepl("week", value)){
    days <- 7
  } else
    days <- 1
  age_Days <- as.numeric(split[[1]][1]) * days
  return(age_Days)
}

combi$AgeuponOutcome <- sapply(combi$AgeuponOutcome, FUN=convert)

summary(combi$AgeuponOutcome) #24 NA's
which(combi$AgeuponOutcome %in% NA) #index locations,we find that only 5 NA's in testing set
#replace NA's by median

combi$AgeuponOutcome[is.na(combi$AgeuponOutcome==TRUE)]<-median(combi$AgeuponOutcome,na.rm=T)
names(combi)[names(combi)=="AgeuponOutcome"]<-"AgeinDays"

##################################################################################
# Set the row with missing sex label to unknown
str(combi$SexuponOutcome) 
table(combi$SexuponOutcome)
combi$SexuponOutcome<-as.character(combi$SexuponOutcome)
combi$SexuponOutcome[which(combi$SexuponOutcome == "")] = 'Unknown'
combi$SexuponOutcome<-as.factor(combi$SexuponOutcome)

#Breed variable has way too many levels;
#1678 to be exact. I'm going to deal with this in part by contrasting mixes with non-mixes
str(combi$Breed)
# Use "grepl" to look for "Mix"
combi$Mix_breed <- ifelse(grepl('Mix', combi$Breed), 1, 0)
# Split on "/" and remove " Mix" to simplify Breed
combi$SimpleBreed <- sapply(as.character(combi$Breed), 
                            function(x) gsub(' Mix', '', strsplit(x, split = '/')[[1]][1]))

#got 231 levels from 1678 levels
table(combi$SimpleBreed,combi$AnimalType)
#I saw from some resources that terrier is a breed of typically small, wiry, very active and fearless dogs,so further manipulation,since we know these dogs are well preffered by people

combi$breed2ndletter <- sapply(as.character(combi$SimpleBreed),FUN= function(x) {strsplit(x, split = ' ')}[[1]][2])
combi$breed3rdletter <- sapply(as.character(combi$SimpleBreed),FUN= function(x) {strsplit(x, split = ' ')}[[1]][3])
combi$breed4thletter <- sapply(as.character(combi$SimpleBreed),FUN= function(x) {strsplit(x, split = ' ')}[[1]][4])

combi$breed3rdletter[is.na(combi$breed3rdletter=="TRUE")]<-0
combi$breed4thletter[is.na(combi$breed4thletter=="TRUE")]<-0

combi$newbreed<- ifelse(combi$breed2ndletter=="Terrier"|combi$breed3rdletter=="Terrier"|combi$breed4thletter=="Terrier","Terrier",as.character(combi$SimpleBreed))
combi$breed2ndletter<-NULL
combi$breed3rdletter<-NULL
combi$breed4thletter<-NULL
########################################################################
# Use strsplit to grab the first color
combi$SimpleColor <- sapply(as.character(combi$Color),function(x) strsplit(x, split = '/| ')[[1]][1])
levels(factor(combi$SimpleColor))
#########################################################################
# We know that Animals are much more likely to be adopted if they've been neutered
# Use "grepl" to look for "Intact",i.e neutered male or spayed female=0,intact=1
combi$Intact <- ifelse(grepl('Intact', combi$SexuponOutcome), 1,
                       ifelse(grepl('Unknown', combi$SexuponOutcome), 'Unknown', 0))

# Use "grepl" to look for sex
combi$Sex <- ifelse(grepl('Male', combi$SexuponOutcome), 'Male',
                    ifelse(grepl('Unknown', combi$SexuponOutcome), 'Unknown', 'Female'))  
##############################################################################
# Use rpart to predict the missing age values
age_fit <- rpart(AgeinDays ~ AnimalType + Sex + Intact + SimpleBreed + HasName, 
                 data = combi[!is.na(combi$AgeinDays), ], 
                 method = 'anova')

# Impute predicted age values where missing using "predict"
combi$AgeinDays[is.na(combi$AgeinDays)] <- predict(age_fit, combi[is.na(combi$AgeinDays), ])

# see if everything is done?
sum(is.na(combi$AgeinDays))
#################################################################################
# Use the age variable to make a puppy/kitten variable
combi$Lifestage[combi$AgeinDays < 365] <- 'baby'
combi$Lifestage[combi$AgeinDays >= 365] <- 'adult'

combi$Lifestage <- as.factor(combi$Lifestage)
prop.table(table(combi$Lifestage,combi$OutcomeType),1)
# above result shows that baby animals are more likely to be adopted than adult animals.
#They are also more likely to be transferred and to have died.

combi$DateTime<-NULL
combi$Name<-NULL
combi$SexuponOutcome<-NULL
combi$SimpleBreed<-NULL
combi$newbreed<-NULL

#################################################################################

#indicator for subtype (expanding the factors of OutcomeSubtype into individual fields, like dcast(long->wide format))
combi_subtype<-as.character(combi$OutcomeSubtype)
combi_subtype<-unique(combi_subtype)
for(i in combi_subtype[2:17]){        #SINCE INDEX 1 is NA and the loop cannot make NA as a parameter
  combi[i]<-as.numeric(grepl(i,combi$OutcomeSubtype))  
}
combi$OutcomeSubtype<-NULL

# Create indicator vars for breeds and mix
combi_breeds <- as.character(combi$Breed)
all_breeds <- unique(combi_breeds)
breed_words1 <- unique(unlist(strsplit(all_breeds, "/")))
breed_words <- gsub('Mix','',breed_words1)

#we have the names of unique breeds of animals,now we shall trace i in this list and make new variables
#in combi by the value of i at that name,nd the grep giving T or F value gives numeric 1&0 value. 
for (i in breed_words){                    
  combi$i <- as.numeric(grepl(i,combi_breeds))
}

#representation type combi$cross_breed or combi["cross_breed"] same
combi$cross_breed <- str_count(combi$Breed, pattern="/| Mix")
combi$Breed<-NULL
###############################################################################

# Create indicator vars for color
combi_colors <- as.character(combi$Color)
all_colors <- unique(combi_colors)
color_words <- unique(unlist(strsplit(all_colors, c("/")))) 
for (color in color_words){
  combi[color] <- as.numeric(grepl(color, combi_colors))
}
combi$diff_col_count <- str_count(combi$Color, pattern="/")+1
combi$Color <- NULL
combi$SimpleColor<-NULL

#good practice will be to convert column to factor
combi$Sex<-as.factor(combi$Sex)
combi$Intact<-as.factor(combi$Intact)
###############################################################################
#VISUALISATIONS
combi2<-combi[1:26729,] # to seperate the training modified data from combi dataframe for plotting

#PLOT1 outcome by AnimalType->> Dog or Cat
outcomes <- combi2 %>% group_by(AnimalType,OutcomeType) %>% summarize(N=n())
qplot(OutcomeType,N,data=outcomes,geom=c("point","line"),group=1,facets = AnimalType~.)

#PLOT2
ggplot(outcomes,aes(x=AnimalType,N,fill=OutcomeType)) +
  geom_bar(stat="identity",position="fill",col="black") +
  labs(x="Animal",y="Proportion",title="Outcomes") + theme_bw()


#PLOT3 outcome by hour
qplot(Hour,data=combi2,fill=OutcomeType,geom="density") #including the training set

#PLOT4
#age in days affect on outcome
plot(combi2$OutcomeType,combi2$AgeinDays)

#PLOT5
#how name length of animal affects outcome
qplot(name_len,data=combi2,fill=OutcomeType)

#PLOT6
#finding outcome based on animal type having name or not
qplot(HasName,data=combi2,fill=OutcomeType,facets=AnimalType~.)

#PLOT7
#finding outcome based on animal type being baby or not
qplot(Lifestage,data=combi2,fill=OutcomeType,facets=AnimalType~.)

#PLOT8
#how neutered or not helps in detecting outcome 
qplot(OutcomeType,data=combi2,fill=Intact,facets=AnimalType~.)  
#in field Intact=>  0- Neutered, 1- intact
#we see that neutered animals are most likely to be adopted
#############################################################################

#CONVERTING THE VARIABLES INTO FACTOR FOR LOADING INSIDE XGBOOST
factorVars <- c('AnimalID','OutcomeType','AnimalType','newbreed','SimpleColor','HasName','Mix_breed','Intact','Sex','TimeofDay','Lifestage')
combi[factorVars] <- lapply(combi[factorVars], function(x) as.factor(x))

# Split up train and test data
train <- combi[1:26729, ]
test  <- combi[26730:nrow(combi), ]
outcome <- train$OutcomeType
train$AnimalID <- NULL
train$OutcomeType <- NULL
test$AnimalID<-NULL
test$OutcomeType<-NULL

# Submission code
set.seed(121)
train<-na.omit(train)
train_matrix <- matrix(as.numeric(data.matrix(train)),ncol=length(names(train)))
test_matrix <- matrix(as.numeric(data.matrix(test)),ncol=length(names(test)))
full_targets_train <- as.numeric(outcome)-1

# Run xgb on full train set
xgb_model_test = xgboost(data=train_matrix, 
                         label=full_targets_train, 
                         nrounds=125, 
                         verbose=1, 
                         eta=0.2, 
                         max_depth=6, 
                         subsample=0.75, 
                         colsample_bytree=0.85,
                         objective="multi:softprob", 
                         eval_metric="mlogloss",
                         num_class=5)


test_preds <- predict(xgb_model_test, test_matrix)
test_preds_frame <- data.frame(matrix(test_preds, ncol = 5, byrow=TRUE))
colnames(test_preds_frame) <- levels(outcome)

#to get the data into submission format
submission <- cbind(data.frame(ID=test_ID), test_preds_frame)

write.csv(submission , "shelter_animals.csv", row.names=FALSE)


###############################################################################################
#the above xgbmodel gave an accuracy of 0.72 on the kaggle competition and helped me manage get in
#top 13% rank. One can incorporate more relevant fields and tune in the parameters better, to increase the accuracy
#further. The mode of evaluation is multi-class logarithmic loss metric.
###############################################################################################
