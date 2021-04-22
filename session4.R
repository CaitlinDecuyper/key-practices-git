### BOSS:  Dutch Names for 1400 Photographs ###
### part of full analysis script for KPLS course
### script by Caitlin Decuyper
### last checked 22-04-2021
### R version 3.5.2



### LOAD PACKAGES ###
library(dplyr)
library(tidyr)
library(ggpubr)
library(car)
library(languageR)
library(reshape)
library(reshape2)
library(data.table)
library(stringr)
library(gridExtra)
library(grid)



### SET YOUR PATH TO FOLDERS AND LOAD DATA ###

dataFolder = 'K:/BOSS_pictureNorming_Dutch/DATA/'
#resultsFolder = 'K:/BOSS_pictureNorming_Dutch/Analysis/Results/'

#responses were coded manually in this file; should be 23 columns x 71145 lines
data <- read.table(paste(dataFolder, "BOSS_aggregated_names.txt", sep=''), header=TRUE, check.names=FALSE, quote="",sep="\t") 

#word prevalence: Keuleers et al. 2015
prevalence <- read.table(paste(dataFolder, "prevalence_B&NL.txt", sep=''), header=TRUE, check.names=FALSE, quote="",sep="\t")
prevalence <- prevalence %>% select(1,4)
prevalence$prevalence <- round(prevalence$prevalence, 2)
colnames(prevalence)[colnames(prevalence) =="prevalence"] <- "WP"

#word frequency SUBTLEX (per million words, log per million words, Zipf): Keuleers et al. 2010; Van Heuven et al. 2014
frequency <- read.table(paste(dataFolder, "SUBTLEX_with_Zipf.txt", sep=''), header=TRUE, check.names=FALSE, quote="",sep="\t")
frequency <- frequency %>% select(1,7,8,9)
frequency$Lg10WF <- round(frequency$Lg10WF, 2)
frequency$SUBTLEXWF <- round(frequency$SUBTLEXWF, 2)

#age of acquisition Brysbaert et al. 2014
AoA <- read.table(paste(dataFolder, "averageAoA.txt", sep=''), header=TRUE, check.names=FALSE, quote="",sep="\t") 

#list of all (EN) picture names (should include 1397 items)
allPictures <- data.frame(unique(data$English_Name)) 





### PREPROCESSING ###

#put "NORESPONSE" in empty cells
data$Input <- sub("^$", "NORESPONSE", data$Input)
data$Input <- sub(" ", "NORESPONSE", data$Input)
data$Input_spellingChecked <- sub("^$", "NORESPONSE", data$Input_spellingChecked)
data$Input_spellingChecked <- sub(" ", "NORESPONSE", data$Input_spellingChecked)
data$Aggregated <- sub("^$", "NORESPONSE", data$Aggregated)
data$Aggregated <- sub(" ", "NORESPONSE", data$Aggregated)

#add column with 1's for counting
data$Count <- 1

#mark valid responses (ExactMatch, NonStandard, Synonym, Plural/Singular, Diminutive, MoreSpecific, MoreGeneral)
data$Valid <- 0
data$Valid[data$ExactMatch == 1 | data$NonStandardDutch == 1  | data$Synonym == 1  | data$PluralSingular == 1  | data$Diminutive == 1  | data$MoreSpecific == 1 |  data$MoreGeneral == 1] = 1






#################################################################################################
### TABLE A1 ### raw data: unproccessed, including typo's... (Input)

#order data by English_Name (= picture name), then assigned Dutch name; select and rename relevant columns
data <- data[order(data$English_Name, data$Input),]

TableA1 <- data[,c(1,2,4,5,6,8)] 
colnames(TableA1)[colnames(TableA1) =="PP_Count"] <- "Participant"
colnames(TableA1)[colnames(TableA1) =="Input"] <- "Dutch_input"
colnames(TableA1)[colnames(TableA1) =="Score"] <- "OA_Score"

#write.csv2(TableA1, paste(resultsFolder, "TableA1_rawData_unprocessed.csv")) #uploaded as Excel sheet at https://osf.io/kwu87/



#################################################################################################
### TABLE A2 ### raw data: aggregated and checked for spelling (Aggregated)

TableA2 <- data.frame(matrix(ncol=5,nrow=0)) # create empty dataframe to start

for (i in 1:nrow(allPictures)) {
  #select all responses for 1 item/picture
  data_item <- data[which(data$English_Name == allPictures[i,1]), ]
  
  #pivot + transpose into table with unique names for this item +  number of times each name was used
  table_item = dcast(data_item, English_Name ~ English_Name + Aggregated, value.var="Count", fun.aggregate=sum)
  table_item <-t(table_item[c(2:ncol(table_item))])
  table_item <- data.frame(cbind(names = rownames(table_item), table_item))
  table_item <- table_item %>% separate(names, c("English_Name", "Name"), "_")
  colnames(table_item)[colnames(table_item) =="V2"] <- "Used"
  rownames(table_item) <- NULL
  
  #sort by most popular response and calculate agreement (n used/n participants)
  table_item$Used <- as.numeric(as.character(table_item$Used))
  table_item <- table_item[order(table_item$Used,decreasing = T),]
  table_item$nPart = nrow(data_item)                                  # count includes subjects that didn't respond
  table_item$NA_all <- round(table_item$Used/table_item$nPart*100, 2) # so NA_all is NA including DKO/DKN responses
  
  TableA2 <- rbind(TableA2, table_item)
  rm(table_item)
  
  i=i+1  
}

#write.csv2(TableA2, paste(resultsFolder, "TableA2_rawData_processed.csv")) #uploaded as Excel sheet at https://osf.io/kwu87/


