# New Tests ideas

# Calling packages----

library(tidyverse) 
library(httr)
library(readxl)
library(googledrive)
library(googlesheets4)
library(readr)

## load files
sieve = read_sheet("https://docs.google.com/spreadsheets/d/1OZctzgF-PNKXvkPmHyvbgMbN20jdieuJlSPt_9QI8v8/edit?gid=1568227482#gid=1568227482")
wf    = read_sheet("https://docs.google.com/spreadsheets/d/1sxE-pn_d9mBh4My8fEpMqESQw_2J3dnU075G0zSE36I/edit?gid=1483737642#gid=1483737642") # the user can adjust these weighting factors manually to indicate your preferences for technical criteria 

# Create the new database
Possible_database = read_delim("Possible_database.csv", 
                                delim = ";", escape_double = FALSE, trim_ws = TRUE)
sheet_write(Possible_database,
            ss = "https://docs.google.com/spreadsheets/d/1sSCeIfPim5i8X93UdUMqgICKzxpJ7gJtia1GIaiqK-k/edit?gid=0#gid=0",
            sheet = "database")

##########
### STEP 1: Select user-preferences

######
## A) Which functions are you interested in?

functions <- c("CR", "NC", "DS", "WR") 
# options are "CR" (carbon and climate regulation), "DS" (disease and pest management), "NC" (nutrient cycling) & "WR" (water regulation and purification)
# user can select one or multiple functions

######
## B) Which ecosystems are you interested in?

ecosystem <- c("arable") 
# options are arable, forest, grassland
# user can only choose one ecosystem. the selection tool and dataset is currently focused on arable ecosystems.

######
## C) Indicate the importance of technical criteria to your assessment program on a scale from 1 to 5?

# you can indicate the importance of technical criteria (weighting factors) in the "Weighting_factors.csv" file. 
# in the standard file, all weighting factors are set to 1. But the idea is that the user scales these from 1 to 5 to indicate which technical criteria 
# are most important to the objective of the user's assessment. 

weights <- wf$Standard
names(weights) <- wf$Criterium

## script adjust weights so that they add up to 50
sum_weights <- sum(weights)
weights <- (weights/sum_weights)*50

######
## D) Would you like to apply an exclusion filter and if so which one?
sieve$Filter <- 1 ## default is that filter scores 1. whether same methods score 0 and are "sieved out" is then determined by the selected user filters.

user_filter <- c("specialised_infrastructure") 
# options are: "time", "sample_storage", "sample_size", "low_costs", "reference", "lab_skills", "reproducibility", "specialised_infrastructure"
# it is possible to select multiple user filters
# NOTE: these filter options may still change a bit in future depending on user demand
# for the meaning of the filters, see text next to user filters below

if ("time" %in% user_filter){
  sieve$Filter[sieve$Collection_time == 0] <- 0 ## cannot visit field multiple times
} 

if ("sample_storage" %in% user_filter){
  sieve$Filter[sieve$Storage <= 2] <- 0 ## Only select methods were samples can be stored for more than a month
} 

if ("sample_size" %in% user_filter){
  sieve$Filter[sieve$Sample_amount <= 2] <- 0 ## Cannot sample more than 100 g 
} 

if ("low_costs" %in% user_filter){
  sieve$Filter[sieve$Cost_per_sample <= 2] <- 0 ## sample analysis should have low costs
} 

if ("reference" %in% user_filter){
  sieve$Filter[sieve$Reference <= 1] <- 0 ## sample analysis should include standardised international or internal reference
} 

if ("lab_skills" %in% user_filter){
  sieve$Filter[sieve$Lab_use <= 2] <- 0 ## samples should be applicable for routine analysis
} 

if ("reproducibility" %in% user_filter){
  sieve$Filter[sieve$Reproducibility <= 2] <- 0 ## high reproducibility of method is required
} 

if ("specialised_infrastructure" %in% user_filter){
  sieve$Filter[sieve$Infrastructure == 1] <- 0 ## user does not have access to specialised labs and infrastructures
}



##########
### STEP 3: START Sieve

# from step 3 onwards, no further user input is required. 

### 3A) TIER 1: Pertinence
# this tier calculates the pertinence based on the functional information, frequenecy and relevance score of the method for the functions that the user has selected
# if one of the pertinence criteria scores a 0, the total pertinence score will be 0 and the method is sieved out. 
# the script will let you know which functions are being assessed and which ones not by saying the "function X sieve is active".
# this is a check to see if the script is indeed assessing the functions that the user is interested in. 

## CR function criteria
if ("CR" %in% functions){
  print("CR pertinence sieve is active")
  sieve$CR_pertinence <- sieve$CR_functional * sieve$CR_freq * sieve$CR_rel / 3
} else {
  print ("CR pertinence sieve is not active")
}

## DS function criteria
if ("DS" %in% functions){
  print("DS pertinence sieve is active")
  sieve$DS_pertinence <- sieve$DS_functional * sieve$DS_freq * sieve$DS_rel / 3
} else {
  print ("DS pertinence sieve is not active")
}

## NC function criteria
if ("NC" %in% functions){
  print("NC pertinence sieve is active")
  sieve$NC_pertinence <- sieve$NC_functional * sieve$NC_freq * sieve$NC_rel / 3
} else {
  print ("NC pertinence sieve is not active")
}

## WR function criteria
if ("WR" %in% functions){
  print("WR pertinence sieve is active")
  sieve$WR_pertinence <- sieve$WR_functional * sieve$WR_freq * sieve$WR_rel / 3
} else {
  print ("WR pertinence sieve is not active")
}


## 3B) TIER 2: APPLICABILITY
# this tier assesses the applicability of the method to the ecosystem of interest
# if applicability of a method scored 0, the method score remains zero and is sieved out. 

if(ecosystem == "arable"){
  sieve$App <- sieve$Applicability_agri
} else if (ecosystem == "forest"){
  sieve$App <- sieve$Applicability_forest
} else if (ecosystem == "grassland"){
  sieve$App <- sieve$Applicability_grass
}



### 3C) TIER 3: TECHNICAL
# This tier calculates the technical scores. 
# First, technical scores are normalized to 0-1. For example, as throughput is scored from 1-4, it is rescaled to 0.25 to 1. 
# Then weighting factors are applied based on user-preferences indicated in the "Weighting_factors.csv" file. 

sieve$Technical <-  sieve$Throughput/4 * weights["Throughput"] + 
  sieve$Storage/3 * weights["Storage"] + 
  sieve$Collection_time * weights["Collection_time"] +
  sieve$Collection_space * weights["Collection_space"] + 
  sieve$Archivability/2 * weights["Archivability"] +
  sieve$Sample_amount/3 * weights["Sample_amount"] + 
  sieve$Cost_per_sample/3 * weights["Cost_per_sample"] + 
  sieve$Lab_use/3 * weights["Lab_use"] +
  sieve$Data_process/3 * weights["Data_process"] + 
  sieve$Reference/3 * weights["Reference"] +
  sieve$Reproducibility/3 * weights["Reproducibility"] + 
  sieve$Deployment/2 * weights["Deployment"] + 
  sieve$Infrastructure/3 * weights["Infrastructure"]


###3D) Final filter for exclusion criteria
# In this final step technical scores are multiplied by filter. This sieves out methods that scored 0 on one of the filters applied. 
sieve$Technical <- sieve$Technical * sieve$Filter

## 3E) Aggregate scores by functions
# here the script aggregated the scores of the pertinence, applicability and technical tier for each function
# in this final step, individual reports for each function are created. the sieve indicates the final score and rank of each method per function. 
# the script will let you know which functions are being assessed and which ones not by saying the "function X aggregation sieve is active".
# this is a check to see if the script is indeed assessing the functions that the user is interested in. 

## CR function
if ("CR" %in% functions){
  print("CR aggregation sieve is active")
  sieve$CR_aggregated <- sieve$CR_pertinence * sieve$App * sieve$Technical
  report_cr <- select(sieve, Method_name, Method_type, Name, Related_process, CR_aggregated)
  
  report_cr <- arrange(report_cr, (desc(CR_aggregated)))
  report_cr$CR_rank <- factor(report_cr$CR_aggregated) # assign same rank to methods with same aggregated score 
  levels(report_cr$CR_rank) <- as.character(seq(from = length(levels(report_cr$CR_rank)), to = 1, by = -1)) 
  report_cr$CR_rank <- as.character(report_cr$CR_rank)
  
  report_cr$CR_rank[report_cr$CR_aggregated == 0] <- "eliminated"
  
} else {
  print ("CR aggregation sieve is not active")
}

## DS function criteria
if ("DS" %in% functions){
  print("DS aggregation sieve is active")
  sieve$DS_aggregated <- sieve$DS_pertinence * sieve$App * sieve$Technical
  report_ds <- select(sieve, Method_name, Method_type, Name, Related_process, DS_aggregated)
  report_ds <- arrange(report_ds, (desc(DS_aggregated)))
  
  report_ds <- arrange(report_ds, (desc(DS_aggregated)))
  report_ds$DS_rank <- factor(report_ds$DS_aggregated) # assign same rank to methods with same aggregated score 
  levels(report_ds$DS_rank) <- as.character(seq(from = length(levels(report_ds$DS_rank)), to = 1, by = -1)) 
  report_ds$DS_rank <- as.character(report_ds$DS_rank)
  
  report_ds$DS_rank[report_ds$DS_aggregated == 0] <- "eliminated"
} else {
  print ("DS aggregation sieve is not active")
}

## NC function criteria
if ("NC" %in% functions){
  print("NC aggregation sieve is active")
  sieve$NC_aggregated <- sieve$NC_pertinence * sieve$App * sieve$Technical
  report_nc <- select(sieve, Method_name, Method_type, Name, Related_process, NC_aggregated)
  report_nc <- arrange(report_nc, (desc(NC_aggregated)))
  
  report_nc <- arrange(report_nc, (desc(NC_aggregated)))
  report_nc$NC_rank <- factor(report_nc$NC_aggregated) # assign same rank to methods with same aggregated score 
  levels(report_nc$NC_rank) <- as.character(seq(from = length(levels(report_nc$NC_rank)), to = 1, by = -1)) 
  report_nc$NC_rank <- as.character(report_nc$NC_rank)
  
  report_nc$NC_rank[report_nc$NC_aggregated == 0] <- "eliminated"
} else {
  print ("NC aggregation sieve is not active")
}

## WR function criteria
if ("WR" %in% functions){
  print("WR aggregation sieve is active")
  sieve$WR_aggregated <- sieve$WR_pertinence * sieve$App * sieve$Technical
  report_wr <- select(sieve, Method_name, Method_type, Name, Related_process, WR_aggregated)
  report_wr <- arrange(report_wr, (desc(WR_aggregated)))
  
  report_wr <- arrange(report_wr, (desc(WR_aggregated)))
  report_wr$WR_rank <- factor(report_wr$WR_aggregated) # assign same rank to methods with same aggregated score 
  levels(report_wr$WR_rank) <- as.character(seq(from = length(levels(report_wr$WR_rank)), to = 1, by = -1)) 
  report_wr$WR_rank <- as.character(report_wr$WR_rank)
  
  report_wr$WR_rank[report_wr$WR_aggregated == 0] <- "eliminated"
} else {
  print ("WR aggregation sieve is not active")
}


