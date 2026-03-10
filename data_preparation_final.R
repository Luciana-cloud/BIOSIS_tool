# Calling packages ----

library(readr)
library(tidyverse)
#library(plyr)
library(googledrive)
library(googlesheets4)
library(readxl)
library(purrr)
library(rPref)
library(GGally)
library(fmsb)
library(stringr)

# RELEVANCE ----

# Calling data 

process_scores      = read_excel("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/process_scores.xlsx")
parameter_scores    = read_excel("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/parameter_scores.xlsx")
parameter_scores_CZ = read_excel("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/parameter_scores.xlsx", 
                                 sheet = "CZ")
parameter_scores_LU = read_excel("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/parameter_scores.xlsx", 
                                 sheet = "LUT")
parameter_scores_CZ_LU = read_excel("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/parameter_scores.xlsx", 
                                    sheet = "LUTxCZ")

# Change names to harmonize datasets 
parameter_scores = parameter_scores %>% mutate(folder_code = case_when(folder_code == "CR" ~ "Climate regulation",
                                                                       folder_code == "HP" ~ "Habitat provision",
                                                                       folder_code == "NC" ~ "Nutrient cycling",
                                                                       folder_code == "WR" ~ "Water regulation and filtration"))

parameter_scores_CZ = parameter_scores_CZ %>% mutate(folder_code = case_when(folder_code == "CR" ~ "Climate regulation",
                                                                             folder_code == "HP" ~ "Habitat provision",
                                                                             folder_code == "NC" ~ "Nutrient cycling",
                                                                             folder_code == "WR" ~ "Water regulation and filtration"))

parameter_scores_LU = parameter_scores_LU %>% mutate(folder_code = case_when(folder_code == "CR" ~ "Climate regulation",
                                                                             folder_code == "HP" ~ "Habitat provision",
                                                                             folder_code == "NC" ~ "Nutrient cycling",
                                                                             folder_code == "WR" ~ "Water regulation and filtration"))

parameter_scores_CZ_LU = parameter_scores_CZ_LU %>% mutate(folder_code = case_when(folder_code == "CR" ~ "Climate regulation",
                                                                                   folder_code == "HP" ~ "Habitat provision",
                                                                                   folder_code == "NC" ~ "Nutrient cycling",
                                                                                   folder_code == "WR" ~ "Water regulation and filtration"))

# Create consolidating variable for the relevance of each parameter based on the questions 

# Total relevance 
parameter_scores = parameter_scores %>% mutate(parameter_relevace = (parameter_scores$Q1_median/max(parameter_scores$Q1_median)+
                                                                       parameter_scores$Q2_median/max(parameter_scores$Q2_median)+
                                                                       parameter_scores$Q3_median/max(parameter_scores$Q3_median))/3)

# Land use relevance 

# Agriculture relevance
parameter_scores_agr = parameter_scores_LU %>% filter(`Land use` == "Agricultural")

parameter_scores = parameter_scores %>% mutate(parameter_relevace_agr = (parameter_scores_agr$Q1_median/max(parameter_scores_agr$Q1_median, na.rm=TRUE)+
                                                                           parameter_scores_agr$Q2_median/max(parameter_scores_agr$Q2_median, na.rm=TRUE)+
                                                                           parameter_scores_agr$Q3_median/max(parameter_scores_agr$Q3_median, na.rm=TRUE))/3)

# Forest relevance
parameter_scores_for = parameter_scores_LU %>% filter(`Land use` == "Forestry")

# Replace na values with 0 
# parameter_scores_for[is.na(parameter_scores_for)] = 0

parameter_scores = parameter_scores %>% mutate(parameter_relevace_for = (parameter_scores_for$Q1_median/max(parameter_scores_for$Q1_median, na.rm=TRUE)+
                                                                           parameter_scores_for$Q2_median/max(parameter_scores_for$Q2_median, na.rm=TRUE)+
                                                                           parameter_scores_for$Q3_median/max(parameter_scores_for$Q3_median, na.rm=TRUE))/3)

# Urban relevance
parameter_scores_urb = parameter_scores_LU %>% filter(`Land use` == "Urban")

# Replace na values with 0 
# parameter_scores_urb[is.na(parameter_scores_urb)] = 0

parameter_scores = parameter_scores %>% mutate(parameter_relevace_urb = (parameter_scores_urb$Q1_median/max(parameter_scores_urb$Q1_median, na.rm=TRUE)+
                                                                           parameter_scores_urb$Q2_median/max(parameter_scores_urb$Q2_median, na.rm=TRUE)+
                                                                           parameter_scores_urb$Q3_median/max(parameter_scores_urb$Q3_median, na.rm=TRUE))/3)

# Climatic zone relevance 

# Atlantic Central 
parameter_scores_AC = parameter_scores_CZ %>% filter(`Climatic Zone` == "Atlantic Central")

# Replace na values with 0 
# parameter_scores_AC[is.na(parameter_scores_AC)] = 0

parameter_scores = parameter_scores %>% mutate(parameter_relevace_AC = (parameter_scores_AC$Q1_median/max(parameter_scores_AC$Q1_median, na.rm=TRUE)+
                                                                          parameter_scores_AC$Q2_median/max(parameter_scores_AC$Q2_median, na.rm=TRUE)+
                                                                          parameter_scores_AC$Q3_median/max(parameter_scores_AC$Q3_median, na.rm=TRUE))/3)

# Atlantic North 
parameter_scores_AN = parameter_scores_CZ %>% filter(`Climatic Zone` == "Atlantic North")

# Replace na values with 0 
# parameter_scores_AN[is.na(parameter_scores_AN)] = 0

parameter_scores = parameter_scores %>% mutate(parameter_relevace_AN = (parameter_scores_AN$Q1_median/max(parameter_scores_AN$Q1_median, na.rm=TRUE)+
                                                                          parameter_scores_AN$Q2_median/max(parameter_scores_AN$Q2_median, na.rm=TRUE)+
                                                                          parameter_scores_AN$Q3_median/max(parameter_scores_AN$Q3_median, na.rm=TRUE))/3)

# Boreal 
parameter_scores_Boreal = parameter_scores_CZ %>% filter(`Climatic Zone` == "Boreal")

# Replace na values with 0 
# parameter_scores_Boreal[is.na(parameter_scores_Boreal)] = 0

parameter_scores = parameter_scores %>% mutate(parameter_relevace_Boreal = (parameter_scores_Boreal$Q1_median/max(parameter_scores_Boreal$Q1_median, na.rm=TRUE)+
                                                                              parameter_scores_Boreal$Q2_median/max(parameter_scores_Boreal$Q2_median, na.rm=TRUE)+
                                                                              parameter_scores_Boreal$Q3_median/max(parameter_scores_Boreal$Q3_median, na.rm=TRUE))/3)


# Continental
parameter_scores_Continental = parameter_scores_CZ %>% filter(`Climatic Zone` == "Continental")

# Replace na values with 0 
# parameter_scores_Continental[is.na(parameter_scores_Continental)] = 0

parameter_scores = parameter_scores %>% mutate(parameter_relevace_Continental = (parameter_scores_Continental$Q1_median/max(parameter_scores_Continental$Q1_median, na.rm=TRUE)+
                                                                                   parameter_scores_Continental$Q2_median/max(parameter_scores_Continental$Q2_median, na.rm=TRUE)+
                                                                                   parameter_scores_Continental$Q3_median/max(parameter_scores_Continental$Q3_median, na.rm=TRUE))/3)

# Lusitanian
parameter_scores_Lusitanian = parameter_scores_CZ %>% filter(`Climatic Zone` == "Lusitanian")

# Replace na values with 0 
# parameter_scores_Lusitanian[is.na(parameter_scores_Lusitanian)] = 0

parameter_scores = parameter_scores %>% mutate(parameter_relevace_Lusitanian = (parameter_scores_Lusitanian$Q1_median/max(parameter_scores_Lusitanian$Q1_median, na.rm=TRUE)+
                                                                                  parameter_scores_Lusitanian$Q2_median/max(parameter_scores_Lusitanian$Q2_median, na.rm=TRUE)+
                                                                                  parameter_scores_Lusitanian$Q3_median/max(parameter_scores_Lusitanian$Q3_median, na.rm=TRUE))/3)

# Mediterranean Mountains
parameter_scores_MM = parameter_scores_CZ %>% filter(`Climatic Zone` == "Mediterranean Mountains")

# Replace na values with 0 
# parameter_scores_MM[is.na(parameter_scores_MM)] = 0

parameter_scores = parameter_scores %>% mutate(parameter_relevace_MM = (parameter_scores_MM$Q1_median/max(parameter_scores_MM$Q1_median, na.rm=TRUE)+
                                                                          parameter_scores_MM$Q2_median/max(parameter_scores_MM$Q2_median, na.rm=TRUE)+
                                                                          parameter_scores_MM$Q3_median/max(parameter_scores_MM$Q3_median, na.rm=TRUE))/3)

# Mediterranean North
parameter_scores_MN = parameter_scores_CZ %>% filter(`Climatic Zone` == "Mediterranean North")

# Replace na values with 0 
# parameter_scores_MN[is.na(parameter_scores_MN)] = 0

parameter_scores = parameter_scores %>% mutate(parameter_relevace_MN = (parameter_scores_MN$Q1_median/max(parameter_scores_MN$Q1_median, na.rm=TRUE)+
                                                                          parameter_scores_MN$Q2_median/max(parameter_scores_MN$Q2_median, na.rm=TRUE)+
                                                                          parameter_scores_MN$Q3_median/max(parameter_scores_MN$Q3_median, na.rm=TRUE))/3)


# Mediterranean South
parameter_scores_MS = parameter_scores_CZ %>% filter(`Climatic Zone` == "Mediterranean South")

# Replace na values with 0 
# parameter_scores_MS[is.na(parameter_scores_MS)] = 0

parameter_scores = parameter_scores %>% mutate(parameter_relevace_MS = (parameter_scores_MS$Q1_median/max(parameter_scores_MS$Q1_median, na.rm=TRUE)+
                                                                          parameter_scores_MS$Q2_median/max(parameter_scores_MS$Q2_median, na.rm=TRUE)+
                                                                          parameter_scores_MS$Q3_median/max(parameter_scores_MS$Q3_median, na.rm=TRUE))/3)

# Nemoral
parameter_scores_Nemoral = parameter_scores_CZ %>% filter(`Climatic Zone` == "Nemoral")

# Replace na values with 0 
# parameter_scores_Nemoral[is.na(parameter_scores_Nemoral)] = 0

parameter_scores = parameter_scores %>% mutate(parameter_relevace_Nemoral = (parameter_scores_Nemoral$Q1_median/max(parameter_scores_Nemoral$Q1_median, na.rm=TRUE)+
                                                                               parameter_scores_Nemoral$Q2_median/max(parameter_scores_Nemoral$Q2_median, na.rm=TRUE)+
                                                                               parameter_scores_Nemoral$Q3_median/max(parameter_scores_Nemoral$Q3_median, na.rm=TRUE))/3)

# Pannonian
parameter_scores_Pannonian = parameter_scores_CZ %>% filter(`Climatic Zone` == "Pannonian")

# Replace na values with 0 
# parameter_scores_Pannonian[is.na(parameter_scores_Pannonian)] = 0

keyA <- paste(parameter_scores_Pannonian$folder_code, parameter_scores_Pannonian$Process,
              parameter_scores_Pannonian$Parameter)
keyB <- paste(parameter_scores_Nemoral$folder_code, parameter_scores_Nemoral$Process, 
              parameter_scores_Nemoral$Parameter)

idx <- match(keyB, keyA)

parameter_scores_Pannonian.1 <- parameter_scores_Pannonian[idx, ]

parameter_scores_Pannonian.1$folder_code  <- parameter_scores_Nemoral$folder_code
parameter_scores_Pannonian.1$Process   <- parameter_scores_Nemoral$Process
parameter_scores_Pannonian.1$Parameter <- parameter_scores_Nemoral$Parameter

# Replace na values with 0 
#parameter_scores_Pannonian.1 <- parameter_scores_Pannonian.1 %>%
#  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))

parameter_scores = parameter_scores %>% mutate(parameter_relevace_Pannonian = (parameter_scores_Pannonian.1$Q1_median/max(parameter_scores_Pannonian.1$Q1_median, na.rm=TRUE)+
                                                                                 parameter_scores_Pannonian.1$Q2_median/max(parameter_scores_Pannonian.1$Q2_median, na.rm=TRUE)+
                                                                                 parameter_scores_Pannonian.1$Q3_median/max(parameter_scores_Pannonian.1$Q3_median, na.rm=TRUE))/3)

# Create general table 

test_table = parameter_scores %>% select("folder_code","Process","Parameter","parameter_relevace","parameter_relevace_agr",
                                         "parameter_relevace_for","parameter_relevace_urb","parameter_relevace_AC",
                                         "parameter_relevace_AN","parameter_relevace_Boreal","parameter_relevace_Continental",
                                         "parameter_relevace_Lusitanian","parameter_relevace_MM","parameter_relevace_MN",
                                         "parameter_relevace_MS","parameter_relevace_Nemoral","parameter_relevace_Pannonian")

test_table1 = test_table

# Match content 

key_test  = paste(test_table1$Process,  test_table1$folder_code)
key_score = paste(process_scores$name, process_scores$Function)

idx = match(key_test, key_score)

# Match the scores of the processes 
test_table1$Subfunction   = process_scores$Subfunction[idx]
test_table1$Score_Process = process_scores$Full_medians[idx] # Total Relevance
test_table1$Score_Process_agri = process_scores$Agr_medians[idx] # Agricultural Relevance
test_table1$Score_Process_for  = process_scores$For_medians[idx] # Forest Relevance
test_table1$Score_Process_urb  = process_scores$Urb_medians[idx] # Urban Relevance

test_table1$Score_Process_AT  = process_scores$ATC_medians[idx] # Atlantic Central Relevance
test_table1$Score_Process_AN  = (process_scores$ATC_medians[idx] + 
                                   process_scores$BOR_medians[idx] + 
                                   process_scores$CON_medians[idx] + 
                                   process_scores$MDN_medians[idx] + 
                                   process_scores$MDS_medians[idx] + 
                                   process_scores$PAN_medians[idx])/6 # Atlantic North Relevance
test_table1$Score_Process_BOR  = process_scores$BOR_medians[idx] # Boreal Relevance
test_table1$Score_Process_CON  = process_scores$CON_medians[idx] # Continental Relevance
test_table1$Score_Process_LUS  = (process_scores$ATC_medians[idx] + 
                                    process_scores$BOR_medians[idx] + 
                                    process_scores$CON_medians[idx] + 
                                    process_scores$MDN_medians[idx] + 
                                    process_scores$MDS_medians[idx] + 
                                    process_scores$PAN_medians[idx])/6 # Lusitanian Relevance
test_table1$Score_Process_MDM  = (process_scores$ATC_medians[idx] + 
                                    process_scores$BOR_medians[idx] + 
                                    process_scores$CON_medians[idx] + 
                                    process_scores$MDN_medians[idx] + 
                                    process_scores$MDS_medians[idx] + 
                                    process_scores$PAN_medians[idx])/6 # Mediterranean Mountains Relevance
test_table1$Score_Process_MDN  = process_scores$MDN_medians[idx] # Mediterranean North Relevance
test_table1$Score_Process_MDS  = process_scores$MDS_medians[idx] # Mediterranean South Relevance
test_table1$Score_Process_NEM  = (process_scores$ATC_medians[idx] + 
                                    process_scores$BOR_medians[idx] + 
                                    process_scores$CON_medians[idx] + 
                                    process_scores$MDN_medians[idx] + 
                                    process_scores$MDS_medians[idx] + 
                                    process_scores$PAN_medians[idx])/6 # Nemoral Relevance
test_table1$Score_Process_PAN  = process_scores$PAN_medians[idx] # Pannonian Relevance

# Match the scores of the subfunctions

idx = match(test_table1$Subfunction, process_scores$name)
test_table1$Score_Subfunction = process_scores$Full_medians[idx]  # Total Relevance
test_table1$Score_Subfunction_agr = process_scores$Agr_medians[idx]  # Agricultural Relevance
test_table1$Score_Subfunction_for = process_scores$For_medians[idx]  # Forest Relevance
test_table1$Score_Subfunction_urb = process_scores$Urb_medians[idx]  # Urban Relevance

test_table1$Score_Subfunction_AT  = process_scores$ATC_medians[idx] # Atlantic Central Relevance
test_table1$Score_Subfunction_AN  = (process_scores$ATC_medians[idx] + 
                                          process_scores$BOR_medians[idx] + 
                                          process_scores$CON_medians[idx] + 
                                          process_scores$MDN_medians[idx] + 
                                          process_scores$MDS_medians[idx] + 
                                          process_scores$PAN_medians[idx])/6 # Atlantic North Relevance
test_table1$Score_Subfunction_BOR  = process_scores$BOR_medians[idx] # Boreal Relevance
test_table1$Score_Subfunction_CON  = process_scores$CON_medians[idx] # Continental Relevance
test_table1$Score_Subfunction_LUS  = (process_scores$ATC_medians[idx] + 
                                        process_scores$BOR_medians[idx] + 
                                        process_scores$CON_medians[idx] + 
                                        process_scores$MDN_medians[idx] + 
                                        process_scores$MDS_medians[idx] + 
                                        process_scores$PAN_medians[idx])/6 # Lusitanian Relevance
test_table1$Score_Subfunction_MDM  = (process_scores$ATC_medians[idx] + 
                                        process_scores$BOR_medians[idx] + 
                                        process_scores$CON_medians[idx] + 
                                        process_scores$MDN_medians[idx] + 
                                        process_scores$MDS_medians[idx] + 
                                        process_scores$PAN_medians[idx])/6 # Mediterranean Mountains Relevance
test_table1$Score_Subfunction_MDN  = process_scores$MDN_medians[idx] # Mediterranean North Relevance
test_table1$Score_Subfunction_MDS  = process_scores$MDS_medians[idx] # Mediterranean South Relevance
test_table1$Score_Subfunction_NEM  = (process_scores$ATC_medians[idx] + 
                                        process_scores$BOR_medians[idx] + 
                                        process_scores$CON_medians[idx] + 
                                        process_scores$MDN_medians[idx] + 
                                        process_scores$MDS_medians[idx] + 
                                        process_scores$PAN_medians[idx])/6 # Nemoral Relevance
test_table1$Score_Subfunction_PAN  = process_scores$PAN_medians[idx] # Pannonian Relevance

write.csv(test_table1,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/Preliminary_relevance.csv", row.names = FALSE)

# Relevance score :: multiplication of the different scores for total relevance
test_table1.1 = test_table1 %>% mutate(total_relevance = 4*parameter_relevace*Score_Process*
                                       Score_Subfunction/3,
                                     relevance_agriculture = 4*parameter_relevace_agr*Score_Process_agri*
                                       Score_Subfunction_agr/3,
                                     relevance_forest = 4*parameter_relevace_for*Score_Process_for*
                                       Score_Subfunction_for/3,
                                     relevance_urban = 4*parameter_relevace_urb*Score_Process_urb*
                                       Score_Subfunction_urb/3,
                                     relevance_ATC = 4*parameter_relevace_AC*Score_Process_AT*
                                       Score_Subfunction_AT/3,
                                     relevance_ATN = 4*parameter_relevace_AN*Score_Process_AN*
                                       Score_Subfunction_AN/3,
                                     relevance_BOR = 4*parameter_relevace_Boreal*Score_Process_BOR*
                                       Score_Subfunction_BOR/3,
                                     relevance_CON = 4*parameter_relevace_Continental*Score_Process_CON*
                                       Score_Subfunction_CON/3,
                                     relevance_LUS = 4*parameter_relevace_Lusitanian*Score_Process_LUS*
                                       Score_Subfunction_LUS/3,
                                     relevance_MDM = 4*parameter_relevace_MM*Score_Process_MDM*
                                       Score_Subfunction_MDM/3,
                                     relevance_MDN = 4*parameter_relevace_MN*Score_Process_MDN*
                                       Score_Subfunction_MDN,
                                     relevance_MDS = 4*parameter_relevace_MS*Score_Process_MDS*
                                       Score_Subfunction_MDS/3,
                                     relevance_NEM = 4*parameter_relevace_Nemoral*Score_Process_NEM*
                                       Score_Subfunction_NEM/3,
                                     relevance_PAN = 4*parameter_relevace_Pannonian*Score_Process_PAN*
                                       Score_Subfunction_PAN/3)

# Aggregate 
test_table2 = test_table1.1 %>% dplyr::group_by(folder_code,Parameter) %>% summarise(total_relevance = mean(total_relevance,na.rm=TRUE),
                                                                                relevance_agriculture = mean(relevance_agriculture,na.rm=TRUE),
                                                                                relevance_forest = mean(relevance_forest,na.rm=TRUE),
                                                                                relevance_urban = mean(relevance_urban,na.rm=TRUE),
                                                                                relevance_ATC = mean(relevance_ATC,na.rm=TRUE),
                                                                                relevance_ATN = mean(relevance_ATN,na.rm=TRUE),
                                                                                relevance_BOR = mean(relevance_BOR,na.rm=TRUE),
                                                                                relevance_CON = mean(relevance_CON,na.rm=TRUE),
                                                                                relevance_LUS = mean(relevance_LUS,na.rm=TRUE),
                                                                                relevance_MDM = mean(relevance_MDM,na.rm=TRUE),
                                                                                relevance_MDN = mean(relevance_MDN,na.rm=TRUE),
                                                                                relevance_MDS = mean(relevance_MDS,na.rm=TRUE),
                                                                                relevance_NEM = mean(relevance_NEM,na.rm=TRUE),
                                                                                relevance_PAN = mean(relevance_PAN,na.rm=TRUE))

colnames(test_table2)[1] = "Function"
test_table2 = test_table2 %>% mutate_all(~ifelse(is.nan(.), NA, .))
write.csv(test_table2,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/Final_relevance_no_combinations_updated.csv", row.names = FALSE)

# FREQUENCY ----

Cognitive_models    = read_excel("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/Cognitive models_all.xlsx")
colnames(Cognitive_models)[1] = "Function"

# Select columns in CM 
Cognitive_models = Cognitive_models %>% select(c("Function","parameter_name","type",
                                                 "Agriculture","Forest","Urban"))

# Replace na values with 0 
Cognitive_models <- Cognitive_models %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))

# Reduce dataset
Cognitive_models = Cognitive_models %>% filter(type %in% c("chemical", "environmental", 
                                                           "physical", "biological"))

# Total
CM_total_FR = Cognitive_models %>% group_by(Function,parameter_name,type) %>% summarise(n = n(), .groups = "drop")
colnames(CM_total_FR)[4] = "Total_frequency"

# Agriculture
CM_Agriculture    = Cognitive_models %>% filter(Agriculture == 1)
CM_Agriculture_FR = CM_Agriculture %>% group_by(Function,parameter_name,type) %>% summarise(n = n(), .groups = "drop")
colnames(CM_Agriculture_FR)[4] = "Frequency_Agriculture"

# Forest
CM_Forest    = Cognitive_models %>% filter(Forest == 1)
CM_Forest_FR = CM_Forest %>% group_by(Function,parameter_name,type) %>% summarise(n = n(), .groups = "drop")
colnames(CM_Forest_FR)[4] = "Frequency_Forest"

# Urban
CM_Urban    = Cognitive_models %>% filter(Urban == 1)
CM_Urban_FR = CM_Urban %>% group_by(Function,parameter_name,type) %>% summarise(n = n(), .groups = "drop")
colnames(CM_Urban_FR)[4] = "Frequency_Urban"

# Merge Frequencies
Final_frequencies = CM_total_FR
key_test  = paste(CM_total_FR$parameter_name,CM_total_FR$type)
#Agriculture
key_score = paste(CM_Agriculture_FR$parameter_name,CM_Agriculture_FR$type)
idx = match(key_test, key_score)
Final_frequencies$Frequency_Agriculture   = CM_Agriculture_FR$Frequency_Agriculture[idx]
#Forest
key_score = paste(CM_Forest_FR$parameter_name,CM_Forest_FR$type)
idx = match(key_test, key_score)
Final_frequencies$Frequency_Forest   = CM_Forest_FR$Frequency_Forest[idx]
#Urban
key_score = paste(CM_Urban_FR$parameter_name,CM_Urban_FR$type)
idx = match(key_test, key_score)
Final_frequencies$Frequency_Urban   = CM_Urban_FR$Frequency_Urban[idx]

# Replace na values with 0 
Final_frequencies <- Final_frequencies %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))

# Condencing the frequencies per group 

# Terms to search: collembola, fungi, archaea, bacteria, nematodes, acari, protozoa
Final_frequencies_collembola = Final_frequencies %>% filter(str_detect(parameter_name, 'collembola'))
Final_frequencies_collembola = Final_frequencies_collembola %>% summarise(sum(Total_frequency, na.rm = TRUE),
                                                                          sum(Frequency_Agriculture, na.rm = TRUE),
                                                                          sum(Frequency_Forest, na.rm = TRUE),
                                                                          sum(Frequency_Urban, na.rm = TRUE))
colnames(Final_frequencies_collembola) = c("Total_frequency", "Frequency_Agriculture",
                                           "Frequency_Forest", "Frequency_Urban")

Final_frequencies_fungi = Final_frequencies %>%
  filter(str_detect(parameter_name, regex("\\bfungi\\b", ignore_case = TRUE)))
Final_frequencies_fungi      = Final_frequencies_fungi %>% summarise(sum(Total_frequency, na.rm = TRUE),
                                                                     sum(Frequency_Agriculture, na.rm = TRUE),
                                                                     sum(Frequency_Forest, na.rm = TRUE),
                                                                     sum(Frequency_Urban, na.rm = TRUE))
colnames(Final_frequencies_fungi) = c("Total_frequency", "Frequency_Agriculture",
                                      "Frequency_Forest", "Frequency_Urban")

Final_frequencies_archaea    = Final_frequencies %>% filter(str_detect(parameter_name, 'archaea'))
Final_frequencies_archaea    = Final_frequencies_archaea %>% summarise(sum(Total_frequency, na.rm = TRUE),
                                                                       sum(Frequency_Agriculture, na.rm = TRUE),
                                                                       sum(Frequency_Forest, na.rm = TRUE),
                                                                       sum(Frequency_Urban, na.rm = TRUE))
colnames(Final_frequencies_archaea) = c("Total_frequency", "Frequency_Agriculture",
                                        "Frequency_Forest", "Frequency_Urban")

Final_frequencies_bacteria   = Final_frequencies %>% filter(str_detect(parameter_name, 'bacteria') &
                                                              !str_detect(parameter_name, regex("bacterial|grazers", ignore_case = TRUE)))
Final_frequencies_bacteria   = Final_frequencies_bacteria %>% summarise(sum(Total_frequency, na.rm = TRUE),
                                                                        sum(Frequency_Agriculture, na.rm = TRUE),
                                                                        sum(Frequency_Forest, na.rm = TRUE),
                                                                        sum(Frequency_Urban, na.rm = TRUE))
colnames(Final_frequencies_bacteria) = c("Total_frequency", "Frequency_Agriculture",
                                         "Frequency_Forest", "Frequency_Urban")

Final_frequencies_nematodes  = Final_frequencies %>% filter(str_detect(parameter_name, 'nematodes'))
Final_frequencies_nematodes  = Final_frequencies_nematodes %>% summarise(sum(Total_frequency, na.rm = TRUE),
                                                                         sum(Frequency_Agriculture, na.rm = TRUE),
                                                                         sum(Frequency_Forest, na.rm = TRUE),
                                                                         sum(Frequency_Urban, na.rm = TRUE))
colnames(Final_frequencies_nematodes) = c("Total_frequency", "Frequency_Agriculture",
                                          "Frequency_Forest", "Frequency_Urban")


Final_frequencies_acari      = Final_frequencies %>% filter(str_detect(parameter_name, 'acari'))
Final_frequencies_acari      = Final_frequencies_acari %>% summarise(sum(Total_frequency, na.rm = TRUE),
                                                                     sum(Frequency_Agriculture, na.rm = TRUE),
                                                                     sum(Frequency_Forest, na.rm = TRUE),
                                                                     sum(Frequency_Urban, na.rm = TRUE))
colnames(Final_frequencies_acari) = c("Total_frequency", "Frequency_Agriculture",
                                      "Frequency_Forest", "Frequency_Urban")

Final_frequencies_protozoa   = Final_frequencies %>% filter(str_detect(parameter_name, 'protozoa'))
Final_frequencies_protozoa   = Final_frequencies_protozoa %>% summarise(sum(Total_frequency, na.rm = TRUE),
                                                                        sum(Frequency_Agriculture, na.rm = TRUE),
                                                                        sum(Frequency_Forest, na.rm = TRUE),
                                                                        sum(Frequency_Urban, na.rm = TRUE))
colnames(Final_frequencies_protozoa) = c("Total_frequency", "Frequency_Agriculture",
                                         "Frequency_Forest", "Frequency_Urban")

Final_frequencies[grepl("collembola", Final_frequencies$parameter_name, ignore.case = TRUE), 4:7] = Final_frequencies_collembola
Final_frequencies[grepl("fungi", Final_frequencies$parameter_name, ignore.case = TRUE) &
                    !grepl("fungivor", Final_frequencies$parameter_name, ignore.case = TRUE),4:7]                 = Final_frequencies_fungi
Final_frequencies[grepl("archaea", Final_frequencies$parameter_name, ignore.case = TRUE), 4:7]    = Final_frequencies_archaea
Final_frequencies[grepl("bacteria", Final_frequencies$parameter_name, ignore.case = TRUE) &
                    !grepl("bacterial|grazers", Final_frequencies$parameter_name, ignore.case = TRUE),4:7] = Final_frequencies_bacteria
Final_frequencies[grepl("nematodes", Final_frequencies$parameter_name, ignore.case = TRUE), 4:7]  = Final_frequencies_nematodes
Final_frequencies[grepl("acari", Final_frequencies$parameter_name, ignore.case = TRUE), 4:7]      = Final_frequencies_acari
Final_frequencies[grepl("protozoa", Final_frequencies$parameter_name, ignore.case = TRUE), 4:7]   = Final_frequencies_protozoa

# Change values according to Marie's paper
Final_frequencies = Final_frequencies %>% mutate(Frequency_Total = case_when(Total_frequency == 0 ~ 0,
                                                                             Total_frequency == 1 ~ 1,
                                                                             Total_frequency == 2 ~ 2,
                                                                             Total_frequency >= as.numeric(quantile(Final_frequencies$Total_frequency, 0.90)) ~ 4,
                                                                             Total_frequency < as.numeric(quantile(Final_frequencies$Total_frequency, 0.90)) ~ 3))

Final_frequencies = Final_frequencies %>% mutate(Frequency_Agr = case_when(Frequency_Agriculture == 0 ~ 0,
                                                                           Frequency_Agriculture == 1 ~ 1,
                                                                           Frequency_Agriculture == 2 ~ 2,
                                                                           Frequency_Agriculture >= as.numeric(quantile(Final_frequencies$Frequency_Agriculture, 0.90)) ~ 4,
                                                                           Frequency_Agriculture < as.numeric(quantile(Final_frequencies$Frequency_Agriculture, 0.90)) ~ 3))

Final_frequencies = Final_frequencies %>% mutate(Frequency_For = case_when(Frequency_Forest == 0 ~ 0,
                                                                           Frequency_Forest == 1 ~ 1,
                                                                           Frequency_Forest == 2 ~ 2,
                                                                           Frequency_Forest >= as.numeric(quantile(Final_frequencies$Frequency_Forest, 0.90)) ~ 4,
                                                                           Frequency_Forest < as.numeric(quantile(Final_frequencies$Frequency_Forest, 0.90)) ~ 3))

Final_frequencies = Final_frequencies %>% mutate(Frequency_Urb = case_when(Frequency_Urban == 0 ~ 0,
                                                                           Frequency_Urban == 1 ~ 1,
                                                                           Frequency_Urban == 2 ~ 2,
                                                                           Frequency_Urban >= as.numeric(quantile(Final_frequencies$Frequency_Urban, 0.90)) ~ 4,
                                                                           Frequency_Urban < as.numeric(quantile(Final_frequencies$Frequency_Urban, 0.90)) ~ 3))

Final_frequencies = Final_frequencies %>% select(Function,parameter_name,type,Frequency_Total,Frequency_Agr,
                                                 Frequency_For,Frequency_Urb)

write.csv(Final_frequencies,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/Final_frequencies_updated_2.csv", row.names = FALSE)

# Inclusion for processes that have indicators

# Call technical aspects
Final_technical_aspects = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/Final_technical_aspects.csv")
process_with_indicators = Final_technical_aspects %>% filter(parameter_type == "process")
process_with_indicators = process_with_indicators %>%
  mutate(across(where(is.character), tolower))
process_with_indicators = process_with_indicators %>% select(c("parameter_type",
                                                               "parameter_name"))
process_with_indicators = process_with_indicators %>% distinct(parameter_name)
colnames(process_with_indicators) = "name"

# Call process scores
process_scores      = read_excel("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/process_scores.xlsx")
process_scores = process_scores %>%
  mutate(across(where(is.character), tolower))

process_scores = process_scores %>%
  mutate(name = str_replace_all(name, c(
    "volatilization" = "volatilisation",
    "mineralization" = "mineralisation",
    "depolymerization" = "depolymerisation",
    "stabilization" = "stabilisation",
    "salinization" = "salinisation",
    "senescense" = "senescence",
    "nitrogen fixation" = "n fixation",
    "adsorption / desorption" = "adsorption/desorption",
    "nitrification / denitrification" = "nitrification/denitrification",
    "plant uptake-transpiration" = "plant uptake/transpiration",
    "ponding / run-off" = "ponding/run-off",
    "senescence and debris formation" = "senescense/debris formation" # correcting the typo
  )))

# Match contents

process_scores_merge = merge(process_scores, process_with_indicators, by = "name")
process_scores_merge = process_scores_merge %>% select(c("Function","Subfunction",
                                                         "name"))
key_test  = paste(process_scores_merge$name,process_scores_merge$Function)
key_score = paste(process_scores$name, process_scores$Function)

idx = match(key_test, key_score)

# Match the scores of the processes 
process_scores_merge$Score_Process = process_scores$Full_medians[idx] # Total Relevance
process_scores_merge$Score_Process_agri = process_scores$Agr_medians[idx] # Agricultural Relevance
process_scores_merge$Score_Process_for  = process_scores$For_medians[idx] # Forest Relevance
process_scores_merge$Score_Process_urb  = process_scores$Urb_medians[idx] # Urban Relevance

process_scores_merge$Score_Process_AT  = process_scores$ATC_medians[idx] # Atlantic Central Relevance
process_scores_merge$Score_Process_AN  = (process_scores$ATC_medians[idx] + 
                                            process_scores$BOR_medians[idx] + 
                                            process_scores$CON_medians[idx] + 
                                            process_scores$MDN_medians[idx] + 
                                            process_scores$MDS_medians[idx] + 
                                            process_scores$PAN_medians[idx])/6 # Atlantic North Relevance
process_scores_merge$Score_Process_BOR  = process_scores$BOR_medians[idx] # Boreal Relevance
process_scores_merge$Score_Process_CON  = process_scores$CON_medians[idx] # Continental Relevance
process_scores_merge$Score_Process_LUS  = (process_scores$ATC_medians[idx] + 
                                             process_scores$BOR_medians[idx] + 
                                             process_scores$CON_medians[idx] + 
                                             process_scores$MDN_medians[idx] + 
                                             process_scores$MDS_medians[idx] + 
                                             process_scores$PAN_medians[idx])/6 # Lusitanian Relevance
process_scores_merge$Score_Process_MDM  = (process_scores$ATC_medians[idx] + 
                                             process_scores$BOR_medians[idx] + 
                                             process_scores$CON_medians[idx] + 
                                             process_scores$MDN_medians[idx] + 
                                             process_scores$MDS_medians[idx] + 
                                             process_scores$PAN_medians[idx])/6 # Mediterranean Mountains Relevance
process_scores_merge$Score_Process_MDN  = process_scores$MDN_medians[idx] # Mediterranean North Relevance
process_scores_merge$Score_Process_MDS  = process_scores$MDS_medians[idx] # Mediterranean South Relevance
process_scores_merge$Score_Process_NEM  = (process_scores$ATC_medians[idx] + 
                                             process_scores$BOR_medians[idx] + 
                                             process_scores$CON_medians[idx] + 
                                             process_scores$MDN_medians[idx] + 
                                             process_scores$MDS_medians[idx] + 
                                             process_scores$PAN_medians[idx])/6 # Nemoral Relevance
process_scores_merge$Score_Process_PAN  = process_scores$PAN_medians[idx] # Pannonian Relevance

# Match the scores of the subfunctions

idx = match(process_scores_merge$Subfunction, process_scores$name)
process_scores_merge$Score_Subfunction = process_scores$Full_medians[idx]  # Total Relevance
process_scores_merge$Score_Subfunction_agr = process_scores$Agr_medians[idx]  # Agricultural Relevance
process_scores_merge$Score_Subfunction_for = process_scores$For_medians[idx]  # Forest Relevance
process_scores_merge$Score_Subfunction_urb = process_scores$Urb_medians[idx]  # Urban Relevance

process_scores_merge$Score_Subfunction_AT  = process_scores$ATC_medians[idx] # Atlantic Central Relevance
process_scores_merge$Score_Subfunction_AN  = (process_scores$ATC_medians[idx] + 
                                                process_scores$BOR_medians[idx] + 
                                                process_scores$CON_medians[idx] + 
                                                process_scores$MDN_medians[idx] + 
                                                process_scores$MDS_medians[idx] + 
                                                process_scores$PAN_medians[idx])/6 # Atlantic North Relevance
process_scores_merge$Score_Subfunction_BOR  = process_scores$BOR_medians[idx] # Boreal Relevance
process_scores_merge$Score_Subfunction_CON  = process_scores$CON_medians[idx] # Continental Relevance
process_scores_merge$Score_Subfunction_LUS  = (process_scores$ATC_medians[idx] + 
                                                 process_scores$BOR_medians[idx] + 
                                                 process_scores$CON_medians[idx] + 
                                                 process_scores$MDN_medians[idx] + 
                                                 process_scores$MDS_medians[idx] + 
                                                 process_scores$PAN_medians[idx])/6 # Lusitanian Relevance
process_scores_merge$Score_Subfunction_MDM  = (process_scores$ATC_medians[idx] + 
                                                 process_scores$BOR_medians[idx] + 
                                                 process_scores$CON_medians[idx] + 
                                                 process_scores$MDN_medians[idx] + 
                                                 process_scores$MDS_medians[idx] + 
                                                 process_scores$PAN_medians[idx])/6 # Mediterranean Mountains Relevance
process_scores_merge$Score_Subfunction_MDN  = process_scores$MDN_medians[idx] # Mediterranean North Relevance
process_scores_merge$Score_Subfunction_MDS  = process_scores$MDS_medians[idx] # Mediterranean South Relevance
process_scores_merge$Score_Subfunction_NEM  = (process_scores$ATC_medians[idx] + 
                                                 process_scores$BOR_medians[idx] + 
                                                 process_scores$CON_medians[idx] + 
                                                 process_scores$MDN_medians[idx] + 
                                                 process_scores$MDS_medians[idx] + 
                                                 process_scores$PAN_medians[idx])/6 # Nemoral Relevance
process_scores_merge$Score_Subfunction_PAN  = process_scores$PAN_medians[idx] # Pannonian Relevance

# Relevance score :: multiplication of the different scores for total relevance
process_scores_merge = process_scores_merge %>% mutate(total_relevance = 4*Score_Process*
                                                         Score_Subfunction/3,
                                                       relevance_agriculture = 4*Score_Process_agri*
                                                         Score_Subfunction_agr/3,
                                                       relevance_forest = 4*Score_Process_for*
                                                         Score_Subfunction_for/3,
                                                       relevance_urban = 4*Score_Process_urb*
                                                         Score_Subfunction_urb/3,
                                                       relevance_ATC = 4*Score_Process_AT*
                                                         Score_Subfunction_AT/3,
                                                       relevance_ATN = 4*Score_Process_AN*
                                                         Score_Subfunction_AN/3,
                                                       relevance_BOR = 4*Score_Process_BOR*
                                                         Score_Subfunction_BOR/3,
                                                       relevance_CON = 4*Score_Process_CON*
                                                         Score_Subfunction_CON/3,
                                                       relevance_LUS = Score_Process_LUS*
                                                         Score_Subfunction_LUS/3,
                                                       relevance_MDM = 4*Score_Process_MDM*
                                                         Score_Subfunction_MDM/3,
                                                       relevance_MDN = 4*Score_Process_MDN*
                                                         Score_Subfunction_MDN/3,
                                                       relevance_MDS = 4*Score_Process_MDS*
                                                         Score_Subfunction_MDS/3,
                                                       relevance_NEM = 4*Score_Process_NEM*
                                                         Score_Subfunction_NEM/3,
                                                       relevance_PAN = 4*Score_Process_PAN*
                                                         Score_Subfunction_PAN/3)

# Aggregate 
process_scores_merge.1 = process_scores_merge %>% dplyr::group_by(Function,name) %>% summarise(total_relevance = mean(total_relevance,na.rm=TRUE),
                                                                                               relevance_agriculture = mean(relevance_agriculture,na.rm=TRUE),
                                                                                               relevance_forest = mean(relevance_forest,na.rm=TRUE),
                                                                                               relevance_urban = mean(relevance_urban,na.rm=TRUE),
                                                                                               relevance_ATC = mean(relevance_ATC,na.rm=TRUE),
                                                                                               relevance_ATN = mean(relevance_ATN,na.rm=TRUE),
                                                                                               relevance_BOR = mean(relevance_BOR,na.rm=TRUE),
                                                                                               relevance_CON = mean(relevance_CON,na.rm=TRUE),
                                                                                               relevance_LUS = mean(relevance_LUS,na.rm=TRUE),
                                                                                               relevance_MDM = mean(relevance_MDM,na.rm=TRUE),
                                                                                               relevance_MDN = mean(relevance_MDN,na.rm=TRUE),
                                                                                               relevance_MDS = mean(relevance_MDS,na.rm=TRUE),
                                                                                               relevance_NEM = mean(relevance_NEM,na.rm=TRUE),
                                                                                               relevance_PAN = mean(relevance_PAN,na.rm=TRUE))

write.csv(process_scores_merge.1,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/Final_relevance_no_combinations_processes.csv", row.names = FALSE)

# Frequency processes

Cognitive_models    = read_excel("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/Cognitive models_all.xlsx")

Cognitive_models = Cognitive_models %>%
  mutate(name = str_replace_all(process, c(
    "volatilization" = "volatilisation",
    "mineralization" = "mineralisation",
    "depolymerization" = "depolymerisation",
    "stabilization" = "stabilisation",
    "salinization" = "salinisation",
    "senescense" = "senescence",
    "nitrogen fixation" = "n fixation",
    "adsorption / desorption" = "adsorption/desorption",
    "nitrification / denitrification" = "nitrification/denitrification",
    "plant uptake-transpiration" = "plant uptake/transpiration",
    "ponding / run-off" = "ponding/run-off",
    "senescence and debris formation" = "senescense/debris formation" # correcting the typo
  )))

colnames(Cognitive_models)[1] = "Function"

# Select columns in CM 
Cognitive_models = Cognitive_models %>% select(c("Function","process","type",
                                                 "Agriculture","Forest","Urban"))
# Reduce dataset
Cognitive_models = Cognitive_models %>% filter(type %in% c("process"))

# Total
CM_total_FR = Cognitive_models %>% group_by(Function,process) %>% 
  summarise(n = n(), .groups = "drop")
colnames(CM_total_FR)[3] = "Total_frequency"
colnames(CM_total_FR)[2] = "name"

# Merge
process_scores_merge.2 = merge(process_scores_merge.1,CM_total_FR, by= "name")
write.csv(process_scores_merge.2,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/Final_frequencies_process.csv", row.names = FALSE)

# TECHNICAL ASPECTS ----

# Call data 
scoring_criteria = read_excel("data/Method list assessment file.xlsx",
                              sheet = "Scoring Criteria", range = "B22:M53")
method_list    = read_excel("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/Method list assessment file_v2.xlsx",
                            sheet = "Method list", range = "A2:AR1019")
# Test for Applicability & Discrimination 

# Agriculture
final_list       = method_list %>% mutate(agriculture = case_when(`Is it applicable to agricultural soils?` == "Yes" ~ 1,
                                                                  `Is it applicable to agricultural soils?` == "No" ~ 0,
                                                                  `Is it applicable to agricultural soils?` == "Under specific conditions" ~ 2,
                                                                  `Is it applicable to agricultural soils?` == NA ~ NA))

# Urban
final_list       = final_list %>% mutate(urban = case_when(`Is it applicable to urban soils?` == "Yes" ~ 1,
                                                           `Is it applicable to urban soils?` == "No" ~ 0,
                                                           `Is it applicable to urban soils?` == "Under specific conditions" ~ 2,
                                                           `Is it applicable to urban soils?` == NA ~ NA))

# Forest
final_list       = final_list %>% mutate(forest = case_when(`Is it applicable to forest soils?` == "Yes" ~ 1,
                                                            `Is it applicable to forest soils?` == "No" ~ 0,
                                                            `Is it applicable to forest soils?` == "Under specific conditions" ~ 2,
                                                            `Is it applicable to forest soils?` == NA ~ NA))

# Scale
final_list       = final_list %>% mutate(scale = case_when(`What is the smallest spatial scale that this method is suitable for?` == "field level" ~ 2,
                                                           `What is the smallest spatial scale that this method is suitable for?` == "Point or pixel" ~ 3,
                                                           `What is the smallest spatial scale that this method is suitable for?` == "Landscape or basin level discrimination" ~ 1,
                                                           `What is the smallest spatial scale that this method is suitable for?` == NA ~ NA,
                                                           `What is the smallest spatial scale that this method is suitable for?` == "Yes" ~ NA))

# In field Collection 

# How long does it take to take one sample in the field? == sample_collection_1
final_list       = final_list %>% mutate(sample_collection_1 = case_when(`How long does it take to take one sample in the field?` == "Less than an hour" ~ 3,
                                                                         `How long does it take to take one sample in the field?` == "More than an hour" ~ 1,
                                                                         `How long does it take to take one sample in the field?` == "No field visits needed" ~ 4,
                                                                         `How long does it take to take one sample in the field?` == "About an hour" ~ 2,
                                                                         `How long does it take to take one sample in the field?` == NA ~ NA))

# How many samples need to be taken per location or treatment? == sample_collection_2
final_list       = final_list %>% mutate(sample_collection_2 = case_when(`How many samples need to be taken per location or treatment?` == "More than one replicate needed per location/treatment" ~ 0,
                                                                         `How many samples need to be taken per location or treatment?` == "One composite sample per location/treatment" ~ 1,
                                                                         `How many samples need to be taken per location or treatment?` == NA ~ NA))

# How many sampling times are required for the method to be realised? == sample_collection_3
final_list       = final_list %>% mutate(sample_collection_3 = case_when(`How many visits to the field do we need?` == "One time enough" ~ 2,
                                                                         `How many visits to the field do we need?` == "More than one sampling needed" ~ 1,
                                                                         `How many visits to the field do we need?` == "The method does not require field visits" ~ 3,
                                                                         `How many visits to the field do we need?` == NA ~ NA))

# Does deployment of the method require specialised tools in the field? == sample_collection_4
final_list       = final_list %>% mutate(sample_collection_4 = case_when(`Does the method require specialised tools in the field?` == "Specialised tools needed (such as density rings or enchytraeid corers needed)" ~ 1,
                                                                         `Does the method require specialised tools in the field?` == "No specialised tools, a spade or a common auger work" ~ 2,
                                                                         `Does the method require specialised tools in the field?` == "The method does not require field visits" ~ 3,
                                                                         `Does the method require specialised tools in the field?` == NA ~ NA))

# Sample storage, amount and archivability 

# What mass of soil is needed for sampling and determination? == storage.amount.archivability_1
final_list       = final_list %>% mutate(storage.amount.archivability_1 = case_when(`How many grams/kilograms do we need for the method?` == "Small mass (<1 kg or < 2 L)" ~ 2,
                                                                                    `How many grams/kilograms do we need for the method?` == "Very small mass (<100g or < 0.5 L)" ~ 3,
                                                                                    `How many grams/kilograms do we need for the method?` == "Large mass (>1 kg or > 2 L)" ~ 1,
                                                                                    `How many grams/kilograms do we need for the method?` == NA ~ NA))

# Given appropriate preservation, how soon do post-sampling measures need to be applied? == storage.amount.archivability_2
final_list       = final_list %>% mutate(storage.amount.archivability_2 = case_when(`How much time can we store the sample before starting with the analysis?` == "Within 1 week" ~ 1,
                                                                                    `How much time can we store the sample before starting with the analysis?` == "Within 6 months to year" ~ 3,
                                                                                    `How much time can we store the sample before starting with the analysis?` == "Within 1 month" ~ 2,
                                                                                    `How much time can we store the sample before starting with the analysis?` == "Not possible" ~ 0,
                                                                                    `How much time can we store the sample before starting with the analysis?` == NA ~ NA))

# In what form is it possible to archive soil samples (i.e. over decades) in order to accurately re-determine these properties? == storage.amount.archivability_3
final_list       = final_list %>% mutate(storage.amount.archivability_3 = case_when(`How long can we store the sample and still get reasonable results?` == "Archivable as fixated or extracted sample or as frozen soil" ~ 1,
                                                                                    `How long can we store the sample and still get reasonable results?` == "Archivable as dried soil sample" ~ 2,
                                                                                    `How long can we store the sample and still get reasonable results?` == "Not archivable" ~ 0,
                                                                                    `How long can we store the sample and still get reasonable results?` == NA ~ NA))

# Duration 

# How much time does it take to pre-process the sample? == duration ** (additional level which is not here)
final_list       = final_list %>% mutate(duration = case_when(`How long does it take to process the sample?` == "In adition, extractions also needed" ~ 1,
                                                              `How long does it take to process the sample?` == "soil drying and/or sieving additionally needed" ~ 2,
                                                              `How long does it take to process the sample?` == "only coarse debris removal or homogeneization needed" ~ 3,
                                                              `How long does it take to process the sample?` == "In adition, extractions and/or an incubation period also needed" ~ 4,
                                                              `How long does it take to process the sample?` == "No preprocessing needed" ~ 0,
                                                              `How long does it take to process the sample?` == "Archivable as fixated or extracted sample or as frozen soil" ~ NA,
                                                              `How long does it take to process the sample?` == NA ~ NA))

# Reference material 

# Is the method amenable to the prescription and provision of such material? == reference.material ** (other way around, level missing)
final_list       = final_list %>% mutate(reference.material = case_when(`Do we need to include or develop reference material, or ground truth the measurements?` == "None" ~ 0,
                                                                        `Do we need to include or develop reference material, or ground truth the measurements?` == "Yes, an internal reference" ~ 2,
                                                                        `Do we need to include or develop reference material, or ground truth the measurements?` == "Yes, an international / standard reference" ~ 3,
                                                                        `Do we need to include or develop reference material, or ground truth the measurements?` == "Potential but not often applied in practice" ~ 1,
                                                                        `Do we need to include or develop reference material, or ground truth the measurements?` == NA ~ NA))

# Throughput 

# How many samples can be processed with optimised laboratory systems and dedicated staff? == throughput 
final_list       = final_list %>% mutate(throughput = case_when(`How many samples can be processed per week?` == "< 20 /week" ~ 1,
                                                                `How many samples can be processed per week?` == "> 100/week" ~ 4,
                                                                `How many samples can be processed per week?` == "20-50/week" ~ 2,
                                                                `How many samples can be processed per week?` == "50-100/week" ~ 3,
                                                                `How many samples can be processed per week?` == NA ~ NA))

# Lab analysis cost per sample 
# If sent to an external lab, how much would it cost to run the sample? == lab.cost 
final_list       = final_list %>% mutate(lab.cost = case_when(`What is the cost per sample/image?` == "less than 10 euros/sample" ~ 3,
                                                              `What is the cost per sample/image?` == "10-50 euros per sample" ~ 2,
                                                              `What is the cost per sample/image?` == "more than 50 euros a sample" ~ 1,
                                                              `What is the cost per sample/image?` == "Free" ~ 4,
                                                              `What is the cost per sample/image?` == NA ~ NA))

# Ease of use in laboratory 
# What is the level of skill required to perform this method in the laboratory? == lab.use 
final_list       = final_list %>% mutate(lab.use = case_when(`Do we need a specialized lab for the method?` == "Specialised" ~ 1,
                                                             `Do we need a specialized lab for the method?` == "Moderate" ~ 2,
                                                             `Do we need a specialized lab for the method?` == "Low" ~ 3,
                                                             `Do we need a specialized lab for the method?` == NA ~ NA))

# Infrastructure 
# Does the implementation of the method require a stable internet connection? == internet.conection
final_list       = final_list %>% mutate(internet.conection = case_when(`Does the implementation of the method require a stable internet connection?` == "no" ~ 1,
                                                                        `Does the implementation of the method require a stable internet connection?` == "No" ~ 1,
                                                                        `Does the implementation of the method require a stable internet connection?` == "yes" ~ 0,
                                                                        `Does the implementation of the method require a stable internet connection?` == NA ~ NA))

# Data storage and processing 
# "How much data storage is needed to store and process the data? == data.storage.1
final_list       = final_list %>% mutate(data.storage.1 = case_when(`How much data storage is needed to store and process the data? ` == "Kilobytes (KB)" ~ 4,
                                                                    `How much data storage is needed to store and process the data? ` == "Gigabytes (GB)" ~ 2,
                                                                    `How much data storage is needed to store and process the data? ` == "Megabytes (MB)" ~ 3,
                                                                    `How much data storage is needed to store and process the data? ` == "Terabytes (TB)" ~ 1,
                                                                    `How much data storage is needed to store and process the data? ` == NA ~ NA))

# How much data processing capacity is needed? == data.storage.2 ** (workstation is easier than having a laptop)
final_list       = final_list %>% mutate(data.storage.2 = case_when(`How much processing capacity is needed? ` == "laptop" ~ 4,
                                                                    `How much processing capacity is needed? ` == "mainframe" ~ 2,
                                                                    `How much processing capacity is needed? ` == "workstation" ~ 3,
                                                                    `How much processing capacity is needed? ` == "computer" ~ 1,
                                                                    `How much processing capacity is needed? ` == "Supercomputer" ~ 0,
                                                                    `How much processing capacity is needed? ` == NA ~ NA))

# How much image processing is needed?  == data.storage.3
final_list       = final_list %>% mutate(data.storage.3 = case_when(`How much image processing is needed?` == "No processing needed" ~ 4,
                                                                    `How much image processing is needed?` == "Specialised" ~ 1,
                                                                    `How much image processing is needed?` == "Straightforward" ~ 3,
                                                                    `How much image processing is needed?` == "Moderate" ~ 2,
                                                                    `How much image processing is needed?` == NA ~ NA))

# How much data analysis  is required to obtain the data product?  == data.storage.4 
final_list       = final_list %>% mutate(data.storage.4 = case_when(`How much data analysis  is required to obtain the data product?` == "No processing needed" ~ 4,
                                                                    `How much data analysis  is required to obtain the data product?` == "Specialised" ~ 1,
                                                                    `How much data analysis  is required to obtain the data product?` == "Moderate" ~ 2,
                                                                    `How much data analysis  is required to obtain the data product?` == "Straightforward" ~ 3,
                                                                    `How much data analysis  is required to obtain the data product?` == NA ~ NA))

# Temporal resolution 
# What is the temporal resolution of the data product?  == temporal.resolution
final_list       = final_list %>% mutate(temporal.resolution = case_when(`What is the temporal resolution of the data product? ` == "Annual-seasonal" ~ 1,
                                                                         `What is the temporal resolution of the data product? ` == "Daily-hourly" ~ 4,
                                                                         `What is the temporal resolution of the data product? ` == "Long-term average" ~ 0,
                                                                         `What is the temporal resolution of the data product? ` == "Weekly" ~ 3,
                                                                         `What is the temporal resolution of the data product? ` == "Monthly" ~ 2,
                                                                         `What is the temporal resolution of the data product? ` == NA ~ NA))
# Spatial resolution 
# How much land surface area can be covered by this data product?  == spatial.resolution.1 ** (what is the logic of the scores?)
final_list       = final_list %>% mutate(spatial.resolution.1 = case_when(`How much land surface area can be covered by this method/data product?  ` == "field" ~ 4,
                                                                          `How much land surface area can be covered by this method/data product?  ` == "Continent" ~ 1,
                                                                          `How much land surface area can be covered by this method/data product?  ` == "Global" ~ 0,
                                                                          `How much land surface area can be covered by this method/data product?  ` == "region" ~ 3,
                                                                          `How much land surface area can be covered by this method/data product?  ` == "Country" ~ 2,
                                                                          `How much land surface area can be covered by this method/data product?  ` == NA ~ NA))

# What is the spatial resolution of the data product?  == spatial.resolution.2 ** (what is the logic of the scores?)
final_list       = final_list %>% mutate(spatial.resolution.2 = case_when(`What is the spatial resolution of the data product? ` == "30-10 m" ~ 3,
                                                                          `What is the spatial resolution of the data product? ` == "Less than 1 m" ~ 4,
                                                                          `What is the spatial resolution of the data product? ` == "100-30 m" ~ 2,
                                                                          `What is the spatial resolution of the data product? ` == "500-100m" ~ 1,
                                                                          `What is the spatial resolution of the data product? ` == "More than 1km" ~ 0,
                                                                          `What is the spatial resolution of the data product? ` == NA ~ NA))

# Spectral resolution	
# What is the spectral resolution of the data product?  == spectral.resolution.1 ** (logic of the scores)
final_list       = final_list %>% mutate(spectral.resolution.1 = case_when(`What is the spectral resolution of the data product?` == "Hyper-band" ~ 3,
                                                                           `What is the spectral resolution of the data product?` == "Multi-band" ~ 2,
                                                                           `What is the spectral resolution of the data product?` == "single band" ~ 1,
                                                                           `What is the spectral resolution of the data product?` == NA ~ NA))

# Which wavelength range(s) does it cover?  == spectral.resolution.2
final_list       = final_list %>% mutate(spectral.resolution.2 = case_when(`What wavelength range does it cover? ` == "UV" ~ 2,
                                                                           `What wavelength range does it cover? ` == "VNIR" ~ 4,
                                                                           `What wavelength range does it cover? ` == "VIS" ~ 3,
                                                                           `What wavelength range does it cover? ` == "Thermal" ~ 7,
                                                                           `What wavelength range does it cover? ` == "Microwave" ~ 8,
                                                                           `What wavelength range does it cover? ` == "Gamma-ray" ~ 0,
                                                                           `What wavelength range does it cover? ` == "X-ray" ~ 1,
                                                                           `What wavelength range does it cover? ` == "SWIR" ~ 5,
                                                                           `What wavelength range does it cover? ` == "MIR" ~ 6,
                                                                           `What wavelength range does it cover? ` == NA ~ NA))

# Interpretation	
# What is the level of skill required to process the raw data ?  == interpretation
final_list       = final_list %>% mutate(interpretation = case_when(`Do we need a high level of expertise to interpret and process the data?` == "Straightforward" ~ 3,
                                                                    `Do we need a high level of expertise to interpret and process the data?` == "Moderate" ~ 2,
                                                                    `Do we need a high level of expertise to interpret and process the data?` == "Specialised" ~ 1,
                                                                    `Do we need a high level of expertise to interpret and process the data?` == NA ~ NA))

# Data license	
# Can the data be publicly shared/can the data product be freely shared with other users?  == data.license
final_list       = final_list %>% mutate(data.license = case_when(`Can the data be publicly shared/can the data product be freely shared with other users? ` == "Yes, the data can be fully shared" ~ 2,
                                                                  `Can the data be publicly shared/can the data product be freely shared with other users? ` == "no" ~ 0,
                                                                  `Can the data be publicly shared/can the data product be freely shared with other users? ` == "Only processed data products can be shared" ~ 1,
                                                                  `Can the data be publicly shared/can the data product be freely shared with other users? ` == NA ~ NA))

# Software use	
# Do we need a specialized software for the method?  == software
final_list       = final_list %>% mutate(software = case_when(`Do we need a specialized software for the method? ` == "no" ~ 2,
                                                              `Do we need a specialized software for the method? ` == "the software is specialised, but freely available" ~ 1,
                                                              `Do we need a specialized software for the method? ` == "the software is specialised and requires payment" ~ 0,
                                                              `Do we need a specialized software for the method? ` == NA ~ NA))

# Reproducibility	
# How reproducible is this method? == reproducibility
final_list       = final_list %>% mutate(reproducibility = case_when(`How reproducible is this method?` == "Moderate" ~ 2,
                                                                     `How reproducible is this method?` == "Low" ~ 1,
                                                                     `How reproducible is this method?` == "High" ~ 3,
                                                                     `How reproducible is this method?` == NA ~ NA))

# Deployment	
# Is this method well-established or still under development? == deployment
final_list       = final_list %>% mutate(deployment = case_when(`Is this method well-established or still under development?` == "Not ready, years of development needed" ~ 0,
                                                                `Is this method well-established or still under development?` == "Developed for experimental use" ~ 1,
                                                                `Is this method well-established or still under development?` == "Fully developed for routine use" ~ 2,
                                                                `Is this method well-established or still under development?` == NA ~ NA))
# Replace na values with 0 
final_list <- final_list %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))
write.csv(final_list,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/Final_technical_aspects_updated.csv", row.names = FALSE)

# LOGICAL SIEVE 2.0 ----

# Call data ----
Frequencies = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/Final_frequencies_updated.csv")
Frequencies = Frequencies %>%
  mutate(across(where(is.character), tolower))
Frequencies$parameter_name[1] = "cation exchange capacity"
Frequencies$parameter_name[2] = "calcium : magnesium ratio"
colnames(Frequencies)[1]      = "Parameter"
Relevance_no_combinations = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/Final_relevance_no_combinations_updated.csv")
Relevance_no_combinations = Relevance_no_combinations %>%
  mutate(across(where(is.character), tolower))
Frequencies_processes = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/Final_frequencies_process.csv")

# PERTINENCE LEVEL ONE ----

# Climate Regulation
Relevance_CR = Relevance_no_combinations %>% filter(Function == "climate regulation")
Pertinece_1_CR = merge(Relevance_CR, Frequencies, by = "Parameter")
Pertinece_1_CR = Pertinece_1_CR %>% mutate(total_pertinence = total_relevance*Frequency_Total,
                                           Agricultural_pertinence = relevance_agriculture*Frequency_Agr,
                                           Forest_pertinence = relevance_forest*Frequency_For,
                                           Urban_pertinence = relevance_urban*Frequency_Urb,
                                           ATC_pertinence = relevance_ATC*Frequency_Total,
                                           ATN_pertinence = relevance_ATN*Frequency_Total,
                                           BOR_pertinence = relevance_BOR*Frequency_Total,
                                           CON_pertinence = relevance_CON*Frequency_Total,
                                           LUS_pertinence = relevance_LUS*Frequency_Total,
                                           MDM_pertinence = relevance_MDM*Frequency_Total,
                                           MDN_pertinence = relevance_MDN*Frequency_Total,
                                           MDS_pertinence = relevance_MDS*Frequency_Total,
                                           NEM_pertinence = relevance_NEM*Frequency_Total,
                                           PAN_pertinence = relevance_PAN*Frequency_Total)
Pertinece_1_CR = Pertinece_1_CR %>% select(Parameter,type,total_pertinence,Agricultural_pertinence,
                                           Forest_pertinence,Urban_pertinence,ATC_pertinence,
                                           ATN_pertinence,BOR_pertinence,CON_pertinence,
                                           LUS_pertinence,MDN_pertinence,MDM_pertinence,
                                           MDS_pertinence,NEM_pertinence,PAN_pertinence)
# Add processes
Frequencies_processes = Frequencies_processes %>% filter(Function == "climate regulation")
Frequencies_processes_CR = Frequencies_processes %>% mutate(total_pertinence = total_relevance*Total_frequency,
                                           Agricultural_pertinence = relevance_agriculture*Total_frequency,
                                           Forest_pertinence = relevance_forest*Total_frequency,
                                           Urban_pertinence = relevance_urban*Total_frequency,
                                           ATC_pertinence = relevance_ATC*Total_frequency,
                                           ATN_pertinence = relevance_ATN*Total_frequency,
                                           BOR_pertinence = relevance_BOR*Total_frequency,
                                           CON_pertinence = relevance_CON*Total_frequency,
                                           LUS_pertinence = relevance_LUS*Total_frequency,
                                           MDM_pertinence = relevance_MDM*Total_frequency,
                                           MDN_pertinence = relevance_MDN*Total_frequency,
                                           MDS_pertinence = relevance_MDS*Total_frequency,
                                           NEM_pertinence = relevance_NEM*Total_frequency,
                                           PAN_pertinence = relevance_PAN*Total_frequency)
Frequencies_processes_CR = Frequencies_processes_CR %>% mutate(type = rep("process", nrow(Frequencies_processes_CR)))

Pertinece_proc_CR = Frequencies_processes_CR %>% select(name,type,total_pertinence,Agricultural_pertinence,
                                           Forest_pertinence,Urban_pertinence,ATC_pertinence,
                                           ATN_pertinence,BOR_pertinence,CON_pertinence,
                                           LUS_pertinence,MDN_pertinence,MDM_pertinence,
                                           MDS_pertinence,NEM_pertinence,PAN_pertinence)
colnames(Pertinece_proc_CR)[1] = "Parameter"

Pertinence_total_CR = as.data.frame(rbind(Pertinece_1_CR,Pertinece_proc_CR))

write.csv(Pertinence_total_CR,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/ranking_parameters_CR_new.csv", row.names = FALSE)

# Nutrient Cycling
Relevance_NC = Relevance_no_combinations %>% filter(Function == "nutrient cycling")
Pertinece_1_NC = merge(Relevance_NC, Frequencies, by = "Parameter")
Pertinece_1_NC = Pertinece_1_NC %>% mutate(total_pertinence = total_relevance*Frequency_Total,
                                           Agricultural_pertinence = relevance_agriculture*Frequency_Agr,
                                           Forest_pertinence = relevance_forest*Frequency_For,
                                           Urban_pertinence = relevance_urban*Frequency_Urb,
                                           ATC_pertinence = relevance_ATC*Frequency_Total,
                                           ATN_pertinence = relevance_ATN*Frequency_Total,
                                           BOR_pertinence = relevance_BOR*Frequency_Total,
                                           CON_pertinence = relevance_CON*Frequency_Total,
                                           LUS_pertinence = relevance_LUS*Frequency_Total,
                                           MDM_pertinence = relevance_MDM*Frequency_Total,
                                           MDN_pertinence = relevance_MDN*Frequency_Total,
                                           MDS_pertinence = relevance_MDS*Frequency_Total,
                                           NEM_pertinence = relevance_NEM*Frequency_Total,
                                           PAN_pertinence = relevance_PAN*Frequency_Total)
Pertinece_1_NC = Pertinece_1_NC %>% select(Parameter,type,total_pertinence,Agricultural_pertinence,
                                           Forest_pertinence,Urban_pertinence,ATC_pertinence,
                                           ATN_pertinence,BOR_pertinence,CON_pertinence,
                                           LUS_pertinence,MDN_pertinence,MDM_pertinence,
                                           MDS_pertinence,NEM_pertinence,PAN_pertinence)
# Add processes
Frequencies_processes_NC = Frequencies_processes %>% filter(Function == "nutrient cycling")
Frequencies_processes_NC = Frequencies_processes_NC %>% mutate(total_pertinence = total_relevance*Total_frequency,
                                                            Agricultural_pertinence = relevance_agriculture*Total_frequency,
                                                            Forest_pertinence = relevance_forest*Total_frequency,
                                                            Urban_pertinence = relevance_urban*Total_frequency,
                                                            ATC_pertinence = relevance_ATC*Total_frequency,
                                                            ATN_pertinence = relevance_ATN*Total_frequency,
                                                            BOR_pertinence = relevance_BOR*Total_frequency,
                                                            CON_pertinence = relevance_CON*Total_frequency,
                                                            LUS_pertinence = relevance_LUS*Total_frequency,
                                                            MDM_pertinence = relevance_MDM*Total_frequency,
                                                            MDN_pertinence = relevance_MDN*Total_frequency,
                                                            MDS_pertinence = relevance_MDS*Total_frequency,
                                                            NEM_pertinence = relevance_NEM*Total_frequency,
                                                            PAN_pertinence = relevance_PAN*Total_frequency)
Frequencies_processes_NC = Frequencies_processes_NC %>% mutate(type = rep("process", nrow(Frequencies_processes_NC)))

Pertinece_proc_NC = Frequencies_processes_NC %>% select(name,type,total_pertinence,Agricultural_pertinence,
                                                        Forest_pertinence,Urban_pertinence,ATC_pertinence,
                                                        ATN_pertinence,BOR_pertinence,CON_pertinence,
                                                        LUS_pertinence,MDN_pertinence,MDM_pertinence,
                                                        MDS_pertinence,NEM_pertinence,PAN_pertinence)
colnames(Pertinece_proc_NC)[1] = "Parameter"

Pertinence_total_NC = as.data.frame(rbind(Pertinece_1_NC,Pertinece_proc_NC))

write.csv(Pertinence_total_NC,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/ranking_parameters_NC_new.csv", row.names = FALSE)

# Habitat Provision
Relevance_HP = Relevance_no_combinations %>% filter(Function == "habitat provision")
Pertinece_1_HP = merge(Relevance_HP, Frequencies, by = "Parameter")
Pertinece_1_HP = Pertinece_1_HP %>% mutate(total_pertinence = total_relevance*Frequency_Total,
                                           Agricultural_pertinence = relevance_agriculture*Frequency_Agr,
                                           Forest_pertinence = relevance_forest*Frequency_For,
                                           Urban_pertinence = relevance_urban*Frequency_Urb,
                                           ATC_pertinence = relevance_ATC*Frequency_Total,
                                           ATN_pertinence = relevance_ATN*Frequency_Total,
                                           BOR_pertinence = relevance_BOR*Frequency_Total,
                                           CON_pertinence = relevance_CON*Frequency_Total,
                                           LUS_pertinence = relevance_LUS*Frequency_Total,
                                           MDM_pertinence = relevance_MDM*Frequency_Total,
                                           MDN_pertinence = relevance_MDN*Frequency_Total,
                                           MDS_pertinence = relevance_MDS*Frequency_Total,
                                           NEM_pertinence = relevance_NEM*Frequency_Total,
                                           PAN_pertinence = relevance_PAN*Frequency_Total)
Pertinece_1_HP = Pertinece_1_HP %>% select(Parameter,type,total_pertinence,Agricultural_pertinence,
                                           Forest_pertinence,Urban_pertinence,ATC_pertinence,
                                           ATN_pertinence,BOR_pertinence,CON_pertinence,
                                           LUS_pertinence,MDN_pertinence,MDM_pertinence,
                                           MDS_pertinence,NEM_pertinence,PAN_pertinence)
# Add processes
Frequencies_processes_HP = Frequencies_processes %>% filter(Function == "nutrient cycling")
Frequencies_processes_HP = Frequencies_processes_HP %>% mutate(total_pertinence = total_relevance*Total_frequency,
                                                               Agricultural_pertinence = relevance_agriculture*Total_frequency,
                                                               Forest_pertinence = relevance_forest*Total_frequency,
                                                               Urban_pertinence = relevance_urban*Total_frequency,
                                                               ATC_pertinence = relevance_ATC*Total_frequency,
                                                               ATN_pertinence = relevance_ATN*Total_frequency,
                                                               BOR_pertinence = relevance_BOR*Total_frequency,
                                                               CON_pertinence = relevance_CON*Total_frequency,
                                                               LUS_pertinence = relevance_LUS*Total_frequency,
                                                               MDM_pertinence = relevance_MDM*Total_frequency,
                                                               MDN_pertinence = relevance_MDN*Total_frequency,
                                                               MDS_pertinence = relevance_MDS*Total_frequency,
                                                               NEM_pertinence = relevance_NEM*Total_frequency,
                                                               PAN_pertinence = relevance_PAN*Total_frequency)
Frequencies_processes_HP = Frequencies_processes_HP %>% mutate(type = rep("process", nrow(Frequencies_processes_HP)))

Pertinece_proc_HP = Frequencies_processes_HP %>% select(name,type,total_pertinence,Agricultural_pertinence,
                                                        Forest_pertinence,Urban_pertinence,ATC_pertinence,
                                                        ATN_pertinence,BOR_pertinence,CON_pertinence,
                                                        LUS_pertinence,MDN_pertinence,MDM_pertinence,
                                                        MDS_pertinence,NEM_pertinence,PAN_pertinence)
colnames(Pertinece_proc_HP)[1] = "Parameter"

Pertinence_total_HP = as.data.frame(rbind(Pertinece_1_HP,Pertinece_proc_HP))

write.csv(Pertinence_total_HP,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/ranking_parameters_HP_new.csv", row.names = FALSE)

# Water Regulation and Filtration
Relevance_WR = Relevance_no_combinations %>% filter(Function == "water regulation and filtration")
Pertinece_1_WR = merge(Relevance_WR, Frequencies, by = "Parameter")
Pertinece_1_WR = Pertinece_1_WR %>% mutate(total_pertinence = total_relevance*Frequency_Total,
                                           Agricultural_pertinence = relevance_agriculture*Frequency_Agr,
                                           Forest_pertinence = relevance_forest*Frequency_For,
                                           Urban_pertinence = relevance_urban*Frequency_Urb,
                                           ATC_pertinence = relevance_ATC*Frequency_Total,
                                           ATN_pertinence = relevance_ATN*Frequency_Total,
                                           BOR_pertinence = relevance_BOR*Frequency_Total,
                                           CON_pertinence = relevance_CON*Frequency_Total,
                                           LUS_pertinence = relevance_LUS*Frequency_Total,
                                           MDM_pertinence = relevance_MDM*Frequency_Total,
                                           MDN_pertinence = relevance_MDN*Frequency_Total,
                                           MDS_pertinence = relevance_MDS*Frequency_Total,
                                           NEM_pertinence = relevance_NEM*Frequency_Total,
                                           PAN_pertinence = relevance_PAN*Frequency_Total)
Pertinece_1_WR = Pertinece_1_WR %>% select(Parameter,type,total_pertinence,Agricultural_pertinence,
                                           Forest_pertinence,Urban_pertinence,ATC_pertinence,
                                           ATN_pertinence,BOR_pertinence,CON_pertinence,
                                           LUS_pertinence,MDN_pertinence,MDM_pertinence,
                                           MDS_pertinence,NEM_pertinence,PAN_pertinence)
# Add processes
Frequencies_processes_WR = Frequencies_processes %>% filter(Function == "water regulation and filtration")
Frequencies_processes_WR = Frequencies_processes_WR %>% mutate(total_pertinence = total_relevance*Total_frequency,
                                                               Agricultural_pertinence = relevance_agriculture*Total_frequency,
                                                               Forest_pertinence = relevance_forest*Total_frequency,
                                                               Urban_pertinence = relevance_urban*Total_frequency,
                                                               ATC_pertinence = relevance_ATC*Total_frequency,
                                                               ATN_pertinence = relevance_ATN*Total_frequency,
                                                               BOR_pertinence = relevance_BOR*Total_frequency,
                                                               CON_pertinence = relevance_CON*Total_frequency,
                                                               LUS_pertinence = relevance_LUS*Total_frequency,
                                                               MDM_pertinence = relevance_MDM*Total_frequency,
                                                               MDN_pertinence = relevance_MDN*Total_frequency,
                                                               MDS_pertinence = relevance_MDS*Total_frequency,
                                                               NEM_pertinence = relevance_NEM*Total_frequency,
                                                               PAN_pertinence = relevance_PAN*Total_frequency)
Frequencies_processes_WR = Frequencies_processes_WR %>% mutate(type = rep("process", nrow(Frequencies_processes_WR)))

Pertinece_proc_WR = Frequencies_processes_WR %>% select(name,type,total_pertinence,Agricultural_pertinence,
                                                        Forest_pertinence,Urban_pertinence,ATC_pertinence,
                                                        ATN_pertinence,BOR_pertinence,CON_pertinence,
                                                        LUS_pertinence,MDN_pertinence,MDM_pertinence,
                                                        MDS_pertinence,NEM_pertinence,PAN_pertinence)
colnames(Pertinece_proc_WR)[1] = "Parameter"

Pertinence_total_WR = as.data.frame(rbind(Pertinece_1_WR,Pertinece_proc_WR))

write.csv(Pertinence_total_WR,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/ranking_parameters_WR_new.csv", row.names = FALSE)

# Selection of parameters ----

ranking_parameters_CR = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/ranking_parameters_CR_new.csv")
ranking_parameters_HP = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/ranking_parameters_HP_new.csv")
ranking_parameters_NC = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/ranking_parameters_NC_new.csv")
ranking_parameters_WR = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/ranking_parameters_WR_new.csv")
Total = as.data.frame(rbind(ranking_parameters_CR,ranking_parameters_HP,ranking_parameters_NC,
                            ranking_parameters_WR))

key_WR = paste(ranking_parameters_WR$Parameter)
key_HP = paste(ranking_parameters_HP$Parameter)
key_NC = paste(ranking_parameters_NC$Parameter)
key_CR = paste(ranking_parameters_CR$Parameter)

all_keys = union(union(key_WR, key_HP), union(key_NC, key_CR)) 

idx_WR = match(all_keys, key_WR)
idx_HP = match(all_keys, key_HP)
idx_NC = match(all_keys, key_NC)
idx_CR = match(all_keys, key_CR)

# TOTAL PERTINENCE PLOT ----

Mixed_pertinence = data.frame(all_keys,
  WR = ranking_parameters_WR$total_pertinence[idx_WR],
  HP = ranking_parameters_HP$total_pertinence[idx_HP],
  NC = ranking_parameters_NC$total_pertinence[idx_NC],
  CR = ranking_parameters_CR$total_pertinence[idx_CR]
)

# Replace na values with 0 
Mixed_pertinence = Mixed_pertinence %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))

sky1 = psel(Mixed_pertinence, high(WR) * high(HP) * high(NC) * high(CR))
colnames(sky1)[1] = "Parameter"

# Add type of indicators
idx = match(sky1$Parameter,Total$Parameter)
sky1$Type = Total$type[idx]

write.csv(sky1,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/optimal_parameters_all_variables_general_sieve_updated.csv", row.names = FALSE)

# Parallel coordinate plot

sky1 %>%
  GGally::ggparcoord(
    columns = 2:5,          # WR, HP, NC, CR
    groupColumn = 1,        # Parameter
    scale = "uniminmax"     # normalizes each axis to [0,1]
  ) +
  theme_minimal() +
  labs(title = "Profiles of the 6 Pareto-Optimal Parameters",
       x = "Criteria",
       y = "Normalized Score")

# AGRICULTURAL PERTINENCE PLOT ----

Mixed_pertinence_agriculture = data.frame(all_keys,
                                          WR = ranking_parameters_WR$Agricultural_pertinence[idx_WR],
                                          HP = ranking_parameters_HP$Agricultural_pertinence[idx_HP],
                                          NC = ranking_parameters_NC$Agricultural_pertinence[idx_NC],
                                          CR = ranking_parameters_CR$Agricultural_pertinence[idx_CR]
)

# Replace na values with 0 
Mixed_pertinence_agriculture = Mixed_pertinence_agriculture %>%
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))

sky1 = psel(Mixed_pertinence_agriculture, high(WR) * high(HP) * high(NC) * high(CR))
colnames(sky1)[1] = "Parameter"

# Add type of indicators
idx = match(sky1$Parameter,Total$Parameter)
sky1$Type = Total$type[idx]

write.csv(sky1,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/optimal_parameters_all_variables_general_sieve_AGRICULTURE_updated.csv", row.names = FALSE)

# PERTINENCE LEVEL TWO ----

# Call data
ranking_parameters_CR = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/ranking_parameters_CR_new.csv")
ranking_parameters_WR = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/ranking_parameters_WR_new.csv")
ranking_parameters_NC = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/ranking_parameters_NC_new.csv")
ranking_parameters_HP = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/ranking_parameters_HP_new.csv")
Final_technical_aspects = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/Final_technical_aspects_updated.csv")
#Final_technical_aspects = Final_technical_aspects %>%
#  filter(!parameter_type %in% c("process") & !is.na(parameter_type))

# Refine the Final technical aspects dataset

Final_technical_aspects = Final_technical_aspects %>% mutate(sample_collection = rowSums(Final_technical_aspects[,49:52])/4,
                                                             storage.amount.archivability = rowSums(Final_technical_aspects[,53:55])/3,
                                                             data.storage = rowSums(Final_technical_aspects[,62:65])/4,
                                                             spatial.resolution = rowSums(Final_technical_aspects[,67:68])/2,
                                                             spectral.resolution = rowSums(Final_technical_aspects[,69:70])/2)
Final_technical_aspects = Final_technical_aspects %>% select("parameter_type","parameter_name",
                                                             "Indicator","Method_Type","Method_Name",
                                                             "Reference","agriculture","urban",
                                                             "forest","scale","sample_collection",
                                                             "storage.amount.archivability",
                                                             "duration","reference.material",
                                                             "throughput","lab.cost","lab.use",
                                                             "internet.conection","data.storage",
                                                             "temporal.resolution","spatial.resolution",
                                                             "spectral.resolution","interpretation",
                                                             "data.license","software","reproducibility",
                                                             "deployment","Functional.information")
Final_technical_aspects$parameter_name[3:13]  = "acari"
Final_technical_aspects$parameter_name[62:65] = "ants"
Final_technical_aspects$parameter_name[76:78] = "bacteria"
Final_technical_aspects$parameter_name[93:96] = "calcium : magnesium ratio"
Final_technical_aspects$parameter_name[105:118] = "cation exchange capacity"
Final_technical_aspects$parameter_name[141:146] = "collembola"
# Final_technical_aspects$parameter_name[212] = "fungi"
Final_technical_aspects$parameter_name[439:447] = "nematodes"
Final_technical_aspects$parameter_name[491:526] = "nutrient availability"
Final_technical_aspects$parameter_name[604:610] = "nutrient availability"
Final_technical_aspects$parameter_name[659:686] = "pollutant concentration"
Final_technical_aspects$parameter_name[734:735] = "protozoa"
Final_technical_aspects = Final_technical_aspects %>%
  mutate(across(where(is.character), tolower))
colnames(Final_technical_aspects)[2] = "Parameter"

# Merge the rankings with the measurements 

# Climate Regulation
common_params = semi_join(ranking_parameters_CR, Final_technical_aspects, by = "Parameter")
technical_criteria_CR = Final_technical_aspects %>%
  semi_join(ranking_parameters_CR, by = "Parameter") %>%   # keep only A
  left_join(common_params, by = "Parameter")     # attach pertinence
technical_criteria_CR = technical_criteria_CR %>%
  mutate(across(11:27, ~ .x / max(.x, na.rm = TRUE)))
technical_criteria_CR = technical_criteria_CR %>% mutate(functional_pertinence = Functional.information*total_pertinence/3)

write.csv(technical_criteria_CR,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/technical_criteria_CR_new.csv", row.names = FALSE)

# Water Regulation
common_params = semi_join(ranking_parameters_WR, Final_technical_aspects, by = "Parameter")
technical_criteria_WR = Final_technical_aspects %>%
  semi_join(ranking_parameters_WR, by = "Parameter") %>%   # keep only A
  left_join(common_params, by = "Parameter")     # attach pertinence
technical_criteria_WR = technical_criteria_WR %>%
  mutate(across(11:27, ~ .x / max(.x, na.rm = TRUE)))
technical_criteria_WR = technical_criteria_WR %>% mutate(functional_pertinence = Functional.information*total_pertinence/3)

write.csv(technical_criteria_WR,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/technical_criteria_WR_new.csv", row.names = FALSE)

# Habitat Provision
common_params = semi_join(ranking_parameters_HP, Final_technical_aspects, by = "Parameter")
technical_criteria_HP = Final_technical_aspects %>%
  semi_join(ranking_parameters_HP, by = "Parameter") %>%   # keep only A
  left_join(common_params, by = "Parameter")     # attach pertinence
technical_criteria_HP = technical_criteria_HP %>%
  mutate(across(11:27, ~ .x / max(.x, na.rm = TRUE)))
technical_criteria_HP = technical_criteria_HP %>% mutate(functional_pertinence = Functional.information*total_pertinence/3)

write.csv(technical_criteria_HP,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/technical_criteria_HP_new.csv", row.names = FALSE)

# Nutrient Cycling
common_params = semi_join(ranking_parameters_NC, Final_technical_aspects, by = "Parameter")
technical_criteria_NC = Final_technical_aspects %>%
  semi_join(ranking_parameters_NC, by = "Parameter") %>%   # keep only A
  left_join(common_params, by = "Parameter")     # attach pertinence
technical_criteria_NC = technical_criteria_NC %>%
  mutate(across(11:27, ~ .x / max(.x, na.rm = TRUE)))
technical_criteria_NC = technical_criteria_NC %>% mutate(functional_pertinence = Functional.information*total_pertinence/3)

write.csv(technical_criteria_NC,"C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/technical_criteria_NC_updated.csv", row.names = FALSE)

