# Calling packages----
library(readr)
library(tidyverse)
library(plyr)
library(googledrive)
library(googlesheets4)
library(readxl)
library(purrr)

# Data Preparation for method expansion----

# Counting Factors for Climate Regulation in Agriculture
agri_bio    = count(Climate_Regulation_agriculture,'actors')
agri_bio$parameter_tye = 'biological'
colnames(agri_bio)     = c("Parameter","CR.Frequency","Parameter.Type")
agri_chem = count(Climate_Regulation_agriculture,'`chemical factors`')
agri_chem$parameter_tye = 'chemical'
colnames(agri_chem)     = c("Parameter","CR.Frequency","Parameter.Type")
agri_phy  = count(Climate_Regulation_agriculture,'`physical factors`')
agri_phy$parameter_tye = 'physical'
colnames(agri_phy)     = c("Parameter","CR.Frequency","Parameter.Type")

# Add Columns and reorganizing
AGRI_CR = as.data.frame(rbind(agri_bio,agri_chem,agri_phy))
AGRI_CR = AGRI_CR %>% relocate(CR.Frequency, .after=Parameter.Type)
AGRI_CR$Related.Subprocess  = ""
AGRI_CR$Related.Process     = ""
AGRI_CR$Related.Subfunction = ""
AGRI_CR$Related.Function    = ""
AGRI_CR$Aplicability.Agri   = 1
AGRI_CR = AGRI_CR %>% relocate(CR.Frequency, .after=Related.Function)
                               
# Save partial output in Google Drive

sheet_write(AGRI_CR,
            ss = "https://docs.google.com/spreadsheets/d/126qDjQEnFkigcg-71qX0Ig9JXhIjpzlx8-Vs8O8vSzI/edit?gid=0#gid=0",
            sheet = "agri_CR")

# Unique parameters

# Chemical
chemicals = read_excel("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Albert_Jan_Internship/working_document.xlsx", sheet = "chemical")
chemical  = as.data.frame(unique(chemicals$parameter_name))
write.csv(chemical,'C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Albert_Jan_Internship/support_data/chemical.csv')

# Physical
physicals = read_excel("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Albert_Jan_Internship/working_document.xlsx", sheet = "physical")
physical  = as.data.frame(unique(physicals$parameter_name))
write.csv(physical,'C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Albert_Jan_Internship/support_data/physical.csv')

# Environment
enviroms = read_excel("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Albert_Jan_Internship/working_document.xlsx", sheet = "environmental")
envirom  = as.data.frame(unique(enviroms$parameter_name))
write.csv(envirom,'C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Albert_Jan_Internship/support_data/environmental.csv')

# Data Preparation for final dataset----

# AGRICULTURE----

# Open file----
Parameter_CR = read_sheet("https://docs.google.com/spreadsheets/d/174HNO_15T42wEQnO2VTnEnhxUBxn_r5_US3IS8h_fis/edit?gid=857079251#gid=857079251",
                          sheet = "CR")
Parameter_WR = read_sheet("https://docs.google.com/spreadsheets/d/174HNO_15T42wEQnO2VTnEnhxUBxn_r5_US3IS8h_fis/edit?gid=857079251#gid=857079251",
                          sheet = "WR")
Parameter_NR = read_sheet("https://docs.google.com/spreadsheets/d/174HNO_15T42wEQnO2VTnEnhxUBxn_r5_US3IS8h_fis/edit?gid=857079251#gid=857079251",
                          sheet = "NR")
Parameter_HP = read_sheet("https://docs.google.com/spreadsheets/d/174HNO_15T42wEQnO2VTnEnhxUBxn_r5_US3IS8h_fis/edit?gid=857079251#gid=857079251",
                          sheet = "HP")

# Environmental Parameters----
# Create individual datasets for each soil function
Parameter_CR.enviromental   = Parameter_CR %>% filter(parameter_type=="environmental")
enviromental_cont           = count(Parameter_CR.enviromental,'parameter')
colnames(enviromental_cont) = c("parameter","CR.Frequency")
Parameter_CR.enviromental.f = merge(Parameter_CR.enviromental,
                                    enviromental_cont,by="parameter")

Parameter_WR.enviromental   = Parameter_WR %>% filter(parameter_type=="environmental")
enviromental_cont           = count(Parameter_WR.enviromental,'parameter')
colnames(enviromental_cont) = c("parameter","WR.Frequency")
Parameter_WR.enviromental.f = merge(Parameter_WR.enviromental,
                                    enviromental_cont,by="parameter")

Parameter_NR.enviromental   = Parameter_NR %>% filter(parameter_type=="environmental")
enviromental_cont           = count(Parameter_NR.enviromental,'parameter')
colnames(enviromental_cont) = c("parameter","NR.Frequency")
Parameter_NR.enviromental.f = merge(Parameter_NR.enviromental,
                                    enviromental_cont,by="parameter")

Parameter_HP.enviromental   = Parameter_HP %>% filter(parameter_type=="environmental")
enviromental_cont           = count(Parameter_HP.enviromental,'parameter')
colnames(enviromental_cont) = c("parameter","HP.Frequency")
Parameter_HP.enviromental.f = merge(Parameter_HP.enviromental,
                                    enviromental_cont,by="parameter")

# Merge datasets for each type of parameter

df_list = list(Parameter_CR.enviromental.f,Parameter_WR.enviromental.f,
               Parameter_NR.enviromental.f,Parameter_HP.enviromental.f)

Parameter_enviromental.final = purrr::reduce(.x = df_list, merge, 
                                             by = c('parameter','parameter_type',
                                                    'land_use','pedoclimatic_zone',
                                                    'process','indicator_type',
                                                    'indicator','comments'), 
                                             all = T)
# Reorganize values
Parameter_enviromental.final = Parameter_enviromental.final %>%
  mutate(land_use = case_when(land_use == "all" ~ "agriculture",
                              land_use == "agriculture" ~ "agriculture")) 

Parameter_enviromental.final = Parameter_enviromental.final %>% 
  mutate(CR.Frequency = replace_na(CR.Frequency, 0))
Parameter_enviromental.final = Parameter_enviromental.final %>% 
  mutate(WR.Frequency = replace_na(WR.Frequency, 0))
Parameter_enviromental.final = Parameter_enviromental.final %>% 
  mutate(NR.Frequency = replace_na(NR.Frequency, 0))
Parameter_enviromental.final = Parameter_enviromental.final %>% 
  mutate(HP.Frequency = replace_na(HP.Frequency, 0))

# Save file
sheet_write(Parameter_enviromental.final,
            ss = "https://docs.google.com/spreadsheets/d/1GuF4U_TA8DfVch40dUyEx63OeTUmjGhhguVziTLTweQ/edit?gid=0#gid=0",
            sheet = "Environmental")

# Chemical Parameters----
Parameter_CR.chemical   = Parameter_CR %>% filter(parameter_type=="chemical")
chemical_cont           = count(Parameter_CR.chemical,'parameter')
colnames(chemical_cont) = c("parameter","CR.Frequency")
Parameter_CR.chemical.f = merge(Parameter_CR.chemical,
                                chemical_cont,by="parameter")

Parameter_WR.chemical   = Parameter_WR %>% filter(parameter_type=="chemical")
chemical_cont           = count(Parameter_WR.chemical,'parameter')
colnames(chemical_cont) = c("parameter","WR.Frequency")
Parameter_WR.chemical.f = merge(Parameter_WR.chemical,
                                chemical_cont,by="parameter")

Parameter_NR.chemical   = Parameter_NR %>% filter(parameter_type=="chemical")
chemical_cont           = count(Parameter_NR.chemical,'parameter')
colnames(chemical_cont) = c("parameter","NR.Frequency")
Parameter_NR.chemical.f = merge(Parameter_NR.chemical,
                                chemical_cont,by="parameter")

Parameter_HP.chemical   = Parameter_HP %>% filter(parameter_type=="chemical")
chemical_cont           = count(Parameter_HP.chemical,'parameter')
colnames(chemical_cont) = c("parameter","HP.Frequency")
Parameter_HP.chemical.f = merge(Parameter_HP.chemical,
                                chemical_cont,by="parameter")

# Merge datasets for each type of parameter

df_list = list(Parameter_CR.chemical.f,Parameter_WR.chemical.f,
               Parameter_NR.chemical.f,Parameter_HP.chemical.f)

Parameter_chemical.final = purrr::reduce(.x = df_list, merge, 
                                             by = c('parameter','parameter_type',
                                                    'land_use','pedoclimatic_zone',
                                                    'process','indicator_type',
                                                    'indicator','comments'), 
                                             all = T)
# Reorganize values
Parameter_chemical.final = Parameter_chemical.final %>%
  mutate(land_use = case_when(land_use == "all" ~ "agriculture",
                              land_use == "agriculture" ~ "agriculture")) 

Parameter_chemical.final = Parameter_chemical.final %>% 
  mutate(CR.Frequency = replace_na(CR.Frequency, 0))
Parameter_chemical.final = Parameter_chemical.final %>% 
  mutate(WR.Frequency = replace_na(WR.Frequency, 0))
Parameter_chemical.final = Parameter_chemical.final %>% 
  mutate(NR.Frequency = replace_na(NR.Frequency, 0))
Parameter_chemical.final = Parameter_chemical.final %>% 
  mutate(HP.Frequency = replace_na(HP.Frequency, 0))

# Save file
sheet_write(Parameter_chemical.final,
            ss = "https://docs.google.com/spreadsheets/d/1GuF4U_TA8DfVch40dUyEx63OeTUmjGhhguVziTLTweQ/edit?gid=0#gid=0",
            sheet = "Chemical")

# Biological Parameters----
Parameter_CR.biological   = Parameter_CR %>% filter(parameter_type=="biological")
biological_cont           = count(Parameter_CR.biological,'parameter')
colnames(biological_cont) = c("parameter","CR.Frequency")
Parameter_CR.biological.f = merge(Parameter_CR.biological,
                                  biological_cont,by="parameter")

Parameter_WR.biological   = Parameter_WR %>% filter(parameter_type=="biological")
biological_cont           = count(Parameter_WR.biological,'parameter')
colnames(biological_cont) = c("parameter","WR.Frequency")
Parameter_WR.biological.f = merge(Parameter_WR.biological,
                                  biological_cont,by="parameter")

Parameter_NR.biological   = Parameter_NR %>% filter(parameter_type=="biological")
biological_cont           = count(Parameter_NR.biological,'parameter')
colnames(biological_cont) = c("parameter","NR.Frequency")
Parameter_NR.biological.f = merge(Parameter_NR.biological,
                                  biological_cont,by="parameter")

Parameter_HP.biological   = Parameter_HP %>% filter(parameter_type=="biological")
biological_cont           = count(Parameter_HP.biological,'parameter')
colnames(biological_cont) = c("parameter","HP.Frequency")
Parameter_HP.biological.f = merge(Parameter_HP.biological,
                                  biological_cont,by="parameter")

# Merge datasets for each type of parameter

df_list = list(Parameter_CR.biological.f,Parameter_WR.biological.f,
               Parameter_NR.biological.f,Parameter_HP.biological.f)

Parameter_biological.final = purrr::reduce(.x = df_list, merge, 
                                         by = c('parameter','parameter_type',
                                                'land_use','pedoclimatic_zone',
                                                'process','indicator_type',
                                                'indicator','comments'), 
                                         all = T)
# Reorganize values
Parameter_biological.final = Parameter_biological.final %>%
  mutate(land_use = case_when(land_use == "all" ~ "agriculture",
                              land_use == "agriculture" ~ "agriculture")) 

Parameter_biological.final = Parameter_biological.final %>% 
  mutate(CR.Frequency = replace_na(CR.Frequency, 0))
Parameter_biological.final = Parameter_biological.final %>% 
  mutate(WR.Frequency = replace_na(WR.Frequency, 0))
Parameter_biological.final = Parameter_biological.final %>% 
  mutate(NR.Frequency = replace_na(NR.Frequency, 0))
Parameter_biological.final = Parameter_biological.final %>% 
  mutate(HP.Frequency = replace_na(HP.Frequency, 0))

# Save file
sheet_write(Parameter_biological.final,
            ss = "https://docs.google.com/spreadsheets/d/1GuF4U_TA8DfVch40dUyEx63OeTUmjGhhguVziTLTweQ/edit?gid=0#gid=0",
            sheet = "Biological")

# Physical Parameters----
Parameter_CR.physical     = Parameter_CR %>% filter(parameter_type=="physical")
physical_cont             = count(Parameter_CR.physical,'parameter')
colnames(physical_cont)   = c("parameter","CR.Frequency")
Parameter_CR.physical.f   = merge(Parameter_CR.physical,
                                  physical_cont,by="parameter")

Parameter_WR.physical     = Parameter_WR %>% filter(parameter_type=="physical")
physical_cont             = count(Parameter_WR.physical,'parameter')
colnames(physical_cont)   = c("parameter","WR.Frequency")
Parameter_WR.physical.f   = merge(Parameter_WR.physical,
                                  physical_cont,by="parameter")

Parameter_NR.physical     = Parameter_NR %>% filter(parameter_type=="physical")
physical_cont             = count(Parameter_NR.physical,'parameter')
colnames(physical_cont)   = c("parameter","NR.Frequency")
Parameter_NR.physical.f   = merge(Parameter_NR.physical,
                                  physical_cont,by="parameter")

Parameter_HP.physical     = Parameter_HP %>% filter(parameter_type=="physical")
physical_cont             = count(Parameter_HP.physical,'parameter')
colnames(physical_cont)   = c("parameter","HP.Frequency")
Parameter_HP.physical.f   = merge(Parameter_HP.physical,
                                  physical_cont,by="parameter")

# Merge datasets for each type of parameter

df_list = list(Parameter_CR.physical.f,Parameter_WR.physical.f,
               Parameter_NR.physical.f,Parameter_HP.physical.f)

Parameter_physical.final = purrr::reduce(.x = df_list, merge, 
                                           by = c('parameter','parameter_type',
                                                  'land_use','pedoclimatic_zone',
                                                  'process','indicator_type',
                                                  'indicator','comments'), 
                                           all = T)
# Reorganize values
Parameter_physical.final = Parameter_physical.final %>%
  mutate(land_use = case_when(land_use == "all" ~ "agriculture",
                              land_use == "agriculture" ~ "agriculture")) 

Parameter_physical.final = Parameter_physical.final %>% 
  mutate(CR.Frequency = replace_na(CR.Frequency, 0))
Parameter_physical.final = Parameter_physical.final %>% 
  mutate(WR.Frequency = replace_na(WR.Frequency, 0))
Parameter_physical.final = Parameter_physical.final %>% 
  mutate(NR.Frequency = replace_na(NR.Frequency, 0))
Parameter_physical.final = Parameter_physical.final %>% 
  mutate(HP.Frequency = replace_na(HP.Frequency, 0))

# Save file
sheet_write(Parameter_physical.final,
            ss = "https://docs.google.com/spreadsheets/d/1GuF4U_TA8DfVch40dUyEx63OeTUmjGhhguVziTLTweQ/edit?gid=0#gid=0",
            sheet = "Physical")

# CARMEN DATASET ----

# Call data ----
scoring_criteria = read_excel("data/Method list assessment file.xlsx",
                              sheet = "Scoring Criteria", range = "B22:M53")
method_list      = read_excel("data/Method list assessment file.xlsx",
                              sheet = "Method list", range = "A2:AM1019")

# Test for Applicability & Discrimination ----

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

# In field Collection ----

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

# Sample storage, amount and archivability ----

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

# Duration ----

# How much time does it take to pre-process the sample? == duration ** (additional level which is not here)
final_list       = final_list %>% mutate(duration = case_when(`How long does it take to process the sample?` == "In adition, extractions also needed" ~ 1,
                                                              `How long does it take to process the sample?` == "soil drying and/or sieving additionally needed" ~ 2,
                                                              `How long does it take to process the sample?` == "only coarse debris removal or homogeneization needed" ~ 3,
                                                              `How long does it take to process the sample?` == "In adition, extractions and/or an incubation period also needed" ~ 4,
                                                              `How long does it take to process the sample?` == "No preprocessing needed" ~ 0,
                                                              `How long does it take to process the sample?` == "Archivable as fixated or extracted sample or as frozen soil" ~ NA,
                                                              `How long does it take to process the sample?` == NA ~ NA))

# Reference material ----

# Is the method amenable to the prescription and provision of such material? == reference.material ** (other way around, level missing)
final_list       = final_list %>% mutate(reference.material = case_when(`Do we need to include or develop reference material, or ground truth the measurements?` == "None" ~ 0,
                                                                        `Do we need to include or develop reference material, or ground truth the measurements?` == "Yes, an internal reference" ~ 2,
                                                                        `Do we need to include or develop reference material, or ground truth the measurements?` == "Yes, an international / standard reference" ~ 3,
                                                                        `Do we need to include or develop reference material, or ground truth the measurements?` == "Potential but not often applied in practice" ~ 1,
                                                                        `Do we need to include or develop reference material, or ground truth the measurements?` == NA ~ NA))

# Throughput ----

# How many samples can be processed with optimised laboratory systems and dedicated staff? == throughput 
final_list       = final_list %>% mutate(throughput = case_when(`How many samples can be processed per week?` == "< 20 /week" ~ 1,
                                                                `How many samples can be processed per week?` == "> 100/week" ~ 4,
                                                                `How many samples can be processed per week?` == "20-50/week" ~ 2,
                                                                `How many samples can be processed per week?` == "50-100/week" ~ 3,
                                                                `How many samples can be processed per week?` == NA ~ NA))

# Lab analysis cost per sample ----
# If sent to an external lab, how much would it cost to run the sample? == lab.cost 
final_list       = final_list %>% mutate(lab.cost = case_when(`What is the cost per sample/image?` == "less than 10 euros/sample" ~ 3,
                                                              `What is the cost per sample/image?` == "10-50 euros per sample" ~ 2,
                                                              `What is the cost per sample/image?` == "more than 50 euros a sample" ~ 1,
                                                              `What is the cost per sample/image?` == "Free" ~ 4,
                                                              `What is the cost per sample/image?` == NA ~ NA))

# Ease of use in laboratory ----
# What is the level of skill required to perform this method in the laboratory? == lab.use 
final_list       = final_list %>% mutate(lab.use = case_when(`Do we need a specialized lab for the method?` == "Specialised" ~ 1,
                                                             `Do we need a specialized lab for the method?` == "Moderate" ~ 2,
                                                             `Do we need a specialized lab for the method?` == "Low" ~ 3,
                                                             `Do we need a specialized lab for the method?` == NA ~ NA))

# Infrastructure ----
# Does the implementation of the method require a stable internet connection? == internet.conection
final_list       = final_list %>% mutate(internet.conection = case_when(`Does the implementation of the method require a stable internet connection?` == "no" ~ 1,
                                                                        `Does the implementation of the method require a stable internet connection?` == "No" ~ 1,
                                                                        `Does the implementation of the method require a stable internet connection?` == "yes" ~ 0,
                                                                        `Does the implementation of the method require a stable internet connection?` == NA ~ NA))

# Data storage and processing ----
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

# Temporal resolution ----
# What is the temporal resolution of the data product?  == temporal.resolution
final_list       = final_list %>% mutate(temporal.resolution = case_when(`What is the temporal resolution of the data product? ` == "Annual-seasonal" ~ 1,
                                                                         `What is the temporal resolution of the data product? ` == "Daily-hourly" ~ 4,
                                                                         `What is the temporal resolution of the data product? ` == "Long-term average" ~ 0,
                                                                         `What is the temporal resolution of the data product? ` == "Weekly" ~ 3,
                                                                         `What is the temporal resolution of the data product? ` == "Monthly" ~ 2,
                                                                         `What is the temporal resolution of the data product? ` == NA ~ NA))
# Spatial resolution ----
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

# Spectral resolution	----
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

# Interpretation	----
# What is the level of skill required to process the raw data ?  == interpretation
final_list       = final_list %>% mutate(interpretation = case_when(`Do we need a high level of expertise to interpret and process the data?` == "Straightforward" ~ 3,
                                                                    `Do we need a high level of expertise to interpret and process the data?` == "Moderate" ~ 2,
                                                                    `Do we need a high level of expertise to interpret and process the data?` == "Specialised" ~ 1,
                                                                    `Do we need a high level of expertise to interpret and process the data?` == NA ~ NA))

# Data license	----
# Can the data be publicly shared/can the data product be freely shared with other users?  == data.license
final_list       = final_list %>% mutate(data.license = case_when(`Can the data be publicly shared/can the data product be freely shared with other users? ` == "Yes, the data can be fully shared" ~ 2,
                                                                  `Can the data be publicly shared/can the data product be freely shared with other users? ` == "no" ~ 0,
                                                                  `Can the data be publicly shared/can the data product be freely shared with other users? ` == "Only processed data products can be shared" ~ 1,
                                                                  `Can the data be publicly shared/can the data product be freely shared with other users? ` == NA ~ NA))

# Software use	----
# Do we need a specialized software for the method?  == software
final_list       = final_list %>% mutate(software = case_when(`Do we need a specialized software for the method? ` == "no" ~ 2,
                                                              `Do we need a specialized software for the method? ` == "the software is specialised, but freely available" ~ 1,
                                                              `Do we need a specialized software for the method? ` == "the software is specialised and requires payment" ~ 0,
                                                              `Do we need a specialized software for the method? ` == NA ~ NA))

# Reproducibility	----
# How reproducible is this method? == reproducibility
final_list       = final_list %>% mutate(reproducibility = case_when(`How reproducible is this method?` == "Moderate" ~ 2,
                                                                     `How reproducible is this method?` == "Low" ~ 1,
                                                                     `How reproducible is this method?` == "High" ~ 3,
                                                                     `How reproducible is this method?` == NA ~ NA))

# Deployment	----
# Is this method well-established or still under development? == deployment
final_list       = final_list %>% mutate(deployment = case_when(`Is this method well-established or still under development?` == "Not ready, years of development needed" ~ 0,
                                                                `Is this method well-established or still under development?` == "Developed for experimental use" ~ 1,
                                                                `Is this method well-established or still under development?` == "Fully developed for routine use" ~ 2,
                                                                `Is this method well-established or still under development?` == NA ~ NA))
