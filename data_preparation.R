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

