# Calling packages----
library(readr)
library(tidyverse)
library(plyr)
library(googledrive)
library(googlesheets4)
library(readxl)
library(xlsx)

# Data Preparation----

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


