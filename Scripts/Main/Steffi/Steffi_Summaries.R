


################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: AGGREGATE and PROCESS DATA (MAIN)                                 #
#------------------------------------------------------------------------------#
################################################################################
#source("./Scripts/SETUP.R")
options(dplyr.summarise.inform = FALSE) # disable massage of dplyr summarise

################################################################################
#
#          Across all primary subgroups
#
################################################################################

tp_begin <- 2005
tp_end <- 2022

path <- "./Reporting/Publication/SteffiOutput"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Postal Code count
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Should only contain three digits!
df_Output_PostalCodeCounts_Main_Filtered <- filter(df_Output_PostalCodeCounts_Main, (FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end))

df_Plot_PostalCodeCounts_Main_Filtered <- df_Output_PostalCodeCounts_Main_Filtered  %>% 
  group_by(PatientSubgroup, PrimaryPostalCode, FirstRelevantAdmissionYear) %>%
  summarise(
    N = sum(Count),
    NSite = n_distinct(Site)) %>%
  rename(Year = FirstRelevantAdmissionYear)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sample size
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_SampleSize_Main_Filtered <- filter(df_Output_SampleSize_Main, (FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end))


df_Plot_SampleSize_Main_Filtered <- df_Output_SampleSize_Main_Filtered %>% 
  group_by(PatientSubgroup, Strata) %>%
  summarise(
    N = sum(N),
    NSite = n_distinct(Site)) 

df_table_SampleSize <- data.frame(
  Period = c(as.character(df_Plot_SampleSize_Main_Filtered[1,1]), as.character(df_Plot_SampleSize_Main_Filtered[4,1]), as.character(df_Plot_SampleSize_Main_Filtered[8,1])),
  Sites = c(as.numeric(df_Plot_SampleSize_Main_Filtered[1,4]), as.numeric(df_Plot_SampleSize_Main_Filtered[4,4]), as.numeric(df_Plot_SampleSize_Main_Filtered[8,4])),
   HIVCancer = c(as.numeric(df_Plot_SampleSize_Main_Filtered[2,3]), as.numeric(df_Plot_SampleSize_Main_Filtered[5,3]), as.numeric(df_Plot_SampleSize_Main_Filtered[8,3])),
  Cancer = c(as.numeric(df_Plot_SampleSize_Main_Filtered[1,3]), as.numeric(df_Plot_SampleSize_Main_Filtered[4,3]), as.numeric(df_Plot_SampleSize_Main_Filtered[7,3])),
  HIV = c(as.numeric(df_Plot_SampleSize_Main_Filtered[3,3]), as.numeric(df_Plot_SampleSize_Main_Filtered[6,3]), as.numeric(df_Plot_SampleSize_Main_Filtered[9,3]))
  )
#   
# # write.csv2(df_table_SampleSize, file.path(path, "SampleSize.csv"), row.names = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sex distribution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_Sex_Main_Filtered <- filter(df_Output_Sex_Main, (FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end))

df_Plot_Sex_Main_Filtered <- df_Output_Sex_Main_Filtered %>% 
  group_by(Strata, PatientSubgroup) %>% mutate(Sum = sum(N)) %>% 
  filter(Sex == "F") %>% mutate(CountFemale = sum(N)) %>% 
  mutate(PercantageFemale = round((CountFemale/Sum)*100, 2)) %>% slice(1)
 

df_table_Sex <- data.frame(
  Period = c(as.character(df_Plot_Sex_Main_Filtered[1,6]), as.character(df_Plot_Sex_Main_Filtered[4,6]), as.character(df_Plot_Sex_Main_Filtered[8,6])),
  HIVCancer = c(paste(df_Plot_Sex_Main_Filtered[2,8], "(",df_Plot_Sex_Main_Filtered[2,9],")"), paste(df_Plot_Sex_Main_Filtered[5,8], "(",df_Plot_Sex_Main_Filtered[5,9],")"), paste(df_Plot_Sex_Main_Filtered[8,8], "(",df_Plot_Sex_Main_Filtered[8,9],")")),
  
  
   Cancer = c(paste(df_Plot_Sex_Main_Filtered[1,8], "(",df_Plot_Sex_Main_Filtered[1,9],")"),paste(df_Plot_Sex_Main_Filtered[4,8], "(",df_Plot_Sex_Main_Filtered[4,9],")"), paste(df_Plot_Sex_Main_Filtered[7,8], "(",df_Plot_Sex_Main_Filtered[7,9],")")),
  HIV = c(paste(df_Plot_Sex_Main_Filtered[3,8], "(",df_Plot_Sex_Main_Filtered[3,9],")"), paste(df_Plot_Sex_Main_Filtered[6,8], "(",df_Plot_Sex_Main_Filtered[6,9],")"), paste(df_Plot_Sex_Main_Filtered[9,8], "(",df_Plot_Sex_Main_Filtered[9,9],")"))
)

# write.csv2(df_table_Sex, file.path(path, "Gender.csv"), row.names = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Age
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Age (at first main admission) distribution over time
df_Output_Age_Main_Filtered <- filter(df_Output_Age_Main, (FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end))

df_Plot_Age_Main_Filtered <- df_Output_Age_Main_Filtered %>% 
  group_by(Strata, PatientSubgroup, AgeGroup) %>%
  summarise(SumGroup = sum(N)) %>%
  ungroup() %>% group_by(Strata, PatientSubgroup) %>%
summarise(AgeGroup = AgeGroup,
          SumGroup =  SumGroup, 
          Sum = sum(SumGroup),
          Percentage = round((SumGroup/Sum)*100,2))

df_table_Age <- data.frame(
  Period = c(as.character(df_Plot_Age_Main_Filtered[1,1]), as.character(df_Plot_Age_Main_Filtered[1,1]), as.character(df_Plot_Age_Main_Filtered[1,1]), as.character(df_Plot_Age_Main_Filtered[1,1]), as.character(df_Plot_Age_Main_Filtered[1,1]),
             as.character(df_Plot_Age_Main_Filtered[14,1]), as.character(df_Plot_Age_Main_Filtered[14,1]), as.character(df_Plot_Age_Main_Filtered[14,1]), as.character(df_Plot_Age_Main_Filtered[14,1]), as.character(df_Plot_Age_Main_Filtered[14,1]),
             as.character(df_Plot_Age_Main_Filtered[28,1]), as.character(df_Plot_Age_Main_Filtered[28,1]), as.character(df_Plot_Age_Main_Filtered[28,1]), as.character(df_Plot_Age_Main_Filtered[28,1]), as.character(df_Plot_Age_Main_Filtered[28,1])),
  
  AgeGroup = c("18 - 39 years old", "40 - 59 years old", "60 - 79 years old", "> 80 years old", "NA",  
               "18 - 39 years old", "40 - 59 years old", "60 - 79 years old", "> 80 years old", "NA", 
               "18 - 39 years old", "40 - 59 years old", "60 - 79 years old", "> 80 years old", "NA"),
 
  HIVCancer = c(paste(df_Plot_Age_Main_Filtered[6,4], "(",df_Plot_Age_Main_Filtered[6,6],")"), paste(df_Plot_Age_Main_Filtered[7,4], "(",df_Plot_Age_Main_Filtered[7,6],")"), paste(df_Plot_Age_Main_Filtered[8,4], "(",df_Plot_Age_Main_Filtered[8,6],")"), paste(0, "(",0,")"), paste(df_Plot_Age_Main_Filtered[9,4], "(",df_Plot_Age_Main_Filtered[9,6],")"),
                paste(df_Plot_Age_Main_Filtered[19,4], "(",df_Plot_Age_Main_Filtered[19,6],")"), paste(df_Plot_Age_Main_Filtered[20,4], "(",df_Plot_Age_Main_Filtered[20,6],")"), paste(df_Plot_Age_Main_Filtered[21,4], "(",df_Plot_Age_Main_Filtered[21,6],")"), paste(df_Plot_Age_Main_Filtered[22,4], "(",df_Plot_Age_Main_Filtered[22,6],")"), paste(0, "(",0,")"),
                paste(df_Plot_Age_Main_Filtered[33,4], "(",df_Plot_Age_Main_Filtered[33,6],")"), paste(df_Plot_Age_Main_Filtered[34,4], "(",df_Plot_Age_Main_Filtered[34,6],")"), paste(df_Plot_Age_Main_Filtered[35,4], "(",df_Plot_Age_Main_Filtered[35,6],")"), paste(df_Plot_Age_Main_Filtered[36,4], "(",df_Plot_Age_Main_Filtered[36,6],")"), paste(df_Plot_Age_Main_Filtered[37,4], "(",df_Plot_Age_Main_Filtered[37,6],")")),
  
  
  Cancer = c(paste(df_Plot_Age_Main_Filtered[1,4], "(",df_Plot_Age_Main_Filtered[1,6],")"), paste(df_Plot_Age_Main_Filtered[2,4], "(",df_Plot_Age_Main_Filtered[2,6],")"), paste(df_Plot_Age_Main_Filtered[3,4], "(",df_Plot_Age_Main_Filtered[3,6],")"), paste(df_Plot_Age_Main_Filtered[4,4], "(",df_Plot_Age_Main_Filtered[4,6],")"), paste(df_Plot_Age_Main_Filtered[5,4], "(",df_Plot_Age_Main_Filtered[5,6],")"),
             paste(df_Plot_Age_Main_Filtered[14,4], "(",df_Plot_Age_Main_Filtered[14,6],")"), paste(df_Plot_Age_Main_Filtered[15,4], "(",df_Plot_Age_Main_Filtered[15,6],")"), paste(df_Plot_Age_Main_Filtered[16,4], "(",df_Plot_Age_Main_Filtered[16,6],")"), paste(df_Plot_Age_Main_Filtered[17,4], "(",df_Plot_Age_Main_Filtered[17,6],")"), paste(df_Plot_Age_Main_Filtered[18,4], "(",df_Plot_Age_Main_Filtered[18,6],")"),
             paste(df_Plot_Age_Main_Filtered[28,4], "(",df_Plot_Age_Main_Filtered[28,6],")"), paste(df_Plot_Age_Main_Filtered[29,4], "(",df_Plot_Age_Main_Filtered[29,6],")"), paste(df_Plot_Age_Main_Filtered[30,4], "(",df_Plot_Age_Main_Filtered[30,6],")"), paste(df_Plot_Age_Main_Filtered[31,4], "(",df_Plot_Age_Main_Filtered[31,6],")"), paste(df_Plot_Age_Main_Filtered[32,4], "(",df_Plot_Age_Main_Filtered[32,6],")")),
  
  
  HIV = c(paste(df_Plot_Age_Main_Filtered[10,4], "(",df_Plot_Age_Main_Filtered[10,6],")"), paste(df_Plot_Age_Main_Filtered[11,4], "(",df_Plot_Age_Main_Filtered[11,6],")"), paste(df_Plot_Age_Main_Filtered[12,4], "(",df_Plot_Age_Main_Filtered[12,6],")"), paste(0, "(",0,")"), paste(df_Plot_Age_Main_Filtered[13,4], "(",df_Plot_Age_Main_Filtered[13,6],")"),
          paste(df_Plot_Age_Main_Filtered[23,4], "(",df_Plot_Age_Main_Filtered[23,6],")"), paste(df_Plot_Age_Main_Filtered[24,4], "(",df_Plot_Age_Main_Filtered[24,6],")"), paste(df_Plot_Age_Main_Filtered[25,4], "(",df_Plot_Age_Main_Filtered[25,6],")"), paste(df_Plot_Age_Main_Filtered[26,4], "(",df_Plot_Age_Main_Filtered[26,6],")"), paste(df_Plot_Age_Main_Filtered[27,4], "(",df_Plot_Age_Main_Filtered[27,6],")"), 
          paste(df_Plot_Age_Main_Filtered[38,4], "(",df_Plot_Age_Main_Filtered[38,6],")"), paste(df_Plot_Age_Main_Filtered[39,4], "(",df_Plot_Age_Main_Filtered[39,6],")"), paste(df_Plot_Age_Main_Filtered[40,4], "(",df_Plot_Age_Main_Filtered[40,6],")"), paste(df_Plot_Age_Main_Filtered[41,4], "(",df_Plot_Age_Main_Filtered[41,6],")"), paste(df_Plot_Age_Main_Filtered[42,4], "(",df_Plot_Age_Main_Filtered[42,6],")"))
)

# write.csv2(df_table_Age, file.path(path, "Age.csv"), row.names = FALSE)

# Not relevant for main analysis.
# Age distribution summary statistics
#df_Output_Age_Summary_Main  

# Age distribution quantiles
#df_Output_Age_Quantiles_Main  



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Case count per patient
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Case count over time
df_Output_CaseCount_Main_Filtered <- filter(df_Output_CaseCount_Main, (FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end))
  


df_Plot_CaseCount_Main_Filtered  <-  df_Output_CaseCount_Main_Filtered %>% 
  group_by(Strata, PatientSubgroup, CaseCountGroup) %>%
  summarise(SumGroup = sum(N)) %>%
  ungroup() %>% group_by(Strata, PatientSubgroup) %>%
  summarise(CaseCountGroup = CaseCountGroup,
            SumGroup =  SumGroup, 
            Sum = sum(SumGroup),
            Percentage = round((SumGroup/Sum)*100,2))

  
df_table_CaseCount <- data.frame(
  Period = c(as.character(df_Plot_CaseCount_Main_Filtered[1,1]), as.character(df_Plot_CaseCount_Main_Filtered[1,1]), as.character(df_Plot_CaseCount_Main_Filtered[1,1]), as.character(df_Plot_CaseCount_Main_Filtered[1,1]),
             as.character(df_Plot_CaseCount_Main_Filtered[14,1]), as.character(df_Plot_CaseCount_Main_Filtered[14,1]), as.character(df_Plot_CaseCount_Main_Filtered[14,1]), as.character(df_Plot_CaseCount_Main_Filtered[14,1]), 
             as.character(df_Plot_CaseCount_Main_Filtered[28,1]), as.character(df_Plot_CaseCount_Main_Filtered[28,1]), as.character(df_Plot_CaseCount_Main_Filtered[28,1]), as.character(df_Plot_CaseCount_Main_Filtered[28,1])),
  
  CaseCountGroup = c("1 Case", "2 - 4 Cases", "5 - 10 Cases ", "More than 10 Cases",  
               "1 Case", "2 - 4 Cases", "5 - 10 Cases ", "More than 10 Cases", 
               "1 Case", "2 - 4 Cases", "5 - 10 Cases ", "More than 10 Cases"),
  
  HIVCancer = c(paste(df_Plot_CaseCount_Main_Filtered[5,4], "(",df_Plot_CaseCount_Main_Filtered[5,6],")"), paste(df_Plot_CaseCount_Main_Filtered[6,4], "(",df_Plot_CaseCount_Main_Filtered[6,6],")"), paste(df_Plot_CaseCount_Main_Filtered[7,4], "(",df_Plot_CaseCount_Main_Filtered[7,6],")"), paste(df_Plot_CaseCount_Main_Filtered[8,4], "(",df_Plot_CaseCount_Main_Filtered[8,6],")"),
                paste(df_Plot_CaseCount_Main_Filtered[17,4], "(",df_Plot_CaseCount_Main_Filtered[17,6],")"), paste(df_Plot_CaseCount_Main_Filtered[18,4], "(",df_Plot_CaseCount_Main_Filtered[18,6],")"), paste(df_Plot_CaseCount_Main_Filtered[19,4], "(",df_Plot_CaseCount_Main_Filtered[19,6],")"), paste(df_Plot_CaseCount_Main_Filtered[20,4], "(",df_Plot_CaseCount_Main_Filtered[20,6],")"),
                paste(df_Plot_CaseCount_Main_Filtered[29,4], "(",df_Plot_CaseCount_Main_Filtered[29,6],")"), paste(df_Plot_CaseCount_Main_Filtered[30,4], "(",df_Plot_CaseCount_Main_Filtered[30,6],")"), paste(df_Plot_CaseCount_Main_Filtered[31,4], "(",df_Plot_CaseCount_Main_Filtered[31,6],")"), paste(df_Plot_CaseCount_Main_Filtered[32,4], "(",df_Plot_CaseCount_Main_Filtered[32,6],")")),
  
  
  Cancer = c(paste(df_Plot_CaseCount_Main_Filtered[1,4], "(",df_Plot_CaseCount_Main_Filtered[1,6],")"), paste(df_Plot_CaseCount_Main_Filtered[2,4], "(",df_Plot_CaseCount_Main_Filtered[2,6],")"), paste(df_Plot_CaseCount_Main_Filtered[3,4], "(",df_Plot_CaseCount_Main_Filtered[3,6],")"), paste(df_Plot_CaseCount_Main_Filtered[4,4], "(",df_Plot_CaseCount_Main_Filtered[4,6],")"),
             paste(df_Plot_CaseCount_Main_Filtered[13,4], "(",df_Plot_CaseCount_Main_Filtered[13,6],")"), paste(df_Plot_CaseCount_Main_Filtered[14,4], "(",df_Plot_CaseCount_Main_Filtered[14,6],")"), paste(df_Plot_CaseCount_Main_Filtered[15,4], "(",df_Plot_CaseCount_Main_Filtered[15,6],")"), paste(df_Plot_CaseCount_Main_Filtered[16,4], "(",df_Plot_CaseCount_Main_Filtered[16,6],")"),
             paste(df_Plot_CaseCount_Main_Filtered[25,4], "(",df_Plot_CaseCount_Main_Filtered[25,6],")"), paste(df_Plot_CaseCount_Main_Filtered[26,4], "(",df_Plot_CaseCount_Main_Filtered[26,6],")"), paste(df_Plot_CaseCount_Main_Filtered[27,4], "(",df_Plot_CaseCount_Main_Filtered[27,6],")"), paste(df_Plot_CaseCount_Main_Filtered[28,4], "(",df_Plot_CaseCount_Main_Filtered[28,6],")")),
  
  
  HIV = c(paste(df_Plot_CaseCount_Main_Filtered[9,4], "(",df_Plot_CaseCount_Main_Filtered[9,6],")"),paste(df_Plot_CaseCount_Main_Filtered[10,4], "(",df_Plot_CaseCount_Main_Filtered[10,6],")"), paste(df_Plot_CaseCount_Main_Filtered[11,4], "(",df_Plot_CaseCount_Main_Filtered[11,6],")"), paste(df_Plot_CaseCount_Main_Filtered[12,4], "(",df_Plot_CaseCount_Main_Filtered[12,6],")"), 
          paste(df_Plot_CaseCount_Main_Filtered[21,4], "(",df_Plot_CaseCount_Main_Filtered[21,6],")"), paste(df_Plot_CaseCount_Main_Filtered[22,4], "(",df_Plot_CaseCount_Main_Filtered[22,6],")"), paste(df_Plot_CaseCount_Main_Filtered[23,4], "(",df_Plot_CaseCount_Main_Filtered[23,6],")"), paste(df_Plot_CaseCount_Main_Filtered[24,4], "(",df_Plot_CaseCount_Main_Filtered[24,6],")"),
          paste(df_Plot_CaseCount_Main_Filtered[33,4], "(",df_Plot_CaseCount_Main_Filtered[33,6],")"), paste(df_Plot_CaseCount_Main_Filtered[34,4], "(",df_Plot_CaseCount_Main_Filtered[34,6],")"), paste(df_Plot_CaseCount_Main_Filtered[35,4], "(",df_Plot_CaseCount_Main_Filtered[35,6],")"), paste(df_Plot_CaseCount_Main_Filtered[36,4], "(",df_Plot_CaseCount_Main_Filtered[36,6],")"))
)

# write.csv2(df_table_CaseCount, file.path(path, "CaseCount.csv"), row.names = FALSE)

# Not relevant for main analysis.
# Case count summary statistics
#df_Output_CaseCount_Summary_Main 

# Case count quantiles
#df_Output_CaseCount_Quantiles_Main  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mean length of stay
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_MeanLengthOfStay_Main_Filtered  <- filter(df_Output_MeanLengthOfStay_Main, (FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end))

df_Plot_MeanLengthOfStay_Main_Filtered  <-  df_Output_MeanLengthOfStay_Main_Filtered %>% 
  group_by(Strata, PatientSubgroup, MeanLengthOfStayGroup) %>%
  summarise(SumGroup = sum(N)) %>%
  ungroup() %>% group_by(Strata, PatientSubgroup) %>%
  summarise(MeanLengthOfStayGroup = MeanLengthOfStayGroup,
            SumGroup =  SumGroup, 
            Sum = sum(SumGroup),
            Percentage = round((SumGroup/Sum)*100,2))


df_table_LengthOfStay <- data.frame(
  Period = c(as.character(df_Plot_MeanLengthOfStay_Main_Filtered[1,1]), as.character(df_Plot_MeanLengthOfStay_Main_Filtered[1,1]), as.character(df_Plot_MeanLengthOfStay_Main_Filtered[1,1]), as.character(df_Plot_MeanLengthOfStay_Main_Filtered[1,1]),
             as.character(df_Plot_MeanLengthOfStay_Main_Filtered[14,1]), as.character(df_Plot_MeanLengthOfStay_Main_Filtered[14,1]), as.character(df_Plot_MeanLengthOfStay_Main_Filtered[14,1]), as.character(df_Plot_MeanLengthOfStay_Main_Filtered[14,1]), 
             as.character(df_Plot_MeanLengthOfStay_Main_Filtered[28,1]), as.character(df_Plot_MeanLengthOfStay_Main_Filtered[28,1]), as.character(df_Plot_MeanLengthOfStay_Main_Filtered[28,1]), as.character(df_Plot_MeanLengthOfStay_Main_Filtered[28,1])),
  
  MeanLengthOfStayGroup = c("Up to 7 days", "7 - 14 days", "14 - 30 days", "More than 30 days",  
               "Up to 7 days", "7 - 14 days", "14 - 30 days", "More than 30 days",  
               "Up to 7 days", "7 - 14 days", "14 - 30 days", "More than 30 days"),
  
  HIVCancer = c(paste(df_Plot_MeanLengthOfStay_Main_Filtered[8,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[8,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[6,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[6,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[5,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[5,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[7,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[7,6],")"),
                paste(df_Plot_MeanLengthOfStay_Main_Filtered[20,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[20,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[18,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[18,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[17,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[17,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[19,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[19,6],")"),
                paste(df_Plot_MeanLengthOfStay_Main_Filtered[32,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[32,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[30,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[30,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[29,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[29,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[31,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[31,6],")")),
  
  
  Cancer = c(paste(df_Plot_MeanLengthOfStay_Main_Filtered[4,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[4,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[2,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[2,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[1,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[1,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[3,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[3,6],")"),
             paste(df_Plot_MeanLengthOfStay_Main_Filtered[16,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[16,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[14,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[14,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[13,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[13,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[15,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[15,6],")"),
             paste(df_Plot_MeanLengthOfStay_Main_Filtered[28,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[28,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[26,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[26,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[25,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[25,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[27,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[27,6],")")),
  
  
  HIV = c(paste(df_Plot_MeanLengthOfStay_Main_Filtered[12,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[12,6],")"),paste(df_Plot_MeanLengthOfStay_Main_Filtered[10,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[10,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[9,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[9,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[11,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[11,6],")"), 
          paste(df_Plot_MeanLengthOfStay_Main_Filtered[24,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[24,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[22,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[22,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[21,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[21,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[23,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[23,6],")"),
          paste(df_Plot_MeanLengthOfStay_Main_Filtered[36,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[36,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[34,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[34,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[33,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[33,6],")"), paste(df_Plot_MeanLengthOfStay_Main_Filtered[35,4], "(",df_Plot_MeanLengthOfStay_Main_Filtered[35,6],")"))
)

# write.csv2(df_table_LengthOfStay, file.path(path, "LengthOfStay.csv"), row.names = FALSE)
# Not relevant for main analysis.
#df_Output_MeanLengthOfStay_Summary_Main  

#df_Output_MeanLengthOfStay_Quantiles_Main  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Number of ICU transfers relative to number of admissions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Not relevant for main analysis.
#df_Output_ICUTransfersRelative_Summary_Main 
#df_Output_ICUTransfersRelative_Quantiles_Main  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Time spent in Intensive Care relative to total time
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Not relevant for main analysis.
#df_Output_ICUTimeRelative_Summary_Main 
#df_Output_ICUTimeRelative_Quantiles_Main  

################################################################################
#
#          Cancer+/HIV+   vs.   Cancer+/HIV- 
#
################################################################################


#!!!-!!!-!!! Plot describing completeness of data (Sankey diagram with nodes main diagnosis - treatment - follow up?)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Documented time span of main cancer care
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Not relevant for main analysis.
#df_Output_MainCancerDocumentedTimeSpan_Summary_Main  
#df_Output_MainCancerDocumentedTimeSpan_Quantiles_Main  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Age at presumed cancer diagnosis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Attention! Freiburg only matched patients available in the full data set!!!
# Age (at cancer diagnosis) distribution over time
df_Output_AgeAtCancerDiagnosis_Main_Filtered <- filter(df_Output_AgeAtCancerDiagnosis_Matched_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))

df_Output_AgeAtCancerDiagnosis_Main_Filtered%>% 
  group_by(PatientSubgroup, Strata) %>% 
  summarise(sum(N))


df_Plot_AgeAtCancerDiagnosis_Main_Filtered <- df_Output_AgeAtCancerDiagnosis_Main_Filtered %>% 
  group_by(Strata, PatientSubgroup, AgeGroup) %>%
  summarise(SumGroup = sum(N)) %>%
  ungroup() %>% group_by(Strata, PatientSubgroup) %>%
  summarise(AgeGroup = AgeGroup,
            SumGroup =  SumGroup, 
            Sum = sum(SumGroup),
            Percentage = round((SumGroup/Sum)*100,2))

df_table_AgeAtCancerDiagnosis <- data.frame(
  Period = c(as.character(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[1,1]), as.character(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[1,1]), as.character(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[1,1]), as.character(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[1,1]), as.character(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[1,1]),
             as.character(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[14,1]), as.character(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[14,1]), as.character(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[14,1]), as.character(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[14,1]), as.character(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[14,1]),
             as.character(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[28,1]), as.character(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[28,1]), as.character(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[28,1]), as.character(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[28,1]), as.character(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[28,1])),
  
  AgeGroup = c("18 - 39 years old", "40 - 59 years old", "60 - 79 years old", "> 80 years old", "NA",  
               "18 - 39 years old", "40 - 59 years old", "60 - 79 years old", "> 80 years old", "NA", 
               "18 - 39 years old", "40 - 59 years old", "60 - 79 years old", "> 80 years old", "NA"),
  
  HIVCancer = c(paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[6,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[6,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[7,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[7,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[8,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[8,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[9,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[9,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[10,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[10,6],")"),
                paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[16,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[16,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[17,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[17,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[18,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[18,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[19,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[19,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[20,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[20,6],")"),
                paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[26,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[26,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[27,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[27,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[28,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[28,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[29,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[29,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[30,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[30,6],")")),
  
  
  Cancer = c(paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[1,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[1,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[2,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[2,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[3,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[3,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[4,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[4,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[5,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[5,6],")"),
             paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[11,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[11,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[12,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[12,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[13,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[13,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[14,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[14,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[15,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[15,6],")"),
             paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[21,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[21,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[22,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[22,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[23,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[23,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[24,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[24,6],")"), paste(df_Plot_AgeAtCancerDiagnosis_Main_Filtered[25,4], "(",df_Plot_AgeAtCancerDiagnosis_Main_Filtered[25,6],")"))
  
)

# write.csv2(df_table_AgeAtCancerDiagnosis, file.path(path, "AgeAtCancerDiagnosis.csv"), row.names = FALSE)


# Not relevant for main analysis.
#df_Output_AgeAtCancerDiagnosis_Summary_Main  

#df_Output_AgeAtCancerDiagnosis_Quantiles_Main  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Count of distinct documented "real" cancer codes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Not avible at Cologne
df_Output_DistinctCodeCountCancer_Main_Filtered <- filter(df_Output_DistinctCodeCountCancer_Matched_Main, (FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end))

df_Output_DistinctCodeCountCancer_Main_Filtered %>% 
  group_by(PatientSubgroup, Strata) %>% 
  summarise(sum(N))



df_Plot_DistinctCodeCountCancer_Main_Filtered  <- df_Output_DistinctCodeCountCancer_Main_Filtered %>%
  group_by(Strata, PatientSubgroup, CancerCountGroup) %>%
  summarise(SumGroup = sum(N)) %>%
  ungroup() %>% group_by(Strata, PatientSubgroup) %>%
  summarise(CancerCountGroup = CancerCountGroup,
            SumGroup =  SumGroup, 
            Sum = sum(SumGroup),
            Percentage = round((SumGroup/Sum)*100,2))

df_table_DistinctCodeCountCancer <- data.frame(
  Period = c(as.character(df_Plot_DistinctCodeCountCancer_Main_Filtered[1,1]), as.character(df_Plot_DistinctCodeCountCancer_Main_Filtered[1,1]), as.character(df_Plot_DistinctCodeCountCancer_Main_Filtered[1,1]), 
             as.character(df_Plot_DistinctCodeCountCancer_Main_Filtered[7,1]), as.character(df_Plot_DistinctCodeCountCancer_Main_Filtered[7,1]), as.character(df_Plot_DistinctCodeCountCancer_Main_Filtered[7,1]), 
             as.character(df_Plot_DistinctCodeCountCancer_Main_Filtered[13,1]), as.character(df_Plot_DistinctCodeCountCancer_Main_Filtered[13,1]), as.character(df_Plot_DistinctCodeCountCancer_Main_Filtered[13,1])),
  
  AgeGroup = c("1 distinct cancer", "2 distinct cancers", "3 or more distinct cancers",
               "1 distinct cancer", "2 distinct cancers", "3 or more distinct cancers", 
               "1 distinct cancer", "2 distinct cancers", "3 or more distinct cancers"),
  
  HIVCancer = c(paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[4,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[4,6],")"), paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[5,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[5,6],")"), paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[6,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[6,6],")"),
                paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[10,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[10,6],")"), paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[11,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[11,6],")"), paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[12,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[12,6],")"), 
                paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[16,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[16,6],")"), paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[17,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[17,6],")"), paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[18,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[18,6],")")),
  
  
  Cancer = c(paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[1,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[1,6],")"), paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[2,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[2,6],")"), paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[3,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[3,6],")"), 
             paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[7,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[7,6],")"), paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[8,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[8,6],")"), paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[9,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[9,6],")"),
             paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[13,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[13,6],")"), paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[14,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[14,6],")"), paste(df_Plot_DistinctCodeCountCancer_Main_Filtered[15,4], "(",df_Plot_DistinctCodeCountCancer_Main_Filtered[15,6],")"))
  
)

# write.csv2(df_table_DistinctCodeCountCancer, file.path(path, "DistinctCodeCountCancer.csv"), row.names = FALSE)




# Not relevant for main analysis.  
#df_Output_DistinctCodeCountCancer_Summary_Main  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer grouping: AD, NAD and Non-HIV-associated cancer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# df_Output_HIVCancerCategories_Main_Filtered <- filter(df_Output_HIVCancerCategories_Matched_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))
# 
# df_Output_HIVCancerCategories_Main_Filtered %>% 
#   group_by(PatientSubgroup, Strata) %>% 
#   summarise(sum(N))


# 
# df_Plot_HIVCancerCategories_Main_Filtered <- df_Output_HIVCancerCategories_Main_Filtered %>%
#   group_by(PatientSubgroup, PatientSubgroupHIVCancerCategory, MainCancerDiagnosisYear) %>%
#   summarize( N = sum(N),
#              NSite = n_distinct(Site))%>%
#   rename(Year = MainCancerDiagnosisYear)
# 
# 
# df_Table_HIVCancerCategories_Main_Filtered  <- df_Output_HIVCancerCategories_Main_Filtered  %>%
#   group_by(Strata, PatientSubgroup, PatientSubgroupHIVCancerCategory) %>%
#   summarise(SumGroup = sum(N)) %>%
#   ungroup() %>% group_by(Strata, PatientSubgroup) %>%
#   summarise(PatientSubgroupHIVCancerCategory = PatientSubgroupHIVCancerCategory,
#             SumGroup =  SumGroup, 
#             Sum = sum(SumGroup),
#             Percentage = round((SumGroup/Sum)*100,2))
# 
# df_table_HIVCancerCategories <- data.frame(
#   Period = c(as.character(df_Table_HIVCancerCategories_Main_Filtered[1,1]), as.character(df_Table_HIVCancerCategories_Main_Filtered[1,1]), as.character(df_Table_HIVCancerCategories_Main_Filtered[1,1]),
#              as.character(df_Table_HIVCancerCategories_Main_Filtered[7,1]), as.character(df_Table_HIVCancerCategories_Main_Filtered[7,1]), as.character(df_Table_HIVCancerCategories_Main_Filtered[7,1]),
#              as.character(df_Table_HIVCancerCategories_Main_Filtered[13,1]), as.character(df_Table_HIVCancerCategories_Main_Filtered[13,1]), as.character(df_Table_HIVCancerCategories_Main_Filtered[13,1])),
#   
#   HIVCancerCategories = c("HIV-associated AD cancer", "HIV-associated non-AD cancer", "Non-HIV-associated cancer",
#                           "HIV-associated AD cancer", "HIV-associated non-AD cancer", "Non-HIV-associated cancer",
#                           "HIV-associated AD cancer", "HIV-associated non-AD cancer", "Non-HIV-associated cancer"),
#   
#   HIVCancer = c(paste(df_Table_HIVCancerCategories_Main_Filtered[4,4], "(",df_Table_HIVCancerCategories_Main_Filtered[4,6],")"), paste(df_Table_HIVCancerCategories_Main_Filtered[5,4], "(",df_Table_HIVCancerCategories_Main_Filtered[5,6],")"), paste(df_Table_HIVCancerCategories_Main_Filtered[6,4], "(",df_Table_HIVCancerCategories_Main_Filtered[6,6],")"), 
#                 paste(df_Table_HIVCancerCategories_Main_Filtered[10,4], "(",df_Table_HIVCancerCategories_Main_Filtered[10,6],")"), paste(df_Table_HIVCancerCategories_Main_Filtered[11,4], "(",df_Table_HIVCancerCategories_Main_Filtered[11,6],")"), paste(df_Table_HIVCancerCategories_Main_Filtered[12,4], "(",df_Table_HIVCancerCategories_Main_Filtered[12,6],")"), 
#                 paste(df_Table_HIVCancerCategories_Main_Filtered[16,4], "(",df_Table_HIVCancerCategories_Main_Filtered[16,6],")"), paste(df_Table_HIVCancerCategories_Main_Filtered[17,4], "(",df_Table_HIVCancerCategories_Main_Filtered[17,6],")"), paste(df_Table_HIVCancerCategories_Main_Filtered[18,4], "(",df_Table_HIVCancerCategories_Main_Filtered[18,6],")")),
#   
#   
#   Cancer = c(paste(df_Table_HIVCancerCategories_Main_Filtered[1,4], "(",df_Table_HIVCancerCategories_Main_Filtered[1,6],")"), paste(df_Table_HIVCancerCategories_Main_Filtered[2,4], "(",df_Table_HIVCancerCategories_Main_Filtered[2,6],")"), paste(df_Table_HIVCancerCategories_Main_Filtered[3,4], "(",df_Table_HIVCancerCategories_Main_Filtered[3,6],")"), 
#              paste(df_Table_HIVCancerCategories_Main_Filtered[7,4], "(",df_Table_HIVCancerCategories_Main_Filtered[7,6],")"), paste(df_Table_HIVCancerCategories_Main_Filtered[8,4], "(",df_Table_HIVCancerCategories_Main_Filtered[8,6],")"),paste(df_Table_HIVCancerCategories_Main_Filtered[9,4], "(",df_Table_HIVCancerCategories_Main_Filtered[9,6],")"),  
#              paste(df_Table_HIVCancerCategories_Main_Filtered[13,4], "(",df_Table_HIVCancerCategories_Main_Filtered[13,6],")"), paste(df_Table_HIVCancerCategories_Main_Filtered[14,4], "(",df_Table_HIVCancerCategories_Main_Filtered[14,6],")"),paste(df_Table_HIVCancerCategories_Main_Filtered[15,4], "(",df_Table_HIVCancerCategories_Main_Filtered[15,6],")"))
#   
# )

# write.csv2(df_table_HIVCancerCategories, file.path(path, "HIVCancerCategories.csv"), row.names = FALSE)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer grouping: Carcinoma in situ
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_CIS_Main_Filtered <- filter(df_Output_CIS_Matched_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))
 
df_Output_CIS_Main_Filtered %>% 
  group_by(PatientSubgroup, Strata) %>% 
  summarise(sum(N))

df_Plot_CIS_Main_Filtered <- df_Output_CIS_Main_Filtered %>%
  group_by(Strata, PatientSubgroup,  MainCancerIsCarcinomaInSitu) %>%
  summarise(SumGroup = sum(N)) %>%
  ungroup() %>% group_by(Strata, PatientSubgroup) %>%
  summarise(MainCancerIsCarcinomaInSitu = MainCancerIsCarcinomaInSitu,
            SumGroup =  SumGroup, 
            Sum = sum(SumGroup),
            Percentage = round((SumGroup/Sum)*100,2))

df_table_CIS <- data.frame(
  Period = c(as.character(df_Plot_CIS_Main_Filtered [1,1]), as.character(df_Plot_CIS_Main_Filtered [1,1]), 
             as.character(df_Plot_CIS_Main_Filtered [5,1]), as.character(df_Plot_CIS_Main_Filtered [5,1]),
             as.character(df_Plot_CIS_Main_Filtered [9,1]), as.character(df_Plot_CIS_Main_Filtered [9,1])),
  
  AgeGroup = c("CIS", "No CIS",
               "CIS", "No CIS",
               "CIS", "No CIS"),
  
  HIVCancer = c(paste(df_Plot_CIS_Main_Filtered [4,4], "(",df_Plot_CIS_Main_Filtered [4,6],")"), paste(df_Plot_CIS_Main_Filtered [3,4], "(",df_Plot_CIS_Main_Filtered [3,6],")"), 
                paste(df_Plot_CIS_Main_Filtered [8,4], "(",df_Plot_CIS_Main_Filtered [8,6],")"), paste(df_Plot_CIS_Main_Filtered [7,4], "(",df_Plot_CIS_Main_Filtered [7,6],")"), 
                paste(df_Plot_CIS_Main_Filtered [12,4], "(",df_Plot_CIS_Main_Filtered [12,6],")"), paste(df_Plot_CIS_Main_Filtered [11,4], "(",df_Plot_CIS_Main_Filtered [11,6],")")),
  
  
  Cancer = c(paste(df_Plot_CIS_Main_Filtered [2,4], "(",df_Plot_CIS_Main_Filtered [2,6],")"), paste(df_Plot_CIS_Main_Filtered [1,4], "(",df_Plot_CIS_Main_Filtered [1,6],")"), 
             paste(df_Plot_CIS_Main_Filtered [6,4], "(",df_Plot_CIS_Main_Filtered [6,6],")"), paste(df_Plot_CIS_Main_Filtered [5,4], "(",df_Plot_CIS_Main_Filtered [5,6],")"), 
             paste(df_Plot_CIS_Main_Filtered [10,4], "(",df_Plot_CIS_Main_Filtered [10,6],")"), paste(df_Plot_CIS_Main_Filtered [9,4], "(",df_Plot_CIS_Main_Filtered [9,6],")"))
  
)

# write.csv2(df_table_CIS, file.path(path, "IsCarcinomaInSitu.csv"), row.names = FALSE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: Topography by Organ
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Not relevant for main analysis.  
#df_Output_MainCancerTopographyDetail_Main  

# Data stratified by year of cancer diagnosis
df_Output_MainCancerTopographyDetail_OverTime_Main_Filtered <- filter(df_Output_MainCancerTopographyDetail_OverTime_Matched_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))

df_Output_MainCancerTopographyDetail_OverTime_Main_Filtered %>% 
  group_by(PatientSubgroup) %>% 
  summarise(sum(N))

df_Count_MainCancerTopographyDetail <- df_Output_MainCancerTopographyDetail_OverTime_Main_Filtered %>% 
  group_by(PatientSubgroup, MainCancerTopographyDetail) %>% 
  summarise(sum(N))

df_Plot_MainCancerTopographyDetail_Main_Filtered  <- df_Output_MainCancerTopographyDetail_OverTime_Main_Filtered  %>%
  group_by(PatientSubgroup, MainCancerTopographyDetail, MainCancerDiagnosisYear) %>%
  summarize(N = sum(N)) 

df_Plot_MainCancerTopographyDetail_Main_Filtered_HIV <- df_Plot_MainCancerTopographyDetail_Main_Filtered  %>%
  filter(PatientSubgroup == "Cancer+/HIV+") %>%
  group_by(PatientSubgroup, MainCancerTopographyDetail) %>%
  summarise(N =sum(N)) %>%
  arrange(-N)


vc_TopographyDetailSorted <- df_Plot_MainCancerTopographyDetail_Main_Filtered %>%
  filter(PatientSubgroup == "Cancer+/HIV+") %>%
  slice_max(n = 15,
            order_by = N) %>%
  pull(MainCancerTopographyDetail)




# Basti added
vc_TopographyGroupSorted <- df_Plot_MainCancerTopographyGroup_Main_Filtered %>%
  filter(PatientSubgroup == "Cancer+/HIV+") %>%
  slice_max(n = 15,
            order_by = N) %>%
  pull(MainCancerTopographyGroup)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: Topography by ICD Grouping
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Not relevant for main analysis.  
#df_Output_MainCancerTopographyGroup_Main  

# Data stratified by year of cancer diagnosis
df_Output_MainCancerTopographyGroup_OverTime_Main_Filtered <- filter(df_Output_MainCancerTopographyGroup_OverTime_Matched_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))


df_Output_MainCancerTopographyGroup_OverTime_Main_Filtered  %>% 
  group_by(PatientSubgroup) %>% 
  summarise(sum(N))

df_Count_MainCancerTopographyGroup <- df_Output_MainCancerTopographyGroup_OverTime_Main_Filtered  %>% 
  group_by(PatientSubgroup, MainCancerTopographyGroup) %>% 
  summarise(sum(N))


df_Plot_MainCancerTopographyGroup_Main_Filtered  <- df_Output_MainCancerTopographyGroup_OverTime_Main_Filtered %>%
  group_by(PatientSubgroup, MainCancerTopographyGroup, MainCancerDiagnosisYear) %>%
  summarize(N = sum(N))

df_Plot_MainCancerTopographyGroup_Main_Filtered_HIV <- df_Plot_MainCancerTopographyGroup_Main_Filtered %>%
  filter(PatientSubgroup == "Cancer+/HIV+") %>%
  group_by(PatientSubgroup, MainCancerTopographyGroup) %>%
  summarise(N =sum(N)) %>%
  arrange(-N)


df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered  <- df_Output_MainCancerTopographyGroup_OverTime_Main_Filtered %>%
  group_by(PatientSubgroup, MainCancerTopographyGroup, MainCancerDiagnosisYear) %>%
  summarize(N = sum(N)) %>%
  group_by(PatientSubgroup)

df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered_HIV <- df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered %>%
  filter(PatientSubgroup == "Cancer+/HIV+" & MainCancerTopographyGroup %in% vc_TopographyGroupSorted) 

#df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered_HIV$MainCancerTopographyGroup <- factor(df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered_HIV$MainCancerTopographyGroup, levels = vc_TopographyGroupSorted)


df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered_Controll <- df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered %>%
  filter(PatientSubgroup == "Cancer+/HIV-" & MainCancerTopographyGroup %in% vc_TopographyGroupSorted) 

#df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered_Controll$MainCancerTopographyGroup <- factor(df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered_Controll$MainCancerTopographyGroup, levels = vc_TopographyGroupSorted)

###

# df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered  <- df_Output_MainCancerTopographyGroup_OverTime_Main_Filtered  %>%
#   group_by(Strata, PatientSubgroup, PatientSubgroupHIVCancerCategory) %>%
#   summarise(SumGroup = sum(N)) %>%
#   ungroup() %>% group_by(Strata, PatientSubgroup) %>%
#   summarise(MainCancerIsCarcinomaInSitu = MainCancerIsCarcinomaInSitu,
#             SumGroup =  SumGroup, 
#             Sum = sum(SumGroup),
#             Percentage = round((SumGroup/Sum)*100,2))

# df_table_MainCancerTopographyGroup <- data.frame(
#   Period = c(as.character(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[1,1]), as.character(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[1,1]), 
#              as.character(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[5,1]), as.character(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[5,1]),
#              as.character(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[9,1]), as.character(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[9,1])),
#   
#   AgeGroup = c("CIS", "No CIS",
#                "CIS", "No CIS",
#                "CIS", "No CIS"),
#   
#   HIVCancer = c(paste(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[4,4], "(",df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[4,6],")"), paste(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[3,4], "(",df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[3,6],")"), 
#                 paste(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[8,4], "(",df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[8,6],")"), paste(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[7,4], "(",df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[7,6],")"), 
#                 paste(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[12,4], "(",df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[12,6],")"), paste(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[11,4], "(",df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[11,6],")")),
#   
#   
#   Cancer = c(paste(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[2,4], "(",df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[2,6],")"), paste(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[1,4], "(",df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[1,6],")"), 
#              paste(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[6,4], "(",df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[6,6],")"), paste(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[5,4], "(",df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[5,6],")"), 
#              paste(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[10,4], "(",df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[10,6],")"), paste(df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[9,4], "(",df_Table_MainCancerTopographyGroup_OverTime_Main_Filtered[9,6],")"))
#   
# )

# write.csv2(df_table_CIS, file.path(path, "IsCarcinomaInSitu.csv"), row.names = FALSE)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: By entity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Not relevant for main analysis.  
# Cancer occurrence grouped by coded entity
#df_Output_CancerEntities_Main  

# Data stratified by year of cancer diagnosis
df_Output_CancerEntities_OverTime_Main_Filtered <- filter(df_Output_CancerEntities_OverTime_Matched_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))

df_Output_CancerEntities_OverTime_Main_Filtered %>% 
  group_by(PatientSubgroup) %>% 
  summarise(sum(N))

#df_Plot_CancerEntities_Main_Filtered  <- df_Output_CancerEntities_OverTime_Main_Filtered %>%
 # group_by(PatientSubgroup, MainCancerCode) %>%
 # summarize(N = sum(N)) %>%
 # group_by(PatientSubgroup) %>%
#  mutate(Proportion = N / sum(N))

#vc_CancerEntitiesSorted <- df_Plot_CancerEntities_Main_Filtered %>%
 # filter(PatientSubgroup == "Cancer+/HIV+") %>%
 # slice_max(n = 50,
 #           order_by = Proportion) %>%
 # pull(MainCancerCode)


df_Plot_CancerEntities_OverTime_Main_Filtered  <- df_Output_CancerEntities_OverTime_Main_Filtered %>%
  group_by(PatientSubgroup, MainCancerCode, MainCancerDiagnosisYear) %>%
  mutate(EntitieSum = sum(N)) %>% slice(1)

df_Plot_CancerEntities_OverTime_Main_Filtered  %>% 
  group_by(PatientSubgroup) %>% 
  summarise(sum(N), sum(EntitieSum))


df_Plot_CancerEntities_OverTime_Main_Filtered_HIV <- df_Plot_CancerEntities_OverTime_Main_Filtered %>%
  filter(PatientSubgroup == "Cancer+/HIV+") 


Test_Entity_Trends <- df_Plot_CancerEntities_OverTime_Main_Filtered_HIV %>%
  group_by(PatientSubgroup, MainCancerCode, Strata) %>%
  summarize(N = sum(EntitieSum)) %>%
  group_by(Strata) %>%
  mutate(Proportion = N / sum(N),
         Number_by_Strata = sum(N))

# Cancer Categories for each Cancer (Not for each Patient)
AD <- c("C46.0", "C46.1", "C46.2", "C46.3", "C46.7", "C46.8", "C46.9", #Kaposi Sarkom
        "C53.0", "C53.8", "C53.9", #Zervixkarzinom
        "C82.2", "C82.3", "C82.7",
        "C83.0", "C83.1", "C83.3", "C83.3+", "C83.4", "C83.5", "C83.7", "C83.8",
        "C84.4", "C84.5", "C84.7",
        "C85.1", "C85.2", "C85.7", "C85.9",
        "C86.2")

virus_NAD <- c("C01", "C02.4", "C09.0", "C09.1", "C09.8", "C09.9",
               "C10.0", "C10.2", "C10.3", "C10.4", "C10.8", "C10.9", "C14.0", "C14.2", 
               "C20", "C21.0", "C21.1", "C21.8", "C22.0", "C51.1", "C51.8", "C51.9",
               "C52", "C60.0", "C60.1", "C60.9",
               "C81.0", "C81.1", "C81.1+", "C81.2", "C81.3", "C81.7", "C81.9"
               )


Test_CancerCategory <- Test_Entity_Trends %>% 
   mutate(CancerCategory = ifelse(MainCancerCode %in% AD, "AD", ifelse(
                                                      MainCancerCode %in% virus_NAD, "virus-NAD", "NAD"))) %>%
  group_by(Strata,CancerCategory) %>% summarise( N = sum(N))

Test_CancerCategory %>% group_by(Strata) %>% mutate(Sum = sum(N),
  Proportion = N/sum(N))


# Top 5 by Strata
df_Output_CancerEntities_OverTime_Main_Filtered$MainCancerCodeDig  <- substring(df_Output_CancerEntities_OverTime_Main_Filtered$MainCancerCode, 1, 3) 

df_CountEntities_OverTime <- df_Output_CancerEntities_OverTime_Main_Filtered %>% 
  filter(PatientSubgroup == "Cancer+/HIV+") %>% 
  group_by(MainCancerCodeDig, MainCancerDiagnosisYear) %>%
  summarise(Sum = sum(N))

df_CountEntities <- df_Output_CancerEntities_OverTime_Main_Filtered %>% 
  filter(PatientSubgroup == "Cancer+/HIV+") %>% 
  group_by(MainCancerCodeDig) %>%
  summarise(Sum = sum(N))

df_CountEntities_Orderd <- df_CountEntities[order(-df_CountEntities$Sum),]
vc_entities <- df_CountEntities_Orderd$MainCancerCodeDig[1:8]

df_CountEntities_OverTime %>% filter(MainCancerCodeDig %in% vc_entities) %>%
ggplot( aes(x=MainCancerDiagnosisYear, y=Sum)) + 
  geom_line(aes(colour=MainCancerCodeDig))

Test_Entity_Trends$MainCancerCodeGroup <- substring(Test_Entity_Trends$MainCancerCode, 1, 3) 


Top_Entities_Strata <- Test_Entity_Trends %>% group_by(Strata, MainCancerCodeGroup) %>%
                  summarise(N_Group = sum(N),
                        Proportion_Group = N_Group/Number_by_Strata) %>% 
  slice(1) %>% ungroup() %>%
  group_by(Strata) %>% slice_max(n = 5,
                                 order_by = Proportion_Group) 

Top_Entities_Overall <- Test_Entity_Trends %>% group_by(MainCancerCodeGroup) %>%
  summarise(Sum = sum(N))


# Absteigend nach Sportnote sortieren
Top_Entities_Overall <- Top_Entities_Overall[order(-Top_Entities_Overall$Sum),]
### Trendanalyse

#df_Plot_CancerEntities_OverTime_Main_Filtered_HIV$MainCancerCode <- substring(df_Plot_CancerEntities_OverTime_Main_Filtered_HIV $MainCancerCode, 1, 3) 

#AD
#non-Hodgkin Lymphome (C82-C86) (C85 insbesondere gefallen)
vc_nhl <- c("C82.2", "C82.3", "C82.7",
            "C83.0", "C83.1", "C83.3", "C83.3+", "C83.4", "C83.5", "C83.7", "C83.8",
            "C84.4", "C84.5", "C84.7",
            "C85.1", "C85.2", "C85.7", "C85.9",
            "C86.2")

#Kaposi Sarkom (C46) (gefallen)
vc_ks <- c("C46.0", "C46.1", "C46.2", "C46.3", "C46.7", "C46.8", "C46.9")

#Zervixkarzinom (C53)
vc_cc <- c("C53.0", "C53.8", "C53.9")

#HIV-assosiert
#Hogkin Lymphom (C81)
vc_hl <- c("C81.0", "C81.1", "C81.2", "C81.3", "C81.4", " C81.7", "C81.9")

#HIV- assosierte Lip, oral cavity, pharynx (C01-C14)
vc_lop <- c("C01", "C02.4", "C09.0", "C09.1", "C09.8", "C09.9",
            "C10.0", "C10.2", "C10.3", "C10.4", "C10.8", "C10.9", "C14.0", "C14.2")

#Anus C21 (leicht gefallen)
vc_a <- c("C21.0", "C21.1", "C21.8")

#Leberzellkarzinom C22.0
vc_hc <- c("C22.0")

#Vulva und Vagina (C51-C52)
vc_vv <- c("C51.1", "C51.8", "C51.9", "C52")

#Penis (C60) 
vc_p <- c("C60.0", "C60.1", "C60.9")

#Trends anderer
#Lungenkrebs (C34)
vc_lc <- c("C34.0", "C34.1", "C34.2", "C34.3", "C34.8", "C34.9", "C34.9+")

#Prostata (C61) (leicht gestiegen)
vc_prosta <-c("C61")
  
#Gehirn (C71) (leicht gestiegen
vc_brain <- c("C71.0", "C71.1", "C71.2", "C71.3", "C71.4", "C71.6", "C71.8", "C71.9")           

df_Trend_Strata_Plot <- Test_Entity_Trends %>% ungroup() %>%
  mutate(TrendCategory = ifelse(MainCancerCode %in% vc_nhl, "Non Hodgkin's lymphoma", 
                                ifelse(MainCancerCode %in% vc_ks, "Kaposi's sarcoma", 
                                ifelse(MainCancerCode %in% vc_cc, "Cervical carcinoma",    
                                ifelse(MainCancerCode %in% vc_hl, "Hodgkin lymphoma",
                                ifelse(MainCancerCode %in% vc_lop, "HIV-associated lip, oral cavity, pharynx",
                                ifelse(MainCancerCode %in% vc_a, "Anal carcinoma",
                                ifelse(MainCancerCode %in% vc_hc, "Hepatocellular carcinoma",
                                ifelse(MainCancerCode %in% vc_vv, "Vulvar and vaginal cancer", 
                                ifelse(MainCancerCode %in% vc_p, "Penile carcinoma",
                                ifelse(MainCancerCode %in% vc_lc, "Lung cancer",
                                ifelse(MainCancerCode %in% vc_prosta, "Prostate cancer",
                                ifelse(MainCancerCode %in% vc_brain, "Brain cancer", "Other")
                                )))))))))))) %>%
  group_by(Strata) %>%
  mutate(Number_by_Strata = sum(N)) %>%       
  group_by(Strata, TrendCategory) %>% mutate(Number_by_Trend = sum(N),
                                             Propotion = Number_by_Trend/Number_by_Strata)

df_Trend_Strata_Plot %>%
 # filter(TrendCategory != "Other") %>%
ggplot(aes(fill=TrendCategory, y=Propotion, x=Strata)) + 
  geom_bar(position="fill", stat="identity")    

Test_Entity_Trends_II <- df_Plot_CancerEntities_OverTime_Main_Filtered_HIV %>%
  group_by(PatientSubgroup, MainCancerCode, MainCancerDiagnosisYear) %>%
  summarize(N = sum(N)) %>%
  group_by(MainCancerDiagnosisYear) %>%
  mutate(Proportion = N / sum(N),
         Number_by_Year = sum(N))

df_Trend_Plot <- Test_Entity_Trends_II %>% ungroup() %>%
  mutate(TrendCategory = ifelse(MainCancerCode %in% vc_nhl, "Non Hodgkin's lymphoma", 
                                ifelse(MainCancerCode %in% vc_ks, "Kaposi's sarcoma", 
                                       ifelse(MainCancerCode %in% vc_cc, "Cervical carcinoma",    
                                              ifelse(MainCancerCode %in% vc_hl, "Hodgkin lymphoma",
                                                     ifelse(MainCancerCode %in% vc_lop, "HIV-associated lip, oral cavity, pharynx",
                                                            ifelse(MainCancerCode %in% vc_a, "Anal carcinoma",
                                                                   ifelse(MainCancerCode %in% vc_hc, "Hepatocellular carcinoma",
                                                                          ifelse(MainCancerCode %in% vc_vv, "Vulvar and vaginal cancer", 
                                                                                 ifelse(MainCancerCode %in% vc_p, "Penile carcinoma",
                                                                                        ifelse(MainCancerCode %in% vc_lc, "Lung cancer",
                                                                                               ifelse(MainCancerCode %in% vc_prosta, "Prostate cancer",
                                                                                                      ifelse(MainCancerCode %in% vc_brain, "Brain cancer", "Other")
                                                                                               )))))))))))) %>%
  group_by(MainCancerDiagnosisYear) %>%
  mutate(Number_by_Year = sum(N)) %>%       
  group_by(MainCancerDiagnosisYear, TrendCategory) %>% mutate(Number_by_Trend = sum(N),
                                             Propotion = Number_by_Trend/Number_by_Year)


df_Trend_Plot %>%
  filter(TrendCategory != "Other") %>%
ggplot(aes(x=MainCancerDiagnosisYear, y=Number_by_Trend, group=1)) + 
  geom_line() +
  geom_smooth(method='glm', colour='#8e1e39',size=0.5)+
  facet_wrap(~TrendCategory)
                                              
  


                                            
                                              
#df_Plot_CancerEntities_OverTime_Main_Filtered_HIV$MainCancerCode <- factor(df_Plot_CancerEntities_OverTime_Main_Filtered_HIV$MainCancerCode, levels = vc_CancerEntitiesSorted)


# df_Output_CancerEntitiesGrouped_OverTime_Main_Filtered$MainCancerCode <- substring(df_Output_CancerEntitiesGrouped_OverTime_Main_Filtered$MainCancerCode, 1, 3) 

# df_Plot_CancerEntitiesGrouped_Main_Filtered  <- df_Output_CancerEntitiesGrouped_OverTime_Main_Filtered %>%
#   group_by(PatientSubgroup, MainCancerCode) %>%
#   summarize(N = sum(N)) %>%
#   group_by(PatientSubgroup) %>%
#   mutate(Proportion = N / sum(N))
# 
# vc_CancerEntitiesGroupedSorted <- df_Plot_CancerEntitiesGrouped_Main_Filtered %>%
#   filter(PatientSubgroup == "Cancer+/HIV+") %>%
#   slice_max(n = 20,
#             order_by = Proportion) %>%
#   pull(MainCancerCode)
# 
# 
# df_Plot_CancerEntitiesGrouped_OverTime_Main_Filtered  <- df_Output_CancerEntitiesGrouped_OverTime_Main_Filtered %>%
#   group_by(PatientSubgroup, MainCancerCode, MainCancerDiagnosisYear) %>%
#   summarize(N = sum(N)) 
# 
# df_Plot_CancerEntitiesGrouped_OverTime_Main_Filtered_HIV <- df_Plot_CancerEntitiesGrouped_OverTime_Main_Filtered %>%
#   filter(PatientSubgroup == "Cancer+/HIV+" & MainCancerCode %in% vc_CancerEntitiesGroupedSorted) 
# 
# df_Plot_CancerEntitiesGrouped_OverTime_Main_Filtered_HIV$MainCancerCode <- factor(df_Plot_CancerEntitiesGrouped_OverTime_Main_Filtered_HIV$MainCancerCode, levels = vc_CancerEntitiesGroupedSorted)
# 
# 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Metastasis occurrence
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_MetastasisOccurrence_Main_Filtered <- filter(df_Output_MetastasisOccurrence_Matched_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))

df_Plot_MetastasisOccurrence_Main_Filtered <- df_Output_MetastasisOccurrence_Main_Filtered %>%
  group_by(PatientSubgroup, MainCancerDiagnosisYear) %>%
  summarize( N = sum(N),
             CountMetastasis_overall = sum(CountMetastasis),
             CountNoMetastasis_overall = sum(CountNoMetastasis),
             CountMetastasisWithCancerDiagnosis_overall = sum(CountMetastasisWithCancerDiagnosis),
             CountMetastasisAfterCancerDiagnosis_overall = sum(CountMetastasisAfterCancerDiagnosis),
             NSite = n_distinct(Site))%>%
  rename(Year = MainCancerDiagnosisYear)



df_Table_MetastasisOccurrence_Main_Filtered  <- df_Output_MetastasisOccurrence_Main_Filtered   %>%
  group_by(Strata, PatientSubgroup) %>%
  summarise(SumGroup = sum(N),
            SumMetastasis = sum(CountMetastasis),
            Percentage = round((SumMetastasis/SumGroup)*100,2))
  


df_table_MetastasisOccurrence <- data.frame(
  Period = c(as.character(df_Table_MetastasisOccurrence_Main_Filtered[1,1]),  
             as.character(df_Table_MetastasisOccurrence_Main_Filtered[3,1]), 
             as.character(df_Table_MetastasisOccurrence_Main_Filtered[5,1])),
  
  MetastasisOccurrence= c("MetastasisOccurrence",
               "MetastasisOccurrence",
               "MetastasisOccurrence"),
  
  HIVCancer = c(paste(df_Table_MetastasisOccurrence_Main_Filtered[2,4], "(",df_Table_MetastasisOccurrence_Main_Filtered[2,5],")"),
                paste(df_Table_MetastasisOccurrence_Main_Filtered[4,4], "(",df_Table_MetastasisOccurrence_Main_Filtered[4,5],")"), 
                paste(df_Table_MetastasisOccurrence_Main_Filtered[6,4], "(",df_Table_MetastasisOccurrence_Main_Filtered[6,5],")")),
  
  
  Cancer = c(paste(df_Table_MetastasisOccurrence_Main_Filtered[1,4], "(",df_Table_MetastasisOccurrence_Main_Filtered[1,5],")"),
             paste(df_Table_MetastasisOccurrence_Main_Filtered[3,4], "(",df_Table_MetastasisOccurrence_Main_Filtered[3,5],")"), 
             paste(df_Table_MetastasisOccurrence_Main_Filtered[4,4], "(",df_Table_MetastasisOccurrence_Main_Filtered[5,5],")"))
  
)

# write.csv2(df_table_MetastasisOccurrence, file.path(path, "MetastasisOccurrence.csv"), row.names = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Time to Metastasis (Add to merge data)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Not relevant for main analysis. 
#df_Output_TimeCancerToMetastasis_Summary_Main  
# Not relevant for main analysis. 
#df_Output_TimeCancerToMetastasis_Quantiles_Main  



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Therapy modalities
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_TherapyModalities_Main_Filtered <- filter(df_Output_TherapyModalities_Matched_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))
  
df_Plot_TherapyModalities_OverTime_Main_Filtered <- df_Output_TherapyModalities_Main_Filtered %>%
  group_by(PatientSubgroup, MainCancerDiagnosisYear) %>%
  summarize( N = sum(N),
             CountNoMajorTherapyCoded_overall = sum(CountNoMajorTherapyCoded),
             CountAnyMajorTherapy_overall = sum(CountAnyMajorTherapy),
             ProportionAnyMajorTherapy_overll = CountAnyMajorTherapy_overall / N,
             CountSurgery_overall = sum(CountSurgery),
             ProportionSurgery = CountSurgery_overall / CountAnyMajorTherapy_overall,
             CountChemotherapy_overall = sum(CountChemotherapy),
             ProportionChemotherapy = CountChemotherapy_overall / CountAnyMajorTherapy_overall,
             CountImmunotherapy_overall = sum(CountImmunotherapy),
             ProportionImmunotherapy = CountImmunotherapy_overall / CountAnyMajorTherapy_overall,
             CountRadiotherapy_overall = sum(CountRadiotherapy),
             ProportionRadiotherapy = CountRadiotherapy_overall / CountAnyMajorTherapy_overall,
             CountNuclearMedicineTherapy_overall = sum(CountNuclearMedicineTherapy),
             ProportionNuclearMedicineTherapy = CountNuclearMedicineTherapy_overall / CountAnyMajorTherapy_overall,
             NSite = n_distinct(Site)) 


            
df_Plot_TherapyModalities_Main_Filtered <- df_Plot_TherapyModalities_OverTime_Main_Filtered %>%
  group_by(PatientSubgroup) %>%
  summarize(N = sum(N),
            CountNoMajorTherapyCoded = sum (CountNoMajorTherapyCoded_overall),
            ProportionNoMajorTherapyCoded = CountNoMajorTherapyCoded/N,
            CountAnyMajorTherapy = sum(CountAnyMajorTherapy_overall),
            ProportiobAnyMajorTherapy = CountAnyMajorTherapy/N,
            CountSurgery = sum(CountSurgery_overall),
            ProportionSurgery = CountSurgery / CountAnyMajorTherapy,
            CountChemotherapy = sum(CountChemotherapy_overall),
            ProportionChemotherapy = CountChemotherapy / CountAnyMajorTherapy,
            CountImmunotherapy = sum(CountImmunotherapy_overall),
            ProportionImmunotherapy = CountImmunotherapy / CountAnyMajorTherapy,
            CountRadiotherapy = sum(CountRadiotherapy_overall),
            ProportionRadiotherapy = CountRadiotherapy / CountAnyMajorTherapy,
            CountNuclearMedicineTherapy = sum(CountNuclearMedicineTherapy_overall),
            ProportionNuclearMedicineTherapy = CountNuclearMedicineTherapy / CountAnyMajorTherapy
            )
            
  
df_Tabel_TherapyModalities_Main_Filtered <- df_Output_TherapyModalities_Main_Filtered  %>%
  group_by(Strata, PatientSubgroup) %>%
  summarise(SumGroup = sum(N),
            NoMayorTherapie =sum(CountNoMajorTherapyCoded),
            PercentNo = round((NoMayorTherapie/SumGroup)*100,2),
            AnyMayorTherapie =sum(CountAnyMajorTherapy),
            PercentAny = round((AnyMayorTherapie/SumGroup)*100,2),
            Surgery = sum(CountSurgery), 
            PercentSurgery = round((Surgery/AnyMayorTherapie)*100,2),
            Chemotherapy = sum(CountChemotherapy),
            PercentChemotherapy = round((Chemotherapy/AnyMayorTherapie)*100,2),
            Radiotherapy = sum(CountRadiotherapy),
            PercentRadiotherapy = round((Radiotherapy/AnyMayorTherapie)*100,2),
            Nucleartherapy = sum(CountNuclearMedicineTherapy),
            PercentNuclear = round((Nucleartherapy/AnyMayorTherapie)*100,2)
  )
  


#df_table_TherapyModalities <- data.frame(
 # Period = c(as.character(df_Tabel_TherapyModalities_Main_Filtered[1,1]), as.character(df_Tabel_TherapyModalities_Main_Filtered[1,1]), 
#              as.character(df_Tabel_TherapyModalities_Main_Filtered[5,1]), as.character(df_Tabel_TherapyModalities_Main_Filtered[5,1]),
#              as.character(df_Tabel_TherapyModalities_Main_Filtered[9,1]), as.character(df_Tabel_TherapyModalities_Main_Filtered[9,1])),
#   
#   AgeGroup = c("CIS", "No CIS",
#                "CIS", "No CIS",
#                "CIS", "No CIS"),
#   
#   HIVCancer = c(paste(df_Tabel_TherapyModalities_Main_Filtered[4,4], "(",df_Tabel_TherapyModalities_Main_Filtered[4,6],")"), paste(df_Tabel_TherapyModalities_Main_Filtered[3,4], "(",df_Tabel_TherapyModalities_Main_Filtered[3,6],")"), 
#                 paste(df_Tabel_TherapyModalities_Main_Filtered[8,4], "(",df_Tabel_TherapyModalities_Main_Filtered[8,6],")"), paste(df_Tabel_TherapyModalities_Main_Filtered[7,4], "(",df_Tabel_TherapyModalities_Main_Filtered[7,6],")"), 
#                 paste(df_Tabel_TherapyModalities_Main_Filtered[12,4], "(",df_Tabel_TherapyModalities_Main_Filtered[12,6],")"), paste(df_Tabel_TherapyModalities_Main_Filtered[11,4], "(",df_Tabel_TherapyModalities_Main_Filtered[11,6],")")),
#   
#   
#   Cancer = c(paste(df_Tabel_TherapyModalities_Main_Filtered[2,4], "(",df_Tabel_TherapyModalities_Main_Filtered[2,6],")"), paste(df_Tabel_TherapyModalities_Main_Filtered[1,4], "(",df_Tabel_TherapyModalities_Main_Filtered[1,6],")"), 
#              paste(df_Tabel_TherapyModalities_Main_Filtered[6,4], "(",df_Tabel_TherapyModalities_Main_Filtered[6,6],")"), paste(df_Tabel_TherapyModalities_Main_Filtered[5,4], "(",df_Tabel_TherapyModalities_Main_Filtered[5,6],")"), 
#              paste(df_Tabel_TherapyModalities_Main_Filtered[10,4], "(",df_Tabel_TherapyModalities_Main_Filtered[10,6],")"), paste(df_Tabel_TherapyModalities_Main_Filtered[9,4], "(",df_Tabel_TherapyModalities_Main_Filtered[9,6],")"))
#   
# )

# write.csv2(df_table_TherapyModalities, file.path(path, "TherapyModalities.csv"), row.names = FALSE)



          

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Therapy sequence
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Not relevant for main analysis. 
# Make use of make_long() to get compatible data frame for Sankey diagram
# x: Stage
# node: Node
#df_Output_CancerTherapySequence_Main  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Complications after (Chemo)therapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_TherapyComplications_Main_Filtered <- filter(df_Output_TherapyComplications_Matched_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))

df_Plot_TherapyComplications_OverTime_Main_Filtered <- df_Output_TherapyComplications_Main_Filtered %>%
  group_by(PatientSubgroup, MainCancerDiagnosisYear) %>%
  summarize( N = sum(N),
             CountAnyMajorTherapy_overall = sum(CountAnyMajorTherapy),
             ProportionAnyMajorTherapy_overll = CountAnyMajorTherapy_overall / N,
             CountChemotherapy_overall = sum(CountChemotherapy),
             ProportionChemotherapy_overll = CountChemotherapy_overall / CountAnyMajorTherapy_overall,
             CountNoChemotherapy_overall = sum(CountNoChemotherapy),
             ProportionNoChemotherapy_overll = CountNoChemotherapy_overall / CountAnyMajorTherapy_overall,
             CountComplicationAfterChemotherapy_overall = sum(CountComplicationAfterChemotherapy),
             ProportionComplicationAfterChemotherapy_overall = CountComplicationAfterChemotherapy_overall/CountChemotherapy_overall, 
             CountNoComplicationAfterChemotherapy_overall = sum(CountNoComplicationAfterChemotherapy),
             ProportionNoComplicationAfterChemotherapy_overall = CountNoComplicationAfterChemotherapy_overall/CountChemotherapy_overall, 
             NSite = n_distinct(Site)) 



df_Plot_TherapyComplications_Main_Filtered <- df_Plot_TherapyComplications_OverTime_Main_Filtered %>%
  group_by(PatientSubgroup) %>%
  summarize(N = sum(N),
            CountAnyMajorTherapy = sum(CountAnyMajorTherapy_overall),
            ProportionAnyMajorTherapy = CountAnyMajorTherapy / N,
            CountChemotherapy = sum(CountChemotherapy_overall),
            ProportionChemotherapy = CountChemotherapy / CountAnyMajorTherapy,
            CountNoChemotherapy = sum(CountNoChemotherapy_overall),
            ProportionNoChemotherapy = CountNoChemotherapy / CountAnyMajorTherapy,
            CountComplicationAfterChemotherapy = sum(CountComplicationAfterChemotherapy_overall),
            ProportionComplicationAfterChemotherapy = CountComplicationAfterChemotherapy/CountChemotherapy, 
            CountNoComplicationAfterChemotherapy = sum(CountNoComplicationAfterChemotherapy_overall),
            ProportionNoComplicationAfterChemotherapy = CountNoComplicationAfterChemotherapy/CountChemotherapy
            )

df_Table_TherapyComplications_Main_Filtered  <- df_Output_TherapyComplications_Main_Filtered   %>%
  group_by(Strata, PatientSubgroup) %>%
  summarise(SumGroup = sum(CountChemotherapy),
            SumComplications = sum(CountComplicationAfterChemotherapy),
            PercentageComplications = round((SumComplications/SumGroup)*100,2),
            SumNone = sum(CountNoComplicationAfterChemotherapy),
              PercentageNone = round((SumNone/SumGroup)*100,2))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Last recorded discharge reason
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_LastRecordedDischargeCategory_Main_Filtered <- filter(df_Output_LastRecordedDischargeCategory_Matched_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))
 
df_Plot_LastRecordedDischargeCategory_OverTime_Main_Filtered <- df_Output_LastRecordedDischargeCategory_Main_Filtered %>%
  group_by(PatientSubgroup,LastRecordedDischargeCategory, MainCancerDiagnosisYear) %>%
  summarize(N = sum(N))

df_Plot_LastRecordedDischargeCategory_Main_Filtered <- df_Output_LastRecordedDischargeCategory_Main_Filtered %>%
  group_by(PatientSubgroup,LastRecordedDischargeCategory) %>%
  summarize(N = sum(N)) %>%
  group_by(PatientSubgroup) %>%
  mutate(Proportion = N / sum(N))

df_Plot_LastRecordedDischargeCategory_Main_Filtered$LastRecordedDischargeCategory_sort <- factor(df_Plot_LastRecordedDischargeCategory_Main_Filtered$LastRecordedDischargeCategory, 
                                                                                            levels = c("Home", "Deceased", "Other Hospital", "Rehabilitation or Residential Care", "Hospice Care", "Same Hospital"))

df_Table_LastRecordedDischargeCategory  <- df_Output_LastRecordedDischargeCategory_Main_Filtered  %>%
  group_by(Strata, PatientSubgroup, LastRecordedDischargeCategory) %>%
  summarise(SumDischarge = sum(N)) %>%
  ungroup() %>% group_by(Strata, PatientSubgroup) %>%
  summarise(LastRecordedDischargeCategory = LastRecordedDischargeCategory,
    SumDischarge = SumDischarge,
            SumGroup =  sum(SumDischarge), 
            PercentageDischarge = round((SumDischarge/SumGroup)*100,2))

################################################################################
#
#          Cancer+/HIV+
#
################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer+/HIV+ Most commonly affected organs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#df_Aux_TopographyDetailTop10_Total 

#df_Aux_TopographyDetailTop10_Recent



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HIV and cancer presumed diagnosis order
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_HIVCancerDiagnosisOrder_Main_Filtered <- filter(df_Output_HIVCancerDiagnosisOrder_Matched_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))


df_Table_HIVCancerDiagnosisOrder  <- df_Output_HIVCancerDiagnosisOrder_Main_Filtered   %>%
  group_by(Strata, HIVCancerDiagnosisOrder) %>%
  summarise(N = sum(N)) %>%
  ungroup() %>% group_by(Strata) %>%
  summarise(HIVCancerDiagnosisOrder = HIVCancerDiagnosisOrder, 
            N = N,
            SumGroup = sum(N),
            Percentage = round((N/SumGroup)*100,2))





df_Output_HIVCancerDiagnosisOrder_Main_Filtered <- df_Output_HIVCancerDiagnosisOrder_Main_Filtered %>%
  group_by(HIVCancerDiagnosisOrder, MainCancerDiagnosisYear) %>%
  summarize(N = sum(N))
# Not relevant for main analysis. 
# Make use of make_long() to get compatible data frame for Sankey diagram
# x: Stage
# node: Node
#df_Output_HIVCancerSequence_Main 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AIDS occurrence in HIV-positive cancer patients
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# df_Output_HIVCancerAIDS_Main_Filtered <- filter(df_Output_HIVCancerAIDS_Matched_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))
#   
# df_Table_HHIVCancerAIDS  <- df_Output_HIVCancerAIDS_Main_Filtered   %>%
#   group_by(Strata, AIDSOccurrence) %>%
#   summarise(N = sum(N)) %>%
#   ungroup() %>% group_by(Strata) %>%
#   summarise(AIDSOccurrence = AIDSOccurrence, 
#             N = N,
#             SumGroup = sum(N),
#             Percentage = round((N/SumGroup)*100,3))


################################################################################
#
#          Cancer+/HIV+   vs.   Cancer-/HIV+ 
#
################################################################################


#---------- HIV Status ---------------------------------------------------------

#!!! Add variable in df_ADM_Patients HIV Status...



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AIDS occurrence across all HIV-positive patients
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# df_Output_AIDSOccurrence_Main_Filtered <- filter(df_Output_AIDSOccurrence_Matched_Main, (FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end))
# 
# df_Table_AIDSOccurrence  <- df_Output_AIDSOccurrence_Main_Filtered    %>%
#   group_by(Strata, PatientSubgroup) %>%
#   summarise(AIDS = sum(CountAIDS),
#             NoAIDS = sum(CountNoAIDS),
#             AIDSwithHIV = sum(CountAIDSWithHIVDiagnosis),
#             AIDSafterHIV = sum(CountAIDSAfterHIVDiagnosis)) #%>%
#  ungroup() %>% group_by(Strata) %>%
#  summarise(PatientSubgroup = PatientSubgroup, 
#            AIDS = AIDS,
 #           NoAIDS = NoAIDS,
  #          AIDSwithHIV = AIDSwithHIV,
   #         AIDSafterHIV = AIDSafterHIV,
     #       Percentage = round((N/SumGroup)*100,3))