


################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: AGGREGATE and PROCESS DATA (MAIN)                                 #
#------------------------------------------------------------------------------#
################################################################################
# Merge Site Data (full data)


################################################################################
#
#          Across all primary subgroups
#
################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Postal Code count
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Output_PostalCodeCounts_Cologne <- SiteOutputData_Full$Cologne$df_Output_PostalCodeCounts
df_Output_PostalCodeCounts_Cologne$Site <- "Cologne"
names(df_Output_PostalCodeCounts_Cologne)[3] <- "FirstRelevantAdmissionYear"

df_Output_PostalCodeCounts_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_PostalCodeCounts
df_Output_PostalCodeCounts_Frankfurt$Site <- "Frankfurt"

df_Output_PostalCodeCounts_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_PostalCodeCounts
df_Output_PostalCodeCounts_Freiburg$Site <- "Freiburg"

df_Output_PostalCodeCounts_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_PostalCodeCounts
df_Output_PostalCodeCounts_MunichLMU$Site <- "MunichLMU"

df_Output_PostalCodeCounts_Main <- rbind(df_Output_PostalCodeCounts_Cologne, 
                                         df_Output_PostalCodeCounts_Frankfurt, 
                                         df_Output_PostalCodeCounts_Freiburg, 
                                         df_Output_PostalCodeCounts_MunichLMU)

df_Output_PostalCodeCounts_Main <- df_Output_PostalCodeCounts_Main %>% 
                              mutate(Strata = case_when(FirstRelevantAdmissionYear == 2004 ~ "2004",
                              FirstRelevantAdmissionYear >= 2005 & FirstRelevantAdmissionYear <= 2008 ~ "2005-2008",
                              FirstRelevantAdmissionYear >= 2009 & FirstRelevantAdmissionYear <= 2014 ~ "2009-2014",
                              FirstRelevantAdmissionYear >= 2015 & FirstRelevantAdmissionYear <= 2021 ~ "2015-2021",
                              FirstRelevantAdmissionYear == 2022 ~ "2022",
                              FirstRelevantAdmissionYear == 2023 ~ "2023"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sample size
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_SampleSize_Cologne <- SiteOutputData_Full$Cologne$df_Output_SampleSize
df_SampleSize_Cologne$Site <- "Cologne"
names(df_SampleSize_Cologne)[2] <- "FirstRelevantAdmissionYear"

df_SampleSize_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_SampleSize
df_SampleSize_Frankfurt$Site <- "Frankfurt"

df_SampleSize_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_SampleSize
df_SampleSize_Freiburg$Site <- "Freiburg"

df_SampleSize_MunichLMU<- SiteOutputData_Full$MunichLMU$df_Output_SampleSize
df_SampleSize_MunichLMU$Site <- "MunichLMU"

df_Output_SampleSize_Main <- rbind(df_SampleSize_Cologne, df_SampleSize_Frankfurt, 
                                   df_SampleSize_Freiburg, df_SampleSize_MunichLMU)

df_Output_SampleSize_Main  <- df_Output_SampleSize_Main  %>% 
  mutate(Strata = case_when(FirstRelevantAdmissionYear == 2004 ~ "2004",
                            FirstRelevantAdmissionYear >= 2005 & FirstRelevantAdmissionYear <= 2008 ~ "2005-2008",
                            FirstRelevantAdmissionYear >= 2009 & FirstRelevantAdmissionYear <= 2014 ~ "2009-2014",
                            FirstRelevantAdmissionYear >= 2015 & FirstRelevantAdmissionYear <= 2021 ~ "2015-2021",
                            FirstRelevantAdmissionYear == 2022 ~ "2022",
                            FirstRelevantAdmissionYear == 2023 ~ "2023"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sex distribution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Sex_Cologne <- SiteOutputData_Full$Cologne$df_Output_Sex
df_Sex_Cologne$Site <- "Cologne"
names(df_Sex_Cologne)[2] <- "FirstRelevantAdmissionYear"
df_Sex_Cologne <-  df_Sex_Cologne %>% 
                    mutate(Sex = case_when(Sex == "w" ~ "F", Sex == "m" ~ "M"))

df_Sex_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_Sex
df_Sex_Frankfurt$Site <- "Frankfurt"

df_Sex_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_Sex
df_Sex_Freiburg$Site <- "Freiburg"

df_Sex_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_Sex
df_Sex_MunichLMU$Site <- "MunichLMU"

df_Output_Sex_Main <-  rbind(df_Sex_Cologne, df_Sex_Frankfurt, df_Sex_Freiburg, 
                             df_Sex_MunichLMU)

df_Output_Sex_Main  <- df_Output_Sex_Main   %>% 
  mutate(Strata = case_when(FirstRelevantAdmissionYear == 2004 ~ "2004",
                            FirstRelevantAdmissionYear >= 2005 & FirstRelevantAdmissionYear <= 2008 ~ "2005-2008",
                            FirstRelevantAdmissionYear >= 2009 & FirstRelevantAdmissionYear <= 2014 ~ "2009-2014",
                            FirstRelevantAdmissionYear >= 2015 & FirstRelevantAdmissionYear <= 2021 ~ "2015-2021",
                            FirstRelevantAdmissionYear == 2022 ~ "2022",
                            FirstRelevantAdmissionYear == 2023 ~ "2023"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Age
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Age (at first main admission) distribution over time
df_Age_Cologne <- SiteOutputData_Full$Cologne$df_Output_Age
df_Age_Cologne$Site <- "Cologne"
names(df_Age_Cologne)[2] <- "FirstRelevantAdmissionYear"

df_Age_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_Age
df_Age_Frankfurt$Site <- "Frankfurt"

df_Age_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_Age
df_Age_Freiburg$Site <- "Freiburg"

df_Age_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_Age
df_Age_MunichLMU$Site <- "MunichLMU"

df_Output_Age_Main <- rbind(df_Age_Cologne, df_Age_Frankfurt, 
                            df_Age_Freiburg, df_Age_MunichLMU)

df_Output_Age_Main   <- df_Output_Age_Main   %>% 
  mutate(Strata = case_when(FirstRelevantAdmissionYear == 2004 ~ "2004",
                            FirstRelevantAdmissionYear >= 2005 & FirstRelevantAdmissionYear <= 2008 ~ "2005-2008",
                            FirstRelevantAdmissionYear >= 2009 & FirstRelevantAdmissionYear <= 2014 ~ "2009-2014",
                            FirstRelevantAdmissionYear >= 2015 & FirstRelevantAdmissionYear <= 2021 ~ "2015-2021",
                            FirstRelevantAdmissionYear == 2022 ~ "2022",
                            FirstRelevantAdmissionYear == 2023 ~ "2023"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Case count per patient
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Case count over time
df_CaseCount_Cologne <- SiteOutputData_Full$Cologne$df_Output_CaseCount
df_CaseCount_Cologne$Site <- "Cologne"
names(df_CaseCount_Cologne)[2] <- "FirstRelevantAdmissionYear"

df_CaseCount_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_CaseCount
df_CaseCount_Frankfurt$Site <- "Frankfurt"

df_CaseCount_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_CaseCount
df_CaseCount_Freiburg$Site <- "Freiburg"

df_CaseCount_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_CaseCount
df_CaseCount_MunichLMU$Site <- "MunichLMU"

df_Output_CaseCount_Main  <- rbind(df_CaseCount_Cologne, df_CaseCount_Frankfurt, df_CaseCount_Freiburg, df_CaseCount_MunichLMU)


df_Output_CaseCount_Main   <- df_Output_CaseCount_Main   %>% 
  mutate(Strata = case_when(FirstRelevantAdmissionYear == 2004 ~ "2004",
                            FirstRelevantAdmissionYear >= 2005 & FirstRelevantAdmissionYear <= 2008 ~ "2005-2008",
                            FirstRelevantAdmissionYear >= 2009 & FirstRelevantAdmissionYear <= 2014 ~ "2009-2014",
                            FirstRelevantAdmissionYear >= 2015 & FirstRelevantAdmissionYear <= 2021 ~ "2015-2021",
                            FirstRelevantAdmissionYear == 2022 ~ "2022",
                            FirstRelevantAdmissionYear == 2023 ~ "2023"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mean length of stay
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_LengOfStay_Cologne <- SiteOutputData_Full$Cologne$df_Output_MeanLengthOfStay
df_LengOfStay_Cologne$Site <- "Cologne"
names(df_LengOfStay_Cologne)[2] <- "FirstRelevantAdmissionYear"

df_LengOfStay_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_MeanLengthOfStay
df_LengOfStay_Frankfurt$Site <- "Frankfurt"

df_LengOfStay_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_MeanLengthOfStay
df_LengOfStay_Freiburg$Site <- "Freiburg"

df_LengOfStay_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_MeanLengthOfStay
df_LengOfStay_MunichLMU$Site <- "MunichLMU"

df_Output_MeanLengthOfStay_Main  <- rbind(df_LengOfStay_Cologne, 
                                          df_LengOfStay_Frankfurt, 
                                          df_LengOfStay_Freiburg, 
                                          df_LengOfStay_MunichLMU)

df_Output_MeanLengthOfStay_Main   <- df_Output_MeanLengthOfStay_Main   %>% 
  mutate(Strata = case_when(FirstRelevantAdmissionYear == 2004 ~ "2004",
                            FirstRelevantAdmissionYear >= 2005 & FirstRelevantAdmissionYear <= 2008 ~ "2005-2008",
                            FirstRelevantAdmissionYear >= 2009 & FirstRelevantAdmissionYear <= 2014 ~ "2009-2014",
                            FirstRelevantAdmissionYear >= 2015 & FirstRelevantAdmissionYear <= 2021 ~ "2015-2021",
                            FirstRelevantAdmissionYear == 2022 ~ "2022",
                            FirstRelevantAdmissionYear == 2023 ~ "2023"))


################################################################################
#
#          Cancer+/HIV+
#
################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HIV and cancer presumed diagnosis order
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Output_HIVCancerDiagnosisOrder_Cologne <- SiteOutputData_Full$Cologne$df_Output_HIVCancerDiagnosisOrder
df_Output_HIVCancerDiagnosisOrder_Cologne$Site <- "Cologne"
names(df_Output_HIVCancerDiagnosisOrder_Cologne)[3] <- "MainCancerDiagnosisYear"

df_Output_HIVCancerDiagnosisOrder_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_HIVCancerDiagnosisOrder
df_Output_HIVCancerDiagnosisOrder_Frankfurt$Site <- "Frankfurt"

df_Output_HIVCancerDiagnosisOrder_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_HIVCancerDiagnosisOrder
df_Output_HIVCancerDiagnosisOrder_Freiburg$Site <- "Freiburg"

df_Output_HIVCancerDiagnosisOrder_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_HIVCancerDiagnosisOrder
df_Output_HIVCancerDiagnosisOrder_MunichLMU$Site <- "MunichLMU"

df_Output_HIVCancerDiagnosisOrder_Main <- rbind(df_Output_HIVCancerDiagnosisOrder_Cologne,
                                                      df_Output_HIVCancerDiagnosisOrder_Frankfurt,
                                                      df_Output_HIVCancerDiagnosisOrder_Freiburg,
                                                      df_Output_HIVCancerDiagnosisOrder_MunichLMU)

###############################################################################
contain <- c("df_Output_PostalCodeCounts_Main", "df_Output_SampleSize_Main", 
             "df_Output_Sex_Main", "df_Output_Age_Main", "df_Output_CaseCount_Main ",
             "df_Output_MeanLengthOfStay_Main",  "SiteObjectAvailability",
             "SiteOutputData_Full", "SiteOutputData_Matched"
)

del <- ls()[-which(ls() %in% contain)]
rm(list = del)
