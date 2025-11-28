################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: AGGREGATE and PROCESS DATA (MAIN)                                 #
#------------------------------------------------------------------------------#
################################################################################
# Merge Site Data (matched data)
# Start aggregation of the matched data from line 307. 
# The data of the matched patients is not available before this.!

################################################################################
#
#          Cancer+/HIV+   vs.   Cancer+/HIV- 
#
################################################################################
# Aggregation of the matched data starts here!

#!!!-!!!-!!! Plot describing completeness of data (Sankey diagram with nodes main diagnosis - treatment - follow up?)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Age at presumed cancer diagnosis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Age (at cancer diagnosis) distribution over time
#df_AgeAtCancerDiagnosis_Cologne <- SiteOutputData_Matched$Cologne$df_Output_AgeAtCancerDiagnosis
#df_AgeAtCancerDiagnosis_Cologne$Site <- "Cologne"
#names(df_AgeAtCancerDiagnosis_Cologne)[2] <- "MainCancerDiagnosisYear"

df_AgeAtCancerDiagnosis_Frankfurt <- SiteOutputData_Matched$Frankfurt$df_Output_AgeAtCancerDiagnosis
df_AgeAtCancerDiagnosis_Frankfurt$Site <- "Frankfurt"

df_AgeAtCancerDiagnosis_Freiburg <- SiteOutputData_Matched$Freiburg$df_Output_AgeAtCancerDiagnosis
df_AgeAtCancerDiagnosis_Freiburg$Site <- "Freiburg"

df_AgeAtCancerDiagnosis_MunichLMU <- SiteOutputData_Matched$MunichLMU$df_Output_AgeAtCancerDiagnosis
df_AgeAtCancerDiagnosis_MunichLMU$Site <- "MunichLMU"

df_Output_AgeAtCancerDiagnosis_Matched_Main  <- rbind(df_AgeAtCancerDiagnosis_Frankfurt, 
                                              df_AgeAtCancerDiagnosis_Freiburg, 
                                              df_AgeAtCancerDiagnosis_MunichLMU)

df_Output_AgeAtCancerDiagnosis_Matched_Main   <- df_Output_AgeAtCancerDiagnosis_Matched_Main   %>% 
  mutate(Strata = case_when(MainCancerDiagnosisYear == 2004 ~ "2004",
                            MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005-2008",
                            MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009-2014",
                            MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2021 ~ "2015-2021",
                            MainCancerDiagnosisYear == 2022 ~ "2022",
                            MainCancerDiagnosisYear == 2023 ~ "2023"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Count of distinct documented "real" cancer codes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Not avible at Cologne

df_Output_DistinctCodeCountCancer_Frankfurt <- SiteOutputData_Matched$Frankfurt$df_Output_DistinctCodeCountCancer
df_Output_DistinctCodeCountCancer_Frankfurt$Site <- "Frankfurt"

df_Output_DistinctCodeCountCancer_Freiburg <- SiteOutputData_Matched$Freiburg$df_Output_DistinctCodeCountCancer
df_Output_DistinctCodeCountCancer_Freiburg$Site <- "Freiburg"

df_Output_DistinctCodeCountCancer_MunichLMU <- SiteOutputData_Matched$MunichLMU$df_Output_DistinctCodeCountCancer
df_Output_DistinctCodeCountCancer_MunichLMU$Site <- "MunichLMU"

df_Output_DistinctCodeCountCancer_Matched_Main <- rbind(df_Output_DistinctCodeCountCancer_Frankfurt, 
                                                df_Output_DistinctCodeCountCancer_Freiburg, 
                                                df_Output_DistinctCodeCountCancer_MunichLMU)

df_Output_DistinctCodeCountCancer_Matched_Main   <- df_Output_DistinctCodeCountCancer_Matched_Main   %>% 
  mutate(Strata = case_when(FirstRelevantAdmissionYear == 2004 ~ "2004",
                            FirstRelevantAdmissionYear >= 2005 & FirstRelevantAdmissionYear <= 2008 ~ "2005-2008",
                            FirstRelevantAdmissionYear >= 2009 & FirstRelevantAdmissionYear <= 2014 ~ "2009-2014",
                            FirstRelevantAdmissionYear >= 2015 & FirstRelevantAdmissionYear <= 2021 ~ "2015-2021",
                            FirstRelevantAdmissionYear == 2022 ~ "2022",
                            FirstRelevantAdmissionYear == 2023 ~ "2023"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer grouping: Carcinoma in situ
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#df_Output_CIS_Cologne <- SiteOutputData_Matched$Cologne$df_Output_CIS
#df_Output_CIS_Cologne$Site <- "Cologne"
#names(df_Output_CIS_Cologne)[2] <- "MainCancerIsCarcinomaInSitu"
#df_Output_CIS_Cologne$MainCancerIsCarcinomaInSitu <- as.character(df_Output_CIS_Cologne$MainCancerIsCarcinomaInSitu)
#names(df_Output_CIS_Cologne)[3] <- "MainCancerDiagnosisYear"

df_Output_CIS_Frankfurt <- SiteOutputData_Matched$Frankfurt$df_Output_CIS
df_Output_CIS_Frankfurt$Site <- "Frankfurt"

df_Output_CIS_Freiburg <- SiteOutputData_Matched$Freiburg$df_Output_CIS
df_Output_CIS_Freiburg$Site <- "Freiburg"

df_Output_CIS_MunichLMU <- SiteOutputData_Matched$MunichLMU$df_Output_CIS
df_Output_CIS_MunichLMU$Site <- "MunichLMU"

df_Output_CIS_Matched_Main <- rbind(df_Output_CIS_Frankfurt,
                            df_Output_CIS_Freiburg,
                            df_Output_CIS_MunichLMU)

df_Output_CIS_Matched_Main   <- df_Output_CIS_Matched_Main %>% 
  mutate(Strata = case_when(MainCancerDiagnosisYear == 2004 ~ "2004",
                            MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005-2008",
                            MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009-2014",
                            MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2021 ~ "2015-2021",
                            MainCancerDiagnosisYear == 2022 ~ "2022",
                            MainCancerDiagnosisYear == 2023 ~ "2023"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: Topography by Organ
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data stratified by year of cancer diagnosis
#df_Output_MainCancerTopographyDetail_OverTime_Cologne <- SiteOutputData_Matched$Cologne$df_Output_CancerTopographyDetail_OverTime
#df_Output_MainCancerTopographyDetail_OverTime_Cologne$Site <- "Cologne"
#names(df_Output_MainCancerTopographyDetail_OverTime_Cologne)[2] <-"MainCancerTopographyDetail"
#names(df_Output_MainCancerTopographyDetail_OverTime_Cologne)[3] <- "MainCancerDiagnosisYear"

df_Output_MainCancerTopographyDetail_OverTime_Frankfurt <- SiteOutputData_Matched$Frankfurt$df_Output_MainCancerTopographyDetail_OverTime
df_Output_MainCancerTopographyDetail_OverTime_Frankfurt$Site <- "Frankfurt"

df_Output_MainCancerTopographyDetail_OverTime_Freiburg <- SiteOutputData_Matched$Freiburg$df_Output_MainCancerTopographyDetail_OverTime
df_Output_MainCancerTopographyDetail_OverTime_Freiburg$Site <- "Freiburg"

df_Output_MainCancerTopographyDetail_OverTime_MunichLMU <- SiteOutputData_Matched$MunichLMU$df_Output_MainCancerTopographyDetail_OverTime
df_Output_MainCancerTopographyDetail_OverTime_MunichLMU$Site <- "MunichLMU"

df_Output_MainCancerTopographyDetail_OverTime_Matched_Main <- rbind(df_Output_MainCancerTopographyDetail_OverTime_Frankfurt,
                                                            df_Output_MainCancerTopographyDetail_OverTime_Freiburg,
                                                            df_Output_MainCancerTopographyDetail_OverTime_MunichLMU)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: Topography by ICD Grouping
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data stratified by year of cancer diagnosis
#df_Output_MainCancerTopographyGroup_OverTime_Cologne <- SiteOutputData_Matched$Cologne$df_Output_CancerTopographyGroup_OverTime
#df_Output_MainCancerTopographyGroup_OverTime_Cologne$Site <- "Cologne"
#names(df_Output_MainCancerTopographyGroup_OverTime_Cologne)[2] <-"MainCancerTopographyGroup"
#names(df_Output_MainCancerTopographyGroup_OverTime_Cologne)[3] <- "MainCancerDiagnosisYear"

df_Output_MainCancerTopographyGroup_OverTime_Frankfurt <- SiteOutputData_Matched$Frankfurt$df_Output_MainCancerTopographyGroup_OverTime
df_Output_MainCancerTopographyGroup_OverTime_Frankfurt$Site <- "Frankfurt"

df_Output_MainCancerTopographyGroup_OverTime_Freiburg <- SiteOutputData_Matched$Freiburg$df_Output_MainCancerTopographyGroup_OverTime
df_Output_MainCancerTopographyGroup_OverTime_Freiburg$Site <- "Freiburg"

df_Output_MainCancerTopographyGroup_OverTime_MunichLMU <- SiteOutputData_Matched$MunichLMU$df_Output_MainCancerTopographyGroup_OverTime
df_Output_MainCancerTopographyGroup_OverTime_MunichLMU$Site <- "MunichLMU"

df_Output_MainCancerTopographyGroup_OverTime_Matched_Main <- rbind(df_Output_MainCancerTopographyGroup_OverTime_Frankfurt,
                                                           df_Output_MainCancerTopographyGroup_OverTime_Freiburg,
                                                           df_Output_MainCancerTopographyGroup_OverTime_MunichLMU)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: By entity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data stratified by year of cancer diagnosis
#df_Output_CancerEntities_OverTime_Cologne <- SiteOutputData_Matched$Cologne$df_Output_CancerEntities_OverTime
#df_Output_CancerEntities_OverTime_Cologne$Site <- "Cologne"
#names(df_Output_CancerEntities_OverTime_Cologne)[2] <-"MainCancerCode"
#names(df_Output_CancerEntities_OverTime_Cologne)[3] <- "MainCancerDiagnosisYear"

df_Output_CancerEntities_OverTime_Frankfurt <- SiteOutputData_Matched$Frankfurt$df_Output_CancerEntities_OverTime
df_Output_CancerEntities_OverTime_Frankfurt$Site <- "Frankfurt"

df_Output_CancerEntities_OverTime_Freiburg <- SiteOutputData_Matched$Freiburg$df_Output_CancerEntities_OverTime
df_Output_CancerEntities_OverTime_Freiburg$Site <- "Freiburg"

df_Output_CancerEntities_OverTime_MunichLMU <- SiteOutputData_Matched$MunichLMU$df_Output_CancerEntities_OverTime
df_Output_CancerEntities_OverTime_MunichLMU$Site <- "MunichLMU"

df_Output_CancerEntities_OverTime_Matched_Main <- rbind(df_Output_CancerEntities_OverTime_Frankfurt,
                                                df_Output_CancerEntities_OverTime_Freiburg,
                                                df_Output_CancerEntities_OverTime_MunichLMU)


df_Output_CancerEntities_OverTime_Matched_Main   <- df_Output_CancerEntities_OverTime_Matched_Main %>% 
  mutate(Strata = case_when(MainCancerDiagnosisYear == 2004 ~ "2004",
                            MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005-2008",
                            MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009-2014",
                            MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2021 ~ "2015-2021",
                            MainCancerDiagnosisYear == 2022 ~ "2022",
                            MainCancerDiagnosisYear == 2023 ~ "2023"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Metastasis occurrence
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#df_Output_MetastasisOccurrence_Cologne <- SiteOutputData_Matched$Cologne$df_Output_MetastasisOccurrence
#df_Output_MetastasisOccurrence_Cologne$Site <- "Cologne"
#names(df_Output_MetastasisOccurrence_Cologne)[2] <- "MainCancerDiagnosisYear"

df_Output_MetastasisOccurrence_Frankfurt <- SiteOutputData_Matched$Frankfurt$df_Output_MetastasisOccurrence
df_Output_MetastasisOccurrence_Frankfurt$Site <- "Frankfurt"

df_Output_MetastasisOccurrence_Freiburg <- SiteOutputData_Matched$Freiburg$df_Output_MetastasisOccurrence
df_Output_MetastasisOccurrence_Freiburg$Site <- "Freiburg"

df_Output_MetastasisOccurrence_MunichLMU <- SiteOutputData_Matched$MunichLMU$df_Output_MetastasisOccurrence
df_Output_MetastasisOccurrence_MunichLMU$Site <- "MunichLMU"

df_Output_MetastasisOccurrence_Matched_Main <- rbind(df_Output_MetastasisOccurrence_Frankfurt,
                                             df_Output_MetastasisOccurrence_Freiburg,
                                             df_Output_MetastasisOccurrence_MunichLMU)

df_Output_MetastasisOccurrence_Matched_Main  <- df_Output_MetastasisOccurrence_Matched_Main %>% 
  mutate(Strata = case_when(MainCancerDiagnosisYear == 2004 ~ "2004",
                            MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005-2008",
                            MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009-2014",
                            MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2021 ~ "2015-2021",
                            MainCancerDiagnosisYear == 2022 ~ "2022",
                            MainCancerDiagnosisYear == 2023 ~ "2023"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Therapy modalities
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#df_Output_TherapyModalities_Cologne <- SiteOutputData_Matched$Cologne$df_Output_TherapyModalities
#df_Output_TherapyModalities_Cologne$Site <- "Cologne"
#names(df_Output_TherapyModalities_Cologne)[2] <- "MainCancerDiagnosisYear"
#names(df_Output_TherapyModalities_Cologne)[10] <- "CountImmunotherapy"
#names(df_Output_TherapyModalities_Cologne)[11] <- "ProportionImmunotherapy"

df_Output_TherapyModalities_Frankfurt <- SiteOutputData_Matched$Frankfurt$df_Output_TherapyModalities
df_Output_TherapyModalities_Frankfurt$Site <- "Frankfurt"

df_Output_TherapyModalities_Freiburg <- SiteOutputData_Matched$Freiburg$df_Output_TherapyModalities
df_Output_TherapyModalities_Freiburg$Site <- "Freiburg"

df_Output_TherapyModalities_MunichLMU <- SiteOutputData_Matched$MunichLMU$df_Output_TherapyModalities
df_Output_TherapyModalities_MunichLMU$Site <- "MunichLMU"

df_Output_TherapyModalities_Matched_Main <- rbind(df_Output_TherapyModalities_Frankfurt,
                                          df_Output_TherapyModalities_Freiburg,
                                          df_Output_TherapyModalities_MunichLMU)

df_Output_TherapyModalities_Matched_Main <-df_Output_TherapyModalities_Matched_Main %>% 
  mutate(Strata = case_when(MainCancerDiagnosisYear == 2004 ~ "2004",
                            MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005-2008",
                            MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009-2014",
                            MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2021 ~ "2015-2021",
                            MainCancerDiagnosisYear == 2022 ~ "2022",
                            MainCancerDiagnosisYear == 2023 ~ "2023"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Complications after (Chemo)therapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#df_Output_TherapyComplications_Cologne <- SiteOutputData_Matched$Cologne$df_Output_TherapyComplications
#df_Output_TherapyComplications_Cologne$Site <- "Cologne"
#names(df_Output_TherapyComplications_Cologne)[2] <- "MainCancerDiagnosisYear"

df_Output_TherapyComplications_Frankfurt <- SiteOutputData_Matched$Frankfurt$df_Output_TherapyComplications
df_Output_TherapyComplications_Frankfurt$Site <- "Frankfurt"

df_Output_TherapyComplications_Freiburg <- SiteOutputData_Matched$Freiburg$df_Output_TherapyComplications
df_Output_TherapyComplications_Freiburg$Site <- "Freiburg"

df_Output_TherapyComplications_MunichLMU <- SiteOutputData_Matched$MunichLMU$df_Output_TherapyComplications
df_Output_TherapyComplications_MunichLMU$Site <- "MunichLMU"

df_Output_TherapyComplications_Matched_Main <- rbind(df_Output_TherapyComplications_Frankfurt,
                                             df_Output_TherapyComplications_Freiburg,
                                             df_Output_TherapyComplications_MunichLMU)

df_Output_TherapyComplications_Matched_Main  <- df_Output_TherapyComplications_Matched_Main   %>% 
  mutate(Strata = case_when(MainCancerDiagnosisYear == 2004 ~ "2004",
                            MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005-2008",
                            MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009-2014",
                            MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2021 ~ "2015-2021",
                            MainCancerDiagnosisYear == 2022 ~ "2022",
                            MainCancerDiagnosisYear == 2023 ~ "2023"))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Last recorded discharge reason
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#df_Output_LastRecordedDischargeCategory_Cologne <- SiteOutputData_Matched$Cologne$df_Output_LastRecordedDischargeReason
#df_Output_LastRecordedDischargeCategory_Cologne$Site <- "Cologne"
#names(df_Output_LastRecordedDischargeCategory_Cologne)[2] <- "LastRecordedDischargeCategory"
#names(df_Output_LastRecordedDischargeCategory_Cologne)[3] <- "MainCancerDiagnosisYear"

df_Output_LastRecordedDischargeCategory_Frankfurt <- SiteOutputData_Matched$Frankfurt$df_Output_LastRecordedDischargeCategory
df_Output_LastRecordedDischargeCategory_Frankfurt$Site <- "Frankfurt"

df_Output_LastRecordedDischargeCategory_Freiburg <- SiteOutputData_Matched$Freiburg$df_Output_LastRecordedDischargeCategory
df_Output_LastRecordedDischargeCategory_Freiburg$Site <- "Freiburg"

df_Output_LastRecordedDischargeCategory_MunichLMU <- SiteOutputData_Matched$MunichLMU$df_Output_LastRecordedDischargeCategory
df_Output_LastRecordedDischargeCategory_MunichLMU$Site <- "MunichLMU"

df_Output_LastRecordedDischargeCategory_Matched_Main <- rbind(df_Output_LastRecordedDischargeCategory_Frankfurt,
                                                      df_Output_LastRecordedDischargeCategory_Freiburg,
                                                      df_Output_LastRecordedDischargeCategory_MunichLMU)

df_Output_LastRecordedDischargeCategory_Matched_Main <- df_Output_LastRecordedDischargeCategory_Matched_Main   %>% 
  mutate(Strata = case_when(MainCancerDiagnosisYear == 2004 ~ "2004",
                            MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005-2008",
                            MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009-2014",
                            MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2021 ~ "2015-2021",
                            MainCancerDiagnosisYear == 2022 ~ "2022",
                            MainCancerDiagnosisYear == 2023 ~ "2023"))

################################################################################
#
#          Cancer+/HIV+
#
################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HIV and cancer presumed diagnosis order
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#df_Output_HIVCancerDiagnosisOrder_Cologne <- SiteOutputData_Matched$Cologne$df_Output_HIVCancerDiagnosisOrder
#df_Output_HIVCancerDiagnosisOrder_Cologne$Site <- "Cologne"
#names(df_Output_HIVCancerDiagnosisOrder_Cologne)[3] <- "MainCancerDiagnosisYear"

df_Output_HIVCancerDiagnosisOrder_Frankfurt <- SiteOutputData_Matched$Frankfurt$df_Output_HIVCancerDiagnosisOrder
df_Output_HIVCancerDiagnosisOrder_Frankfurt$Site <- "Frankfurt"

df_Output_HIVCancerDiagnosisOrder_Freiburg <- SiteOutputData_Matched$Freiburg$df_Output_HIVCancerDiagnosisOrder
df_Output_HIVCancerDiagnosisOrder_Freiburg$Site <- "Freiburg"

df_Output_HIVCancerDiagnosisOrder_MunichLMU <- SiteOutputData_Matched$MunichLMU$df_Output_HIVCancerDiagnosisOrder
df_Output_HIVCancerDiagnosisOrder_MunichLMU$Site <- "MunichLMU"

df_Output_HIVCancerDiagnosisOrder_Matched_Main <- rbind(df_Output_HIVCancerDiagnosisOrder_Frankfurt,
                                                df_Output_HIVCancerDiagnosisOrder_Freiburg,
                                                df_Output_HIVCancerDiagnosisOrder_MunichLMU)

df_Output_HIVCancerDiagnosisOrder_Matched_Main <- df_Output_HIVCancerDiagnosisOrder_Matched_Main   %>% 
  mutate(Strata = case_when(MainCancerDiagnosisYear == 2004 ~ "2004",
                            MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005-2008",
                            MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009-2014",
                            MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2021 ~ "2015-2021",
                            MainCancerDiagnosisYear == 2022 ~ "2022",
                            MainCancerDiagnosisYear == 2023 ~ "2023"))

################################################################################
# contain <- c("df_Output_PostalCodeCounts_Main", "df_Output_SampleSize_Main", 
#              "df_Output_Sex_Main", "df_Output_Age_Main", "df_Output_CaseCount_Main ",
#              "df_Output_MeanLengthOfStay_Main", "df_Output_AgeAtCancerDiagnosis_Matched_Main ",
#              "df_Output_DistinctCodeCountCancer_Matched_Main", "df_Output_CIS_Matched_Main",
#              "df_Output_MainCancerTopographyDetail_OverTime_Matched_Main", 
#              "df_Output_MainCancerTopographyGroup_OverTime_Matched_Main",
#              "df_Output_CancerEntities_OverTime_Matched_Main", "df_Output_MetastasisOccurrence_Matched_Main",
#              "df_Output_TherapyModalities_Matched_Main", "df_Output_TherapyComplications_Matched_Main",
#              "df_Output_LastRecordedDischargeCategory_Matched_Main", "df_Output_HIVCancerDiagnosisOrder_Matched_Main", 
#              "SiteObjectAvailability", "SiteOutputData_Full", "SiteOutputData_Matched"
# )
# 
# del <- ls()[-which(ls() %in% contain)]
# rm(list = del)
