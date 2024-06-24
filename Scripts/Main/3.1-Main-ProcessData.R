


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

# Age distribution summary statistics
df_Age_Summary_Cologne <- SiteOutputData_Full$Cologne$df_Output_Age_Summary
df_Age_Summary_Cologne$Site <- "Cologne"

df_Age_Summary_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_Age_Summary
df_Age_Summary_Frankfurt$Site <- "Frankfurt"

df_Age_Summary_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_Age_Summary
df_Age_Summary_Freiburg$Site <- "Freiburg"

df_Age_Summary_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_Age_Summary
df_Age_Summary_MunichLMU$Site <- "MunichLMU"

df_Output_Age_Summary_Main <-  rbind(df_Age_Summary_Cologne, df_Age_Summary_Frankfurt, 
                                     df_Age_Summary_Freiburg, df_Age_Summary_MunichLMU)
# Age distribution quantiles
df_Age_Quantiles_Cologne <- SiteOutputData_Full$Cologne$df_Output_Age_Quantiles
df_Age_Quantiles_Cologne$Site <- "Cologne"

df_Age_Quantiles_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_Age_Quantiles
df_Age_Quantiles_Frankfurt$Site <- "Frankfurt"

df_Age_Quantiles_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_Age_Quantiles
df_Age_Quantiles_Freiburg$Site <- "Freiburg"

df_Age_Quantiles_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_Age_Quantiles
df_Age_Quantiles_MunichLMU$Site <- "MunichLMU"


df_Output_Age_Quantiles_Main <-  rbind(df_Age_Quantiles_Cologne, 
                                       df_Age_Quantiles_Frankfurt, 
                                       df_Age_Quantiles_Freiburg, 
                                       df_Age_Quantiles_MunichLMU)



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


# Case count summary statistics
df_CaseCount_Summary_Cologne <- SiteOutputData_Full$Cologne$df_Output_CaseCount_Summary
df_CaseCount_Summary_Cologne$Site <- "Cologne"

df_CaseCount_Summary_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_CaseCount_Summary
df_CaseCount_Summary_Frankfurt$Site <- "Frankfurt"

df_CaseCount_Summary_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_CaseCount_Summary
df_CaseCount_Summary_Freiburg$Site <- "Freiburg"

df_CaseCount_Summary_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_CaseCount_Summary
df_CaseCount_Summary_MunichLMU$Site <- "MunichLMU"

df_Output_CaseCount_Summary_Main <- rbind(df_CaseCount_Summary_Cologne, 
                                          df_CaseCount_Summary_Frankfurt, 
                                          df_CaseCount_Summary_Freiburg,
                                          df_CaseCount_Summary_MunichLMU)


# Case count quantiles
df_CaseCount_Quantiles_Cologne <- SiteOutputData_Full$Cologne$df_Output_CaseCount_Quantiles
df_CaseCount_Quantiles_Cologne$Site <- "Cologne"

df_CaseCount_Quantiles_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_CaseCount_Quantiles
df_CaseCount_Quantiles_Frankfurt$Site <- "Frankfurt"

df_CaseCount_Quantiles_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_CaseCount_Quantiles
df_CaseCount_Quantiles_Freiburg$Site <- "Freiburg"

df_CaseCount_Quantiles_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_CaseCount_Quantiles
df_CaseCount_Quantiles_MunichLMU$Site <- "MunichLMU"

df_Output_CaseCount_Quantiles_Main <- rbind(df_CaseCount_Quantiles_Cologne, 
                                          df_CaseCount_Quantiles_Frankfurt, 
                                          df_CaseCount_Quantiles_Freiburg,
                                          df_CaseCount_Quantiles_MunichLMU)


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


# Mean length of stay summary statistics
df_LengOfStay_Summary_Cologne <- SiteOutputData_Full$Cologne$df_Output_MeanLengthOfStay_Summary
df_LengOfStay_Summary_Cologne$Site <- "Cologne"

df_LengOfStay_Summary_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_MeanLengthOfStay_Summary
df_LengOfStay_Summary_Frankfurt$Site <- "Frankfurt"

df_LengOfStay_Summary_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_MeanLengthOfStay_Summary
df_LengOfStay_Summary_Freiburg$Site <- "Freiburg"

df_LengOfStay_Summary_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_MeanLengthOfStay_Summary
df_LengOfStay_Summary_MunichLMU$Site <- "MunichLMU"

df_Output_MeanLengthOfStay_Summary_Main <- rbind(df_LengOfStay_Summary_Cologne, 
                                                 df_LengOfStay_Summary_Frankfurt, 
                                                 df_LengOfStay_Summary_Freiburg, 
                                                 df_LengOfStay_Summary_MunichLMU) 


# Mean length of stay quantiles
df_LengOfStay_Quantiles_Cologne <- SiteOutputData_Full$Cologne$df_Output_MeanLengthOfStay_Quantiles
df_LengOfStay_Quantiles_Cologne$Site <- "Cologne"

df_LengOfStay_Quantiles_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_MeanLengthOfStay_Quantiles
df_LengOfStay_Quantiles_Frankfurt$Site <- "Frankfurt"

df_LengOfStay_Quantiles_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_MeanLengthOfStay_Quantiles
df_LengOfStay_Quantiles_Freiburg$Site <- "Freiburg"

df_LengOfStay_Quantiles_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_MeanLengthOfStay_Quantiles
df_LengOfStay_Quantiles_MunichLMU$Site <- "MunichLMU"

df_Output_MeanLengthOfStay_Quantiles_Main <- rbind(df_LengOfStay_Quantiles_Cologne, 
                                                 df_LengOfStay_Quantiles_Frankfurt, 
                                                 df_LengOfStay_Quantiles_Freiburg, 
                                                 df_LengOfStay_Quantiles_MunichLMU) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Number of ICU transfers relative to number of admissions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Only Frankfurt und Freiburg availble
# ICU Transfer summary statistic
df_ICUTransfersRelative_Summary_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_ICUTransfersRelative_Summary
df_ICUTransfersRelative_Summary_Frankfurt$Site <- "Frankfurt"

df_ICUTransfersRelative_Summary_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_ICUTransfersRelative_Summary
df_ICUTransfersRelative_Summary_Freiburg$Site <- "Freiburg"

df_Output_ICUTransfersRelative_Summary_Main <- rbind(df_ICUTransfersRelative_Summary_Frankfurt, 
                                                 df_ICUTransfersRelative_Summary_Freiburg) 

# ICU Transfer quantiles
df_ICUTransfersRelative_Quantiles_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_ICUTransfersRelative_Quantiles
df_ICUTransfersRelative_Quantiles_Frankfurt$Site <- "Frankfurt"

df_ICUTransfersRelative_Quantiles_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_ICUTransfersRelative_Quantiles
df_ICUTransfersRelative_Quantiles_Freiburg$Site <- "Freiburg"

df_Output_ICUTransfersRelative_Quantiles_Main <- rbind(df_ICUTransfersRelative_Quantiles_Frankfurt, 
                                                     df_ICUTransfersRelative_Quantiles_Freiburg) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Time spent in Intensive Care relative to total time
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ICU Transfer summary statistic
df_ICUTimeRelative_Summary_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_ICUTimeRelative_Summary
df_ICUTimeRelative_Summary_Frankfurt$Site <- "Frankfurt"

df_ICUTimeRelative_Summary_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_ICUTimeRelative_Summary
df_ICUTimeRelative_Summary_Freiburg$Site <- "Freiburg"

df_Output_ICUTimeRelative_Summary_Main <- rbind(df_ICUTimeRelative_Summary_Frankfurt, 
                                                     df_ICUTimeRelative_Summary_Freiburg) 

# ICU Transfer quantiles
df_ICUTimeRelative_Quantiles_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_ICUTimeRelative_Quantiles
df_ICUTimeRelative_Quantiles_Frankfurt$Site <- "Frankfurt"

df_ICUTimeRelative_Quantiles_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_ICUTimeRelative_Quantiles
df_ICUTimeRelative_Quantiles_Freiburg$Site <- "Freiburg"

df_Output_ICUTimeRelative_Quantiles_Main <- rbind(df_ICUTimeRelative_Quantiles_Frankfurt, 
                                                       df_ICUTimeRelative_Quantiles_Freiburg) 


################################################################################
#
#          Cancer+/HIV+   vs.   Cancer+/HIV- 
#
################################################################################
# Attention! Freiburg only matched patients available in the full data set!!!

#!!!-!!!-!!! Plot describing completeness of data (Sankey diagram with nodes main diagnosis - treatment - follow up?)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Documented time span of main cancer care
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Not avaible at Munich
df_MainCancerDocumentedTimeSpan_Summary_Cologne <- SiteOutputData_Full$Cologne$df_Output_MainRecordedTimeSpan_Summary
df_MainCancerDocumentedTimeSpan_Summary_Cologne$Site <- "Cologne"

df_MainCancerDocumentedTimeSpan_Summary_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_MainCancerDocumentedTimeSpan_Summary
df_MainCancerDocumentedTimeSpan_Summary_Frankfurt$Site <- "Frankfurt"

df_MainCancerDocumentedTimeSpan_Summary_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_MainCancerDocumentedTimeSpan_Summary
df_MainCancerDocumentedTimeSpan_Summary_Freiburg$Site <- "Freiburg"

df_Output_MainCancerDocumentedTimeSpan_Summary_Main  <- rbind(df_MainCancerDocumentedTimeSpan_Summary_Cologne, 
                                                              df_MainCancerDocumentedTimeSpan_Summary_Frankfurt, 
                                                              df_MainCancerDocumentedTimeSpan_Summary_Freiburg)

 
df_MainCancerDocumentedTimeSpan_Quantiles_Cologne <- SiteOutputData_Full$Cologne$df_Output_MainRecordedTimeSpan_Quantiles
df_MainCancerDocumentedTimeSpan_Quantiles_Cologne$Site <- "Cologne"

df_MainCancerDocumentedTimeSpan_Quantiles_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_MainCancerDocumentedTimeSpan_Quantiles
df_MainCancerDocumentedTimeSpan_Quantiles_Frankfurt$Site <- "Frankfurt"

df_MainCancerDocumentedTimeSpan_Quantiles_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_MainCancerDocumentedTimeSpan_Quantiles
df_MainCancerDocumentedTimeSpan_Quantiles_Freiburg$Site <- "Freiburg"

df_Output_MainCancerDocumentedTimeSpan_Quantiles_Main  <- rbind(df_MainCancerDocumentedTimeSpan_Quantiles_Cologne, 
                                                              df_MainCancerDocumentedTimeSpan_Quantiles_Frankfurt, 
                                                              df_MainCancerDocumentedTimeSpan_Quantiles_Freiburg)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Age at presumed cancer diagnosis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Age (at cancer diagnosis) distribution over time
df_AgeAtCancerDiagnosis_Cologne <- SiteOutputData_Full$Cologne$df_Output_AgeAtCancerDiagnosis
df_AgeAtCancerDiagnosis_Cologne$Site <- "Cologne"
names(df_AgeAtCancerDiagnosis_Cologne)[2] <- "MainCancerDiagnosisYear"

df_AgeAtCancerDiagnosis_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_AgeAtCancerDiagnosis
df_AgeAtCancerDiagnosis_Frankfurt$Site <- "Frankfurt"

df_AgeAtCancerDiagnosis_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_AgeAtCancerDiagnosis
df_AgeAtCancerDiagnosis_Freiburg$Site <- "Freiburg"

df_AgeAtCancerDiagnosis_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_AgeAtCancerDiagnosis
df_AgeAtCancerDiagnosis_MunichLMU$Site <- "MunichLMU"

df_Output_AgeAtCancerDiagnosis_Main  <- rbind(df_AgeAtCancerDiagnosis_Cologne, 
                                              df_AgeAtCancerDiagnosis_Frankfurt, 
                                              df_AgeAtCancerDiagnosis_Freiburg, 
                                              df_AgeAtCancerDiagnosis_MunichLMU)



df_AgeAtCancerDiagnosis_Summary_Cologne <- SiteOutputData_Full$Cologne$df_Output_AgeAtCancerDiagnosis_Summary
df_AgeAtCancerDiagnosis_Summary_Cologne$Site <- "Cologne"

df_AgeAtCancerDiagnosis_Summary_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_AgeAtCancerDiagnosis_Summary
df_AgeAtCancerDiagnosis_Summary_Frankfurt$Site <- "Frankfurt"

df_AgeAtCancerDiagnosis_Summary_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_AgeAtCancerDiagnosis_Summary
df_AgeAtCancerDiagnosis_Summary_Freiburg$Site <- "Freiburg"

df_AgeAtCancerDiagnosis_Summary_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_AgeAtCancerDiagnosis_Summary
df_AgeAtCancerDiagnosis_Summary_MunichLMU$Site <- "MunichLMU"

df_Output_AgeAtCancerDiagnosis_Summary_Main   <- rbind(df_AgeAtCancerDiagnosis_Summary_Cologne, 
                                                       df_AgeAtCancerDiagnosis_Summary_Frankfurt, 
                                                       df_AgeAtCancerDiagnosis_Summary_Freiburg, 
                                                       df_AgeAtCancerDiagnosis_Summary_MunichLMU)


df_AgeAtCancerDiagnosis_Quantiles_Cologne <- SiteOutputData_Full$Cologne$df_Output_AgeAtCancerDiagnosis_Quantiles
df_AgeAtCancerDiagnosis_Quantiles_Cologne$Site <- "Cologne"

df_AgeAtCancerDiagnosis_Quantiles_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_AgeAtCancerDiagnosis_Quantiles
df_AgeAtCancerDiagnosis_Quantiles_Frankfurt$Site <- "Frankfurt"

df_AgeAtCancerDiagnosis_Quantiles_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_AgeAtCancerDiagnosis_Quantiles
df_AgeAtCancerDiagnosis_Quantiles_Freiburg$Site <- "Freiburg"

df_AgeAtCancerDiagnosis_Quantiles_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_AgeAtCancerDiagnosis_Quantiles
df_AgeAtCancerDiagnosis_Quantiles_MunichLMU$Site <- "MunichLMU"

df_Output_AgeAtCancerDiagnosis_Quantiles_Main   <- rbind(df_AgeAtCancerDiagnosis_Quantiles_Cologne, 
                                                       df_AgeAtCancerDiagnosis_Quantiles_Frankfurt, 
                                                       df_AgeAtCancerDiagnosis_Quantiles_Freiburg, 
                                                       df_AgeAtCancerDiagnosis_Quantiles_MunichLMU)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Count of distinct documented "real" cancer codes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Not avible at Cologne

df_Output_DistinctCodeCountCancer_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_DistinctCodeCountCancer
df_Output_DistinctCodeCountCancer_Frankfurt$Site <- "Frankfurt"

df_Output_DistinctCodeCountCancer_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_DistinctCodeCountCancer
df_Output_DistinctCodeCountCancer_Freiburg$Site <- "Freiburg"

df_Output_DistinctCodeCountCancer_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_DistinctCodeCountCancer
df_Output_DistinctCodeCountCancer_MunichLMU$Site <- "MunichLMU"

df_Output_DistinctCodeCountCancer_Main <- rbind(df_Output_DistinctCodeCountCancer_Frankfurt, 
                                              df_Output_DistinctCodeCountCancer_Freiburg, 
                                              df_Output_DistinctCodeCountCancer_MunichLMU)


df_Output_DistinctCodeCountCancer_Summary_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_DistinctCodeCountCancer_Summary
df_Output_DistinctCodeCountCancer_Summary_Frankfurt$Site <- "Frankfurt"

df_Output_DistinctCodeCountCancer_Summary_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_DistinctCodeCountCancer_Summary
df_Output_DistinctCodeCountCancer_Summary_Freiburg$Site <- "Freiburg"

df_Output_DistinctCodeCountCancer_Summary_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_DistinctCodeCountCancer_Summary
df_Output_DistinctCodeCountCancer_Summary_MunichLMU$Site <- "MunichLMU"

df_Output_DistinctCodeCountCancer_Summary_Main <- rbind(df_Output_DistinctCodeCountCancer_Summary_Frankfurt, 
                                                df_Output_DistinctCodeCountCancer_Summary_Freiburg, 
                                                df_Output_DistinctCodeCountCancer_Summary_MunichLMU)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer grouping: AD, NAD and Non-HIV-associated cancer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Output_HIVCancerCategories_Cologne <- SiteOutputData_Full$Cologne$df_Output_HIVCancerCategories
df_Output_HIVCancerCategories_Cologne$Site <- "Cologne"
names(df_Output_HIVCancerCategories_Cologne)[3] <- "MainCancerDiagnosisYear"

df_Output_HIVCancerCategories_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_HIVCancerCategories
df_Output_HIVCancerCategories_Frankfurt$Site <- "Frankfurt"

df_Output_HIVCancerCategories_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_HIVCancerCategories
df_Output_HIVCancerCategories_Freiburg$Site <- "Freiburg"

df_Output_HIVCancerCategories_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_HIVCancerCategories
df_Output_HIVCancerCategories_MunichLMU$Site <- "MunichLMU"

df_Output_HIVCancerCategories_Main <- rbind(df_Output_HIVCancerCategories_Cologne,
                                            df_Output_HIVCancerCategories_Frankfurt,
                                            df_Output_HIVCancerCategories_Freiburg,
                                            df_Output_HIVCancerCategories_MunichLMU)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer grouping: Carcinoma in situ
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Output_CIS_Cologne <- SiteOutputData_Full$Cologne$df_Output_CIS
df_Output_CIS_Cologne$Site <- "Cologne"
names(df_Output_CIS_Cologne)[3] <- "MainCancerDiagnosisYear"

df_Output_CIS_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_CIS
df_Output_CIS_Frankfurt$Site <- "Frankfurt"

df_Output_CIS_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_CIS
df_Output_CIS_Freiburg$Site <- "Freiburg"

df_Output_CIS_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_CIS
df_Output_CIS_MunichLMU$Site <- "MunichLMU"

df_Output_CIS_Main <- rbind(df_Output_CIS_Cologne,
                                            df_Output_CIS_Frankfurt,
                                            df_Output_CIS_Freiburg,
                                            df_Output_CIS_MunichLMU)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: Topography by Organ
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Output_MainCancerTopographyDetail_Cologne <- SiteOutputData_Full$Cologne$df_Output_CancerTopographyDetail
df_Output_MainCancerTopographyDetail_Cologne$Site <- "Cologne"
names(df_Output_MainCancerTopographyDetail_Cologne)[2] <- "MainCancerTopographyDetail"

df_Output_MainCancerTopographyDetail_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_MainCancerTopographyDetail
df_Output_MainCancerTopographyDetail_Frankfurt$Site <- "Frankfurt"

df_Output_MainCancerTopographyDetail_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_MainCancerTopographyDetail
df_Output_MainCancerTopographyDetail_Freiburg$Site <- "Freiburg"

df_Output_MainCancerTopographyDetail_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_MainCancerTopographyDetail
df_Output_MainCancerTopographyDetail_MunichLMU$Site <- "MunichLMU"

df_Output_MainCancerTopographyDetail_Main <- rbind(df_Output_MainCancerTopographyDetail_Cologne,
                            df_Output_MainCancerTopographyDetail_Frankfurt,
                            df_Output_MainCancerTopographyDetail_Freiburg,
                            df_Output_MainCancerTopographyDetail_MunichLMU)


# Data stratified by year of cancer diagnosis
df_Output_MainCancerTopographyDetail_OverTime_Cologne <- SiteOutputData_Full$Cologne$df_Output_CancerTopographyDetail_OverTime
df_Output_MainCancerTopographyDetail_OverTime_Cologne$Site <- "Cologne"
names(df_Output_MainCancerTopographyDetail_OverTime_Cologne)[2] <-"MainCancerTopographyDetail"
names(df_Output_MainCancerTopographyDetail_OverTime_Cologne)[3] <- "MainCancerDiagnosisYear"

df_Output_MainCancerTopographyDetail_OverTime_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_MainCancerTopographyDetail_OverTime
df_Output_MainCancerTopographyDetail_OverTime_Frankfurt$Site <- "Frankfurt"

df_Output_MainCancerTopographyDetail_OverTime_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_MainCancerTopographyDetail_OverTime
df_Output_MainCancerTopographyDetail_OverTime_Freiburg$Site <- "Freiburg"

df_Output_MainCancerTopographyDetail_OverTime_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_MainCancerTopographyDetail_OverTime
df_Output_MainCancerTopographyDetail_OverTime_MunichLMU$Site <- "MunichLMU"

df_Output_MainCancerTopographyDetail_OverTime_Main <- rbind(df_Output_MainCancerTopographyDetail_OverTime_Cologne,
                                                   df_Output_MainCancerTopographyDetail_OverTime_Frankfurt,
                                                   df_Output_MainCancerTopographyDetail_OverTime_Freiburg,
                                                   df_Output_MainCancerTopographyDetail_OverTime_MunichLMU)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: Topography by ICD Grouping
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Output_MainCancerTopographyGroup_Cologne <- SiteOutputData_Full$Cologne$df_Output_CancerTopographyGroup
df_Output_MainCancerTopographyGroup_Cologne$Site <- "Cologne"
names(df_Output_MainCancerTopographyGroup_Cologne)[2] <- "MainCancerTopographyGroup"

df_Output_MainCancerTopographyGroup_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_MainCancerTopographyGroup
df_Output_MainCancerTopographyGroup_Frankfurt$Site <- "Frankfurt"

df_Output_MainCancerTopographyGroup_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_MainCancerTopographyGroup
df_Output_MainCancerTopographyGroup_Freiburg$Site <- "Freiburg"

df_Output_MainCancerTopographyGroup_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_MainCancerTopographyGroup
df_Output_MainCancerTopographyGroup_MunichLMU$Site <- "MunichLMU"

df_Output_MainCancerTopographyGroup_Main <- rbind(df_Output_MainCancerTopographyGroup_Cologne,
                                                   df_Output_MainCancerTopographyGroup_Frankfurt,
                                                   df_Output_MainCancerTopographyGroup_Freiburg,
                                                   df_Output_MainCancerTopographyGroup_MunichLMU)
  

# Data stratified by year of cancer diagnosis
df_Output_MainCancerTopographyGroup_OverTime_Cologne <- SiteOutputData_Full$Cologne$df_Output_CancerTopographyGroup_OverTime
df_Output_MainCancerTopographyGroup_OverTime_Cologne$Site <- "Cologne"
names(df_Output_MainCancerTopographyGroup_OverTime_Cologne)[2] <-"MainCancerTopographyGroup"
names(df_Output_MainCancerTopographyGroup_OverTime_Cologne)[3] <- "MainCancerDiagnosisYear"

df_Output_MainCancerTopographyGroup_OverTime_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_MainCancerTopographyGroup_OverTime
df_Output_MainCancerTopographyGroup_OverTime_Frankfurt$Site <- "Frankfurt"

df_Output_MainCancerTopographyGroup_OverTime_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_MainCancerTopographyGroup_OverTime
df_Output_MainCancerTopographyGroup_OverTime_Freiburg$Site <- "Freiburg"

df_Output_MainCancerTopographyGroup_OverTime_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_MainCancerTopographyGroup_OverTime
df_Output_MainCancerTopographyGroup_OverTime_MunichLMU$Site <- "MunichLMU"

df_Output_MainCancerTopographyGroup_OverTime_Main <- rbind(df_Output_MainCancerTopographyGroup_OverTime_Cologne,
                                                            df_Output_MainCancerTopographyGroup_OverTime_Frankfurt,
                                                            df_Output_MainCancerTopographyGroup_OverTime_Freiburg,
                                                            df_Output_MainCancerTopographyGroup_OverTime_MunichLMU)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: By entity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Cancer occurrence grouped by coded entity
df_Output_CancerEntities_Cologne <- SiteOutputData_Full$Cologne$df_Output_CancerEntities
df_Output_CancerEntities_Cologne$Site <- "Cologne"
names(df_Output_CancerEntities_Cologne)[2] <- "MainCancerCode"

df_Output_CancerEntities_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_CancerEntities
df_Output_CancerEntities_Frankfurt$Site <- "Frankfurt"

df_Output_CancerEntities_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_CancerEntities
df_Output_CancerEntities_Freiburg$Site <- "Freiburg"

df_Output_CancerEntities_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_CancerEntities
df_Output_CancerEntities_MunichLMU$Site <- "MunichLMU"

df_Output_CancerEntities_Main <- rbind(df_Output_CancerEntities_Cologne,
                                                  df_Output_CancerEntities_Frankfurt,
                                                  df_Output_CancerEntities_Freiburg,
                                                  df_Output_CancerEntities_MunichLMU)

# Data stratified by year of cancer diagnosis
df_Output_CancerEntities_OverTime_Cologne <- SiteOutputData_Full$Cologne$df_Output_CancerEntities_OverTime
df_Output_CancerEntities_OverTime_Cologne$Site <- "Cologne"
names(df_Output_CancerEntities_OverTime_Cologne)[2] <-"MainCancerCode"
names(df_Output_CancerEntities_OverTime_Cologne)[3] <- "MainCancerDiagnosisYear"

df_Output_CancerEntities_OverTime_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_CancerEntities_OverTime
df_Output_CancerEntities_OverTime_Frankfurt$Site <- "Frankfurt"

df_Output_CancerEntities_OverTime_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_CancerEntities_OverTime
df_Output_CancerEntities_OverTime_Freiburg$Site <- "Freiburg"

df_Output_CancerEntities_OverTime_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_CancerEntities_OverTime
df_Output_CancerEntities_OverTime_MunichLMU$Site <- "MunichLMU"

df_Output_CancerEntities_OverTime_Main <- rbind(df_Output_CancerEntities_OverTime_Cologne,
                                                           df_Output_CancerEntities_OverTime_Frankfurt,
                                                           df_Output_CancerEntities_OverTime_Freiburg,
                                                           df_Output_CancerEntities_OverTime_MunichLMU)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Metastasis occurrence
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Output_MetastasisOccurrence_Cologne <- SiteOutputData_Full$Cologne$df_Output_MetastasisOccurrence
df_Output_MetastasisOccurrence_Cologne$Site <- "Cologne"
names(df_Output_MetastasisOccurrence_Cologne)[2] <- "MainCancerDiagnosisYear"

df_Output_MetastasisOccurrence_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_MetastasisOccurrence
df_Output_MetastasisOccurrence_Frankfurt$Site <- "Frankfurt"

df_Output_MetastasisOccurrence_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_MetastasisOccurrence
df_Output_MetastasisOccurrence_Freiburg$Site <- "Freiburg"

df_Output_MetastasisOccurrence_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_MetastasisOccurrence
df_Output_MetastasisOccurrence_MunichLMU$Site <- "MunichLMU"

df_Output_MetastasisOccurrence_Main <- rbind(df_Output_MetastasisOccurrence_Cologne,
                                       df_Output_MetastasisOccurrence_Frankfurt,
                                       df_Output_MetastasisOccurrence_Freiburg,
                                       df_Output_MetastasisOccurrence_MunichLMU)
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Time to Metastasis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Output_TimeCancerToMetastasis_Summary_Cologne <- SiteOutputData_Full$Cologne$df_Output_TimeCancerToMetastasis_Summary
df_Output_TimeCancerToMetastasis_Summary_Cologne$Site <- "Cologne"

df_Output_TimeCancerToMetastasis_Summary_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_TimeCancerToMetastasis_Summary
df_Output_TimeCancerToMetastasis_Summary_Frankfurt$Site <- "Frankfurt"

df_Output_TimeCancerToMetastasis_Summary_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_TimeCancerToMetastasis_Summary
df_Output_TimeCancerToMetastasis_Summary_Freiburg$Site <- "Freiburg"

df_Output_TimeCancerToMetastasis_Summary_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_TimeCancerToMetastasis_Summary
df_Output_TimeCancerToMetastasis_Summary_MunichLMU$Site <- "MunichLMU"

df_Output_TimeCancerToMetastasis_Summary_Main <- rbind(df_Output_TimeCancerToMetastasis_Summary_Cologne,
                                             df_Output_TimeCancerToMetastasis_Summary_Frankfurt,
                                             df_Output_TimeCancerToMetastasis_Summary_Freiburg,
                                             df_Output_TimeCancerToMetastasis_Summary_MunichLMU)


df_Output_TimeCancerToMetastasis_Quantiles_Cologne <- SiteOutputData_Full$Cologne$df_Output_TimeCancerToMetastasis_Quantiles
df_Output_TimeCancerToMetastasis_Quantiles_Cologne$Site <- "Cologne"

df_Output_TimeCancerToMetastasis_Quantiles_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_TimeCancerToMetastasis_Quantiles
df_Output_TimeCancerToMetastasis_Quantiles_Frankfurt$Site <- "Frankfurt"

df_Output_TimeCancerToMetastasis_Quantiles_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_TimeCancerToMetastasis_Quantiles
df_Output_TimeCancerToMetastasis_Quantiles_Freiburg$Site <- "Freiburg"

df_Output_TimeCancerToMetastasis_Quantiles_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_TimeCancerToMetastasis_Quantiles
df_Output_TimeCancerToMetastasis_Quantiles_MunichLMU$Site <- "MunichLMU"

df_Output_TimeCancerToMetastasis_Quantiles_Main <- rbind(df_Output_TimeCancerToMetastasis_Quantiles_Cologne,
                                                       df_Output_TimeCancerToMetastasis_Quantiles_Frankfurt,
                                                       df_Output_TimeCancerToMetastasis_Quantiles_Freiburg,
                                                       df_Output_TimeCancerToMetastasis_Quantiles_MunichLMU)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Therapy modalities
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Output_TherapyModalities_Cologne <- SiteOutputData_Full$Cologne$df_Output_TherapyModalities
df_Output_TherapyModalities_Cologne$Site <- "Cologne"
names(df_Output_TherapyModalities_Cologne)[2] <- "MainCancerDiagnosisYear"
names(df_Output_TherapyModalities_Cologne)[10] <- "CountImmunotherapy"
names(df_Output_TherapyModalities_Cologne)[11] <- "ProportionImmunotherapy"

df_Output_TherapyModalities_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_TherapyModalities
df_Output_TherapyModalities_Frankfurt$Site <- "Frankfurt"

df_Output_TherapyModalities_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_TherapyModalities
df_Output_TherapyModalities_Freiburg$Site <- "Freiburg"

df_Output_TherapyModalities_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_TherapyModalities
df_Output_TherapyModalities_MunichLMU$Site <- "MunichLMU"

df_Output_TherapyModalities_Main <- rbind(df_Output_TherapyModalities_Cologne,
                                                         df_Output_TherapyModalities_Frankfurt,
                                                         df_Output_TherapyModalities_Freiburg,
                                                         df_Output_TherapyModalities_MunichLMU)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Therapy sequence
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make use of make_long() to get compatible data frame for Sankey diagram
# x: Stage
# node: Node
df_Output_CancerTherapySequence_Cologne <- SiteOutputData_Full$Cologne$df_Output_CancerTherapySequence
df_Output_CancerTherapySequence_Cologne$Site <- "Cologne"

df_Output_CancerTherapySequence_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_CancerTherapySequence
df_Output_CancerTherapySequence_Frankfurt$Site <- "Frankfurt"

df_Output_CancerTherapySequence_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_CancerTherapySequence
df_Output_CancerTherapySequence_Freiburg$Site <- "Freiburg"

df_Output_CancerTherapySequence_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_CancerTherapySequence
df_Output_CancerTherapySequence_MunichLMU$Site <- "MunichLMU"

df_Output_CancerTherapySequence_Main <- rbind(df_Output_CancerTherapySequence_Cologne,
                                          df_Output_CancerTherapySequence_Frankfurt,
                                          df_Output_CancerTherapySequence_Freiburg,
                                          df_Output_CancerTherapySequence_MunichLMU)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Complications after (Chemo)therapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Output_TherapyComplications_Cologne <- SiteOutputData_Full$Cologne$df_Output_TherapyComplications
df_Output_TherapyComplications_Cologne$Site <- "Cologne"
names(df_Output_TherapyComplications_Cologne)[2] <- "MainCancerDiagnosisYear"

df_Output_TherapyComplications_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_TherapyComplications
df_Output_TherapyComplications_Frankfurt$Site <- "Frankfurt"

df_Output_TherapyComplications_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_TherapyComplications
df_Output_TherapyComplications_Freiburg$Site <- "Freiburg"

df_Output_TherapyComplications_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_TherapyComplications
df_Output_TherapyComplications_MunichLMU$Site <- "MunichLMU"

df_Output_TherapyComplications_Main <- rbind(df_Output_TherapyComplications_Cologne,
                                              df_Output_TherapyComplications_Frankfurt,
                                              df_Output_TherapyComplications_Freiburg,
                                              df_Output_TherapyComplications_MunichLMU)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Last recorded discharge reason
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Output_LastRecordedDischargeCategory_Cologne <- SiteOutputData_Full$Cologne$df_Output_LastRecordedDischargeReason
df_Output_LastRecordedDischargeCategory_Cologne$Site <- "Cologne"
names(df_Output_LastRecordedDischargeCategory_Cologne)[2] <- "LastRecordedDischargeCategory"
names(df_Output_LastRecordedDischargeCategory_Cologne)[3] <- "MainCancerDiagnosisYear"

df_Output_LastRecordedDischargeCategory_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_LastRecordedDischargeCategory
df_Output_LastRecordedDischargeCategory_Frankfurt$Site <- "Frankfurt"

df_Output_LastRecordedDischargeCategory_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_LastRecordedDischargeCategory
df_Output_LastRecordedDischargeCategory_Freiburg$Site <- "Freiburg"

df_Output_LastRecordedDischargeCategory_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_LastRecordedDischargeCategory
df_Output_LastRecordedDischargeCategory_MunichLMU$Site <- "MunichLMU"

df_Output_LastRecordedDischargeCategory_Main <- rbind(df_Output_LastRecordedDischargeCategory_Cologne,
                                             df_Output_LastRecordedDischargeCategory_Frankfurt,
                                             df_Output_LastRecordedDischargeCategory_Freiburg,
                                             df_Output_LastRecordedDischargeCategory_MunichLMU)


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


# Make use of make_long() to get compatible data frame for Sankey diagram
# x: Stage
# node: Node
# Not avaible at Freiburg
df_Output_HIVCancerSequence_Cologne <- SiteOutputData_Full$Cologne$df_Output_HIVCancerSequence
df_Output_HIVCancerSequence_Cologne$Site <- "Cologne"

df_Output_HIVCancerSequence_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_HIVCancerSequence
df_Output_HIVCancerSequence_Frankfurt$Site <- "Frankfurt"

df_Output_HIVCancerSequence_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_HIVCancerSequence
df_Output_HIVCancerSequence_MunichLMU$Site <- "MunichLMU"

df_Output_HIVCancerSequence_Main <- rbind(df_Output_HIVCancerSequence_Cologne,
                                                df_Output_HIVCancerSequence_Frankfurt,
                                                df_Output_HIVCancerSequence_MunichLMU)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AIDS occurrence in HIV-positive cancer patients
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Output_HIVCancerAIDS_Cologne <- SiteOutputData_Full$Cologne$df_Output_HIVCancerAIDS
df_Output_HIVCancerAIDS_Cologne$Site <- "Cologne"
names(df_Output_HIVCancerAIDS_Cologne)[2] <- "MainCancerDiagnosisYear"

df_Output_HIVCancerAIDS_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_HIVCancerAIDS
df_Output_HIVCancerAIDS_Frankfurt$Site <- "Frankfurt"

df_Output_HIVCancerAIDS_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_HIVCancerAIDS
df_Output_HIVCancerAIDS_Freiburg$Site <- "Freiburg"

df_Output_HIVCancerAIDS_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_HIVCancerAIDS
df_Output_HIVCancerAIDS_MunichLMU$Site <- "MunichLMU"

df_Output_HIVCancerAIDS_Main <- rbind(df_Output_HIVCancerAIDS_Cologne,
                                                df_Output_HIVCancerAIDS_Frankfurt,
                                                df_Output_HIVCancerAIDS_Freiburg,
                                                df_Output_HIVCancerAIDS_MunichLMU)


################################################################################
#
#          Cancer+/HIV+   vs.   Cancer-/HIV+ 
#
################################################################################


#---------- HIV Status ---------------------------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AIDS occurrence across all HIV-positive patients
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df_Output_AIDSOccurrence_Cologne <- SiteOutputData_Full$Cologne$df_Output_AIDSOccurrence
df_Output_AIDSOccurrence_Cologne$Site <- "Cologne"
names(df_Output_AIDSOccurrence_Cologne)[2] <- "FirstRelevantAdmissionYear"

df_Output_AIDSOccurrence_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_AIDSOccurrence
df_Output_AIDSOccurrence_Frankfurt$Site <- "Frankfurt"

df_Output_AIDSOccurrence_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_AIDSOccurrence
df_Output_AIDSOccurrence_Freiburg$Site <- "Freiburg"

df_Output_AIDSOccurrence_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_AIDSOccurrence
df_Output_AIDSOccurrence_MunichLMU$Site <- "MunichLMU"

df_Output_AIDSOccurrence_Main <- rbind(df_Output_AIDSOccurrence_Cologne,
                                      df_Output_AIDSOccurrence_Frankfurt,
                                      df_Output_AIDSOccurrence_Freiburg,
                                      df_Output_AIDSOccurrence_MunichLMU)


###############################################################################
contain <- c("df_Output_PostalCodeCounts_Main" , "df_Output_SampleSize_Main", "df_Output_Sex_Main",
  "df_Output_Age_Main", "df_Output_Age_Summary_Main", "df_Output_Age_Quantiles_Main",
  "df_Output_CaseCount_Main", "df_Output_CaseCount_Summary_Main", "df_Output_CaseCount_Quantiles_Main",
  "df_Output_MeanLengthOfStay_Main", "df_Output_MeanLengthOfStay_Summary_Main", "df_Output_MeanLengthOfStay_Quantiles_Main",
  "df_Output_ICUTransfersRelative_Summary_Main", "df_Output_ICUTransfersRelative_Quantiles_Main",
  "df_Output_ICUTimeRelative_Summary_Main", "df_Output_ICUTimeRelative_Quantiles_Main",
  "df_Output_MainCancerDocumentedTimeSpan_Summary_Main", "df_Output_MainCancerDocumentedTimeSpan_Quantiles_Main",
  "df_Output_AgeAtCancerDiagnosis_Main", "df_Output_AgeAtCancerDiagnosis_Summary_Main", "df_Output_AgeAtCancerDiagnosis_Quantiles_Main",
  "df_Output_DistinctCodeCountCancer_Main", "df_Output_DistinctCodeCountCancer_Summary_Main",
  "df_Output_HIVCancerCategories_Main", "df_Output_CIS_Main", "df_Output_MainCancerTopographyDetail_Main",
  "df_Output_MainCancerTopographyDetail_OverTime_Main", "df_Output_MainCancerTopographyGroup_Main",
  "df_Output_MainCancerTopographyGroup_OverTime_Main", "df_Output_CancerEntities_Main",
  "df_Output_CancerEntities_OverTime_Main", "df_Output_MetastasisOccurrence_Main",
  "df_Output_TimeCancerToMetastasis_Summary_Main", "df_Output_TimeCancerToMetastasis_Quantiles_Main",  
  "df_Output_TherapyModalities_Main", "df_Output_CancerTherapySequence_Main", "df_Output_TherapyComplications_Main", 
  "df_Output_LastRecordedDischargeCategory_Main", "df_Output_HIVCancerDiagnosisOrder_Main",
  "df_Output_HIVCancerSequence_Main", "df_Output_HIVCancerAIDS_Main", "df_Output_AIDSOccurrence_Main",
  "SiteOutputData_Full", "SiteOutputData_Matched", "SiteObjectAvailability"
  )

del <- ls()[-which(ls() %in% contain)]
rm(list = del)
