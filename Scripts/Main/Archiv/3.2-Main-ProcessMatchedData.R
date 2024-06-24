################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: AGGREGATE and PROCESS Matched DATA (MAIN)                         #
#------------------------------------------------------------------------------#
################################################################################

############ Demographics ######################################################
# Age at Cancer Diagnosis
df_AgeAtCancer_Matched_Cologne <- SiteOutputData_Matched$Cologne$df_Output_AgeAtCancerDiagnosis_Summary
df_AgeAtCancer_Matched_Cologne$Site <- "Cologne"

df_AgeAtCancer_Matched_Frankfurt <- SiteOutputData_Matched$Frankfurt$df_Output_AgeAtCancerDiagnosis_Summary
df_AgeAtCancer_Matched_Frankfurt$Site <- "Frankfurt"

df_AgeAtCancer_Matched_Freiburg <- SiteOutputData_Matched$Freiburg$df_Output_AgeAtCancerDiagnosis_Summary
df_AgeAtCancer_Matched_Freiburg$Site <- "Freiburg"

df_AgeAtCancer_Matched_MunichLMU <- SiteOutputData_Matched$MunichLMU$df_Output_AgeAtCancerDiagnosis_Summary
df_AgeAtCancer_Matched_MunichLMU$Site <- "MunichLMU"

df_AgeAtCancerDiagnosis_Matched <- rbind(df_AgeAtCancer_Matched_Cologne, df_AgeAtCancer_Matched_Frankfurt, df_AgeAtCancer_Matched_Freiburg, df_AgeAtCancer_Matched_MunichLMU)


subgroup <- unique(df_AgeAtCancerDiagnosis_Matched$PatientSubgroup)
df_AgeAtCancerDiagnosis_MatchedOverall <- c()
for(s in subgroup){
  df_temp <- df_AgeAtCancerDiagnosisM %>% filter(PatientSubgroup %in% s)
  
  n <- nrow(df_temp)
  
  a <- df_temp$N[1]
  b <- df_temp$N[2]
  c <- df_temp$N[3]
  d <- df_temp$N[4]
  
  m1 <- df_temp$Mean[1]
  m2 <- df_temp$Mean[2]
  m3 <- df_temp$Mean[3]
  m4 <- df_temp$Mean[4]
  
  w_mean <- round((a*m1 + b*m2 + c*m3 + d*m4)/(a+b+c+d),2)
  print(w_mean)
  
  #ask Ian!
  w_sd_numerator <- (a*(m1-w_mean)^2) + (b*(m2-w_mean)^2) + (c*(m3-w_mean)^2) + (d*(m4-w_mean)^2)
  w_sd_denumerator <- (n-1/n)*(a+b+c+d)
  
  w_sd <- round(sqrt(w_sd_numerator / w_sd_denumerator),2)
  print(w_sd)
  
  values_temp <- c(s, w_mean, w_sd)
  df_AgeAtCancerDiagnosis_MatchedOverall <- rbind(df_AgeAtCancerDiagnosis_MatchedOverall, values_temp)
}

df_AgeAtCancerDiagnosis_MatchedOverall <- as.data.frame(df_AgeAtCancerDiagnosis_MatchedOverall)
df_AgeAtCancerDiagnosis_MatchedOverall <- df_AgeAtCancerDiagnosis_MatchedOverall%>% rename(PatientsSubgroup = V1,
                                                                  WightedMean = V2, WightedSD = V3)


df_AgeAtCancerDiagnosis_Matched$Mean <- round(df_AgeAtCancerDiagnosis_Matched$Mean, 2)
df_AgeAtCancerDiagnosis_Matched$SD <- round(df_AgeAtCancerDiagnosis_Matched$SD, 2)

################################################################################
# Distinct CodebCountbCancer

#df_DistinctCancer_Matched_Cologne <- SiteOutputData_Matched$Cologne$df_Output_Di
#df_DistinctCancer_Matched_Cologne$Site <- "Cologne"

df_DistinctCancer_Matched_Frankfurt <- SiteOutputData_Matched$Frankfurt$df_Output_DistinctCodeCountCancer_Summary
df_DistinctCancer_Matched_Frankfurt$Site <- "Frankfurt"

df_DistinctCancer_Matched_Freiburg <- SiteOutputData_Matched$Freiburg$df_Output_DistinctCodeCountCancer_Summary
df_DistinctCancer_Matched_Freiburg$Site <- "Freiburg"

df_DistinctCancer_Matched_MunichLMU <- SiteOutputData_Matched$MunichLMU$df_Output_DistinctCodeCountCancer_Summary
df_DistinctCancer_Matched_MunichLMU$Site <- "MunichLMU"

#df_DistinctCancer_Matched <- rbind(df_DistinctCancer_Matched_Cologne, df_DistinctCancer_Matched_Frankfurt, df_DistinctCancer_Matched_Freiburg, df_DistinctCancer_Matched_MunichLMU)
df_DistinctCancer_Matched <- rbind(df_DistinctCancer_Matched_Frankfurt, df_DistinctCancer_Matched_Freiburg, df_DistinctCancer_Matched_MunichLMU)

subgroup <- unique(df_DistinctCancer_Matched$PatientSubgroup)
df_DistinctCancer_MatchedOverall <- c()
for(s in subgroup){
  df_temp <- df_DistinctCancer_Matched %>% filter(PatientSubgroup %in% s)
  
  n <- nrow(df_temp)
  
  a <- df_temp$N[1]
  b <- df_temp$N[2]
  c <- df_temp$N[3]
  #d <- df_temp$N[4]
  
  m1 <- df_temp$Mean[1]
  m2 <- df_temp$Mean[2]
  m3 <- df_temp$Mean[3]
 # m4 <- df_temp$Mean[4]
  
  w_mean <- round((a*m1 + b*m2 + c*m3)/(a+b+c), 2)
  #w_mean <- round((a*m1 + b*m2 + c*m3 + d*m4)/(a+b+c+d),2)
  #print(w_mean)
  
  w_sd_numerator <- (a*(m1-w_mean)^2) + (b*(m2-w_mean)^2) + (c*(m3-w_mean)^2)
  w_sd_denumerator <- (n-1/n)*(a+b+c)
  #w_sd_numerator <- (a*(m1-w_mean)^2) + (b*(m2-w_mean)^2) + (c*(m3-w_mean)^2) + (d*(m4-w_mean)^2)
  #w_sd_denumerator <- (n-1/n)*(a+b+c+d)
  
  w_sd <- round(sqrt(w_sd_numerator / w_sd_denumerator),2)
  #print(w_sd)
  
  values_temp <- c(s, w_mean, w_sd)
  df_DistinctCancer_MatchedOverall <- rbind(df_DistinctCancer_MatchedOverall, values_temp)
}

df_DistinctCancer_MatchedOverall <- as.data.frame(df_DistinctCancer_MatchedOverall)
df_DistinctCancer_MatchedOverall<- df_DistinctCancer_MatchedOverall%>% rename(PatientsSubgroup = V1,
                                                                                           WightedMean = V2, WightedSD = V3)


df_DistinctCancer_Matched$Mean <- round(df_DistinctCancer_Matched$Mean, 2)
df_DistinctCancer_Matched$SD <- round(df_DistinctCancer_Matched$SD, 2)


################################################################################
# Crcinoma in situ


df_CIS_Matched_Cologne <- SiteOutputData_Matched$Cologne$df_Output_CIS
df_CIS_Matched_Cologne$Site <- "Cologne"

df_CIS_Matched_Frankfurt <- SiteOutputData_Matched$Frankfurt$df_Output_CIS
df_CIS_Matched_Frankfurt$Site <- "Frankfurt"

df_CIS_Matched_Freiburg <- SiteOutputData_Matched$Freiburg$df_Output_CIS
df_CIS_Matched_Freiburg$Site <- "Freiburg"

df_CIS_Matched_MunichLMU <- SiteOutputData_Matched$MunichLMU$df_Output_CIS
df_CIS_Matched_MunichLMU$Site <- "MunichLMU"

df_CIS_Matched <- rbind(df_CIS_Matched_Cologne, df_CIS_Matched_Frankfurt, df_CIS_Matched_Freiburg, df_CIS_Matched_MunichLMU)


sum(df_CIS_Matched[which((df_CIS_Matched[,2] == TRUE | df_CIS_Matched[,6] == TRUE) & df_CIS_Matched[,1] == "Cancer+/HIV-"),4])
sum(df_CIS_Matched[which((df_CIS_Matched[,2] == TRUE | df_CIS_Matched[,6] == TRUE)  & df_CIS_Matched[,1] == "Cancer+/HIV+"),4])

sum(df_CIS_Matched[which((df_CIS_Matched[,2] == FALSE |df_CIS_Matched[,6] == FALSE) & df_CIS_Matched[,1] == "Cancer+/HIV-"),4])
sum(df_CIS_Matched[which((df_CIS_Matched[,2] == FALSE | df_CIS_Matched[,6] == FALSE) & df_CIS_Matched[,1] == "Cancer+/HIV+"),4])
sum(df_CIS_Matched$N)

################################################################################
# HIV Cancer Categories

SiteOutputData_Matched$Cologne$df_Output_HIVCancerCategories


################################################################################
#Therapy modalities

df_TherapyModalities_Matched_Cologne <- SiteOutputData_Matched$Cologne$df_Output_TherapyModalities


################################################################################
# Case Count per patient

# Case Count
df_CaseCount_Matched_Cologne <- SiteOutputData_Matched$Cologne$df_Output_CaseCount_Summary
df_CaseCount_Matched_Cologne$Site <- "Cologne"


df_CaseCount_Matched_Frankfurt <- SiteOutputData_Matched$Frankfurt$df_Output_CaseCount_Summary
df_CaseCount_Matched_Frankfurt$Site <- "Frankfurt"

df_CaseCount_Matched_Freiburg <- SiteOutputData_Matched$Freiburg$df_Output_CaseCount_Summary
df_CaseCount_Matched_Freiburg$Site <- "Freiburg"

df_CaseCount_Matched_MunichLMU <- SiteOutputData_Matched$MunichLMU$df_Output_CaseCount
df_CaseCount_Matched_MunichLMU$Site <- "MunichLMU"

df_CaseCount_Matched <- rbind(df_CaseCount_Matched_Cologne, df_CaseCount_Matched_Frankfurt, df_CaseCount_Matched_Freiburg, df_CaseCount_Matched_MunichLMU)


################################################################################


#Mean Length of Stay
df_LengOfStay_Cologne <- SiteOutputData_Full$Cologne$df_Output_MeanLengthOfStay_Summary
df_LengOfStay_Cologne$Site <- "Cologne"

df_LengOfStay_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_MeanLengthOfStay_Summary
df_LengOfStay_Frankfurt$Site <- "Frankfurt"

df_LengOfStay_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_MeanLengthOfStay_Summary
df_LengOfStay_Freiburg$Site <- "Freiburg"

df_LengOfStay_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_MeanLengthOfStay_Summary
df_LengOfStay_MunichLMU$Site <- "MunichLMU"

df_LengOfStay <- rbind(df_LengOfStay_Cologne, df_LengOfStay_Frankfurt, df_LengOfStay_Freiburg, df_LengOfStay_MunichLMU)
#df_LengOfStay <- rbind(df_LengOfStay_Cologne, df_LengOfStay_Frankfurt, df_LengOfStay_MunichLMU)


################################################################################


################################################################################
###################### Cancer Topography #######################################
df_CancerEntities_OverTime_Cologne <- SiteOutputData_Full$Cologne$df_Output_CancerEntities_OverTime
df_CancerEntities_OverTime_Cologne$Site <- "Cologne"
names(df_CancerEntities_OverTime_Cologne)[2] <- "MainCancerCode"
names(df_CancerEntities_OverTime_Cologne)[3] <- "MainCancerDiagnosisYear"

df_CancerEntities_OverTime_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_CancerEntities_OverTime
df_CancerEntities_OverTime_Frankfurt$Site <- "Frankfurt"

df_CancerEntities_OverTime_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_CancerEntities_OverTime
df_CancerEntities_OverTime_Freiburg$Site <- "Freiburg"

df_CancerEntities_OverTime_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_CancerEntities_OverTime
df_CancerEntities_OverTime_MunichLMU$Site <- "MunichLMU"

df_CancerEntities_OverTime <- rbind(df_CancerEntities_OverTime_Cologne, df_CancerEntities_OverTime_Frankfurt, df_CancerEntities_OverTime_Freiburg, df_CancerEntities_OverTime_MunichLMU)

df_CancerEntities_OverTime$MainCancerCode <- str_sub(df_CancerEntities_OverTime$MainCancerCode, 1, 3)


