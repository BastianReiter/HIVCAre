


################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: AGGREGATE and PROCESS DATA (MAIN)                                 #
#------------------------------------------------------------------------------#
################################################################################

############ Demographics ######################################################

# Age (Number of patients)
df_AgeCologne <- SiteOutputData_Full$Cologne$df_Output_Age_Summary
df_AgeCologne$Site <- "Cologne"

df_AgeFrankfurt <- SiteOutputData_Full$Frankfurt$df_Output_Age_Summary
df_AgeFrankfurt$Site <- "Frankfurt"

df_AgeFreiburg <- SiteOutputData_Full$Freiburg$df_Output_Age_Summary
df_AgeFreiburg$Site <- "Freiburg"

df_AgeMunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_Age_Summary
df_AgeMunichLMU$Site <- "MunichLMU"

df_Age <- rbind(df_AgeCologne, df_AgeFrankfurt, df_AgeFreiburg, df_AgeMunichLMU)

# Age over Time
df_AgeOverTimeCologne <-SiteOutputData_Full$Cologne$df_Output_Age
df_AgeOverTimeCologne$Site <- "Cologne"
names(df_AgeOverTimeCologne)[2] <- "FirstRelevantAdmissionYear"

df_AgeOverTimeFrankfurt <- SiteOutputData_Full$Frankfurt$df_Output_Age
df_AgeOverTimeFrankfurt$Site <- "Frankfurt"

df_AgeOverTimeFreiburg <- SiteOutputData_Full$Freiburg$df_Output_Age
df_AgeOverTimeFreiburg$Site <- "Freiburg"

df_AgeOverTimeMunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_Age
df_AgeOverTimeMunichLMU$Site <- "MunichLMU"

df_AgeOverTime <- rbind(df_AgeOverTimeCologne, df_AgeOverTimeFrankfurt, 
                        df_AgeOverTimeFreiburg, df_AgeOverTimeMunichLMU)

# Sex
df_SexCologne <- SiteOutputData_Full$Cologne$df_Output_Sex
df_SexCologne$Site <- "Cologne"

names(df_SexCologne)[2] <- "FirstRelevantAdmissionYear"

df_SexCologne <-  df_SexCologne %>% mutate(Sex = case_when(
                                            Sex == "w" ~ "F", Sex == "m" ~ "M"
))


df_SexFrankfurt <- SiteOutputData_Full$Frankfurt$df_Output_Sex
df_SexFrankfurt$Site <- "Frankfurt"

df_SexFreiburg <- SiteOutputData_Full$Freiburg$df_Output_Sex
df_SexFreiburg$Site <- "Freiburg"

df_SexMunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_Sex
df_SexMunichLMU$Site <- "MunichLMU"

df_Sex <- rbind(df_SexCologne, df_SexFrankfurt, df_SexFreiburg, df_SexMunichLMU)




# Age at Cancer Diagnosis
df_AgeAtCancerCologne <- SiteOutputData_Full$Cologne$df_Output_AgeAtCancerDiagnosis_Summary
df_AgeAtCancerCologne$Site <- "Cologne"

df_AgeAtCancerFrankfurt <- SiteOutputData_Full$Frankfurt$df_Output_AgeAtCancerDiagnosis_Summary
df_AgeAtCancerFrankfurt$Site <- "Frankfurt"

df_AgeAtCancerFreiburg <- SiteOutputData_Full$Freiburg$df_Output_AgeAtCancerDiagnosis_Summary
df_AgeAtCancerFreiburg$Site <- "Freiburg"

df_AgeAtCancerMunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_AgeAtCancerDiagnosis_Summary
df_AgeAtCancerMunichLMU$Site <- "MunichLMU"

df_AgeAtCancerDiagnosis <- rbind(df_AgeAtCancerCologne, df_AgeAtCancerFrankfurt, df_AgeAtCancerFreiburg, df_AgeAtCancerMunichLMU)

###############################################################################
# Age at Cancer Diagnosis matched Data
df_AgeAtCancerCologneM <- SiteOutputData_Matched$Cologne$df_Output_AgeAtCancerDiagnosis_Summary
df_AgeAtCancerCologneM$Site <- "Cologne"

df_AgeAtCancerFrankfurtM <- SiteOutputData_Matched$Frankfurt$df_Output_AgeAtCancerDiagnosis_Summary
df_AgeAtCancerFrankfurtM$Site <- "Frankfurt"

df_AgeAtCancerFreiburgM <- SiteOutputData_Matched$Freiburg$df_Output_AgeAtCancerDiagnosis_Summary
df_AgeAtCancerFreiburgM$Site <- "Freiburg"

df_AgeAtCancerMunichLMUM <- SiteOutputData_Matched$MunichLMU$df_Output_AgeAtCancerDiagnosis_Summary
df_AgeAtCancerMunichLMUM$Site <- "MunichLMU"

df_AgeAtCancerDiagnosisM <- rbind(df_AgeAtCancerCologneM, df_AgeAtCancerFrankfurtM, df_AgeAtCancerFreiburgM, df_AgeAtCancerMunichLMUM)




# samplesize
df_SampleSizeCologne <- SiteOutputData_Full$Cologne$df_Output_SampleSize
df_SampleSizeCologne$Site <- "Cologne"
names(df_SampleSizeCologne)[2] <- "FirstRelevantAdmissionYear"

df_SampleSizeFrankfurt <- SiteOutputData_Full$Frankfurt$df_Output_SampleSize
df_SampleSizeFrankfurt$Site <- "Frankfurt"

df_SampleSizeFreiburg <- SiteOutputData_Full$Freiburg$df_Output_SampleSize
df_SampleSizeFreiburg$Site <- "Freiburg"

df_SampleSizeMunichLMU<- SiteOutputData_Full$MunichLMU$df_Output_SampleSize
df_SampleSizeMunichLMU$Site <- "MunichLMU"

df_SampleSize <- rbind(df_SampleSizeCologne, df_SampleSizeFrankfurt, df_SampleSizeFreiburg, df_SampleSizeMunichLMU)

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
# Case Count
df_CaseCount_Cologne <- SiteOutputData_Full$Cologne$df_Output_CaseCount_Summary
df_CaseCount_Cologne$Site <- "Cologne"

df_CaseCount_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_CaseCount_Summary
df_CaseCount_Frankfurt$Site <- "Frankfurt"

df_CaseCount_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_CaseCount_Summary
df_CaseCount_Freiburg$Site <- "Freiburg"

df_CaseCount_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_CaseCount_Summary
df_CaseCount_MunichLMU$Site <- "MunichLMU"

df_CaseCount <- rbind(df_CaseCount_Cologne, df_CaseCount_Frankfurt, df_CaseCount_Freiburg, df_CaseCount_MunichLMU)




################################################################################
###################### Cancer Entites  #########################################
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
# Filter from 2009 -2021

df_CancerEntities_OverTime_filtered <- subset(df_CancerEntities_OverTime, (MainCancerDiagnosisYear>2008 & MainCancerDiagnosisYear <2022) & PatientSubgroup == "Cancer+/HIV+")

df_plot_CancerEntities_OverTime <- c()
 u_icd <- unique(df_CancerEntities_OverTime_filtered$MainCancerCode)
 for(ent in u_icd){
   df_temp_sub_ent <- df_CancerEntities_OverTime_filtered %>% filter(MainCancerCode %in% ent)
   
   count_2009 <- sum(df_temp_sub_ent[which((df_temp_sub_ent[,3] == 2009)),4])
   count_2010 <- sum(df_temp_sub_ent[which((df_temp_sub_ent[,3] == 2010)),4])
   count_2011 <- sum(df_temp_sub_ent[which((df_temp_sub_ent[,3] == 2011)),4])
   count_2012 <- sum(df_temp_sub_ent[which((df_temp_sub_ent[,3] == 2012)),4])
   count_2013 <- sum(df_temp_sub_ent[which((df_temp_sub_ent[,3] == 2013)),4])
   count_2014 <- sum(df_temp_sub_ent[which((df_temp_sub_ent[,3] == 2014)),4])
   count_2015 <- sum(df_temp_sub_ent[which((df_temp_sub_ent[,3] == 2015)),4])
   count_2016 <- sum(df_temp_sub_ent[which((df_temp_sub_ent[,3] == 2016)),4])
   count_2017 <- sum(df_temp_sub_ent[which((df_temp_sub_ent[,3] == 2017)),4])
   count_2018 <- sum(df_temp_sub_ent[which((df_temp_sub_ent[,3] == 2018)),4])
   count_2019 <- sum(df_temp_sub_ent[which((df_temp_sub_ent[,3] == 2019)),4])
   count_2020 <- sum(df_temp_sub_ent[which((df_temp_sub_ent[,3] == 2020)),4])
   count_2021 <- sum(df_temp_sub_ent[which((df_temp_sub_ent[,3] == 2021)),4])
   
   subg<- "Cancer+/HIV+"
   df_ent <- data.frame("PatientSubgroup" = c( subg, subg, subg, subg, subg, subg, subg, subg, subg, subg, subg, subg, subg),
                        "MainCancerCode"= c( ent, ent, ent, ent, ent, ent, ent, ent, ent, ent, ent, ent, ent),
                        "MainCancerDiagnosisYear"=c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"),
                        "N" = c( count_2009, count_2010, count_2011, count_2012, count_2013, count_2014, count_2015, count_2016, count_2017, count_2018, count_2019, count_2020, count_2021)
                        )
   
   df_plot_CancerEntities_OverTime <- rbind(df_plot_CancerEntities_OverTime, df_ent)
 }
 
 df_plot_CancerEntities_OverTime <- as.data.frame( df_plot_CancerEntities_OverTime)

ggplot(df_plot_CancerEntities_OverTime, aes(x= MainCancerDiagnosisYear, y= N, group = 1)) +
  geom_line()+
   facet_wrap(vars(MainCancerCode))

###################### Cancer Topographie Detail################################
df_CancerTopography_OverTime_Cologne <- SiteOutputData_Full$Cologne$df_Output_CancerTopographyDetail_OverTime
df_CancerTopography_OverTime_Cologne$Site <- "Cologne"
names(df_CancerTopography_OverTime_Cologne)[2] <- "MainCancerTopographyDetail"
names(df_CancerTopography_OverTime_Cologne)[3] <- "MainCancerDiagnosisYear"

df_CancerTopography_OverTime_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_MainCancerTopographyDetail_OverTime
df_CancerTopography_OverTime_Frankfurt$Site <- "Frankfurt"

df_CancerTopography_OverTime_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_MainCancerTopographyDetail_OverTime
df_CancerTopography_OverTime_Freiburg$Site <- "Freiburg"

df_CancerTopography_OverTime_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_MainCancerTopographyDetail_OverTime
df_CancerTopography_OverTime_MunichLMU$Site <- "MunichLMU"

df_CancerTopography_OverTime <- rbind(df_CancerTopography_OverTime_Cologne, df_CancerTopography_OverTime_Frankfurt, df_CancerTopography_OverTime_Freiburg, df_CancerTopography_OverTime_MunichLMU)


# Filter from 2009 -2021
df_CancerTopography_OverTime_filtered <- subset(df_CancerTopography_OverTime, (MainCancerDiagnosisYear>2008 & MainCancerDiagnosisYear <2022) & PatientSubgroup == "Cancer+/HIV+")

df_plot_CancerTopography_OverTime <- c()
u_topo <- unique(df_CancerTopography_OverTime_filtered$MainCancerTopographyDetail)
for(topo in u_topo){
  df_temp_topo <- df_CancerTopography_OverTime_filtered %>% filter(MainCancerTopographyDetail %in% topo)
  
  count_2009 <- sum(df_temp_topo[which((df_temp_topo [,3] == 2009)),4])
  count_2010 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2010)),4])
  count_2011 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2011)),4])
  count_2012 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2012)),4])
  count_2013 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2013)),4])
  count_2014 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2014)),4])
  count_2015 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2015)),4])
  count_2016 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2016)),4])
  count_2017 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2017)),4])
  count_2018 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2018)),4])
  count_2019 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2019)),4])
  count_2020 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2020)),4])
  count_2021 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2021)),4])
  
  df_topo <- data.frame("PatientSubgroup" = c( subg, subg, subg, subg, subg, subg, subg, subg, subg, subg, subg, subg, subg),
                       "MainCancerTopography"= c( topo, topo, topo, topo, topo, topo, topo, topo, topo, topo, topo, topo, topo),
                       "MainCancerDiagnosisYear"=c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"),
                       "N" = c( count_2009, count_2010, count_2011, count_2012, count_2013, count_2014, count_2015, count_2016, count_2017, count_2018, count_2019, count_2020, count_2021)
  )
  
  df_plot_CancerTopography_OverTime <- rbind(df_plot_CancerTopography_OverTime, df_topo)
}

df_plot_CancerTopography_OverTime <- as.data.frame(df_plot_CancerTopography_OverTime)

ggplot(df_plot_CancerTopography_OverTime, aes(x= MainCancerDiagnosisYear, y= N, group = 1)) +
  geom_line()+
  facet_wrap(vars(MainCancerTopography))


###################### Cancer Topographie Group #########################################
df_CancerTopography_OverTime_Cologne <- SiteOutputData_Full$Cologne$df_Output_CancerTopographyGroup_OverTime
df_CancerTopography_OverTime_Cologne$Site <- "Cologne"
names(df_CancerTopography_OverTime_Cologne)[2] <- "MainCancerTopographyGroup"
names(df_CancerTopography_OverTime_Cologne)[3] <- "MainCancerDiagnosisYear"

df_CancerTopography_OverTime_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_MainCancerTopographyGroup_OverTime
df_CancerTopography_OverTime_Frankfurt$Site <- "Frankfurt"

df_CancerTopography_OverTime_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_MainCancerTopographyGroup_OverTime
df_CancerTopography_OverTime_Freiburg$Site <- "Freiburg"

df_CancerTopography_OverTime_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_MainCancerTopographyGroup_OverTime
df_CancerTopography_OverTime_MunichLMU$Site <- "MunichLMU"

df_CancerTopography_OverTime <- rbind(df_CancerTopography_OverTime_Cologne, df_CancerTopography_OverTime_Frankfurt, df_CancerTopography_OverTime_Freiburg, df_CancerTopography_OverTime_MunichLMU)


# Filter from 2009 -2021
df_CancerTopography_OverTime_filtered <- subset(df_CancerTopography_OverTime, (MainCancerDiagnosisYear>2008 & MainCancerDiagnosisYear <2022) & PatientSubgroup == "Cancer+/HIV+")

df_plot_CancerTopography_OverTime <- c()
u_topo <- unique(df_CancerTopography_OverTime_filtered$MainCancerTopographyGroup)
for(topo in u_topo){
  df_temp_topo <- df_CancerTopography_OverTime_filtered %>% filter(MainCancerTopographyGroup %in% topo)
  
  count_2009 <- sum(df_temp_topo[which((df_temp_topo [,3] == 2009)),4])
  count_2010 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2010)),4])
  count_2011 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2011)),4])
  count_2012 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2012)),4])
  count_2013 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2013)),4])
  count_2014 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2014)),4])
  count_2015 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2015)),4])
  count_2016 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2016)),4])
  count_2017 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2017)),4])
  count_2018 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2018)),4])
  count_2019 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2019)),4])
  count_2020 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2020)),4])
  count_2021 <- sum(df_temp_topo[which((df_temp_topo[,3] == 2021)),4])
  
  df_topo <- data.frame("PatientSubgroup" = c( subg, subg, subg, subg, subg, subg, subg, subg, subg, subg, subg, subg, subg),
                        "MainCancerTopography"= c( topo, topo, topo, topo, topo, topo, topo, topo, topo, topo, topo, topo, topo),
                        "MainCancerDiagnosisYear"=c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"),
                        "N" = c( count_2009, count_2010, count_2011, count_2012, count_2013, count_2014, count_2015, count_2016, count_2017, count_2018, count_2019, count_2020, count_2021)
  )
  
  df_plot_CancerTopography_OverTime <- rbind(df_plot_CancerTopography_OverTime, df_topo)
}

df_plot_CancerTopographyGroup_OverTime <- as.data.frame(df_plot_CancerTopography_OverTime)

ggplot(df_plot_CancerTopographyGroup_OverTime, aes(x= MainCancerDiagnosisYear, y= N, group = 1)) +
  geom_line()+
  facet_wrap(vars(MainCancerTopography))

write.csv2(df_plot_CancerTopographyGroup_OverTime , "../../HIVandCancer/Analyse/Main/Plots/Beispiel Daten/df_TopographyGroup.csv", row.names = FALSE)



################ Cancer Group (AD, NAD, virus-NAD) #############################
df_HIVCancerCategories_OverTime_Cologne <- SiteOutputData_Full$Cologne$df_Output_HIVCancerCategories
df_HIVCancerCategories_OverTime_Cologne$Site <- "Cologne"
names(df_HIVCancerCategories_OverTime_Cologne)[3] <- "MainCancerDiagnosisYear"

df_HIVCancerCategories_OverTime_Frankfurt <- SiteOutputData_Full$Frankfurt$df_Output_HIVCancerCategories
df_HIVCancerCategories_OverTime_Frankfurt$Site <- "Frankfurt"

df_HIVCancerCategories_OverTime_Freiburg <- SiteOutputData_Full$Freiburg$df_Output_MainCancerTopographyGroup_OverTime
df_HIVCancerCategories_OverTime_Freiburg$Site <- "Freiburg"

df_HIVCancerCategories_OverTime_MunichLMU <- SiteOutputData_Full$MunichLMU$df_Output_MainCancerTopographyGroup_OverTime
df_HIVCancerCategories_OverTime_MunichLMU$Site <- "MunichLMU"

df_HIVCancerCategories_OverTime <- rbind(df_HIVCancerCategories_OverTime_Cologne, df_HIVCancerCategories_OverTime_Frankfurt, df_HIVCancerCategories_OverTime_Freiburg, df_HIVCancerCategories_OverTime_MunichLMU)


# Filter from 2009 -2021
df_HIVCancerCategories_OverTime_filtered <- subset(df_HIVCancerCategories_OverTime, (MainCancerDiagnosisYear>2008 & MainCancerDiagnosisYear <2022))

df_plot_HIVCancerCategories_OverTime <- c()
#subgroup <- unique(df_HIVCancerCategories_OverTime_filtered$PatientSubgroup)
subgroup <- "Cancer+/HIV+"
for (sub in subgroup) {
  df_temp <- df_HIVCancerCategories_OverTime_filtered %>% filter(PatientSubgroup %in% sub)

u_cat <- unique(df_temp$PatientSubgroupHIVCancerCategory)
for(categories in u_cat){
  df_temp_cat <- df_temp %>% filter(PatientSubgroupHIVCancerCategory %in% categories)
  
  count_2009 <- sum(df_temp_cat[which((df_temp_cat [,3] == 2009)),4])
  count_2010 <- sum(df_temp_cat[which((df_temp_cat[,3] == 2010)),4])
  count_2011 <- sum(df_temp_cat[which((df_temp_cat[,3] == 2011)),4])
  count_2012 <- sum(df_temp_cat[which((df_temp_cat[,3] == 2012)),4])
  count_2013 <- sum(df_temp_cat[which((df_temp_cat[,3] == 2013)),4])
  count_2014 <- sum(df_temp_cat[which((df_temp_cat[,3] == 2014)),4])
  count_2015 <- sum(df_temp_cat[which((df_temp_cat[,3] == 2015)),4])
  count_2016 <- sum(df_temp_cat[which((df_temp_cat[,3] == 2016)),4])
  count_2017 <- sum(df_temp_cat[which((df_temp_cat[,3] == 2017)),4])
  count_2018 <- sum(df_temp_cat[which((df_temp_cat[,3] == 2018)),4])
  count_2019 <- sum(df_temp_cat[which((df_temp_cat[,3] == 2019)),4])
  count_2020 <- sum(df_temp_cat[which((df_temp_cat[,3] == 2020)),4])
  count_2021 <- sum(df_temp_cat[which((df_temp_cat[,3] == 2021)),4])
  
  df_categories <- data.frame("PatientSubgroup" = c( subg, subg, subg, subg, subg, subg, subg, subg, subg, subg, subg, subg, subg),
                        "PatientSubgroupHIVCancerCategory"= c( categories, categories, categories, categories, categories, categories, categories, categories, categories, categories, categories, categories, categories),
                        "MainCancerDiagnosisYear"=c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"),
                        "N" = c( count_2009, count_2010, count_2011, count_2012, count_2013, count_2014, count_2015, count_2016, count_2017, count_2018, count_2019, count_2020, count_2021)
  )
  
  df_plot_HIVCancerCategories_OverTime  <- rbind(df_plot_HIVCancerCategories_OverTime , df_categories)
}
}
df_plot_HIVCancerCategories_OverTime  <- as.data.frame(df_plot_HIVCancerCategories_OverTime )


ggplot(df_plot_HIVCancerCategories_OverTime, aes(x= MainCancerDiagnosisYear, y= N, group=1)) +
  geom_line()+
  facet_wrap(vars(PatientSubgroupHIVCancerCategory))