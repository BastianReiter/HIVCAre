

################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: ANALYSIS (MAIN)                                                   #
#------------------------------------------------------------------------------#
################################################################################

#####Sex########################################################################
n_all <- sum(df_Sex$N)

df_female <- df_Sex %>% filter(Sex == "F")
n_female <- sum(df_female$N)

percent_female_all <- round((100/n_all)*n_female, 2)


u_site <- unique(df_Sex$Site)
subgroup <- unique(df_Sex$PatientSubgroup)
df_female_all_percent <- c("All", percent_female_all )

for (subg in subgroup) {
  df_temp_sub <- df_Sex %>% filter(PatientSubgroup %in% subg)
  
  n_sub <- sum(df_temp_sub$N)
  
  df_female <- df_temp_sub %>% filter(Sex == "F")
  n_female <- sum(df_female$N)
  
  percent_female <- round((100/n_sub)*n_female, 2)
  #print(percent_female)
  df_female_temp <- c(subg, percent_female)
  
  df_female_all_percent <- rbind(df_female_all_percent , df_female_temp)
}

df_female_all_percent  <- as.data.frame(df_female_all_percent)
df_female_all_percent <- df_female_all_percent %>% rename( PateintSubgroup = V1,
                                                           PercentFemale = V2)

df_female_site_percent <- c()

for (site in u_site) {
  df_temp_sex <- df_Sex %>% filter(Site %in% site)
  
  n_all_site <- sum(df_temp_sex$N)
  
  df_temp_sex_f <- df_temp_sex %>% filter(Sex == "F")
  n_female_site <- sum(df_temp_sex_f$N)
  
  percent_female_site <- round((100/n_all_site)*n_female_site, 2)
  
  percent_site <- c(site, percent_female_site)
  df_female_site_percent <- rbind(df_female_site_percent, percent_site)
  
  for (sub in subgroup) {
    df_temp_sex_sub <- df_temp_sex %>% filter(PatientSubgroup %in% sub)
    
    
    n_sub <- sum(df_temp_sex_sub$N)
    
    df_female_sub <- df_temp_sex_sub %>% filter(Sex == "F")
    n_female <- sum(df_female_sub$N)
    
    percent_female <- round((100/n_sub)*n_female, 2)
    #print(percent_female)
    df_female_temp <- c(subg, percent_female)
    
    df_female_site_percent <- rbind(df_female_site_percent, df_female_temp)
    
    
  }
  
}

df_female_site_percent  <- as.data.frame(df_female_site_percent)
df_female_site_percent <- df_female_site_percent %>% rename( PateintSubgroup = V1,
                                                           PercentFemale = V2)

####Age#########################################################################

subgroup <- unique(df_Age$PatientSubgroup)
df_Age_Subgruoup <- c()

for(s in subgroup){
  df_temp <- df_Age %>% filter(PatientSubgroup %in% s)
  
  n <- nrow(df_temp)
  
  a <- df_temp$N[1]
  b <- df_temp$N[2]
  c <- df_temp$N[3]
  d <- df_temp$N[4]
  
  m1 <- df_temp$Mean[1]
  m2 <- df_temp$Mean[2]
  m3 <- df_temp$Mean[3]
  m4 <- df_temp$Mean[4]
  
  w_mean <- round((a*m1 + b*m2 + c*m3 + d*m4)/(a+b+c+d), 2)
  #print(w_mean)
  
  w_sd_numerator <- (a*(m1-w_mean)^2) + (b*(m2-w_mean)^2) + (c*(m3-w_mean)^2) + (d*(m4-w_mean)^2)
  w_sd_denumerator <- (n-1/n)*(a+b+c+d)
  
  w_sd <- round(sqrt(w_sd_numerator / w_sd_denumerator), 2)
  #print(w_sd)
  
  values_temp <- c(s, w_mean, w_sd)
  df_Age_Subgruoup <- rbind(df_Age_Subgruoup, values_temp)
}

df_Age_Subgruoup <- as.data.frame(df_Age_Subgruoup)
df_Age_Subgruoup <- df_Age_Subgruoup %>% rename(PatientsSubgroup = V1,
                                                WightedMean = V2, WightedSD = V3)


df_Age$Mean <- round(df_Age$Mean, 2)
df_Age$SD <- round(df_Age$SD, 2)

################################################################################
#mean Age at Cancer Diagnosis

#Formel Gesamtmittelwert (gewichteter Mittelwert) = (a*m1 + b*m2 + c*m3 + d*m4)/(a+b+c+d)
#Formel gewichtete Standartabweichung = Wurzel aus((a*(m1-w_mean)^2+ b*(m2-w_mean)^2+c*(m3-w_mean)^2+ d*(m4-w_mean)^2)/((Nw)-1/Nw)*(a+b+c+d))
subgroup <- unique(df_AgeAtCancerDiagnosis$PatientSubgroup)
df_AgeAtCancer_Subgruoup <- c()
for(s in subgroup){
  df_temp <- df_AgeAtCancerDiagnosis %>% filter(PatientSubgroup %in% s)
  
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
  df_AgeAtCancer_Subgruoup <- rbind(df_AgeAtCancer_Subgruoup, values_temp)
}

df_AgeAtCancer_Subgruoup <- as.data.frame(df_AgeAtCancer_Subgruoup)
df_AgeAtCancer_Subgruoup <- df_AgeAtCancer_Subgruoup %>% rename(PatientsSubgroup = V1,
                                                                WightedMean = V2, WightedSD = V3)


df_AgeAtCancerDiagnosis$Mean <- round(df_AgeAtCancerDiagnosis$Mean, 2)
df_AgeAtCancerDiagnosis$SD <- round(df_AgeAtCancerDiagnosis$SD, 2)



###matched
subgroup <- unique(df_AgeAtCancerDiagnosisM$PatientSubgroup)
df_AgeAtCancer_SubgruoupM <- c()
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
  df_AgeAtCancer_SubgruoupM <- rbind(df_AgeAtCancer_SubgruoupM, values_temp)
}

df_AgeAtCancer_SubgruoupM <- as.data.frame(df_AgeAtCancer_SubgruoupM)
df_AgeAtCancer_SubgruoupM <- df_AgeAtCancer_SubgruoupM %>% rename(PatientsSubgroup = V1,
                                                WightedMean = V2, WightedSD = V3)


df_AgeAtCancerDiagnosisM$Mean <- round(df_AgeAtCancerDiagnosisM$Mean, 2)
df_AgeAtCancerDiagnosisM$SD <- round(df_AgeAtCancerDiagnosisM$SD, 2)


###############################################################################



# SampleSize

subgroup <- unique(df_SampleSize$PatientSubgroup)
output_line <- c()

for(s in subgroup){
  df_temp <- df_SampleSize %>% 
    filter(PatientSubgroup %in% s) 
  
  year <- unique(df_temp$FirstRelevantAdmissionYear)
  for(y in year){
    df_temp_y <- df_temp %>% 
      filter(FirstRelevantAdmissionYear %in% y)
    
    n <- nrow(df_temp_y)
    sum <- sum(df_temp_y$N)
    site <- "All"
    row <- c(s, y, sum, site, n)
    output_line <- rbind(output_line, row)
  }
}

df_SamplSizePerYearOverall <- as.data.frame(output_line) %>% 
  rename("PatientSubgroup" = "V1",  "FirstRelevantAdmissionYear" = "V2",  "N" = "V3",  "Site" = "V4",  "NumberOfSites" = "V5")

df_SamplSizePerYearOverall$N <- as.numeric(df_SamplSizePerYearOverall$N)

neworder <- c("Cancer+/HIV-", "Cancer+/HIV+", "Cancer-/HIV+")
df_SamplSizePerYearOverall$N <- as.integer(df_SamplSizePerYearOverall$N)

df_SamplSizePerYearOverall <- arrange(transform(df_SamplSizePerYearOverall,
                                                PatientSubgroup=factor(PatientSubgroup,levels=neworder)),PatientSubgroup)

ggplot(df_SamplSizePerYearOverall, aes(FirstRelevantAdmissionYear, N, fill= PatientSubgroup)) + 
  geom_bar(stat='identity') + scale_fill_brewer(palette="Pastel1") +
  facet_wrap(vars(PatientSubgroup), scales = "free") +ggtitle("Sample size")
 
ggplot(df_SamplSizePerYearOverall, aes(FirstRelevantAdmissionYear, NumberOfSites, fill= PatientSubgroup)) + 
  geom_bar(stat='identity') + scale_fill_brewer(palette="Pastel1") +
  facet_wrap(vars(PatientSubgroup), scales = "free") + ggtitle("Number of locations")

write.csv2(df_SamplSizePerYearOverall, "../../HIVandCancer/Analyse/Main/Plots/Beispiel Daten/df_samplesize.csv", row.names = FALSE)


####################################################################################

subgroup <- unique(df_Sex$PatientSubgroup)
output_line <- c()

for(s in subgroup){
  df_temp <- df_Sex %>% 
    filter(PatientSubgroup %in% s) 
  
  year <- unique(df_temp$FirstRelevantAdmissionYear)
  for(y in year){
    df_temp_y <- df_temp %>% 
      filter(FirstRelevantAdmissionYear %in% y)
  
      
    df_male <- df_temp_y %>% filter(Sex == "M")
    n_male <- sum(df_male$N, na.rm = TRUE)
    line_male <- c(s, y, "M", n_male)
    
    df_female <- df_temp_y %>% filter(Sex == "F")
    n_female <- sum(df_female$N, na.rm = TRUE)
    line_female <- c(s, y, "F", n_female)
    
    df_unknown <- df_temp_y %>% filter(Sex == "Unknow")
    n_unknown <- sum(df_unknown$N, na.rm = TRUE)
    line_unknown <- c(s, y, "Unknown", n_unknown)
  
  line <- rbind(line_male, line_female, line_unknown)
    
    
    output_line <- rbind(output_line, line)
  }
}

df_SexPlot <- as.data.frame(output_line) %>% 
  rename(PatientSubgroup = V1, FirstRelevantAdmissionYear = V2, Sex= V3,
          N = V4)

df_SexPlot$N <- as.integer(df_SexPlot$N)


df_SexPlot  <- arrange(transform(df_SexPlot,
          PatientSubgroup=factor(PatientSubgroup,levels=neworder)),PatientSubgroup)


ggplot(df_SexPlot) +
  aes(x = FirstRelevantAdmissionYear, fill = Sex, weight = N) +
  scale_fill_brewer(palette="BuGn") +
  geom_bar(position = "fill")+ facet_grid(cols = vars(PatientSubgroup))+
  ggtitle("Gender")

write.csv2(df_SexPlot, "../../HIVandCancer/Analyse/Main/Plots/Beispiel Daten/df_gender.csv", row.names = FALSE)



#####Age########################################################################
subgroup <- unique(df_AgeOverTime$PatientSubgroup)
output_line <- c()

for(s in subgroup){
  df_temp <- df_AgeOverTime %>% 
    filter(PatientSubgroup %in% s) 
  
  year <- unique(df_temp$FirstRelevantAdmissionYear)
  for(y in year){
    df_temp_y <- df_temp %>% 
      filter(FirstRelevantAdmissionYear %in% y)
    
    
    df_ag1 <- df_temp_y %>% filter(AgeGroup == "18 - 39 years old")
    n_ag1 <- sum(df_ag1$N, na.rm = TRUE)
    line_ag1 <- c(s, y, "18 - 39 years old", n_ag1)
    
    df_ag2 <- df_temp_y %>% filter(AgeGroup == "40 - 59 years old")
    n_ag2 <- sum(df_ag2$N, na.rm = TRUE)
    line_ag2 <- c(s, y, "40 - 59 years old", n_ag2)
    
    df_ag3 <- df_temp_y %>% filter(AgeGroup == "60 - 79 years old")
    n_ag3 <- sum(df_ag3$N, na.rm = TRUE)
    line_ag3 <- c(s, y, "60 - 79 years old", n_ag3)
    
    df_ag4 <- df_temp_y %>% filter(AgeGroup== "> 80 years old")
    n_ag4 <- sum(df_ag4$N, na.rm = TRUE)
    line_ag4 <- c(s, y, "> 80 years old", n_ag4)
    
    df_unknown <- df_temp_y %>% filter(is.na(AgeGroup))
    n_unknown <- sum(df_unknown$N, na.rm = TRUE)
    line_unknown <- c(s, y, "NA", n_unknown)
    
    line <- rbind(line_ag1, line_ag2, line_ag3, line_ag4,  line_unknown)
    
    
    output_line <- rbind(output_line, line)
  }
}

df_AgeOverTimePlot <- as.data.frame(output_line) %>% 
  rename(PatientSubgroup = V1, FirstRelevantAdmissionYear = V2, AgeGroup= V3,
         N = V4)

df_AgeOverTimePlot$N <- as.integer(df_AgeOverTimePlot$N)

neworder_age <- c("> 80 years old", "60 - 79 years old","40 - 59 years old","18 - 39 years old", "NA")

df_AgeOverTimePlot  <- arrange(transform(df_AgeOverTimePlot,
                                 PatientSubgroup=factor(PatientSubgroup,levels=neworder)),PatientSubgroup)

df_AgeOverTimePlot  <- arrange(transform(df_AgeOverTimePlot,
                                         AgeGroup=factor(AgeGroup,levels=neworder_age)),AgeGroup)



ggplot(df_AgeOverTimePlot) +
  aes(x = FirstRelevantAdmissionYear, fill = AgeGroup, weight = N) +
  scale_fill_brewer(palette="BuGn") +
  geom_bar(position = "fill")+ facet_grid(cols = vars(PatientSubgroup))+
  ggtitle("Age over Time")

write.csv2(df_AgeOverTimePlot, "../../HIVandCancer/Analyse/Main/Plots/Beispiel Daten/df_age.csv", row.names = FALSE)



########

subgroup <- unique(df_LengOfStay$PatientSubgroup)
df_LengOfStay_Subgruoup <- c()

for(s in subgroup){
  df_temp <- df_LengOfStay %>% filter(PatientSubgroup %in% s)
  
  n <- nrow(df_temp)
  
  a <- df_temp$N[1]
  b <- df_temp$N[2]
  c <- df_temp$N[3]
  d <- df_temp$N[4]
  
  m1 <- df_temp$Mean[1]
  m2 <- df_temp$Mean[2]
  m3 <- df_temp$Mean[3]
  m4 <- df_temp$Mean[4]
  
  #w_mean <- round((a*m1 + b*m2 + c*m3)/(a+b+c), 2)
  w_mean <- round((a*m1 + b*m2 + c*m3 + d*m4)/(a+b+c+d), 2)
  #print(w_mean)
  
  #w_sd_numerator <- (a*(m1-w_mean)^2) + (b*(m2-w_mean)^2) + (c*(m3-w_mean)^2)
  #w_sd_denumerator <- (n-1/n)*(a+b+c)
  
  w_sd_numerator <- (a*(m1-w_mean)^2) + (b*(m2-w_mean)^2) + (c*(m3-w_mean)^2) + (d*(m4-w_mean)^2)
  w_sd_denumerator <- (n-1/n)*(a+b+c+d)
  w_sd <- round(sqrt(w_sd_numerator / w_sd_denumerator), 2)
  #print(w_sd)
  
  values_temp <- c(s, w_mean, w_sd)
  df_LengOfStay_Subgruoup <- rbind(df_LengOfStay_Subgruoup, values_temp)
}

df_LengOfStay_Subgruoup <- as.data.frame(df_LengOfStay_Subgruoup)
df_LengOfStay_Subgruoup <- df_LengOfStay_Subgruoup %>% rename(PatientsSubgroup = V1,
                                                WightedMean = V2, WightedSD = V3)


df_LengOfStay_Subgruoup$Mean <- round(df_LengOfStay_Subgruoup$WightedMean, 2)
df_LengOfStay_Subgruoup$SD <- round(df_LengOfStay_Subgruoup$WightedSD, 2)

df_LengOfStay$Mean <- round(df_LengOfStay$Mean, 2)
df_LengOfStay$SD <- round(df_LengOfStay$SD, 2)

##########################
subgroup <- unique(df_CaseCount$PatientSubgroup)
df_CaseCount_Subgruoup <- c()

for(s in subgroup){
  df_temp <- df_CaseCount %>% filter(PatientSubgroup %in% s)
  
  n <- nrow(df_temp)
  
  a <- df_temp$N[1]
  b <- df_temp$N[2]
  c <- df_temp$N[3]
  d <- df_temp$N[4]
  
  m1 <- df_temp$Mean[1]
  m2 <- df_temp$Mean[2]
  m3 <- df_temp$Mean[3]
  m4 <- df_temp$Mean[4]
  
  
  w_mean <- round((a*m1 + b*m2 + c*m3 + d*m4)/(a+b+c+d), 2)
  #print(w_mean)
  
 
  w_sd_numerator <- (a*(m1-w_mean)^2) + (b*(m2-w_mean)^2) + (c*(m3-w_mean)^2) + (d*(m4-w_mean)^2)
  w_sd_denumerator <- (n-1/n)*(a+b+c+d)
  w_sd <- round(sqrt(w_sd_numerator / w_sd_denumerator), 2)
  #print(w_sd)
  
  values_temp <- c(s, w_mean, w_sd)
  df_CaseCount_Subgruoup <- rbind(df_CaseCount_Subgruoup, values_temp)
}

df_CaseCount_Subgruoup <- as.data.frame(df_CaseCount_Subgruoup)
df_CaseCount_Subgruoup <- df_CaseCount_Subgruoup %>% rename(PatientsSubgroup = V1,
                                                              WightedMean = V2, WightedSD = V3)


df_CaseCount_Subgruoup$Mean <- round(df_CaseCount_Subgruoup$WightedMean, 2)
df_CaseCount_Subgruoup$SD <- round(df_CaseCount_Subgruoup$WightedSD, 2)

df_CaseCount$Mean <- round(df_CaseCount$Mean, 2)
df_CaseCount$SD <- round(df_CaseCount$SD, 2)


################################################################################
# Cancer Topography
subgroup <- c("Cancer+/HIV-", "Cancer+/HIV+")
output_line <- c()
output_table <- c()

for(s in subgroup){
  df_temp <-df_CancerTopography_OverTime  %>% 
    filter(PatientSubgroup %in% s) 
  
  year <- unique(df_temp$MainCancerDiagnosisYear)
  for(y in year){
    df_temp_y <- df_temp %>% 
      filter(MainCancerDiagnosisYear %in% y)
    
    u_icd <- unique(df_temp_y$MainCancerTopographyGroup)
    
    
    for (icd in u_icd) {
      df_temp_icd <- df_temp_y %>% 
        filter(MainCancerTopographyGroup%in% icd)
      
      line_icd <- c(s, icd, y, sum(df_temp_icd$N))
      output_line <- rbind(output_line, line_icd)
    }
    
    output_table <- rbind(output_table, output_line)
    
    
    
  }
  
}

df_All_CancerEntities_OverTime_Group <- as.data.frame(output_table)%>% 
  rename(PatientSubgroup = V1, MainCancerTopographyGroup = V2, MainCancerDiagnosisYear= V3,
         N = V4)

df_All_CancerEntities_OverTime_Group$N <- as.integer(df_All_CancerEntities_OverTime_Group$N)

df_All_CancerEntities_OverTime_GroupPlot  <- arrange(transform(df_All_CancerEntities_OverTime_Group,
                                         PatientSubgroup=factor(PatientSubgroup,levels=neworder)),PatientSubgroup)

#metadata_topography <- read_excel("Data/MetaData/MetaData_CancerCoding.xlsx", na="NA")

#df_All_CancerEntities_OverTime_GroupTopo <- left_join(df_All_CancerEntities_OverTime_GroupPlot, metadata_topography[,c(1:4)], by=c("MainCancerCode"="ICDCodeShort"))
df_All_CancerEntities_OverTime_GroupTopo <- df_All_CancerEntities_OverTime_GroupPlot

df_plot_CancerTopography <- df_All_CancerEntities_OverTime_GroupTopo %>%
  group_by(PatientSubgroup, MainCancerTopographyGroup) %>% 
  summarise(N = as.integer(sum(N))) %>% group_by(PatientSubgroup) %>% mutate(Proportion = N / sum(N)) 

vc_TopographyDetailSorted <- df_plot_CancerTopography %>%
  filter(PatientSubgroup == "Cancer+/HIV+") %>%
  slice_max(n = 50,
            order_by = Proportion) %>%
  pull(MainCancerTopographyGroup)

 
plot_Output_CancerTopographyDetail <- arrange(transform(df_plot_CancerTopography,
                        MainCancerTopographyGroup=factor(MainCancerTopographyGroup,levels=vc_TopographyDetailSorted)), MainCancerTopographyGroup) 


ggplot(plot_Output_CancerTopographyDetail) +
  aes(x = MainCancerTopographyGroup , fill = PatientSubgroup, weight = Proportion) +
  #scale_fill_brewer(palette="BuGn") +
  geom_bar(position = "dodge")+ 
  scale_x_discrete(guide = guide_axis(n.dodge = 2))+ #facet_grid(cols = vars(PatientSubgroup), scales = "free") +
  ggtitle("Cancer Topography")

write.csv2(plot_Output_CancerTopographyDetail, "../../HIVandCancer/Analyse/Main/Plots/Beispiel Daten/df_CancerTopography_Subgroups.csv", row.names = FALSE)


