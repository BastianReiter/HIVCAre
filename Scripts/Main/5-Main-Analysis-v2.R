


################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: AGGREGATE and PROCESS DATA (MAIN)                                 #
#------------------------------------------------------------------------------#
################################################################################
source("./Scripts/SETUP.R")


################################################################################
#
#          Across all primary subgroups
#
################################################################################


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
  group_by(PatientSubgroup, FirstRelevantAdmissionYear) %>%
  summarise(
    N = sum(N),
    NSite = n_distinct(Site)) %>%
  rename(Year = FirstRelevantAdmissionYear)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sex distribution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_Sex_Main_Filtered <- filter(df_Output_Sex_Main, (FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end))

df_Plot_Sex_Main_Filtered <- df_Output_Sex_Main_Filtered %>% 
  group_by(PatientSubgroup, FirstRelevantAdmissionYear, Sex) %>%
  summarise(
    N = sum(N),
    NSite = n_distinct(Site),
    CountFemale = sum(Sex == "F", na.rm = TRUE),
    CountMale = sum(Sex == "M", na.rm = TRUE),
    CountOther = sum(Sex == "Unknown", na.rm = TRUE),
    RateFemale = sum(Sex == "F", na.rm = TRUE) / N,
    RateMale = sum(Sex == "M", na.rm = TRUE) / N,
    RateOther = sum(Sex == "Unknown", na.rm = TRUE) / N) %>%
  rename(Year = FirstRelevantAdmissionYear)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Age
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Age (at first main admission) distribution over time
df_Output_Age_Main_Filtered <- filter(df_Output_Age_Main, (FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end))

df_Plot_Age_Main_Filtered <- df_Output_Age_Main_Filtered %>% 
  group_by(PatientSubgroup, FirstRelevantAdmissionYear, AgeGroup) %>%
  summarise(
    N = sum(N),
    NSite = n_distinct(Site)) %>%
  rename(Year = FirstRelevantAdmissionYear)

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
  
df_Plot_CaseCount_Main_Filtered  <- df_Output_CaseCount_Main_Filtered %>%
  group_by(PatientSubgroup, FirstRelevantAdmissionYear, CaseCountGroup) %>%
  summarize( N = sum(N),
             NSite = n_distinct(Site))%>%
  rename(Year = FirstRelevantAdmissionYear)

# Not relevant for main analysis.
# Case count summary statistics
#df_Output_CaseCount_Summary_Main 

# Case count quantiles
#df_Output_CaseCount_Quantiles_Main  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mean length of stay
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_MeanLengthOfStay_Main_Filtered  <- filter(df_Output_MeanLengthOfStay_Main, (FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end))

df_Plot_MeanLengthOfStay_Main_Filtered <- df_Output_MeanLengthOfStay_Main_Filtered %>%
  group_by(PatientSubgroup, FirstRelevantAdmissionYear, MeanLengthOfStayGroup) %>%
  summarize( N = sum(N),
             NSite = n_distinct(Site))%>%
  rename(Year = FirstRelevantAdmissionYear)
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

df_Plot_AgeAtCancerDiagnosis_Main_Filtered <- df_Output_AgeAtCancerDiagnosis_Main_Filtered %>% 
  group_by(PatientSubgroup, MainCancerDiagnosisYear, AgeGroup) %>%
  summarise(
    N = sum(N),
    NSite = n_distinct(Site)) %>%
  rename(Year = MainCancerDiagnosisYear)


# Not relevant for main analysis.
#df_Output_AgeAtCancerDiagnosis_Summary_Main  

#df_Output_AgeAtCancerDiagnosis_Quantiles_Main  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Count of distinct documented "real" cancer codes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Not avible at Cologne
df_Output_DistinctCodeCountCancer_Main_Filtered <- filter(df_Output_DistinctCodeCountCancer_Matched_Main, (FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end))

df_Plot_DistinctCodeCountCancer_Main_Filtered  <- df_Output_DistinctCodeCountCancer_Main_Filtered %>%
  group_by(PatientSubgroup, FirstRelevantAdmissionYear, CancerCountGroup) %>%
  summarize( N = sum(N),
             NSite = n_distinct(Site))%>%
  rename(Year = FirstRelevantAdmissionYear)

# Not relevant for main analysis.  
#df_Output_DistinctCodeCountCancer_Summary_Main  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer grouping: AD, NAD and Non-HIV-associated cancer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_HIVCancerCategories_Main_Filtered <- filter(df_Output_HIVCancerCategories_Matched_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))

df_Plot_HIVCancerCategories_Main_Filtered <- df_Output_HIVCancerCategories_Main_Filtered %>%
  group_by(PatientSubgroup, PatientSubgroupHIVCancerCategory, MainCancerDiagnosisYear) %>%
  summarize( N = sum(N),
             NSite = n_distinct(Site))%>%
  rename(Year = MainCancerDiagnosisYear)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer grouping: Carcinoma in situ
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df_Output_CIS_Main_Filtered <- filter(df_Output_CIS_Matched_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))
 

df_Plot_CIS_Main_Filtered <- df_Output_CIS_Main_Filtered %>%
  group_by(PatientSubgroup, MainCancerDiagnosisYear, CancerIsCarcinomaInSitu) %>%
  summarize( N = sum(N),
             NSite = n_distinct(Site))%>%
  mutate(Proportion = N / sum(N)) %>%
  rename(Year = MainCancerDiagnosisYear)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: Topography by Organ
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Not relevant for main analysis.  
#df_Output_MainCancerTopographyDetail_Main  

# Data stratified by year of cancer diagnosis
df_Output_MainCancerTopographyDetail_OverTime_Main_Filtered <- filter(df_Output_MainCancerTopographyDetail_OverTime_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))


df_Plot_MainCancerTopographyDetail_Main_Filtered  <- df_Output_MainCancerTopographyDetail_OverTime_Main_Filtered  %>%
  group_by(PatientSubgroup, MainCancerTopographyDetail) %>%
  summarize(N = sum(N)) %>%
  group_by(PatientSubgroup) %>%
  mutate(Proportion = N / sum(N))

vc_TopographyDetailSorted <- df_Plot_MainCancerTopographyDetail_Main_Filtered %>%
  filter(PatientSubgroup == "Cancer+/HIV+") %>%
  slice_max(n = 15,
            order_by = Proportion) %>%
  pull(MainCancerTopographyDetail)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: Topography by ICD Grouping
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Not relevant for main analysis.  
#df_Output_MainCancerTopographyGroup_Main  

# Data stratified by year of cancer diagnosis
df_Output_MainCancerTopographyGroup_OverTime_Main_Filtered <- filter(df_Output_MainCancerTopographyGroup_OverTime_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))

df_Plot_MainCancerTopographyGroup_Main_Filtered  <- df_Output_MainCancerTopographyGroup_OverTime_Main_Filtered %>%
  group_by(PatientSubgroup, MainCancerTopographyGroup) %>%
  summarize(N = sum(N)) %>%
  group_by(PatientSubgroup) %>%
  mutate(Proportion = N / sum(N))

vc_TopographyGroupSorted <- df_Plot_MainCancerTopographyGroup_Main_Filtered %>%
  filter(PatientSubgroup == "Cancer+/HIV+") %>%
  slice_max(n = 12,
            order_by = Proportion) %>%
  pull(MainCancerTopographyGroup)


df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered  <- df_Output_MainCancerTopographyGroup_OverTime_Main_Filtered %>%
  group_by(PatientSubgroup, MainCancerTopographyGroup, MainCancerDiagnosisYear) %>%
  summarize(N = sum(N)) %>%
  group_by(PatientSubgroup)

df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered_HIV <- df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered %>%
  filter(PatientSubgroup == "Cancer+/HIV+" & MainCancerTopographyGroup %in% vc_TopographyGroupSorted) 

df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered_HIV$MainCancerTopographyGroup <- factor(df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered_HIV$MainCancerTopographyGroup, levels = vc_TopographyGroupSorted)


df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered_Controll <- df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered %>%
  filter(PatientSubgroup == "Cancer+/HIV-" & MainCancerTopographyGroup %in% vc_TopographyGroupSorted) 

df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered_Controll$MainCancerTopographyGroup <- factor(df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered_Controll$MainCancerTopographyGroup, levels = vc_TopographyGroupSorted)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: By entity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Not relevant for main analysis.  
# Cancer occurrence grouped by coded entity
#df_Output_CancerEntities_Main  

# Data stratified by year of cancer diagnosis
df_Output_CancerEntities_OverTime_Main_Filtered <- filter(df_Output_CancerEntities_OverTime_Matched_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))

df_Plot_CancerEntities_Main_Filtered  <- df_Output_CancerEntities_OverTime_Main_Filtered %>%
  group_by(PatientSubgroup, MainCancerCode) %>%
  summarize(N = sum(N)) %>%
  group_by(PatientSubgroup) %>%
  mutate(Proportion = N / sum(N))

vc_CancerEntitiesSorted <- df_Plot_CancerEntities_Main_Filtered %>%
  filter(PatientSubgroup == "Cancer+/HIV+") %>%
  slice_max(n = 50,
            order_by = Proportion) %>%
  pull(MainCancerCode)


df_Plot_CancerEntities_OverTime_Main_Filtered  <- df_Output_CancerEntities_OverTime_Main_Filtered %>%
  group_by(PatientSubgroup, MainCancerCode, MainCancerDiagnosisYear) %>%
  summarize(N = sum(N)) 

df_Plot_CancerEntities_OverTime_Main_Filtered_HIV <- df_Plot_CancerEntities_OverTime_Main_Filtered %>%
  filter(PatientSubgroup == "Cancer+/HIV+" & MainCancerCode %in% vc_CancerEntitiesSorted) 

df_Plot_CancerEntities_OverTime_Main_Filtered_HIV$MainCancerCode <- factor(df_Plot_CancerEntities_OverTime_Main_Filtered_HIV$MainCancerCode, levels = vc_CancerEntitiesSorted)

# Group Cancer Entities 3 digits
df_Output_CancerEntitiesGrouped_OverTime_Main_Filtered <- df_Output_CancerEntities_OverTime_Main_Filtered

df_Output_CancerEntitiesGrouped_OverTime_Main_Filtered$MainCancerCode <- substring(df_Output_CancerEntitiesGrouped_OverTime_Main_Filtered$MainCancerCode, 1, 3) 

df_Plot_CancerEntitiesGrouped_Main_Filtered  <- df_Output_CancerEntitiesGrouped_OverTime_Main_Filtered %>%
  group_by(PatientSubgroup, MainCancerCode) %>%
  summarize(N = sum(N)) %>%
  group_by(PatientSubgroup) %>%
  mutate(Proportion = N / sum(N))

vc_CancerEntitiesGroupedSorted <- df_Plot_CancerEntitiesGrouped_Main_Filtered %>%
  filter(PatientSubgroup == "Cancer+/HIV+") %>%
  slice_max(n = 20,
            order_by = Proportion) %>%
  pull(MainCancerCode)


df_Plot_CancerEntitiesGrouped_OverTime_Main_Filtered  <- df_Output_CancerEntitiesGrouped_OverTime_Main_Filtered %>%
  group_by(PatientSubgroup, MainCancerCode, MainCancerDiagnosisYear) %>%
  summarize(N = sum(N)) 

df_Plot_CancerEntitiesGrouped_OverTime_Main_Filtered_HIV <- df_Plot_CancerEntitiesGrouped_OverTime_Main_Filtered %>%
  filter(PatientSubgroup == "Cancer+/HIV+" & MainCancerCode %in% vc_CancerEntitiesGroupedSorted) 

df_Plot_CancerEntitiesGrouped_OverTime_Main_Filtered_HIV$MainCancerCode <- factor(df_Plot_CancerEntitiesGrouped_OverTime_Main_Filtered_HIV$MainCancerCode, levels = vc_CancerEntitiesGroupedSorted)




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

df_Plot_MetastasisOccurrence_Main_Filtered 
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

df_Output_HIVCancerDiagnosisOrder_Main_Filtered <- filter(df_Output_HIVCancerDiagnosisOrder_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))


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
df_Output_HIVCancerAIDS_Main_Filtered <- filter(df_Output_HIVCancerAIDS_Matched_Main, (MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end))
  


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

df_Output_AIDSOccurrence_Main_Filtered <- filter(df_Output_AIDSOccurrence_Main, (FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end))

