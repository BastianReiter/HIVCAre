

################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: OUTPUT (MAIN)                                                     #
#------------------------------------------------------------------------------#
################################################################################

df_output_DemographocData_unmatched <- data.frame(
  Demographic = c("Number of patients (N)", "Cancer+/HIV-", "Cancer+/HIV+", "Cancer-/HIV+",
                  "Female patients (%)", "Cancer+/HIV-", "Cancer+/HIV+", "Cancer-/HIV+",
                  "Age (mean)", "Cancer+/HIV-", "Cancer+/HIV+", "Cancer-/HIV+",
                  "Age (SD)", "Cancer+/HIV-", "Cancer+/HIV+", "Cancer-/HIV+",
                  "Age at cancer diagnosis (mean)", "Cancer+/HIV-", "Cancer+/HIV+", "Cancer-/HIV+",
                  "Age at cancer diagnosis(SD)", "Cancer+/HIV-", "Cancer+/HIV+", "Cancer-/HIV+"),
  All = c(sum(df_Age[13,2],df_Age[1,2], df_Age[5,2], df_Age[9,2]), 
          sum(df_Age[14,2], df_Age[2,2], df_Age[6,2], df_Age[10,2]),
          sum(df_Age[15,2], df_Age[3,2], df_Age[7,2],  df_Age[11,2]),
          sum(df_Age[16,2], df_Age[4,2], df_Age[8,2], df_Age[12,2]),
          df_female_all_percent[1,2], df_female_all_percent[2,2], df_female_all_percent[3,2], df_female_all_percent[4,2],
          df_Age_Subgruoup[1,2], df_Age_Subgruoup[2,2], df_Age_Subgruoup[3,2], df_Age_Subgruoup[4,2],
          df_Age_Subgruoup[1,3], df_Age_Subgruoup[2,3], df_Age_Subgruoup[3,3], df_Age_Subgruoup[4,3],
          df_AgeAtCancer_Subgruoup[1,2], df_AgeAtCancer_Subgruoup[2,2], df_AgeAtCancer_Subgruoup[3,2], "unmatched",
          df_AgeAtCancer_Subgruoup[1,3], df_AgeAtCancer_Subgruoup[2,3], df_AgeAtCancer_Subgruoup[3,3], "unmatched"),
  
  SiteA = c(as.numeric(df_Age[13,2]), as.numeric(df_Age[14,2]), as.numeric(df_Age[15,2]), as.numeric(df_Age[16,2]),
            df_female_site_percent[13,2], df_female_site_percent[14,2], df_female_site_percent[15,2], df_female_site_percent[16,2],
            as.numeric(df_Age[13,9]), as.numeric(df_Age[14,9]), as.numeric(df_Age[15,9]), as.numeric(df_Age[16,9]),
            as.numeric(df_Age[13,10]), as.numeric(df_Age[14,10]), as.numeric(df_Age[15,10]), as.numeric(df_Age[16,10]),
            as.numeric(df_AgeAtCancerDiagnosis[10,9]), as.numeric(df_AgeAtCancerDiagnosis[11,9]), as.numeric(df_AgeAtCancerDiagnosis[12,9]), "unmatched",
            as.numeric(df_AgeAtCancerDiagnosis[10,10]), as.numeric(df_AgeAtCancerDiagnosis[11,10]), as.numeric(df_AgeAtCancerDiagnosis[12,10]), "unmatched"),
 
   SiteB = c(as.numeric(df_Age[1,2]), as.numeric(df_Age[2,2]), as.numeric(df_Age[3,2]), as.numeric(df_Age[4,2]),
             df_female_site_percent[1,2], df_female_site_percent[2,2], df_female_site_percent[3,2], df_female_site_percent[4,2],
             as.numeric(df_Age[1,9]), as.numeric(df_Age[2,9]), as.numeric(df_Age[3,9]), as.numeric(df_Age[4,9]),
             as.numeric(df_Age[1,10]), as.numeric(df_Age[2,10]), as.numeric(df_Age[3,10]), as.numeric(df_Age[4,10]),
             as.numeric(df_AgeAtCancerDiagnosis[1,9]), as.numeric(df_AgeAtCancerDiagnosis[2,9]), as.numeric(df_AgeAtCancerDiagnosis[3,9]), "unmatched",
             as.numeric(df_AgeAtCancerDiagnosis[1,10]), as.numeric(df_AgeAtCancerDiagnosis[2,10]), as.numeric(df_AgeAtCancerDiagnosis[3,10]), "unmatched"),
  
  SiteC = c(as.numeric(df_Age[5,2]), as.numeric(df_Age[6,2]), as.numeric(df_Age[7,2]), as.numeric(df_Age[8,2]),
            df_female_site_percent[5,2], df_female_site_percent[6,2], df_female_site_percent[7,2], df_female_site_percent[8,2],
            as.numeric(df_Age[5,9]), as.numeric(df_Age[6,9]), as.numeric(df_Age[7,9]), as.numeric(df_Age[8,9]),
            as.numeric(df_Age[5,10]), as.numeric(df_Age[6,10]), as.numeric(df_Age[7,10]), as.numeric(df_Age[8,10]),
            as.numeric(df_AgeAtCancerDiagnosis[4,9]), as.numeric(df_AgeAtCancerDiagnosis[5,9]), as.numeric(df_AgeAtCancerDiagnosis[6,9]), "unmatched",
            as.numeric(df_AgeAtCancerDiagnosis[4,10]), as.numeric(df_AgeAtCancerDiagnosis[5,10]), as.numeric(df_AgeAtCancerDiagnosis[6,10]), "unmatched"),
  
  SiteD = c(as.numeric(df_Age[9,2]), as.numeric(df_Age[10,2]), as.numeric(df_Age[11,2]), as.numeric(df_Age[12,2]),
            df_female_site_percent[9,2], df_female_site_percent[10,2], df_female_site_percent[11,2], df_female_site_percent[12,2],
            as.numeric(df_Age[9,9]), as.numeric(df_Age[10,9]), as.numeric(df_Age[11,9]), as.numeric(df_Age[12,9]),
            as.numeric(df_Age[9,10]), as.numeric(df_Age[10,10]), as.numeric(df_Age[11,10]), as.numeric(df_Age[12,10]),
            as.numeric(df_AgeAtCancerDiagnosis[7,9]), as.numeric(df_AgeAtCancerDiagnosis[8,9]), as.numeric(df_AgeAtCancerDiagnosis[9,9]), "unmatched",
            as.numeric(df_AgeAtCancerDiagnosis[7,10]), as.numeric(df_AgeAtCancerDiagnosis[8,10]), as.numeric(df_AgeAtCancerDiagnosis[9,10]), "unmatched")
)

write.csv2(df_output_DemographocData_unmatched, "~/Projekte/HIVandCancer/Analyse/Demographics_unmatched.csv", row.names = FALSE)


df_output_DemographocData_matched <- data.frame(
  Demographic = c("Number of patients (N)", "Cancer+/HIV-", "Cancer+/HIV+", "Cancer-/HIV+",
                  "Female patients (%)", "Cancer+/HIV-", "Cancer+/HIV+", "Cancer-/HIV+",
                  "Age (mean)", "Cancer+/HIV-", "Cancer+/HIV+", "Cancer-/HIV+",
                  "Age (SD)", "Cancer+/HIV-", "Cancer+/HIV+", "Cancer-/HIV+",
                  "Age at cancer diagnosis (mean)", "Cancer+/HIV-", "Cancer+/HIV+", "Cancer-/HIV+",
                  "Age at cancer diagnosis(SD)", "Cancer+/HIV-", "Cancer+/HIV+", "Cancer-/HIV+"),
  All = c(sum(df_Age[13,2],df_Age[1,2], df_Age[5,2], df_Age[9,2]), 
          sum(df_Age[14,2], df_Age[2,2], df_Age[6,2], df_Age[10,2]),
          sum(df_Age[15,2], df_Age[3,2], df_Age[7,2],  df_Age[11,2]),
          sum(df_Age[16,2], df_Age[4,2], df_Age[8,2], df_Age[12,2]),
          df_female_all_percent[1,2], df_female_all_percent[2,2], df_female_all_percent[3,2], df_female_all_percent[4,2],
          df_Age_Subgruoup[1,2], df_Age_Subgruoup[2,2], df_Age_Subgruoup[3,2], df_Age_Subgruoup[4,2],
          df_Age_Subgruoup[1,3], df_Age_Subgruoup[2,3], df_Age_Subgruoup[3,3], df_Age_Subgruoup[4,3],
          df_AgeAtCancer_SubgruoupM[1,2], df_AgeAtCancer_SubgruoupM[2,2], df_AgeAtCancer_SubgruoupM[3,2], "matched",
          df_AgeAtCancer_SubgruoupM[1,3], df_AgeAtCancer_SubgruoupM[2,3], df_AgeAtCancer_SubgruoupM[3,3], "matched"),
  
  SiteA = c(as.numeric(df_Age[13,2]), as.numeric(df_Age[14,2]), as.numeric(df_Age[15,2]), as.numeric(df_Age[16,2]),
            df_female_site_percent[13,2], df_female_site_percent[14,2], df_female_site_percent[15,2], df_female_site_percent[16,2],
            as.numeric(df_Age[13,9]), as.numeric(df_Age[14,9]), as.numeric(df_Age[15,9]), as.numeric(df_Age[16,9]),
            as.numeric(df_Age[13,10]), as.numeric(df_Age[14,10]), as.numeric(df_Age[15,10]), as.numeric(df_Age[16,10]),
            as.numeric(df_AgeAtCancerDiagnosisM[10,9]), as.numeric(df_AgeAtCancerDiagnosisM[11,9]), as.numeric(df_AgeAtCancerDiagnosisM[12,9]), "matched",
            as.numeric(df_AgeAtCancerDiagnosisM[10,10]), as.numeric(df_AgeAtCancerDiagnosisM[11,10]), as.numeric(df_AgeAtCancerDiagnosisM[12,10]), "matched"),
  
  SiteB = c(as.numeric(df_Age[1,2]), as.numeric(df_Age[2,2]), as.numeric(df_Age[3,2]), as.numeric(df_Age[4,2]),
            df_female_site_percent[1,2], df_female_site_percent[2,2], df_female_site_percent[3,2], df_female_site_percent[4,2],
            as.numeric(df_Age[1,9]), as.numeric(df_Age[2,9]), as.numeric(df_Age[3,9]), as.numeric(df_Age[4,9]),
            as.numeric(df_Age[1,10]), as.numeric(df_Age[2,10]), as.numeric(df_Age[3,10]), as.numeric(df_Age[4,10]),
            as.numeric(df_AgeAtCancerDiagnosisM[1,9]), as.numeric(df_AgeAtCancerDiagnosisM[2,9]), as.numeric(df_AgeAtCancerDiagnosisM[3,9]), "matched",
            as.numeric(df_AgeAtCancerDiagnosisM[1,10]), as.numeric(df_AgeAtCancerDiagnosisM[2,10]), as.numeric(df_AgeAtCancerDiagnosisM[3,10]), "matched"),
  
  SiteC = c(as.numeric(df_Age[5,2]), as.numeric(df_Age[6,2]), as.numeric(df_Age[7,2]), as.numeric(df_Age[8,2]),
            df_female_site_percent[5,2], df_female_site_percent[6,2], df_female_site_percent[7,2], df_female_site_percent[8,2],
            as.numeric(df_Age[5,9]), as.numeric(df_Age[6,9]), as.numeric(df_Age[7,9]), as.numeric(df_Age[8,9]),
            as.numeric(df_Age[5,10]), as.numeric(df_Age[6,10]), as.numeric(df_Age[7,10]), as.numeric(df_Age[8,10]),
            as.numeric(df_AgeAtCancerDiagnosisM[4,9]), as.numeric(df_AgeAtCancerDiagnosisM[5,9]), as.numeric(df_AgeAtCancerDiagnosisM[6,9]), "matched",
            as.numeric(df_AgeAtCancerDiagnosisM[4,10]), as.numeric(df_AgeAtCancerDiagnosisM[5,10]), as.numeric(df_AgeAtCancerDiagnosisM[6,10]), "matched"),
  
  SiteD = c(as.numeric(df_Age[9,2]), as.numeric(df_Age[10,2]), as.numeric(df_Age[11,2]), as.numeric(df_Age[12,2]),
            df_female_site_percent[9,2], df_female_site_percent[10,2], df_female_site_percent[11,2], df_female_site_percent[12,2],
            as.numeric(df_Age[9,9]), as.numeric(df_Age[10,9]), as.numeric(df_Age[11,9]), as.numeric(df_Age[12,9]),
            as.numeric(df_Age[9,10]), as.numeric(df_Age[10,10]), as.numeric(df_Age[11,10]), as.numeric(df_Age[12,10]),
            as.numeric(df_AgeAtCancerDiagnosisM[7,9]), as.numeric(df_AgeAtCancerDiagnosisM[8,9]), as.numeric(df_AgeAtCancerDiagnosisM[9,9]), "matched",
            as.numeric(df_AgeAtCancerDiagnosisM[7,10]), as.numeric(df_AgeAtCancerDiagnosisM[8,10]), as.numeric(df_AgeAtCancerDiagnosisM[9,10]), "matched")
)


########## Across all primary subgroups ########################################

#--------- All subgroups: Sample size ------------------------------------------

df_Output_SampleSize_OverTime <- df_SamplSizeOverTime_plot %>%
  group_by(PatientSubgroup, FirstRelevantAdmissionYear) %>%
  rename(Year = FirstRelevantAdmissionYear)

plot_Output_SampleSize_OverTime <- df_Output_SampleSize_OverTime %>%
  f_MakeColumnPlot(inp_X = Year,
                   inp_Y = N,
                   inp_FacetFeature = PatientSubgroup,
                   inp_ls_FacetArguments = list(dir = "v",
                                                scales = "free_y"),
                   inp_FacetMapping = "fill",
                   inp_FillPalette = vc_FillPalette_Subgroup)      # Individual y-scales for different subgroups

plot_Output_SampleSize_OverTime_A <- df_Output_SampleSize_OverTime %>%
  filter(PatientSubgroup == "Cancer without HIV") %>%
  f_MakeColumnPlot(inp_X = Year,
                   inp_Y = N,
                   inp_FillPalette = color_CancerOnly,
                   inp_LegendPosition = "none")

plot_Output_SampleSize_OverTime_B <- df_Output_SampleSize_OverTime %>%
  filter(PatientSubgroup == "Cancer with HIV") %>%
  f_MakeColumnPlot(inp_X = Year,
                   inp_Y = N,
                   inp_FillPalette = color_HIVCancer,
                   inp_LegendPosition = "none")

plot_Output_SampleSize_OverTime_C <- df_Output_SampleSize_OverTime %>%
  filter(PatientSubgroup == "HIV without Cancer") %>%
  f_MakeColumnPlot(inp_X = Year,
                   inp_Y = N,
                   inp_FillPalette = color_HIVOnly,
                   inp_LegendPosition = "none")


#--------- All subgroups: Sex distribution -------------------------------------

# Sex distribution summary output
df_Output_SexDistribution <- df_Patients %>%
  group_by(PatientSubgroup) %>%
  summarize(N = n(),
            CountFemale = sum(Sex == "w", na.rm = TRUE),
            CountMale = sum(Sex == "m", na.rm = TRUE),
            CountOther = sum(Sex == "x", na.rm = TRUE),
            RateFemale = sum(Sex == "w", na.rm = TRUE) / N,
            RateMale = sum(Sex == "m", na.rm = TRUE) / N,
            RateOther = sum(Sex == "x", na.rm = TRUE) / N)

plot_Output_SexDistribution <- df_Patients %>%
  group_by(PatientSubgroup, Sex) %>%
  summarize(N = n()) %>%
  f_MakeColumnPlot(inp_X = PatientSubgroup,
                   inp_XSpecs = c("Cancer without HIV",
                                  "Cancer with HIV",
                                  "HIV without Cancer"),
                   inp_XAdditionalMapping = "fill",
                   inp_FillPalette = vc_FillPalette_Subgroup,
                   inp_Y = N,
                   inp_GroupingFeature = Sex,
                   inp_GroupingSpecs = c("Other" = "x", "Male" = "m", "Female" = "w"),
                   inp_GroupingPosition = position_fill(),
                   inp_GroupingMapping = "alpha",
                   inp_AlphaPalette = vc_AlphaPalette_3,
                   inp_AxisType_y = "proportional",
                   inp_ls_ThemeArguments = list(inp_Theme_SizeFactorTickLabels_x = 1.3,
                                                inp_Theme_SizeFactorLegendLabels = 1.3))


# Facet plot of subgroup-specific sex distribution over time
plot_Output_SexDistribution_OverTime <- df_Patients %>%
  group_by(PatientSubgroup, FirstMainAdmissionYear, Sex) %>%
  summarize(N = n()) %>%
  f_MakeColumnPlot(inp_X = FirstMainAdmissionYear,
                   inp_Y = N,
                   inp_GroupingFeature = Sex,
                   inp_GroupingSpecs = c("Other" = "x", "Male" = "m", "Female" = "w"),
                   inp_GroupingPosition = position_fill(),
                   inp_GroupingMapping = "alpha",
                   inp_AlphaPalette = vc_AlphaPalette_3,
                   inp_AxisType_y = "proportional",
                   inp_FacetFeature = PatientSubgroup,
                   inp_ls_FacetArguments = list(dir = "v"),
                   inp_FacetMapping = "fill",
                   inp_FillPalette = vc_FillPalette_Subgroup)

plot_Output_SexDistribution_OverTime_A <- df_Patients %>%
  filter(PatientSubgroup == "Cancer without HIV") %>%
  group_by(FirstMainAdmissionYear, Sex) %>%
  summarize(N = n()) %>%
  f_MakeColumnPlot(inp_X = FirstMainAdmissionYear,
                   inp_Y = N,
                   inp_GroupingFeature = Sex,
                   inp_GroupingSpecs = c("Other" = "x", "Male" = "m", "Female" = "w"),
                   inp_GroupingPosition = position_fill(),
                   inp_GroupingMapping = "alpha",
                   inp_AlphaPalette = vc_AlphaPalette_3,
                   inp_AxisType_y = "proportional",
                   inp_FillPalette = color_CancerOnly,
                   inp_LegendShowFillGuide = FALSE)

plot_Output_SexDistribution_OverTime_B <- df_Patients %>%
  filter(PatientSubgroup == "Cancer with HIV") %>%
  group_by(FirstMainAdmissionYear, Sex) %>%
  summarize(N = n()) %>%
  f_MakeColumnPlot(inp_X = FirstMainAdmissionYear,
                   inp_Y = N,
                   inp_GroupingFeature = Sex,
                   inp_GroupingSpecs = c("Male" = "m", "Female" = "w"),
                   inp_GroupingPosition = position_fill(),
                   inp_GroupingMapping = "alpha",
                   inp_AlphaPalette = vc_AlphaPalette_2,
                   inp_AxisType_y = "proportional",
                   inp_FillPalette = color_HIVCancer,
                   inp_LegendShowFillGuide = FALSE)

plot_Output_SexDistribution_OverTime_C <- df_Patients %>%
  filter(PatientSubgroup == "HIV without Cancer") %>%
  group_by(FirstMainAdmissionYear, Sex) %>%
  summarize(N = n()) %>%
  f_MakeColumnPlot(inp_X = FirstMainAdmissionYear,
                   inp_Y = N,
                   inp_GroupingFeature = Sex,
                   inp_GroupingSpecs = c("Male" = "m", "Female" = "w"),
                   inp_GroupingPosition = position_fill(),
                   inp_GroupingMapping = "alpha",
                   inp_AlphaPalette = vc_AlphaPalette_2,
                   inp_AxisType_y = "proportional",
                   inp_FillPalette = color_HIVOnly,
                   inp_LegendShowFillGuide = FALSE)



#--------- All subgroups: Age distribution -------------------------------------

# Age distribution summary output
df_Output_Age_Summary <- df_Patients %>%
  group_by(PatientSubgroup) %>%
  f_GetSampleStatistics(inp_MetricFeature = FirstMainAdmissionAge,
                        inp_na.rm = TRUE)

# Box and Violin plots of subgroups
plot_Output_AgeDistribution <- df_Patients %>%
  f_MakeBoxViolinPlot(inp_X = PatientSubgroup,
                      inp_Y = FirstMainAdmissionAge,
                      inp_AxisTitle_y = "Age at Diagnosis",
                      inp_AxisLimits_y = c(0, NA_integer_),
                      inp_FillPalette = vc_FillPalette_Subgroup)

# Age distribution over time
# Age is the age at first diagnosis of either HIV or cancer
df_Output_AgeDistribution_OverTime <- df_Patients %>%
  mutate(AgeGroup = case_when(between(FirstMainAdmissionAge, 18, 39) == TRUE ~ "18 - 39 years old",
                              between(FirstMainAdmissionAge, 40, 59) == TRUE ~ "40 - 59 years old",
                              between(FirstMainAdmissionAge, 60, 79) == TRUE ~ "60 - 79 years old",
                              FirstMainAdmissionAge >= 80 ~ "> 80 years old")) %>%
  group_by(PatientSubgroup, FirstMainAdmissionYear, AgeGroup) %>%
  summarize(N = n())

# Facet plot of subgroup-specific age distribution over time
plot_Output_AgeDistribution_OverTime <- df_Output_AgeDistribution_OverTime %>%
  f_MakeColumnPlot(inp_X = FirstMainAdmissionYear,
                   inp_Y = N,
                   inp_GroupingFeature = AgeGroup,
                   inp_GroupingPosition = position_fill(),
                   inp_AxisType_y = "proportional",
                   inp_GroupingSpecs = c("> 80 years old",
                                         "60 - 79 years old",
                                         "40 - 59 years old",
                                         "18 - 39 years old"),
                   inp_GroupingMapping = "alpha",
                   inp_AlphaPalette = vc_AlphaPalette_4,
                   inp_FacetFeature = PatientSubgroup,
                   inp_ls_FacetArguments = list(dir = "v",
                                                scales = "free_y"),
                   inp_FacetMapping = "fill",
                   inp_FillPalette = vc_FillPalette_Subgroup)


plot_Output_AgeDistribution_OverTime_A <- df_Output_AgeDistribution_OverTime %>%
  filter(PatientSubgroup == "Cancer without HIV") %>%
  f_MakeColumnPlot(inp_X = FirstMainAdmissionYear,
                   inp_Y = N,
                   inp_GroupingFeature = AgeGroup,
                   inp_GroupingPosition = position_fill(),
                   inp_AxisType_y = "proportional",
                   inp_GroupingSpecs = c("> 80 years old",
                                         "60 - 79 years old",
                                         "40 - 59 years old",
                                         "18 - 39 years old"),
                   inp_GroupingMapping = "alpha",
                   inp_AlphaPalette = vc_AlphaPalette_4,
                   inp_FillPalette = color_CancerOnly,
                   inp_LegendShowFillGuide = FALSE)

plot_Output_AgeDistribution_OverTime_B <- df_Output_AgeDistribution_OverTime %>%
  filter(PatientSubgroup == "Cancer with HIV") %>%
  f_MakeColumnPlot(inp_X = FirstMainAdmissionYear,
                   inp_Y = N,
                   inp_GroupingFeature = AgeGroup,
                   inp_GroupingPosition = position_fill(),
                   inp_AxisType_y = "proportional",
                   inp_GroupingSpecs = c("> 80 years old",
                                         "60 - 79 years old",
                                         "40 - 59 years old",
                                         "18 - 39 years old"),
                   inp_GroupingMapping = "alpha",
                   inp_AlphaPalette = vc_AlphaPalette_4,
                   inp_FillPalette = color_HIVCancer,
                   inp_LegendShowFillGuide = FALSE)

plot_Output_AgeDistribution_OverTime_C <- df_Output_AgeDistribution_OverTime %>%
  filter(PatientSubgroup == "HIV without Cancer") %>%
  f_MakeColumnPlot(inp_X = FirstMainAdmissionYear,
                   inp_Y = N,
                   inp_GroupingFeature = AgeGroup,
                   inp_GroupingPosition = position_fill(),
                   inp_AxisType_y = "proportional",
                   inp_GroupingSpecs = c("> 80 years old",
                                         "60 - 79 years old",
                                         "40 - 59 years old",
                                         "18 - 39 years old"),
                   inp_GroupingMapping = "alpha",
                   inp_AlphaPalette = vc_AlphaPalette_4,
                   inp_FillPalette = color_HIVOnly,
                   inp_LegendShowFillGuide = FALSE)


#--------- All subgroups: Case count per patient -------------------------------

# Case count summary output
df_Output_CaseCount_Summary <- df_Patients %>%
  group_by(PatientSubgroup) %>%
  f_GetSampleStatistics(inp_MetricFeature = CaseCount)

# Case count detailed output
df_Output_CaseCount <- df_Patients %>%
  group_by(PatientSubgroup, FirstMainAdmissionYear) %>%
  summarize(N = n())

# Box and Violin plots of subgroups
plot_Output_CaseCount <- df_Patients %>%
  f_MakeBoxViolinPlot(inp_X = PatientSubgroup,
                      inp_Y = CaseCount,
                      inp_OutlierAcrossAll = TRUE,
                      inp_OutlierQuantile = 0.95,
                      inp_LogTransform = FALSE,
                      inp_ShowViolinPlot = FALSE,
                      inp_AxisTitle_y = "Number of cases per patient",
                      inp_AxisLimits_y = c(0, NA_integer_),
                      inp_FillPalette = vc_FillPalette_Subgroup)      # Fixed lower and auto upper y-Axis limit


#--------- All subgroups: Mean length of stay ----------------------------------

# Mean length of stay summary output
df_Output_MeanLengthOfStay <- df_Patients %>%
  group_by(PatientSubgroup) %>%
  f_GetSampleStatistics(inp_MetricFeature = MeanLengthOfStay)

# Box and Violin plots of subgroups
plot_Output_MeanLengthOfStay <- df_Patients %>%
  f_MakeBoxViolinPlot(inp_X = PatientSubgroup,
                      inp_Y = MeanLengthOfStay,
                      inp_OutlierAcrossAll = TRUE,
                      inp_OutlierQuantile = 0.95,
                      inp_LogTransform = FALSE,
                      inp_AxisTitle_y = "Mean length of stay per patient (days)",
                      inp_AxisLimits_y = c(0, NA_integer_),
                      inp_FillPalette = vc_FillPalette_Subgroup)      # Fixed lower and auto upper y-Axis limit


#--------- All subgroups: Recorded time span of main diagnosis -----------------

df_Output_MainRecordedTimeSpan <- df_Patients %>%
  group_by(PatientSubgroup) %>%
  f_GetSampleStatistics(inp_MetricFeature = MainRecordedTimeSpan)

plot_Output_MainRecordedTimeSpan <- df_Patients %>%
  f_MakeBoxViolinPlot(inp_X = PatientSubgroup,
                      inp_Y = MainRecordedTimeSpan,
                      inp_OutlierAcrossAll = TRUE,
                      inp_OutlierQuantile = 0.95,
                      inp_LogTransform = FALSE,
                      inp_AxisTitle_y = "Recorded time span of main diagnosis (days)",
                      inp_AxisLimits_y = c(0, NA_integer_),
                      inp_FillPalette = vc_FillPalette_Subgroup)


########## HIVandCancer vs. CancerOnly #########################################


#!!!-!!!-!!! Plot describing completeness of data (Sankey diagram with nodes main diagnosis - treatment - follow up?)



#--------- Age at presumed cancer diagnosis ----------------------------------------

df_Output_AgeAtCancerDiagnosis <- df_PatientsCancer %>%
  group_by(PatientSubgroup) %>%
  f_GetSampleStatistics(inp_MetricFeature = PresumedMainCancerDiagnosisAge,
                        inp_na.rm = TRUE)

plot_Output_AgeAtCancerDiagnosis <- df_PatientsCancer %>%
  f_MakeBoxViolinPlot(inp_X = PatientSubgroup,
                      inp_Y = PresumedMainCancerDiagnosisAge,
                      inp_AxisTitle_y = "Age at cancer diagnosis",
                      inp_AxisLimits_y = c(0, NA_integer_),
                      inp_FillPalette = vc_FillPalette_Subgroup)      # Fixed lower and auto upper y-Axis limit


#---------- Cancer grouping: AD and NAD and Non-HIV-associated cancer ----------

df_Output_HIVCancerCategories <- df_PatientsCancer %>%
  group_by(PatientSubgroup, PatientSubgroupHIVCancerCategory) %>%
  summarize(N = n())

plot_Output_HIVCancerCategories <- df_Output_HIVCancerCategories %>%
  f_MakeColumnPlot(inp_X = PatientSubgroup,
                   inp_XAdditionalMapping = "fill",
                   inp_Y = N,
                   inp_GroupingFeature = PatientSubgroupHIVCancerCategory,
                   inp_GroupingSpecs = c("Non-HIV-associated cancer",
                                         "HIV-associated non-AD cancer",
                                         "HIV-associated AD cancer"),
                   inp_GroupingPosition = position_fill(),
                   inp_GroupingMapping = "alpha",
                   inp_AxisType_y = "proportional",
                   inp_ls_ThemeArguments = list(inp_Theme_SizeFactorTickLabels_x = 1.3,
                                                inp_Theme_SizeFactorLegendLabels = 1.3),
                   inp_FillPalette = vc_FillPalette_Subgroup)

df_Output_HIVCancerCategories_OverTime <- df_PatientsCancer %>%
  group_by(PatientSubgroup, PatientSubgroupHIVCancerCategory, PresumedMainCancerDiagnosisYear) %>%
  summarize(N = n())

plot_Output_HIVCancerCategories_OverTime <- df_Output_HIVCancerCategories_OverTime %>%
  f_MakeColumnPlot(inp_X = PresumedMainCancerDiagnosisYear,
                   inp_Y = N,
                   inp_GroupingFeature = PatientSubgroupHIVCancerCategory,
                   inp_GroupingSpecs = c("Non-HIV-associated cancer",
                                         "HIV-associated non-AD cancer",
                                         "HIV-associated AD cancer"),
                   inp_GroupingPosition = position_fill(),
                   inp_GroupingMapping = "alpha",
                   inp_AxisType_y = "proportional",
                   inp_FacetFeature = PatientSubgroup,
                   inp_ls_FacetArguments = list(dir = "v"),
                   inp_FacetMapping = "fill",
                   inp_FillPalette = vc_FillPalette_Subgroup)


#---------- Cancer grouping: CIS -----------------------------------------------


df_Output_CIS <- df_PatientsCancer %>%
  group_by(PatientSubgroup, CancerIsCarcinomaInSitu) %>%
  summarize(N = n())

plot_Output_CIS <- df_PatientsCancer %>%
  group_by(PatientSubgroup, CancerIsCarcinomaInSitu) %>%
  summarize(N = n()) %>%
  f_MakeColumnPlot(inp_X = PatientSubgroup,
                   inp_XAdditionalMapping = "fill",
                   inp_FillPalette = vc_FillPalette_Subgroup,
                   inp_Y = N,
                   inp_GroupingFeature = CancerIsCarcinomaInSitu,
                   inp_GroupingSpecs = c("Other" = FALSE, "Carcinoma in situ" = TRUE),
                   inp_GroupingPosition = position_fill(),
                   inp_AxisType_y = "proportional",
                   inp_GroupingMapping = "alpha",
                   inp_ls_ThemeArguments = list(inp_Theme_SizeFactorTickLabels_x = 1.3,
                                                inp_Theme_SizeFactorLegendLabels = 1.3))



#---------- Cancer Grouping: Topography detail ---------------------------------

df_Output_CancerTopographyDetail <- df_PatientsCancer %>%
  group_by(PatientSubgroup, CancerTopographyDetail) %>%
  summarize(N = n()) %>%
  group_by(PatientSubgroup) %>%
  mutate(Proportion = N / sum(N))

vc_TopographyDetailSorted <- df_Output_CancerTopographyDetail %>%
  filter(PatientSubgroup == "Cancer with HIV") %>%
  slice_max(n = 15,
            order_by = Proportion) %>%
  pull(CancerTopographyDetail)

# Make "pyramid plot"
plot_Output_CancerTopographyDetail <- df_Output_CancerTopographyDetail %>%
  mutate(Proportion = case_when(PatientSubgroup == "Cancer without HIV" ~ -Proportion,
                                TRUE ~ Proportion)) %>%
  f_MakeColumnPlot(inp_X = CancerTopographyDetail,
                   inp_XSpecs = rev(vc_TopographyDetailSorted),
                   inp_Y = Proportion,
                   inp_GroupingFeature = PatientSubgroup,
                   inp_GroupingSpecs = c("Cancer with HIV", "Cancer without HIV"),
                   inp_ColumnWidth = 0.8,
                   inp_FillPalette = vc_FillPalette_Subgroup,
                   inp_AxisType_y = "proportional",
                   inp_CoordFlip = TRUE,
                   inp_TickLabelWidth_x = 40,
                   inp_LegendPosition = "top")

# Manual modifications to comply pyramid plot needs
plot_Output_CancerTopographyDetail <- plot_Output_CancerTopographyDetail +
  theme(axis.text.y = element_text(margin = margin(r = 10, unit = "pt"))) +
  scale_y_continuous(limits = c(-0.3, 0.3),
                     breaks = seq(from = -0.3, to = 0.3, by = 0.1),
                     labels = function(x) paste(abs(round(x * 100, 0)), "%")) +
  scale_fill_manual(values = vc_FillPalette_Subgroup,
                    breaks = c("Cancer without HIV", "Cancer with HIV"),
                    name = NULL) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.8)))



#---------- Cancer Grouping: Topography groups ---------------------------------

df_Output_CancerTopographyGroup <- df_PatientsCancer %>%
  group_by(PatientSubgroup, CancerTopographyGroup) %>%
  summarize(N = n()) %>%
  group_by(PatientSubgroup) %>%
  mutate(Proportion = N / sum(N))

vc_TopographyGroupsSorted <- df_Output_CancerTopographyGroup %>%
  filter(PatientSubgroup == "Cancer without HIV") %>%
  arrange(desc(Proportion)) %>%
  pull(CancerTopographyGroup)

# Make "pyramid plot"
plot_Output_CancerTopographyGroup <- df_Output_CancerTopographyGroup %>%
  mutate(Proportion = case_when(PatientSubgroup == "Cancer without HIV" ~ -Proportion,
                                TRUE ~ Proportion)) %>%
  f_MakeColumnPlot(inp_X = CancerTopographyGroup,
                   inp_XSpecs = rev(vc_TopographyGroupsSorted),
                   inp_Y = Proportion,
                   inp_GroupingFeature = PatientSubgroup,
                   inp_GroupingSpecs = c("Cancer with HIV", "Cancer without HIV"),
                   inp_ColumnWidth = 0.8,
                   inp_FillPalette = vc_FillPalette_Subgroup,
                   inp_AxisType_y = "proportional",
                   inp_CoordFlip = TRUE,
                   inp_TickLabelWidth_x = 40,
                   inp_LegendPosition = "top")

# Manual modifications to comply pyramid plot needs
plot_Output_CancerTopographyGroup <- plot_Output_CancerTopographyGroup +
  theme(axis.text.y = element_text(margin = margin(r = 10, unit = "pt"))) +
  scale_y_continuous(limits = c(-0.3, 0.3),
                     breaks = seq(from = -0.3, to = 0.3, by = 0.1),
                     labels = function(x) paste(abs(round(x * 100, 0)), "%")) +
  scale_fill_manual(values = vc_FillPalette_Subgroup,
                    breaks = c("Cancer without HIV", "Cancer with HIV"),
                    name = NULL) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.8)))



#--------- Metastasis occurrence -----------------------------------------------

df_Output_MetastasisOccurrence <- df_PatientsCancer %>%
  group_by(PatientSubgroup) %>%
  summarize(N = n(),
            CountMetastasis = sum(PatIsMetastasisCoded == TRUE, na.rm = TRUE),
            ProportionMetastasis = CountMetastasis / N,
            CountNoMetastasis = N - CountMetastasis,
            ProportionNoMetastasis = CountNoMetastasis / N,
            CountMetastasisWithCancerDiagnosis = sum(TimeCancerToMetastasis == 0, na.rm = TRUE),
            ProportionMetastasisWithCancerDiagnosis = CountMetastasisWithCancerDiagnosis / CountMetastasis,
            CountMetastasisAfterCancerDiagnosis = CountMetastasis - CountMetastasisWithCancerDiagnosis,
            ProportionMetastasisAfterCancerDiagnosis = CountMetastasisAfterCancerDiagnosis / CountMetastasis)


plot_Output_MetastasisOccurrence <- df_Output_MetastasisOccurrence %>%
  select(PatientSubgroup,
         CountNoMetastasis,
         CountMetastasisWithCancerDiagnosis,
         CountMetastasisAfterCancerDiagnosis) %>%
  pivot_longer(cols = starts_with("Count"),
               names_to = "Group",
               values_to = "Count") %>% 
  f_MakeColumnPlot(inp_X = PatientSubgroup,
                   inp_XAdditionalMapping = "fill",
                   inp_FillPalette = vc_FillPalette_Subgroup,
                   inp_Y = Count,
                   inp_GroupingFeature = Group,
                   inp_GroupingSpecs = c("No Metastasis" = "CountNoMetastasis",
                                         "Metastasis after cancer diagnosis" = "CountMetastasisAfterCancerDiagnosis",
                                         "Metastasis with cancer diagnosis" = "CountMetastasisWithCancerDiagnosis"),
                   inp_GroupingPosition = position_fill(),
                   inp_GroupingMapping = "alpha",
                   inp_AxisType_y = "proportional",
                   inp_ls_ThemeArguments = list(inp_Theme_SizeFactorTickLabels_x = 1.3,
                                                inp_Theme_SizeFactorLegendLabels = 1.3))



#--------- HIVandCancer vs. CancerOnly: Time to Metastasis ---------------------

# Include only patients that presumably had no metastasis at time of cancer diagnosis
df_TimeCancerToMetastasis <- df_PatientsCancer %>%
  filter(!is.na(TimeCancerToMetastasis) & TimeCancerToMetastasis > 0)

df_Output_TimeCancerToMetastasis <- df_TimeCancerToMetastasis %>%
  group_by(PatientSubgroup) %>%
  f_GetSampleStatistics(inp_MetricFeature = TimeCancerToMetastasis,
                        inp_na.rm = FALSE)


model_TimeCancerToMetastasis <- survfit(Surv(TimeCancerToMetastasis, PatIsMetastasisCoded) ~ PatientSubgroup,
                                        data = df_PatientsCancer)


plot_Output_TimeCancerToMetastasis <- ggsurvplot(fit = model_TimeCancerToMetastasis,
                                                 data = df_PatientsCancer,
                                                 fun = "cumhaz",
                                                 conf.int = TRUE,
                                                 palette = c(color_CancerOnly, color_HIVCancer),
                                                 legend.title = "",
                                                 legend.labs = c("Cancer without HIV", "Cancer with HIV"))$plot +
  theme_CCP(inp_Theme_LegendPosition = "top") +
  scale_x_continuous(labels = function(value) round(value / 365, 0)) +
  coord_cartesian(xlim = c(0, 2000),
                  ylim = c(0, 5)) +
  labs(x = "Years after cancer diagnosis",
       y = "Cum. hazard of metastasis")



#--------- HIVandCancer vs. CancerOnly: Count of main cancer entities ----------

df_Output_HIVCancerEntityCount <- df_PatientsCancer %>%
  group_by(PatientSubgroup) %>%
  f_GetSampleStatistics(inp_MetricFeature = DistinctCodeCountMainCancer)

plot_Output_HIVCancerEntityCount <- df_PatientsCancer %>%
  f_MakeBoxViolinPlot(inp_X = PatientSubgroup,
                      inp_Y = DistinctCodeCountMainCancer,
                      inp_AxisTitle_y = "Number of different cancer entities",
                      inp_AxisLimits_y = c(0, NA_integer_),
                      inp_FillPalette = vc_FillPalette_Subgroup)



#--------- HIVandCancer vs. CancerOnly: Therapy modalities ---------------------

df_Output_TherapyModalities <- df_PatientsCancer %>%
  group_by(PatientSubgroup) %>%
  summarize(N = n(),
            CountNoMajorTherapyCoded = sum(PatHadAnyPresumedCancerTherapy == FALSE, na.rm = TRUE),
            CountAnyMajorTherapy = sum(PatHadAnyPresumedCancerTherapy == TRUE, na.rm = TRUE),
            CountSurgery = sum(PatHadSurgery, na.rm = TRUE),
            ProportionSurgery = CountSurgery / CountAnyMajorTherapy,
            CountChemotherapy = sum(PatHadChemotherapy == TRUE, na.rm = TRUE),
            ProportionChemotherapy = CountChemotherapy / CountAnyMajorTherapy,
            CountOtherImmunotherapy = sum(PatHadImmunotherapy == TRUE, na.rm = TRUE),
            ProportionOtherImmunotherapy = CountOtherImmunotherapy / CountAnyMajorTherapy,
            CountRadiotherapy = sum(PatHadRadiotherapy == TRUE, na.rm = TRUE),
            ProportionRadiotherapy = CountRadiotherapy / CountAnyMajorTherapy,
            CountNuclearMedicineTherapy = sum(PatHadNuclearmedTherapy == TRUE, na.rm = TRUE),
            ProportionNuclearMedicineTherapy = CountNuclearMedicineTherapy / CountAnyMajorTherapy)

plot_Output_AnyMajorTherapy <- df_Output_TherapyModalities %>%
  select(PatientSubgroup,
         CountNoMajorTherapyCoded,
         CountAnyMajorTherapy) %>%
  pivot_longer(cols = starts_with("Count"),
               names_to = "Group",
               values_to = "Count") %>%
  f_MakeColumnPlot(inp_X = PatientSubgroup,
                   inp_XAdditionalMapping = "fill",
                   inp_FillPalette = vc_FillPalette_Subgroup,
                   inp_Y = Count,
                   inp_GroupingFeature = Group,
                   inp_GroupingPosition = position_fill(),
                   inp_GroupingSpecs = c("No Major Therapy coded" = "CountNoMajorTherapyCoded",
                                         "Any Major Therapy" = "CountAnyMajorTherapy"),
                   inp_GroupingMapping = "alpha",
                   inp_AxisType_y = "proportional",
                   inp_ls_ThemeArguments = list(inp_Theme_SizeFactorTickLabels_x = 1.3,
                                                inp_Theme_SizeFactorLegendLabels = 1.3))


plot_Output_TherapyModalities <- df_Output_TherapyModalities %>%
  pivot_longer(cols = starts_with("Proportion"),
               names_to = "Group",
               values_to = "Proportion") %>%
  f_MakeColumnPlot(inp_X = Group,
                   inp_XSpecs = c("Surgery" = "ProportionSurgery",
                                  "Chemo- therapy" = "ProportionChemotherapy",
                                  "Radio- therapy" = "ProportionRadiotherapy",
                                  "Immuno- therapy" = "ProportionOtherImmunotherapy",
                                  "Nuclear Medicine Therapy" = "ProportionNuclearMedicineTherapy"),
                   inp_Y = Proportion,
                   inp_GroupingFeature = PatientSubgroup,
                   inp_GroupingPosition = position_dodge(),
                   inp_FillPalette = vc_FillPalette_Subgroup,
                   inp_AxisType_y = "proportional",
                   inp_AxisTitle_y = "Prop. of pat. who received major therapy",
                   inp_ls_ThemeArguments = list(inp_Theme_SizeFactorTickLabels_x = 1.3,
                                                inp_Theme_SizeFactorLegendLabels = 1.3))



#--------- HIVandCancer vs. CancerOnly: Therapy complications ------------------

df_Output_TherapyComplications <- df_PatientsCancer %>%
  group_by(PatientSubgroup) %>%
  summarize(N = n(),
            AnyMajorTherapy = sum(PatHadAnyPresumedCancerTherapy == TRUE, na.rm = TRUE),
            Chemotherapy = sum(PatHadChemotherapy == TRUE, na.rm = TRUE),
            ComplicationAfterChemotherapy = sum(PatHadComplicationAfterChemo == TRUE, na.rm = TRUE))


model_TimeChemoToComplication <- survfit(Surv(TimeChemoToFirstComplication, PatHadComplicationAfterChemo) ~ PatientSubgroup,
                                         data = df_PatientsCancer)


plot_Output_TimeChemoToComplication <- ggsurvplot(fit = model_TimeChemoToComplication,
                                                  data = df_PatientsCancer,
                                                  fun = "cumhaz",
                                                  conf.int = TRUE,
                                                  palette = c(color_CancerOnly, color_HIVCancer),
                                                  legend.title = "",
                                                  legend.labs = c("Cancer without HIV", "Cancer with HIV"))$plot +
  theme_CCP(inp_Theme_LegendPosition = "top") +
  scale_x_continuous(labels = function(x) x) +
  coord_cartesian(xlim = c(0, 100),
                  ylim = c(0, 1)) +
  labs(x = "Days after first chemotherapy administration",
       y = "Cum. hazard of complication")



#--------- Last recorded discharge reason --------------------------------------

df_Output_LastRecordedDischargeReason <- df_PatientsCancer %>%
  group_by(PatientSubgroup, LastRecordedDischargeReason) %>%
  summarize(N = n()) %>%
  group_by(PatientSubgroup) %>%
  mutate(Proportion = N / sum(N))

plot_Output_LastRecordedDischargeReason <- df_Output_LastRecordedDischargeReason %>%
  f_MakeColumnPlot(inp_X = PatientSubgroup,
                   inp_XAdditionalMapping = "fill",
                   inp_FillPalette = vc_FillPalette_Subgroup,
                   inp_Y = N,
                   inp_GroupingFeature = LastRecordedDischargeReason,
                   inp_GroupingSpecs = c("Other",
                                         "Rehabilitation or Residential Care",
                                         "Home",
                                         "Other Hospital",
                                         "Hospice Care",
                                         "Deceased"),
                   inp_GroupingPosition = "fill",
                   inp_GroupingMapping = "alpha",
                   inp_AxisType_y = "proportional")




########## HIVandCancer ########################################################

#--------- HIV and cancer diagnosis order ------------------------------------------
df_Output_HIVCancerDiagnosisOrder <- df_PatientsHIVCancer %>%
  group_by(HIVCancerDiagnosisOrder, PresumedMainCancerDiagnosisYear) %>%
  summarize(N = n()) %>% 
  ungroup() %>%
  complete(HIVCancerDiagnosisOrder, PresumedMainCancerDiagnosisYear, fill = list(N = 0))

plot_Output_HIVCancerDiagnosisOrder <- df_Output_HIVCancerDiagnosisOrder %>%
  f_MakeColumnPlot(inp_X = PresumedMainCancerDiagnosisYear,
                   inp_Y = N,
                   inp_GroupingFeature = HIVCancerDiagnosisOrder,
                   inp_GroupingMapping = "alpha",
                   inp_AlphaPalette = vc_AlphaPalette_3,
                   inp_FillPalette = color_HIVCancer,
                   inp_GroupingPosition = position_fill(),
                   inp_AxisType_y = "proportional")


#--------- Primary HIV cancer category -----------------------------------------
df_Output_HIVCancerCategory <- df_PatientsHIVCancer %>%
  group_by(HIVCancerCategory, PresumedMainCancerDiagnosisYear) %>%
  summarize(N = n())
# pivot_wider(names_from = HIVCancerCategory,
#             values_from = N)

# Plot with absolute counts
plot_Output_HIVCancerCategory_A <- df_Output_HIVCancerCategory %>%
  f_MakeColumnPlot(inp_X = PresumedMainCancerDiagnosisYear,
                   inp_Y = N,
                   inp_GroupingFeature = HIVCancerCategory,
                   inp_GroupingSpecs = c("Non-HIV-associated cancer",
                                         "HIV-associated non-AD cancer",
                                         "HIV-associated AD cancer"),
                   inp_GroupingMapping = "alpha",
                   inp_AlphaPalette = vc_AlphaPalette_3,
                   inp_FillPalette = color_HIVCancer)

# Plot with proportions
plot_Output_HIVCancerCategory_B <- df_Output_HIVCancerCategory %>%
  f_MakeColumnPlot(inp_X = PresumedMainCancerDiagnosisYear,
                   inp_Y = N,
                   inp_GroupingFeature = HIVCancerCategory,
                   inp_GroupingSpecs = c("Non-HIV-associated cancer",
                                         "HIV-associated non-AD cancer",
                                         "HIV-associated AD cancer"),
                   inp_GroupingMapping = "alpha",
                   inp_AlphaPalette = vc_AlphaPalette_3,
                   inp_FillPalette = color_HIVCancer,
                   inp_GroupingPosition = position_fill(),
                   inp_AxisType_y = "proportional")


#--------- Age at presumed cancer diagnosis ------------------------------------

plot_Output_HIVCancerAgeAtDiagnosis <- df_PatientsHIVCancer %>%
  filter(HIVCancerDiagnosisOrder != "Cancer before HIV") %>%
  f_MakeBoxViolinPlot(inp_X = HIVCancerCategory,
                      inp_Y = PresumedMainCancerDiagnosisAge,
                      inp_AxisTitle_y = "",
                      inp_AxisLimits_y = c(0, NA_integer_),
                      inp_FillPalette = vc_FillPalette_Subgroup)      # Fixed lower and auto upper y-Axis limit


#--------- HIVandCancer: AIDS occurrence ---------------------------------------
df_Output_HIVCancerAIDS <- df_PatientsHIVCancer %>%
  group_by(AIDSOccurrence, PresumedMainCancerDiagnosisYear) %>%
  summarize(N = n()) %>% 
  ungroup() %>%
  complete(AIDSOccurrence, PresumedMainCancerDiagnosisYear, fill = list(N = 0))

plot_Output_HIVCancerAIDS <- df_Output_HIVCancerAIDS %>%
  f_MakeColumnPlot(inp_X = PresumedMainCancerDiagnosisYear,
                   inp_Y = N,
                   inp_GroupingFeature = AIDSOccurrence,
                   inp_GroupingSpecs = c("Cancer and HIV without AIDS",
                                         "AIDS after cancer diagnosis",
                                         "AIDS at or before cancer diagnosis"),
                   inp_GroupingMapping = "alpha",
                   inp_AlphaPalette = vc_AlphaPalette_3,
                   inp_FillPalette = color_HIVCancer,
                   inp_GroupingPosition = position_fill(),
                   inp_AxisType_y = "proportional")


#--------- Overview plot of HIV cancer  ----------------------------------------
plot_Output_HIVCancerOverview <- ggarrange(plot_Output_HIVCancerDiagnosisOrder,
                                           plot_Output_HIVCancerCategory_B,
                                           plot_Output_HIVCancerAIDS,
                                           nrow = 3,
                                           align = "hv",
                                           labels = LETTERS[1:4])



########## HIVandCancer vs. HIVOnly ############################################


#---------- HIV Status ---------------------------------------------------------

#!!! Add variable in df_Patients HIV Status...



#--------- HIVandCancer vs. HIVOnly: AIDS occurrence ---------------------------

# df_Output_AIDSOccurrence <- df_Patients %>%
#                                 filter(PatIsHIVCoded == TRUE) %>%
#                                 distinct(PatientPseudonym, .keep_all = TRUE) %>%
#                                 group_by(PatientSubgroup) %>%
#                                 summarize(N = n(),
#                                           CountAIDS = sum(PatIsAIDSCoded == TRUE, na.rm = TRUE),
#                                           ProportionAIDS = CountAIDS / N,
#                                           CountNoAIDS = N - CountAIDS,
#                                           ProportionNoAIDS = CountNoAIDS / N,
#                                           CountAIDSWithHIVDiagnosis = sum(TimeHIVToAIDS == 0, na.rm = TRUE),
#                                           ProportionAIDSWithHIVDiagnosis = CountAIDSWithHIVDiagnosis / CountAIDS,
#                                           CountAIDSAfterHIVDiagnosis = CountAIDS - CountAIDSWithHIVDiagnosis,
#                                           ProportionAIDSAfterHIVDiagnosis = CountAIDSAfterHIVDiagnosis / CountAIDS)
# 
# 
# plot_Output_AIDSOccurrence <- df_Output_AIDSOccurrence %>%
#                                   f_MakeColumnPlot(inp_Features = c("No AIDS" = "CountNoAIDS",
#                                                                     "AIDS after HIV diagnosis" = "CountAIDSAfterHIVDiagnosis",
#                                                                     "AIDS with HIV diagnosis" = "CountAIDSWithHIVDiagnosis"),
#                                                    inp_GroupingFeature = PatientSubgroup,
#                                                    inp_ColumnPlotType = "proportional",
#                                                    inp_ls_ThemeArguments = list(inp_Theme_SizeFactorTickLabels_x = 1.3,
#                                                                                 inp_Theme_SizeFactorLegendLabels = 1.3),
#                                                    inp_Palette = c(color_LightGrey, color_Secondary, color_Primary))
# 
# 
# 


#--------- HIVandCancer vs. HIVOnly: Time to AIDS ------------------------------

# Include only patients that presumably had no AIDS at time of HIV diagnosis
# df_TimeHIVToAIDS <- df_Patients %>%
#                         filter(!is.na(TimeHIVToAIDS) & TimeHIVToAIDS > 0) %>%
#                         distinct(PatientPseudonym, .keep_all = TRUE)
# 
# df_Output_TimeHIVToAIDS <- df_TimeHIVToAIDS %>%
#                                 group_by(PatientSubgroup) %>%
#                                 f_GetSampleStatistics(inp_Feature = TimeHIVToAIDS,
#                                                       inp_na.rm = FALSE)
# 
# plot_Output_TimeHIVToAIDS <- df_TimeHIVToAIDS %>%
#                                   f_MakeComparisonPlot(inp_Feature = TimeHIVToAIDS,
#                                                        inp_GroupingFeature = PatientSubgroup,
#                                                        inp_OutlierQuantile = 0.98,
#                                                        inp_OutlierAcrossAll = TRUE,
#                                                        inp_LogTransform = FALSE)

#!!!---!!!---!!! Add Time-to-event plot (Kaplan-Meier?)

#!!!---!!!---!!! Stratify HIVandCancer into "AIDS after HIV only" and "AIDS after cancer diagnosis"


#!!!---!!!---!!! Age related subgroups in HIVCancer? (Children, Adults, Elderly)



########## Sankey diagram: Presumed disease diagnosis and progress #################

# Make use of make_long() to get compatible data frame for Sankey diagram
# x: Stage
# node: Node
df_Plotdata_Sankey <- df_HIVCancerSequence %>%
  ggsankey::make_long(Diagnosis_1, Diagnosis_2, Diagnosis_3, Diagnosis_4) %>%
  filter(!is.na(node))

# df_Nodeweights <- df_Plotdata_Sankey %>%
#                       count(x, node) %>%
#                       arrange(x, desc(n)) %>%
#                       nest(Nodes = c(node, n))

vc_NodeOrder <- c("Cancer & HIV",
                  "Cancer & HIV & AIDS",
                  "Cancer & Metastasis & HIV",
                  "Cancer & Metastasis & HIV & AIDS",
                  "HIV",
                  "HIV & AIDS",
                  "AIDS",
                  "Cancer",
                  "Cancer & AIDS",
                  "Cancer & Metastasis",
                  "Metastasis",
                  "Metastasis & HIV",
                  "Metastasis & AIDS",
                  "Metastasis & HIV & AIDS")

df_Plotdata_Sankey$node <- factor(df_Plotdata_Sankey$node, levels = vc_NodeOrder)
df_Plotdata_Sankey$next_node <- factor(df_Plotdata_Sankey$next_node, levels = vc_NodeOrder)


plot_Output_SankeyDiagnosisProgress <- ggplot(df_Plotdata_Sankey,
                                              aes(x = x,
                                                  next_x = next_x,
                                                  node = node,
                                                  next_node = next_node,
                                                  fill = factor(node),
                                                  label = node)) +
  geom_sankey(type = "sankey",
              flow.alpha = 0.5,
              width = 0.3,
              node.color = "white",
              show.legend = FALSE) +
  geom_sankey_label(size = 2.5,
                    color = "black",
                    fill= "white",
                    hjust = 0.5) +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid = element_blank()) +
  scale_fill_viridis_d(option = "inferno", alpha = 0.95) +
  scale_fill_manual(values = c("HIV" = color_Primary,
                               "Cancer & HIV" = color_Primary,
                               "Cancer & Metastasis & HIV" = color_Primary,
                               "Metastasis & HIV" = color_Primary,
                               "HIV & AIDS" = color_Secondary,
                               "AIDS" = color_Secondary,
                               "Cancer & HIV & AIDS" = color_Secondary,
                               "Cancer & Metastasis & HIV & AIDS" = color_Secondary,
                               "Metastasis & HIV & AIDS" = color_Secondary,
                               "Metastasis & AIDS" = color_Secondary))

#plot_Output_Sankey

# Output Plot as pdf
# pdf(file = "./output/HIVCancerSequences_Detail.pdf",
#     width = 10,
#     height = 7)
# plot_Output_Sankey
# dev.off()

#-------------------------------------------------------------------------------



########## Sankey diagram: Cancer therapy sequence #############################

# Make use of make_long() to get compatible data frame for Sankey diagram
# x: Stage
# node: Node
df_Plotdata_Sankey <- df_CancerTherapySequence %>%
  ggsankey::make_long(TherapyDiagnosis_1,
                      TherapyDiagnosis_2,
                      TherapyDiagnosis_3,
                      TherapyDiagnosis_4,
                      TherapyDiagnosis_5,
                      TherapyDiagnosis_6) %>%
  filter(!is.na(node))

plot_Output_SankeyTherapySequence <- ggplot(df_Plotdata_Sankey,
                                            aes(x = x,
                                                next_x = next_x,
                                                node = node,
                                                next_node = next_node,
                                                fill = factor(node),
                                                label = node)) +
  geom_sankey(type = "sankey",
              flow.alpha = 0.5,
              width = 0.3,
              node.color = "white",
              show.legend = FALSE) +
  geom_sankey_label(size = 2.5,
                    color = "black",
                    fill= "white",
                    hjust = 0.5) +
  theme_sankey(base_size = 16) +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), 
        panel.grid = element_blank()) +
  scale_fill_viridis_d(option = "inferno", alpha = 0.95)

#plot_Output_Sankey

# Output Plot as pdf
# pdf(file = "./output/CancerTherapySequence.pdf",
#     width = 15,
#     height = 20)
# plot_Output_Sankey
# dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Attrition Diagram
# -----------------
# Using Graphviz Syntax based on DOT language
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_Output_Attrition <- DiagrammeR::grViz("
                                           digraph attrition {
                                           
                                           graph [overlap = true,
                                                  fontsize = 10,
                                                  fontname = Helvetica]
                                           
                                           rankdir = TD                         # Flow from top down
                                                 
                                           node [shape = box,
                                                 style = 'rounded, filled',
                                                 fillcolor = AliceBlue,
                                                 fontname = Helvetica]
                                                 
                                              N_1   [label = 'Primary SQL selection \n (N = 104.477)']
                                              Ex_1  [label = 'Patients with no documented cancer or HIV ICD codes \n (N = 49.601)']
                                              N_2   [label = 'Patients with ICD codes for cancer or HIV \n (N = 54.876)']
                                              Ex_2  [label = 'Patients with uncertain primary cancer diagnosis \n (N = 8.676)']
                                              N_3   [label = 'Patients with plausibly documented primary cancer diagnosis or HIV \n (N = 46.190)']
                                           
                                           # Invisible Joint nodes
                                           node [shape = point,
                                                 height = 0]
                                           
                                              Joint_1
                                              Joint_2
                                          
                                           { rank = same Joint_1 Ex_1 }
                                           { rank = same Joint_2 Ex_2 }
                                              
                                           N_1 -> Joint_1   [arrowhead = none]
                                           Joint_1 -> Ex_1  [minlen = 3]
                                           Joint_1 -> N_2
                                           N_2 -> Joint_2   [arrowhead = none]
                                           Joint_2 -> Ex_2  [minlen = 3]
                                           Joint_2 -> N_3
                                           
                                           }
                                           ")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Stratification Plot
# -------------------
# Using Graphviz Syntax based on DOT language
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
plot_Output_Stratification <- DiagrammeR::grViz("
                                                graph stratification {
                                                
                                                graph [overlap = true,
                                                       fontsize = 10,
                                                       fontname = Helvetica,
                                                       splines = line]
                                                       
                                                rankdir = LR
                                                compound = true
                                                      
                                                node [shape = box,
                                                      style = rounded,
                                                      fontname = Helvetica]
                                                  
                                                    Included   [label = 'Included\npatients']
                                                    Cancer     [label = 'Cancer only']
                                                    HIVCancer  [label = 'HIV and Cancer']
                                                    HIV        [label = 'HIV only']
                                                  
                                                node [shape = record
                                                      fontsize = 8
                                                      style = filled]
                                                  
                                                    CancerCategory   [color = Lavender
                                                                      label = 'AIDS-defining cancer |
                                                                               HIV-associated cancer |
                                                                               Non-HIV-associated cancer']
                                                
                                                # Invisible point node
                                                node [shape = point, height = 0] CenterSubgraph
                                                
                                                node [shape = record]
                                                
                                                    AIDS        [color = Azure
                                                                 label = 'AIDS before or at cancer diagnosis |
                                                                          AIDS after cancer diagnosis |
                                                                          No AIDS']

                                                subgraph cluster_HIVCancer {
                                                    rank = same
                                                    ordering = 'out'
                                                    CancerCategory; CenterSubgraph; AIDS
                                                    bgcolor = 'WhiteSmoke'
                                                }                  
                                                
                                                Included -- {Cancer HIVCancer HIV}
                                                HIVCancer -- CenterSubgraph [lhead = cluster_HIVCancer]
                                                }
                                                ")
#plot_Output_Stratification




########## Postal code map #####################################################


# Single Feature object of (aggregated) postal code areas in Hessen 
sf_PostalCodeAreas_Hessen <- st_read(dsn = "./data/PostalCodeAreas_Germany.geojson") %>%
  mutate(plz_short = factor(str_trunc(plz_code, 3, ellipsis = ""))) %>%      # Add attribute / variable of shortened postal codes to match with analysis data
  filter(lan_name == "Hessen") %>%      # Filter for postal codes in Hessen
  group_by(plz_short) %>%
  summarize(geometry = st_union(geometry)) %>%      # Create polygons of aggregated postal code areas (based on shortened postal code)
  ungroup()

# Get all (aggregated) postal code areas as a single column for joining with postal code areas in analysis data
# This is kind of a workaround for "select", because geometry is a "sticky column" that would stick to plz_short
df_PostalCodeAreas_Hessen <- tibble(PLZ = factor(sf_PostalCodeAreas_Hessen$plz_short))

# Report table: Counts and frequencies of patients in all postal code areas
df_Output_PostalCodeCounts <- df_Patients %>%
  group_by(PatientSubgroup, PrimaryPostalCode) %>%
  summarize(Count = n()) %>%
  mutate(Percentage = Count / sum(Count)) %>%
  ungroup()

# Join of all Hessen postal code areas and all represented. This is necessary to make non-represented areas be drawn on the map.
df_PostalCodeCounts_Hessen <- df_Output_PostalCodeCounts %>%
  right_join(df_PostalCodeAreas_Hessen, by = c("PrimaryPostalCode" = "PLZ")) %>%
  mutate(PrimaryPostalCode = factor(PrimaryPostalCode)) %>%
  complete(PatientSubgroup, PrimaryPostalCode)

# Join of spatial data with analysis data
sf_PostalCodeAreas_Hessen <- sf_PostalCodeAreas_Hessen %>%
  left_join(df_PostalCodeCounts_Hessen, by = c("plz_short" = "PrimaryPostalCode"))


# Draw (faceted) map
map_PatientPostalCodes <- ggplot() +
  geom_sf(data = sf_PostalCodeAreas_Hessen,
          mapping = aes(fill = Percentage),
          colour = NA) +
  scale_fill_continuous(low = "#05499630",
                        high =  color_Accent,
                        na.value = color_LightGrey,
                        labels = function(x) paste0(round(100 * x, 0), "%"),
                        name = "Percentage of patients") +
  theme_CCP() +
  theme(axis.line.x = element_blank(),      # Get rid of axes
        axis.text.x = element_blank(),
        axis.line.y = element_blank(),
        axis.text.y = element_blank()) +
  facet_wrap(facets = vars(PatientSubgroup))


########## EXPORT OF PLOTS #####################################################

f_ExportPlot(inp_Plot = plot_Output_AgeAtCancerDiagnosis,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "AgeAtCancerDiagnosis.svg",
             inp_Width = 10,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_AgeDistribution_OverTime,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "AgeDistributionOverTime.svg",
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_CancerTopography,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "CancerTopography.svg",
             inp_Width = 14,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_CaseCount,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "CaseCountPerPatient.svg",
             inp_Width = 16,
             inp_Height = 12)

f_ExportPlot(inp_Plot = plot_Output_CIS,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "CarcinomaInSitu.svg",
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_HIVCancerOverview,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "HIVCancerOverview.svg",
             inp_Width = 20,
             inp_Height = 15)

f_ExportPlot(inp_Plot = plot_Output_MeanLengthOfStay,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "MeanLengthOfStay.svg",
             inp_Width = 16,
             inp_Height = 12)

f_ExportPlot(inp_Plot = plot_Output_MetastasisOccurrence,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "MetastasisOccurrence.svg",
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_SampleSize_OverTime,
             inp_Directory = "~/Projekte/HIVandCancer/Plots",
             inp_Filename = "SampleSizeOverTime.svg",
             inp_Width = 16,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_SankeyDiagnosisProgress,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "SankeyDiagnosisProgress.svg",
             inp_Width = 20,
             inp_Height = 16)

f_ExportPlot(inp_Plot = plot_Output_SankeyTherapySequence,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "SankeyTherapySequence.svg",
             inp_Width = 80,
             inp_Height = 50)

f_ExportPlot(inp_Plot = plot_Output_SexDistribution_OverTime,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "SexDistributionOverTime.svg",
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_Output_TherapyModalities,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "TherapyModalities.svg",
             inp_Width = 20,
             inp_Height = 14,
             inp_LegendPosition = c(0.8, 0.68))

f_ExportPlot(inp_Plot = plot_Output_TimeCancerToMetastasis,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "TimeCancerToMetastasis.svg",
             inp_Width = 15,
             inp_Height = 9,
             inp_LegendPosition = c(0.3, 0.82))

f_ExportPlot(inp_Plot = plot_Output_TimeChemoToComplication,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "TimeChemoToComplication.svg",
             inp_Width = 15,
             inp_Height = 9,
             inp_LegendPosition = "none")

f_ExportPlot(inp_Plot = map_PatientPostalCodes,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "MapPatientPostalCodes.svg",
             inp_Width = 30,
             inp_Height = 15)



f_ExportPlot(inp_Plot = plot_Output_AgeDistribution_OverTime_A,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "AgeDistributionOverTime_A.svg",
             inp_Width = 20,
             inp_Height = 5,
             inp_LegendPosition = "none")

f_ExportPlot(inp_Plot = plot_Output_AgeDistribution_OverTime_B,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "AgeDistributionOverTime_B.svg",
             inp_Width = 20,
             inp_Height = 5,
             inp_LegendPosition = "none")

f_ExportPlot(inp_Plot = plot_Output_AgeDistribution_OverTime_C,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "AgeDistributionOverTime_C.svg",
             inp_Width = 20,
             inp_Height = 5,
             inp_LegendPosition = "none")


f_ExportPlot(inp_Plot = plot_Output_SampleSize_OverTime_A,
             inp_Directory = "~/Projekte/HIVandCancer/Plots",
             inp_FileName = "SampleSizeOverTime_A",
             inp_Width = 20,
             inp_Height = 5,
             inp_LegendPosition = "none")

f_ExportPlot(inp_Plot = plot_Output_SampleSize_OverTime_B,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "SampleSizeOverTime_B_.svg",
             inp_Width = 20,
             inp_Height = 5,
             inp_LegendPosition = "none")

f_ExportPlot(inp_Plot = plot_Output_SampleSize_OverTime_C,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "SampleSizeOverTime_C.svg",
             inp_Width = 20,
             inp_Height = 5,
             inp_LegendPosition = "none")


f_ExportPlot(inp_Plot = plot_Output_SexDistribution_OverTime_A,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "SexDistributionOverTime_A.svg",
             inp_Width = 20,
             inp_Height = 5,
             inp_LegendPosition = "none")

f_ExportPlot(inp_Plot = plot_Output_SexDistribution_OverTime_B,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "SexDistributionOverTime_B.svg",
             inp_Width = 20,
             inp_Height = 5,
             inp_LegendPosition = "none")

f_ExportPlot(inp_Plot = plot_Output_SexDistribution_OverTime_C,
             inp_Directory = "./output/PosterPlots",
             inp_Filename = "SexDistributionOverTime_C.svg",
             inp_Width = 20,
             inp_Height = 5,
             inp_LegendPosition = "none")

