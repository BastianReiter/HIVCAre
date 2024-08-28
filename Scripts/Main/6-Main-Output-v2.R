

################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: OUTPUT (MAIN)                                                     #
#------------------------------------------------------------------------------#
################################################################################
################################################################################
#
#          Across all primary subgroups
#
################################################################################


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Postal Code count
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sample size
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

plot_Output_SampleSize_OverTime <- df_Plot_SampleSize_Main_Filtered %>%
  f_MakeColumnPlot(inp_X = Year,
                   inp_Y = N,
                   inp_FacetFeature = PatientSubgroup,
                   inp_ls_FacetArguments = list(dir = "v",
                                                scales = "free_y"),
                   inp_FacetMapping = "fill",
                   inp_FillPalette = vc_FillPalette_Subgroup)

  f_ExportPlot(inp_Plot = plot_Output_SampleSize_OverTime,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 16,
               inp_Height = 10)
  
  
  plot_Output_SampleSize_OverTime_A <- df_Plot_SampleSize_Main_Filtered %>%
    filter(PatientSubgroup == "Cancer+/HIV-") %>%
    f_MakeColumnPlot(inp_X = Year,
                     inp_Y = N,
                     inp_FillPalette = color_CancerOnly,
                     inp_LegendPosition = "none")
  
  f_ExportPlot(inp_Plot = plot_Output_SampleSize_OverTime_A,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 16,
               inp_Height = 10)
  
  
  plot_Output_SampleSize_OverTime_B <- df_Plot_SampleSize_Main_Filtered %>%
    filter(PatientSubgroup == "Cancer+/HIV+") %>%
    f_MakeColumnPlot(inp_X = Year,
                     inp_Y = N,
                     inp_FillPalette = color_HIVCancer,
                     inp_LegendPosition = "none")
  
  f_ExportPlot(inp_Plot = plot_Output_SampleSize_OverTime_B,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 16,
               inp_Height = 10)
  
  plot_Output_SampleSize_OverTime_C <- df_Plot_SampleSize_Main_Filtered %>%
    filter(PatientSubgroup == "Cancer-/HIV+") %>%
    f_MakeColumnPlot(inp_X = Year,
                     inp_Y = N,
                     inp_FillPalette = color_HIVOnly,
                     inp_LegendPosition = "none")
  
  f_ExportPlot(inp_Plot = plot_Output_SampleSize_OverTime_C,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 16,
               inp_Height = 10)
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sex distribution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Over TIME
   plot_Output_SexDistribution <-   df_Plot_Sex_Main_Filtered %>%
    f_MakeColumnPlot(inp_X = PatientSubgroup,
                     inp_XSpecs = c("Cancer+/HIV-",
                                    "Cancer+/HIV+",
                                    "Cancer-/HIV+"),
                     inp_XAdditionalMapping = "fill",
                     inp_FillPalette = vc_FillPalette_Subgroup,
                     inp_Y = N,
                     inp_GroupingFeature = Sex,
                     inp_GroupingSpecs = c("Other" = "Unknown", "Male" = "M", "Female" = "F"),
                     inp_GroupingPosition = position_fill(),
                     inp_GroupingMapping = "alpha",
                     inp_AlphaPalette = vc_AlphaPalette_3,
                     inp_AxisType_y = "proportional",
                     inp_ls_ThemeArguments = list(inp_Theme_SizeFactorTickLabels_x = 1.3,
                                                  inp_Theme_SizeFactorLegendLabels = 1.3))
  
   f_ExportPlot(inp_Plot = plot_Output_SexDistribution,
                inp_Directory = path,
                inp_FileName = "default",
                inp_FileFormat = "svg",
                inp_Width = 20,
                inp_Height = 10)
   
   plot_Output_SexDistribution_OverTime <- df_Plot_Sex_Main_Filtered %>%
     f_MakeColumnPlot(inp_X = Year,
                      inp_Y = N,
                      inp_GroupingFeature = Sex,
                      inp_GroupingSpecs = c("Other" = "Unknown", "Male" = "M", "Female" = "F"),
                      inp_GroupingPosition = position_fill(),
                      inp_GroupingMapping = "alpha",
                      inp_AlphaPalette = vc_AlphaPalette_3,
                      inp_AxisType_y = "proportional",
                      inp_FacetFeature = PatientSubgroup,
                      inp_ls_FacetArguments = list(dir = "v"),
                      inp_FacetMapping = "fill",
                      inp_FillPalette = vc_FillPalette_Subgroup)
  
  f_ExportPlot(inp_Plot = plot_Output_SexDistribution_OverTime,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
  plot_Output_SexDistribution_OverTime_A <- df_Plot_Sex_Main_Filtered %>%
    filter(PatientSubgroup == "Cancer+/HIV-") %>%
    f_MakeColumnPlot(inp_X = Year,
                     inp_Y = N,
                     inp_GroupingFeature = Sex,
                     inp_GroupingSpecs = c("Other" = "Unknown", "Male" = "M", "Female" = "F"),
                     inp_GroupingPosition = position_fill(),
                     inp_GroupingMapping = "alpha",
                     inp_AlphaPalette = vc_AlphaPalette_3,
                     inp_AxisType_y = "proportional",
                     inp_FillPalette = color_CancerOnly,
                     inp_LegendShowFillGuide = FALSE)
  
  f_ExportPlot(inp_Plot = plot_Output_SexDistribution_OverTime_A,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
  plot_Output_SexDistribution_OverTime_B <- df_Plot_Sex_Main_Filtered %>%
    filter(PatientSubgroup == "Cancer+/HIV+") %>%
    f_MakeColumnPlot(inp_X = Year,
                     inp_Y = N,
                     inp_GroupingFeature = Sex,
                     inp_GroupingSpecs = c("Other" = "Unknown", "Male" = "M", "Female" = "F"),
                     inp_GroupingPosition = position_fill(),
                     inp_GroupingMapping = "alpha",
                     inp_AlphaPalette = vc_AlphaPalette_3,
                     inp_AxisType_y = "proportional",
                     inp_FillPalette = color_CancerOnly,
                     inp_LegendShowFillGuide = FALSE)
  
  f_ExportPlot(inp_Plot = plot_Output_SexDistribution_OverTime_B,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
  plot_Output_SexDistribution_OverTime_C <- df_Plot_Sex_Main_Filtered %>%
    filter(PatientSubgroup == "Cancer-/HIV+") %>%
    f_MakeColumnPlot(inp_X = Year,
                     inp_Y = N,
                     inp_GroupingFeature = Sex,
                     inp_GroupingSpecs = c("Other" = "Unknown", "Male" = "M", "Female" = "F"),
                     inp_GroupingPosition = position_fill(),
                     inp_GroupingMapping = "alpha",
                     inp_AlphaPalette = vc_AlphaPalette_3,
                     inp_AxisType_y = "proportional",
                     inp_FillPalette = color_CancerOnly,
                     inp_LegendShowFillGuide = FALSE)
  
  f_ExportPlot(inp_Plot = plot_Output_SexDistribution_OverTime_C,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Age distribution
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Over TIME
  
plot_Output_AgeDistribution_OverTime <- df_Plot_Age_Main_Filtered %>%
    f_MakeColumnPlot(inp_X = Year,
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
  
  
  f_ExportPlot(inp_Plot = plot_Output_AgeDistribution_OverTime,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
  
  plot_Output_AgeDistribution_OverTime_A <- df_Plot_Age_Main_Filtered %>%
    filter(PatientSubgroup == "Cancer+/HIV-") %>%
    f_MakeColumnPlot(inp_X = Year,
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
  
  f_ExportPlot(inp_Plot = plot_Output_AgeDistribution_OverTime_A,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
  plot_Output_AgeDistribution_OverTime_B <- df_Plot_Age_Main_Filtered %>%
    filter(PatientSubgroup == "Cancer+/HIV+") %>%
    f_MakeColumnPlot(inp_X = Year,
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
  
  f_ExportPlot(inp_Plot = plot_Output_AgeDistribution_OverTime_B,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
  plot_Output_AgeDistribution_OverTime_C <- df_Plot_Age_Main_Filtered %>%
    filter(PatientSubgroup == "Cancer-/HIV+") %>%
    f_MakeColumnPlot(inp_X = Year,
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
  
  f_ExportPlot(inp_Plot = plot_Output_AgeDistribution_OverTime_C,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Case count per patient
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  plot_Output_CaseCount_OverTime <- df_Plot_CaseCount_Main_Filtered %>%
    f_MakeColumnPlot(inp_X = Year,
                     inp_Y = N,
                     inp_GroupingFeature = CaseCountGroup,
                     inp_GroupingPosition = position_fill(),
                     inp_AxisType_y = "proportional",
                     inp_GroupingSpecs = c("More than 10 Cases",
                                           "5 - 10 Cases",
                                           "2 - 4 Cases",
                                           "1 Case"),
                     inp_GroupingMapping = "alpha",
                     inp_AlphaPalette = vc_AlphaPalette_4,
                     inp_FacetFeature = PatientSubgroup,
                     inp_ls_FacetArguments = list(dir = "v",
                                                  scales = "free_y"),
                     inp_FacetMapping = "fill",
                     inp_FillPalette = vc_FillPalette_Subgroup)
  
  
  f_ExportPlot(inp_Plot = plot_Output_CaseCount_OverTime,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Mean length of stay
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  plot_Output_MeanLengthOfStay_OverTime <- df_Plot_MeanLengthOfStay_Main_Filtered %>%
    f_MakeColumnPlot(inp_X = Year,
                     inp_Y = N,
                     inp_GroupingFeature = MeanLengthOfStayGroup,
                     inp_GroupingPosition = position_fill(),
                     inp_AxisType_y = "proportional",
                     inp_GroupingSpecs = c("More than 30 days on average",
                                           "14 - 30 days on average",
                                           "7 - 14 days on average",
                                           "Up to 7 days on average"),
                     inp_GroupingMapping = "alpha",
                     inp_AlphaPalette = vc_AlphaPalette_4,
                     inp_FacetFeature = PatientSubgroup,
                     inp_ls_FacetArguments = list(dir = "v",
                                                  scales = "free_y"),
                     inp_FacetMapping = "fill",
                     inp_FillPalette = vc_FillPalette_Subgroup)
  
  
  f_ExportPlot(inp_Plot =  plot_Output_MeanLengthOfStay_OverTime,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
########## HIVandCancer vs. CancerOnly #########################################
  
  
#!!!-!!!-!!! Plot describing completeness of data (Sankey diagram with nodes main diagnosis - treatment - follow up?)
  
  
  
#--------- Age at presumed cancer diagnosis ----------------------------------------
  # Attention! Freiburg only matched patients available in the full data set!!!
  
  plot_Output_AgeAtCancerDiagnosis_OverTime <- df_Plot_AgeAtCancerDiagnosis_Main_Filtered %>%
    f_MakeColumnPlot(inp_X = Year,
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
  
  
  f_ExportPlot(inp_Plot = plot_Output_AgeAtCancerDiagnosis_OverTime,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
  
  plot_Output_AgeAtCancerDiagnosis_OverTime_A <- df_Plot_AgeAtCancerDiagnosis_Main_Filtered %>%
    filter(PatientSubgroup == "Cancer+/HIV-") %>%
    f_MakeColumnPlot(inp_X = Year,
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
  
  f_ExportPlot(inp_Plot = plot_Output_AgeAtCancerDiagnosis_OverTime_A,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
  plot_Output_AgeAtCancerDiagnosis_OverTime_B <- df_Plot_AgeAtCancerDiagnosis_Main_Filtered %>%
    filter(PatientSubgroup == "Cancer+/HIV+") %>%
    f_MakeColumnPlot(inp_X = Year,
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
  
  f_ExportPlot(inp_Plot = plot_Output_AgeAtCancerDiagnosis_OverTime_B,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Count of distinct documented "real" cancer codes
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Not avible at Cologne
  
  plot_Output_DistinctCodeCountCancer_Main_Filtered <- df_Plot_DistinctCodeCountCancer_Main_Filtered %>%
    f_MakeColumnPlot(inp_X = Year,
                     inp_Y = N,
                     inp_GroupingFeature = CancerCountGroup,
                     inp_GroupingPosition = position_fill(),
                     inp_AxisType_y = "proportional",
                     inp_GroupingSpecs = c("3 or more distinct cancers",
                                           "2 distinct cancers",
                                           "1 distinct cancer"),
                     inp_GroupingMapping = "alpha",
                     inp_AlphaPalette = vc_AlphaPalette_4,
                     inp_FacetFeature = PatientSubgroup,
                     inp_ls_FacetArguments = list(dir = "v",
                                                  scales = "free_y"),
                     inp_FacetMapping = "fill",
                     inp_FillPalette = vc_FillPalette_Subgroup)
  
  
  f_ExportPlot(inp_Plot =   plot_Output_DistinctCodeCountCancer_Main_Filtered,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer grouping: AD, NAD and Non-HIV-associated cancer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#df_proportion_HIVCancerCategories <- df_Plot_HIVCancerCategories_Main_Filtered %>%
 #  filter(PatientSubgroup == "Cancer+/HIV+") %>% 
  # group_by(Year) %>% mutate(Sum = sum(N)) %>% group_by(Year, PatientSubgroupHIVCancerCategory)%>%
   #mutate(Rate = N/Sum) 
  
df_temp <- df_proportion_HIVCancerCategories %>% 
   filter(PatientSubgroupHIVCancerCategory == "HIV-associated AD cancer")

model <- lm(Rate~Year, df_temp)
summary(model)
  plot(df_temp$Year, df_temp$Rate)
  abline(model, col = "red")
 
df_Plot_HIVCancerCategories_Main_Filtered %>%
  filter(PatientSubgroup == "Cancer+/HIV+") %>% 
  group_by(Year) %>% mutate(Sum = sum(N)) %>% group_by(Year, PatientSubgroupHIVCancerCategory)%>%
  mutate(Rate = N/Sum) %>%
  ggplot() +
  aes(x = Year, y= Rate) +
  #scale_fill_brewer(palette="BuGn") +
  geom_line()+ 
  geom_smooth(method='lm', colour="#054996", size=0.5)+
  facet_wrap(~PatientSubgroupHIVCancerCategory)
  
  plot_Output_HIVCancerCategories_Main_Filtered <- df_Plot_HIVCancerCategories_Main_Filtered %>%
    filter(PatientSubgroup == "Cancer+/HIV+") %>%
      f_MakeColumnPlot(inp_X = Year,
                     inp_Y = N,
                     inp_GroupingFeature = PatientSubgroupHIVCancerCategory,
                     inp_GroupingPosition = position_fill(),
                     inp_AxisType_y = "proportional",
                     inp_GroupingSpecs = c("Non-HIV-associated cancer",
                                           "HIV-associated non-AD cancer",
                                           "HIV-associated AD cancer"),
                     inp_GroupingMapping = "alpha",
                     inp_AlphaPalette = vc_AlphaPalette_4,
                     inp_FacetFeature = PatientSubgroup,
                     inp_ls_FacetArguments = list(dir = "v",
                                                  scales = "free_y"),
                     inp_FacetMapping = "fill",
                     inp_FillPalette = vc_FillPalette_Subgroup, 
                     insert_line = TRUE)
  
  
  f_ExportPlot(inp_Plot = plot_Output_HIVCancerCategories_Main_Filtered,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
  plot_Output_HIVCancerCategories_Main_Filtered_HIV <- df_Plot_HIVCancerCategories_Main_Filtered_HIV %>%
    f_MakeColumnPlot(inp_X = Year,
                     inp_Y = N,
                     inp_GroupingFeature = PatientSubgroupHIVCancerCategory,
                     inp_GroupingPosition = position_fill(),
                     inp_AxisType_y = "proportional",
                     inp_GroupingSpecs = c("Non-HIV-associated cancer",
                                           "HIV-associated non-AD cancer",
                                           "HIV-associated AD cancer"),
                     inp_GroupingMapping = "alpha",
                     inp_AlphaPalette = vc_AlphaPalette_4,
                     inp_FacetFeature = PatientSubgroup,
                     inp_ls_FacetArguments = list(dir = "v",
                                                  scales = "free_y"),
                     inp_FacetMapping = "fill",
                     inp_FillPalette = vc_FillPalette_Subgroup)
  
  
  f_ExportPlot(inp_Plot = plot_Output_HIVCancerCategories_Main_Filtered_HIV,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
  ggplot(df_Plot_HIVCancerCategories_Main_Filtered_HIV, aes(x=Year, y=N, group=1)) + 
           geom_line() +
    facet_wrap(~PatientSubgroupHIVCancerCategory) +
    
    geom_smooth(method='glm', colour='#8e1e39',size=0.5)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer grouping: Carcinoma in situ
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  plot_Output_CIS_Main_Filtered <- df_Plot_CIS_Main_Filtered %>%
    f_MakeColumnPlot(inp_X = Year,
                     inp_Y = N,
                     inp_GroupingFeature = CancerIsCarcinomaInSitu,
                     inp_GroupingPosition = position_fill(),
                     inp_AxisType_y = "proportional",
                     inp_GroupingSpecs = c("TRUE",
                                           "FALSE",
                                           "NA"),
                     inp_GroupingMapping = "alpha",
                     inp_AlphaPalette = vc_AlphaPalette_4,
                     inp_FacetFeature = PatientSubgroup,
                     inp_ls_FacetArguments = list(dir = "v",
                                                  scales = "free_y"),
                     inp_FacetMapping = "fill",
                     inp_FillPalette = vc_FillPalette_Subgroup)
  
  
  f_ExportPlot(inp_Plot = plot_Output_CIS_Main_Filtered,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: Topography by Organ
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  

  
  # Make "pyramid plot"
  plot_Output_CancerTopographyDetail <- df_Plot_MainCancerTopographyDetail_Main_Filtered %>%
    mutate(Proportion = case_when(PatientSubgroup == "Cancer+/HIV-" ~ -Proportion,
                                  TRUE ~ Proportion)) %>%
    f_MakeColumnPlot(inp_X = MainCancerTopographyDetail,
                     inp_XSpecs = rev(vc_TopographyDetailSorted),
                     inp_Y = Proportion,
                     inp_GroupingFeature = PatientSubgroup,
                     inp_GroupingSpecs = c("Cancer+/HIV+", "Cancer+/HIV-"),
                     inp_ColumnWidth = 0.8,
                     inp_FillPalette = vc_FillPalette_Subgroup,
                     inp_AxisType_y = "proportional",
                     inp_CoordFlip = TRUE,
                     inp_TickLabelWidth_x = 40,
                     inp_LegendPosition = "top")
  
  # Manual modifications to comply pyramid plot needs
  plot_Output_CancerTopographyDetail <- plot_Output_CancerTopographyDetail +
    theme(axis.text.y = element_text(margin = margin(r = 10, unit = "pt"))) +
    scale_y_continuous(limits = c(-0.15, 0.15),
                       breaks = seq(from = -0.15, to = 0.15, by = 0.05),
                       labels = function(x) paste(abs(round(x * 100, 0)), "%")) +
    scale_fill_manual(values = vc_FillPalette_Subgroup,
                      breaks = c("Cancer+/HIV-", "Cancer+/HIV+"),
                      name = NULL) +
    guides(fill = guide_legend(override.aes = list(alpha = 0.8)))
  
  f_ExportPlot(inp_Plot = plot_Output_CancerTopographyDetail,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
  
  plot_Output_MainCancerTopographyDetail_HIV <- df_Plot_MainCancerTopographyDetail_Main_Filtered %>%
    filter(PatientSubgroup == "Cancer+/HIV+") %>%
    f_MakeColumnPlot(inp_X = MainCancerTopographyDetail,
                     inp_XSpecs = rev(vc_TopographyDetailSorted),
                     inp_Y = Proportion,
                     inp_ColumnWidth = 0.8,
                     inp_FillPalette = color_HIVCancer,
                     inp_AxisType_y = "proportional",
                     inp_CoordFlip = TRUE,
                     inp_TickLabelWidth_x = 40,
                     inp_LegendShowFillGuide = FALSE 
                    )
  
  f_ExportPlot(inp_Plot = plot_Output_MainCancerTopographyDetail_HIV,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 10)
  
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: Topography by ICD Grouping
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  plot_Output_MainCancerTopographyGroup <- ggplot(df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered_HIV) +
    aes(x = MainCancerDiagnosisYear, y= N) +
    #scale_fill_brewer(palette="BuGn") +
    geom_line()+ 
    geom_smooth(method='lm', colour='#8e1e39',size=0.5)+
     facet_wrap(~MainCancerTopographyGroup) +
          # Margin around entire plot) +
    ggtitle("Cancer Topography")
  
  ggsave("plot_Output_MainCancerTopographyGroup_HIV.png", 
         path = path,
         width = 20, 
         height = 15, 
         units = "cm")
  
  plot_Output_MainCancerTopographyGroup_Controll <- ggplot(df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered_Controll) +
    aes(x = MainCancerDiagnosisYear, y= N) +
    #scale_fill_brewer(palette="BuGn") +
    geom_line()+ 
    geom_smooth(method='lm', colour="#054996", size=0.5)+
    facet_wrap(~MainCancerTopographyGroup) +
    ggtitle("Cancer Topography")
  
  ggsave("plot_Output_MainCancerTopographyGroup_Controll.png", 
         path = path,
         width = 20, 
         height = 15, 
         units = "cm")
  
 plot_CancerPerYear <-  df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered_HIV %>%
   group_by(MainCancerDiagnosisYear) %>%
   summarise(CancerSum = sum(N)) %>%
   ggplot() + aes(x = MainCancerDiagnosisYear, y= CancerSum) +
   geom_line()
  
  ggplot(df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered_HIV) +
    aes(x = MainCancerDiagnosisYear, y= N, fill=MainCancerTopographyGroup) +
    geom_bar(position="fill", stat="identity")
  
  ggplot(df_Plot_MainCancerTopographyGroup_OverTime_Main_Filtered_HIV) +
    aes(x = MainCancerDiagnosisYear, y= N, fill=MainCancerTopographyGroup) +
    geom_bar(position="dodge", stat="identity")
  
  #Proportion per Year per Group berechnen
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cancer Grouping: By entity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  
  plot_Output_CancerEntities <- ggplot(df_Plot_CancerEntities_OverTime_Main_Filtered_HIV) +
    aes(x = MainCancerDiagnosisYear, y= N) +
    #scale_fill_brewer(palette="BuGn") +
    geom_line()+ 
    geom_smooth(method='lm') +
    facet_wrap(~MainCancerCode) +
    ggtitle("Cancer Code: Entity (detailed)")
  
  ggsave("plot_Output_CancerEntitiesDetailed.png", 
         path = path,
         width = 20, 
         height = 15, 
         units = "cm")
  
#Cancer Entities Grouped (3 digits)
  plot_Output_CancerEntitiesGrouped <- ggplot(df_Plot_CancerEntitiesGrouped_OverTime_Main_Filtered_HIV) +
    aes(x = MainCancerDiagnosisYear, y= N) +
    #scale_fill_brewer(palette="BuGn") +
    geom_line()+ 
    geom_smooth(method='lm') +
    facet_wrap(~MainCancerCode) +
    ggtitle("Cancer Code: Entity (detailed)")
  
  ggsave("plot_Output_CancerEntitiesGroupedDetailed.png", 
         path = path,
         width = 20, 
         height = 15, 
         units = "cm")
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Therapy modalities
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  plot_Output_AnyMajorTherapy <- df_Plot_TherapyModalities_OverTime_Main_Filtered %>%
    select(PatientSubgroup,
           CountNoMajorTherapyCoded_overall,
           CountAnyMajorTherapy_overall) %>%
    pivot_longer(cols = starts_with("Count"),
                 names_to = "Group",
                 values_to = "Count") %>%
    f_MakeColumnPlot(inp_X = PatientSubgroup,
                     inp_XAdditionalMapping = "fill",
                     inp_FillPalette = vc_FillPalette_Subgroup,
                     inp_Y = Count,
                     inp_GroupingFeature = Group,
                     inp_GroupingPosition = position_fill(),
                     inp_GroupingSpecs = c("No Major Therapy coded" = "CountNoMajorTherapyCoded_overall",
                                           "Any Major Therapy" = "CountAnyMajorTherapy_overall"),
                     inp_GroupingMapping = "alpha",
                     inp_AxisType_y = "proportional",
                     inp_ls_ThemeArguments = list(inp_Theme_SizeFactorTickLabels_x = 1.3,
                                                  inp_Theme_SizeFactorLegendLabels = 1.3))
  
  f_ExportPlot(inp_Plot =   plot_Output_AnyMajorTherapy,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 14
               )
  
  plot_Output_TherapyModalities <- df_Plot_TherapyModalities_Main_Filtered %>%
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

  
  df_Output_TherapyModalities_Main_Filtered %>% ggplot( aes(x = MainCancerDiagnosisYear, y= ProportionSurgery)) +
    geom_bar(stat="identity") +
    facet_wrap(~PatientSubgroup) 
  
  f_ExportPlot(inp_Plot =   plot_Output_TherapyModalities,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 14
               )
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Complications after (Chemo)therapy
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ggplot(data=df_Plot_TherapyComplications_Main_Filtered, aes(x=PatientSubgroup, y=(ProportionComplicationAfterChemotherapy*100), fill=PatientSubgroup)) +
    geom_bar(stat="identity") +
  #  ggtitle("Complications after Chemotherapy") +
    ylab("Proportion [%]") +
    theme(text = element_text(family = "Karla", size = 15), 
          #plot.background = element_rect(fill = "transparent", color = NA),      # Transparent plot background, no border
          plot.margin = margin(0.5, 1, 0.5, 1, unit = "cm"),      # Margin around entire plot
          plot.title = element_text(face = "bold", size = rel(2.5), hjust = 0),
          plot.subtitle = element_text(size = rel(1.5), margin = margin(0.2, 0, 1, 0, unit = "cm"), hjust = 0),
          plot.caption = element_text(size = rel(1.5), margin = margin(1, 0, 0, 0, unit = "cm"), hjust = 1),
          #--- Axis parameters -----------------------------------------------
          axis.text = element_text(face = "bold", color = color_DarkGrey),      # Axis tick labels
          axis.text.x = element_text(size = rel(1.5)),      # x Axis tick label size
          axis.text.y = element_text(size = rel(1.5)),      # y Axis tick label size
          axis.title = element_text(face = "bold", color = color_DarkGrey, size = rel(1.5)),      # Axis title labels
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),      # No axis tick marks
          axis.line = element_line(color = "black",
                                                linewidth = 0.5,
                                                linetype = 1),      # Arrow at top end of y-axis
          legend.background = element_rect(fill = "transparent", color = NA), 
          legend.title =  element_blank(),
          legend.text = element_text(color = color_DarkGrey, size = rel(1.5), face = "bold"),
          # Panel background
          panel.background = element_rect(fill = "transparent", color = NA),      # Transparent panel background, no border
          panel.border = element_blank(),      # No panel border
          panel.grid.minor = element_blank(),      # Do not display minor grid lines
          panel.grid.major.x = element_blank(),      # Do not display major grid lines of x axis
          panel.grid.major.y =  element_line(color = color_MediumGrey)      # Color of y axis major grid lines
          # Text
          ) +
    scale_fill_manual(values=c('#054996','#8e1e39'))
  
  ggsave("plot_Output_TherapyComplications.png", 
         path = path,
         width = 20, 
         height = 15, 
         units = "cm")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Last recorded discharge reason
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  plot_Output_LastRecordedDischargeReason <- df_Output_LastRecordedDischargeCategory_Main_Filtered %>%
    f_MakeColumnPlot(inp_X = PatientSubgroup,
                     inp_XAdditionalMapping = "fill",
                     inp_FillPalette = vc_FillPalette_Subgroup,
                     inp_Y = N,
                     inp_GroupingFeature = LastRecordedDischargeCategory,
                     inp_GroupingSpecs = c("Other",
                                           "Rehabilitation or Residential Care",
                                           "Home",
                                           "Other Hospital",
                                           "Hospice Care",
                                           "Deceased"),
                     inp_GroupingPosition = "fill",
                     inp_GroupingMapping = "alpha",
                     inp_AxisType_y = "proportional")
  
  f_ExportPlot(inp_Plot = plot_Output_LastRecordedDischargeReason,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 14
  )
  
  
ggplot(data=df_Plot_LastRecordedDischargeCategory_Main_Filtered, aes(x=LastRecordedDischargeCategory_sort, y=(Proportion*100), fill=PatientSubgroup)) +
   geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("Last Recorded Discharge Category") +
  ylab("Proportion [%]") +
  theme(text = element_text(family = "Karla", size = 11), 
        plot.margin = margin(0.5, 1, 0.5, 1, unit = "cm"),      # Margin around entire plot
        plot.title = element_text(face = "bold", size = rel(2.5), hjust = 0),
        axis.text = element_text(face = "bold", color = color_DarkGrey),      # Axis tick labels
        axis.text.x = element_text(size = rel(2)),      # x Axis tick label size
        axis.text.y = element_text(size = rel(1.5)), 
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = rel(2)), 
        legend.background = element_rect(fill = "transparent", color = NA), 
        legend.title = element_text(color = color_DarkGrey, size = rel(1.5), face = "bold"),
        legend.text = element_text(color = color_DarkGrey, size = rel(1.5), face = "bold")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_fill_manual(values=c('#054996','#8e1e39'))

ggsave("plot_Output_LastRecordedDischargeCategory.png", 
       path = path,
       width = 20, 
       height = 15, 
       units = "cm")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # HIV and cancer presumed diagnosis order
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
  plot_Output_HIVCancerDiagnosisOrder <- df_Output_HIVCancerDiagnosisOrder_Main_Filtered  %>%
    f_MakeColumnPlot(inp_X = MainCancerDiagnosisYear,
                     inp_Y = N,
                     inp_GroupingFeature = HIVCancerDiagnosisOrder,
                     inp_GroupingMapping = "alpha",
                     inp_AlphaPalette = vc_AlphaPalette_3,
                     inp_FillPalette = color_HIVCancer,
                     inp_GroupingPosition = position_fill(),
                     inp_AxisType_y = "proportional")
  
  f_ExportPlot(inp_Plot = plot_Output_HIVCancerDiagnosisOrder,
               inp_Directory = path,
               inp_FileName = "default",
               inp_FileFormat = "svg",
               inp_Width = 20,
               inp_Height = 14
  )