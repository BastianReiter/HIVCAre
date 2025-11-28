

################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: OUTPUT (MAIN)                                                     #
#------------------------------------------------------------------------------#
################################################################################


#PlotOutputPath <- "./Reporting/Publication"
PlotOutputPath <- "C:/Users/Basti/OneDrive/ARBEIT/IDMKD/Projekte/HIVCAre/Publikation/Paper/JMIR Public Health and Surveillance/Review/Images"


tp_begin <- 2005
tp_end <- 2022


GetCI <- function(x, ...)
{
  DescTools::MultinomCI(x, ...) %>%
      as_tibble() %>%
      rename(PropCheck = 1, CI.lower = 2, CI.upper = 3)
}


FormatNumbers <- function(x)
{
  format(x, big.mark = ",",
            decimal.mark = ".",
            width = NULL)
}


FormatPValue <- function(p)
{
  case_when(p < 0.001 ~ "P < .001",
            p < 0.01 ~ paste0("P = ", sub("^0\\.", ".", formatC(p, digits = 3, format = "f"))),
            between(p, 0.045, 0.05499999) ~ paste0("P = ", sub("^0\\.", ".", formatC(p, digits = 3, format = "f"))),
            p > 0.99 ~ "P > 0.99",
            .default = paste0("P = ", sub("^0\\.", ".", formatC(p, digits = 2, format = "f"))))
}



#===============================================================================
# Characteristics of ALL GROUPS (temporally stratified)
#===============================================================================

#-------------------------------------------------------------------------------
Full.SampleSize.TimeGroups <- CumulatedData_Full$df_Output_SampleSize %>%
                                  filter(FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end) %>%
                                  mutate(Strata = case_when(FirstRelevantAdmissionYear >= 2005 & FirstRelevantAdmissionYear <= 2008 ~ "2005 - 2008",
                                                            FirstRelevantAdmissionYear >= 2009 & FirstRelevantAdmissionYear <= 2014 ~ "2009 - 2014",
                                                            FirstRelevantAdmissionYear >= 2015 & FirstRelevantAdmissionYear <= 2022 ~ "2015 - 2022")) %>%
                                  group_by(PatientSubgroup, Strata) %>%
                                      summarize(N = sum(N)) %>%
                                  ungroup()

Full.SampleSize.TotalTime <- Full.SampleSize.TimeGroups %>%
                                group_by(PatientSubgroup) %>%
                                    summarize(Strata = "Total",
                                              N = sum(N))

Full.SampleSize <- Full.SampleSize.TimeGroups %>%
                        bind_rows(Full.SampleSize.TotalTime) %>%
                        pivot_wider(names_from = PatientSubgroup,
                                    values_from = N) %>%
                        mutate(across(-Strata, ~ FormatNumbers(.x)))


#-------------------------------------------------------------------------------

Full.FemaleSex.TimeGroups <- CumulatedData_Full$df_Output_Sex %>%
                                filter(FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end) %>%
                                mutate(Strata = case_when(FirstRelevantAdmissionYear >= 2005 & FirstRelevantAdmissionYear <= 2008 ~ "2005 - 2008",
                                                          FirstRelevantAdmissionYear >= 2009 & FirstRelevantAdmissionYear <= 2014 ~ "2009 - 2014",
                                                          FirstRelevantAdmissionYear >= 2015 & FirstRelevantAdmissionYear <= 2022 ~ "2015 - 2022")) %>%
                                pivot_wider(names_from = Site,
                                            values_from = N) %>%
                                mutate(N = rowSums(across(all_of(SiteNames)), na.rm = TRUE)) %>%
                                group_by(PatientSubgroup, Strata, Sex) %>%
                                    summarize(N = sum(N)) %>%
                                group_by(PatientSubgroup, Strata) %>%
                                    reframe(Sex = Sex,
                                            N = N,
                                            Proportion = N / sum(N)) %>%
                                ungroup() %>%
                                filter(Sex == "F") %>%
                                select(-Sex)

Full.FemaleSex.TotalTime <- Full.FemaleSex.TimeGroups %>%
                                group_by(PatientSubgroup) %>%
                                    summarize(Strata = "Total",
                                              Proportion = sum(N) / sum(N / Proportion),
                                              N = sum(N)) %>%
                                ungroup()

Full.FemaleSex <- Full.FemaleSex.TimeGroups %>%
                      bind_rows(Full.FemaleSex.TotalTime) %>%
                      mutate(Output = paste0(FormatNumbers(N), " (", FormatNumbers(round(Proportion * 100, digits = 1)), "%)")) %>%
                      select(-N, -Proportion) %>%
                      pivot_wider(names_from = PatientSubgroup,
                                  values_from = Output)


#-------------------------------------------------------------------------------
Full.Age.TimeGroups <- CumulatedData_Full$df_Output_Age %>%
                           filter(FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end) %>%
                           mutate(Strata = case_when(FirstRelevantAdmissionYear >= 2005 & FirstRelevantAdmissionYear <= 2008 ~ "2005 - 2008",
                                                     FirstRelevantAdmissionYear >= 2009 & FirstRelevantAdmissionYear <= 2014 ~ "2009 - 2014",
                                                     FirstRelevantAdmissionYear >= 2015 & FirstRelevantAdmissionYear <= 2022 ~ "2015 - 2022")) %>%
                           pivot_wider(names_from = Site,
                                       values_from = N) %>%
                           mutate(N = rowSums(across(all_of(SiteNames)), na.rm = TRUE)) %>%
                           group_by(PatientSubgroup, Strata, AgeGroup) %>%
                               summarize(N = sum(N)) %>%
                           group_by(PatientSubgroup, Strata) %>%
                               reframe(AgeGroup = AgeGroup,
                                       N = N,
                                       Proportion = N / sum(N)) %>%
                           ungroup()

Full.Age.TotalTime <- expand(Full.Age.TimeGroups,
                             PatientSubgroup,
                             Strata,
                             AgeGroup) %>%
                        left_join(Full.Age.TimeGroups) %>%
                        group_by(PatientSubgroup, AgeGroup) %>%
                            summarize(Strata = "Total",
                                      Proportion = ifelse(any(is.na(N)), NA, sum(N) / sum(N / Proportion)),
                                      N = sum(N, na.rm = TRUE)) %>%
                        ungroup()

Full.Age <- Full.Age.TimeGroups %>%
                bind_rows(Full.Age.TotalTime) %>%
                mutate(Output = paste0(FormatNumbers(N), " (", ifelse(!is.na(Proportion), FormatNumbers(round(Proportion * 100, digits = 1)), "CALCMAN"), "%)")) %>%
                select(-N, -Proportion) %>%
                pivot_wider(names_from = PatientSubgroup,
                            values_from = Output)


#-------------------------------------------------------------------------------
Full.Admissions.TimeGroups <- CumulatedData_Full$df_Output_CaseCount %>%
                                   filter(FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end) %>%
                                   mutate(Strata = case_when(FirstRelevantAdmissionYear >= 2005 & FirstRelevantAdmissionYear <= 2008 ~ "2005 - 2008",
                                                             FirstRelevantAdmissionYear >= 2009 & FirstRelevantAdmissionYear <= 2014 ~ "2009 - 2014",
                                                             FirstRelevantAdmissionYear >= 2015 & FirstRelevantAdmissionYear <= 2022 ~ "2015 - 2022")) %>%
                                   pivot_wider(names_from = Site,
                                               values_from = N) %>%
                                   mutate(N = rowSums(across(all_of(SiteNames)), na.rm = TRUE)) %>%
                                   group_by(PatientSubgroup, Strata, CaseCountGroup) %>%
                                       summarize(N = sum(N)) %>%
                                   group_by(PatientSubgroup, Strata) %>%
                                       reframe(CaseCountGroup = CaseCountGroup,
                                               N = N,
                                               Proportion = N / sum(N)) %>%
                                   ungroup()

Full.Admissions.TotalTime <- expand(Full.Admissions.TimeGroups,
                                    PatientSubgroup,
                                    Strata,
                                    CaseCountGroup) %>%
                               left_join(Full.Admissions.TimeGroups) %>%
                               group_by(PatientSubgroup, CaseCountGroup) %>%
                                   summarize(Strata = "Total",
                                             Proportion = ifelse(any(is.na(N)), NA, sum(N) / sum(N / Proportion)),
                                             N = sum(N, na.rm = TRUE)) %>%
                               ungroup()

Full.Admissions <- Full.Admissions.TimeGroups %>%
                        bind_rows(Full.Admissions.TotalTime) %>%
                        mutate(Output = paste0(FormatNumbers(N), " (", ifelse(!is.na(Proportion), FormatNumbers(round(Proportion * 100, digits = 1)), "CALCMAN"), "%)")) %>%
                        select(-N, -Proportion) %>%
                        pivot_wider(names_from = PatientSubgroup,
                                    values_from = Output) %>%
                        mutate(CaseCountGroup = str_replace(CaseCountGroup, "Case", "admission"))


#-------------------------------------------------------------------------------
Full.MeanLengthOfStay.TimeGroups <- CumulatedData_Full$df_Output_MeanLengthOfStay %>%
                                         filter(FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end) %>%
                                         mutate(Strata = case_when(FirstRelevantAdmissionYear >= 2005 & FirstRelevantAdmissionYear <= 2008 ~ "2005 - 2008",
                                                                   FirstRelevantAdmissionYear >= 2009 & FirstRelevantAdmissionYear <= 2014 ~ "2009 - 2014",
                                                                   FirstRelevantAdmissionYear >= 2015 & FirstRelevantAdmissionYear <= 2022 ~ "2015 - 2022")) %>%
                                         pivot_wider(names_from = Site,
                                                     values_from = N) %>%
                                         mutate(N = rowSums(across(all_of(SiteNames)), na.rm = TRUE)) %>%
                                         group_by(PatientSubgroup, Strata, MeanLengthOfStayGroup) %>%
                                             summarize(N = sum(N)) %>%
                                         group_by(PatientSubgroup, Strata) %>%
                                             reframe(MeanLengthOfStayGroup = MeanLengthOfStayGroup,
                                                     N = N,
                                                     Proportion = N / sum(N)) %>%
                                         ungroup()

Full.MeanLengthOfStay.TotalTime <- expand(Full.MeanLengthOfStay.TimeGroups,
                                          PatientSubgroup,
                                          Strata,
                                          MeanLengthOfStayGroup) %>%
                                     left_join(Full.MeanLengthOfStay.TimeGroups) %>%
                                     group_by(PatientSubgroup, MeanLengthOfStayGroup) %>%
                                         summarize(Strata = "Total",
                                                   Proportion = ifelse(any(is.na(N)), NA, sum(N) / sum(N / Proportion)),
                                                   N = sum(N, na.rm = TRUE)) %>%
                                     ungroup()

Full.MeanLengthOfStay <- Full.MeanLengthOfStay.TimeGroups %>%
                              bind_rows(Full.MeanLengthOfStay.TotalTime) %>%
                              mutate(Output = paste0(FormatNumbers(N), " (", ifelse(!is.na(Proportion), FormatNumbers(round(Proportion * 100, digits = 1)), "CALCMAN"), "%)")) %>%
                              select(-N, -Proportion) %>%
                              pivot_wider(names_from = PatientSubgroup,
                                          values_from = Output) %>%
                              mutate(MeanLengthOfStayGroup = str_replace(MeanLengthOfStayGroup, "average", "avg."),
                                     MeanLengthOfStayGroup = str_replace(MeanLengthOfStayGroup, "More than", ">")) %>%
                              group_by(Strata) %>%
                              arrange(factor(MeanLengthOfStayGroup, levels = c("Up to 7 days on avg.",
                                                                               "7 - 14 days on avg.",
                                                                               "14 - 30 days on avg.",
                                                                               "> 30 days on avg.")),
                                      .by_group = TRUE)
                              


#-------------------------------------------------------------------------------
Full.DistinctCancers.TimeGroups <- CumulatedData_Full$df_Output_DistinctCodeCountCancer %>%
                                       filter(FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end) %>%
                                       mutate(Strata = case_when(FirstRelevantAdmissionYear >= 2005 & FirstRelevantAdmissionYear <= 2008 ~ "2005 - 2008",
                                                                 FirstRelevantAdmissionYear >= 2009 & FirstRelevantAdmissionYear <= 2014 ~ "2009 - 2014",
                                                                 FirstRelevantAdmissionYear >= 2015 & FirstRelevantAdmissionYear <= 2022 ~ "2015 - 2022")) %>%
                                       pivot_wider(names_from = Site,
                                                   values_from = N) %>%
                                       mutate(N = rowSums(across(all_of(SiteNames)), na.rm = TRUE)) %>%
                                       group_by(PatientSubgroup, Strata, CancerCountGroup) %>%
                                           summarize(N = sum(N)) %>%
                                       group_by(PatientSubgroup, Strata) %>%
                                           reframe(CancerCountGroup = CancerCountGroup,
                                                   N = N,
                                                   Proportion = N / sum(N)) %>%
                                       ungroup()

Full.DistinctCancers.TotalTime <- expand(Full.DistinctCancers.TimeGroups,
                                         PatientSubgroup,
                                         Strata,
                                         CancerCountGroup) %>%
                                    left_join(Full.DistinctCancers.TimeGroups) %>%
                                    group_by(PatientSubgroup, CancerCountGroup) %>%
                                        summarize(Strata = "Total",
                                                  Proportion = ifelse(any(is.na(N)), NA, sum(N) / sum(N / Proportion)),
                                                  N = sum(N, na.rm = TRUE)) %>%
                                    ungroup()

Full.DistinctCancers <- Full.DistinctCancers.TimeGroups %>%
                                bind_rows(Full.DistinctCancers.TotalTime) %>%
                                mutate(Output = paste0(FormatNumbers(N), " (", ifelse(!is.na(Proportion), FormatNumbers(round(Proportion * 100, digits = 1)), "CALCMAN"), "%)")) %>%
                                select(-N, -Proportion) %>%
                                pivot_wider(names_from = PatientSubgroup,
                                            values_from = Output)


#-------------------------------------------------------------------------------
Full.CIS.TimeGroups <- CumulatedData_Full$df_Output_CIS %>%
                            filter(MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end) %>%
                            mutate(PatientSubgroup = droplevels(PatientSubgroup),      # Drop empty level 'Cancer-/HIV+'
                                   Strata = case_when(MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005 - 2008",
                                                      MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009 - 2014",
                                                      MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2022 ~ "2015 - 2022")) %>%
                            pivot_wider(names_from = Site,
                                        values_from = N) %>%
                            mutate(N = rowSums(across(all_of(SiteNames)), na.rm = TRUE)) %>%
                            group_by(PatientSubgroup, Strata, MainCancerIsCarcinomaInSitu) %>%
                                summarize(N = sum(N)) %>%
                            group_by(PatientSubgroup, Strata) %>%
                                reframe(MainCancerIsCarcinomaInSitu = MainCancerIsCarcinomaInSitu,
                                        N = N,
                                        Proportion = N / sum(N)) %>%
                            ungroup() %>%
                            filter(MainCancerIsCarcinomaInSitu == TRUE) %>%
                            select(-MainCancerIsCarcinomaInSitu)

Full.CIS.TotalTime <- expand(Full.CIS.TimeGroups,
                             PatientSubgroup,
                             Strata) %>%
                          left_join(Full.CIS.TimeGroups) %>%
                          group_by(PatientSubgroup) %>%
                              summarize(Strata = "Total",
                                        Proportion = ifelse(any(is.na(N)), NA, sum(N) / sum(N / Proportion)),
                                        N = sum(N, na.rm = TRUE)) %>%
                          ungroup()

Full.CIS <- Full.CIS.TimeGroups %>%
                bind_rows(Full.CIS.TotalTime) %>%
                mutate(Output = paste0(FormatNumbers(N), " (", ifelse(!is.na(Proportion), FormatNumbers(round(Proportion * 100, digits = 1)), "CALCMAN"), "%)")) %>%
                select(-N, -Proportion) %>%
                pivot_wider(names_from = PatientSubgroup,
                            values_from = Output)


#-------------------------------------------------------------------------------
Full.CancerTopography.TimeGroups <- CumulatedData_Full$df_Output_MainCancerTopographyGroup_OverTime %>%
                                         filter(MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end) %>%
                                         mutate(PatientSubgroup = droplevels(PatientSubgroup),
                                                Strata = case_when(MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005 - 2008",
                                                                   MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009 - 2014",
                                                                   MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2022 ~ "2015 - 2022")) %>%
                                         pivot_wider(names_from = Site,
                                                     values_from = N) %>%
                                         mutate(N = rowSums(across(all_of(SiteNames)), na.rm = TRUE)) %>%
                                         group_by(PatientSubgroup, Strata, MainCancerTopographyGroup) %>%
                                             summarize(N = sum(N)) %>%
                                         group_by(PatientSubgroup, Strata) %>%
                                             reframe(MainCancerTopographyGroup = MainCancerTopographyGroup,
                                                     N = N,
                                                     Proportion = N / sum(N)) %>%
                                         ungroup()

Full.CancerTopography.TotalTime <- expand(Full.CancerTopography.TimeGroups,
                                         PatientSubgroup,
                                         Strata,
                                         MainCancerTopographyGroup) %>%
                                      left_join(Full.CancerTopography.TimeGroups) %>%
                                      group_by(PatientSubgroup, MainCancerTopographyGroup) %>%
                                          summarize(Strata = "Total",
                                                    Proportion = ifelse(any(is.na(N)), NA, sum(N) / sum(N / Proportion)),
                                                    N = sum(N, na.rm = TRUE)) %>%
                                      ungroup()

Full.CancerTopography <- Full.CancerTopography.TimeGroups %>%
                                bind_rows(Full.CancerTopography.TotalTime) %>%
                                mutate(Output = paste0(FormatNumbers(N), " (", ifelse(!is.na(Proportion), FormatNumbers(round(Proportion * 100, digits = 1)), "CALCMAN"), "%)")) %>%
                                select(-N, -Proportion) %>%
                                pivot_wider(names_from = PatientSubgroup,
                                            values_from = Output)



#===============================================================================
# Characteristics of MATCHED CANCER GROUPS (temporally stratified)
#===============================================================================

# Note: Time stratification by Cancer DIAGNOSIS YEAR, not first admission year (like above)
#-------------------------------------------------------------------------------
Matched.SampleSize.TimeGroups <- CumulatedData_Matched$df_Output_AgeAtCancerDiagnosis %>%
                                    filter(MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end) %>%
                                    mutate(Strata = case_when(MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005 - 2008",
                                                              MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009 - 2014",
                                                              MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2022 ~ "2015 - 2022")) %>%
                                    group_by(PatientSubgroup, Strata) %>%
                                        summarize(N = sum(N)) %>%
                                    ungroup()

Matched.SampleSize.TotalTime <- Matched.SampleSize.TimeGroups %>%
                                    group_by(PatientSubgroup) %>%
                                        summarize(Strata = "Total",
                                                  N = sum(N))

Matched.SampleSize <- Matched.SampleSize.TimeGroups %>%
                          bind_rows(Matched.SampleSize.TotalTime) %>%
                          pivot_wider(names_from = PatientSubgroup,
                                      values_from = N) %>%
                          mutate(across(-Strata, ~ FormatNumbers(.x)))

Matched.SampleSize.Summary <- Matched.SampleSize %>%
                                  filter(Strata == "Total")


#-------------------------------------------------------------------------------
Matched.AgeCancer.TimeGroups <- CumulatedData_Matched$df_Output_AgeAtCancerDiagnosis %>%
                                   filter(MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end) %>%
                                   mutate(PatientSubgroup = droplevels(PatientSubgroup),      # This removes empty factor level 'Cancer-/HIV+'. 'PatientSubgroup' still contains the factor level 'Cancer-/HIV+' although there is no data for that group, so the following expand would create combinations with that factor level as well.
                                          Strata = case_when(MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005 - 2008",
                                                             MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009 - 2014",
                                                             MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2022 ~ "2015 - 2022")) %>%
                                   pivot_wider(names_from = Site,
                                               values_from = N) %>%
                                   mutate(N = rowSums(across(all_of(SiteNames)), na.rm = TRUE)) %>%
                                   group_by(PatientSubgroup, Strata, AgeGroup) %>%
                                       summarize(N = sum(N)) %>%
                                   group_by(PatientSubgroup, Strata) %>%
                                       reframe(AgeGroup = AgeGroup,
                                               N = N,
                                               Proportion = N / sum(N)) %>%
                                   ungroup()

Matched.AgeCancer.TotalTime <- expand(Matched.AgeCancer.TimeGroups,
                                      PatientSubgroup,
                                      Strata,
                                      AgeGroup) %>%
                                  left_join(Matched.AgeCancer.TimeGroups) %>%
                                  group_by(PatientSubgroup, AgeGroup) %>%
                                      summarize(Strata = "Total",
                                                Proportion = ifelse(any(is.na(N)), NA, sum(N) / sum(N / Proportion)),
                                                N = sum(N, na.rm = TRUE)) %>%
                                  ungroup()

Matched.AgeCancer <- Matched.AgeCancer.TimeGroups %>%
                          bind_rows(Matched.AgeCancer.TotalTime) %>%
                          mutate(Output = paste0(FormatNumbers(N), " (", ifelse(!is.na(Proportion), FormatNumbers(round(Proportion * 100, digits = 1)), "CALCMAN"), "%)")) %>%
                          select(-N, -Proportion) %>%
                          pivot_wider(names_from = PatientSubgroup,
                                      values_from = Output)


#-------------------------------------------------------------------------------
Matched.CIS.TimeGroups <- CumulatedData_Matched$df_Output_CIS %>%
                              filter(MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end) %>%
                              mutate(PatientSubgroup = droplevels(PatientSubgroup),      # Drop empty level 'Cancer-/HIV+'
                                     Strata = case_when(MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005 - 2008",
                                                        MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009 - 2014",
                                                        MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2022 ~ "2015 - 2022")) %>%
                              pivot_wider(names_from = Site,
                                          values_from = N) %>%
                              mutate(N = rowSums(across(all_of(SiteNames)), na.rm = TRUE)) %>%
                              group_by(PatientSubgroup, Strata, MainCancerIsCarcinomaInSitu) %>%
                                  summarize(N = sum(N)) %>%
                              group_by(PatientSubgroup, Strata) %>%
                                  reframe(MainCancerIsCarcinomaInSitu = MainCancerIsCarcinomaInSitu,
                                          N = N,
                                          Proportion = N / sum(N)) %>%
                              ungroup() %>%
                              filter(MainCancerIsCarcinomaInSitu == TRUE) %>%
                              select(-MainCancerIsCarcinomaInSitu)

Matched.CIS.TotalTime <- expand(Matched.CIS.TimeGroups,
                                PatientSubgroup,
                                Strata) %>%
                            left_join(Matched.CIS.TimeGroups) %>%
                            group_by(PatientSubgroup) %>%
                                summarize(Strata = "Total",
                                          Proportion = ifelse(any(is.na(N)), NA, sum(N) / sum(N / Proportion)),
                                          N = sum(N, na.rm = TRUE)) %>%
                            ungroup()

Matched.CIS <- Matched.CIS.TimeGroups %>%
                    bind_rows(Matched.CIS.TotalTime) %>%
                    mutate(Output = paste0(FormatNumbers(N), " (", ifelse(!is.na(Proportion), FormatNumbers(round(Proportion * 100, digits = 1)), "CALCMAN"), "%)")) %>%
                    select(-N, -Proportion) %>%
                    pivot_wider(names_from = PatientSubgroup,
                                values_from = Output)

Matched.CIS.pValues <- Matched.CIS.TimeGroups %>%
                            bind_rows(Matched.CIS.TotalTime) %>%
                            group_by(Strata) %>%
                            mutate(Count = N,
                                   N = sum(Count)) %>%
                            summarize(ChiSqTest = list(stats::prop.test(x = Count,
                                                                        n = N)),
                                      FisherTest = list(stats::fisher.test(x = matrix(data = c(Count, N - Count),
                                                                                      nrow = 2, ncol = 2, byrow = FALSE)))) %>%
                            ungroup() %>%
                            rowwise() %>%
                                mutate(PValue.ChiSq = FormatPValue(ChiSqTest$p.value),
                                       PValue.Fisher = FormatPValue(FisherTest$p.value)) %>%
                            ungroup() %>%
                            select(-ChiSqTest,
                                   -FisherTest)

Matched.CIS <- Matched.CIS %>%
                    left_join(Matched.CIS.pValues, by = join_by(Strata))


#-------------------------------------------------------------------------------
Matched.Metastasis.TimeGroups <- CumulatedData_Matched$df_Output_MetastasisOccurrence %>%
                                      filter(MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end) %>%
                                      mutate(PatientSubgroup = droplevels(PatientSubgroup),      # Drop empty level 'Cancer-/HIV+',
                                             Strata = case_when(MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005 - 2008",
                                                                MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009 - 2014",
                                                                MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2022 ~ "2015 - 2022")) %>%
                                      select(-CountNoMetastasis,
                                             -starts_with("Proportion")) %>%
                                      pivot_wider(names_from = Site,
                                                  values_from = c(N, starts_with("Count"), starts_with("Proportion"))) %>%
                                      mutate(N = rowSums(across(all_of(paste0("N_", SiteNames))), na.rm = TRUE),
                                             CountMetastasis = rowSums(across(all_of(paste0("CountMetastasis_", SiteNames))), na.rm = TRUE),
                                             CountMetastasisWithCancerDiagnosis = rowSums(across(all_of(paste0("CountMetastasisWithCancerDiagnosis_", SiteNames))), na.rm = TRUE),
                                             CountMetastasisAfterCancerDiagnosis = rowSums(across(all_of(paste0("CountMetastasisAfterCancerDiagnosis_", SiteNames))), na.rm = TRUE)) %>%
                                      select(-(ends_with(SiteNames))) %>%
                                      group_by(PatientSubgroup, Strata) %>%
                                          summarize(across(c(starts_with("N"), starts_with("Count")),
                                                           ~ sum(.x))) %>%
                                          mutate(ProportionMetastasis = CountMetastasis / N,
                                                 ProportionMetastasisWithCancerDiagnosis = CountMetastasisWithCancerDiagnosis / CountMetastasis,
                                                 ProportionMetastasisAfterCancerDiagnosis = CountMetastasisAfterCancerDiagnosis / CountMetastasis) %>%
                                      ungroup()

Matched.Metastasis.TotalTime <- expand(Matched.Metastasis.TimeGroups,
                                       PatientSubgroup,
                                       Strata) %>%
                                    left_join(Matched.Metastasis.TimeGroups) %>%
                                    group_by(PatientSubgroup) %>%
                                        summarize(Strata = "Total",
                                                  ProportionMetastasis = sum(CountMetastasis) / sum(N),
                                                  ProportionMetastasisWithCancerDiagnosis = sum(CountMetastasisWithCancerDiagnosis) / sum(CountMetastasis),
                                                  ProportionMetastasisAfterCancerDiagnosis = sum(CountMetastasisAfterCancerDiagnosis) / sum(CountMetastasis),
                                                  across(starts_with("Count"), ~ sum(.x)),
                                                  N = sum(N)) %>%
                                    ungroup()

Matched.Metastasis <- Matched.Metastasis.TimeGroups %>%
                          bind_rows(Matched.Metastasis.TotalTime) %>%
                          mutate(across(.cols = starts_with("Count"),
                                        .fns  = ~ paste0(FormatNumbers(.x), " (", FormatNumbers(round(get(sub("^Count", "Proportion", cur_column())) * 100, 1)), "%)"),
                                        .names = "Output_{sub('^Count', '', .col)}")) %>%
                          select(-c(starts_with("Count"),
                                    starts_with("Proportion"))) %>%
                          mutate(N = as.character(N)) %>%
                          pivot_longer(cols = c(N, starts_with("Output")),
                                       names_to = "OutputType",
                                       values_to = "Output") %>%
                          mutate(OutputType = str_remove(OutputType, "Output_"),
                                 OutputType = case_match(OutputType, "Metastasis" ~ "Metastasis documented",
                                                                     "MetastasisWithCancerDiagnosis" ~ "Metastasis at time of cancer diagnosis",
                                                                     "MetastasisAfterCancerDiagnosis" ~ "Metastasis after cancer diagnosis",
                                                         .default = OutputType)) %>%
                          pivot_wider(names_from = PatientSubgroup,
                                      values_from = Output)


Matched.Metastasis.pValues <- Matched.Metastasis.TimeGroups %>%
                                  bind_rows(Matched.Metastasis.TotalTime) %>%
                                  select(-c(starts_with("Proportion"))) %>%
                                  pivot_longer(cols = starts_with("Count"),
                                               names_to = "OutputType",
                                               values_to = "Count") %>%
                                  group_by(PatientSubgroup, Strata) %>%
                                      mutate(N = case_when(OutputType != "CountMetastasis" ~ Count[OutputType == "CountMetastasis"],
                                                           .default = N)) %>%
                                  ungroup() %>%
                                  group_by(Strata, OutputType) %>%
                                  summarize(ChiSqTest = list(stats::prop.test(x = Count,
                                                                              n = N)),
                                            FisherTest = list(stats::fisher.test(x = matrix(data = c(Count, N - Count),
                                                                                            nrow = 2, ncol = 2, byrow = FALSE)))) %>%
                                  ungroup() %>%
                                  rowwise() %>%
                                  mutate(PValue.ChiSq = FormatPValue(ChiSqTest$p.value),
                                         PValue.Fisher = FormatPValue(FisherTest$p.value)) %>%
                                  ungroup() %>%
                                  mutate(OutputType = case_match(OutputType, "CountMetastasis" ~ "Metastasis documented",
                                                                             "CountMetastasisWithCancerDiagnosis" ~ "Metastasis at time of cancer diagnosis",
                                                                             "CountMetastasisAfterCancerDiagnosis" ~ "Metastasis after cancer diagnosis",
                                                                 .default = OutputType)) %>%
                                  select(-ChiSqTest,
                                         -FisherTest)

Matched.Metastasis <- Matched.Metastasis %>%
                          left_join(Matched.Metastasis.pValues, by = join_by(Strata, OutputType))


Matched.Metastasis.Summary <- Matched.Metastasis %>%
                                  filter(Strata == "Total")


#-------------------------------------------------------------------------------
Matched.Therapy.TimeGroups <- CumulatedData_Matched$df_Output_TherapyModalities %>%
                                      filter(MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end) %>%
                                      mutate(PatientSubgroup = droplevels(PatientSubgroup),      # Drop level 'Cancer-/HIV+'
                                             Strata = case_when(MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005 - 2008",
                                                                MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009 - 2014",
                                                                MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2022 ~ "2015 - 2022")) %>%
                                      select(-CountNoMajorTherapyCoded,
                                             -starts_with("Proportion")) %>%
                                      pivot_wider(names_from = Site,
                                                  values_from = c(N, starts_with("Count"), starts_with("Proportion"))) %>%
                                      mutate(N = rowSums(across(all_of(paste0("N_", SiteNames))), na.rm = TRUE),
                                             CountAnyMajorTherapy = rowSums(across(all_of(paste0("CountAnyMajorTherapy_", SiteNames))), na.rm = TRUE),
                                             CountSurgery = rowSums(across(all_of(paste0("CountSurgery_", SiteNames))), na.rm = TRUE),
                                             CountChemotherapy = rowSums(across(all_of(paste0("CountChemotherapy_", SiteNames))), na.rm = TRUE),
                                             CountImmunotherapy = rowSums(across(all_of(paste0("CountImmunotherapy_", SiteNames))), na.rm = TRUE),
                                             CountRadiotherapy = rowSums(across(all_of(paste0("CountRadiotherapy_", SiteNames))), na.rm = TRUE),
                                             CountStemCellTherapy = rowSums(across(all_of(paste0("CountStemCellTherapy_", SiteNames))), na.rm = TRUE),
                                             CountBoneMarrowTransplant = rowSums(across(all_of(paste0("CountBoneMarrowTransplant_", SiteNames))), na.rm = TRUE),
                                             CountCARTCellTherapy = rowSums(across(all_of(paste0("CountCARTCellTherapy_", SiteNames))), na.rm = TRUE)) %>%
                                      select(-(ends_with(SiteNames))) %>%
                                      group_by(PatientSubgroup, Strata) %>%
                                          summarize(across(c(starts_with("N"), starts_with("Count")),
                                                           ~ sum(.x))) %>%
                                          mutate(across(.cols = starts_with("Count"),
                                                        .fns = ~ .x / N,
                                                        .names = "{sub('^Count', 'Proportion', .col)}")) %>%
                                      ungroup()

Matched.Therapy.TotalTime <- expand(Matched.Therapy.TimeGroups,
                                    PatientSubgroup,
                                    Strata) %>%
                                left_join(Matched.Therapy.TimeGroups) %>%
                                group_by(PatientSubgroup) %>%
                                    summarize(Strata = "Total",
                                              across(.cols = starts_with("Proportion"),
                                                     .fns = ~ sum(get(sub("^Proportion", "Count", cur_column()))) / sum(N)),
                                              across(starts_with("Count"), ~ sum(.x)),
                                              N = sum(N)) %>%
                                ungroup()


# Matched.Therapy.Temp <- Matched.Therapy.TimeGroups %>%
#                             bind_rows(Matched.Therapy.TotalTime) %>%
#                             select(-starts_with("Proportion")) %>%
#                             pivot_longer(cols = starts_with("Count"),
#                                          names_to = "TherapyGroup",
#                                          values_to = "Count") %>%
#                             mutate(TherapyGroup = str_remove(TherapyGroup, "Count"),
#                                    Proportion = Count / N) %>%
#                             select(-N) %>%
#                             arrange(PatientSubgroup, Strata)

# Matched.Therapy <- Matched.Therapy.Temp %>%
#                         group_by(PatientSubgroup, Strata) %>%
#                             group_modify(~ bind_cols(.x, GetCI(.x$Count))) %>%
#                         ungroup()
#                         right_join(Matched.Therapy.Temp) %>%
#                         mutate(OutputLean = paste0(FormatNumbers(Count), " (", ifelse(!is.na(Proportion), FormatNumbers(round(Proportion * 100, digits = 1)), "CALCMAN"), "%"),
#                                OutputWithCI = paste0(OutputLean, ", ", round(CI.lower * 100, 1), "-", round(CI.upper * 100, 1), ")"),
#                                OutputLean = paste0(OutputLean, ")")) %>%
#                         arrange(desc(PatientSubgroup))
# 
# 
# Matched.Therapy.Table <- Matched.Therapy %>%
#                                           select(-c(N, Proportion, PropCheck, CI.lower, CI.upper, OutputWithCI)) %>%
#                                           pivot_wider(names_from = PatientSubgroup,
#                                                       values_from = OutputLean) %>%
#                                           group_by(Strata) %>%
#                                           arrange(factor(LastRecordedDischargeCategory, levels = c("N",
#                                                                                                    "Home",
#                                                                                                    "Deceased",
#                                                                                                    "Other Hospital",
#                                                                                                    "Rehabilitation or Residential Care",
#                                                                                                    "Hospice Care",
#                                                                                                    "Unclear")),
#                                                   .by_group = TRUE)
# 
# Matched.Therapy.Summary <- Matched.Therapy %>%
#                                           select(-c(N, Proportion, PropCheck, CI.lower, CI.upper, OutputLean)) %>%
#                                           filter(Strata == "Total") %>%
#                                           pivot_wider(names_from = PatientSubgroup,
#                                                       values_from = OutputWithCI) %>%
#                                           group_by(Strata) %>%
#                                           arrange(factor(LastRecordedDischargeCategory, levels = c("N",
#                                                                                                    "Home",
#                                                                                                    "Deceased",
#                                                                                                    "Other Hospital",
#                                                                                                    "Rehabilitation or Residential Care",
#                                                                                                    "Hospice Care",
#                                                                                                    "Unclear")),
#                                                   .by_group = TRUE)



Matched.Therapy <- Matched.Therapy.TimeGroups %>%
                        bind_rows(Matched.Therapy.TotalTime) %>%
                        mutate(across(.cols = starts_with("Count"),
                                      .fns  = ~ paste0(FormatNumbers(.x), " (", FormatNumbers(round(get(sub("^Count", "Proportion", cur_column())) * 100, 1)), "%)"),
                                      .names = "Output_{sub('^Count', '', .col)}")) %>%
                        select(-c(starts_with("Count"),
                                  starts_with("Proportion"))) %>%
                        mutate(N = as.character(N)) %>%
                        pivot_longer(cols = c(N, starts_with("Output")),
                                     names_to = "OutputType",
                                     values_to = "Output") %>%
                        mutate(OutputType = str_remove(OutputType, "Output_"),
                               OutputType = case_match(OutputType, "AnyMajorTherapy" ~ "Any major therapy documented",
                                                                   "StemCellTherapy" ~ "Stem cell therapy",
                                                                   "BoneMarrowTransplant" ~ "Bone marrow transplant",
                                                                   "CARTCellTherapy" ~ "CAR T-cell therapy",
                                                       .default = OutputType)) %>%
                        pivot_wider(names_from = PatientSubgroup,
                                    values_from = Output)


Matched.Therapy.pValues <- Matched.Therapy.TimeGroups %>%
                                bind_rows(Matched.Therapy.TotalTime) %>%
                                select(-c(starts_with("Proportion"))) %>%
                                pivot_longer(cols = starts_with("Count"),
                                             names_to = "OutputType",
                                             values_to = "Count") %>%
                                group_by(Strata, OutputType) %>%
                                    summarize(ChiSqTest = list(stats::prop.test(x = Count,
                                                                                n = N)),
                                              FisherTest = list(stats::fisher.test(x = matrix(data = c(Count, N - Count),
                                                                                              nrow = 2, ncol = 2, byrow = FALSE)))) %>%
                                ungroup() %>%
                                rowwise() %>%
                                    mutate(PValue.ChiSq = FormatPValue(ChiSqTest$p.value),
                                           PValue.Fisher = FormatPValue(FisherTest$p.value)) %>%
                                ungroup() %>%
                                mutate(OutputType = str_remove(OutputType, "Count"),
                                       OutputType = case_match(OutputType, "AnyMajorTherapy" ~ "Any major therapy documented",
                                                                           "StemCellTherapy" ~ "Stem cell therapy",
                                                                           "BoneMarrowTransplant" ~ "Bone marrow transplant",
                                                                           "CARTCellTherapy" ~ "CAR T-cell therapy",
                                                               .default = OutputType)) %>%
                                select(-ChiSqTest,
                                       -FisherTest)

Matched.Therapy <- Matched.Therapy %>%
                          left_join(Matched.Therapy.pValues, by = join_by(Strata, OutputType))

Matched.Therapy.Summary <- Matched.Therapy %>%
                                filter(Strata == "Total")


#-------------------------------------------------------------------------------
Matched.ChemoComplications.TimeGroups <- CumulatedData_Matched$df_Output_TherapyComplications %>%
                                              filter(MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end) %>%
                                              mutate(PatientSubgroup = droplevels(PatientSubgroup),      # Drop empty level 'Cancer-/HIV+',
                                                     Strata = case_when(MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005 - 2008",
                                                                        MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009 - 2014",
                                                                        MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2022 ~ "2015 - 2022")) %>%
                                              select(-CountNoChemotherapy,
                                                     -CountNoComplicationAfterChemotherapy,
                                                     -starts_with("Proportion")) %>%
                                              pivot_wider(names_from = Site,
                                                          values_from = c(N, starts_with("Count"), starts_with("Proportion"))) %>%
                                              mutate(N = rowSums(across(all_of(paste0("N_", SiteNames))), na.rm = TRUE),
                                                     CountChemotherapy = rowSums(across(all_of(paste0("CountChemotherapy_", SiteNames))), na.rm = TRUE),
                                                     CountComplicationAfterChemotherapy = rowSums(across(all_of(paste0("CountComplicationAfterChemotherapy_", SiteNames))), na.rm = TRUE)) %>%
                                                     #CountNoComplicationAfterChemotherapy = rowSums(across(all_of(paste0("CountNoComplicationAfterChemotherapy_", SiteNames))), na.rm = TRUE)) %>%
                                              select(-(ends_with(SiteNames))) %>%
                                              group_by(PatientSubgroup, Strata) %>%
                                                  summarize(across(c(starts_with("N"),
                                                                     starts_with("Count")),
                                                                   ~ sum(.x))) %>%
                                                  mutate(ProportionChemotherapy = CountChemotherapy / N,
                                                         ProportionComplicationAfterChemotherapy = CountComplicationAfterChemotherapy / CountChemotherapy) %>%
                                              ungroup()

Matched.ChemoComplications.TotalTime <- expand(Matched.ChemoComplications.TimeGroups,
                                               PatientSubgroup,
                                               Strata) %>%
                                            left_join(Matched.ChemoComplications.TimeGroups) %>%
                                            group_by(PatientSubgroup) %>%
                                                summarize(Strata = "Total",
                                                          ProportionChemotherapy = sum(CountChemotherapy) / sum(N),
                                                          ProportionComplicationAfterChemotherapy = sum(CountComplicationAfterChemotherapy) / sum(CountChemotherapy),
                                                          across(starts_with("Count"), ~ sum(.x)),
                                                          N = sum(N)) %>%
                                            ungroup()

Matched.ChemoComplications <- Matched.ChemoComplications.TimeGroups %>%
                                  bind_rows(Matched.ChemoComplications.TotalTime) %>%
                                  mutate(across(.cols = starts_with("Count"),
                                                .fns  = ~ paste0(FormatNumbers(.x), " (", FormatNumbers(round(get(sub("^Count", "Proportion", cur_column())) * 100, 1)), "%)"),
                                                .names = "Output_{sub('^Count', '', .col)}")) %>%
                                  select(-c(starts_with("Count"),
                                            starts_with("Proportion"))) %>%
                                  mutate(N = as.character(N)) %>%
                                  pivot_longer(cols = c(N, starts_with("Output")),
                                               names_to = "OutputType",
                                               values_to = "Output") %>%
                                  mutate(OutputType = str_remove(OutputType, "Output_"),
                                         OutputType = case_match(OutputType, "Chemotherapy" ~ "Chemotherapy documented",
                                                                             "ComplicationAfterChemotherapy" ~ "Complication after chemotherapy",
                                                                 .default = OutputType)) %>%
                                  pivot_wider(names_from = PatientSubgroup,
                                              values_from = Output)

Matched.ChemoComplications.pValues <- Matched.ChemoComplications.TimeGroups %>%
                                          bind_rows(Matched.ChemoComplications.TotalTime) %>%
                                          select(-c(starts_with("Proportion"))) %>%
                                          pivot_longer(cols = starts_with("Count"),
                                                       names_to = "OutputType",
                                                       values_to = "Count") %>%
                                          group_by(PatientSubgroup, Strata) %>%
                                              mutate(N = case_when(OutputType != "CountChemotherapy" ~ Count[OutputType == "CountChemotherapy"],
                                                                   .default = N)) %>%
                                          ungroup() %>%
                                          group_by(Strata, OutputType) %>%
                                              summarize(ChiSqTest = list(stats::prop.test(x = Count,
                                                                                          n = N)),
                                                        FisherTest = list(stats::fisher.test(x = matrix(data = c(Count, N - Count),
                                                                                                        nrow = 2, ncol = 2, byrow = FALSE)))) %>%
                                          ungroup() %>%
                                          rowwise() %>%
                                              mutate(PValue.ChiSq = FormatPValue(ChiSqTest$p.value),
                                                     PValue.Fisher = FormatPValue(FisherTest$p.value)) %>%
                                          ungroup() %>%
                                          mutate(OutputType = str_remove(OutputType, "Count"),
                                                 OutputType = case_match(OutputType, "Chemotherapy" ~ "Chemotherapy documented",
                                                                                     "ComplicationAfterChemotherapy" ~ "Complication after chemotherapy",
                                                                         .default = OutputType)) %>%
                                          select(-ChiSqTest,
                                                 -FisherTest)

Matched.ChemoComplications <- Matched.ChemoComplications %>%
                                  left_join(Matched.ChemoComplications.pValues, by = join_by(Strata, OutputType))

Matched.ChemoComplications.Summary <- Matched.ChemoComplications %>%
                                          filter(Strata == "Total")



#===============================================================================
# Matched analysis with reduced number of sites (Frankfurt, Freiburg)
#===============================================================================

# Calculate modified sample sizes for
# a) Reduced selection of sites and stratified by 'FirstRelevantAdmissionYear'
# b) Reduced selection of sites and stratified by 'LastRecordedDischargeYear' instead of 'MainCancerDiagnosisYear'
#-------------------------------------------------------------------------------
#SiteNames.Temp <- SiteNames
SiteNames.Temp <- c("Frankfurt", "Freiburg")


# a) Sample size for reduced selection of sites and stratified by 'FirstRelevantAdmissionYear'
Matched.ModSampleSize.FirstAdmissionYear.TimeGroups <- CumulatedData_Matched$df_Output_MeanLengthOfStay %>%
                                                            filter(Site %in% SiteNames.Temp,
                                                                   FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end) %>%
                                                            mutate(Strata = case_when(FirstRelevantAdmissionYear >= 2005 & FirstRelevantAdmissionYear <= 2008 ~ "2005 - 2008",
                                                                                      FirstRelevantAdmissionYear >= 2009 & FirstRelevantAdmissionYear <= 2014 ~ "2009 - 2014",
                                                                                      FirstRelevantAdmissionYear >= 2015 & FirstRelevantAdmissionYear <= 2022 ~ "2015 - 2022")) %>%
                                                            group_by(PatientSubgroup, Strata) %>%
                                                                summarize(N = sum(N)) %>%
                                                            ungroup()

Matched.ModSampleSize.FirstAdmissionYear.TotalTime <- Matched.ModSampleSize.FirstAdmissionYear.TimeGroups %>%
                                                          group_by(PatientSubgroup) %>%
                                                              summarize(Strata = "Total",
                                                                        N = sum(N))

Matched.ModSampleSize.FirstAdmissionYear <- Matched.ModSampleSize.FirstAdmissionYear.TimeGroups %>%
                                                bind_rows(Matched.ModSampleSize.FirstAdmissionYear.TotalTime) %>%
                                                pivot_wider(names_from = PatientSubgroup,
                                                            values_from = N) %>%
                                                mutate(across(-Strata, ~ FormatNumbers(.x)))

#-------------------------------------------------------------------------------

# b) Sample size for reduced selection of sites and stratified by 'LastRecordedDischargeYear' instead of 'MainCancerDiagnosisYear'
Matched.ModSampleSize.DischargeYear.TimeGroups <- CumulatedData_Matched$df_Output_LastRecordedDischargeCategory %>%
                                                      select(-MainCancerDiagnosisYear,
                                                                  -Proportion) %>%
                                                      filter(LastRecordedDischargeYear >= tp_begin & LastRecordedDischargeYear <= tp_end,
                                                             Site %in% SiteNames.Temp) %>%
                                                      mutate(PatientSubgroup = droplevels(PatientSubgroup),
                                                             Strata = case_when(LastRecordedDischargeYear >= 2005 & LastRecordedDischargeYear <= 2008 ~ "2005 - 2008",
                                                                                LastRecordedDischargeYear >= 2009 & LastRecordedDischargeYear <= 2014 ~ "2009 - 2014",
                                                                                LastRecordedDischargeYear >= 2015 & LastRecordedDischargeYear <= 2022 ~ "2015 - 2022")) %>%
                                                      group_by(PatientSubgroup, Strata) %>%
                                                          summarize(N = sum(N)) %>%
                                                      ungroup()

Matched.ModSampleSize.DischargeYear.TotalTime <- Matched.ModSampleSize.DischargeYear.TimeGroups %>%
                                                    group_by(PatientSubgroup) %>%
                                                        summarize(Strata = "Total",
                                                                  N = sum(N))

Matched.ModSampleSize.DischargeYear <- Matched.ModSampleSize.DischargeYear.TimeGroups %>%
                                            bind_rows(Matched.ModSampleSize.DischargeYear.TotalTime) %>%
                                            pivot_wider(names_from = PatientSubgroup,
                                                        values_from = N) %>%
                                            mutate(across(-Strata, ~ FormatNumbers(.x)))


#-------------------------------------------------------------------------------

AddModSampleSize.MeanLengthOfStay <- Matched.ModSampleSize.FirstAdmissionYear %>%
                                          pivot_longer(cols = -Strata,
                                                       names_to = "PatientSubgroup",
                                                       values_to = "SampleSize") %>%
                                          mutate(SampleSize = as.integer(SampleSize))

Matched.MeanLengthOfStay.TimeGroups <- CumulatedData_Matched$df_Output_MeanLengthOfStay %>%
                                            filter(Site %in% SiteNames.Temp,
                                                   FirstRelevantAdmissionYear >= tp_begin & FirstRelevantAdmissionYear <= tp_end) %>%
                                            mutate(PatientSubgroup = droplevels(PatientSubgroup),
                                                   Strata = case_when(FirstRelevantAdmissionYear >= 2005 & FirstRelevantAdmissionYear <= 2008 ~ "2005 - 2008",
                                                                      FirstRelevantAdmissionYear >= 2009 & FirstRelevantAdmissionYear <= 2014 ~ "2009 - 2014",
                                                                      FirstRelevantAdmissionYear >= 2015 & FirstRelevantAdmissionYear <= 2022 ~ "2015 - 2022")) %>%
                                            pivot_wider(names_from = Site,
                                                       values_from = N) %>%
                                            mutate(N = rowSums(across(all_of(SiteNames.Temp)), na.rm = TRUE)) %>%
                                            group_by(PatientSubgroup, Strata, MeanLengthOfStayGroup) %>%
                                                summarize(N = sum(N)) %>%
                                            ungroup() %>%
                                            left_join(AddModSampleSize.MeanLengthOfStay) %>%
                                            mutate(Proportion = N / SampleSize) %>%
                                            select(-SampleSize)

Matched.MeanLengthOfStay.TotalTime <- expand(Matched.MeanLengthOfStay.TimeGroups,
                                             PatientSubgroup,
                                             Strata,
                                             MeanLengthOfStayGroup) %>%
                                         left_join(Matched.MeanLengthOfStay.TimeGroups) %>%
                                         group_by(PatientSubgroup, MeanLengthOfStayGroup) %>%
                                             summarize(Strata = "Total",
                                                       Proportion = ifelse(any(is.na(N)), NA, sum(N) / sum(N / Proportion)),
                                                       N = sum(N, na.rm = TRUE)) %>%
                                         ungroup()

AddSampleSizeRow.MeanLengthOfStay <- AddModSampleSize.MeanLengthOfStay %>%
                                          mutate(MeanLengthOfStayGroup = "N",
                                                 N = SampleSize) %>%
                                          select(-SampleSize)

Matched.MeanLengthOfStay <- Matched.MeanLengthOfStay.TimeGroups %>%
                                bind_rows(Matched.MeanLengthOfStay.TotalTime) %>%
                                bind_rows(AddSampleSizeRow.MeanLengthOfStay) %>%
                                mutate(Output = paste0(FormatNumbers(N), " (", ifelse(!is.na(Proportion), FormatNumbers(round(Proportion * 100, digits = 1)), "CALCMAN"), "%)")) %>%
                                select(-N, -Proportion) %>%
                                pivot_wider(names_from = PatientSubgroup,
                                            values_from = Output)


Matched.MeanLengthOfStay.pValues <- Matched.MeanLengthOfStay.TimeGroups %>%
                                        bind_rows(Matched.MeanLengthOfStay.TotalTime) %>%
                                        select(-Proportion) %>%
                                        group_by(PatientSubgroup, Strata) %>%
                                            mutate(Count = N,
                                                   N = sum(Count)) %>%
                                        ungroup() %>%
                                        group_by(Strata, MeanLengthOfStayGroup) %>%
                                            summarize(ChiSqTest = list(stats::prop.test(x = Count,
                                                                                        n = N)),
                                                      FisherTest = list(stats::fisher.test(x = matrix(data = c(Count, N - Count),
                                                                                                      nrow = 2, ncol = 2, byrow = FALSE)))) %>%
                                        ungroup() %>%
                                        rowwise() %>%
                                            mutate(PValue.ChiSq = FormatPValue(ChiSqTest$p.value),
                                                   PValue.Fisher = FormatPValue(FisherTest$p.value)) %>%
                                        ungroup() %>%
                                        select(-ChiSqTest,
                                               -FisherTest)

Matched.MeanLengthOfStay <- Matched.MeanLengthOfStay %>%
                                left_join(Matched.MeanLengthOfStay.pValues, by = join_by(Strata, MeanLengthOfStayGroup)) %>%
                                mutate(MeanLengthOfStayGroup = str_replace(MeanLengthOfStayGroup, "average", "avg."),
                                       MeanLengthOfStayGroup = str_replace(MeanLengthOfStayGroup, "More than", ">")) %>%
                                group_by(Strata) %>%
                                    arrange(factor(MeanLengthOfStayGroup, levels = c("N",
                                                                                     "Up to 7 days on avg.",
                                                                                     "7 - 14 days on avg.",
                                                                                     "14 - 30 days on avg.",
                                                                                     "> 30 days on avg.")),
                                            .by_group = TRUE) %>%
                                ungroup()

Matched.MeanLengthOfStay.Summary <- Matched.MeanLengthOfStay %>%
                                        filter(Strata == "Total")



# OLD - Solution with Confidence Intervals
#-------------------------------------------------------------------------------
                                            
# Matched.MeanLengthOfStay.Temp <- Matched.MeanLengthOfStay.TimeGroups %>%
#                                       bind_rows(Matched.MeanLengthOfStay.TotalTime)
# 
# Matched.MeanLengthOfStay <- Matched.MeanLengthOfStay.Temp %>%
#                                 group_by(PatientSubgroup, Strata) %>%
#                                     group_modify(~ bind_cols(.x, GetCI(.x$N))) %>%
#                                 ungroup() %>%
#                                 right_join(Matched.MeanLengthOfStay.Temp) %>%
#                                 bind_rows(AddSampleSizeRow.MeanLengthOfStay) %>%
#                                 mutate(OutputLean = paste0(FormatNumbers(N), " (", ifelse(!is.na(Proportion), FormatNumbers(round(Proportion * 100, digits = 1)), "CALCMAN"), "%"),
#                                        OutputWithCI = paste0(OutputLean, ", ", round(CI.lower * 100, 1), "-", round(CI.upper * 100, 1), ")"),
#                                        OutputLean = paste0(OutputLean, ")")) %>%
#                                 arrange(desc(PatientSubgroup))
# 
# 
# Matched.MeanLengthOfStay.Table <- Matched.MeanLengthOfStay %>%
#                                       select(-c(N, Proportion, PropCheck, CI.lower, CI.upper, OutputWithCI)) %>%
#                                       pivot_wider(names_from = PatientSubgroup,
#                                                   values_from = OutputLean) %>%
#                                       mutate(MeanLengthOfStayGroup = str_replace(MeanLengthOfStayGroup, "average", "avg."),
#                                              MeanLengthOfStayGroup = str_replace(MeanLengthOfStayGroup, "More than", ">")) %>%
#                                       group_by(Strata) %>%
#                                       arrange(factor(MeanLengthOfStayGroup, levels = c("N",
#                                                                                        "Up to 7 days on avg.",
#                                                                                        "7 - 14 days on avg.",
#                                                                                        "14 - 30 days on avg.",
#                                                                                        "> 30 days on avg.")),
#                                               .by_group = TRUE)
# 
# 
# Matched.MeanLengthOfStay.Summary <- Matched.MeanLengthOfStay %>%
#                                         select(-c(N, Proportion, PropCheck, CI.lower, CI.upper, OutputLean)) %>%
#                                         filter(Strata == "Total") %>%
#                                         pivot_wider(names_from = PatientSubgroup,
#                                                     values_from = OutputWithCI) %>%
#                                         mutate(MeanLengthOfStayGroup = str_replace(MeanLengthOfStayGroup, "average", "avg."),
#                                                MeanLengthOfStayGroup = str_replace(MeanLengthOfStayGroup, "More than", ">")) %>%
#                                         group_by(Strata) %>%
#                                         arrange(factor(MeanLengthOfStayGroup, levels = c("N",
#                                                                                          "Up to 7 days on avg.",
#                                                                                          "7 - 14 days on avg.",
#                                                                                          "14 - 30 days on avg.",
#                                                                                          "> 30 days on avg.")),
#                                                 .by_group = TRUE)


#-------------------------------------------------------------------------------

AddModSampleSize.DischargeCategories <- Matched.ModSampleSize.DischargeYear %>%
                                            pivot_longer(cols = -Strata,
                                                         names_to = "PatientSubgroup",
                                                         values_to = "SampleSize") %>%
                                            mutate(SampleSize = as.integer(SampleSize))

Matched.DischargeCategories.TimeGroups <- CumulatedData_Matched$df_Output_LastRecordedDischargeCategory %>%
                                              select(-MainCancerDiagnosisYear,
                                                     -Proportion) %>%
                                              filter(LastRecordedDischargeYear >= tp_begin & LastRecordedDischargeYear <= tp_end,
                                                     Site %in% SiteNames.Temp) %>%
                                              mutate(PatientSubgroup = droplevels(PatientSubgroup),
                                                     Strata = case_when(LastRecordedDischargeYear >= 2005 & LastRecordedDischargeYear <= 2008 ~ "2005 - 2008",
                                                                        LastRecordedDischargeYear >= 2009 & LastRecordedDischargeYear <= 2014 ~ "2009 - 2014",
                                                                        LastRecordedDischargeYear >= 2015 & LastRecordedDischargeYear <= 2022 ~ "2015 - 2022")) %>%
                                              pivot_wider(names_from = Site,
                                                          values_from = N) %>%
                                              mutate(N = rowSums(across(all_of(SiteNames.Temp)), na.rm = TRUE)) %>%
                                              group_by(PatientSubgroup, Strata, LastRecordedDischargeCategory) %>%
                                                  summarize(N = sum(N)) %>%
                                              ungroup() %>%
                                              left_join(AddModSampleSize.DischargeCategories) %>%
                                              mutate(Proportion = N / SampleSize) %>%
                                              select(-SampleSize)
                                            

# AddUnknownCategory <- Matched.DischargeCategories.TimeGroups %>%
#                           group_by(PatientSubgroup, Strata) %>%
#                               summarize(LastRecordedDischargeCategory = "AllKnown",
#                                         N = sum(N)) %>%
#                           ungroup() %>%
#                           left_join(AddModSampleSize) %>%
#                           mutate(LastRecordedDischargeCategory = "Unclear",
#                                  N = SampleSize - N) %>%
#                           select(-SampleSize)

Matched.DischargeCategories.TotalTime <- expand(Matched.DischargeCategories.TimeGroups,
                                                PatientSubgroup,
                                                Strata,
                                                LastRecordedDischargeCategory) %>%
                                            left_join(Matched.DischargeCategories.TimeGroups) %>%
                                            group_by(PatientSubgroup, LastRecordedDischargeCategory) %>%
                                                summarize(Strata = "Total",
                                                          Proportion = ifelse(any(is.na(N)), NA, sum(N) / sum(N / Proportion)),
                                                          N = sum(N, na.rm = TRUE)) %>%
                                            ungroup()

AddSampleSizeRow.DischargeCategories <- AddModSampleSize.DischargeCategories %>%
                                            mutate(LastRecordedDischargeCategory = "N",
                                                   N = SampleSize) %>%
                                            select(-SampleSize)


Matched.DischargeCategories <- Matched.DischargeCategories.TimeGroups %>%
                                    bind_rows(Matched.DischargeCategories.TotalTime) %>%
                                    bind_rows(AddSampleSizeRow.DischargeCategories) %>%
                                    mutate(Output = paste0(FormatNumbers(N), " (", ifelse(!is.na(Proportion), FormatNumbers(round(Proportion * 100, digits = 1)), "CALCMAN"), "%)")) %>%
                                    select(-N, -Proportion) %>%
                                    pivot_wider(names_from = PatientSubgroup,
                                                values_from = Output)


Matched.DischargeCategories.pValues <- Matched.DischargeCategories.TimeGroups %>%
                                            bind_rows(Matched.DischargeCategories.TotalTime) %>%
                                            select(-Proportion) %>%
                                            group_by(PatientSubgroup, Strata) %>%
                                                mutate(Count = N,
                                                       N = sum(Count)) %>%
                                            ungroup() %>%
                                            group_by(Strata, LastRecordedDischargeCategory) %>%
                                                summarize(ChiSqTest = list(stats::prop.test(x = Count,
                                                                                            n = N)),
                                                          FisherTest = list(stats::fisher.test(x = matrix(data = c(Count, N - Count),
                                                                                                          nrow = 2, ncol = 2, byrow = FALSE)))) %>%
                                            ungroup() %>%
                                            rowwise() %>%
                                                mutate(PValue.ChiSq = FormatPValue(ChiSqTest$p.value),
                                                       PValue.Fisher = FormatPValue(FisherTest$p.value)) %>%
                                            ungroup() %>%
                                            select(-ChiSqTest,
                                                   -FisherTest)

Matched.DischargeCategories <- Matched.DischargeCategories %>%
                                    left_join(Matched.DischargeCategories.pValues, by = join_by(Strata, LastRecordedDischargeCategory)) %>%
                                    group_by(Strata) %>%
                                        arrange(factor(LastRecordedDischargeCategory, levels = c("N",
                                                                                                 "Home",
                                                                                                 "Deceased",
                                                                                                 "Other Hospital",
                                                                                                 "Rehabilitation or Residential Care",
                                                                                                 "Hospice Care",
                                                                                                 "Unclear")),
                                                .by_group = TRUE) %>%
                                    ungroup()

Matched.DischargeCategories.Summary <- Matched.DischargeCategories %>%
                                            filter(Strata == "Total")












# OLD - Solution with Confidence Intervals
#-------------------------------------------------------------------------------

# Matched.DischargeCategories.Temp <- Matched.DischargeCategories.TimeGroups %>%
#                                         bind_rows(Matched.DischargeCategories.TotalTime)
# 
# Matched.DischargeCategories <- Matched.DischargeCategories.Temp %>%
#                                     group_by(PatientSubgroup, Strata) %>%
#                                         group_modify(~ bind_cols(.x, GetCI(.x$N))) %>%
#                                     ungroup() %>%
#                                     right_join(Matched.DischargeCategories.Temp) %>%
#                                     bind_rows(AddSampleSizeRow.DischargeCategories) %>%
#                                     mutate(OutputLean = paste0(FormatNumbers(N), " (", ifelse(!is.na(Proportion), FormatNumbers(round(Proportion * 100, digits = 1)), "CALCMAN"), "%"),
#                                            OutputWithCI = paste0(OutputLean, ", ", round(CI.lower * 100, 1), "-", round(CI.upper * 100, 1), ")"),
#                                            OutputLean = paste0(OutputLean, ")")) %>%
#                                     arrange(desc(PatientSubgroup))
# 
# 
# Matched.DischargeCategories.Table <- Matched.DischargeCategories %>%
#                                           select(-c(N, Proportion, PropCheck, CI.lower, CI.upper, OutputWithCI)) %>%
#                                           pivot_wider(names_from = PatientSubgroup,
#                                                       values_from = OutputLean) %>%
#                                           group_by(Strata) %>%
#                                           arrange(factor(LastRecordedDischargeCategory, levels = c("N",
#                                                                                                    "Home",
#                                                                                                    "Deceased",
#                                                                                                    "Other Hospital",
#                                                                                                    "Rehabilitation or Residential Care",
#                                                                                                    "Hospice Care",
#                                                                                                    "Unclear")),
#                                                   .by_group = TRUE)
# 
# Matched.DischargeCategories.Summary <- Matched.DischargeCategories %>%
#                                           select(-c(N, Proportion, PropCheck, CI.lower, CI.upper, OutputLean)) %>%
#                                           filter(Strata == "Total") %>%
#                                           pivot_wider(names_from = PatientSubgroup,
#                                                       values_from = OutputWithCI) %>%
#                                           group_by(Strata) %>%
#                                           arrange(factor(LastRecordedDischargeCategory, levels = c("N",
#                                                                                                    "Home",
#                                                                                                    "Deceased",
#                                                                                                    "Other Hospital",
#                                                                                                    "Rehabilitation or Residential Care",
#                                                                                                    "Hospice Care",
#                                                                                                    "Unclear")),
#                                                   .by_group = TRUE)
                        
#-------------------------------------------------------------------------------

#SiteNames.Temp <- SiteNames
SiteNames.Temp <- c("Frankfurt", "Freiburg")

#Matched.ICU.TimeGroups




#===============================================================================
  
HIVCancer.Categories.TimeGroups <- CumulatedData_Full$df_Output_HIVCancerCategories %>%
                                       filter(MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end) %>%
                                       mutate(PatientSubgroup = droplevels(PatientSubgroup),
                                              Strata = case_when(MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005 - 2008",
                                                                 MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009 - 2014",
                                                                 MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2022 ~ "2015 - 2022")) %>%
                                       pivot_wider(names_from = Site,
                                                   values_from = N) %>%
                                       mutate(N = rowSums(across(all_of(SiteNames)), na.rm = TRUE)) %>%
                                       group_by(PatientSubgroup, Strata, PatientSubgroupHIVCancerCategory) %>%
                                           summarize(N = sum(N)) %>%
                                       group_by(PatientSubgroup, Strata) %>%
                                           reframe(PatientSubgroupHIVCancerCategory = PatientSubgroupHIVCancerCategory,
                                                   N = N,
                                                   Proportion = N / sum(N)) %>%
                                       ungroup()

HIVCancer.Categories.TotalTime <- expand(HIVCancer.Categories.TimeGroups,
                                         PatientSubgroup,
                                         Strata,
                                         PatientSubgroupHIVCancerCategory) %>%
                                    left_join(HIVCancer.Categories.TimeGroups) %>%
                                    group_by(PatientSubgroup, PatientSubgroupHIVCancerCategory) %>%
                                        summarize(Strata = "Total",
                                                  Proportion = ifelse(any(is.na(N)), NA, sum(N) / sum(N / Proportion)),
                                                  N = sum(N, na.rm = TRUE)) %>%
                                    ungroup()

HIVCancer.Categories <- HIVCancer.Categories.TimeGroups %>%
                                bind_rows(HIVCancer.Categories.TotalTime) %>%
                                mutate(Output = paste0(FormatNumbers(N), " (", ifelse(!is.na(Proportion), FormatNumbers(round(Proportion * 100, digits = 1)), "CALCMAN"), "%)")) %>%
                                select(-N, -Proportion) %>%
                                pivot_wider(names_from = PatientSubgroup,
                                            values_from = Output)

#-------------------------------------------------------------------------------

HIVCancer.DiagnosisOrder.TimeGroups <- CumulatedData_Matched$df_Output_HIVCancerDiagnosisOrder %>%
                                            filter(MainCancerDiagnosisYear >= tp_begin & MainCancerDiagnosisYear <= tp_end) %>%
                                            mutate(Strata = case_when(MainCancerDiagnosisYear >= 2005 & MainCancerDiagnosisYear <= 2008 ~ "2005 - 2008",
                                                                     MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2014 ~ "2009 - 2014",
                                                                     MainCancerDiagnosisYear >= 2015 & MainCancerDiagnosisYear <= 2022 ~ "2015 - 2022")) %>%
                                            pivot_wider(names_from = Site,
                                                       values_from = N) %>%
                                            mutate(N = rowSums(across(all_of(SiteNames)), na.rm = TRUE)) %>%
                                            group_by(Strata, HIVCancerDiagnosisOrder) %>%
                                               summarize(N = sum(N)) %>%
                                            group_by(Strata) %>%
                                               reframe(HIVCancerDiagnosisOrder = HIVCancerDiagnosisOrder,
                                                       N = N,
                                                       Proportion = N / sum(N)) %>%
                                            ungroup()

HIVCancer.DiagnosisOrder.TotalTime <- expand(HIVCancer.DiagnosisOrder.TimeGroups,
                                             Strata,
                                             HIVCancerDiagnosisOrder) %>%
                                          left_join(HIVCancer.DiagnosisOrder.TimeGroups) %>%
                                          group_by(HIVCancerDiagnosisOrder) %>%
                                              summarize(Strata = "Total",
                                                        Proportion = ifelse(any(is.na(N)), NA, sum(N) / sum(N / Proportion)),
                                                        N = sum(N, na.rm = TRUE)) %>%
                                          ungroup()

HIVCancer.DiagnosisOrder <- HIVCancer.DiagnosisOrder.TimeGroups %>%
                                bind_rows(HIVCancer.DiagnosisOrder.TotalTime) %>%
                                mutate(Output = paste0(FormatNumbers(N), " (", ifelse(!is.na(Proportion), FormatNumbers(round(Proportion * 100, digits = 1)), "CALCMAN"), "%)")) %>%
                                select(-N, -Proportion)


#===============================================================================

TableOutputPath <- "C:/Users/Basti/OneDrive/ARBEIT/IDMKD/Projekte/HIVCAre/Publikation/Paper/JMIR Public Health and Surveillance/Review/Tables"


TableList <- list(Full.SampleSize = Full.SampleSize,
                  Full.FemaleSex = Full.FemaleSex,
                  Full.Age = Full.Age,
                  Full.Admissions = Full.Admissions,
                  Full.MeanLengthOfStay = Full.MeanLengthOfStay,
                  Full.DistinctCancers = Full.DistinctCancers,
                  Full.CIS = Full.CIS,
                  Full.CancerTopography = Full.CancerTopography,
                  Matched.SampleSize = Matched.SampleSize,
                  Matched.AgeCancer = Matched.AgeCancer,
                  Matched.CIS = Matched.CIS,
                  Matched.Metastasis = Matched.Metastasis,
                  Matched.Therapy = Matched.Therapy,
                  Matched.ChemoComplications = Matched.ChemoComplications,
                  #--- Matched Analysis with reduced sites ---
                  Matched.MeanLengthOfStay = Matched.MeanLengthOfStay,
                  Matched.DischargeCategories = Matched.DischargeCategories,
                  #--- Matched Analysis Summary tables ---
                  Matched.SampleSize.Summary = Matched.SampleSize.Summary,
                  Matched.Metastasis.Summary = Matched.Metastasis.Summary,
                  Matched.Therapy.Summary = Matched.Therapy.Summary,
                  Matched.ChemoComplications.Summary = Matched.ChemoComplications.Summary,
                  Matched.MeanLengthOfStay.Summary = Matched.MeanLengthOfStay.Summary,
                  Matched.DischargeCategories.Summary = Matched.DischargeCategories.Summary,
                  #--- Only HIVCancer ---
                  HIVCancer.Categories = HIVCancer.Categories,
                  HIVCancer.DiagnosisOrder = HIVCancer.DiagnosisOrder)

TableList <- TableList %>%
                purrr::iwalk(function(Table, tablename)
                             {
                                  write.csv2(Table, file = paste0(TableOutputPath, "/", tablename, ".csv"))
                })


#===============================================================================

Plot_SampleSize <- CumulatedData_Full$df_Output_SampleSize %>%
                        filter(FirstRelevantAdmissionYear < 2023) %>%
                        dsFredaClient::MakeColumnPlot(XFeature = FirstRelevantAdmissionYear,
                                                      YFeature = N,
                                                      GroupingFeature = Site,
                                                      GroupingPosition = position_stack(),
                                                      GroupingMapping = "alpha",
                                                      AlphaPalette = c(0.2, 0.5, 0.9),
                                                      GroupingSpecs = c("Site A" = "MunichLMU",
                                                                        "Site B" = "Freiburg",
                                                                        "Site C" = "Frankfurt"),
                                                      FacetFeature = PatientSubgroup,
                                                      FacetMapping = "fill",
                                                      FillPalette = vc_FillPalette_Subgroup,
                                                      FacetArguments = list(scales = "free_y"))
              
f_ExportPlot(inp_Plot = Plot_SampleSize,
             inp_Directory = PlotOutputPath,
             inp_Width = 30,
             inp_Height = 10)


#-------------------------------------------------------------------------------

# Age at cancer diagnosis over time for PNLWH; using unmatched data (because age was a matching variable), excluding Freiburg
plot_AgeOverTime_PNLWH <- CumulatedData_Full$df_Output_AgeAtCancerDiagnosis %>%
                              filter(PatientSubgroup == "Cancer+/HIV-") %>%
                              pivot_wider(names_from = Site,
                                          values_from = N) %>%
                              mutate(All = rowSums(across(all_of(SiteNames)), na.rm = TRUE)) %>%
                              filter(between(MainCancerDiagnosisYear, 2005, 2022)) %>%
                              dsFredaClient::MakeColumnPlot(XFeature = MainCancerDiagnosisYear,
                                                            YFeature = All,
                                                            GroupingFeature = AgeGroup,
                                                            GroupingPosition = position_fill(),
                                                            AxisType_y = "proportional",
                                                            GroupingSpecs = c("> 80 years old",
                                                                              "60 - 79 years old",
                                                                              "40 - 59 years old",
                                                                              "18 - 39 years old"),
                                                            GroupingMapping = "alpha",
                                                            AlphaPalette = c(0.3, 0.5, 0.7, 0.9),
                                                            FacetFeature = PatientSubgroup,
                                                            FacetMapping = "fill",
                                                            FillPalette = vc_FillPalette_Subgroup)


# Age at cancer diagnosis over time for PLWH
plot_AgeOverTime_PLWH <- CumulatedData_Full$df_Output_AgeAtCancerDiagnosis %>%
                              filter(PatientSubgroup == "Cancer+/HIV+") %>%
                              pivot_wider(names_from = Site,
                                          values_from = N) %>%
                              mutate(All = rowSums(across(all_of(SiteNames)), na.rm = TRUE)) %>%
                              filter(between(MainCancerDiagnosisYear, 2005, 2022)) %>%
                              dsFredaClient::MakeColumnPlot(XFeature = MainCancerDiagnosisYear,
                                                            YFeature = All,
                                                            GroupingFeature = AgeGroup,
                                                            GroupingPosition = position_fill(),
                                                            AxisType_y = "proportional",
                                                            GroupingSpecs = c("> 80 years old",
                                                                              "60 - 79 years old",
                                                                              "40 - 59 years old",
                                                                              "18 - 39 years old"),
                                                            GroupingMapping = "alpha",
                                                            AlphaPalette = c(0.3, 0.5, 0.7, 0.9),
                                                            FacetFeature = PatientSubgroup,
                                                            FacetMapping = "fill",
                                                            FillPalette = vc_FillPalette_Subgroup)

f_ExportPlot(inp_Plot = plot_AgeOverTime_PNLWH,
             inp_Directory = PlotOutputPath,
             inp_Width = 30,
             inp_Height = 10)

f_ExportPlot(inp_Plot = plot_AgeOverTime_PLWH,
             inp_Directory = PlotOutputPath,
             inp_Width = 30,
             inp_Height = 10)


#-------------------------------------------------------------------------------

Plot_CancerTopography <- CumulatedData_Full$df_Output_MainCancerTopographyGroup_OverTime %>%
                              filter(PatientSubgroup == "Cancer+/HIV+") %>%
                              pivot_wider(names_from = Site,
                                          values_from = c(N, Proportion)) %>%
                              group_by(MainCancerDiagnosisYear, PatientSubgroup) %>%
                                  mutate(N_All = rowSums(across(all_of(paste0("N_", SiteNames))), na.rm = TRUE),
                                         Proportion_All = N_All / sum(N_All)) %>%
                              ungroup() %>%
                              filter(between(MainCancerDiagnosisYear, 2005, 2022)) %>%
                              dsFredaClient::MakeColumnPlot(XFeature = MainCancerDiagnosisYear,
                                                            YFeature = Proportion_All,
                                                            AxisType_y = "proportional",
                                                            GroupingFeature = MainCancerTopographyGroup,
                                                            GroupingPosition = position_fill(),
                                                            GroupingMapping = "fill")
  

#-------------------------------------------------------------------------------

SiteNames.Temp <- SiteNames
#SiteNames.Temp <- "Freiburg"

Plot_ChemoComplications_Matched <- CumulatedData_Matched$df_Output_TherapyComplications %>%
                                # select(-CountNoChemotherapy,
                                #        -CountNoComplicationAfterChemotherapy,
                                #        -ProportionNoChemotherapy,
                                #        -ProportionNoComplicationAfterChemotherapy) %>%
                                #filter(PatientSubgroup == "Cancer+/HIV+") %>%
                                filter(Site %in% SiteNames.Temp) %>%
                                pivot_wider(names_from = Site,
                                            values_from = c(N, starts_with("Count"), starts_with("Proportion"))) %>%
                                mutate(N_All = rowSums(across(all_of(paste0("N_", SiteNames.Temp))), na.rm = TRUE),
                                       CountAnyMajorTherapy_All = rowSums(across(all_of(paste0("CountAnyMajorTherapy_", SiteNames.Temp))), na.rm = TRUE),
                                       CountNoMajorTherapy_All = N_All - CountAnyMajorTherapy_All,
                                       CountChemotherapy_All = rowSums(across(all_of(paste0("CountChemotherapy_", SiteNames.Temp))), na.rm = TRUE),
                                       CountNoChemotherapy_All = rowSums(across(all_of(paste0("CountNoChemotherapy_", SiteNames.Temp))), na.rm = TRUE),
                                       CountComplicationAfterChemotherapy_All = rowSums(across(all_of(paste0("CountComplicationAfterChemotherapy_", SiteNames.Temp))), na.rm = TRUE),
                                       CountNoComplicationAfterChemotherapy_All = rowSums(across(all_of(paste0("CountNoComplicationAfterChemotherapy_", SiteNames.Temp))), na.rm = TRUE),
                                       ProportionAnyMajorTherapy_All = CountAnyMajorTherapy_All / N_All,
                                       ProportionNoMajorTherapy_All = CountNoMajorTherapy_All / N_All,
                                       ProportionChemotherapy_All = CountChemotherapy_All / N_All,
                                       ProportionNoChemotherapy_All = CountNoChemotherapy_All / N_All,
                                       ProportionComplicationAfterChemotherapy_All = CountComplicationAfterChemotherapy_All / N_All,
                                       ProportionNoComplicationAfterChemotherapy_All = CountNoComplicationAfterChemotherapy_All / N_All) %>%
                                       # ProportionComplicationAfterChemotherapy_All = CountComplicationAfterChemotherapy_All / CountChemotherapy_All,
                                       # ProportionNoComplicationAfterChemotherapy_All = CountNoComplicationAfterChemotherapy_All / CountChemotherapy_All) %>%
                                select(PatientSubgroup,
                                       MainCancerDiagnosisYear,
                                       #ProportionNoMajorTherapy_All,
                                       ProportionNoChemotherapy_All,
                                       ProportionNoComplicationAfterChemotherapy_All,
                                       ProportionComplicationAfterChemotherapy_All) %>%
                                pivot_longer(cols = starts_with("Proportion"),
                                             names_to = "Group",
                                             values_to = "Value") %>%
                                mutate(Group = str_remove(str_remove(Group, "Proportion"), "_All")) %>%
                                filter(between(MainCancerDiagnosisYear, 2005, 2022)) %>%
                                dsFredaClient::MakeColumnPlot(XFeature = MainCancerDiagnosisYear,
                                                              YFeature = Value,
                                                              GroupingFeature = Group,
                                                              GroupingPosition = position_fill(),
                                                              GroupingMapping = "alpha",
                                                              AlphaPalette = c(0.9, 0.5, 0.2),
                                                              GroupingSpecs = c("Complication after Chemotherapy" = "ComplicationAfterChemotherapy",
                                                                                "No complication after Chemotherapy" = "NoComplicationAfterChemotherapy",
                                                                                "No Chemotherapy" = "NoChemotherapy"),
                                                              AxisType_y = "proportional",
                                                              FacetFeature = PatientSubgroup,
                                                              FacetMapping = "fill",
                                                              FillPalette = vc_FillPalette_Subgroup)

f_ExportPlot(inp_Plot = Plot_ChemoComplications_Full,
             inp_Directory = PlotOutputPath,
             inp_Width = 24,
             inp_Height = 10)

f_ExportPlot(inp_Plot = Plot_ChemoComplications_Matched,
             inp_Directory = PlotOutputPath,
             inp_Width = 24,
             inp_Height = 10)
            


#-------------------------------------------------------------------------------

#SiteNames.Temp <- SiteNames
SiteNames.Temp <- c("Frankfurt", "Freiburg")

Plot_Discharge_Matched <- CumulatedData_Matched$df_Output_LastRecordedDischargeCategory %>%
                      #filter(PatientSubgroup == "Cancer+/HIV+") %>%
                      select(-MainCancerDiagnosisYear) %>%
                      filter(Site %in% SiteNames.Temp) %>%
                      pivot_wider(names_from = Site,
                                  values_from = c(N, Proportion)) %>%
                      group_by(LastRecordedDischargeYear, PatientSubgroup) %>%
                          mutate(N_All = rowSums(across(all_of(paste0("N_", SiteNames.Temp))), na.rm = TRUE),
                                 Proportion_All = N_All / sum(N_All)) %>%
                      ungroup() %>%
                      filter(between(LastRecordedDischargeYear, 2005, 2022)) %>%
                      dsFredaClient::MakeColumnPlot(XFeature = LastRecordedDischargeYear,
                                                    YFeature = Proportion_All,
                                                    AxisType_y = "proportional",
                                                    GroupingFeature = LastRecordedDischargeCategory,
                                                    GroupingPosition = position_fill(),
                                                    GroupingMapping = "alpha",
                                                    GroupingSpecs = c("Deceased",
                                                                      "Hospice Care",
                                                                      "Same Hospital",
                                                                      "Other Hospital",
                                                                      "Rehabilitation or Residential Care",
                                                                      "Home"),
                                                    #AlphaPalette = c(0.2, 0.35, 0.5, 0.6, 0.75, 0.9),
                                                    AlphaPalette = c(0.9, 0.75, 0.6, 0.5, 0.35, 0.2),
                                                    FacetFeature = PatientSubgroup,
                                                    FacetMapping = "fill",
                                                    FillPalette = vc_FillPalette_Subgroup)


f_ExportPlot(inp_Plot = Plot_Discharge_Full,
             inp_Directory = PlotOutputPath,
             inp_Width = 24,
             inp_Height = 10)

f_ExportPlot(inp_Plot = Plot_Discharge_Matched,
             inp_Directory = PlotOutputPath,
             inp_Width = 24,
             inp_Height = 10)

#-------------------------------------------------------------------------------

SiteNames.Temp <- SiteNames
#SiteNames.Temp <- "Freiburg"

Plot_Metastasis <- CumulatedData_Matched$df_Output_MetastasisOccurrence %>%
                      select(-CountNoMetastasis,
                             -ProportionNoMetastasis,) %>%
                      #filter(PatientSubgroup == "Cancer+/HIV+") %>%
                      filter(Site %in% SiteNames.Temp) %>%
                      pivot_wider(names_from = Site,
                                  values_from = c(N, starts_with("Count"), starts_with("Proportion"))) %>%
                      mutate(N_All = rowSums(across(all_of(paste0("N_", SiteNames.Temp))), na.rm = TRUE),
                             CountMetastasis_All = rowSums(across(all_of(paste0("CountMetastasis_", SiteNames.Temp))), na.rm = TRUE),
                             CountMetastasisWithCancerDiagnosis_All = rowSums(across(all_of(paste0("CountMetastasisWithCancerDiagnosis_", SiteNames.Temp))), na.rm = TRUE),
                             CountMetastasisAfterCancerDiagnosis_All = rowSums(across(all_of(paste0("CountMetastasisAfterCancerDiagnosis_", SiteNames.Temp))), na.rm = TRUE),
                             ProportionMetastasis_All = CountMetastasis_All / N_All,
                             ProportionMetastasisWithCancerDiagnosis_All = CountMetastasisWithCancerDiagnosis_All / N_All,
                             ProportionMetastasisAfterCancerDiagnosis_All = CountMetastasisAfterCancerDiagnosis_All / N_All) %>%
                      filter(between(MainCancerDiagnosisYear, 2005, 2022)) %>%
                      dsFredaClient::MakeColumnPlot(XFeature = MainCancerDiagnosisYear,
                                                    YFeature = ProportionMetastasisWithCancerDiagnosis_All,
                                                    #GroupingFeature = AgeGroup,
                                                    #GroupingPosition = position_fill(),
                                                    AxisType_y = "proportional",
                                                    FacetFeature = PatientSubgroup,
                                                    FacetMapping = "fill",
                                                    FillPalette = vc_FillPalette_Subgroup)


#-------------------------------------------------------------------------------

Plot_HIVCancer.Absolute <- CumulatedData_Full$df_Output_HIVCancerCategories %>%
                                filter(PatientSubgroup == "Cancer+/HIV+",
                                       between(MainCancerDiagnosisYear, 2009, 2022)) %>%
                                pivot_wider(names_from = Site,
                                            values_from = N) %>%
                                mutate(N = rowSums(across(all_of(SiteNames)), na.rm = TRUE)) %>%
                                group_by(PatientSubgroup, MainCancerDiagnosisYear, PatientSubgroupHIVCancerCategory) %>%
                                   summarize(N = sum(N)) %>%
                                group_by(PatientSubgroup, MainCancerDiagnosisYear) %>%
                                   reframe(PatientSubgroupHIVCancerCategory = PatientSubgroupHIVCancerCategory,
                                           N = N,
                                           Proportion = N / sum(N)) %>%
                                ungroup() %>%
                                dsFredaClient::MakeColumnPlot(XFeature = MainCancerDiagnosisYear,
                                                              YFeature = N,
                                                              GroupingFeature = PatientSubgroupHIVCancerCategory,
                                                              GroupingPosition = position_stack(),
                                                              GroupingMapping = "alpha",
                                                              GroupingSpecs = c("AIDS-defining cancer" = "HIV-associated AD cancer",
                                                                                "Virus-associated non-AD cancer" = "HIV-associated non-AD cancer",
                                                                                "Non-virus-associated non-AD cancer" = "Non-HIV-associated cancer"),
                                                              AlphaPalette = c(0.9, 0.5, 0.2),
                                                              FacetFeature = PatientSubgroup,
                                                              FacetMapping = "fill",
                                                              FillPalette = vc_FillPalette_Subgroup)

Plot_HIVCancer.Relative <- CumulatedData_Full$df_Output_HIVCancerCategories %>%
                                filter(PatientSubgroup == "Cancer+/HIV+",
                                       between(MainCancerDiagnosisYear, 2009, 2022)) %>%
                                pivot_wider(names_from = Site,
                                           values_from = N) %>%
                                mutate(N = rowSums(across(all_of(SiteNames)), na.rm = TRUE)) %>%
                                group_by(PatientSubgroup, MainCancerDiagnosisYear, PatientSubgroupHIVCancerCategory) %>%
                                   summarize(N = sum(N)) %>%
                                group_by(PatientSubgroup, MainCancerDiagnosisYear) %>%
                                   reframe(PatientSubgroupHIVCancerCategory = PatientSubgroupHIVCancerCategory,
                                           N = N,
                                           Proportion = N / sum(N)) %>%
                                ungroup() %>%
                                dsFredaClient::MakeColumnPlot(XFeature = MainCancerDiagnosisYear,
                                                              YFeature = Proportion,
                                                              AxisType_y = "proportional",
                                                              GroupingFeature = PatientSubgroupHIVCancerCategory,
                                                              GroupingPosition = position_fill(),
                                                              GroupingMapping = "alpha",
                                                              GroupingSpecs = c("AIDS-defining cancer" = "HIV-associated AD cancer",
                                                                                "Virus-associated non-AD cancer" = "HIV-associated non-AD cancer",
                                                                                "Non-virus-associated non-AD cancer" = "Non-HIV-associated cancer"),
                                                              AlphaPalette = c(0.9, 0.5, 0.2),
                                                              FacetFeature = PatientSubgroup,
                                                              FacetMapping = "fill",
                                                              FillPalette = vc_FillPalette_Subgroup)
                    
f_ExportPlot(inp_Plot = Plot_HIVCancer.Absolute,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)

f_ExportPlot(inp_Plot = Plot_HIVCancer.Relative,
             inp_Directory = PlotOutputPath,
             inp_Width = 20,
             inp_Height = 10)


#-------------------------------------------------------------------------------

SiteNames.Temp <- SiteNames
#SiteNames.Temp <- "Freiburg"

Plot_AIDS <- CumulatedData_Matched$df_Output_HIVCancerAIDS %>%
                  group_by(Site, MainCancerDiagnosisYear) %>%
                      mutate(AIDSOccurrence = case_when(is.na(AIDSOccurrence) ~ "Unclear",
                                                        .default = AIDSOccurrence),
                             Proportion = N / sum(N)) %>%
                  ungroup() %>%
                  filter(Site %in% SiteNames.Temp) %>%
                  pivot_wider(names_from = Site,
                              values_from = c(N, Proportion)) %>%
                  group_by(MainCancerDiagnosisYear) %>%
                      mutate(N_All = rowSums(across(all_of(paste0("N_", SiteNames.Temp))), na.rm = TRUE),
                             Proportion_All = N_All / sum(N_All)) %>%
                  ungroup() %>%
                  filter(between(MainCancerDiagnosisYear, 2005, 2022)) %>%
                  dsFredaClient::MakeColumnPlot(XFeature = MainCancerDiagnosisYear,
                                                YFeature = Proportion_All,
                                                AxisType_y = "proportional",
                                                GroupingFeature = AIDSOccurrence,
                                                GroupingPosition = position_fill(),
                                                GroupingMapping = "alpha",
                                                GroupingSpecs = c("AIDS after cancer diagnosis",
                                                                  "AIDS at or before cancer diagnosis",
                                                                  "Cancer and HIV without AIDS",
                                                                  "Unclear"),
                                                AlphaPalette = c(0.9, 0.6, 0.3, 0.1),
                                                FillPalette = color_Secondary)

#-------------------------------------------------------------------------------

Test <- CumulatedData_Matched$df_Output_AIDSOccurrence

#-------------------------------------------------------------------------------


Test <- CumulatedData_Matched$df_Output_DistinctCodeCountCancer

#-------------------------------------------------------------------------------

Test <- CumulatedData_Matched$df_Output_HIVCancerDiagnosisOrder

#-------------------------------------------------------------------------------

Test <- CumulatedData_Matched$df_Output_HIVCancerCategories

#-------------------------------------------------------------------------------

SiteNames.Temp <- SiteNames
#SiteNames.Temp <- "Freiburg"

Plot_AnyMajorTherapy <- CumulatedData_Matched$df_Output_TherapyModalities
                              select(-CountNoMajorTherapyCoded,
                                     -CountAnyMajorTherapy) %>%
  
#-------------------------------------------------------------------------------

SiteNames.Temp <- SiteNames
#SiteNames.Temp <- "Freiburg"

Plot_TherapyModalities <- CumulatedData_Matched$df_Output_TherapyModalities %>%
                              select(-CountNoMajorTherapyCoded,
                                     -CountAnyMajorTherapy) %>%
                              #filter(PatientSubgroup == "Cancer+/HIV+") %>%
                              filter(Site %in% SiteNames.Temp) %>%
                              pivot_wider(names_from = Site,
                                          values_from = c(N, starts_with("Count"), starts_with("Proportion"))) %>%
                              mutate(N_All = rowSums(across(all_of(paste0("N_", SiteNames.Temp))), na.rm = TRUE),
                                     CountSurgery_All = rowSums(across(all_of(paste0("CountSurgery_", SiteNames.Temp))), na.rm = TRUE),
                                     CountChemotherapy_All = rowSums(across(all_of(paste0("CountChemotherapy_", SiteNames.Temp))), na.rm = TRUE),
                                     CountImmunotherapy_All = rowSums(across(all_of(paste0("CountImmunotherapy_", SiteNames.Temp))), na.rm = TRUE),
                                     CountRadiotherapy_All = rowSums(across(all_of(paste0("CountRadiotherapy_", SiteNames.Temp))), na.rm = TRUE),
                                     CountStemCellTherapy_All = rowSums(across(all_of(paste0("CountStemCellTherapy_", SiteNames.Temp))), na.rm = TRUE),
                                     CountBoneMarrowTransplant_All = rowSums(across(all_of(paste0("CountBoneMarrowTransplant_", SiteNames.Temp))), na.rm = TRUE),
                                     CountCARTCellTherapy_All = rowSums(across(all_of(paste0("CountCARTCellTherapy_", SiteNames.Temp))), na.rm = TRUE),
                                     ProportionSurgery_All = CountSurgery_All / N_All,
                                     ProportionChemotherapy_All = CountChemotherapy_All / N_All,
                                     ProportionImmunotherapy_All = CountImmunotherapy_All / N_All,
                                     ProportionRadiotherapy_All = CountRadiotherapy_All / N_All,
                                     ProportionStemCellTherapy_All = CountStemCellTherapy_All / N_All,
                                     ProportionBoneMarrowTransplant_All = CountBoneMarrowTransplant_All / N_All,
                                     ProportionCARTCellTherapy_All = CountCARTCellTherapy_All / N_All) %>%
                              select(PatientSubgroup,
                                     MainCancerDiagnosisYear,
                                     ends_with("_All")) %>%
                              select(PatientSubgroup,
                                     MainCancerDiagnosisYear,
                                     starts_with("Proportion")) %>%
                              pivot_longer(cols = starts_with("Proportion"),
                                           names_to = "TherapyModality",
                                           values_to = "Value") %>% 
                              mutate(TherapyModality = str_remove(str_remove(TherapyModality, "Proportion"), "_All")) %>%
                              filter(between(MainCancerDiagnosisYear, 2009, 2022)) %>%
                              dsFredaClient::MakeColumnPlot(XFeature = MainCancerDiagnosisYear,
                                                            YFeature = Value,
                                                            GroupingFeature = TherapyModality,
                                                            GroupingPosition = position_stack(),
                                                            GroupingMapping = "alpha",
                                                            # GroupingSpecs = c("AIDS after cancer diagnosis",
                                                            #                   "AIDS at or before cancer diagnosis",
                                                            #                   "Cancer and HIV without AIDS",
                                                            #                   "Unclear"),
                                                            AxisType_y = "proportional",
                                                            FacetFeature = PatientSubgroup,
                                                            FacetMapping = "fill",
                                                            FillPalette = vc_FillPalette_Subgroup)
        
          


