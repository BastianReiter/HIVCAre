


df_Aux_CancerSurgeryExport <- read_excel(path = here("Data/MetaData/MetaData_CancerCoding.xlsx"),
                                         sheet = "CancerSurgery") %>%
                                  select(ICDCodeShort,
                                         OPSCodeShort) %>%
                                  left_join(read_excel(path = here("Data/MetaData/MetaData_OPSCoding.xlsx"), sheet = "OPSCodes"),
                                            by = join_by(OPSCodeShort == OPSCode)) %>%
                                  select(ICDCodeShort,
                                         OPSCodeShort,
                                         Procedure) %>%
                                  left_join(df_Meta_ICD10CancerGrouping, by = join_by(ICDCodeShort)) %>%
                                  select(ICDCodeShort,
                                         CancerTopographyDetail_ICD10,
                                         OPSCodeShort,
                                         Procedure)

write_csv(df_Aux_CancerSurgeryExport, file = "CancerSurgeryExport.csv")





df_Aux_MostCommonCancerSurgeries <- df_ADM_CaseProcedures %>%
                                        group_by(PatientPseudonym) %>%
                                        filter(is.na(Patient_PresumedMainCancerCodeShort) == FALSE & ProcedureType == "Surgery") %>%
                                        distinct(OPSCodeShort, .keep_all = TRUE) %>%
                                        group_by(Patient_PresumedMainCancerCodeShort) %>%
                                        count(OPSCodeShort, name = "Count") %>%
                                        arrange(desc(Count), .by_group = TRUE) %>%
                                        left_join(df_Meta_ICD10CancerGrouping, by = join_by(Patient_PresumedMainCancerCodeShort == ICDCodeShort)) %>%
                                        left_join(df_Meta_OPSCoding, by = join_by(OPSCodeShort == OPSCode)) %>%
                                        select(Patient_PresumedMainCancerCodeShort,
                                               CancerTopographyDetail_ICD10,
                                               OPSCodeShort,
                                               Procedure,
                                               Count)

write_csv(df_Aux_MostCommonCancerSurgeries, file = "MostCommonCancerSurgeries.csv")




