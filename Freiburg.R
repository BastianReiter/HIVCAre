

df_IDM_Cases <- Validation_df_IDM_Cases_Raw$tbl
df_IDM_CasesDepartment <- Validation_df_IDM_CasesDepartment_Raw$tbl
df_IDM_CasesICD <- Validation_df_IDM_CasesICD_Raw$tbl
df_IDM_CasesOPS <- Validation_df_IDM_CasesOPS_Raw$tbl
df_ADM_Patients <- Validation_df_ADM_Patients$tbl
df_ADM_PatientsCancer <- Validation_df_ADM_PatientsCancer$tbl


DataFreiburg <- list(df_IDM_Cases = df_IDM_Cases,
                     df_IDM_CasesDepartment = df_IDM_CasesDepartment,
                     df_IDM_CasesICD = df_IDM_CasesICD,
                     df_IDM_CasesOPS = df_IDM_CasesOPS,
                     df_ADM_Patients = df_ADM_Patients,
                     df_ADM_PatientsCancer = df_ADM_PatientsCancer)

saveRDS(DataFreiburg, "DataFreiburg.rds")


df_IDM_Cases <- DataFreiburg$df_IDM_Cases
df_IDM_CasesDepartment <- DataFreiburg$df_IDM_CasesDepartment
df_IDM_CasesICD <- DataFreiburg$df_IDM_CasesICD
df_IDM_CasesOPS <- DataFreiburg$df_IDM_CasesOPS

# Remove all admissions after year 2022
df_IDM_Cases <- df_IDM_Cases %>%
                    mutate(Year = year(AdmissionDate)) %>%
                    filter(Year < 2023) %>%
                    select(-Year)



OLD.Val.IDM.Cases.Harmonized <- SiteOutputData_Full$Freiburg$ValidationReport_df_IDM_Cases_Harmonized
OLD.Val.IDM.CasesDepartment.Harmonized <- SiteOutputData_Full$Freiburg$ValidationReport_df_IDM_CasesDepartment_Harmonized
OLD.Val.IDM.CasesICD.Harmonized <- SiteOutputData_Full$Freiburg$ValidationReport_df_IDM_CasesICD_Harmonized
OLD.Val.IDM.CasesOPS.Harmonized <- SiteOutputData_Full$Freiburg$ValidationReport_df_IDM_CasesOPS_Harmonized
OLD.Val.ADM.Patients <- SiteOutputData_Full$Freiburg$ValidationReport_df_ADM_Patients
OLD.Val.ADM.PatientsCancer <- SiteOutputData_Full$Freiburg$ValidationReport_df_ADM_PatientsCancer


OLDValReports <- list(OLD.Val.ADM.Patients,
                      OLD.Val.ADM.PatientsCancer,
                      OLD.Val.IDM.Cases.Harmonized,
                      OLD.Val.IDM.CasesDepartment.Harmonized,
                      OLD.Val.IDM.CasesICD.Harmonized,
                      OLD.Val.IDM.CasesOPS.Harmonized)

saveRDS(OLDValReports, file = "OldValReports.rds")


NEWValReports <- list(NEW.Val.ADM.Patients = ValidationReport_df_ADM_Patients,
                      NEW.Val.ADM.PatientsCancer = ValidationReport_df_ADM_PatientsCancer,
                      NEW.Val.IDM.Cases.Harmonized = ValidationReport_df_IDM_Cases_Harmonized,
                      NEW.Val.IDM.CasesDepartment.Harmonized = ValidationReport_df_IDM_CasesDepartment_Harmonized,
                      NEW.Val.IDM.CasesICD.Harmonized = ValidationReport_df_IDM_CasesICD_Harmonized,
                      NEW.Val.IDM.CasesOPS.Harmonized = ValidationReport_df_IDM_CasesOPS_Harmonized)

saveRDS(NEWValReports, file = "NewValReports.rds")
