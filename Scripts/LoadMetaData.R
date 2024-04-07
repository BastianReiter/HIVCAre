

#==============================================================================#
#------------------------------------------------------------------------------#
#   HIVCAre: LOAD META DATA                                                    #
#------------------------------------------------------------------------------#
#==============================================================================#



#===============================================================================
# Project-specific Meta Data
#===============================================================================

# Eligible Values Sets for certain Features
df_Meta_ValueSets <- read_excel(path = here("Data/MetaData/MetaData_Project.xlsx"),
                                sheet = "ValueSets")

df_Meta_AdmissionCauses <- df_Meta_ValueSets %>%
                                filter(Feature == "AdmissionCauseCode") %>%
                                select(Value,
                                       Label) %>%
                                rename(AdmissionCauseCode = Value,
                                       AdmissionCause = Label)

df_Meta_DischargeReasons <- read_excel(path = here("Data/MetaData/MetaData_Project.xlsx"),
                                       sheet = "DischargeReasons")

df_Meta_Departments <- read_excel(path = here("Data/MetaData/MetaData_Project.xlsx"),
                                  sheet = "Departments")



#===============================================================================
# OPS Code Lookup and Grouping
#===============================================================================

df_Meta_OPSCoding <- read_excel(path = here("Data/MetaData/MetaData_OPSCoding.xlsx"),
                                sheet = "OPSCodes") %>%
                          mutate(OPSCode = str_replace_all(OPSCode, "[^[:alnum:]]", ""))      # Remove all special characters in OPS Codes



#===============================================================================
# Data on Cancer Grouping based on ICD-10 Codes
#===============================================================================

df_Meta_ICD10CancerGrouping <- read_excel(path = here("Data/MetaData/MetaData_CancerCoding.xlsx"),
                                          sheet = "CancerGrouping")



#===============================================================================
# Data on Cancer-specific Surgery based on short ICD-10 and OPS Codes
#===============================================================================

df_Meta_CancerSurgery <- read_excel(path = here("Data/MetaData/MetaData_CancerCoding.xlsx"),
                                    sheet = "CancerSurgery") %>%
                            select(ICDCodeShort,
                                   OPSCodeShort) %>%
                            mutate(OPSCodeShort = str_remove_all(OPSCodeShort, "-"),
                                   IsLikelyCancerRelated = TRUE,
                                   IsLikelyCurativeIntention_Primary = TRUE)

# Temporary: Working on Cancer Curgery Classification with Frankfurt P21-Data
df_Meta_CancerSurgeryInFrankfurt <- read_excel(path = here("Data/MetaData/MetaData_CancerCoding.xlsx"),
                                               sheet = "CancerSurgeryInFrankfurt") %>%
                                        select(ICDCodeShort,
                                               OPSCodeShort,
                                               IsLikelyCancerRelated,
                                               IsLikelyCurativeIntention_Primary,
                                               IsLikelyCurativeIntention_Secondary,
                                               IsLikelySupportive_Direct,
                                               IsLikelySupportive_Indirect) %>%
                                        filter(IsLikelyCancerRelated == TRUE) %>%
                                        mutate(OPSCodeShort = str_remove_all(OPSCodeShort, "-"),
                                               across(starts_with("IsLikely"), ~ as.logical(.)))

df_Meta_CancerSurgery <- df_Meta_CancerSurgeryInFrankfurt %>%
                              add_row(df_Meta_CancerSurgery) %>%
                              distinct() %>%
                              arrange(ICDCodeShort, OPSCodeShort)



#===============================================================================
# Data on how to interpret HIV Status and HIV Disease bases on ICD-10 Codes
#===============================================================================

df_Meta_ICD10HIVStatus <- read_excel(path = here("Data/MetaData/MetaData_HIVCoding.xlsx"),
                                     sheet = "HIVStatus") %>%
                              select(-HIVCondition, -HIVConditionGerman)      # Deselect unneeded variables

df_Meta_ICD10HIVDiseases <- read_excel(path = here("Data/MetaData/MetaData_HIVCoding.xlsx"),
                                       sheet = "HIVDiseases") %>%
                                select(-HIVCode, -HIVDisease)      # Deselect unneeded variables



#===============================================================================
# ICD Code Data
#===============================================================================
# Notes and sources:
#       - ICD-10-GM Versions 2005 - 2019
#       - We assume that HIV/AIDS coding was performed abiding by the HIV coding manual of
#         "Zentralinstitut für die kassenärztliche Versorgung in Deutschland" (ZI)
#       - ICD-10-codes of AIDS-defining illnesses were inherited from the ZI HIV coding manual 2015
#       - All codes were checked with regard to possible modification throughout years from 2005 to 2019
#       - Data on ICD coding interpretation taken from preformed csv-files
#===============================================================================

# List of ICD catalogs from 2005 to 2019
ls_ICD10Catalog <- get(load(here("Data/MetaData/icd_catalog_list_2005_2019.RData")))

# Transform list into data frame for easier lookup of ICD coded diagnoses
df_Meta_ICD10Catalog <- bind_rows(ls_ICD10Catalog) %>%
                            select(year, code, title) %>%
                            mutate_at("year", as.integer) %>%
                            rename(ICDVersion = year,
                                   ICDCode = code,
                                   Diagnosis = title) %>%
                            mutate(ICDCode = str_remove_all(ICDCode, "\\.-")) %>%
                            filter(ICDVersion == 2019)

# Removing not further needed objects
rm(list = c("ls_ICD10Catalog",
            "icd_cat"))


# General Cancer Codes
# Metastasis codes are included ("C") but will be excluded in script
vc_CancerCodes <- c("C",                                                        # All malignant neoplasms (primary and secondary)
                    "D0",                                                       # In-situ neoplasms
                    "D37", "D38", "D39", "D40", "D41", "D42", "D43", "D44", "D45", "D46", "D47", "D48",      # Neoplasms of uncertain / unknown behaviour
                    "Z08",                                                      # Follow-up examination after treatment for malignant neoplasms
                    "Z85", "Z92.6")                                             # Personal history of malignant neoplasms

# General Codes for Cancer Metastases
vc_MetastasisCodes <- c("C77", "C78", "C79")


# Primary HIV codes indicating HIV infection (as used in ZI HIV coding manual)
vc_HIVCodes <- c("O98.7",                                                       # HIV disease complicating pregnancy, childbirth and the puerperium
                 "Z21",                                                         # Asymptomatic HIV infection
                 "B20",                                                         # HIV disease resulting in infectious and parasitic diseases
                 "B21",                                                         # HIV disease resulting in malignant neoplasms
                 "B22",                                                         # HIV disease resulting in other specified diseases
                 "B23.-",                                                       # HIV disease resulting in other conditions
                 "B23.0",                                                       # Acute HIV infection syndrome
                 "B23.8",                                                       # Persistent generalized Lymphadenopathy due to HIV disease
                 "B24")                                                         # Unspecified HIV disease
