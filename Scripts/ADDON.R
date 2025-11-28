



load("./Data/SiteOutputData/Frankfurt/SiteOutput_Frankfurt.RData")
Unmatched <- sapply(ls(), get)
rm(list = ls()[ls() != "Unmatched"])

load("./Data/SiteOutputData/Frankfurt/SiteOutput_MatchedDataSet_Frankfurt.RData")
Matched <- sapply(ls()[ls() != "Unmatched"], get)
rm(list = ls()[!ls() %in% c("Matched", "Unmatched")])


# MUNICH
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("./Data/SiteOutputData/MunichLMU/SiteOutput_MunichLMU.RData")
Unmatched <- sapply(ls(), get)
rm(list = ls()[ls() != "Unmatched"])


load("./Data/SiteOutputData/MunichLMU/SiteOutput_MatchedDataSet_MunichLMU.RData")
Matched <- sapply(ls()[ls() != "Unmatched"], get)
rm(list = ls()[!ls() %in% c("Matched", "Unmatched")])

View(Unmatched$df_Output_Age_Summary)
View(Matched$df_Output_Age_Summary)


# FREIBURG
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load("./Data/SiteOutputData/Freiburg/SiteOutput_Freiburg.RData")

Unmatched <- sapply(ls(), get)

rm(list = ls()[ls() != "Unmatched"])


load("./Data/SiteOutputData/Freiburg/SiteOutput_MatchedDataSet_Freiburg.RData")

Matched <- sapply(ls()[ls() != "Unmatched"], get)

rm(list = ls()[!ls() %in% c("Matched", "Unmatched")])





rm(list=ls())
