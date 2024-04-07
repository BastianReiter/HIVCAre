

################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: VALIDATE INPUT DATA (MAIN)                                        #
#------------------------------------------------------------------------------#
################################################################################


ls_ValidationFreiburg <- list.files(path = here::here("Data/SiteOutputData/Freiburg/Validation"))

for(i in 1:length(ls_ValidationFreiburg))
{
    load(file = paste0(here("Data/SiteOutputData/Freiburg/Validation"), "/", ls_ValidationFreiburg[i]),
         envir = env_Freiburg,
         verbose = TRUE)
}


load(file = "C:/Users/Basti/ARBEIT Lokal/HIVCAre/Data/SiteOutputData/Frankfurt/SiteOutput_MatchedDataset_Frankfurt.RData",
     envir = env_Freiburg)
