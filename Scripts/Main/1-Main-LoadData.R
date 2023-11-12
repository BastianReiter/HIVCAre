

################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: LOAD DATA (MAIN)                                                  #
#------------------------------------------------------------------------------#
################################################################################


vc_SiteNames <- c("Frankfurt",
                  "Freiburg")



f_LoadSiteOutput <- function(inp_SiteName)
{
    decrypt_file(.path = here("Data/SiteOutputData", inp_SiteName, paste0("SiteOutput_", inp_SiteName, ".RData.encryptr.bin")),
                 private_key_path = here("id_rsa"))

    decrypt_file(.path = here("Data/SiteOutputData", inp_SiteName, paste0("SiteOutput_MatchedDataset_", inp_SiteName, ".RData.encryptr.bin")),
                 private_key_path = here("id_rsa"))
    
}



inp_SiteName <- "Frankfurt"

SiteEnvName <- paste0("env_", inp_SiteName)

# Create environment for site-specific output data
assign(SiteEnvName, new.env(parent = .GlobalEnv))

decrypt_file(.path = here("Data/SiteOutputData", inp_SiteName, paste0("SiteOutput_", inp_SiteName, ".RData.encryptr.bin")),
             private_key_path = "./id_rsa")


load(file = here("Data/SiteOutputData", inp_SiteName, paste0("SiteOutput_", inp_SiteName, ".RData")),
     envir = get(SiteEnvName))

#ls(envir = get(SiteEnvName))



for (object in ls(envir = get(SiteEnvName)))
{
    if (str_detect(object, "df_Output_") == TRUE 
          & str_detect(object, "_Quantiles") == FALSE)
    {
        Object_NewName <- str_replace_all(object, "_Output_", "_Main_")
        assign(Object_NewName,
               value = get(object, envir = get(SiteEnvName)))
    }
}




