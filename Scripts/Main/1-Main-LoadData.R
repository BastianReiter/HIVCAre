

################################################################################
#------------------------------------------------------------------------------#
#   HIVCAre: LOAD DATA (MAIN)                                                  #
#------------------------------------------------------------------------------#
################################################################################

library(purrr)


f_LoadSiteOutput <- function(SiteName,
                             AnalysisType,
                             CommonDataDirectory)
{
    # Set file name prefix depending on 'AnalysisType'
    FileNamePrefix <- switch(EXPR = AnalysisType,
                             Full = "SiteOutput_",
                             Matched = "SiteOutput_MatchedDataset_")
    
    # Name of .RData-file containing site output data
    FileName <- paste0(FileNamePrefix, SiteName, ".RData")
    
    # Path to directory containing .RData-file
    SiteDataDirectory <- paste0(CommonDataDirectory, "/", SiteName)
    
    # Decrypt file if necessary
    if (FileName %in% list.files(path = SiteDataDirectory))
    {
        cat("The file '", FileName, "' is already present in [", SiteDataDirectory, "]. Proceeding without decryption.\n", sep = "")
    }
    else
    {
        decrypt_file(.path = paste0(SiteDataDirectory, "/", FileName, ".encryptr.bin"),
                     private_key_path = "./id_rsa")
    }
    
    # Create environment to unload data
    DataEnvironment <- new.env()
    
    # Load data into 'FullData'-environment
    load(file = paste0(SiteDataDirectory, "/", FileName),
         envir = DataEnvironment)
    
    # Turn environment into list and return it
    return(as.list(DataEnvironment))
}


SiteNames <- c("Cologne",
               "Frankfurt",
               "Freiburg",
               "MunichLMU")

SiteOutputData_Full <- SiteNames %>%
                            map(\(sitename) f_LoadSiteOutput(SiteName = sitename,
                                                             CommonDataDirectory = here("Data/SiteOutputData"),
                                                             AnalysisType = "Full")) %>%
                            set_names(SiteNames)

SiteOutputData_Matched <- SiteNames %>%
                              map(\(sitename) f_LoadSiteOutput(SiteName = sitename,
                                                               CommonDataDirectory = here("Data/SiteOutputData"),
                                                               AnalysisType = "Matched")) %>%
                              set_names(SiteNames)




f_MergeSiteData <- function(SiteOutputData_Full
                            )
{
    
    
}



SiteObjectAvailability <- SiteNames %>%
                              map(function(sitename)
                                  { 
                                    tibble(Object = names(SiteOutputData_Full[[sitename]]),
                                           Available = TRUE) %>%
                                        set_names(c("Object", sitename))
                                  }) %>%
                              set_names(SiteNames)

UniqueOccurringObjectNames <- SiteObjectAvailability %>%
                                  map(\(Site) Site$Object) %>%
                                  unlist() %>%
                                  unique()


ObjectAvailability <- tibble(Object = UniqueOccurringObjectNames) %>% 
                          arrange(Object)

for (i in 1:length(SiteObjectAvailability))
{
    ObjectAvailability <- ObjectAvailability %>%
                              left_join(SiteObjectAvailability[[i]], by = join_by(Object))
}
  



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




