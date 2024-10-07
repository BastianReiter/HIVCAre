################################################################################

# Trend Analysis

################################################################################

# load metatdatei CancerGrouping
df_Meta_ICD10CancerGrouping <- read_excel(path = here("Data/MetaData/MetaData_CancerCoding.xlsx"),
                                          sheet = "CancerGrouping")

# Data stratified by year of cancer diagnosis
df_Output_CancerEntities_OverTime_Main_Filtered <- filter(df_Output_CancerEntities_OverTime_Matched_Main, (MainCancerDiagnosisYear >= 2009 & MainCancerDiagnosisYear <= 2021))


df_CancerEntities_OverTime  <- df_Output_CancerEntities_OverTime_Main_Filtered %>%
  group_by(PatientSubgroup, MainCancerCode, MainCancerDiagnosisYear) %>%
  mutate(EntitieSum = sum(N)) %>% slice(1) %>% filter(PatientSubgroup == "Cancer+/HIV+") %>% 
  select(-Strata, -N, -Site, -Proportion)

df_CancerEntities_OverTime$ICDCodeShort <- substring(df_CancerEntities_OverTime$MainCancerCode, 1, 3) 

df_CancerEntitiesGrouped_OverTime <- df_CancerEntities_OverTime %>% left_join(df_Meta_ICD10CancerGrouping, by = join_by(ICDCodeShort)) %>%
  select(MainCancerCode, MainCancerDiagnosisYear,EntitieSum, ICDCodeShort, CancerTopographyGroup_ICD10, CancerTopographyDetail_ICD10, CancerTopographyGroup_ZFKD, CancerSpecification)

###Plot Group 
df_CancerEntitiesGrouped_OverTime %>% 
   group_by(MainCancerDiagnosisYear, CancerTopographyGroup_ICD10) %>%
  mutate(N = sum(EntitieSum)) %>% slice (1) %>%
  ggplot() +
  aes(x = MainCancerDiagnosisYear, y= N) +
  geom_line() +
  facet_wrap(~CancerTopographyGroup_ICD10) +
  geom_smooth(method='glm', colour='#8e1e39',size=0.5)
   

tiff(paste0(path,"/",'TrendGroup.tiff'), units="in", width=10, height=4, res=300, compression = 'lzw')

   ###Plot Group spcification
   df_CancerEntitiesGrouped_OverTime %>% 
     filter(CancerTopographyGroup_ICD10 == "Digestive organs" |
              CancerTopographyGroup_ICD10 == "Lymphoid and haematopoietic tissue" | 
              CancerTopographyGroup_ICD10 == "Male genital organs" ) %>%
   group_by(MainCancerDiagnosisYear, CancerTopographyGroup_ICD10) %>%
     mutate(N = sum(EntitieSum)) %>% slice (1) %>%
     ggplot() +
     aes(x = MainCancerDiagnosisYear, y= N) +
     geom_line() +
     labs(y = "Number of patients") +
     geom_smooth(method='glm', colour='#0570b0',size=0.5) + 
     facet_wrap(~CancerTopographyGroup_ICD10, ncol= 3) +
     theme_light() +
     theme(text = element_text(family = "Karla", size = 40),
           axis.title.x = element_blank(),
           axis.title.y = element_text(face = "bold", color = color_DarkGrey))
  
   dev.off()

###Plot Group Digestive organs
df_CancerEntitiesGrouped_OverTime %>% 
  filter(CancerTopographyGroup_ICD10 == "Digestive organs") %>%
  group_by(MainCancerDiagnosisYear, CancerTopographyDetail_ICD10) %>%
  mutate(N = sum(EntitieSum)) %>% slice (1) %>%
  ggplot() +
  aes(x = MainCancerDiagnosisYear, y= N) +
  geom_line()+
  facet_wrap(~CancerTopographyDetail_ICD10,  scales = "free") +
  geom_smooth(method='glm', colour='#8e1e39',size=0.5)


#Specific
tiff(paste0(path,"/",'TrendDigestiveOrgans.tiff'), units="in", width=3.5, height=6, res=300, compression = 'lzw')

df_CancerEntitiesGrouped_OverTime %>% 
  filter(CancerTopographyDetail_ICD10 == "Anus and anal canal" | 
      CancerTopographyDetail_ICD10 == "Liver and intrahepatic bile ducts") %>%
  group_by(MainCancerDiagnosisYear, CancerTopographyDetail_ICD10) %>%
  mutate(N = sum(EntitieSum)) %>% slice (1) %>%
  ggplot() +
  aes(x = MainCancerDiagnosisYear, y= N) +
  geom_line()+
  facet_wrap(~CancerTopographyDetail_ICD10, ncol=1) + 
  labs(y = "Number of patients") +
  geom_smooth(method='glm', colour='#0570b0',size=0.5) + 
  theme_light() +
  theme(text = element_text(family = "Karla", size = 40),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = color_DarkGrey))

dev.off()

###Plot Group Lip, oral cavity, pharynx
df_CancerEntitiesGrouped_OverTime %>% 
  filter(CancerTopographyGroup_ICD10 == "Lip, oral cavity, pharynx") %>%
  group_by(MainCancerDiagnosisYear, CancerTopographyGroup_ZFKD) %>%
  mutate(N = sum(EntitieSum)) %>% slice (1) %>%
  ggplot() +
  aes(x = MainCancerDiagnosisYear, y= N) +
  geom_line()+
  facet_wrap(~CancerTopographyDetail_ICD10,  scales = "free") +
  geom_smooth(method='glm', colour='#8e1e39',size=0.5)


###Plot Group Respiratory and intrathoracic organs
df_CancerEntitiesGrouped_OverTime %>% 
  filter(CancerTopographyGroup_ICD10 == "Respiratory and intrathoracic organs") %>%
  group_by(MainCancerDiagnosisYear, CancerTopographyGroup_ZFKD) %>%
  mutate(N = sum(EntitieSum)) %>% slice (1) %>%
  ggplot() +
  aes(x = MainCancerDiagnosisYear, y= N) +
  geom_line()+
  facet_wrap(~CancerTopographyDetail_ICD10,  scales = "free") +
  geom_smooth(method='glm', colour='#8e1e39',size=0.5)

###Plot Group Male genital organs
df_CancerEntitiesGrouped_OverTime %>% 
  filter(CancerTopographyGroup_ICD10 == "Male genital organs") %>%
  group_by(MainCancerDiagnosisYear, CancerTopographyGroup_ZFKD) %>%
  mutate(N = sum(EntitieSum)) %>% slice (1) %>%
  ggplot() +
  aes(x = MainCancerDiagnosisYear, y= N) +
  geom_line()+
  facet_wrap(~CancerTopographyDetail_ICD10,  scales = "free") +
  geom_smooth(method='glm', colour='#8e1e39',size=0.5)

#Specific
tiff(paste0(path,"/",'TrendMaleOrgans.tiff'), units="in", width=3.5, height=6, res=300, compression = 'lzw')

df_CancerEntitiesGrouped_OverTime %>% 
  filter(CancerTopographyDetail_ICD10 == "Penis" | 
           CancerTopographyDetail_ICD10 == "Prostate") %>%
  group_by(MainCancerDiagnosisYear,CancerTopographyDetail_ICD10) %>%
  mutate(N = sum(EntitieSum)) %>% slice (1) %>%
  ggplot() +
  aes(x = MainCancerDiagnosisYear, y= N) +
  geom_line()+
  facet_wrap(~CancerTopographyDetail_ICD10, ncol=1) + 
  #labs(y = "Number of patients") +
  geom_smooth(method='glm', colour='#0570b0',size=0.5) + 
  theme_light() +
  theme(text = element_text(family = "Karla", size = 40),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = color_DarkGrey))

dev.off()


###Plot Group Female genital organs
df_CancerEntitiesGrouped_OverTime %>% 
  filter(CancerTopographyGroup_ICD10 == "Female genital organs") %>%
  group_by(MainCancerDiagnosisYear, CancerTopographyGroup_ZFKD) %>%
  mutate(N = sum(EntitieSum)) %>% slice (1) %>%
  ggplot() +
  aes(x = MainCancerDiagnosisYear, y= N) +
  geom_line()+
  facet_wrap(~CancerTopographyDetail_ICD10,  scales = "free") +
  geom_smooth(method='glm', colour='#8e1e39',size=0.5)

###Plot Group Urinary tract
df_CancerEntitiesGrouped_OverTime %>% 
  filter(CancerTopographyGroup_ICD10 == "Urinary tract") %>%
  group_by(MainCancerDiagnosisYear, CancerTopographyGroup_ZFKD) %>%
  mutate(N = sum(EntitieSum)) %>% slice (1) %>%
  ggplot() +
  aes(x = MainCancerDiagnosisYear, y= N) +
  geom_line()+
  facet_wrap(~CancerTopographyDetail_ICD10,  scales = "free") +
  geom_smooth(method='glm', colour='#8e1e39',size=0.5)

###Plot Group Eye, brain and other parts of CNS
df_CancerEntitiesGrouped_OverTime %>% 
  filter(CancerTopographyGroup_ICD10 == "Eye, brain and other parts of CNS") %>%
  group_by(MainCancerDiagnosisYear, CancerTopographyGroup_ZFKD) %>%
  mutate(N = sum(EntitieSum)) %>% slice (1) %>%
  ggplot() +
  aes(x = MainCancerDiagnosisYear, y= N) +
  geom_line()+
  facet_wrap(~CancerTopographyDetail_ICD10,  scales = "free") +
  geom_smooth(method='glm', colour='#8e1e39',size=0.5)



###Plot Group Lymphoid and haematopoietic tissue
df_CancerEntitiesGrouped_OverTime %>% 
  filter(CancerTopographyGroup_ICD10 == "Lymphoid and haematopoietic tissue") %>%
  group_by(MainCancerDiagnosisYear, CancerTopographyGroup_ICD10) %>%
  mutate(N = sum(EntitieSum)) %>% slice (1) %>%
ggplot() +
  aes(x = MainCancerDiagnosisYear, y= N) +
  geom_line() +
  facet_wrap(~CancerTopographyDetail_ICD10,  scales = "free") +
  geom_smooth(method='glm', colour='#8e1e39',size=0.5)

### Plot Group ZFKD
df_CancerEntitiesGrouped_OverTime %>% 
  filter(CancerTopographyGroup_ICD10 == "Lymphoid and haematopoietic tissue") %>%
  group_by(MainCancerDiagnosisYear, CancerTopographyGroup_ZFKD) %>%
  mutate(N = sum(EntitieSum)) %>% slice (1) %>%
  ggplot() +
  aes(x = MainCancerDiagnosisYear, y= N) +
  geom_line()+
  facet_grid(~CancerTopographyGroup_ZFKD,  scales = "free")+
  geom_smooth(method='glm', colour='#8e1e39',size=0.5)


#Specific
tiff(paste0(path,"/",'TrendBlood.tiff'), units="in", width=5, height=11, res=300, compression = 'lzw')

df_CancerEntitiesGrouped_OverTime %>% 
  filter(CancerTopographyGroup_ZFKD == "Leukemia" | 
           CancerTopographyGroup_ZFKD == "Hodgkin Lymphoma" |
           CancerTopographyGroup_ZFKD == "Non-Hodgkin-Lymphoma") %>%
  group_by(MainCancerDiagnosisYear, CancerTopographyGroup_ZFKD) %>%
  mutate(N = sum(EntitieSum)) %>% slice (1) %>%
  ggplot() +
  aes(x = MainCancerDiagnosisYear, y= N) +
  geom_line()+
  facet_wrap(~factor(CancerTopographyGroup_ZFKD, c("Leukemia", "Hodgkin Lymphoma", "Non-Hodgkin-Lymphoma")), ncol=1) + 
  #labs(y = "Number of patients") +
  geom_smooth(method='glm', colour='#0570b0',size=0.5) + 
  theme_light() +
  theme(text = element_text(family = "Karla", size = 60),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = color_DarkGrey))

dev.off()

### CancerSpecification
df_CancerEntitiesGrouped_OverTime %>% 
  filter(CancerTopographyGroup_ZFKD == "Non-Hodgkin-Lymphoma") %>%
  group_by(MainCancerDiagnosisYear, CancerSpecification) %>%
  mutate(N = sum(EntitieSum)) %>% slice (1) %>%
  ggplot() +
  aes(x = MainCancerDiagnosisYear, y= N) +
  geom_line()+
  facet_grid(~CancerSpecification,  scales = "free")+
  geom_smooth(method='glm', colour='#8e1e39',size=0.5)


# Kaposi
tiff(paste0(path,"/",'TrendKaposi.tiff'), units="in", width=5, height=3, res=300, compression = 'lzw')

df_CancerEntitiesGrouped_OverTime %>% 
  filter(CancerTopographyDetail_ICD10 == "Kaposi sarcoma") %>%
  group_by(MainCancerDiagnosisYear, CancerTopographyDetail_ICD10) %>%
  mutate(N = sum(EntitieSum)) %>% slice (1) %>%
  ggplot() +
  aes(x = MainCancerDiagnosisYear, y= N) +
  geom_line()+
  facet_wrap(~CancerTopographyDetail_ICD10, ncol=1) + 
  labs(y = "Number of patients") +
  geom_smooth(method='glm', colour='#0570b0',size=0.5) + 
  theme_light() +
  theme(text = element_text(family = "Karla", size = 40),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", color = color_DarkGrey))

dev.off()

