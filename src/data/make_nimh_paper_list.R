library(data.table)
library(dplyr)
library(lubridate)

links <- list.files('data/external/RePORTER/Link_tables/', full.names = T)
df_links <- do.call(rbind, lapply(links, fread))

#I get some errors if i do.call(rbind.... ) the project and publication files 
setwd('/Users/riddleta/Desktop/promethium/home/riddleta/nimh_fulltexts/')
proj <- list.files('data/external/RePORTER/Projects/', full.names = T)
df_proj1 <- read.csv(proj[1])
df_proj2 <- read.csv(proj[2])
df_proj3 <- read.csv(proj[3])
df_proj4 <- read.csv(proj[4])
df_proj5 <- read.csv(proj[5])
df_proj6 <- read.csv(proj[6])
df_proj7 <- read.csv(proj[7])
df_proj8 <- read.csv(proj[8])
df_proj9 <- read.csv(proj[9])
df_proj10 <- read.csv(proj[10])
df_proj11 <- read.csv(proj[11])
df_proj12 <- read.csv(proj[12])
df_proj13 <- read.csv(proj[13])
df_proj14 <- read.csv(proj[14])
df_proj15 <- read.csv(proj[15])
df_proj <- rbind(df_proj1, df_proj2, df_proj3, df_proj4, df_proj5, df_proj6, df_proj7, df_proj8,
                 df_proj9, df_proj10, df_proj11, df_proj12, df_proj13, df_proj14, df_proj15)
rm(df_proj1, df_proj2, df_proj3, df_proj4, df_proj5, df_proj6, df_proj7, df_proj8,
   df_proj9, df_proj10, df_proj11, df_proj12, df_proj13, df_proj14, df_proj15)

pubs <- list.files('data/external/RePORTER/Publications/', full.names = T)
df_pubs1 <- read.csv(pubs[1])
df_pubs2 <- read.csv(pubs[2])
df_pubs3 <- read.csv(pubs[3])
df_pubs4 <- read.csv(pubs[4])
df_pubs5 <- read.csv(pubs[5])
df_pubs6 <- read.csv(pubs[6])
df_pubs7 <- read.csv(pubs[7])
df_pubs8 <- read.csv(pubs[8])
df_pubs9 <- read.csv(pubs[9])
df_pubs10 <- read.csv(pubs[10])
df_pubs11 <- read.csv(pubs[11])
df_pubs12 <- read.csv(pubs[12])
df_pubs13 <- read.csv(pubs[13])
df_pubs14 <- read.csv(pubs[14])
df_pubs15 <- read.csv(pubs[15])
df_pubs <- rbind(df_pubs1, df_pubs2, df_pubs3, df_pubs4, df_pubs5, 
  df_pubs6, df_pubs7, df_pubs8, df_pubs9, df_pubs10, 
  df_pubs11, df_pubs12, df_pubs13, df_pubs14, df_pubs15
  )
rm(#df_pubs1, df_pubs2, df_pubs3, df_pubs4, df_pubs5, 
  df_pubs6, df_pubs7, df_pubs8, df_pubs9, df_pubs10, 
  df_pubs11, df_pubs12, df_pubs13, df_pubs14, df_pubs15
  )

#pmc to pmid linking file
pmc_ids <- read.csv('data/external/PMC-ids.csv')

df_proj %>% 
  filter(IC_CENTER=='NIMH') %>% 
  mutate(PROJECT_NUMBER = as.character(PROJECT_NUMBER)) -> nimh_proj

##### clean up the project numbers #####
nimh_proj %>% 
  mutate(proj_num_len_pre = nchar(PROJECT_NUMBER)) %>%
  mutate(PROJECT_NUMBER = stringr::str_remove(PROJECT_NUMBER, '\\s?\\([0-9]+\\)')) %>%
  mutate(PROJECT_NUMBER = stringr::str_remove(PROJECT_NUMBER, '-.+')) %>%
  mutate(PROJECT_NUMBER = stringr::str_remove(PROJECT_NUMBER, '^[0-9]')) %>%
  mutate(proj_num_len_post = nchar(PROJECT_NUMBER)) -> nimh_proj

nimh_proj %>% 
  mutate(PROJECT_NUMBER = stringr::str_remove(PROJECT_NUMBER, '\\s?\\([0-9]+\\)')) %>%
  mutate(PROJECT_NUMBER = stringr::str_remove(PROJECT_NUMBER, '-.+')) %>%
  mutate(PROJECT_NUMBER = stringr::str_remove(PROJECT_NUMBER, '^[0-9]')) %>%
  mutate(nas = rowSums(is.na(.))) %>%
  group_by(PROJECT_NUMBER) %>%
  filter(nas == max(nas, na.rm=T)) %>%
  filter(FY == max(FY, na.rm=T)) %>%
  ungroup() %>%
  select(PROJECT_ID, PROJECT_TERMS, PROJECT_TITLE, DEPARTMENT, 
         AGENCY, PROJECT_NUMBER, PROJECT_START_DATE, PROJECT_END_DATE) -> unique_grants

unique_grants %>%
  mutate(PROJECT_START_DATE = mdy(as.character(PROJECT_START_DATE)),
         PROJECT_END_DATE = mdy(as.character(PROJECT_END_DATE))) %>%
  right_join(df_links) %>%
  filter(!is.na(PROJECT_TERMS)) -> nimh_papers

nimh_papers %>%
  select(PMID) %>%
  distinct() %>%
  left_join(pmc_ids) %>%
  filter(!is.na(PMCID)) %>%
  filter(Year>2008) -> nimh_papers

write.csv(nimh_papers, 'data/interim/nimh_papers.csv', row.names=FALSE)
