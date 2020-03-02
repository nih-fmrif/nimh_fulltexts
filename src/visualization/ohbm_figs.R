library(dplyr)
library(ggplot2)
library(data.table)
library(stringr)
library(gridExtra)
library(grid)

##### helper functions #####
read_fn <- function(x){
  return(scan(x, character(), quote = ""))
}
`%ni%` <- Negate(`%in%`)

##### all the predicted instances of data sharing or open data #####
df_marked_share <- read.csv('data/external/clf_marked_data_sharing_sentences.csv')
df_marked_od <- read.csv('data/external/clf_marked_open_data_sentences.csv')

##### Projects (to identify institutions) #####
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

##### So we can link projects and papers #####
links <- list.files('data/external/RePORTER/Link_tables/', full.names = T)
df_links <- do.call(rbind, lapply(links, fread))

##### So we can link projects and papers #####
miss_files <- list.files('data/interim/full_texts/', full.names = T, pattern = 'misses*')
missed_pmcs <- do.call(c, lapply(miss_files, read_fn))

##### Those papers whose full-text we could get through the API #####
nimh_papers <- read.csv('data/interim/nimh_papers.csv')
nimh_papers %>%
  filter(PMCID %ni% missed_pmcs) %>%
  mutate(pmcid = str_sub(PMCID, 4)) -> nimh_papers

nimh_papers$open_data <- nimh_papers$pmcid %in% df_marked_od$pmcid
nimh_papers$data_share <- nimh_papers$pmcid %in% df_marked_share$pmcid

##### nimnh projects #####
df_proj %>% 
  filter(IC_CENTER=='NIMH') %>% 
  mutate(PROJECT_NUMBER = as.character(PROJECT_NUMBER)) -> nimh_proj

#clean up the project numbers
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
  #mutate(nas = rowSums(is.na(.))) %>%
  group_by(PROJECT_NUMBER) %>%
  #filter(nas == max(nas, na.rm=T)) %>%
  filter(FY == max(FY, na.rm=T)) %>%
  filter(is.na(FY_TOTAL_COST_SUB_PROJECTS)) %>%
  filter(FY_TOTAL_COST==max(FY_TOTAL_COST)) %>%
  ungroup() %>%
  select(PROJECT_ID, PROJECT_TERMS, PROJECT_TITLE, DEPARTMENT, IC_CENTER, 
         CONTACT_PI_PROJECT_LEADER, AGENCY, PROJECT_NUMBER, PROJECT_START_DATE, 
         PROJECT_END_DATE, ORGANIZATION_NAME) -> unique_grants

df_links %>%
  filter(PMID %in% nimh_papers$PMID) %>%
  filter(PROJECT_NUMBER %in% unique_grants$PROJECT_NUMBER) %>%
  left_join(nimh_papers) %>%
  left_join(unique_grants) -> project_paper_pairs

##### figure 1 (sharing/reuse by institution) #####
project_paper_pairs %>%
  select(ORGANIZATION_NAME, open_data, data_share, PMID, pmcid) %>%
  distinct() %>%
  group_by(ORGANIZATION_NAME) %>%
  summarise(open_data_count = sum(open_data, na.rm=T),
            data_sharing_count = sum(data_share, na.rm=T),
            total_papes = n()) %>%
  mutate(data_reuse_prop = open_data_count/total_papes,
         data_sharing_prop = data_sharing_count/total_papes) %>%
  filter(total_papes>2) %>%
  arrange(data_reuse_prop) %>%
  ungroup() %>%
  mutate(cumulative_dist = cume_dist(open_data_count),
         cumulative_perc = cumsum(open_data_count)/sum(open_data_count),
         cumulative_count = cumsum(open_data_count),
         institution_number = row_number()) %>%
  ggplot(aes(x=institution_number, y=data_reuse_prop)) +
  geom_point() +
  xlab('Sorted Institution (min. 3 pubs)') +
  ylab('Estimated Prop. of papers w/data reuse') +
  theme_classic() -> fig1

project_paper_pairs %>%
  select(ORGANIZATION_NAME, open_data, data_share, PMID, pmcid) %>%
  distinct() %>%
  group_by(ORGANIZATION_NAME) %>%
  summarise(open_data_count = sum(open_data, na.rm=T),
            data_sharing_count = sum(data_share, na.rm=T),
            total_papes = n()) %>%
  mutate(data_reuse_prop = open_data_count/total_papes,
         data_sharing_prop = data_sharing_count/total_papes) %>%
  filter(total_papes>2) %>%
  arrange(data_sharing_prop) %>%
  ungroup() %>%
  mutate(cumulative_dist = cume_dist(open_data_count),
         cumulative_perc = cumsum(open_data_count)/sum(open_data_count),
         cumulative_count = cumsum(open_data_count),
         institution_number = row_number()) %>%
  ggplot(aes(x=institution_number, y=data_sharing_prop)) +
  geom_point() +
  xlab('Sorted Institution (min. 3 pubs)') +
  ylab('Estimated Prop. of papers w/data sharing') +
  theme_classic() -> fig2

grid.arrange(fig1, fig2, nrow=1, top = textGrob("Most data sharing & reuse comes from\na small subset of NIMH funded institutions",
                                                gp=gpar(fontsize=20)))

##### figure 2 (sharing/reuse by PI) #####
project_paper_pairs %>%
  select(CONTACT_PI_PROJECT_LEADER, open_data, data_share, PMID, pmcid) %>%
  distinct() %>%
  group_by(CONTACT_PI_PROJECT_LEADER) %>%
  summarise(open_data_count = sum(open_data, na.rm=T),
            data_sharing_count = sum(data_share, na.rm=T),
            total_papes = n()) %>%
  mutate(data_reuse_prop = open_data_count/total_papes,
         data_sharing_prop = data_sharing_count/total_papes) %>%
  filter(total_papes>2) %>%
  arrange(data_reuse_prop) %>%
  ungroup() %>%
  mutate(cumulative_dist = cume_dist(open_data_count),
         cumulative_perc = cumsum(open_data_count)/sum(open_data_count),
         cumulative_count = cumsum(open_data_count),
         PI_number = row_number()) %>%
  ggplot(aes(x=PI_number, y=data_reuse_prop)) +
  geom_point() +
  xlab('Sorted PI (min. 3 pubs)') +
  ylab('Estimated Prop. of papers w/data reuse') +
  theme_classic() -> fig1

project_paper_pairs %>%
  select(CONTACT_PI_PROJECT_LEADER, open_data, data_share, PMID, pmcid) %>%
  distinct() %>%
  group_by(CONTACT_PI_PROJECT_LEADER) %>%
  summarise(open_data_count = sum(open_data, na.rm=T),
            data_sharing_count = sum(data_share, na.rm=T),
            total_papes = n()) %>%
  mutate(data_reuse_prop = open_data_count/total_papes,
         data_sharing_prop = data_sharing_count/total_papes) %>%
  filter(total_papes>2) %>%
  arrange(data_sharing_prop) %>%
  ungroup() %>%
  mutate(cumulative_dist = cume_dist(open_data_count),
         cumulative_perc = cumsum(open_data_count)/sum(open_data_count),
         cumulative_count = cumsum(open_data_count),
         PI_number = row_number()) %>%
  ggplot(aes(x=PI_number, y=data_sharing_prop)) +
  geom_point() +
  xlab('Sorted PI (min. 3 pubs)') +
  ylab('Estimated Prop. of papers w/data sharing') +
  theme_classic() -> fig2

grid.arrange(fig1, fig2, nrow=1, top = textGrob("Most data sharing & reuse comes from a\nsmall subset of NIMH funded Investigators",
                                                gp=gpar(fontsize=20)))
