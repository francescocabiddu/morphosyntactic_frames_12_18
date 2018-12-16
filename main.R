lib <- c("magrittr", "tidyverse", 
         "data.table", "fastmatch",
         "beepr", "mailR", "janitor")
lapply(lib, require, character.only = TRUE)
rm(lib)


#### model frames ####
mod_frames <- "Phrases in model 3 or more words used 5 times or more.csv" %>%
  read_csv(col_names = FALSE) %>%
  `colnames<-`(c("frame", "freq")) %>%
  mutate(frame = frame %>%
           str_replace_all("^\\(|\\)$", "") %>%
           str_split("\\)\\("),
         frame_joined = frame %>%
           sapply(paste, collapse = "-"))

#### mot and chi utterances ####
load("~/Dropbox/phd/morphosyn_frames_18/mc_dfs.RData")

subset_original <- function(original_df, actor, type_phon) {
  original_df %>%
    filter(id == actor) %>%
    mutate(utt_phon = string %>%
             sapply(function(x) {
               type_phon$phon[fmatch(x, type_phon$word)]
             }),
           utt_phon_joined = utt_phon %>%
             sapply(paste, collapse = "-")) 
}

mot_na <- mot_chi_na %>%
  subset_original("MOT", mot_phon)

chi_na <- mot_chi_na %>%
  subset_original("CHI", chi_phon)

# detele phrases included in in longer phrases
mod_frames %<>%
  mutate(frame_joined_long = frame_joined %>%
           sapply(function(x) {
             if (mod_frames %>%
                 filter(frame_joined != x) %>%
                 .$frame_joined %>%
                 str_count(x %>%
                           paste("-", sep = "")) %>%
                 sum() > 0) {
               NA
             } else {
               x
             }
           }))
