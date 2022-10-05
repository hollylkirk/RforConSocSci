# this is Holly's script

# before workshop survey analysis and summary

library(tidyverse)
library(here)
library(ggplot2)
library(scales)
library(paletteer)
library(RColorBrewer)
# library(GGally)

outfolder <- here("data/derivedData/")
figFolder <- here("figs")

# source(here("r/prepFunctionsInd.R"))

#SURVEY 1
# load & prep the data survey 1
df <- read_csv(here("data/rawData/BSUD workshop survey 1_November 7, 2021_02.14.csv")) %>%
  # remove first 2 rows of data which are nonsense
  slice(-1, -2)
# filter incomplete records
df <- filter(df, df$Finished == "True")

# remove junk columns
df <- select(df, -c(1:7))
df <- select(df, -c(2:10))
# remove data from before workshops
# convert to date
df %>%
  # convert date column to a date
  mutate(RecordedDate = as.Date(RecordedDate)) %>%
  # filter to remove all from before workshops
  filter(RecordedDate >= as.Date("2021-04-28")) -> df



# remove the extra answers in the person type column
df %>%
  mutate(
    jobID = str_remove(Q3, ",.*$"), #regex expression to remove stuff after commas in the string
    jobID = case_when(
      Q3 == "Other (please describe)" ~ Q3_11_TEXT, # add answer from "other please describe
      TRUE ~ jobID)    ) -> df 

# extract just the data we want for preliminary analysis and for later grouping with 
# Q12, Q14, affordable housing, marketability, low development costs, affordable landscape management
surveyOne <- select(df, RecordedDate, IDcode, SurveyNum, jobID, Q12, Q14, Q13_2, Q13_6, Q13_7, Q13_9)

# group by different person types
# jobGrouped <- df %>%
#   group_by(jobID) %>%
#   summarise(total = n()) %>%
#   mutate(percentage = (total/sum(total))*100)
# write.csv(jobGrouped, file.path(outfolder, "jobIDs_S1.csv"), row.names = FALSE)


# this code combined two dataframes into one called "both"

# q12 question - including provisions for biodiversity is importan
both %>% 
  group_by(SurveyNum, Q12) %>%
  summarise(total = n()) %>%
  mutate(proportion = total/sum(total)) -> important

# q14 question - easy to include
b <- filter(both, !is.na(Q14)) # remove NAs
b %>% 
  group_by(SurveyNum, Q14) %>%
  summarise(total = n()) %>%
  mutate(proportion = total/sum(total)) -> easy

# Q13_2, affordable housing, 
b <- filter(both, !is.na(Q13_2)) # remove NAs
b %>% 
  group_by(SurveyNum, Q13_2) %>%
  summarise(total = n()) %>%
  mutate(proportion = total/sum(total)) -> affordable

# Q13_6 , marketability, 
b <- filter(both, !is.na(Q13_6)) # remove NAs
b %>% 
  group_by(SurveyNum, Q13_6) %>%
  summarise(total = n()) %>%
  mutate(proportion = total/sum(total)) -> marketable

# Q13_7,  low development costs,
b <- filter(both, !is.na(Q13_7)) # remove NAs
b %>% 
  group_by(SurveyNum, Q13_7) %>%
  summarise(total = n()) %>%
  mutate(proportion = total/sum(total)) -> devCost

# Q13_9 affordable landscape management
b <- filter(both, !is.na(Q13_9)) # remove NAs
b %>% 
  group_by(SurveyNum, Q13_9) %>%
  summarise(total = n()) %>%
  mutate(proportion = total/sum(total)) -> management
