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

# create personal code for participants
# convert month to number
df %>%
  mutate(
    monthNum = case_when(
      Q2_1 == "January" ~ "1",
      Q2_1 == "February" ~ "2", 
      Q2_1 == "March" ~ "3",
      Q2_1 == "April" ~ "4",
      Q2_1 == "May" ~ "5",
      Q2_1 == "June" ~ "6",
      Q2_1 == "July" ~ "7",
      Q2_1 == "August" ~ "8",
      Q2_1 == "September" ~ "9",
      Q2_1 == "October" ~ "10",
      Q2_1 == "November" ~ "11",
      Q2_1 == "December" ~ "12")) -> df
# combine month number with 
df$IDcode <- paste0(df$monthNum, df$Q2_2, df$Q2_3)
df$SurveyNum <- 1
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


# survey 2 data
#SURVEY 1
# load & prep the data survey 1
df2 <- read_csv(here("data/rawData/BSUD workshop survey 2_November 7, 2021_02.15.csv")) %>%
  # remove first 2 rows of data which are nonsense
  slice(-1, -2)
# filter incomplete records
df2$Progress <- as.numeric(df2$Progress)
df2 <- filter(df2, df2$Progress > 27)

# remove junk columns
df2 <- select(df2, -c(1:7))
df2 <- select(df2, -c(2:10))
# remove data from before workshops
# convert to date
df2 %>%
  # convert date column to a date
  mutate(RecordedDate = as.Date(RecordedDate)) %>%
  # filter to remove all from before workshops
  filter(RecordedDate >= as.Date("2021-04-28")) -> df2

# create personal code for participants
# convert month to number
df2 %>%
  mutate(
    monthNum = case_when(
      Q2_1 == "January" ~ "1",
      Q2_1 == "February" ~ "2", 
      Q2_1 == "March" ~ "3",
      Q2_1 == "April" ~ "4",
      Q2_1 == "May" ~ "5",
      Q2_1 == "June" ~ "6",
      Q2_1 == "July" ~ "7",
      Q2_1 == "August" ~ "8",
      Q2_1 == "September" ~ "9",
      Q2_1 == "October" ~ "10",
      Q2_1 == "November" ~ "11",
      Q2_1 == "December" ~ "12")) -> df2
# combine month number with 
df2$IDcode <- paste0(df2$monthNum, df2$Q2_2, df2$Q2_3)
df2$SurveyNum <- 2
# remove the extra answers in the person type column
df2 %>%
  mutate(
    jobID = str_remove(Q3, ",.*$"), #regex expression to remove stuff after commas in the string
    jobID = case_when(
      Q3 == "Other (please describe)" ~ Q3_11_TEXT, # add answer from "other please describe
      TRUE ~ jobID)    ) -> df2 

# extract just the data we want for preliminary analysis and for later grouping with 
# Q12, Q14
surveyTwo <- select(df2, RecordedDate, IDcode, SurveyNum, jobID, Q12, Q14, Q13_2, Q13_6, Q13_7, Q13_9)

# what proportion of person types?

# group by different person types
# jobGrouped <- df2 %>%
#   group_by(jobID) %>%
#   summarise(total = n()) %>%
#   mutate(percentage = (total/sum(total))*100)
# write.csv(jobGrouped, file.path(outfolder, "jobIDs_S2.csv"), row.names = FALSE)

both <- rbind(surveyOne, surveyTwo)

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



#easy plotting
# create a new variable A, B,C,D
easy %>%
  mutate(
    ansOrd = case_when(
      Q14 == "Strongly disagree" ~ "A",
      Q14 == "Somewhat disagree" ~ "B",
      Q14 == "Neither agree nor disagree" ~ "C",
      Q14 == "Somewhat agree" ~ "D",
      Q14 == "Strongly agree" ~ "E")) -> easy

p <- ggplot(easy, aes(fill = ansOrd, y = proportion, x = SurveyNum)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = c("darkslategray", "darkgray", "azure2", "chartreuse4", "darkgreen"), 
                    labels=c("Strongly disagree", "Somewhat disagree", "Neither agree nor disagree", "Somewhat agree", "Strongly agree")) +
  scale_y_reverse() +
  scale_x_reverse() +
  theme_minimal() +
  coord_flip() +
  theme(axis.text.x = element_text(size = 12)) +
  theme(axis.text.y = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.title = element_blank(), legend.position = "bottom")
f <- paste0(figFolder, "/easy2.png")
ggsave(filename = f, plot = p, width = 9, height = 5, dpi = 600)



#easy plotting
# create a new variable A, B,C,D
important %>%
  mutate(
    ansOrd = case_when(
      Q12 == "Neither agree nor disagree" ~ "C",
      Q12 == "Somewhat agree" ~ "D",
      Q12 == "Strongly agree" ~ "E")) -> important

p <- ggplot(important, aes(fill = ansOrd, y = proportion, x = SurveyNum)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = c("azure2", "chartreuse4", "darkgreen"), 
                    labels=c("Neither agree nor disagree", "Somewhat agree", "Strongly agree")) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.title = element_blank())
f <- paste0(figFolder, "/important.png")
ggsave(filename = f, plot = p, width = 5, height = 9, dpi = 600)


#easy plotting
# create a new variable A, B,C,D
management %>%
  mutate(
    ansOrd = case_when(
      Q13_9 == "Somewhat incompatible" ~ "B",
      Q13_9 == "Neither" ~ "C",
      Q13_9 == "Somewhat compatible" ~ "D",
      Q13_9 == "Very compatible" ~ "E")) -> management

p <- ggplot(management, aes(fill = ansOrd, y = proportion, x = SurveyNum)) + 
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = c("darkgray", "azure2", "chartreuse4", "darkgreen"), 
                    labels=c("Somewhat incompatible", "Neither", "Somewhat compatible", "Very compatible")) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_blank()) +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
  theme(legend.title = element_blank())
f <- paste0(figFolder, "/management.png")
ggsave(filename = f, plot = p, width = 5, height = 9, dpi = 600)


people <- left_join(surveyOne, surveyTwo, by = "IDcode")
people <- filter(people, !is.na(SurveyNum.y))
# write.csv(people, file.path(outfolder, "people.csv"), row.names = FALSE)

people %>%
  mutate(
    ans1 = case_when(
      Q14.x == "Strongly disagree" ~ "1",
      Q14.x == "Somewhat disagree" ~ "2",
      Q14.x == "Neither agree nor disagree" ~ "3",
      Q14.x == "Somewhat agree" ~ "4",
      Q14.x == "Strongly agree" ~ "5"),
    ans2 = case_when(
      Q14.y == "Strongly disagree" ~ "1",
      Q14.y == "Somewhat disagree" ~ "2",
      Q14.y == "Neither agree nor disagree" ~ "3",
      Q14.y == "Somewhat agree" ~ "4",
      Q14.y == "Strongly agree" ~ "5")) -> people

people %>%
  mutate(
    ans1 = as.numeric(ans1),
    ans2 = as.numeric(ans2)) -> people

ggplot(aes(x = ))


ggparcoord(people,
           columns = 20:21,
           showPoints = TRUE, 
           title = "Before and after BSUD workshop",
           alphaLines = 0.3) + 
  geom_jitter()+
  theme_minimal()

