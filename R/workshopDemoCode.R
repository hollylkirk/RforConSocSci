# code for running during the demonstration workshop

### LOAD PACKAGES ### 
# if you don't have the package installed run the following code:
# install.packages("name of package")

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
# library(here)


### LOAD DATA ### 
# df <- read_csv(here("data/rawData/Connection to Nature Survey R Workshop.csv"))
#OR
# getwd()
df <- read_csv("C:/Users/ilex0/OneDrive/AARR/RforConSocSci/data/rawData/Connection to Nature Survey R Workshop.csv")
# look at the data using 
# head(df)




###CLEAN & FILTER DATA###
#Remove unnecessary data, nonsense data and filter unwanted responses

# Remove rows 3 to 58 of data which are unneeded headings and test/preview responses
filt <- slice(df, -(1:58))
# use the $ symbol to specify a particular variable
# head(df$Q14)

# Filter incomplete records
filt <- filter(filt, filt$Finished == "TRUE") #!
# Filter agree to participate responses Q1
filt <- filter(filt, filt$Q1 == "Agree")
#Filter out those responses in Q14 where people have not been to CoM
# e.g "I never visit the City of Melbourne", "I rarely visit the City of Melbourne (e.g. once or twice per year)"
filt <- filter(filt, filt$Q14 == "I live in the City of Melbourne" |
                 filt$Q14 == "I live outside the City of Melbourne but generally commute to the City of Melbourne for work" |
                 filt$Q14 == "I do not live or work in the City of Melbourne but normally I visit often (e.g. on average twice or more times a month)")
# Filter attention check response Q47
filt <- filter(filt, filt$Q47 == "Your connection to nature in the City of Melbourne")
# Remove junk columns
filt <- select(filt, -c(1:11))


# alternative syntax that might be useful - PIPES %>% 
filtered <- df %>% 
  slice(-(1:58)) %>% 
  filter(Finished == "TRUE") %>% 
  filter(Q1 == "Agree") %>% 
  filter(Q14 == "I live in the City of Melbourne" |
           Q14 == "I live outside the City of Melbourne but generally commute to the City of Melbourne for work" |
           Q14 == "I do not live or work in the City of Melbourne but normally I visit often (e.g. on average twice or more times a month)") %>% 
  filter(Q47 == "Your connection to nature in the City of Melbourne") %>% 
  select(-c(1:11))

# you can remove unwanted objects from the environment
# remove(filt)


# example code to remove data from before a survey was live
# convert to date
# example %>%
#   # convert date column to a date
#   mutate(RecordedDate = as.Date(RecordedDate)) %>%
#   # filter to remove all from before workshops
#   filter(RecordedDate >= as.Date("2021-04-28")) -> newData


# its better to select only the data columns you want to use
# Q9, Q11, Q14, Q48, Q50, Q15, Q19, Q24_1 to Q24_8, Q28_4
shortDf <- select(filtered, c(1:6), Q48, Q50, Q15, Q19, Q28_4,
                  Q24_1, Q24_2, Q24_2, Q24_3, Q24_4)
shortDf <- select(shortDf, -c(4,5))

# let's rename the variables
shortDf <- rename(shortDf, post = Q9,
                  ageCat = Q11,
                  melbLife  = Q14,
                  language = Q48,
                  education = Q50,
                  natureYears = Q15,
                  natureActive = Q19,
                  paperRecycle = Q28_4)
shortDf <- rename(shortDf, natureFeel = Q16...17)

# group ages using case_when()
shortDf %>%
  mutate(
    ageBin = case_when(
      ageCat == "18-20" ~ "18-30",
      ageCat == "21-30" ~ "18-30",
      ageCat == "31-40" ~ "31-50",
      ageCat == "41-50" ~ "31-50",
      ageCat == "51-60" ~ "51-70",
      ageCat == "61-70" ~ "51-70",
      ageCat == "71 or older" ~ "70+")) -> shortDf

# lets make a numeric age category
# what's in the age categories? unique(shortDf$ageCat)
# we can use the "mutate" function which is REALL useful for data manipulation
shortDf %>%
  mutate(
    ageNum = case_when(
      ageCat == "18-20" ~ 18,
      ageCat == "21-30" ~ 21,
      ageCat == "31-40" ~ 31,
      ageCat == "41-50" ~ 41,
      ageCat == "51-60" ~ 51,
      ageCat == "61-70" ~ 61,
      ageCat == "71 or older" ~ 71)) -> shortDf


# data that might need changing type/ or cleaning up?
# the nature activity column... what does it look like?
# there are some NAs - need to think about how these are handled, e.g.
# !is.na(), na.omit, is.na() etc.
# there are answers separated by commas YUCK!
shortDf %>% 
  mutate(
    melbRBG = grepl("Visiting the Royal Botanic Gardens", natureActive),
    melbZoo = grepl("Visiting the Melbourne Zoo", natureActive),
    melbAqua = grepl("Visiting the Melbourne Aquarium", natureActive),
    walk = grepl("Taking a nature walk within the City of Melbourne", natureActive),
    feeding = grepl("Feed urban wildlife within the City of Melbourne (e.g. birds, possums)", natureActive),
    watching = grepl("Bird watching, or other nature watching within the City of Melbourne", natureActive),
    other = grepl("Other (Please specify)", natureActive)) -> shortDf



### SUMMARISE & GROUP data ###

#Age group by ages
ageGrouped <- shortDf %>%
  group_by(ageBin) %>%
  summarise(total = n()) %>%
  mutate(percentage = (total/sum(total))*100)

#Community member type: resident, commuter, visitor 
residency <- shortDf %>%
  group_by(melbLife) %>%
  summarise(total = n()) %>%
  mutate(percentage = (total/sum(total))*100)

#Education
edu <- shortDf %>%
  group_by(education) %>%
  summarise(total = n()) %>%
  mutate(percentage = (total/sum(total))*100)




###SCALE ITEM QUESTIONS###

#Conservation behaviours - paper recycle


#Perceptions of amount of nature within the City of Melbourne
shortDf %>%
  mutate(
  naturefeelTooMuch = natureFeel %in% c("Moderately too much", 
                                        "Far too much", 
                                        "Slightly too much"),
  naturefeelTooLittle = natureFeel %in% c("Moderately too little", 
                                          "Far too little",
                                          "Slightly too little", 
                                          "Neither too much nor too little")) -> shortDf


# conservation perspectives (Agree-disagree) give numeric value
shortDf %>%
  mutate(
  naturefeelvalue = case_when(
    naturefeel %in% blah ~ 1)) -> shortDf


###VISUALISE DATA####

#Plot connection to nature (mean for response) percentage per average
# plot ages against mean score



#Plot amount of nature stacked horizontal bar chart percentage

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





# Things we might not have time to discuss
# Postcode Q9 map?
# What to do about qualitative data? (Q17)
# What about text entry questions? (Q19_11_TEXT)




###FURTHER ANALYSIS###

install.packages("MASS")
install.packages("psych")

##FACTOR ANALYSIS##

fa

##SCALE RELIABILITY ANALYSIS###
alpha
omega
