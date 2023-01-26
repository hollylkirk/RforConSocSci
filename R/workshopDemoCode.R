# code for running during the demonstration workshop

### LOAD PACKAGES ### 
# if you don't have the package installed run the following code:
# install.packages("name of package")

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(paletteer)
# library(here)

# tell R where to store files that you create in this script
outfolder <- here("C:/Users/ilex0/OneDrive/AARR/RforConSocSci/data/derivedData/")
figFolder <- here("C:/Users/ilex0/OneDrive/AARR/RforConSocSci/figs")


### LOAD DATA ### 
# df <- read_csv(here("data/rawData/Connection to Nature Survey R Workshop.csv"))
#OR
# getwd()
df <- read_csv("C:/Users/ilex0/OneDrive/AARR/RforConSocSci/data/rawData/Connection to Nature Survey R Workshop.csv")
# look at the data using 
# head(df)




###CLEAN & FILTER DATA###
#Remove unnecessary data, nonsense data and filter unwanted responses

# Remove rows 1 to 58 of data which are unneeded headings and test/preview responses
filtdf <- slice(df, -(1:58))

# use the '$' symbol to specify a particular variable
# head(df$Q14)

# Filter incomplete records using the 'filter' function
filtdf <- filter(filtdf, filtdf$Finished == "TRUE") #!
# Filter agree to participate responses Q1
filtdf <- filter(filtdf, filtdf$Q1 == "Agree")
#Filter out those responses in Q14 where people have not been to CoM
# e.g "I never visit the City of Melbourne", "I rarely visit the City of Melbourne (e.g. once or twice per year)"
filtdf <- filter(filtdf, filtdf$Q14 == "I live in the City of Melbourne" |
                   filtdf$Q14 == "I live outside the City of Melbourne but generally commute to the City of Melbourne for work" |
                   filtdf$Q14 == "I do not live or work in the City of Melbourne but normally I visit often (e.g. on average twice or more times a month)")
# Filter attention check response Q47
filtdf <- filter(filtdf, filtdf$Q47 == "Your connection to nature in the City of Melbourne")
# Remove junk columns using the 'select' function
filtdf <- select(filtdf, -c(1:11))


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
# remove(filtdf)

# its better to select only the data columns you want to use
# now is the time when you can re-order columns if you would like to
newData <- select(filtered, c(1:5), Q48, Q50, Q16...17, Q19, Q15, Q28_4,
                  Q23_1, Q23_2, Q23_3, Q23_4, Q23_5, Q23_6, Q23_7, Q23_8)
# remove two extra columns that are hard to name, using the '-'
newData <- select(newData, - Q13, -Q13_3_TEXT)


# example code to remove data from before a survey was live
# you need to convert the data type from 'character' to a 'date' 
# using the 'mutate' function
# exampleData %>%
#   # convert date column to a date
#   mutate(RecordedDate = as.Date(RecordedDate)) %>%
#   # filter to remove all from before workshops
#   filter(RecordedDate >= as.Date("2021-04-28")) -> newData


# let's rename the variables, use the "rename" function
newData <- rename(newData, postCode = Q9,
                  ageCat = Q11,
                  melbLife  = Q14,
                  language = Q48,
                  education = Q50,
                  natureChange = Q15,
                  natureActive = Q19,
                  paperRecycle = Q28_4, 
                  natureFeel = Q16...17)

# create bigger age group categories using case_when() and the "mutate" function
newData <- newData %>%
  mutate(
    ageBin = case_when(
      ageCat == "18-20" ~ "18-30",
      ageCat == "21-30" ~ "18-30",
      ageCat == "31-40" ~ "31-50",
      ageCat == "41-50" ~ "31-50",
      ageCat == "51-60" ~ "51-70",
      ageCat == "61-70" ~ "51-70",
      ageCat == "71 or older" ~ "70+"))

# lets make a numeric age category
# what's in the age categories? unique(shortDf$ageCat)
# we can use the "mutate" function which is REALL useful for data manipulation
newData <- newData %>%
  mutate(
    ageNum = case_when(
      ageCat == "18-20" ~ 18,
      ageCat == "21-30" ~ 21,
      ageCat == "31-40" ~ 31,
      ageCat == "41-50" ~ 41,
      ageCat == "51-60" ~ 51,
      ageCat == "61-70" ~ 61,
      ageCat == "71 or older" ~ 71))


# data columns that might need changing type/ or cleaning up?
# the nature activity column... what does it look like?
# head(newData) or newData$natureActive
# there are some NAs - need to think about how these are handled, e.g.
# !is.na(), na.omit, is.na() etc.
# there are answers separated by commas YUCK! 
# let's make that easier to understand using mutate and "grepl" function
newData <- newData %>% 
  mutate(
    melbRBG = grepl("Visiting the Royal Botanic Gardens", natureActive),
    melbZoo = grepl("Visiting the Melbourne Zoo", natureActive),
    melbAqua = grepl("Visiting the Melbourne Aquarium", natureActive),
    walk = grepl("Taking a nature walk within the City of Melbourne", natureActive),
    feeding = grepl("Feed urban wildlife within the City of Melbourne (e.g. birds, possums)", natureActive),
    watching = grepl("Bird watching, or other nature watching within the City of Melbourne", natureActive),
    other = grepl("Other (Please specify)", natureActive))



### SUMMARISE & GROUP data ###

# use the "group_by", "summarise" and mutate functions
# here I create a new small summary data frame 
#Age group by ages
ageGrouped <- newData %>%
  group_by(ageBin) %>% #group by the ageBin Column
  summarise(total = n()) %>% # 'n' counts the number of individuals in each group
  mutate(percentage = (total/sum(total))*100) %>% # add another column calculating the %
  rename(demoQuestion = ageBin) # rename the demographic variable
# save this as separate csv file
# I tell it the file path we specified earlier, plus the name of the file
write.csv(ageGrouped, file.path(outfolder, "ageSummary.csv"), row.names = FALSE)


#Community member type: resident, commuter, visitor 
residency <- newData %>%
  group_by(melbLife) %>%
  summarise(total = n()) %>%
  mutate(percentage = (total/sum(total))*100) %>% 
  rename(demoQuestion = melbLife)

#Education
edu <- newData %>%
  group_by(education) %>%
  summarise(total = n()) %>%
  mutate(percentage = (total/sum(total))*100) %>% 
  rename(demoQuestion = education)

# combine these three dataframes together to save them externally
demographSummary <- bind_rows(ageGrouped, residency, edu)
# save this as separate csv file
write.csv(demographSummary, file.path(outfolder, "demographSummary.csv"), row.names = FALSE)

# lets clear the environment a little:
remove(ageGrouped, demographSummary, edu, residency)



### MAIN DATA QUESTIONS###
# sometimes the easiest way to analyse some of these questions is to make smaller dataframes 
# with just the variables we want in them

# summarising the recycling data by age
recycle <- newData %>%
  group_by(ageBin, paperRecycle) %>% # you can group by more than one column
  summarise(total = n()) %>%
  mutate(proportion = (total/sum(total)))


#summarising the activity data 
activities <- newData %>% 
  group_by(melbLife) %>% 
  summarise(propRBG = mean(melbRBG),
            propZoo = mean(melbZoo),
            propAqua = mean(melbAqua),
            propWalk = mean(walk),
            propFeed = mean(feeding),
            propWatch = mean(watching))
write.csv(activities, file.path(outfolder, "natureActivitiesByMelbLife.csv"), row.names = FALSE)


# adding a few summary variables to the dataframe for some questions
#Perceptions of amount of nature within the City of Melbourne
# we can group the perceptions into two types using the in operator for speed 
newData <- newData %>%
  mutate(
    naturefeelTooMuch = natureFeel %in% c("Moderately too much", 
                                          "Far too much", 
                                          "Slightly too much"),
    naturefeelTooLittle = !naturefeelTooMuch) # use the ! to indicate NOT

# another way to summarise data is to create a numeric variable
# conservation perspectives (Agree-disagree) - give numeric values 
# to a latent construct
newData <- newData %>%
  mutate(
    Q1 = case_when(
      Q23_1 == "Strongly disagree" ~ 1,
      Q23_1 == "Disagree" ~ 2,
      Q23_1 == "Somewhat disagree" ~ 3,
      Q23_1 == "Neither agree nor disagree" ~ 4,
      Q23_1 == "Somewhat agree" ~ 5,
      Q23_1 == "Agree" ~ 6,
      Q23_1 == "Strongly agree" ~ 7),
    Q2 = case_when(
      Q23_2 == "Strongly disagree" ~ 1,
      Q23_2 == "Disagree" ~ 2,
      Q23_2 == "Somewhat disagree" ~ 3,
      Q23_2 == "Neither agree nor disagree" ~ 4,
      Q23_2 == "Somewhat agree" ~ 5,
      Q23_2 == "Agree" ~ 6,
      Q23_2 == "Strongly agree" ~ 7),
    Q3 = case_when(
      Q23_3 == "Strongly disagree" ~ 1,
      Q23_3 == "Disagree" ~ 2,
      Q23_3 == "Somewhat disagree" ~ 3,
      Q23_3 == "Neither agree nor disagree" ~ 4,
      Q23_3 == "Somewhat agree" ~ 5,
      Q23_3 == "Agree" ~ 6,
      Q23_3 == "Strongly agree" ~ 7))

# create a mean of this value
newData$meanMelbConnect <- rowMeans(newData[,27:29], na.rm=TRUE)



### VISUALISE DATA####
# simple barplot
# shorten labels
activities <- activities %>%
  mutate(
    shortAns = case_when(
      melbLife == "I do not live or work in the City of Melbourne but normally I visit often (e.g. on average twice or more times a month)" ~ "Vist often",
      melbLife == "I live in the City of Melbourne" ~ "Live in the city",
      melbLife == "I live outside the City of Melbourne but generally commute to the City of Melbourne for work" ~ "Work in the City",))
# code for the barplot
ggplot(activities, aes(shortAns, propRBG)) +
  geom_bar(stat = "identity", width = 0.6, fill = "grey") +
  labs(title = "Poportion of respondents that visit RBG")  +
  theme_minimal() +
  scale_fill_manual(values = "grey") +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.text.x = element_text(size = 12, angle = 45, hjust = 0.9)) +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank())

# stacked barplot
library(viridis)
library(hrbrthemes)
#needs to be long
act <- pivot_longer(activities, cols = c(2:7), names_to = "Activity", values_to = "Proportion")

# plotting code
p <- ggplot(act, aes(fill = shortAns, y = Proportion, x = Activity)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() +
  coord_flip() +
  xlab("")
p
# saving plot externally
f <- paste0(figFolder, "/activities.png")
ggsave(filename = f, plot = p, width = 9, height = 5, dpi = 600)



# plotting numeric data
#Plot connection to nature (mean for response)
hist(newData$meanMelbConnect)
# plot ages against mean score
plot(newData$ageNum, newData$meanMelbConnect)
# density plot by age bins
d <- density(newData$meanMelbConnect) # returns the density data
plot(d) # plots the results

ggplot(data = newData, aes(x = ageBin, y = meanMelbConnect, fill = ageBin, colour = ageBin)) +
  geom_violin(width = 0.5, linewidth = 0.3) +  # violin geom, use ?geom_violin in console to get help file info on what width and size do
  scale_fill_paletteer_d("wesanderson::Moonrise1") + # fill using the Moonrise Kingdom palette No1
  scale_color_paletteer_d("wesanderson::Moonrise1") + # border colour matches
  stat_summary(aes(y = meanMelbConnect), fun = "median", geom ="point", size = 2, colour = "black") + # plot  a median point
  theme_minimal()+  # different theme here
  theme(legend.position = "none") + # remove the legend
  coord_flip() # horizontal rather than vertical plot





# # stacked bar plotting demo
# # just the columns we need from newDAta
# natureInCity <- select(newData, c(1:5), natureChange)
# natureInCity <- filter(natureInCity, natureInCity$natureChange != "Unsure")
# 
# natureSummary <-natureInCity %>%
#   group_by(natureChange) %>%
#   summarise(total = n()) %>%
#   mutate(proportion = total/sum(total)) -> easy
# 
# # create a new variable A, B,C,D
# natureSummary <- natureSummary %>%
#   mutate(
#     ansOrd = case_when(
#       natureChange == "Less" ~ "A",
#       natureChange == "About the same" ~ "B",
#       natureChange == "More" ~ "C"))
# 
# # plotting code
# ggplot(natureSummary, aes(fill = ansOrd, y = proportion, x = ansOrd)) +
#   geom_bar(position = "stack", stat = "identity") 
#   # scale_fill_manual(values = c("darkslategray", "grey", "darkgreen"),
#   #                   labels=c("Less", "About the same", "More")) +
#   # theme_minimal() +
#   # coord_flip() +
#   # theme(axis.text.x = element_text(size = 12)) +
#   # theme(axis.text.y = element_blank()) +
#   # theme(axis.title.x = element_blank(), axis.title.y = element_blank()) +
#   # theme(legend.title = element_blank(), legend.position = "bottom")
# 
# 
# # saving plot externally
# f <- paste0(figFolder, "/easy2.png")
# ggsave(filename = f, plot = p, width = 9, height = 5, dpi = 600)





# Things we might not have time to discuss
# Postcode Q9 map?
# What to do about qualitative data? (Q17)
# What about text entry questions? (Q19_11_TEXT)
# str_remove(Q3, ",.*$"),


###FURTHER ANALYSIS###

install.packages("MASS")
install.packages("psych")

##FACTOR ANALYSIS##

fa

##SCALE RELIABILITY ANALYSIS###
alpha
omega
