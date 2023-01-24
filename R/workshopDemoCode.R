# code for running during the demonstration workshop
#### R Workshop ###

#Main Script Outline
#Tasks: 
# 1 Load packages 
# 2 Load data
# 3 remove data unneeded
# 4 cleaning data
#   a coding free text
#   b converting numbers to numbers
#   c dealing with dates
#   d na responses
#  e reverse coding
# 5 filtering data- pros and cons
# 6 grouping 
#Code that shows how to group responses by demographic
#Code that shows how to plot/visualise Likert scale data
#Code that shows how to pull out numbers from a multi-response question
# 7 summary analyses
# 8 plotting

###LOAD PACKAGES### 
library(tidyverse)
library(ggplot2)
library(RColorBrewer)

###LOAD DATA### 
df <- read_csv(here("R Workshop Connection to Nature Survey_May 17, 2022_21.12")) %>%
  
  # Set working directory
  setwd()

###REMOVE UNWANTED DATA###
#Remove unnecessary data and filter unwanted responses

# Remove rows 3 to 58 of data which are unneeded headings and test/preview responses
slice(-3:-58)

# Filter incomplete records
df <- filter(df, df$Finished == "True")

# Filter agree to participate responses Q1
df <- filter(df, df$Q1 == "Agree")

#Filter those responses in Q14 "I never visit the City of Melbourne", "I rarely visit the City of Melbourne (e.g. once or twice per year)"
df <- filter(df, df$Q14 == "I live in the City of Melbourne", "I live outside the City of Melbourne but generally commute to the City of Melbourne for work", "I do not live or work in the City of Melbourne but normally I visit often (e.g. on average twice or more times a month)")

# Filter attention check response Q47
df <- filter(df, df$Q47 == "Your connection to nature in the City of Melbourne")

# Remove junk columns
df <- select(df, -c(1:11))

###CLEAN DATA###

###ORGANISE DATA###


###SUMMARISE DEMOGRAPHICS###

#Postcode Q9 map?

#Age Q11 

#Community member type: resident, commuter, visitor Q14

#Gender Q13

#Time in Melbourne area Q46

#Country of origin Q78

#Second language Q48	yes/no

#Second language Q49 type + other

#Education Q50

#Income Q51

#Employment status Q52

###SCALE ITEM QUESTIONS###

#Connection to nature Q19_1:Q19_12

#Barriers to nature engagement Q21_1:Q21_14

#Conservation behaviours Q28_15:Q28_15

#Perceptions of amount of nature within the City of Melbourne #Q16


####Q19 what to do when you have answers separated by commas?
####Q17 what to do about qualitative data? 
###Q19_11_TEXT what about text entry questions?

###VISUALISE DATA####

#Plot connection to nature (mean for response) percentage per average Q19_1:Q19_12

#Plot barriers stacked horizontal bar chart percentage  Q21_1:Q21_14

#Plot conservation behaviours stacked horizontal bar chart percentage Q28_15:Q28_15

#Plot amount of nature stacked horizontal bar chart percentage #Q16

###FURTHER ANALYSIS###

install.packages("MASS")
install.packages("psych")

##FACTOR ANALYSIS##

fa

##SCALE RELIABILITY ANALYSIS###
alpha
omega
