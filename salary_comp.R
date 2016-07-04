# Author - Anupama Rajaram
# Data Visualization for US Salary


# To clean up the memory of your current R session run the following line
rm(list=ls(all=TRUE))

library(data.table)   # for fread() 
library(bit64)
library(dplyr)
library(ggplot2)
library(sqldf)
library(heatmaply)
library(plotly)
library(htmltools)
library(ggvis)

library('dplyr')      # for data manipulation
library('tidyr')      # for reshaping data

library('ggplot2')    # plotting data
library('scales')     # for scale_y_continuous(label = percent)
library('ggthemes')   # for scale_fill_few('medium')

library('ztable')     # format tables for reporting
library("maps")
library(leaflet)

options(digits=5)

options(scipen = 999)



# load data
saldata = fread("salary_data.csv")
# 167278 rows and 26 columns.

state_us = fread("C:/anu/data analytics/us_states_abbr.csv")



# ================================================== #
# ==== Descriptive statistics & Data Exploration === #
# ================================================== #
summary(saldata)

# looking for NAs
sapply(saldata, function(x) sum(is.na(x)))
# IGNORE the following variables:
# EDUCATION_LEVEL_REQUIRED ,  COLLEGE_MAJOR_REQUIRED, 
# EXPERIENCE_REQUIRED_NUM_MONTHS , COUNTRY_OF_CITIZENSHIP , 
# WORK_POSTAL_CODE, 

# check for unique values:
sapply(saldata, function(x) length(unique(x)))



# ================================================== #
# ================= Data Preparation =============== #
# ================================================== #


# Assign values to NAs for the following variables :

# variable 1:
saldata$FULL_TIME_POSITION_Y_N[is.na(saldata$FULL_TIME_POSITION_Y_N)] = "y"

# variable 2:
saldata$EXPERIENCE_REQUIRED_Y_N[is.na(saldata$EXPERIENCE_REQUIRED_Y_N)] = "y"

# variable 3:
saldata$PREVAILING_WAGE_PER_YEAR[is.na(saldata$PREVAILING_WAGE_PER_YEAR )] = 
  saldata$PAID_WAGE_PER_YEAR

# Delete the following columns as they have too many NAs:
saldata$EDUCATION_LEVEL_REQUIRED = NULL
saldata$COLLEGE_MAJOR_REQUIRED = NULL
saldata$EXPERIENCE_REQUIRED_NUM_MONTHS = NULL
saldata$COUNTRY_OF_CITIZENSHIP = NULL
saldata$WORK_POSTAL_CODE = NULL


# convert case date to date format:
saldata$case_date = as.Date(saldata$CASE_RECEIVED_DATE, "%m/%d/%Y")
saldata$caseyr = as.numeric(format(saldata$case_date, "%Y"))

# subsetting only the active cases and full-time employees only
activesal = subset(saldata,
                   CASE_STATUS == "certified"  &
                   FULL_TIME_POSITION_Y_N == "y"     )



state_us$WORK_STATE2 = tolower(state_us$State)

#statesal = merge(activesal, state_us, by = WORK_STATE2, ALL.X = TRUE )

#statesal =  merge(activesal, saldata, by.x = WORK_STATE2, all.x = TRUE)
stsal = sqldf("select count(*) as 'Case_count',
                  caseyr, avg(PAID_WAGE_PER_YEAR) as 'Annual_sal',
                  JOB_TITLE_SUBGROUP , WORK_STATE2
                 FROM activesal 
                 group by WORK_STATE2, caseyr, JOB_TITLE_SUBGROUP"   
                )

statewise_sal = subset(stsal, JOB_TITLE_SUBGROUP == "software engineer" 
                       & caseyr== 2015)
# join geographical coordinates from table state_us to the statewise salary 
# data from table statewise_sal
statewise_sal2 = sqldf("select statewise_sal.Case_count,
                      statewise_sal.WORK_STATE2,
                      statewise_sal.Annual_sal, 
                      state_us.Latitude, state_us.Longitude
                      FROM statewise_sal , state_us
                      where statewise_sal.WORK_STATE2  = state_us.WORK_STATE2")
colnames(statewise_sal2) = c( "case_count", "state", "avg_sal",
                              "latitude", "longitude")

# information on location markers
stateInfo <- paste("No. of cases = ", statewise_sal2[['case_count']], ", ",
                   "Salary = ", statewise_sal2[['avg_sal']], "<br>", ", ",
                     "State = ", statewise_sal2[['state']], "<br>", 
                   "<br>", 
                     sep='')
statewise_sal2$infos = stateInfo

# delstate = subset(statewise_sal, WORK_STATE2 == "delaware")


# plot function
gp1 = ggplot(delstate, aes(x = caseyr, y = Annual_sal,
                    color = JOB_TITLE_SUBGROUP)) + geom_line()
ggplotly(gp1)



gp2 <- delstate %>% 
  plot_ly(x = caseyr, y = Annual_sal, mode = "markers", 
          color = JOB_TITLE_SUBGROUP, 
          hoverinfo = "text",
          text = paste("Year = ", delstate$caseyr, "<br>",
                       " Job Subgroup = ", delstate$JOB_TITLE_SUBGROUP,"<br>",
                       " No. of Cases = ", delstate$Case_count, "<br>",
                       " Average annual salary = ", delstate$Annual_sal))  %>%
  layout(title = "Delaware Salary Breakdown")




p <- plot_ly(economics, x = date, y = uempmed, name = "unemployment")
p %>% add_trace(y = fitted(loess(uempmed ~ as.numeric(date))), x = date)


x = delstate %>% ggvis(x = ~caseyr, y = ~Annual_sal,
                   fill = ~JOB_TITLE_SUBGROUP ) 


library(maps)
map("state", interior = FALSE)
map("state", boundary = FALSE, col="gray", add = TRUE)




map_demog <- leaflet(statewise_sal2) %>% 
  setView(-93.65, 42.0285, zoom = 4) %>%
  addTiles() %>%
  addMarkers(~longitude, ~latitude, popup=~infos,
             options = popupOptions(closeButton = TRUE)
  )
map_demog
