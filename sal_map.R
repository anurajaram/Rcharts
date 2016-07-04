
# Author - Anupama Rajaram
# Data Visualization for US Visa-Salary dataset (high-skilled immigrants only)



# ================================================== #
# ============== Prepare the Workspace ============= #
# ================================================== #

# Start by cleaning up memory of current R session:
rm(list=ls(all=TRUE))

library(data.table)   # for fread() 

library(sqldf)        # so we can use SQL queries within R

library(htmltools)    # to create formatted strings for popup-info.

# data visualization libraries
library(ggvis)
library(dplyr)
library(ggplot2)
library(plotly)
library('dplyr')      # for data manipulation
library("maps")
library(leaflet)      # creation of state-level and city-level interactive maps


# I prefer <5 decimal places
options(digits=4)

# suppress scientific notation e.g.: 2.4e5 instead 2,400,367 
# This helps with salary values (eg. law firms in NYC).
options(scipen = 999)   



# load data - do not add the clause "STRINGSASFACTOR = TRUE"
saldata = fread("salary_data.csv")  # 167278 rows and 26 columns.


state_us = fread("C:/anu/data analytics/us_states_abbr.csv")
state_us$WORK_STATE2 = tolower(state_us$State)





# ================================================== #
# ================= Data Preparation =============== #
# ================================================== #

summary(saldata)    ## gives information regarding the quantiles for each column/variable.


# looking for NAs
sapply(saldata, function(x) sum(is.na(x)))
# Based on output, we can IGNORE the following variables:
# EDUCATION_LEVEL_REQUIRED ,  COLLEGE_MAJOR_REQUIRED, 
# EXPERIENCE_REQUIRED_NUM_MONTHS , COUNTRY_OF_CITIZENSHIP , 
# WORK_POSTAL_CODE, 

# check for unique values:
sapply(saldata, function(x) length(unique(x)))


# ==============================================================
# Assign values to missing cells for the following variables :
# ==============================================================

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



# ==============================================================
# convert case-date to date format:
# ==============================================================
saldata$case_date = as.Date(saldata$CASE_RECEIVED_DATE, "%m/%d/%Y")
saldata$caseyr = as.numeric(format(saldata$case_date, "%Y"))

# We see both "district of Columbia" and "dc" as separate work states, 
# so we modify both to refelct as "dc"
saldata$WORK_STATE2[saldata$WORK_STATE2 == "district of columbia"] <- "dc"




# subsetting only the active cases and full-time employees only
activesal = subset(saldata,
                     CASE_STATUS == "certified"  &
                     FULL_TIME_POSITION_Y_N == "y"     )


# ==============================================================
# join state coordinates and activesal
# ==============================================================
sal = sqldf("select activesal.EMPLOYER_NAME, activesal.JOB_TITLE,
              activesal.WORK_STATE2, activesal.VISA_CLASS,
              activesal.PAID_WAGE_PER_YEAR, 
              activesal.JOB_TITLE_SUBGROUP, activesal.caseyr,
              state_us.Latitude, state_us.Longitude
              FROM activesal , state_us
              where activesal.WORK_STATE2  = state_us.WORK_STATE2")
colnames(sal) = c( "employer", "job_title", "state", 
                              "visa_class", "salary", "job_subgroup", 
                              "year", "latitude", "longitude")





# ================================================== #
# ========== Graphical Data Exploration ============ #
# ================================================== #

# Graph 1:
# ===========
# Number of high-skilled visa applications per year
setct = sqldf("select year, count(*) as 'count',
              visa_class as 'class',
              job_subgroup as 'title'
              from sal
              group by year, job_subgroup, visa_class")
setct %>% ggvis(~year, ~count ) %>% layer_bars()



# Graph 2:
# ===========
# visualize number of visa applications by year by visa type.
ggplot(setct, aes(x = year, y = count, fill = class)) + 
  geom_bar(stat = "identity")


# Graph 3:
# ===========
# visualize number of visa applications by year by job title.
ggplotly(ggplot(setct, aes(x = year, y = count, fill = title)) + 
           geom_bar(stat = "identity"))



# Graph 4:
# ===========
# Get information on the BEST Employers by State
setctstate = sqldf("select count(*) as 'count',
              EMPLOYER_NAME as 'employer',
              WORK_STATE2 as 'state',
              avg(PAID_WAGE_PER_YEAR) as 'salary'
              from activesal
              group by EMPLOYER_NAME, WORK_STATE2")
#top_setctstate = setctstate[with(setctstate, order(state, -count, -salary)),]
top_setctstate = setctstate[with(setctstate, order( -count)),]

# graph 4a:
## Employers with highest number of applications submitted
top_setctstate[1:10,]   

# graph 4b:
## Employers with highest number of applications submitted
# only considering verage salaries >100k
tpx = subset(top_setctstate, salary > 100000)
tpx = tpx[with(tpx, order(-count, -salary)),]
tpx[1:10, ]

# graph 4c:
# visa applications with highest salary.
# many are law firms, which probably explains the million $ ranges! 
topsal = setctstate[with(setctstate,order(-salary)),]
topsal[1:10,]




# Graph 5:
# ===========
# Checking how the software engineers fared! :)
# A topic (obviously) close to my heart! 

setstatesal = sqldf("select caseyr as 'year', 
                    count(*) as 'count',
                    JOB_TITLE_SUBGROUP as 'title',
                    WORK_STATE2 as 'state',
                    max(PAID_WAGE_PER_YEAR) as 'maxsal',
                    avg(PAID_WAGE_PER_YEAR) as 'avgsal',
                    min(PAID_WAGE_PER_YEAR) as 'minsal'
                    from activesal
                    group by caseyr, JOB_TITLE_SUBGROUP, WORK_STATE2 ")
setstatesal$y25 = setstatesal$avgsal*0.75
setstatesal$y75 = setstatesal$maxsal*0.75


sengr_statesal = subset(setstatesal, title == "software engineer"
                        & year == 2014)
#sengr_statesal2 <- sengr_statesal[order(avgsal)]
sengr_statesal2 <- sengr_statesal[with(sengr_statesal, order(-avgsal)),]
sengr_statesal2$avg = round(sengr_statesal2$avgsal)


# graph 5a:
# visualize states with highest avg salary for software engineers.
ggplot(sengr_statesal2[1:10,], aes(x=state, ymin=minsal, lower=y25, 
                           middle=avgsal, upper=y75, ymax=maxsal)) +
    geom_boxplot(stat="identity", fill="#53cfff") + 
  geom_text(aes(x=state, y=y75, ymax = maxsal, hjust = 0.95,
                label=paste0("$", avg)), size=4) + 
  theme_light(base_size=16) +
  theme(axis.text.y = element_text(hjust=0, color="black")) +
  coord_flip() +
  xlab("Average Salary") + ylab("State") +
  ggtitle("Top 10 US States with Highest Average Salary for Software Engineers")

# graph 5b:
# visualize states with highest avg salary for software engineers.
# we notice the point markers are not as emphatic as the boxplot above!
gp1 <- ggplot(sengr_statesal2[1:10,], aes(x = avg, y = state)) + geom_point()
 ggplotly(gp1)


 
 

# Graph 6:
# ===========
# salary by year and avgsal
# attorneys overall, make the highest salaries!
# although 2012 was the year of the software engineer! :)
setstatesal %>% 
   plot_ly(x = year, y =avgsal, type = "bar", color = title)






# ===============================================================
# Graph 6: Interactive Map using Leaflet
# ===============================================================
# create interactive map to show Top employers in every state 
# based on salary on visa applications 
emp_sal = sqldf("select caseyr as 'year', 
                   EMPLOYER_NAME as 'Employer',
                   count(CASE_NUMBER) as 'count',
                   WORK_STATE2 as 'state',
                   avg(PAID_WAGE_PER_YEAR) as 'Avgsal'
                   from activesal
                   group by caseyr, WORK_STATE2, EMPLOYER_NAME ")

top_empsal = emp_sal[with(emp_sal, order(state,year, -Avgsal)),]

top_empsal$combkey = paste(top_empsal$year, " ", top_empsal$state)
top_empsal = top_empsal[!duplicated(top_empsal$combkey),] 
top_empsal = top_empsal[with(top_empsal, order(year, -Avgsal)),]


tpsal2014 = subset(top_empsal, year == 2014)
rowsel = c("Employer", "state", "Avgsal", "count")
t1 = subset(tpsal2014, select = rowsel) 
colnames(t1) = c("Employer Name", "State", "Avg. Salary", "No. of visas")
print( t1[1:10,], row.names = FALSE)  # prints the top 10 employers in 2014 by average salary 
t1[1:10,] # similar thing in a different command.

# join state coordinates and activesal
t1sal = sqldf("select t1.Employer, t1.state, t1.avgsal,
            state_us.Latitude, state_us.Longitude
            FROM t1 , state_us
            where t1.state  = state_us.WORK_STATE2")
colnames(t1sal) = c( "Employer", "State", 
                   "Avg. Salary", "latitude", "longitude")
t1sal$sal = round(t1sal$`Avg. Salary`)
t1sal$info = paste("Highest Paying Employer: ", "<br>",
                   t1sal[['Employer']],  ", ", "<br>",
                    "State = ", t1sal[['State']], "<br>", 
                    "Avg Annual Salary = ", t1sal[['sal']], 
                    sep='')


# create map to show univ details
map_demog <- leaflet(t1sal) %>% 
  setView(-93.65, 42.0285, zoom = 4) %>%
  addTiles() %>%
  addMarkers(~longitude, ~latitude, popup=~info,
             options = popupOptions(closeButton = TRUE),
             clusterOptions = markerClusterOptions() 
  )
map_demog 


