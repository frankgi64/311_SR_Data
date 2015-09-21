
# Read in 311 Service Request csv file as sr
# Sample 10000 rows of the file for quicker calculations
sr = read.csv(file.choose())
library(dplyr)
sr = sample_n(sr, 10000)

# save the data frame as a 'tbl_df' in the dplyr package
# this allows for interesting data manipulations and wrangling
sr = tbl_df(sr)
ggplot(sr, aes(Department)) + geom_bar()

# load the ggplot2 and lubridate packages
library(ggplot2)
library(lubridate)

# plot number of requests by department
ggplot(sr, aes(SUBJECT)) + geom_bar()

# Plot "on-time status" to get a feel for how many requests are completed on time vs. late
ggplot(sr, aes(OnTime_Status)) + geom_bar()

# Make a table of on-time status for the public works dept because they seem to get the most requests
table(sr$OnTime_Status[sr$SUBJECT=="Public Works Department"], sr$SUBJECT[sr$SUBJECT=="Public Works Department"])

# Make a copy of the sr data frame.  This is so I don't mess with the original data.
sr1 = sr

# Convert OPEN_DT, TARGET_DT, and CLOSED_DT to dates so that we can calculate time differences
sr1$OPEN_DT = mdy_hms(sr1$OPEN_DT)
sr1$TARGET_DT = mdy_hms(sr1$TARGET_DT)
sr1$CLOSED_DT = mdy_hms(sr1$CLOSED_DT)
sr1

# Calculate the mean time difference between open date and close date.  
mean(difftime(sr1$CLOSED_DT,sr1$OPEN_DT),na.rm=TRUE)

# Spot check calculation for row 10
difftime(sr1$OPEN_DT[10],sr1$CLOSED_DT[10])

#add column to sr1 containing the time between open and closed dates (in seconds)
sr1 = sr1 %>% mutate(closed_minus_open = difftime(CLOSED_DT,OPEN_DT))

# add a column to sr1 containing the time between target and closed dates (in seconds)
# negative times indicate requests that were closed before the target date
sr1 = sr1 %>% mutate(target_minus_close = difftime(TARGET_DT,CLOSED_DT))
sr1

# group by department and arrange by the mean of the difference between target minus close
sr1  %>% select(c(SUBJECT, REASON, TYPE, neighborhood, closed_minus_open, target_minus_close))  %>%
  group_by(SUBJECT) %>%
  summarise(mean_target_close = mean(target_minus_close, na.rm = TRUE)) %>% 
  arrange(mean_target_close)

# group by department and arrange by the mean of the difference between open minus close
sr1  %>% select(c(SUBJECT, REASON, TYPE, neighborhood, closed_minus_open, target_minus_close))  %>%
  group_by(SUBJECT) %>%
  summarise(mean_open_close = mean(closed_minus_open, na.rm = TRUE)) %>%
  arrange(mean_open_close)

