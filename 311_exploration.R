sr = read.csv(file.choose())
library(dplyr)
sr = sample_n(sr, 10000)
sr = tbl_df(sr)
ggplot(sr, aes(Department)) + geom_bar()

library(ggplot2)
library(lubridate)

ggplot(sr, aes(OnTime_Status)) + geom_bar()
table(sr$OnTime_Status[sr$SUBJECT=="Public Works Department"], sr$SUBJECT[sr$SUBJECT=="Public Works Department"])

sr1 = sr
sr1$OPEN_DT = mdy_hms(sr1$OPEN_DT)
sr1$TARGET_DT = mdy_hms(sr1$TARGET_DT)
sr1$CLOSED_DT = mdy_hms(sr1$CLOSED_DT)
sr1

mean(difftime(sr1$CLOSED_DT,sr1$OPEN_DT),na.rm=TRUE)
difftime(sr1$OPEN_DT[10],sr1$CLOSED_DT[10])
sr1 = sr1 %>% mutate(closed_minus_open = difftime(CLOSED_DT,OPEN_DT))
sr1 = sr1 %>% mutate(target_minus_close = difftime(TARGET_DT,CLOSED_DT))
sr1

sr1  %>% select(c(SUBJECT, REASON, TYPE, neighborhood, closed_minus_open, target_minus_close))  %>%
  group_by(SUBJECT) %>%
  summarise(mean_target_close = mean(target_minus_close, na.rm = TRUE)) %>% 
  arrange(mean_target_close)

sr1  %>% select(c(SUBJECT, REASON, TYPE, neighborhood, closed_minus_open, target_minus_close))  %>%
  group_by(SUBJECT) %>%
  summarise(mean_open_close = mean(closed_minus_open, na.rm = TRUE)) %>%
  arrange(mean_open_close)

