" Andrew Cochran 
  ISTA 320 Final
  5/4/2021      "

library(knitr)
library(tidyverse)
library(dplyr)
opts_chunk$set(echo = TRUE)
# Dataset Description:
"The data was retrieved from kaggle.com and it was data based on student performance from two schools. The link to the dataset is here https://www.kaggle.com/larsen0966/student-performance-data-set. I retrieved the dataset by downloading the dataset uploading it to github."

#Dataset Import
student_performance <- read_csv('student_performance - Sheet1.csv')
glimpse(student_performance)

# 1st Question: How do students' grades differentiate by gender?

# Grouping by sex and then grabbing the average final grade between the two. G3 = Final Grade
average_gender <- student_performance %>%
  group_by(sex)%>%
  summarise(average = mean(G3, na.rm = TRUE))

# Using geom_col for visualization.
average_gender %>%
  ggplot(aes( x = sex, y = average))+
  geom_col()

# Females on average had better final grades than males between the two schools.

# Second Question: What semesters do students typically perform better in based on health and absences?

# Pivot longer table to compare both periods.
period_performance <- student_performance %>%
  pivot_longer(cols = c(`G1`, `G2`),
               names_to = 'Period',
               values_to = 'Grade')

# Visualization using health as my x grade to my y and color to period.
period_performance %>%
  ggplot(aes(x = Grade, y = health, color = Period))+
  geom_point(aes(size = absences))

# Based on the visualization I noticed that more students recived a grade above 15 in the second period. However I noticed that students who had poor health had more absences. Also I noticed that students who marked 2, which is a step above very poor health recorded more absences.  

#3rd Question: Based on gender and school period, do students who have more freetime recieve better grades.

# Pivot longer for each period and having the values to grade.
student_longer <- student_performance %>%
  pivot_longer(cols = c(`G1`, `G2`, `G3`),
                        names_to = 'Period',
                        values_to = 'Grade')
# Grouping by period, gender, and freetime then creating an average grade for each.  
student_longer <- student_longer %>%
  group_by(Period,sex,freetime)%>%
  summarise(avg_grade = mean(Grade, na.rm = TRUE))

# Lineplot visualization
student_longer %>%
  ggplot(aes(x = freetime, y = avg_grade, color = sex))+
  geom_point()+
  geom_line()+
  facet_wrap(~`Period`,ncol = 3, scales = 'free')
# The visualization provided me with some interestign insight on the student's performance.  The first thing I noticed is that females who reported that they had a lot of freetime recived on average the worst grades out of anyone. Students who reported that they had a step above very low for their freetime had the best grades for both males and females.  So to answer the question students who have more freetime do not recieve better grades and in fact worse grades. 
