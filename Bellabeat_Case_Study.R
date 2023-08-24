---
title: "Bellabeat Case Study With R"
author: "Mariana Rosária"
date: "2023-08-24"
output: pdf_document
---

## **Bellabeat Case Study with R**

### Summary

This case study is the Capstone project for the Google Data Analytics Professional Certificate. In this case study, I am a Junior Data Analyst and I am analyzing data from consumer's smart devices in order to gain insights on how they use them and look for growth opportunities for the company giving data-driven suggestions for a marketing strategy. 

### The company

Bellabeat is a high-tech manufacturer of health-focused products for women that was founded in 2013. It is a successful small company, but it has the potential to become a larger player in the global smart device market. They have been collecting data on activity, sleep, stress, and reproductive health to empower women with knowledge about their health and wellness.


### **Ask and Prepare**
### Business Task

* Analyze the data from the consumer's devices to understand how they are already using the tools.
* Giving insights and suggestions for a new marketing strategy.

### Stakeholders

* Urška Sršen: Cofounder and Chief Creative Officer
* Sando Mur: Cofounder and part of Executive Team
* The marketing analytics team

### Dataset Characteristics

The data source that was provided for the this case study is FitBit Fitness Tracker Data (CC0: Public Domain, dataset made available through Mobius), on Kaggle. The dataset consists of 18 csv files containing 33 users health data logs from smart devices. The data was collected from users over a period of 2 months from 2016-03-12 to 2016-05-12, who consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring.

I will start by loading the necessary libraries for my analysis.

```{r Importing libraries}

install.packages("tidyverse")
install.packages("janitor")

library(tidyverse)
library(readr)
library(lubridate)
library(tidyr)
library(dplyr)
library(janitor)
```

And after downloading and storing the datasets I will import them to R Studio and name them.


```{r Importing datasets}

daily_activity <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
daily_calories <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/dailyCalories_merged.csv")
daily_intensities <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/dailyIntensities_merged.csv")
daily_steps <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/dailySteps_merged.csv")
hourly_calories <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/hourlyCalories_merged.csv")
hourly_intensities <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/hourlyIntensities_merged.csv")
hourly_steps <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/hourlySteps_merged.csv")
minute_calories_narrow <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/minuteCaloriesNarrow_merged.csv")
minute_calories_wide <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/minuteCaloriesWide_merged.csv")
minute_intensities_narrow <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/minuteIntensitiesNarrow_merged.csv")
minute_intensities_wide <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/minuteIntensitiesWide_merged.csv")
minute_mets_narrow <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/minuteMETsNarrow_merged.csv")
minute_sleep <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/minuteSleep_merged.csv")
minute_steps_narrow <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/minuteStepsNarrow_merged.csv")
minute_steps_wide <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/minuteStepsWide_merged.csv")
sleep_day <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weight_log_info <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")
heartrate_seconds <- read_csv("/cloud/project/Bellabeat/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")
             
```

```{r Checking the sample size in each dataset}

n_distinct(daily_activity$Id)
n_distinct(daily_calories$Id)
n_distinct(daily_intensities$Id)
n_distinct(daily_steps$Id)
n_distinct(hourly_calories$Id)
n_distinct(hourly_intensities$Id)
n_distinct(sleep_day$Id)
n_distinct(weight_log_info$Id)

```

After taking a peek at each dataset by clicking on them in the environment panel, I checked that the daily_activity dataset has a consolidated list of data from the other daily records, like daily steps, intensities and calories. For this reason, I will consider the daily_activity dataset for analysis instead of the other smaller datasets included. 
I do not think that the datasets containg data per minutes or seconds are interesting to get insights for a general marketing campaign, because they give us really individualistic insights.
The weight_log_info has only 8 individuals so I will not use it since the sample is too small for analysis.
These are the datasets I will then use in this case study:
* dailyActivity
* hourlyCalories
* hourlyIntensities
* hourlySteps
* sleepDay

**Data Limitations**

* The data is outdated - more than 7 years old.
* The sample is too small and does not cover a long period of time.
* It does not have information on how the users were chosen for data collection - is it randomized? Not sure if data is reliable or biased.

Back to the cleaning, I will now run str() function to check the tables structure, column names and general info.

```{r Checking info on each dataset}

 str(daily_activity)
 str(hourly_calories)
 str(hourly_intensities)
 str(hourly_steps)
 str(sleep_day)
 
```

I will clean the names of the columns.

```{r Cleaning column names}

daily_activity <- clean_names(daily_activity)
hourly_calories <- clean_names(hourly_calories)
hourly_intensities<- clean_names(hourly_intensities)
hourly_steps <- clean_names(hourly_steps)
sleep_day <- clean_names(sleep_day)

# Checking the cleaned column names

colnames(daily_activity)
colnames(hourly_calories)
colnames(hourly_intensities)
colnames(hourly_steps)
colnames(sleep_day)
```

With this function, I could see that the names of the columns have a good syntax but the columns referring to dates and date/time are in "character" format and shall then be changed to the proper class.

```{r Changing and checking format of dates}

daily_activity <- daily_activity %>%
  mutate(activity_date = as.Date(activity_date, format = "%m/%d/%Y"))
class(daily_activity$activity_date)

hourly_calories <- hourly_calories %>% 
  mutate(activity_hour = as.POSIXct(activity_hour, format = "%m/%d/%Y %H:%M:%S"))
class(hourly_calories$activity_hour)

hourly_intensities <- hourly_intensities %>% 
  mutate(activity_hour = as.POSIXct(activity_hour, format = "%m/%d/%Y %H:%M:%S"))
class(hourly_intensities$activity_hour)
  
hourly_steps <- hourly_steps %>% 
  mutate(activity_hour = as.POSIXct(activity_hour, format = "%m/%d/%Y %H:%M:%S"))
class(hourly_steps$activity_hour)

 sleep_day <- sleep_day %>% 
  mutate(sleep_day = as.POSIXct(sleep_day, format = "%m/%d/%Y %H:%M:%S")) %>% 
  rename(activity_hour = sleep_day) 
class(sleep_day$activity_hour)
```

Now I will check specifically for missing values and duplicates in the datasets. I use the is.na() and duplicated() function for the purpose. 

```{r Checking for missing values and duplicates}

sum(is.na(daily_activity))
sum(is.na(hourly_calories))
sum(is.na(hourly_intensities))
sum(is.na(hourly_steps))
sum(is.na(sleep_day))

sum(duplicated(daily_activity))
sum(duplicated(hourly_calories))
sum(duplicated(hourly_intensities))
sum(duplicated(hourly_steps))
sum(duplicated(sleep_day))
```
And I will clean the datasets from the duplicated values.

```{r Cleaning datasets from duplicated values}

daily_activity <- distinct(daily_activity)
hourly_calories <- distinct(hourly_calories)
hourly_intensities <- distinct(hourly_intensities)
hourly_steps <- distinct(hourly_steps)
sleep_day <- distinct(sleep_day)
```

### **Analyze and Visualize the Data**

I will now work with the 5 cleaned datasets to verify trends and create data visualization in order to present my findings.

#### Daily Activity Minutes Distribution

I want to understand how the Bellabeat customers pass their days. For this, I will firstly merge the daily_activity and sleep_day datasets, per Id.

```{r Merging daily_activity and sleep_day datasets}

daily_activity_sleep <- merge(x = daily_activity, y = sleep_day, by.x = c("id", "activity_date"), by.y = c("id", "activity_hour"))
                              
head(daily_activity_sleep)

```

Now I have a table with the distribution of minutes per activity, including sleep. I will download ggplot2 for graphic charts creation.

```{r Importing ggplot2}

library(ggplot2)

```

I will now find the average time that users are passing in each type of activity in order to create a pie chart showing the distribution.

```{r Finding the averages per activity}

avg_fairly_active_min <- mean(daily_activity_sleep$fairly_active_minutes)

avg_lightly_active_min <- mean(daily_activity_sleep$lightly_active_minutes)

avg_very_active_min <- mean(daily_activity_sleep$very_active_minutes)

avg_sedentary_min <- mean(daily_activity_sleep$sedentary_minutes)

```

I will create a data frame and a pie chart.


```{r Creating data frame and pie chart}

activity_distribution <- data.frame("Activity" = c('Avg Very Active Minutes', 'Avg Fairly Active Minutes', 'Avg Lightly Active Minutes', 'Avg Sedentary Minutes'),
                   "number" = c( 25.05, 17.92, 216.54, 712.1))

ggplot(activity_distribution, aes(x="", y=number, fill=Activity)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  labs(title = "Distribution of Minutes per Type of Activity", x = " ", y = " ")

```

As stated by the World Health Organization, an adult between 18 and 64 years old should do:
* at least 150–300 minutes of moderate-intensity aerobic physical activity
* or at least 75–150 minutes of vigorous-intensity aerobic physical activity or an equivalent combination of moderate- and vigorous-intensity activity throughout the week.

Our users, on average are active for a total of 227.5 minutes (3.8 hours) per day which consists of:
21.2 minutes being very active (average of 148.4 minutes per week)
13.6 minutes being moderately active (average of 95.2 minutesper week)
192.8 minutes (3.2 hours) being lightly active
Are sedentary for 991 minutes (16.5 hours)

So we can conclude that, on average, Bellabeat users are not active enough.


#### Relations between Activity / Steps and Calories Burned 

To verify the relationship between activity and calories I will firstly create a new variable that is the sum of all the minutes spent in the different types of activities.

```{r Creating new variable of total active minutes}

daily_activity <- daily_activity %>% 
  mutate(total_active_minutes = very_active_minutes + fairly_active_minutes + lightly_active_minutes)

# Checking the variable

head(daily_activity)
```

Moving on, I will check how the activity and calories relate, in a chart.

```{r Creating graph: Activity vs. Calories}

ggplot(data = daily_activity, aes(x = total_active_minutes, y = calories)) +
    geom_point() + 
    geom_smooth() +
    labs(title = "Daily Activity Duration vs. Calories Burned", x = "Minutes of Activity", y = "Calories Burned")
```

We can see here in this scatterplot graph that the daily activity and the calories burned are positively related. 
The more time a user is being active, the more calories they will burn.

As for the relationship between steps taken and calories burned, we will repeat the process and create another visualization:

```{r Creating graph: Steps vs. Calories}

ggplot(data = daily_activity, aes(x = total_steps, y = calories)) +
    geom_point() + 
    geom_smooth() +
    labs(title="Daily Steps Taken vs. Calories Burned", x = "Number of Steps", y = "Calories Burned")
```


```{r Checking average steps}

average_steps <- mean(daily_activity$total_steps)
```

We can also conclude that the steps taken per day and the calories burned in that same day have a linear positive relationship.
The average steps users take per day is 7637.91.

#### Activity Intensity vs Calories

To understand how the intensity of the activity and the calories burned are related, I will use the datasets hourly_calories and hourly_intensities.
I will merge the two, firstly.

```{r Merging hourly_calories and hourly_intensities}

hourly_activity <- merge(hourly_intensities, hourly_calories, by = c('id', 'activity_hour'))
head(hourly_activity)
```

```{r Creating the graph: Activity Intensity vs Calories}

ggplot(data=hourly_activity, mapping=aes(x=average_intensity, y=calories)) +
   geom_point() +
   geom_smooth() +
   labs(title = "Activity Intensity vs Calories Burned", x = "Activity Intensity", y = "Calories Burned")
```
As we could predict, if the activity is more intense, more calories are burned. 
We will now analyze more questions.

#### Intensity of Activities per week and day 

I want to understand how the activities with different intensity levels are spread over the week and during the day.
For that, I will analyze the daily_activity dataset.
I will firstly create a new column assigning a weekday to each activity date in the table and make a new variable with the total active minutes per day.


```{r Creating weekdays and total intensity}

daily_activity <- daily_activity %>%
  mutate(day = weekdays(activity_date)) %>% 
  mutate(total_intensity = (fairly_active_minutes + very_active_minutes + lightly_active_minutes))

```

```{Creating the chart: Intensity per Week}

ggplot(data=daily_activity)+
  geom_col(mapping = aes(x=factor(day, level = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y=total_intensity), fill="#99ccff") +
  labs(title="Average Activity Intensity Throughout the Week", x = "Days of the Week", y = "Intensity of Activity") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
```

As we can see in the bar chart created, users are mostly engaged in high intense activities on Tuesdays and it declines gradually until Friday.
During the weekends, the activity intensity is lower, probably due to the resting nature of these days.
Now we will look into the intensity of activities throughout the day. I will use the hourly_activity dataset that I have previously created and separate date and time, in order to organize a chart by hour.


```{r Separate date and time}

hourly_activity <- hourly_activity %>% 
  separate(activity_hour, into = c("date", "time"), sep= " ") 


head(hourly_activity)
```


```{r Creating graph: Intensity per Hour}

ggplot(data=hourly_activity) +
  geom_col(mapping = aes(x=time, y=(average_intensity * 100)), fill="#ff8c69") +
  labs(title="Average Activity Intensity Throughout the Day", x = "Hours of the Day", y = "Intensity of Activity") +
  theme(axis.text.x = element_text(angle = 90), axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
```

As we can take from the later chart, users are mostly active between 5h to 23h on an average day, and between 0h to 4h they are asleep so there is a huge decrease in the activity.
Users have higher intensity activities between 12h to 14h and 17h to 19h hours.

Next and finally, I am going to check on the sleeping patterns and activity levels to see if they correlate.

Firstly, I will check on the sleeping patterns throughout the week.
I will assign weekdays to variable day in dataset sleep_day, so the graphs get more readable.

```{r Assigning weekdays}

sleep_day <- sleep_day %>%
mutate(day = weekdays(activity_hour))
```

And now I will program R to make a data viz of the dataset.

```{r Creating graph: Sleep Minutes Troughout Week}

average_time_asleep <- mean(sleep_day$total_minutes_asleep)
 
 ggplot(data=sleep_day) +
   geom_col(mapping = aes(x = factor(day, level = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")), y = average_time_asleep), fill = "#c8a2c8") + 
   labs(title="Average Daily Sleep Minutes Throughout the Week", x = "Days of the Week", y = "Minutes") +
   theme(axis.text.x = element_text(angle = 90), axis.line = element_line(colour = "black"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank())

```

As we can see in the bar graph above, the sleeping patterns do not change a lot from day to day during the week.

Finally, I will try to figure out if there is any relation between the activity per day and the sleeping patterns. I will do that by comparing the total minutes asleep with the sedentary minutes, in daily_activity_sleep dataset that I have created on a previous code chunk.

```{r Creating scatterplot Sleep Vs Sedentary Minutes}

 ggplot(data=daily_activity_sleep, mapping=aes(x=total_minutes_asleep, y=sedentary_minutes)) +
   geom_point() + 
   geom_smooth() + 
   stat_cor(p.accuracy = 0.001, r.accuracy = 0.01, label.x.npc="middle")
   labs(title="Sleep Vs Sedentary Minutes", x="Minutes of Sleep", y="Sedentary Minutes")

```
Although, the correlation is not very strong (-0,6), we can hipothesize that there is a negative relation between the two variables. If one is more sedentary, the less minutes of sleep they have or if one has a good sleep time, they we'll be more active during the day.

### **Share and Act**

#### Findings

Here is a summary of my findings, giving the insights provided by the analysis and data viz:

* On average per day, users take 7638 steps.
* On average per day, users are active for a total of 227.5 minutes (3.8 hours), which consists of: 21.2 minutes being very active, 13.6 minutes being moderately active, 192.8 minutes (3.2 hours) being lightly active.
* On average per day, users are sedentary for 991 minutes (16.5 hours).
* Steps taken and intensity of activity have positive correlations with calories burned.
* Sleeping patterns and sedentary minutes have negative correlation.
* Users are most active on Tuesday and Saturday with 2 peaks of activity from 12-16h and 17-19h, and least active on Thursday and Sunday. 
* Users spend approximately 7h per day sleeping.

#### Recommendations for Marketing Team

* Create an educational pop up notifications so users can understand more about the benefits of activity and types of intensity and their relations to calories burned.
* Send notifications when users are sedentary for a long time and write them motivational messages so they can meet a individualized goal per day.
* Make a social ad campaign about the app's tools that make it possible to measure steps taken, intensity of activity and sleeping patterns and how these insights empower women with amazing knowledge - explore success stories.
* A reward system when meeting goals per week, maybe a very good way of making users engage - orchestrate a survey.

