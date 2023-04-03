if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales)
install.packages("rdrobust")
library(rdrobust)

#1 
final.plans <- final.data %>%
  filter(snp == "No" & partd == "No" &
           (planid < 800 | planid >= 900))
question1 <- final.plans %>% group_by(county, year) %>% count() 

q1.plot <- question1 %>%
  ggplot(aes(x = factor(year), y = log(n))) +
  geom_boxplot() +
  labs (
    x="Year", 
    y = "Number of Plans"
  ) +
  theme_bw()

q1.plot

#I think the number of plans is low for each country over time. Some counties only have 1 option, especially in the later years. Overtime, there has been a decrease in the number of plans per country. This may be because plans are merging within the insurance companies to have a higher quality rating. 

#2 
data_filtered <- final.data %>% 
  filter(year %in% c(2009, 2012, 2015)) %>% 
  group_by(year, Star_Rating) %>% 
  summarise(count = n()) %>% 
  ungroup()
my_colors <- c("lightgreen", "purple", "skyblue")

graph_2 <- ggplot(data_filtered, aes(x = Star_Rating, y = count, fill = as.factor(year))) + 
  geom_bar(position = "dodge", stat = "identity") + 
  labs(x = "Star Ratings", y = "Count", title = "Distribution of Star Ratings (2009, 2012,2015)") +
  scale_fill_manual(values = my_colors, name = "Year") +
  ylim(0, 25000) +
  xlim(1, 5) +
  theme_classic()

graph_2

#The star ratings have increased overtime. Especially after 2012, when the ACA was introduced, there has been a shift. The overall highest star ratings were seen in 2015, then 2012, and 2009 has the lowest overall star ratings. 

#3: 

q3.plot <- final.data %>% 
  filter(year >= 2009 & year <= 2015)%>%
  group_by(year) %>% 
  summarize(avg_rate = mean(ma_rate, na.rm =TRUE))%>% 
  ggplot( aes(year, avg_rate))+
  geom_line()

q3.plot

#The average benchmark payment has risen by about $40 from 2009 to 2015. It then drops from 2014 to 2013 by about $75. 

#4 

final_data_four <- final.data %>%
  filter(year >= 2009 & year <= 2015)

final_data_four_new <- final_data_four %>% 
  mutate(dummy_data = ifelse(partd == "No", 0, 1))

graph_4 <- final_data_four_new %>%
  group_by(year) %>%
  summarise(avg_dummy_data = mean(dummy_data)) %>%
  ggplot(aes(year,avg_dummy_data)) + 
  geom_line() + 
  labs(x = "Year", y = "Average Share of Medicare Advantage",
       title = "Average Share of Medicare Advantage from 2009 through 2015")

graph_4

#Medicare Advantage has increased in popularity over the years. This share correlates with benchmarks for the most part until 2014. From 2014 to 2014, the benchmark decreases but the average share of medicare advantage continues to increase. 

#5 

final.data.2009 <- final.data %>%
  filter(year == 2009)

final.data.2009.new <- final.data.2009 %>%
  mutate(raw_rating=rowMeans(
    cbind(breastcancer_screen,rectalcancer_screen,cv_cholscreen,diabetes_cholscreen,
          glaucoma_test,monitoring,flu_vaccine,pn_vaccine,physical_health,
          mental_health,osteo_test,physical_monitor,primaryaccess,
          hospital_followup,depression_followup,nodelays,carequickly,
          overallrating_care,overallrating_plan,calltime,
          doctor_communicate,customer_service,osteo_manage,
          diabetes_eye,diabetes_kidney,diabetes_bloodsugar,
          diabetes_chol,antidepressant,bloodpressure,ra_manage,
          copd_test,betablocker,bladder,falling,appeals_timely,
          appeals_review),
    na.rm=T))

final.data.2009.new <- final.data.2009.new[!is.na(final.data.2009.new$Star_Rating), ]

final.data.2009.new$indicator <- ifelse(final.data.2009.new$Star_Rating > final.data.2009.new$raw_rating, 1,0)

table_5 <- final.data.2009.new %>% group_by(Star_Rating) %>% 
  summarise(mean(indicator))
table_5

#6 
ma.rd6 <- final.data.2009.new %>%
  mutate(score1 = raw_rating - 2.75,
         score2 =raw_rating - 3.25, 
         score3 = raw_rating - 3.75, 
         score4 = raw_rating - 4.25,
         mkt_share = avg_enrollment/avg_eligibles,
         ln_share = log(mkt_share))


regression6_1 <- rdrobust(y=ma.rd6$mkt_share, x=ma.rd6$score1, c=0,
         h=0.125, p=1, kernel="uniform", vce="hc0",
         masspoints="off")

summary(regression6_1)

regression6_2 <- rdrobust(y=ma.rd6$mkt_share, x=ma.rd6$score2, c=0,
                          h=0.125, p=1, kernel="uniform", vce="hc0",
                          masspoints="off")

summary(regression6_2)

regression6_3 <- rdrobust(y=ma.rd6$mkt_share, x=ma.rd6$score3, c=0,
                          h=0.125, p=1, kernel="uniform", vce="hc0",
                          masspoints="off")

summary(regression6_3)


#7

reg_1 <- rdrobust(y=ma.rd6$mkt_share, x=ma.rd6$score1, c=0,
                    h=0.1, p=1, kernel="uniform", vce="hc0",
                    masspoints="off")

reg_2 <- rdrobust(y=ma.rd6$mkt_share, x=ma.rd6$score1, c=0,
                     h=0.12, p=1, kernel="uniform", vce="hc0",
                     masspoints="off")

reg_3 <- rdrobust(y=ma.rd6$mkt_share, x=ma.rd6$score1, c=0,
                     h=0.13, p=1, kernel="uniform", vce="hc0",
                     masspoints="off")

reg_4 <- rdrobust(y=ma.rd6$mkt_share, x=ma.rd6$score1, c=0,
                     h=0.14, p=1, kernel="uniform", vce="hc0",
                     masspoints="off")

reg_5 <- rdrobust(y=ma.rd6$mkt_share, x=ma.rd6$score1, c=0,
                     h=0.15, p=1, kernel="uniform", vce="hc0",
                     masspoints="off")


#As the bandwidth increases, the sensitivity decreases. Thus, a smaller bandwidth is more sensitive and more representative. 

#8 

graph_7a <- rdplot(y=ma.rd6$mkt_share, x=ma.rd6$score1, binselect = "es", 
                   title = "RD Plot: Market Share", x.label="Summary Score", 
                   y.label="Market Share", masspoints="off")



graph_7b <- rdplot(y=ma.rd6$mkt_share, x=ma.rd6$score2, binselect = "es", 
                   title = "RD Plot: Market Share", x.label="Summary Score", 
                   y.label="Market Share", masspoints="off")

graph_7b

graph_7c <- rdplot(y=ma.rd6$mkt_share, x=ma.rd6$score3, binselect = "es", 
                   title = "RD Plot: Market Share", x.label="Summary Score", 
                   y.label="Market Share", masspoints="off")

graph_7c

#As the bandwidth increases, the ratings become less significant. There is a wider range so the rating does not reflect as strongly the quality of the program. 

#9 

summary1 <- ma.rd6 %>% filter(score1>-0.25 & score1<0.25)

summary1$above <- ifelse(sum1$score1 > 0, 1,0 )

tab9 <- summary1 %>% group_by(above) %>% summarize(prop_partd = mean(partd == "Yes"))



#10 

#Increasing star rating increases enrollments. This can be seen by the fact that the plans with a higher star rating had a higher number of enrollees, despite not having a drastic difference in quality. There is a bigger impact when the star ratings are on the lower end compared to when the star ratings are on the higher end. For exmaple, people are more likely to choose a 2.5 over a 2 rating versus a 4.5 over a 4 rating. 

save.image("Hwk4_workspace.Rdata")
