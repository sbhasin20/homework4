if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales)
library(rdrobust)
install.packages("stargazer")
library(stargazer)
install.packages("jtools")

#1 

f <- function(x) {
  r <- quantile(x, probs = c(0.10, 0.25, 0.5, 0.75, 0.90))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

q1.plot <- final.data %>%
  group_by(fips, year) %>%
  select(fips, year) %>% summarize(plan_count=n()) %>%
  ggplot(aes(x=as.factor(year), y=plan_count)) + 
  stat_summary(fun.data=f, geom="boxplot") + 
  labs(
    x="Year", 
    y = "Number of Plans"
  ) + scale_y_continuous(labels=comma) +
  theme_bw()

q1.plot 

#I think that the number of plans is too many. In 2009, the average number of plans in a county was almost 40. I think the average number of plans should be around 8, which was not the case for any of the years from 2007 to 2015. 

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
  xlim(1, 5.5) +
  theme_classic()

graph_2

#The star ratings have increased overtime. Especially after 2012, when the ACA was introduced, there has been a shift. The overall highest star ratings were seen in 2015, then 2012, and 2009 has the lowest overall star ratings.

#3 

avg.benchmark <- final.data %>% filter(year > 2007) %>% group_by(ssa, year) %>%
  ggplot(aes(x=as.factor(year), y=ma_rate, group = 1)) + 
  stat_summary(fun = "mean", geom = "line", na.rm = TRUE) + 
  labs(
    x="Year", 
    y = "Benchmark Payments (Dollars)"
  ) + scale_y_continuous(labels=comma, limits = c(600,900))+
  theme_bw()
avg.benchmark

#The average benchmark payment has risen by about 60 dollars from 2008 to 2014. It then drops from 2014 to 2015 by about 40 dollars. 

#4 

ma.mkt.data <- final.data %>% group_by(fips, year) %>%
  summarize(enroll=first(avg_enrolled), medicare=first(avg_eligibles)) %>%
            mutate(mkt_share = enroll/medicare)

ma.share <- ma.mkt.data %>% filter(year>2007) %>%
  ggplot(aes(x=as.factor(year), y = mkt_share, group=1)) +
  stat_summary(fun="mean", geom = "line", na.rm = TRUE) +
  labs(
    x="Year",
    y = "Medicare Advantage Market Share"
  ) + theme_bw()

ma.share

#Medicare Advantage has increased in popularity over the years. This share correlates with benchmarks for the most part until 2014. From 2014 to 2015, the benchmark decreases but the average share of medicare advantage continues to increase. 

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
    na.rm=T)) %>% 
  select(contractid, planid, fips, avg_enrollment,
                      state, county, raw_rating, partc_score,
                         avg_eligibles, avg_enrolled, premium_partc, risk_ab, Star_Rating,
                         bid, avg_ffscost, ma_rate)

final.data.2009.new <- final.data.2009.new[!is.na(final.data.2009.new$raw_rating), ]

Star_Rating <- final.data.2009.new$Star_Rating
raw_rating <- final.data.2009.new$raw_rating

positive_diff <- data.frame(Star_Rating = Star_Rating, raw_rating = raw_rating)
positive_diff <- positive_diff[positive_diff$Star_Rating - positive_diff$raw_rating > 0, ]

print(positive_diff)

star_count <- table(positive_diff$Star_Rating)
star_count

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

question8 <- ggplot(ma.rd6, aes(x = raw_rating)) +
  geom_histogram(aes(y = ..density..), color = "black", fill = "gray", bins = 20) + 
  geom_density(alpha = 0.5, color = "black") + 
  labs(title = "Histogram of Raw Rating t", x = "Value", y = "Density") + 
  theme_minimal() 

question8

#There appears to be a high density just above 3 stars. This means that there are a larger amount of plans being rounded up to 3 stars compared to rounded down. This could put the plans that are rounded down to 3 stars at a disadvantage. There is also high density just above 2 star ratings, and those plans are likely being rounded down to 2 stars. 

#9 

#I was not sure how to go about this question. In the dataset I created for part 2 of the assignment (just for 2009), there is no year column. So, I tried to go back and work with the final data set. I initially tried to filter the data that had HMO/HMOPOS under the plan_type column, yes under the pland column, and 2009 under the year column using the filter and group_by function. I know how to evaluate the plans above the threshold values but the part that I was unsure about was how to incorporate the plan characteristics into the analysis. I would assume that I have to use the rorobust package and filter the variables that I describe before as I created a density graph like I did in problem 4. However, I kept getting an error for this. 


#10 

#Increasing star rating increases enrollments. This can be seen by the fact that the plans with a higher star rating had a higher number of enrollees, despite not having a drastic difference in quality. There is a bigger impact when the star ratings are on the lower end compared to when the star ratings are on the higher end. For exmaple, people are more likely to choose a 2.5 over a 2 rating versus a 4.5 over a 4 rating. 

save.image("Hwk4_workspace.Rdata")
