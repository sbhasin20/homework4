if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, scales)
library(rdrobust)
install.packages("stargazer")
library(stargazer)
install.packages("jtools")
install.packages("broom") 
library(broom) 
install.packages("kableExtra")
library(kableExtra)
install.packages("modelsummary")
library(modelsummary)
install.packages("cobalt")
library(cobalt)  
install.packages("rddensity")
library(rddensity)
install.packages("lemon")

full_ma_data <- read_rds('data/output/full_ma_data.rds')

final.data<- read_rds("data/output/final_ma_data.rds")


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

#I think the number of plans is too few. In 2015, some counties had about 5 plans to choose from, which I think it is a little low. The number of plans are most likely decreasing overtime because insurance companies are combining their plans to gain a higher Star-Rating.

#2 
data_filtered <- final.data %>% 
  filter(year %in% c(2009, 2012, 2015)) %>% 
  group_by(year, Star_Rating) %>% 
  summarise(count = n()) %>% 
  ungroup()
my_colors <- c("lightgreen", "purple", "skyblue")

graph_2 <- ggplot(data_filtered, aes(x = Star_Rating, y = count, fill = as.factor(year))) + 
  geom_bar(position = "dodge", stat = "identity") + 
  labs(x = "Star Ratings", y = "Count", title = "Distribution of Star Ratings") +
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
    y = "Benchmark Payments (Dollars)",
    title = "Average Benchmark Overtime"
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
    y = "Medicare Advantage Market Share", 
    title = "Average Share of Medicare Advantage Overtime"
  ) + theme_bw()

ma.share

#Medicare Advantage has increased in popularity over the years. This market share correlates with benchmarks for the most part until 2014. From 2014 to 2015, the benchmark decreases but the average share of medicare advantage continues to increase. 

#5 

final.data.2009.new <- final.data %>% ungroup() %>% 
  filter(!is.na(avg_enrollment) & year==2009 & !is.na(partc_score)) %>%
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
         bid, avg_ffscost, ma_rate, plan_type, partd) %>%
  mutate(mkt_share = avg_enrollment/avg_eligibles,
         HMO=str_detect(plan_type, "HMO"))

  final.2009<- final.data.2009.new %>%
  mutate(rounded_30=ifelse(raw_rating>= 2.75 & raw_rating<3.00 & Star_Rating == 3.0,1,0),
         rounded_35=ifelse(raw_rating>= 3.25 & raw_rating<3.50 & Star_Rating == 3.5,1,0),
         rounded_40=ifelse(raw_rating>= 3.75 & raw_rating<4.00 & Star_Rating == 4.0,1,0),
         rounded_45=ifelse(raw_rating>= 4.25 & raw_rating<4.50 & Star_Rating == 4.5,1,0),
         rounded_50=ifelse(raw_rating>= 4.75 & raw_rating<5.00 & Star_Rating == 5,1,0)) %>%
  
  group_by(Star_Rating) %>% filter(Star_Rating %in% c(3,3.5,4,4.5,5)) %>%
  summarize(count_30 = sum(rounded_30),
            count_35 = sum(rounded_35),
            count_40 = sum(rounded_40),
            count_45 = sum(rounded_45),
            count_50 = sum(rounded_50)) %>%
  mutate(rounded=count_30+count_35 +count_40+count_45+ count_50) %>%
select(Star_Rating, rounded)
  
final.2009
  
#6

star30<- lm(mkt_share ~ treat+ score,
            data=(final.data.2009.new %>%
                    filter(raw_rating>= (2.75-0.125),
                           raw_rating<= (2.75+0.125),
                           Star_Rating %in% c(2.5,3.0)) %>%
                    mutate(treat=(Star_Rating == 3.0),
                           score= raw_rating-2.75)))

star35<- lm(mkt_share ~ treat+ score,
            data=(final.data.2009.new %>%
                    filter(raw_rating>= (3.25-0.125),
                           raw_rating<= (3.25+0.125),
                           Star_Rating %in% c(3.0, 3.5)) %>%
                    mutate(treat=(Star_Rating == 3.5),
                           score= raw_rating-3.25)))

star40<- lm(mkt_share ~ treat+ score,
            data=(final.data.2009.new %>%
                    filter(raw_rating>= (3.75-0.125),
                           raw_rating<= (3.75+0.125),
                           Star_Rating %in% c(3.5, 4.0)) %>%
                    mutate(treat=(Star_Rating == 4.0),
                    score= raw_rating-3.75)))

star45<- lm(mkt_share ~ treat+ score,
            data=(final.data.2009.new %>%
                    filter(raw_rating>= (4.0-0.125),
                           raw_rating<= (4.0+0.125),
                           Star_Rating %in% c(4.0, 4.5)) %>%
                    mutate(treat=(Star_Rating == 4.5),
                           score= raw_rating-4.25)))


table6 <- modelsummary(list("Star 3.0"=star30, "Star 3.5 "=star35, "Star 4.0"=star40, "Star 4.5"=star45), 
                       title="Estimates", 
                       coef_map=c('treatTRUE'="Treatment",
                                  'Score'="score"), 
                      gof_map=list(list("raw"="nobs","clean"="N", "fmt"=0),
                                    list("raw"="r.squared", "clean"="R^2", "fmt"=2))) %>%
  kable_styling(latex_options=c("hold_position"), full_width = TRUE, position="center")

table6


#7 

for (h in seq(0.1, 0.15, 0.01)) {
  star30bw <- lm(mkt_share ~ treat +score, 
                 data= (final.data.2009.new %>%
                          filter(raw_rating >= (2.75-h),
                                 raw_rating<= (2.75+h),
                                 Star_Rating %in% c(2.5,3.0)) %>%
                          mutate(treat= (Star_Rating == 3.0),
                                 score = raw_rating-2.75)))
  coef.30 <- tidy(star30bw, conf.int=TRUE) %>% mutate(rating=30)
  
  
  star35bw <- lm(mkt_share ~ treat+score, 
                 data=(final.data.2009.new %>%
                         filter(raw_rating >= (3.25-h),
                                raw_rating<= (3.25+h),
                                Star_Rating %in% c(3.0,3.5)) %>%
                         mutate(treat= (Star_Rating == 3.5),
                                score = raw_rating-3.25)))
  coef.35 <- tidy(star35bw, conf.int=TRUE) %>% mutate(rating=35)
  
  star40bw <- lm(mkt_share ~ treat+score, 
                 data=(final.data.2009.new %>%
                         filter(raw_rating >= (3.75-h),
                                raw_rating<= (3.75+h),
                                Star_Rating %in% c(3.5,4.0)) %>%
                         mutate(treat= (Star_Rating == 4.0),
                                score = raw_rating-3.75)))
  coef.40 <- tidy(star40bw, conf.int=TRUE) %>% mutate(rating=40)
  
  
  est.collect <- rbind(coef.30, coef.35, coef.40) %>%
    mutate(bandwidth=h)
  
  if(h==0.1) {
    est.final <- est.collect
  }
  else {
    est.final <- rbind(est.final, est.collect)
  }
  
}

question7 <- est.final %>% filter (term== "treatTRUE") %>%
  ggplot(aes(x=as.factor(bandwidth), y=estimate, shape=as.factor(rating))) +
geom_hline(aes(yintercept=0), linetype="dashed") +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                lwd=1, width=0, position=position_dodge(width = 0.5))+
  labs(
    y="Estimate and 95 Percent Confidence Interval",
    x = "Bandwidth",
    shape= "Star Rating") +
  geom_point(size=3, position=position_dodge(width=0.5)) + 
  theme_bw()

question7

#The bandwidths for 3 to 3.5 star are the least reliable since it has the largest range in its 95 percent confidence interval (CI). With a smaller bandwidth of 0.1, all of the confidence intervals include 0. As the bandwidth increases, the 2.5 to 3 stars CI no longer overlaps with the 3 to 3.5 and 3.5 to 4 CIs. The larger star rating (3 to 3.5 and 3.5 to 4) CI seem to be more consistent compared to the 2.5 to 3.0 star ratings as the bandwidth increases. Thus, our findings are not sensitive to our choice of bandwidth as changes in bandwidth cause no significant changes in the estimates for all the star ratings. 

  
#8 

question8a <- final.data.2009.new %>% filter(raw_rating>= (2.75-0.125),raw_rating<= (2.75+0.125),
                           Star_Rating %in% c(2.5,3.0)) %>%
  mutate(score = raw_rating-2.75)
dens253 <- rddensity(question8a$score, c=0)
question8aplot <- rdplotdensity(dens253, question8a$score)



question8b <- final.data.2009.new %>% filter(raw_rating>= (3.25-0.125),raw_rating<= (3.25+0.125),
                           Star_Rating %in% c(3.0,3.5)) %>%
  mutate(score = raw_rating-3.25)
dens335 <- rddensity(question8b $score, c=0)
question8bplot <- rdplotdensity(dens335, question8b$score)


question8c <- final.data.2009.new %>% filter(raw_rating>= (3.75-0.125),raw_rating<= (3.75+0.125),
                           Star_Rating %in% c(3.5,4.0)) %>%
  mutate(score = raw_rating-3.75)
dens354 <- rddensity(question8c$score, c=0)
question8cplot <- rdplotdensity(dens354, question8c$score)


#Based on the figures above, RD is not the best estimator since there is not a lot of mass around the thresholds. The RD does not seem to be close to continuous above and below the threshold given the jump seen on the graphs. It does not appear that the contracts manipulate the running variable. 

#9 

lp.vars <- final.data.2009.new %>% ungroup() %>%
  filter( (raw_rating >= 2.75-0.125 & Star_Rating== 2.5) |
            (raw_rating  <= 2.75+0.125 & Star_Rating == 3) ) %>%
  mutate(rounded = (Star_Rating == 3)) %>% 
  select(HMO, partd, rounded) %>%
  filter(complete.cases(.))

lp.covs <- lp.vars %>% select(HMO, partd)

plot.30 <- love.plot(bal.tab(lp.covs, treat= lp.vars$rounded), colors="black")+
  theme_bw() + theme(legend.position="none")

plot.30

lp.vars <- final.data.2009.new %>% ungroup() %>%
  filter((raw_rating >= 3.25-0.125 & Star_Rating==3.0) |
           (raw_rating <= 3.25+0.125 & Star_Rating==3.5)) %>%
  mutate(rounded=(Star_Rating==3.5)) %>%
  select(HMO, partd, rounded) %>%
  filter(complete.cases(.))

lp.covs <- lp.vars %>% select(HMO, partd)

plot.35 <- love.plot(bal.tab(lp.covs, treat=lp.vars$rounded), colors="black") + 
  theme_bw() + theme(legend.position="none")

plot.35

lp.vars <- final.data.2009.new %>% ungroup() %>%
  filter((raw_rating >= 3.75-0.125 & Star_Rating==3.5) |
           (raw_rating <= 3.75+0.125 & Star_Rating==4.0)) %>%
  mutate(rounded=(Star_Rating==4.0)) %>%
  select(HMO, partd, rounded) %>%
  filter(complete.cases(.))

lp.covs <- lp.vars %>% select(HMO, partd)

plot.4 <- love.plot(bal.tab(lp.covs, treat=lp.vars$rounded), colors="black") + 
  theme_bw() + theme(legend.position="none")

plot.4

#Based on the figures above, the plans above the threshold values have similar characteristics than contracts just below the threshold values using HMO and Part D status as plan characteristics. The difference is somewhat small and inconsistent across the star ratings. Given this small difference, the characteristics are similar for values just above and below the threshold value. 



#10 

#Based on my findings from 1-5, the effect of increasing a star rating on enrollments is positive for the lower star ratings and negative for the higher star ratings. Plans with a lower Star Rating that are rounded up will have more enrollments from being rounded up. On the other hand, plans with a higher Star Rating, such as 3.25 or 3.75, have a negative effect on market share. 

rm(list=c("final.data.2009.new","final.data"))
save.image("Hwk4_workspace_4b.Rdata")



