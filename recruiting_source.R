library(dplyr)
library(ggplot2)

recruitment<-read.csv("C:/Users/Keerthivasan/Desktop/Data Analytics/recruitment_data.csv")
head(recruitment)
summary(recruitment)
count(recruitment,recruitment$recruiting_source)
summarize(.data=recruitment,avg_sales_quota_pct = mean(recruitment$sales_quota_pct))

avg_sales <- recruitment %>%
  group_by(recruiting_source) %>% 
  summarize(avg_sales_quota_pct = mean(sales_quota_pct)) 
avg_sales

avg_attrition <- recruitment %>%
  group_by(recruiting_source) %>% 
  summarize(attrition_rate = mean(attrition))%>% 
  arrange(attrition_rate)
avg_attrition

ggplot(avg_sales, aes(x = recruiting_source, y = avg_sales_quota_pct)) +
  geom_col(fill="#800080",width=0.75)

ggplot(avg_attrition, aes(x = recruiting_source, y = attrition_rate)) +
  geom_col(fill="#800050",width = 0.75)
