library(dplyr)
library(ggplot2)
library(mosaic)

set.seed(13)
#summary of the data set
summary(Friends)
#performing t-test to see if the number of friends between genders is 
#statistically significant
t.test(friends ~ gender, data=Friends)
#One-way Anova with explanatory variable being gender
gender_anova <- aov(friends ~ gender, data = Friends)
#shows result of anova
summary(gender_anova)
# t test at 95% confidence level (0.05 significance level) testing if population mean is 2
t.test(Friends$friends, mu=2)
# splitting the Friends df into two based on gender of the experimental unit
male_friends <- Friends %>% filter(gender == "male")
female_friends <- Friends %>% anti_join(male_friends, by = "gender")

male_friends <- male_friends %>% select(friends)
female_friends <- female_friends %>% select(friends)

#sampling from the male and female population
n1 <- 460
n2 <- 540
male_sample <- male_friends %>% sample_n(n1)
female_sample <- female_friends %>% sample_n(n2)

#printing summaries of the data from the male and female samples
favstats(male_sample$friends)
favstats(female_sample$friends)
#testing whether the true mean number of friends of the two genders are different.
t.test(male_sample, female_sample, var.equal = F)
#plotting a histogram of number of friends
Friends %>% ggplot() + geom_bar(aes(x=friends, col=gender)) + 
  labs(title = "Sampling Distribution of number of friends", y = "frequency", x = "number of friends")

#plotting a histogram of number of friends of all male subjects
male_friends %>% ggplot() + geom_bar(aes(x=friends), col = "blue") + 
  labs(title = "Sampling Distribution of number of friends of men", y = "frequency", x = "number of friends")

#plotting a histogram and boxplot of number of friends of male subjects in a sample
male_sample %>% ggplot() + geom_bar(aes(x=friends), col = "brown") + 
  labs(title = "Sampling Distribution of number of friends of men", y = "frequency", x = "number of friends")

male_sample %>% ggplot() + geom_boxplot(aes(y=friends), col = "brown") + 
  labs(title = "Sampling Distribution of number of friends of men in the sample", y = "frequency", x = "number of friends") 

#plotting a histogram of number of friends of all female subjects
female_friends %>% ggplot() + geom_bar(aes(x=friends), col = "red") + 
  labs(title = "Sampling Distribution of number of friends of women", y = "frequency", x = "number of friends")

#plotting a histogram of number of friends of female subjects in a sample
female_sample %>% ggplot() + geom_bar(aes(x=friends), col = "purple") + 
  labs(title = "Sampling Distribution of number of friends of women in the sample", y = "frequency", x = "number of friends")

