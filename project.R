library(MatchIt)
library(haven)
library(dplyr)
library(ggplot2)
library(tidyverse)

mydata <- read.csv("C:/Users/17855/OneDrive/Desktop/637/PROJECT/COVID-19 Survey Student Responses.csv")


t.test(TimespentonOnlineClass ~ Regionofresidence, data = mydata)

mydata_cov <- c('AgeofSubject', 'Timespentonselfstudy', 'Timespentonfitness', 'Timespentonsleep', 'Timespentonsocialmedia')

mydata %>%
  group_by(Regionofresidence)%>%
  select(one_of(mydata_cov)) %>%
  summarise_all(funs(mean(., na.rm = TRUE)))


attach(mydata)
t.test(AgeofSubject ~ Regionofresidence)
t.test(Timespentonselfstudy ~ Regionofresidence)
t.test(Timespentonfitness ~ Regionofresidence)
t.test(Timespentonsleep ~ Regionofresidence)
t.test(Timespentonsocialmedia ~ Regionofresidence)
detach(mydata)

check_balance <- matchit(Regionofresidence ~ AgeofSubject + Timespentonselfstudy + Timespentonfitness + Timespentonsleep + Timespentonsocialmedia,
                         method = NULL, distance = "glm", data = mydata)
summary(check_balance)

#estimate propensity score

psm <- glm(Regionofresidence ~ AgeofSubject + Timespentonselfstudy + Timespentonfitness + Timespentonsleep + Timespentonsocialmedia,
           family = binomial(link = ("probit")), data = mydata)
summary(psm)

prs_df <- data.frame(pr_score = predict(psm, type = "response"),
                     Regionofresidence = psm$model$Regionofresidence)
head(prs_df)

mydatacb <- cbind(mydata, prs_df$pr_score)

#look the region of common suppurt

labs <- paste(c("Residence in New Dehli", "Residence outside New Dehli"))
prs_df %>%
  mutate(Regionofresidence = ifelse(Regionofresidence == 1, labs[1], labs[2])) %>%
  ggplot(aes(x = pr_score)) +
  geom_histogram(color = "white") +
  facet_wrap(~Regionofresidence) +
  xlab("Probability of having in town student") +
  theme_bw()

#very strong region of common support between 0.45 - 0.7

#matchit does not allow missing values
mydata_nomiss <- mydatacb %>%
  select(TimespentonOnlineClass, Regionofresidence, one_of(mydata_cov)) %>%
  na.omit()

#Now we using PSM with logit model using nearest neighbour:
#If want to do covariate matching, set distance = mahalanobis
mod_match <- matchit(data = mydata_nomiss, Regionofresidence ~ AgeofSubject + Timespentonselfstudy + Timespentonfitness + Timespentonsleep + Timespentonsocialmedia,
                     method = "nearest", ratio = 1,
                     caliper = c(0.1, AgeofSubject = 0.5), distance = "glm",
                     discard = "both", estimand = "ATT")
summary(mod_match)

plot(mod_match, type = "jitter", interactive = FALSE)

#create a dataset of succesful matches
dta_m <- match.data(mod_match)
dim(dta_m)

plot(mod_match, type = "hist")


t.test(TimespentonOnlineClass ~ Regionofresidence, data = dta_m)












