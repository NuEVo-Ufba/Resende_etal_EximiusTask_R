#####START OF CODE####

#Note you will need to set your working directory to the location you have saved the data file.

#Load packages and data####

#Load the required packages. Note if you do not have these installed you will have to do that first.
library(dplyr)
library(glmmTMB)
library(ggplot2)
library(gridExtra)
library(broom.mixed)
library(car)
library(MASS)

#Load data
resende_data = read.table("ResendeEximiusTaskData.txt", header=T)

summary(resende_data)

#"date" is the date of the observation
#"web" is the unique ID of the colony
#"bask_vol" is the volume of the web basket (cm^3), calculated as the product of the length, width, and height of the basket
#"ind" is the ID of the individual spider, which is unique within its own web but NOT unique across different webs
#"web_ind" is the concatenation of "web" and "ind", and so gives a unique ID for each spider in the study.
#"latency" is the time in seconds the spider took to respond to a puff of air, higher values indicate more timid spiders
#"space" is the location the spider was in when observed, with "middle_basket", "edge_basket", or "sail"
#"sail" is a binary variable of whether the spider was in the basket (either middle or edge, coded as "0"), or the sail (coded as "1")
#"edge_b" is a binary variable of whether the spider was in the edge of the basket (coded as "1"), or not (either in the middle of the basket or the sail, coded as "0")
#"active" is a binary variable of whether the spider was resting (coded as "0"), or engaged in some kind of activity (coded as "1")
#"rest_exp" is a binary variable of whether the spider was resting in an exposed location (coded as "1"), or not (either resting in a protected position or not resting, coded as "0")
#"care_young" is a binary variable of whether the spider was caring for offspring or eggs (coded as "1"), or not (engaged in some other activity or resting, coded as "0")
#"walk" is a binary variable of whether the spider was walking (coded as "1"), or not (engaged in some other activity or resting, coded as "0")

#Note that the analysis requires certain subsets to be made (these are performed in the code before the given model is run) 
#such that, for example, only resting spiders are compared for whether they are resting in an exposed vs protected position

#Histogram of latency scores from the source colony individuals
fig3 = ggplot(resende_data, aes(x= latency)) + 
  geom_histogram(aes(y = ..density..), color = "gray", fill = "white") +
  geom_density(fill = "black", alpha = 0.2) + theme_classic() +
  xlab("Seconds to move after air puff") + ylab("Frequency Density")

#Analysis of location####

#In basket vs in sail

web_m1 = glmmTMB(sail ~ scale(latency)  + scale(log(bask_vol)) +
                   (1|web/web_ind) +  (1|date) , family = binomial,
                 resende_data)
summary(web_m1)

Anova(web_m1, type=2)

tidy(web_m1, effects="ran_pars", conf.int =T, conf.method = "Wald")

exp(fixef(web_m1)[[1]]) 

confint(web_m1)

#If in basket, edge vs middle

resende_data_b = resende_data %>%
  filter(space !="sail") #only individuals in the basket
  
basket_m1 = glmmTMB(edge_b ~ scale(latency)  + scale(log(bask_vol)) +
                        (1|web/web_ind) +  (1|date) , family = binomial,
                      resende_data_b)
summary(basket_m1)

Anova(basket_m1, type=2)

tidy(basket_m1, effects="ran_pars", conf.int =T, conf.method = "Wald")

exp(fixef(basket_m1)[[1]])

confint(basket_m1)

#Analysis of activity####

#Resting vs active

rest_m1 = glmmTMB(active ~ scale(latency)* space + scale(log(bask_vol)) +
                   (1|web/web_ind) +  (1|date) , family = binomial,
                 resende_data)
summary(rest_m1)

Anova(rest_m1, type=2)

tidy(rest_m1, effects="ran_pars", conf.int =T, conf.method = "Wald")

exp(fixef(rest_m1)[[1]])

confint(rest_m1)

#Resting exposed vs resting protected

resende_data_r = resende_data_b %>%
  filter(active == 0) #only want spiders that are resting, 
#and those not in sail (which we filtered for earlier), as spiders in sail are very rarely resting

exposed_m1 = glmmTMB(rest_exp ~ scale(latency) * space + scale(log(bask_vol)) +
                    (1|web/web_ind) +  (1|date), family = binomial,
                  resende_data_r)

summary(exposed_m1)

Anova(exposed_m1, type=2)

tidy(exposed_m1, effects="ran_pars", conf.int =T, conf.method = "Wald")

exp(fixef(exposed_m1)[[1]])

confint(exposed_m1)

#If active, taking care of young vs other

resende_data_a = resende_data %>%
  filter(active == 1) #only want active spiders
  
care_m1 = glmmTMB(care_young ~ scale(latency) + space + 
                       (1|web/web_ind) +  (1|date), family = binomial,
                     resende_data_a)
summary(care_m1) #Note the model does not converge if we include web volume

Anova(care_m1, type=2)

tidy(care_m1, effects="ran_pars", conf.int =T, conf.method = "Wald")

exp(fixef(care_m1)[[1]])

confint(care_m1)

#If active, walking vs other

walk_m1 = glmmTMB(walk ~ scale(latency) * space + scale(log(bask_vol)) +
                    (1|web/web_ind) +  (1|date), family = binomial,
                  resende_data_a)
summary(walk_m1)

Anova(walk_m1, type=2)

tidy(walk_m1, effects="ran_pars", conf.int =T, conf.method = "Wald")

exp(fixef(walk_m1)[[1]])

confint(walk_m1)

#Figures####

#Figures 1 & 2 are explanatory figures rather than data figures

#Figure 4, Latency by web location

#basket vs sail
fig4a = ggplot(resende_data, aes(x = latency, y = sail)) +
  geom_jitter(height = 0.01, width = 0.01) + theme_classic()+
  xlab("Seconds to move after air puff") + ylab("Chance of being out of basket")+
  stat_smooth(method = "glm", se=T, method.args = list(family = "binomial"), col="black")

#edge vs middle basket
fig4b = ggplot(resende_data, aes(x = latency, y = edge_b)) +
  geom_jitter(height = 0.01, width = 0.01) + theme_classic()+
  xlab("Seconds to move after air puff") + ylab("Chance of being on edge of basket")+
  stat_smooth(method = "glm", se=T, method.args = list(family = "binomial"), col="black")

grid.arrange(fig3a, fig3b, ncol=2)

#Figure 5, Latency on activity

#resting vs active
fig5a = ggplot(resende_data, aes(x = latency, y = active)) +
  geom_jitter(height = 0.01, width = 0.01) + theme_classic()+
  xlab("Seconds to move after air puff") + ylab("Chance of being active")+
  stat_smooth(method = "glm", se=T, method.args = list(family = "binomial"), col="black")

fig5b = ggplot(resende_data_r, aes(x = latency, y = rest_exp)) +
  geom_jitter(height = 0.01, width = 0.01) + theme_classic()+
  xlab("Seconds to move after air puff") + ylab("Chance of resting in exposed position")+
  stat_smooth(method = "glm", se=T, method.args = list(family = "binomial"), col="black")

fig5c = ggplot(resende_data_a, aes(x = latency, y = care_young)) +
  geom_jitter(height = 0.01, width = 0.01) + theme_classic()+
  xlab("Seconds to move after air puff") + ylab("Chance of taking care of young compared to other activity")+
  stat_smooth(method = "glm", se=T, method.args = list(family = "binomial"), col="black")

fig5d = ggplot(resende_data_a, aes(x = latency, y = walk)) +
  geom_jitter(height = 0.01, width = 0.01) + theme_classic()+
  xlab("Seconds to move after air puff") + ylab("Chance of walking compared to other activity")+
  stat_smooth(method = "glm", se=T, method.args = list(family = "binomial"), col="black")

grid.arrange(fig5a, fig5b, fig5c, fig5d, ncol=2)

#Figure in Appendix 2, Web volume vs in/out basket

appen2 = ggplot(resende_data, aes(x = (bask_vol),y = sail)) +
  geom_jitter(height = 0.01, width = 0.01) + theme_classic()+
  xlab("Web basket volume (m³)") + ylab("Chance of being out of basket")+
  stat_smooth(method = "glm", se=T, method.args = list(family = "binomial"), col="black")

#####END OF CODE####
