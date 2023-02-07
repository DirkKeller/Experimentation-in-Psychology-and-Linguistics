library(dplyr)
library(ggplot2)
library(lme4)

selfies <- read.csv("C:/Users/DK/Desktop/Experimentation in Psychology and Linguistics/Lab 3 Mixed design/selfies.csv")
str(selfies)

################################################## Part 1 ##############################################

# eliminate trials with missing values
c.selfies <- na.omit(selfies)
nrow(selfies)-nrow(c.selfies)

# prepare data set for female/male condition of boring measures
self.gen.bor <- c.selfies %>% group_by(ResponseId, StimGender) %>% summarise(Boring = mean(Boring))
female.bor <- self.gen.bor$Boring[self.gen.bor$StimGender == 'Female']
male.bor <-  self.gen.bor$Boring[self.gen.bor$StimGender == 'Male']

# check assumptions
shapiro.test(female.bor)
shapiro.test(male.bor)
hist(female.bor)
hist(male.bor)

# transform data into zscores
Zscore = (self.gen.bor$Boring - mean(self.gen.bor$Boring))/sd(self.gen.bor$Boring)
z.self.gen.bor <- cbind(self.gen.bor, data.frame(Zscore))

z.female.bor <- z.self.gen.bor$Zscore[z.self.gen.bor$StimGender == 'Female']
z.male.bor <-  z.self.gen.bor$Zscore[z.self.gen.bor$StimGender == 'Male']

hist(z.female.bor)
hist(z.male.bor)

# significance test (parametric)
t.test(z.female.bor, z.male.bor, paired = TRUE, var.equal = TRUE)

# Boring categorization,
BoringYesNo <- selfies$Boring
selfies <- cbind(selfies, data.frame(BoringYesNo))
selfies$BoringYesNo[selfies$Boring < 3] <- 0
selfies$BoringYesNo[selfies$Boring > 3] <- 1
selfies$BoringYesNo[selfies$Boring == 3] <- NaN

c.selfies <- na.omit(selfies)

# sum contrast coding (different values)
#conStimGender <- c.selfies$StimGender
#c.selfies <- cbind(c.selfies, data.frame(conStimGender))
#c.selfies$conStimGender[c.selfies$StimGender == 'Female'] <- 1
#c.selfies$conStimGender[c.selfies$StimGender == 'Male'] <- -1

m1 <- glmer(BoringYesNo ~ 1 + (1 | ResponseId), c.selfies, family = binomial(link = "logit"))
summary(m1)
m2 <- glmer(BoringYesNo ~ 1 + StimGender + (1 |ResponseId), c.selfies, family = binomial(link = "logit"))
summary(m2)
m3 <- glmer(BoringYesNo ~ 1 + StimGender + (1 + StimGender |ResponseId), c.selfies, family = binomial(link = "logit"))
summary(m3)

anova(m1,m2)
anova(m2,m3)

# Results are consistent with the paired t-test, both yield a significant effect of stimulus gender.

# for question 3
m4 <- glmer(BoringYesNo ~ 1 + StimGender * Gender + (1 + StimGender | ResponseId), c.selfies, family = binomial(link = "logit"))
print(summary(m4))  
m5 <- glmer(BoringYesNo ~ 1 + StimGender * Gender + (1 + StimGender + Gender| ResponseId), c.selfies, family = binomial(link = "logit"))
print(summary(m5))  
m6 <- glmer(BoringYesNo ~ 1 + StimGender * Gender + (1 + StimGender * Gender|ResponseId), c.selfies, family = binomial(link = "logit"))
print(summary(m6)) 

anova(m4,m5)

rm(self.gen.bor, z.self.gen.bor, female.bor, male.bor, z.female.bor, z.male.bor, Zscore, BoringYesNo, conStimGender, m1, m2, m3,m5, m6)


################################################## Part 2 ##############################################
# T-test does not correct for dependencies of subjects in the data, other trends that might be hidden in the aggregated data might be relvant for prediction (e.g. Simon paradoxon)
# Other variables might be relvant in the prediction, such was whether the judge is male or female, age, sexual preferences, type of socialmedia platform (e.g. dating website vs. twitter)
# binary transformation gives a lot of information away, continuity, fine scaledness etc.
# The generalized linear model improves on the t-test with respect to confound controls, dependencies and non aggregated data (that alltogether might affect the significance value).
# However, both models may be still a too strong oversimplification, that does not adress other relevant factors.

################################################## Part 3 ##############################################
novel_selfies <- read.csv("C:/Users/DK/Desktop/Experimentation in Psychology and Linguistics/Lab 3 Mixed design/novel_data.csv")
c.novel_selfies <- na.omit(novel_selfies)

m4 <- glmer(BoringYesNo ~ 1 + StimGender * Gender + (1 + StimGender | ResponseId), c.selfies, family = binomial(link = "logit"))
print(summary(m4))  

ResponseId <- c(1:400)
novel_selfies <- cbind(novel_selfies, data.frame(ResponseId))
prob.GeStim <- simulate(m4, newdata = novel_selfies, allow.new.levels = TRUE)

drawprobabilities <- function(probs) {
  if (length(probs) != 400) {
    print("Wrong length of the vector of calculated probabilities. Should be 400 data points.")
  } else {
    matrixprobs <- matrix(ifelse(probs > 0.5, "X", ""), nrow = 20)
    x <- rep(NA, 400)
    y <- rep(NA, 400)
    k <- 1
    for (i in 1:20) {
      for (j in 1:20) {
        if (matrixprobs[i, j] == "X") {
          y[k] <- i
          x[k] <- j
          k <- k + 1
          4
        }
      }
    }
    plot(x, y, xlim = c(0, 40), ylim = c(0, 40), pch = 15)
  }
}
drawprobabilities(prob.GeStim)

rm(m4, prob.GeStim, ResponseId,drawprobabilities)


################################################## Part 4 ##############################################

library(ordinal) #install.packages('ordinal')
selfies <- read.csv("C:/Users/DK/Desktop/Experimentation in Psychology and Linguistics/Lab 3 Mixed design/selfies.csv")
c.selfies <- na.omit(selfies)

# model comprison
m1 <- clmm(factor(Boring) ~ 1 + StimGender + (1 | ResponseId), link = "probit", c.selfies)
summary(m1)
m2 <- clmm(factor(Boring) ~ 1 + StimGender + (1 + StimGender | ResponseId), link = "probit", c.selfies)
summary(m2)

m3 <- clmm(factor(Boring) ~ 1 + StimGender * Gender + (1 + StimGender | ResponseId), link = "probit", c.selfies)
summary(m3)
m4 <- clmm(factor(Boring) ~ 1 + StimGender * Gender + (1 + StimGender * Gender | ResponseId), link = "probit", c.selfies)
summary(m4)

anova(m1, m2, m3, m4)
# model 2 is the best model here, interaction between StimGender and Gender does not explain more variance.

# Additional confounder with more than 3 levels
m0.1 <- clmm(factor(Boring) ~ 1 + StimGender + (1 + StimGender | ResponseId) + (1 + StimGender | Dur), link = "probit", c.selfies) #*
m0.2 <- clmm(factor(Boring) ~ 1 + StimGender + (1 + StimGender | ResponseId) + (1 + StimGender | Age), link = "probit", c.selfies)
m0.3 <- clmm(factor(Boring) ~ 1 + StimGender + (1 + StimGender | ResponseId) + (1 + StimGender | Country), link = "probit", c.selfies)
m0.4 <- clmm(factor(Boring) ~ 1 + StimGender + (1 + StimGender | ResponseId) + (1 + StimGender | Socialmedia), link = "probit", c.selfies)
m0.5 <- clmm(factor(Boring) ~ 1 + StimGender + (1 + StimGender | ResponseId) + (1 + StimGender | Selfietaking), link = "probit", c.selfies)
summary(m0.1)
summary(m0.2)
summary(m0.3)
summary(m0.4)
summary(m0.5)
anova(m2, m0.1)
anova(m2, m0.2)
anova(m2, m0.3)
anova(m2, m0.4) # *for fixed effects
anova(m2, m0.5)

# When accounting for possible confounding variables with more than 3 levels, StimGender is still significant in all 4 models and the models with a more complex random structure are not able to significantly account for more variance than the base line model (m2)

# Including them as fixed effects
m0.1 <- clmm(factor(Boring) ~ 1 + StimGender * Dur + (1 + StimGender | ResponseId), link = "probit", c.selfies)
m0.2 <- clmm(factor(Boring) ~ 1 + StimGender * Age + (1 + StimGender | ResponseId), link = "probit", c.selfies) # interaction*
m0.3 <- clmm(factor(Boring) ~ 1 + StimGender * Country + (1 + StimGender | ResponseId), link = "probit", c.selfies)
m0.4 <- clmm(factor(Boring) ~ 1 + StimGender * Socialmedia + (1 + StimGender | ResponseId), link = "probit", c.selfies)
m0.5 <- clmm(factor(Boring) ~ 1 + StimGender * Selfietaking + (1 + StimGender | ResponseId), link = "probit", c.selfies)

# Additional confounder with less than 3 levels
m0.6 <- clmm(factor(Boring) ~ 1 + StimGender * Gender + (1 + StimGender | ResponseId), link = "probit", c.selfies)
m0.7 <- clmm(factor(Boring) ~ 1 + StimGender * Tilt + (1 + StimGender | ResponseId), link = "probit", c.selfies) # **StimGender, *interaction effect
m0.8 <- clmm(factor(Boring) ~ 1 + StimGender * Distance + (1 + StimGender | ResponseId), link = "probit", c.selfies) # ** Distance, ***interaction effect
m0.9 <- clmm(factor(Boring) ~ 1 + StimGender * Eyes + (1 + StimGender | ResponseId), link = "probit", c.selfies) # ***StimGender

summary(m0.5)
summary(m0.6)
summary(m0.7)
summary(m0.8)
summary(m0.9)
anova(m2, m0.5, m0.6, m0.7, m0.8, m0.9)

# When including possible confounds as randome effects, the significance of StimGender is not affected. However, when including the less than 3 level variables as fixed effects StimGender is often significant. Only for Distance (and Age, Selfietaking, Country) the effect of StimGender was not significant anymore. Importantly the interaction effect of Tilt and Distance was significant. Thus, generally speaking male selfies are generally significantly differ from female ones with respect to boringness? However, Tilt and Distance might be contribute to the difference hence the difference might be restricted and driven by these specific factors. 

m1.1 <- clmm(factor(Boring) ~ 1 + StimGender + (1 + StimGender | Eyes), c.selfies)
m1.2 <- clmm(factor(Boring) ~ 1 + StimGender + (1 + StimGender | Dur), c.selfies)
m1.3 <- clmm(factor(Boring) ~ 1 + StimGender + (1 + StimGender | Age), c.selfies)
m1.4 <- clmm(factor(Boring) ~ 1 + StimGender + (1 + StimGender | Country), c.selfies)
m1.5 <- clmm(factor(Boring) ~ 1 + StimGender + (1 + StimGender | Gender), c.selfies)
m1.6 <- clmm(factor(Boring) ~ 1 + StimGender + (1 + StimGender | Socialmedia), c.selfies)
m1.7 <- clmm(factor(Boring) ~ 1 + StimGender + (1 + StimGender | Selfietaking), c.selfies)
m1.8 <- clmm(factor(Boring) ~ 1 + StimGender + (1 + StimGender | Tilt), c.selfies)
m1.9 <- clmm(factor(Boring) ~ 1 + StimGender + (1 + StimGender | Distance), c.selfies)
m1.0 <- clmm(factor(Boring) ~ 1 + StimGender + (1 + StimGender | ResponseId), c.selfies)

m2.1 <- clmm(factor(Boring) ~ 1 + StimGender * Dur + (1 + StimGender | Dur), c.selfies)
m2.2 <- clmm(factor(Boring) ~ 1 + StimGender * Age + (1 + StimGender | Age), c.selfies) #*
m2.3 <- clmm(factor(Boring) ~ 1 + StimGender * Country + (1 + StimGender | Country), c.selfies)
m2.4 <- clmm(factor(Boring) ~ 1 + StimGender * Gender + (1 + StimGender | Gender), c.selfies)
m2.5 <- clmm(factor(Boring) ~ 1 + StimGender * Socialmedia + (1 + StimGender | Socialmedia), c.selfies)
m2.6 <- clmm(factor(Boring) ~ 1 + StimGender * Selfietaking + (1 + StimGender | Selfietaking), c.selfies)
m2.7 <- clmm(factor(Boring) ~ 1 + StimGender * Tilt + (1 + StimGender | ResponseId), c.selfies)
m2.8 <- clmm(factor(Boring) ~ 1 + StimGender * Distance + (1 + StimGender | Distance), c.selfies) #*** 
m2.9 <- clmm(factor(Boring) ~ 1 + StimGender * Eyes + (1 + StimGender | Eyes), c.selfies)
m2.0 <- clmm(factor(Boring) ~ 1 + StimGender * ResponseId (1 + StimGender | ResponseId), c.selfies) #***

resid(m2)
anova(m0.1, m0.2, m0.3, m0.4, m0.5, m0.6, m0.7, m0.8, m0.9, m0.0)

summary(m0.1)
summary(m0.2)
summary(m0.3)
summary(m0.4)
summary(m0.5)
summary(m0.6)
summary(m0.7)
summary(m0.8)
summary(m0.9)
summary(m0.0)

anova(m1.1, m1.2, m1.3, m1.4, m1.5, m1.6, m1.7, m1.8, m1.9, m0.0)

summary(m1.1)
summary(m1.2)
summary(m1.3)
summary(m1.4)
summary(m1.5)
summary(m1.6)
summary(m1.7)
summary(m1.8)
summary(m1.9)
summary(m1.0)

anova(m2.1, m2.2, m2.3, m2.4, m2.5, m2.6, m2.7, m2.8, m2.9, m2.0)

summary(m2.1)
summary(m2.2)
summary(m2.3)
summary(m2.4)
summary(m2.5)
summary(m2.6)
summary(m2.7)
summary(m2.8)
summary(m2.9)
summary(m2.0)

rm(m0.1, m0.2, m0.3, m0.4, m0.5, m0.6, m0.7, m0.8, m0.9, m0.0, m1.1, m1.2, m1.3, m1.4, m1.5, m1.6, m1.7, m1.8, m1.0)

################################################## Part 5 ##############################################

library(ordinal) #install.packages('ordinal')
# eliminate trials with missing values
selfies <- read.csv("C:/Users/DK/Desktop/Experimentation in Psychology and Linguistics/Lab 3 Mixed design/selfies.csv")
c.selfies <- na.omit(selfies)

# link = "probit"
m1 <- clmm(factor(Boring) ~ StimGender + (1|ResponseId), c.selfies)
summary(m1)

# female
O1 <- pnorm(-16.540, mean = 0, sd = 1)
O2 <- pnorm(-6.176, mean = 0, sd = 1) - pnorm(-16.540, mean = 0, sd = 1)
O3 <- pnorm(0.356, mean = 0, sd = 1) - pnorm(-6.176, mean = 0, sd = 1)
O4 <- pnorm(16.348, mean = 0, sd = 1) - pnorm(0.356, mean = 0, sd = 1)
O5 <- pnorm(16.348, mean = 0, sd = 1, lower.tail = FALSE)
O1; O2; O3; O4; O5

# males
O1 <- pnorm(-16.540, mean = -0.66163, sd = 1)
O2 <- pnorm(-6.176, mean = -0.66163, sd = 1) - pnorm(-16.540, mean = 0, sd = 1)
O3 <- pnorm(0.356, mean = -0.66163, sd = 1) - pnorm(-6.176, mean = 0, sd = 1)
O4 <- pnorm(16.348, mean = -0.66163, sd = 1) - pnorm(0.356, mean = 0, sd = 1)
O5 <- pnorm(16.348, mean = -0.66163, sd = 1, lower.tail = FALSE)
O1; O2; O3; O4; O5


O1 <- pnorm(-2.53856, mean = 0, sd = 1)
O2 <- pnorm(-0.88367, mean = 0, sd = 1) - pnorm(-2.53856, mean = 0, sd = 1)
O3 <- pnorm(0.05039, mean = 0, sd = 1) - pnorm(-0.88367, mean = 0, sd = 1)
O4 <- pnorm(2.57149, mean = 0, sd = 1) - pnorm(0.05039, mean = 0, sd = 1)
O5 <- pnorm(2.57149, mean = 0, sd = 1, lower.tail = FALSE)
O1; O2; O3; O4; O5


O1 <- pnorm(-2.53856, mean = -0.66163, sd = 1)
O2 <- pnorm(-0.88367, mean = -0.66163, sd = 1) - pnorm(-2.53856, mean = 0, sd = 1)
O3 <- pnorm(0.05039, mean = -0.66163, sd = 1) - pnorm(-0.88367, mean = 0, sd = 1)
O4 <- pnorm(2.57149, mean = -0.66163, sd = 1) - pnorm(0.05039, mean = 0, sd = 1)
O5 <- pnorm(2.57149, mean = -0.66163, sd = 1, lower.tail = FALSE)
O1; O2; O3; O4; O5