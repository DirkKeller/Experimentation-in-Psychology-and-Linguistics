library(dplyr)
library(ggplot2)
library(broom)
library(ggpubr)

itemdata <- read.csv("C:/Users/DK/Desktop/Experimentation in Psychology and Linguistics/Lab 1 Data manipulation, factorial design/MALD1_SelectedItemData.csv", sep = "")
str(itemdata)
head(itemdata)

responsedata <- read.csv("C:/Users/DK/Desktop/Experimentation in Psychology and Linguistics/Lab 1 Data manipulation, factorial design/MALD1_SelectedResponseData.csv", sep = "")
str(responsedata)

head(responsedata)

g1 <- ggplot(responsedata, aes(RT))
g1 <- g1 + geom_bar()

g1

#### part 1
combined_dataframe <- left_join(responsedata,itemdata)

discriptives_all <- combined_dataframe %>%
  filter(RT > 0 | FreqCOCAspok >= 0) %>%
  group_by(IsWord) %>%
  summarise(Mfreq = mean(FreqCOCAspok), SDfreq = sd(FreqCOCAspok), Mrt = mean(RT), SDrt = sd(RT))

discriptives_s15351 <- combined_dataframe %>%
  filter(RT > 0 | FreqCOCAspok >= 0) %>%
  filter(Subject == 15351) %>%
  group_by(IsWord) %>%
  summarise(Mfreq  = mean(FreqCOCAspok), SDfreq = sd(FreqCOCAspok), Mrt = mean(RT), SDrt = sd(RT))

discriptives_s16854 <- combined_dataframe %>%
  filter(RT > 0 | FreqCOCAspok >= 0) %>%
  filter(Subject == 16854) %>%
  group_by(IsWord) %>%
  summarise(Mfreq  = mean(FreqCOCAspok), SDfreq = sd(FreqCOCAspok), Mrt = mean(RT), SDrt = sd(RT))

discriptives_s170373 <- combined_dataframe %>%
  filter(RT > 0 | FreqCOCAspok >= 0) %>%
  filter(Subject == 170373) %>%
  group_by(IsWord) %>%
  summarise(Mfreq  = mean(FreqCOCAspok), SDfreq= sd(FreqCOCAspok), Mrt = mean(RT), SDrt = sd(RT))

word_s170373_hist <- combined_dataframe %>%
  filter(RT > 0 | FreqCOCAspok >= 0) %>%
  filter(Subject == 170373) %>%
  filter(IsWord == TRUE)

hist(word_s170373_hist$RT)

nonword_s170373_hist <- combined_dataframe %>%
  filter(RT > 0 | FreqCOCAspok >= 0) %>%
  filter(Subject == 170373) %>%
  filter(IsWord == FALSE)

hist(nonword_s170373_hist$RT)

###### part 2
real_words <- combined_dataframe[combined_dataframe$IsWord == TRUE,]
pseudo_words <- combined_dataframe[combined_dataframe$IsWord == FALSE,]

subject1_real <- real_words[real_words$Subject == 15351,]
subject1_pseudo <- pseudo_words[pseudo_words$Subject == 15351,]



cohend <- function(x1, x2) {
  n <- length(x1)
  s <- sqrt( ((n-1) * var(x1) + (n-1) * var(x2)) / (2*n - 2) )
  d <- (mean(x1) - mean(x2)) / s
  return(d)
}

#all
real_words <- combined_dataframe[combined_dataframe$IsWord == TRUE,]
pseudo_words <- combined_dataframe[combined_dataframe$IsWord == FALSE,]
cohend(real_words$RT, pseudo_words$RT)
#s15351
subject1_real <- real_words[real_words$Subject == 15351,]
subject1_pseudo <- pseudo_words[pseudo_words$Subject == 15351,]
cohend(subject1_real$RT, subject1_pseudo$RT)
#s16854
subject2_real <- real_words[real_words$Subject == 16854,]
subject2_pseudo <- pseudo_words[pseudo_words$Subject == 16854,]
cohend(subject2_real$RT, subject2_pseudo$RT)
#s170373
subject3_real <- real_words[real_words$Subject == 170373,]
subject3_pseudo <- pseudo_words[pseudo_words$Subject == 170373,]
cohend(subject3_real$RT, subject3_pseudo$RT)
#artificial
word_15292 <- c(2206, 1583, 1154, 1010, 865, 931, 1129, 683, 820, 1132, 1049, 1211, 1261, 957, 1058,
                790, 851, 1908, 1504, 1400, 924)
pseudoword_15292 <- c(677, 949, 889, 881, 917, 769, 772, 922, 1944, 881, 976, 1087, 1252, 914, 1277,
                      825, 1295, 1336, 788, 885, 932)
cohend(word_15292, pseudoword_15292)

### part 3
tcalculation <- function(x1, x2){
  n <- length(x1)
  SE <- sqrt(var(x1)/n + var(x2)/n)
  t <- (mean(x1) - mean(x2)) / SE
  return(t)
}

tcalculation(real_words$RT, pseudo_words$RT)
tcalculation(subject1_real$RT, subject1_pseudo$RT)
tcalculation(word_15292, pseudoword_15292)

shapiro.test(real_words$RT[1:5000]); shapiro.test(pseudo_words$RT[1:5000])
t.test(real_words$RT, pseudo_words$RT) #crossvalidation; should be paired but is not

###part 4
generate.t <- function(){
  
    mysample <- rnorm(20, mean = 0, sd = 10)
    tvalue <- mean(mysample)/sqrt((var(mysample)/20))
    tvalue
}

simulated_data <- rep(NA, 2e+05)
  
for (i in 1:length(simulated_data)){
    simulated_data[i] <- generate.t()
}


# Q-Q plot qqplot(qt(ppoints(200000), df=19), simulated_data) qqline(simulated_data, distribution=
                                                                     # function(p) qt(p, df=19)) Not used - see the png plot in this assignment
                                                                     # Histograms comparing t-values from simulated data and predicted based on the t-probability
                                                                     # distribution
par(mfrow = c(2, 1))
hist(simulated_data)
hist(qt(ppoints(2e+05), df = 19))

1 - pt(abs(tcalculation(word_15292, pseudoword_15292)), 40) # one tailed; two tailed (*2)

shapiro.test(word_15292); shapiro.test(pseudoword_15292)
t.test(word_15292, pseudoword_15292) #crossvalidation (two-tailed)
 
# normalization
par(mfrow = c(1, 2))
hist(pseudo_words$RT)
hist(real_words$RT)
hist(real_words$RT^2)
hist(real_words$RT^3)
hist(real_words$RT^-1)
hist(sqrt(real_words$RT))
hist(log10(real_words$RT))
hist(log(real_words$RT))

shapiro.test(sqrt(word_15292)); shapiro.test(sqrt(pseudoword_15292))
shapiro.test(log10(word_15292)); shapiro.test(log10(pseudoword_15292))
shapiro.test(word_15292^-1); shapiro.test(pseudoword_15292^-1)

1 - pt(abs(log2(tcalculation(log10(word_15292), log10(pseudoword_15292)))), 40)
1 - pt(abs(tcalculation(word_15292^-1, pseudoword_15292^-1)), 40)

### part 5 
aggregated_dataframe <- combined_dataframe %>%
  group_by(Subject, IsWord) %>%
  summarise(RT = mean(RT))

aggregated_real_words <- subset(aggregated_dataframe, aggregated_dataframe$IsWord == TRUE)
aggregated_pseudo_words <- subset(aggregated_dataframe, aggregated_dataframe$IsWord == FALSE)

# tcalculation(aggregated_real_words$RT, aggregated_pseudo_words$RT) # paired t-test (within-subject design)
# pt(abs(tcalculation(aggregated_real_words$RT, aggregated_pseudo_words$RT)), length(aggregated_dataframe)-1)

shapiro.test(aggregated_real_words$RT); shapiro.test(aggregated_pseudo_words$RT)
shapiro.test(aggregated_real_words$RT^-1); shapiro.test(aggregated_pseudo_words$RT^-1)

t.test(aggregated_real_words$RT, aggregated_pseudo_words$RT, paired = TRUE) # paired t-test (within-subject design)
t.test(aggregated_real_words$RT^-1, aggregated_pseudo_words$RT^-1, paired = TRUE) # paired t-test (within-subject design)

# part 6
# Since it is stated that 30 participants took place in the experiment (degrees of freedom = 29), 
# however 1198 degrees of freedom were reported in the experiment it can be assume that this number of observations-1 
# reflects the trails of each participant and not the trail-avergae for each participant. THus the analysis was carried 
# out on nonaggregated data. (1) This can be problematic, since a single trail often includes a low signal to noise ration, 
# such as in EEG measures. Therefore the randome error component of this measurement is quite high affecting reliability 
# and thus the representativeness of the analysis (e.g. high p-values might be obtain in one analysis, but not in reproduction
# of the analysis). Data should be aggregated to improve the signal to noise ratio and nivilate random error in the data set 
# (test theory assumtion is that the mean of truely randome measurement errors is 0). 
# (2) Similarly, as stated in Q5 analysis on trails can violate the assumption of independence, since participants
# tend to differ in reaction times and so there will be dependence in reaction times of a participant.
# Disclaimer: aggregated data has its own limitations (e.g. -> simpson paradox).

# part 7
# The question and description is confusionly phrased. If the goal is to ask whether sample size affects the p-value, then my answer is:
# Yes the p-value will be affected and it will be lower, because with a smaller sample size and a assumed normal distribution many values 
# will be sampled from the center. A samller sample size will lead to sampling error and an unrepresentative sample can result in a
# false negatives (type II) and a low statistical power (the chance to find a true positive). Thus the p-value here is more likely to be on average
# smaller than in Q4, however, can, since the samples lack representativity, result in large p- values (due to randomeness of sampling/chance).

word_sub12 <- rnorm(6, mean(aggregated_real_words$RT), sd(aggregated_real_words$RT))
pseudo_sub12 <- rnorm(6, mean(aggregated_pseudo_words$RT), sd(aggregated_pseudo_words$RT))

word_sub42 <- rnorm(21, mean(aggregated_real_words$RT), sd(aggregated_real_words$RT))
pseudo_sub42 <- rnorm(21, mean(aggregated_pseudo_words$RT), sd(aggregated_pseudo_words$RT))

p1 <- (1 - pt(tcalculation(word_sub12,pseudo_sub12), 10))*2
p2 <- (1 - pt(tcalculation(word_sub42,pseudo_sub42), 40))*2
p3 <- p1*0.5+p2*0.5

p1
p3

# uncertainty about the sample ouctome will inflate the p-value (higher than expect) resulting in a type I error (false positives)

# Linear models
m1 <- lm(RT ~ aIsWord, data=aggregated_dataframe) 
print(summary(m1))

m1 <- lm(RT ~ IsWord*ACC, data=aggregated_dataframe) 
print(summary(m1))

# part  8
library(survminer) #install.packages("survminer")

aggregated_dataframe <- combined_dataframe %>%
  group_by(Subject, IsWord, ACC) %>%
  summarise(RT = mean(RT))

mean.RT <- aggregated_dataframe %>%
           group_by(IsWord, ACC) %>%
           summarise(RT =mean(RT))

#mean.RT$IsWord <- c('Pseudoword', 'Pseudoword', 'Word', 'Word')
#mean.RT$ACC <- c('NoACC', 'ACC', 'NoACC', 'ACC')

two.way.plot <- ggplot(data=aggregated_dataframe, aes(x = IsWord, y = RT, group = ACC)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0)) +

  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.3, col='red') +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.RT, aes(x=IsWord, y=RT, col = 'red'), show.legend=FALSE) +
  facet_wrap(~ ACC) +
  
  theme_classic2() +
  labs(title = "Differences in RT in response to Word state and ACC",
       x = "Wordtype",
       y = "Reaction time (ms)")

two.way.plot

# part  9
library(AICcmodavg)# install.packages("AICcmodavg")

log.aggregated_dataframe <- combined_dataframe %>%
   group_by(Subject, IsWord, ACC, FreqCOCA, FreqGoogle, FreqSUBTLEX, FreqCOCAspok) %>%
   summarise(RT = mean(RT))#  summarise(RT = mean(RT))??

#log.aggregated_dataframe$IsWord <- as.factor(log.aggregated_dataframe$IsWord)

# LogRT (Log transformation)
log.aggregated_dataframe$RT <- log(log.aggregated_dataframe$RT)
log.aggregated_dataframe <- log.aggregated_dataframe %>% 
    filter(FreqSUBTLEX > 0 & FreqGoogle > 0 & FreqSUBTLEX > 0 & FreqCOCAspok > 0)

# Model estimation (raw frequencies)
LogRT.Model.FreqCOCA <- lm(RT ~ FreqCOCA, data=log.aggregated_dataframe) 
LogRT.Model.FreqGoogle <- lm(RT ~ FreqGoogle, data=log.aggregated_dataframe) 
LogRT.Model.FreqSUBTLEX <- lm(RT ~ FreqSUBTLEX, data=log.aggregated_dataframe) 
LogRT.Model.FreqCOCAspok <- lm(RT ~ FreqCOCAspok, data=log.aggregated_dataframe) 

summary(LogRT.Model.FreqCOCA)
summary(LogRT.Model.FreqGoogle)
summary(LogRT.Model.FreqSUBTLEX)
summary(LogRT.Model.FreqCOCAspok)

# sum of residuals
sum(abs(resid(LogRT.Model.FreqCOCA)))
sum(abs(resid(LogRT.Model.FreqGoogle)))
sum(abs(resid(LogRT.Model.FreqSUBTLEX)))
sum(abs(resid(LogRT.Model.FreqCOCAspok)))

Model.set <- list(LogRT.Model.FreqCOCA, LogRT.Model.FreqGoogle, LogRT.Model.FreqSUBTLEX, LogRT.Model.FreqCOCAspok)
Model.names <- c("FreCOCA", "FreqGoogle", "FreqSUBTLEX", "FreqCOCAspok")

# Akaike information criterion (AIC) test for model fit
# Calculates the information value of each model by balancing the variation explained against the number of parameters used.
aictab(Model.set, modnames = Model.names)

# NonLog Plots
par(mfrow=c(1,4))
plot(RT ~ FreqCOCA, data=log.aggregated_dataframe, xlab="FreqCOCA",ylab="Reaction time (ms)") +
  abline(LogRT.Model.FreqCOCA, col = "red")

plot(RT ~ FreqGoogle, data=log.aggregated_dataframe, xlab="FreqGoogle",ylab="Reaction time (ms)") + 
  abline(LogRT.Model.FreqCOCA, col = "red")

plot(RT ~ FreqSUBTLEX, data=log.aggregated_dataframe, xlab="FreqSUBTLEX",ylab="Reaction time (ms)") +
  abline(LogRT.Model.FreqCOCA, col = "red")

plot(RT ~ FreqCOCAspok, data=log.aggregated_dataframe, xlab="FreqCOCAspok",ylab="Reaction time (ms)") +
  abline(LogRT.Model.FreqCOCA, col = "red")

################################### Log frequencies #####################################
# Logfrequencies (Log transformation)
log.aggregated_dataframe[,4:7] <- log(log.aggregated_dataframe[,4:7])
#log.aggregated_dataframe[sapply(log.aggregated_dataframe, is.infinite)] <- 0

# Model estimation (log frequencies)
LogFreq.Model.FreqCOCA <- lm(RT ~ FreqCOCA, data=log.aggregated_dataframe) 
LogFreq.Model.FreqGoogle <- lm(RT ~ FreqGoogle, data=log.aggregated_dataframe) 
LogFreq.Model.FreqSUBTLEX <- lm(RT ~ FreqSUBTLEX, data=log.aggregated_dataframe) 
LogFreq.Model.FreqCOCAspok <- lm(RT ~ FreqCOCAspok, data=log.aggregated_dataframe) 

summary(LogFreq.Model.FreqCOCA)
summary(LogFreq.Model.FreqGoogle)
summary(LogFreq.Model.FreqSUBTLEX)
summary(LogFreq.Model.FreqCOCAspok)

# Sum of residuals per model
sum(abs(resid(LogFreq.Model.FreqCOCA)))
sum(abs(resid(LogFreq.Model.FreqGoogle)))
sum(abs(resid(LogFreq.Model.FreqSUBTLEX)))
sum(abs(resid(LogFreq.Model.FreqCOCAspok)))

Model.set <- list(LogFreq.Model.FreqCOCA, LogFreq.Model.FreqGoogle, LogFreq.Model.FreqSUBTLEX, LogFreq.Model.FreqCOCAspok)
Model.names <- c("FreCOCA", "FreqGoogle", "FreqSUBTLEX", "FreqCOCAspok")

# Akaike information criterion (AIC) test for model fit
# Calculates the information value of each model by balancing the variation explained against the number of parameters used.
aictab(Model.set, modnames = Model.names)

anova(LogFreq.Model.FreqCOCA, LogFreq.Model.FreqGoogle, LogFreq.Model.FreqSUBTLEX, LogFreq.Model.FreqCOCAspok)

# Log plots
par(mfrow=c(1,4))
plot(RT ~ IsWord * FreqCOCA, data=log.aggregated_dataframe, xlab="FreqCOCA",ylab="Reaction time (ms)") +
  abline(LogFreq.Model.FreqCOCA, col = "red")

plot(RT ~ IsWord * FreqGoogle, data=log.aggregated_dataframe, xlab="FreqGoogle",ylab="Reaction time (ms)") + 
  abline(LogFreq.Model.FreqCOCA, col = "red")

plot(RT ~ IsWord * FreqSUBTLEX, data=log.aggregated_dataframe, xlab="FreqSUBTLEX",ylab="Reaction time (ms)") +
  abline(LogFreq.Model.FreqCOCA, col = "red")

plot(RT ~ IsWord * FreqCOCAspok, data=log.aggregated_dataframe, xlab="FreqCOCAspok",ylab="Reaction time (ms)") +
  abline(LogFreq.Model.FreqCOCA, col = "red")
par(mfrow=c(1,1))

### part 10
expl.aggregated_dataframe <- combined_dataframe %>%
    group_by(Subject, IsWord, ACC, FreqCOCA, FreqGoogle, FreqSUBTLEX, FreqCOCAspok,
           NumSylls, NumPhones, NumMorphs, Duration) %>%
    summarise(RT = mean(RT))

expl.aggregated_dataframe <- expl.aggregated_dataframe %>% 
  filter(FreqSUBTLEX > 0 & FreqGoogle > 0 & FreqSUBTLEX > 0 & FreqCOCAspok > 0)
# log.aggregated_dataframe$IsWord <- as.factor(log.aggregated_dataframe$IsWord)
  
expl.aggregated_dataframe$RT <- log(expl.aggregated_dataframe$RT)
expl.aggregated_dataframe[,4:7] <- log(expl.aggregated_dataframe[,4:7])
# expl.aggregated_dataframe[sapply(expl.aggregated_dataframe, is.infinite)] <- 0

Model1.FreqGoogle <- lm(RT ~ FreqGoogle*NumSylls, data=expl.aggregated_dataframe) 
Model1.FreqSUBTLEX <- lm(RT ~ FreqSUBTLEX*NumSylls, data=expl.aggregated_dataframe) 

Model2.FreqGoogle <- lm(RT ~ FreqGoogle*Duration, data=expl.aggregated_dataframe) 
Model2.FreqSUBTLEX <- lm(RT ~ FreqSUBTLEX*IsWord*Duration, data=expl.aggregated_dataframe) 

Model3.FreqGoogle <- lm(RT ~ FreqGoogle*NumSylls*Duration, data=expl.aggregated_dataframe) 
Model3.FreqSUBTLEX <- lm(RT ~ FreqSUBTLEX*NumSylls*Duration, data=expl.aggregated_dataframe) 

Model4.FreqGoogle <- lm(RT ~ FreqGoogle*NumMorphs, data=expl.aggregated_dataframe) 
Model4.FreqSUBTLEX <- lm(RT ~ FreqSUBTLEX*IsWord*NumMorphs, data=expl.aggregated_dataframe) 

Model5.FreqGoogle <- lm(RT ~ FreqGoogle*NumPhones, data=expl.aggregated_dataframe) 
Model5.FreqSUBTLEX <- lm(RT ~ FreqSUBTLEX*IsWord*NumPhones, data=expl.aggregated_dataframe) 

Model5.FreqGoogle <- lm(RT ~ FreqGoogle*NumMorphs*NumPhones, data=expl.aggregated_dataframe) 
Model5.FreqSUBTLEX <- lm(RT ~ FreqSUBTLEX*IsWord*NumMorphs*NumPhones, data=expl.aggregated_dataframe) 

Model.set <- list(Model1.FreqGoogle, Model1.FreqSUBTLEX,
                  Model2.FreqGoogle, Model2.FreqSUBTLEX, 
                  Model3.FreqGoogle, Model3.FreqSUBTLEX,
                  Model4.FreqGoogle, Model4.FreqSUBTLEX,
                  Model5.FreqGoogle, Model5.FreqSUBTLEX)
Model.names <- c("Model1.FreqGoogle", "Model1.FreqSUBTLEX",
                 "Model2.FreqGoogle", "Model2.FreqSUBTLEX",
                 "Model3.FreqGoogle", "Model3.FreqSUBTLEX",
                 "Model4.FreqGoogle", "Model4.FreqSUBTLEX",
                 "Model5.FreqGoogle", "Model5.FreqSUBTLEX")

# Akaike information criterion (AIC) test for model fit
# Calculates the information value of each model by balancing the variation explained against the number of parameters used.
aictab(Model.set, modnames = Model.names)

summary(Model1.FreqGoogle)
summary(Model1.FreqSUBTLEX)
summary(Model2.FreqGoogle)
summary(Model2.FreqSUBTLEX)
summary(Model3.FreqGoogle)
summary(Model3.FreqSUBTLEX)
summary(Model4.FreqGoogle)
summary(Model4.FreqSUBTLEX)
summary(Model5.FreqGoogle)
summary(Model5.FreqSUBTLEX)

