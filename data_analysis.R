#analysis of pea plant data

library(xlsx)
pea <- read.xlsx('project_data.xlsx', sheetIndex = 1)

#some preprocessing
pea$TrtCoke <- factor(pea$TrtCoke)
pea$PF <- factor(pea$PF)
pea$Block <- factor(pea$Block)

#some initial data analysis
head(pea)

round(prop.table(table(interaction(pea$TrtCoke, pea$PF),pea$Germinate),1),2)


#A quick assessment of germination rates
pea$trtcode <- interaction(pea$TrtCoke,pea$PF)
props <- tapply(pea$Germinate,pea$trtcode, sum)

#do a chi-square test of independence
xt <- table(pea$trtcode, pea$Germinate)
chisq.test(xt)
barplot(props/16, col = "green3", main = "Germination Rates by Treatment", names.arg = c("DC/MG","W/MG","WDC/MG","DCN","WN","WDC/N"), ylab = "Proportion Germinated")
abline(h = 0)
text(.7, .07, round(props[1]/16,2), cex = 2, col = "white", font = 2)
text(.7 + 1.2, .07, round(props[2]/16,2), cex = 2, col = "white", font = 2)
text(.7 + 1.2*2, .07, round(props[3]/16,2), cex = 2, col = "white", font = 2)
text(.7 + 1.2*3, .07, round(props[4]/16,2), cex = 2, col = "white", font = 2)
text(.7 + 1.2*4, .07, round(props[5]/16,2), cex = 2, col = "white", font = 2)
text(.7 + 1.2*5, .07, round(props[6]/16,2), cex = 2, col = "white", font = 2)
text(.7 + 1.2*6, .07, round(props[7]/16,2), cex = 2, col = "white", font = 2)

####IMPORTANT HERE: FILTER OUT PLANTS THAT DIDNT GERMINATE FOR PLANT LEVEL ANALYSIS
library(dplyr)
pea <- dplyr::filter(pea, !Germinate == 0)


#some initial EDA
library(beanplot)
beanplot(Height ~ PF + TrtCoke, data = pea, col = c('lawngreen','forestgreen'), ylab = "Plant Height (mm)")
title("Heights by Treatment Combinations")

#some summary statistics
tapply(pea$Height, interaction(pea$TrtCoke,pea$PF), mean)
tapply(pea$Height, interaction(pea$TrtCoke,pea$PF), sd)

tapply(pea$Height, interaction(pea$TrtCoke,pea$PF), summary)

#this treats things at the plant level, so we need different Sums of Squares
library(car)

#anova model
m1 <- lm(Height ~ PF * TrtCoke * Block, data = pea)
summary(m1)
Anova(m1, type = "II")
AIC(m1)

m2 <- lm(Height ~ PF * TrtCoke + Block, data = pea)
summary(m2)
Anova(m2, type = 'II')
AIC(m2)


m3 <- lm(Height ~ PF + TrtCoke + Block, data = pea)
summary(m3)
Anova(m3, type = "II")
AIC(m3)
library(effects)
plot(allEffects(m3))


m4 <- lm(Height ~ TrtCoke, data = pea)
summary(m4)
Anova(m4, type = "II")
AIC(m4)


#model with cups as a random effect
library(lme4);library(lmerTest)
pea$Cup_Rand <- interaction(pea$Cup,pea$TrtCoke,pea$PF)



lmer1 <- lmer(Height ~ TrtCoke * PF * Block + (1|Cup_Rand), data = pea )
summary(lmer1)
Anova(lmer1, type = "II")

#fit additive model with interactions between 
lmer2 <- lmer(Height ~ TrtCoke * PF + (1|Block) + (1|Cup_Rand), data = pea)
summary(lmer2)
Anova(lmer2, type = "II")
plot(allEffects(lmer2))
##################################this is the model that we chose 


#fit main effects additive
#fit additive model
lmer3 <- lmer(Height ~ TrtCoke + PF + Block + (1|Cup_Rand), data = pea)
summary(lmer3)
Anova(lmer3, type = "II")
plot(allEffects(lmer3))

#lme_version
library(nlme)
lme.1 <- lme(Height ~ TrtCoke + PF + Block, random = ~1|Cup_Rand, data = pea)

library(multcomp)
q <- glht(lmer2, linfct = mcp(TrtCoke = "Tukey"))
xtable(cld(q))


####WE could instead average over the cups ###

#reload pea dataset in
pea <- read.xlsx('project_data.xlsx', sheetIndex = 1)

#some preprocessing
pea$TrtCoke <- factor(pea$TrtCoke)
pea$PF <- factor(pea$PF)
pea$Block <- factor(pea$Block)

#now we aggregate by the means
library(magrittr)
pea_av <- pea %>% group_by(interaction(Block,Cup,TrtCoke,PF)) %>% 
                summarise(mean_Height = mean(Height))

pea_av <- aggregate(pea$Height, list(pea$Block,pea$Cup,pea$TrtCoke,pea$PF), mean)
names(pea_av) <- c("Block","Cup","TrtCoke","PF","AvHeight")
pea_av$Block <- factor(pea_av$Block)
pea_av$TrtCoke <- factor(pea_av$TrtCoke)
pea_av$PF <- factor(pea_av$PF)

#then the model is:
m1 <- lm(AvHeight ~ Block * TrtCoke * PF, data = pea_av)
anova(m1)
summary(m1)

##Post-Hocs from the Tukey Test

#pairwise comparisons
library(stats)
TukeyHSD(lmer3)

#predicted means
library(lmerTest) 
lsmeansLT(lmer3) #these are the same as what you'd find in the lsmeans


library()










