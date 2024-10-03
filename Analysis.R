library(ggplot2)
ggplot(data = slf_na, aes(x = Tree_DBH_cm, y = Number_covered_masses_seen)) + geom_point() +labs(x = "Tree DBH in cm", y = "Number of covered masses")
ggsave("images/tree-dbh-vs-number-of-masses.png", height = 5, width = 5)
slf_dbh_mass <- lm(Number_covered_masses_seen ~ Tree_DBH_cm, data = slf_na)
shapiro.test(slf_na$Tree_DBH_cm)
shapiro.test(slf_na$Number_covered_masses_seen)
slf_na$Number_covered_masses_seen_log <- log(slf_na$Number_covered_masses_seen)
slf_na$Tree_DBH_cm_log <- log(slf_na$Tree_DBH_cm)
shapiro.test(slf_na$Tree_DBH_cm_log)
shapiro.test(slf_na$Number_covered_masses_seen_log)



summary(slf_dbh_mass)



#READING DATA
slf <-read.csv("data/fecundity_data_full_v1_3.csv") 
slf_na <- na.omit(slf)

#Attempts to normalize and transform the data
shapiro.test(slf_na$Tree_DBH_cm)
shapiro.test(slf_na$Number_covered_masses_seen)
slf_na$Number_covered_masses_seen_log <- log(slf_na$Number_covered_masses_seen)
slf_na$Tree_DBH_cm_log <- log(slf_na$Tree_DBH_cm)
shapiro.test(slf_na$Tree_DBH_cm_log)
shapiro.test(slf_na$Number_covered_masses_seen_log)


#CORRELATION
cor.test(slf_na$Number_covered_masses_seen, slf_na$Tree_DBH_cm, method  = "spearman")# Correlation for Height and DBH

#LOGISTIC REGRESSION
slf_na$binomhigh <- ifelse(slf_na$high_masses == "y", 1, 0) #Binomial Preparation
high_dbh.glm <- glm(binomhigh ~ Tree_DBH_cm, data = slf_na, family = binomial(link = logit)) #GLM for Height and DBH Logistic Regression
summary(high_dbh.glm)
anova(high_dbh.glm, test = "Chi") #Logistic Regression


#RANDOM EFFECTS ANOVA
library(nlme)
slf_species_treedbh <- lme(fixed = Tree_DBH_cm_log ~ 1, random = ~1|Tree_Species, data = slf_na) #Alt hypothesis for Random Effects ANOVA
library(car)
leveneTest(data = slf_na, Tree_DBH_cm ~ Tree_Species, center = mean) #homogeneity of Variance
shapiro.test(resid(slf_species_treedbh)) #Shapiro Test for Normality
slf_species_treedbh_gls <- gls(Tree_DBH_cm_log ~ 1, data = slf_na, method = "REML") #Null hypothesis for Random Effects ANOVA
anova(slf_species_treedbh, slf_species_treedbh_gls) #ANOVA
varcomp <- VarCorr(slf_species_treedbh) #Calculating Variance Components
varcomp
varamong <- as.numeric(varcomp[1,1])
varwithin <- as.numeric(varcomp[2,1])
repeatability <- varamong / (varamong + varwithin)
repeatability


#FISHER'S EXACT TEST #Contingency Table Made in Excel, then imported
Contingency <- read.csv("data/Contingency.csv")
cont <-data.frame(Yes = c(Contingency$Count.of.Yes), No = c(Contingency$Count.of.No), row.names = c(Contingency$Tree_Species)) #making Contingency Table
fisher.test(cont, workspace = 2e9) #Fisher's Exact Test, workspace = 2e9 was due to small workspace size. It is tied to RAM, but I do not know how much it needs.



ggplot(slf_na, aes(x=Tree_DBH_cm, y = binomhigh)) + geom_point() + geom_smooth(method = glm, se = FALSE, methods = list(family = binomial)) + labs(x= "Tree Diameter at Breast Height (cm)", y = "Occurance of inaccessible Egg Cases")
ggsave("images/corr-inaccessible-egg-cases-dbh.png", height = 5, width = 5)
ggplot(data = slf_na, aes(x=Tree_Species, fill = high_masses)) +geom_bar(stat = "count") + theme(axis.text.x = element_text(angle=90)) + xlab("Species of Tree") + ylab("Count of Masses found")
ggsave("images/high-masses-and-species.png", height = 5, width = 5)
ggplot(data = slf_na, aes(x=Tree_DBH_cm)) + geom_histogram(binwidth = 5) + labs(x="Tree DBH in cm", y = "Frequency")
ggsave("images/freq-tree-dbh.png", height = 5, width = 5)
