#Summary Statistics
slf <- read.csv("data/fecundity_data_full_v1_3.csv") #load in csv
mean(na.omit(slf$Tree_DBH_cm)) #average of DBH of tree
median(na.omit(slf$Tree_DBH_cm)) #median of DBH of tree
var(na.omit(slf$Tree_DBH_cm)) #variance of DBH of tree
sd(na.omit(slf$Tree_DBH_cm)) #standard deviation of DBH of tree
range(na.omit(slf$Tree_DBH_cm)) #range of DBH of tree
IQR(na.omit(slf$Tree_DBH_cm)) #interquartile range of DBH of tree
100 * (sd(na.omit(slf$Tree_DBH_cm)) / mean(na.omit(slf$Tree_DBH_cm))) #coefficient of variation
slf_na <- na.omit(slf)
library(ggplot2)
ggplot(data = slf_na, aes(x=Tree_Species)) + geom_bar() + theme(axis.text.x = element_text(angle=90)) + xlab("Species of Tree")
ggsave("images/tree-count-species.png", height = 5, width = 5)
ggplot(data = slf_na, aes(x=Tree_Species, y=Tree_DBH_cm)) +geom_boxplot() + theme(axis.text.x = element_text(angle=90)) + xlab("Species of Tree") + ylab("Diameter at Breast Height of Tree (cm)")
ggsave("images/DBH-and-species-boxplot.png", height = 5, width = 5)
