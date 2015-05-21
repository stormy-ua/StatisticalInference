library(ggplot2)
data(ToothGrowth)


ToothGrowth$dose <-as.factor(ToothGrowth$dose)

ggplot(ToothGrowth) +
    geom_histogram(binwidth = 1, aes(x = len, fill = supp)) + 
    facet_grid(dose ~ supp)


t.test(len ~ supp, paired = F, data = ToothGrowth[ToothGrowth$dose == 0.5,])
t.test(len ~ supp, paired = F, data = ToothGrowth[ToothGrowth$dose == 1,])
t.test(len ~ supp, paired = F, data = ToothGrowth[ToothGrowth$dose == 2,])

t.test(len ~ dose, paired = F, data = ToothGrowth)
