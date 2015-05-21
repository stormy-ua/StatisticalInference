library(ggplot2)
data(ToothGrowth)


ToothGrowth$dose <-as.factor(ToothGrowth$dose)

ggplot(ToothGrowth) +
    geom_histogram(binwidth = 1, aes(x = len, fill = supp)) + 
    facet_grid(supp ~ dose)


t.test(len ~ supp, paired = F, var.equal = F, data = ToothGrowth[ToothGrowth$dose == 0.5,])
t.test(len ~ supp, paired = F, var.equal = F, data = ToothGrowth[ToothGrowth$dose == 1,])
t2 <- t.test(len ~ supp, paired = F, var.equal = F, data = ToothGrowth[ToothGrowth$dose == 2,])

t.test(len ~ dose, paired = F, data = ToothGrowth)


stat_by_supp <- sapply(unique(ToothGrowth$dose), function(d) {
    stat <- t.test(len ~ supp, paired = F, var.equal = F, data = ToothGrowth[ToothGrowth$dose == d,])
    stat$dose <- d
    stat
})
t(stat_by_supp)

dose_combn <- combn(as.character(unique(ToothGrowth$dose)), 2)
stat_by_dose <- mapply(function(f, s) {
    dose_pair <- c(f, s) 
    print(dose_pair)
    stat <- t.test(len ~ dose, paired = F, var.equal = F, data = ToothGrowth[ToothGrowth$dose %in% dose_pair,])
    stat$dose.first <- dose_pair[1]   
    stat$dose.second <- dose_pair[2]
    stat
}, dose_combn[1, ], dose_combn[2, ])
t(stat_by_dose)
