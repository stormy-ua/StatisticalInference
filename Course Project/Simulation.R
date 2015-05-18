library(ggplot2)

lambda <- 0.2
theoretical.mean <- 1/lambda
theoretical.sd <- 1/lambda
n <- 40
nsim <- 1000

simulation.data <- matrix(rexp(n * nsim, lambda), ncol = n)
simulation.means <- apply(simulation.data, 1, mean)


# plot a distribution of a large collection of random exponentials
ggplot(data = data.frame(x = simulation.data), aes(x = x)) + 
    geom_histogram(binwidth = 0.4, color = "black", fill = "orange",  aes(y = ..density..)) +
    geom_density(lwd = 1) +
    scale_x_continuous(limits = c(0, 25)) +
    ggtitle("Distribution of random exponentials")

# plot a distribution of a large collection of averages
ggplot(data = data.frame(x = (simulation.means - theoretical.mean)/(theoretical.sd/sqrt(n))), aes(x = x)) + 
    geom_histogram(binwidth = 0.1, color = "black", fill = "orange",  aes(y = ..density..)) +
    stat_function(fun = dnorm, size = 1) +
    #scale_x_continuous(limits = c(2, 8.5), breaks = 0:10) +
    #geom_vline(xintercept = theoretical.mean, linetype = "dotted", color = "red", lwd = 2) +
    ggtitle("Distribution of random exponentials averages")




