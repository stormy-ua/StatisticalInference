library(ggplot2)

lambda <- 0.2
theoretical.mean <- 1/lambda
theoretical.sd <- 1/lambda
n <- 40
nsim <- 1000

simulation.data <- matrix(rexp(n * nsim, lambda), ncol = n)
simulation.means <- apply(simulation.data, 1, mean)

 
# plot a distribution of a large collection of random exponentials
ggplot(data = data.frame(x = rexp(n * nsim, lambda)), aes(x = x, y = ..density..)) + 
    geom_histogram(binwidth = 0.7, color = "black", fill = "orange") +
    geom_density(lwd = 1) +
    scale_x_continuous(limits = c(0, 25)) +
    ggtitle("Distribution of random exponentials")


nvars <- 10000
randoms <- rexp(nvars, lambda)
vars <- sapply(1:nvars, function(i) var(randoms[1:i]))
ggplot(data.frame(x = 1 : nvars, y = vars), aes(x = x, y = y)) + 
   geom_hline(yintercept = theoretical.sd**2, linetype = "dotted", color = "red", lwd = 1) +
   geom_line(size = 2) +
   scale_y_continuous(breaks = seq(10, 30, 5)) +
   labs(x = "Number of observations", y = "Variance") +
   ggtitle("Sample variance vs. theoretical variance")


# sample mean vs. theoretical mean
nmeans <- 5000
means <- cumsum(rexp(nmeans, lambda)) / (1  : nmeans)
ggplot(data.frame(x = 1 : nmeans, y = means), aes(x = x, y = y)) + 
    geom_hline(yintercept = theoretical.mean, linetype = "dotted", color = "red", lwd = 1) +
    geom_line(size = 2) +
    labs(x = "Number of observations", y = "Mean") +
    ggtitle("Sample mean vs. theoretical mean")

ggplot(data = data.frame(x = simulation.means), aes(x = x)) + 
    geom_histogram(binwidth = 0.1, color = "black", fill = "orange",  aes(y = ..density..)) +
    stat_function(fun = dnorm, size = 1) +
    #scale_x_continuous(limits = c(2, 8.5), breaks = 0:10) +
    geom_vline(xintercept = theoretical.mean, linetype = "dotted", color = "red", lwd = 2) +
    ggtitle("Distribution of random exponentials averages")


# plot a distribution of a large collection of averages
ggplot(data = data.frame(x = (simulation.means - theoretical.mean)/(theoretical.sd/sqrt(n))), aes(x = x)) + 
    geom_histogram(binwidth = 0.1, color = "black", fill = "orange",  aes(y = ..density..)) +
    stat_function(fun = dnorm, size = 1) +
    #scale_x_continuous(limits = c(2, 8.5), breaks = 0:10) +
    #geom_vline(xintercept = theoretical.mean, linetype = "dotted", color = "red", lwd = 2) +
    ggtitle("Distribution of random exponentials averages")



cfunc <- function(x, n) sqrt(n) * (mean(x) - theoretical.mean) / theoretical.sd

dat <- data.frame(
    x = c(apply(matrix(rexp(10 * nsim, lambda), nsim), 1, cfunc, 5),
          apply(matrix(rexp(20 * nsim, lambda), nsim), 1, cfunc, 15),
          apply(matrix(rexp(n * nsim, lambda), nsim), 1, cfunc, n)
    ),
    size = factor(rep(c(10, 20, n), rep(nsim, 3))))
g <- ggplot(dat, aes(x = x, fill = size)) + geom_histogram(binwidth=.3, colour = "black", aes(y = ..density..)) 
g <- g + stat_function(fun = dnorm, size = 2)
g + facet_grid(. ~ size)


