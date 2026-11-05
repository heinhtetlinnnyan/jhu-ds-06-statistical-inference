library(swirl)
swirl()
install_from_swirl("Statistical Inference")

library(UsingR); data(father.son);
x <- father.son$sheight
n <- length(x)
mean(x)/sqrt(n)
hist(x)

round(c(var(x),
        var(x)/n,
        sd(x),
        sd(x)/sqrt(n)),2)

choose(8,7)* 0.5 ^ 8 + choose(8,8) *0.5 ^8

pbinom(6,
       size = 8,
       prob = .5,
       lower.tail = FALSE)

qnorm(.95,
      mean = 0,
      sd = 1,
      lower.tail = FALSE)

(1160-1020)/50

pnorm( 1160,
       mean = 1020,
       sd = 50,
       lower.tail = FALSE)

pnorm(2.8,
      lower.tail = FALSE)

qnorm(0.75,
      mean = 1020,
      sd = 50)
)

ppois(3,
      lambda = 2.5*4)

pbinom(2,
       size = 500,
       prob = 0.01)

ppois(2,
      lambda = 500 * 0.01)


n <- 1000
means <- cumsum(rnorm(n))/(1:n)
plot(means)

binom.test(56, 100)$conf.int

Quiz2

pnorm(70,
      mean = 80,
      sd = 10,
      lower.tail = TRUE)

qnorm(0.95,
      mean = 1100,
      sd = 75,
      lower.tail = TRUE
      )

qnorm(0.95,
      mean = 1100, 
      sd = 75 / sqrt(100),
      lower.tail = TRUE)

choose(5,4)* 0.5 ^ 5 + choose(5,5) *0.5 ^5
pbinom(3, size = 5, prob = 0.5, lower.tail = FALSE)

ppois(100,
      lambda = 15*10)

pnorm(16, 
      mean = 15, 
      sd = 10 / sqrt(100)) - 
        pnorm(14, 
              mean = 15, 
              sd = 10 / sqrt(100))

ppois(10, 
      lambda = 15,
      lower.tail = TRUE)


library(datasets); data("ChickWeight"); library(reshape2)
View(ChickWeight)

wideCW <- dcast(ChickWeight, Diet + Chick ~ Time, value.var = "weight")
View(wideCW)


## Quiz 3

install.packages("BSDA")
library(BSDA)
tsum.test(mean.x = 1100,
          s.x = 30,
          n.x = 9,
          conf.level = 0.95)

1100 + c(-1,1) * qt(0.975, df = 8) * (30/sqrt(9))

2 * sqrt(9) / qt(0.975, df = 8)


tsum.test(
        mean.x = 3, s.x = sqrt(0.60), n.x = 10,
        mean.y = 5, s.y = sqrt(0.68), n.y = 10,
        var.equal = TRUE,
        conf.level = 0.95
)

tsum.test(
        n.x = 9,
        n.y =9,
        mean.x = -3,
        mean.y = 1,
        s.x = 1.5,
        s.y = 1.8,
        var.equal = TRUE,
        conf.level = 0.90
)


tsum.test(
        mean.y = 4,
        s.y = 0.5,
        n.y = 100,
        mean.x = 6,
        s.x = 2,
        n.x = 100,
        var.equal = TRUE,
        conf.level = 0.95
        
)



## quiz 4

# Data setup
baseline <- c(140, 138, 150, 148, 135)
week2    <- c(132, 135, 151, 146, 130)

# Paired two-sided t-test
t.test(baseline, week2, paired = TRUE)

tsum.test(
        n.x = 9,
        mean.x = 1100,
        s.x = 30,
        conf.level = 0.95)

binom.test(
        x = 3,
        n = 4,
        p = 0.5,
        alternative = "greater"
        
)

poisson.test(
        x = 10,
        T = 1787,
        r = 0.01,
        alternative = "less"
)

tsum.test(
        mean.x = -3,
        mean.y = 1,
        s.x = 1.5,
        s.y = 1.8,
        n.x = 9,
        n.y = 9,
        var.equal = TRUE,
        conf.level = 0.95)

power.t.test(
        n = 100,
        delta = 0.01,
        sd = 0.04,
        sig.level = 0.05,
        type = "one.sample",
        alternative = "one.sided"
)

power.t.test(
        delta = 0.01,
        sd = 0.04,
        power = 0.9,
        sig.level = 0.05,
        type = "one.sample",
        alternative = "one.sided"
)

lambda <- 0.2
n <- 40
sims <- 1000
rexp(40,0.2)
mean(rexp(40,0.2))

set.seed(44)
sim_means <- replicate(1000, mean(rexp(40, 0.2)))


library(ggplot2)

sample_mean <- mean(sim_means)
theo_mean <- 1 / 0.2
sample_mean; theo_mean

mydata <- data.frame(sim_means)
ggplot(mydata,
       aes( x = sim_means)) +
        geom_histogram(binwidth = 0.1, color = "black") +
        geom_vline(aes(xintercept = sample_mean, linetype = "sample_mean")) +
        geom_vline(aes(xintercept = theo_mean, linetype = "theo_mean")) +
        labs(title = "Sample Mean vs Theoretical Mean",
             x = "Samples Means of 40 Exponentials",
             y = "Counts") +
        theme_minimal()

sample_var <- var(sim_means)
theo_var <- (1/lambda)^2/n
sample_var;theo_var

sample_sd <- sd(sim_means)
theo_sd <- sqrt(theo_var)

ggplot(mydata,
       aes( x = sim_means)) +
        geom_histogram(binwidth = 0.1, color = "black") +
        geom_vline(aes(xintercept = sample_mean + sample_sd, linetype = "sample")) +
        geom_vline(aes(xintercept = sample_mean - sample_sd, linetype = "sample")) +
        geom_vline(aes(xintercept = sample_mean + theo_sd, linetype = "theoretical")) +
        geom_vline(aes(xintercept = sample_mean - theo_sd, linetype = "theoretical")) +
        labs(title = "Sample Mean vs Theoretical Mean",
             x = "Samples Means of 40 Exponentials",
             y = "Counts") +
        theme_minimal()


ggplot(mydata,
       aes( x = sim_means)) +
        geom_histogram(binwidth = 0.1, color = "black", aes(y = after_stat(density))) +
        stat_function(fun = dnorm, args = list(mean = theo_mean, sd = theo_sd), color = "red", linewidth = 1.2) +
        labs(title = "Density of Sample means with Normal Curve",
             x = "Sample Means of 40 Exponentials",
             y = "Density") +
        theme_minimal()




