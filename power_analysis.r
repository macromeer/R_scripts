install.packages("pwr")
library(pwr)
# https://github.com/heliosdrm/pwr

# perform power analysis of dataset

# load data
df <- read.csv("/home/marco/Documents/mean_crista_width_results.csv")

# define parameters
power <- 0.9 # chance of detecting true effect
alpha <- 0.01 # prob of rejecting null hypothesis when it is true

# calculate effect size (cohen's d)
effect_size <- function(mean1, mean2, sd1, sd2) {
  abs(mean1 - mean2) / sqrt((sd1^2 + sd2^2) / 2)
}

effect_size(
    mean(df$Width..nm.[df$SampleType == "Knockout"]),
    mean(df$Width..nm.[df$SampleType == "Control"]),
    sd(df$Width..nm.[df$SampleType == "Knockout"]),
    sd(df$Width..nm.[df$SampleType == "Control"]))

# perform power analysis
pwr.t.test(
    d = effect_size,
    sig.level = alpha,
    power = power,
    type = "two.sample")
