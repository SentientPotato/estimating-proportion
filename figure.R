## Load libraries
library(ggplot2)
## Set seed for reproducibility
set.seed(314)
## Define the population of 330 million Twitter users
## I use 0 & 1 to represent humans and bots respectively;
## it's more memory efficient &
## you can just sum a sample to see the number of bots
population = c(rep(0L, 330e6 * 0.95), rep(1L, 330e6 * 0.05))
## Check 100 randomly chosen users per day for 90 days
N = 90
observations = replicate(N, sum(sample(population, size = 100)))
## Calculate the posterior mean and 95% credible interval
## for the proportion of bots after each day's evidence
alpha = cumsum(observations) + 1
beta  = seq(from = 100, to = N * 100, by = 100) - cumsum(observations) + 1
means = alpha / (alpha + beta)
lows  = qbeta(p = 0.025, shape1 = alpha, shape2 = beta)
highs = qbeta(p = 0.975, shape1 = alpha, shape2 = beta)
## Plot results
dat = data.frame(Days = 1:N, Estimate = means, Low = lows, High = highs)
ttl = "Estimate of bot proportion after each day"
cap = "Figure by Sentient Potato (Twitter: @SentientPotato6)"
url = "https://github.com/SentientPotato/estimating-proportion"
cap = paste(cap, paste("Code to reproduce at", url), sep = "\n")
plt = ggplot(data = dat, mapping = aes(x = Days, y = Estimate)) +
    geom_ribbon(aes(ymin = Low, ymax = High), fill = "#0072b240") +
    geom_hline(yintercept = 0.05, linetype = "dashed", size = 1) +
    geom_line(color = "#0072b2", size = 1) +
    labs(title = ttl, caption = cap) +
    scale_x_continuous(breaks = c(30, 60, 90)) +
    theme_bw() +
    theme(plot.caption = element_text(color = "#808080", hjust = 0))
ggsave(
    filename = "estimate.png", plot = plt,
    height = 3.76, width = 6.684, units = "in", dpi = 180
)
