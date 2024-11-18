library(dplyr)
library(readxl)
LungCapData <- read_excel("C:/Users/sarah/Downloads/LungCapData.xls")
View(LungCapData)

LungCapData |>
  summarize(Q1 = quantile(LungCap, 0.25), 
            Q2 = quantile(LungCap, 0.50), 
            Q3 = quantile(LungCap, 0.75))

LungCapData |>
  summarize(n = n(), 
            .by = 'Smoke')

n <- 77 + 648
n

p_smoke <- 77/n
p_smoke

se_p <- sqrt((p_smoke * (1-p_smoke))/n)
se_p

alpha <- 0.05
alpha

# Positive z_star
z_star <- qnorm(1-alpha/2)
z_star

lower_p <- p_smoke - z_star * se_p
lower_p

upper_p <- p_smoke + z_star * se_p
upper_p

# Negative z_star
z_star <- qnorm(alpha/2)
z_star

lower_p <- p_smoke + z_star * se_p
lower_p

upper_p <- p_smoke - z_star * se_p
upper_p

# With 95% confidence, the proportion of US citizens that smoke is 0.0838 to 0.1286.

LungCapData |>
  summarize(n = n(), 
            mean = mean(LungCap), 
            sd = sd(LungCap), 
            .by = 'Smoke') |> View()

mean_smoke <- 8.6455
sd_smoke <- 1.8829
n_smoke <- 77

mean_non <- 7.7702
sd_non <- 2.7261
n_non <- 648

mean_diff <- mean_smoke - mean_non
mean_diff

se_diff <- sqrt((sd_smoke^2)/n_smoke + (sd_non^2)/n_non)
se_diff

dof <- min(n_smoke, n_non) - 1
dof

t_statistic <- mean_diff / se_diff
t_statistic

p_value <- pt(t_statistic, dof, lower.tail = F)
p_value

# Probability of observing sample under null hypothesis less than alpha, reject H0

# No, association and causation are different things