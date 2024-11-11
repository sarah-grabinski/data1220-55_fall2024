library(readr)
library(writexl)
library(tidyverse)

df_trump <- read_csv('./delays_trump/Airline_Delay_Cause.csv')

df_biden <- read_csv('./delays_biden/Airline_Delay_Cause.csv')

df <- rbind(df_trump |> mutate(admin = 'Trump'), 
            df_biden |> mutate(admin = 'Biden')) |>
  summarize(flights = sum(arr_flights, na.rm = T), 
            delayed = sum(arr_del15, na.rm = T),
            pct_delay = sum(arr_del15, na.rm = T) /
                          sum(arr_flights, na.rm = T),
            avg_delay = sum(arr_delay, na.rm = T) / 
                          sum(arr_flights, na.rm = T),
            .by = c('admin', 'airport',
                    'airport_name')) |>
  drop_na()

set.seed(16554); df_sample <- df[sample(1:nrow(df), 800),]

write_xlsx(df_sample, 'airline_delays.xlsx')

stats <- df_sample |>
  summarize(delayed = sum(delayed), 
            flights = sum(flights),
            proportion = sum(delayed) / 
              sum(flights), 
            n = n(), 
            mean_delay = mean(avg_delay, na.rm = T), 
            sd = sd(avg_delay, na.rm = T), 
            .by = 'admin')

stats

mean_b <- stats[stats$admin == 'Biden',][['mean_delay']]

sd_b <- stats[stats$admin == 'Biden',][['sd']]

prop_b <- stats[stats$admin == 'Biden',][['proportion']]

n_b <- stats[stats$admin == 'Biden',][['n']]

se_xb <- sd_b / sqrt(n_b)

se_pb <- sqrt((prop_b * (1 - prop_b))/n_b)

low_bx <- mean_b - qt(0.975, df = n_b - 1) * se_xb

high_bx <- mean_b + qt(0.975, df = n_b - 1) * se_xb

low_bp <- prop_b - qnorm(0.975) * se_pb

high_bp <- prop_b + qnorm(0.975) * se_pb

mean_t <- stats[stats$admin == 'Trump',][['mean_delay']]

sd_t <- stats[stats$admin == 'Trump',][['sd']]

prop_t <- stats[stats$admin == 'Trump',][['proportion']]

n_t <- stats[stats$admin == 'Trump',][['n']]

se_xt <- sd_t / sqrt(n_t)

se_pt <- sqrt((prop_t * (1 - prop_t))/n_t)

low_tx <- mean_t - qt(0.975, df = n_t - 1) * se_xt

high_tx <- mean_t + qt(0.975, df = n_t - 1) * se_xt

low_tp <- prop_t - qnorm(0.975) * se_pt

high_tp <- prop_t + qnorm(0.975) * se_pt

mean_diff <- mean_b - mean_t

se_xdiff <- sqrt((sd_b^2)/n_b + (sd_t^2)/n_t)

diff_low <- mean_diff - qt(0.975, df = min(n_t, n_b) - 1) * se_xdiff

diff_high <- mean_diff + qt(0.975, df = min(n_t, n_b) - 1) * se_xdiff

p_diff <- prop_b - prop_t

p_pool <- sum(stats$delayed) / sum(stats$flights)

se_pdiff <- sqrt((p_pool * (1 - p_pool)) / n_b + (p_pool * (1 - p_pool)) / n_t)

p_low <- p_diff - qnorm(0.975) * se_pdiff

p_high <- p_diff + qnorm(0.975) * se_pdiff

