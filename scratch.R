library(trackeR)
library(tidyverse)
library(lubridate)

ride_april <- read_container('ride_april.gpx', type = 'gpx')
ride_may <- read_container('ride_may.gpx', type = 'gpx')

summary(c(ride_april, ride_may))
# april: Average speed: 6.66 m_per_s 
# may: Average speed: 6.36 m_per_s 

d <- c(
  ride_april,
  ride_may
) %>% lapply(function(x) {
  fortify.zoo(x, names = 'time') %>% as_tibble()
}) %>% 
  bind_rows() %>% 
  mutate(date = date(time))

d_ride <- d %>% 
  select(date, time, distance, heart_rate) %>% 
  group_by(date) %>% 
  nest(data = -c(date))

dist_sample <- 10

d_ride$data_interp <- lapply(d_ride$data, function(data) {
  time_interp <- approx(x = data$distance, y = data$time, xout = seq(from = 0, to = ceiling(max(data$distance)), by = dist_sample))
  heart_interp <- approx(x = data$distance, y = data$heart_rate, xout = seq(from = 0, to = ceiling(max(data$distance)), by = dist_sample))
  
  tibble(distance = time_interp$x, time = as_datetime(time_interp$y), heart_rate = heart_interp$y) %>% 
    mutate(
      speed = ((distance - lag(distance))) / as.double(time - lag(time), units = 'secs') # m_per_s
    )
})

d_ride

d_sample <- d_ride %>% 
  select(date, data_interp) %>% 
  unnest(cols = data_interp)

d_sample %>% 
  pivot_longer(cols = c(heart_rate, speed)) %>% 
  ggplot(mapping = aes(x = distance, y = value, color = as.factor(date))) +
  geom_line() +
  facet_wrap(.~name, scales = 'free')

calc_speed <- function(x, dist = 10) {
  # x is vector of speed for each `dist` meter block
  duration_total <- sum(replace_na(dist / x, 0))
  dist_total <- length(x) * dist
  speed <- dist_total / duration_total
  speed
}

x1 <- d_ride$data_interp[[1]]$speed
x2 <- d_ride$data_interp[[2]]$speed
n_blocks <- length(x1) # assume equal

statistic1 <- calc_speed(x1) - calc_speed(x2)
statistic1
# H0: mean(x1) - mean(x2) = 0
# H1: mean(x1) - mean(x2) != 0
permute <- function() {
  id <- sample(c(TRUE, FALSE), n_blocks, replace = TRUE)
  x1_0 <- if_else(id, x1, x2)
  x2_0 <- if_else(id, x2, x1)
  speed1_0 <- calc_speed(x1_0)
  speed2_0 <- calc_speed(x2_0)
  statistic0 <- speed1_0 - speed2_0
  statistic0
}

statistic0s <- replicate(10000, permute())

tibble(statistic0s) %>% 
  ggplot(mapping = aes(x = statistic0s)) +
  geom_density() +
  geom_vline(xintercept = statistic1)

sum(statistic0s > statistic1 | statistic0s < -statistic1)

