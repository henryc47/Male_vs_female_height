#height data https://ourworldindata.org/human-height#:~:text=The%20standard%20deviation%20was%207.59,standard%20deviation%20of%207.07%20cm.
male_height_mean = 178.4
male_height_sd = 7.59
female_height_mean = 164.7
female_height_sd = 7.07
world_adult_population = 0.65*8*10^9
num_males = world_adult_population*0.5
num_females = world_adult_population*0.5


#function from https://stats.stackexchange.com/a/89592
T.test <- function(n, mean, sd) {
  s <- sum((n - 1) * sd^2) / (sum(n) - 2) # weighted variance
  t <- sqrt(prod(n) / sum(n)) * (diff(mean) / sqrt(s)) # t statistic
  df <- sum(n) - 2  # degrees of freedom
  p <- (1 - pt(abs(t), df)) * 2 # p value
  c(t = t, p = p)
}

#create a dataframe storing the summary statistics
height_data <- data.frame(n=c(num_males,num_females), mean=c(male_height_mean,female_height_mean), sd=c(male_height_sd,female_height_sd))
a = T.test(height_data$n, height_data$mean, height_data$sd)
