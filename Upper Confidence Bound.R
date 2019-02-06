# Reinforcement Learning/Interactive Learning (Strategy is dynamic)

# Upper Confidence Bound

dataset <- read.csv('Ads_CTR_Optimisation.csv')

# Random Selection

N <- 10000
d <- 10

ads_selected <- integer(0)
total_reward <- 0
for (n in 1:N){
  ad = sample(1:10, 1)
  ads_selected <- append(ads_selected, ad)
  reward = dataset[n, ad]
  total_reward = total_reward + reward
}

library(ggplot2)
hist(ads_selected, 
     col = 'blue',
     main = 'Histogram of ads selected',
     xlab = 'Ads',
     ylab = 'Count')


# Similar to Python, there is no package for UCB algorithm

# Upper Confidence Bound Algorithm


N = 10000
d = 10
ads_selected = integer(0)
numbers_of_selections = integer(d) # A vector of ten 0s
sums_of_rewards = integer(d) # A vector of d components (all 0 since the sums of rewards of each version of the ads is equal to zero)
total_reward = 0
for (n in 1:N){
  ad = 0
  max_upper_bound = 0 
  for (i in 1:d){
    if (numbers_of_selections[i] > 0) {
    average_reward = sums_of_rewards[i] / numbers_of_selections[i]
    delta_i = sqrt(3/2 * log(n) / numbers_of_selections[i])
    upper_bound = average_reward + delta_i
    } 
    else {
      upper_bound = 1e400
    }
    
    if (upper_bound > max_upper_bound){
      max_upper_bound = upper_bound
      ad = i
    }
  }
  ads_selected = append(ads_selected, ad)
  numbers_of_selections[ad] = numbers_of_selections[ad] + 1
  reward = dataset[n, ad]
  sums_of_rewards[ad] = sums_of_rewards[ad] + reward
  total_reward = total_reward + reward
}


hist(ads_selected, 
     col = 'blue',
     main = 'Histogram of ads selected',
     xlab = 'Ads',
     ylab = 'Count')