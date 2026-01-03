library(tidyverse)
library(odin)


###1. import data
covid_data <- read.csv("C:/Users/LENOVO/Downloads/measles_hagelloch.csv")
View(covid_data) #if you need to see the data


### 2. explore your data
covid_data %>% 
  ggplot(aes(x=t,y=infectious)) +
  geom_point(col="red",size=4) +
  theme_classic() +
  labs(x="Time", y="Infectious people")


### 3. SIR model
sir_odin <- odin({
  ## model parameters
  beta <- user(0.5) ## transmission rate
  gamma <- 1/8 ## recovery rate (assumed)
  N <- 577  ## total population
  
  ## initial conditions
  initial(S) <- N - 1 - 0
  initial(I) <- 1
  initial(R) <- 0
  
  ## differential equations
  deriv(S) <- -beta * S * I / N
  deriv(I) <- beta * S * I / N - gamma * I
  deriv(R) <- gamma * I
})

###Simulation
# Setup models with different beta parameters
sir_flu_1 <- sir_odin$new(beta = 0.224)
sir_flu_2 <- sir_odin$new(beta = 0.223)
sir_flu_3 <- sir_odin$new(beta = 0.222)
sir_flu_4 <- sir_odin$new(beta = 0.221)

# Time sequence for the simulation
times <- seq(0, 60, by = 1)

# Run models and save output as data.frame
output_flu_1 <- tibble(data.frame((sir_flu_1$run(times)))) %>% filter(t!=0)
output_flu_2 <- tibble(data.frame((sir_flu_2$run(times)))) %>% filter(t!=0)
output_flu_3 <- tibble(data.frame((sir_flu_3$run(times)))) %>% filter(t!=0)
output_flu_4 <- tibble(data.frame((sir_flu_4$run(times)))) %>% filter(t!=0)


# Visualize beta = 0.224
output_flu_1 %>% 
  ggplot(aes(x=t)) +
  geom_point(aes(y=infectious), size=4, col="red") +
  geom_line(aes(y=I), linewidth=2, col="blue") +
  theme_classic() +
  labs(x="Time", y="Infectious people", 
       title="SIR Model Fit: β = 0.224",
       subtitle="Red dots = data, Blue line = model")

# Visualize beta = 0.223
output_flu_2 %>% 
  ggplot(aes(x=t)) +
  geom_point(aes(y=covid_data$IN), size=4, col="red") +
  geom_line(aes(y=I), linewidth=2, col="blue") +
  theme_classic() +
  labs(x="Time", y="Infectious people", 
       title="SIR Model Fit: β = 0.223",
       subtitle="Red dots = data, Blue line = model")

# Visualize beta = 0.222
output_flu_3 %>% 
  ggplot(aes(x=t)) +
  geom_point(aes(y=covid_data$IN), size=4, col="red") +
  geom_line(aes(y=I), linewidth=2, col="blue") +
  theme_classic() +
  labs(x="Time", y="Infectious people", 
       title="SIR Model Fit: β = 0.222",
       subtitle="Red dots = data, Blue line = model")

# Visualize beta = 0.221
output_flu_4 %>% 
  ggplot(aes(x=t)) +
  geom_point(aes(y=covid_data$IN), size=4, col="red") +
  geom_line(aes(y=I), linewidth=2, col="blue") +
  theme_classic() +
  labs(x="Time", y="Infectious people", 
       title="SIR Model Fit: β = 0.221",
       subtitle="Red dots = data, Blue line = model")


###MLE
# Write a function to calculate binomial log-likelihood
binom_loglik <- function(data, par, N=112000000) {
  # Create a sir model with a beta value of: par
  sir_odin_run <- sir_odin$new(beta = par)
  
  # Run the model
  sir_output <- sir_odin_run$run(times)
  sir_output <- data.frame(sir_output) %>% mutate(prev=I/N)
  
  # Binomial log-likelihood; need to remove the first simulation t=0
  -sum(dbinom(x=data$IN, size=N, prob=sir_output$prev[-1], log=TRUE))
}

# Plug the function into optim, to numerically find the beta parameter
# Set minimum of 0.5 and max of 3
beta_mle <- optim(par=0.01, fn=binom_loglik, 
                  data=covid_data,
                  method="Brent", 
                  lower=0.1, upper=0.5)

cat("Best β from MLE:", round(beta_mle$par, 3))


##visualize
# According to MLE, our best fit is beta = 0.29
sir_flu_5 <- sir_odin$new(beta = beta_mle$par)


# Run models and save output as data.frame
output_flu_5 <- tibble(data.frame((sir_flu_5$run(times)))) %>% filter(t!=0)

# Combine each output with the data and visualise
output_flu_5 %>% 
  ggplot(aes(x=t)) +
  geom_point(aes(y=covid_data$IN), size=4, col="red") +
  geom_line(aes(y=I), linewidth=2, col="blue") +
  theme_classic() +
  labs(x="Time", y="Infectious people",
       title=paste("Best Fit using MLE: β =", round(beta_mle$par, 3)),
       subtitle="Red dots = data, Blue line = model")

#MSE
#  data and output of LSE model
lse_data <- output_flu_3  # output_flu_2 dari beta = 0.31

# Calculate MSE
mse_lse <- mean((covid_data$IN - lse_data$I)^2, na.rm = TRUE)

#  data and output of MLE model
mle_data <- output_flu_5  # output_flu_5 dari beta_mle$par

# Calculate MSE
mse_mle <- mean((covid_data$IN - mle_data$I)^2, na.rm = TRUE)

cat("MSE of LSE model (β = 0.222):", round(mse_lse, 2), "\n")
cat("MSE of MLE model (β =", round(beta_mle$par, 3), "):", round(mse_mle, 2))

#predicting for 14 days ahead
times_future <- seq(0, 74, by = 1)  # extend 14 days
sir_future <- sir_odin$new(beta = beta_mle$par)
output_future <- data.frame(sir_future$run(times_future))
prediction_14days <- output_future %>% 
  filter(t > 60) %>% 
  select(t, I) %>% 
  mutate(I = round(I))

print(prediction_14days)
# Ensure 'date' is a Date object
combined_data$date <- as.Date(combined_data$date)




library(ggplot2)
library(dplyr)

# Convert the date column to Date format (important for x-axis)
covid_data$date <- as.Date(covid_data$date)

#  Add 'source' label to original data
original_data <- covid_data %>%
  mutate(t = 1:n(),
         source = "Original Data") %>%
  select(t, date, IN, source)

#  Create 14-day prediction data with date and source
prediction_14days <- output_future %>% 
  filter(t > 60) %>% 
  select(t, I) %>% 
  mutate(
    I = round(I),
    date = seq(from = max(covid_data$date) + 1, by = "day", length.out = 14),
    source = "Prediction"
  ) %>% 
  rename(IN = I) %>%
  select(t, date, IN, source)

# Combine original and predicted data
combined_data <- bind_rows(original_data, prediction_14days)

#  Plot
plot_combined <- combined_data %>%
  ggplot(aes(x = date, y = IN, color = source)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  theme_classic() +
  labs(
    title = "COVID-19 Infectious Cases: Actual vs 14-Day Prediction",
    subtitle = "Red = Original Data | Blue = Model Prediction",
    x = "Date",
    y = "Number of Infectious People",
    color = "Source"
  ) +
  scale_color_manual(values = c("Original Data" = "red", "Prediction" = "blue")) +
  theme(
    text = element_text(size = 12),
    plot.title = element_text(face = "bold")
  )

#  Show the plot
print(plot_combined)

