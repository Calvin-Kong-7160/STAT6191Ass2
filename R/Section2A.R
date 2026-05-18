#high_lifeExp = ifelse(lifeExp > median(lifeExp), 1, 0) # Given variable
#log_gdp = log(gdpPercap) # Given predictor

library(gapminder)
library(dplyr)
library(ggplot2)

# Load and prep the data
data <- gapminder %>%
  mutate(
    high_lifeExp = ifelse(lifeExp > median(lifeExp), 1, 0),
    log_gdp = log(gdpPercap)
  )

head(data)

summary(data$high_lifeExp)
summary(data$log_gdp)

# Log-likelihood function
log_likelihood <- function(beta, data) {

  beta0 <- beta[1]
  beta1 <- beta[2]

  y <- data$high_lifeExp
  x <- data$log_gdp

  eta <- beta0 + beta1 * x

  p <- exp(eta) / (1 + exp(eta))

  log_lik <- sum(y * log(p) + (1 - y) * log(1 - p))

  return(log_lik)
}

# Optimization function
neg_log_likelihood <- function(beta, data) {
  -log_likelihood(beta, data)
}

optim_output <- optim(
  par = c(0, 0),
  fn = neg_log_likelihood,
  data = data,
  method = "BFGS",
  hessian = TRUE
)

optim_coef <- optim_output$par
names(optim_coef) <- c("beta0_intercept", "beta1_log_gdp")
optim_coef


### Fit the model w/ glm()
glm_fit <- glm(
  high_lifeExp ~ log_gdp,
  data = data,
  family = binomial(link = "logit")
)
summary(glm_fit)
glm_coef <- coef(glm_fit)
glm_coef


## Compare optim() and glm() estimates
comparison_table <- data.frame(
  Parameter = c("Intercept", "log_gdp"),
  Optim_Estimate = optim_coef,
  GLM_Estimate = glm_coef,
  Difference = optim_coef - glm_coef
)

comparison_table


###
