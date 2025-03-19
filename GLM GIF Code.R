# Load necessary libraries
library(ggplot2)
library(gganimate)
library(dplyr)

# Generate simulated insurance data
set.seed(123)
n <- 100
x <- runif(n, 1, 10)  # Predictor (e.g., age, vehicle age)
y <- rpois(n, lambda = exp(0.3 * x))  # Claims frequency with Poisson noise
data <- data.frame(x, y)

# Fit a Generalized Linear Model (GLM) with Poisson distribution
glm_model <- glm(y ~ x, family = poisson(link = "log"), data = data)

# Predict values over a smooth range
x_seq <- seq(min(x), max(x), length.out = 100)
preds <- predict(glm_model, newdata = data.frame(x = x_seq), type = "response")

# Create data for animation (step-by-step fitting)
anim_data <- data.frame(x = x_seq, y = preds) %>%
  mutate(step = row_number())

# Base plot
p <- ggplot(data, aes(x, y)) +
  geom_point(color = "blue", size = 2, alpha = 0.6) +  # Raw data points
  labs(title = "GLM Fitting Process", x = "Predictor (e.g., Age)", y = "Response (e.g., Claims Frequency)") +
  theme_minimal()

# Animated plot showing step-by-step fitting
p_anim <- p +
  geom_line(data = anim_data, aes(x, y, group = 1, frame = step), color = "red", size = 1) +
  transition_reveal(step)

# Save GIF
animate(p_anim, renderer = gifski_renderer(), fps = 20, duration = 4, width = 600, height = 400)
anim_save("glm_fitting.gif")

