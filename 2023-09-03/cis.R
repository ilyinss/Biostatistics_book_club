library(tidyverse)

# Доверительный интервал по формуле ---------------------------------------
library(confintr)
x <- rnorm(100, mean = 10, sd = 5)

mean(x)

ci <- ci_mean(x)

tibble(x, 
       min = ci$interval[1],
       max = ci$interval[2]) |> 
  ggplot(aes(x))+
  geom_histogram()+
  geom_linerange(aes(xmin = min, xmax = max), y = -0.1, color = "red")

cor.test(rnorm(100, 10, 5), rlnorm(100, 10, 5))

# другие функции
ci_median()
ci_mean_diff()
ci_median_diff()
ci_proportion()

# bootstrap ---------------------------------------------------------------
x

map_dbl(1:100, function(x){
  x |> 
    sample(x = _, 100, replace = TRUE) |> 
    mean()
}) |> 
  tibble(bootstraped = _) |> 
  ggplot(aes(bootstraped))+
  geom_histogram()

bootstraped <- bootstrap::bootstrap(x, 1000, theta = mean)

tibble(bootstraped = bootstraped$thetastar) |> 
  ggplot(aes(bootstraped))+
  geom_histogram()

quantile(bootstraped$thetastar, c(0.05/2, 1-0.05/2))

# linear regression -------------------------------------------------------

iris$Sepal.Length
iris$Petal.Length

iris_model <- lm(Sepal.Length ~ Petal.Length, data = iris)

summary(iris_model)

library(ggeffects)
ggpredict(iris_model, terms = "Petal.Length") |> 
  plot()


iris_model2 <- lm(Sepal.Length ~ Species, data = iris)

summary(iris_model2)

ggpredict(iris_model2, terms = c("Species")) |> 
  plot()

# credible intervals ------------------------------------------------------

# априорное распределение aka prior

tibble(x = seq(0, 1, length.out = 1000),
       y = dbeta(x, shape1 = 20, shape2 = 20)) |> 
  ggplot(aes(x, y))+
  geom_line()

# данные aka likelihood

tibble(x = seq(0, 1, length.out = 1000),
       y = dbeta(x, shape1 = 75, shape2 = 25)) |> 
  ggplot(aes(x, y))+
  geom_line()

# апостериорное распределение aka posterior

tibble(x = seq(0, 1, length.out = 1000),
       y = dbeta(x, shape1 = 75+20, shape2 = 25+20)) |> 
  ggplot(aes(x, y))+
  geom_line()

qbeta(c(0.05/2, 1-0.05/2), shape1 = 75+20, shape2 = 25+20)
