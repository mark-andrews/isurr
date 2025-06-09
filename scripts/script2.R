
# Load packages -----------------------------------------------------------

library(tidyverse)
library(skimr)

# Read in the data --------------------------------------------------------

weight_df <- read_csv('weight.csv')

glimpse(weight_df) # this command is part of dplyr


# Summary statistics etc --------------------------------------------------

skim(weight_df)


# Statistical analysis ----------------------------------------------------

# Independent samples t-test
result_1 <- t.test(height ~ gender, data = weight_df)

# Correlation analysis or test
result_2 <- cor.test(~ height + weight, data = weight_df)

# Spearman's rho correlation analysis or test
# Tell it not to try exact p-value calculation (because of ties)
# Also, change confidence interval to 99%
result_3 <- cor.test(~ height + weight, data = weight_df, 
                     exact = FALSE,
                     method = 'spearman')
result_3a <- cor.test(~ height + weight, data = weight_df, 
                     exact = FALSE,
                     conf.level = 0.99,
                     method = 'pearson')

# Linear regression analysis ----------------------------------------------

result_4 <- lm(weight ~ height, data = weight_df)
summary(result_4)
confint(result_4)
confint(result_4, level = 0.99) # 99% confidence interval


result_5 <- lm(weight ~ height + age, data = weight_df)
summary(result_5)


# additive general linear model
result_6 <- lm(weight ~ height + gender, data = weight_df)
summary(result_6)


# interaction effect general linear model
result_7 <- lm(weight ~ height * gender, data = weight_df)
result_7a <- lm(weight ~ height + gender + height:gender, data = weight_df)
summary(result_7)$coef
summary(result_7a)$coef

summary(result_7)

anova(result_6, result_7) # model null hypothesis 


# One-way ANOVA -----------------------------------------------------------

result_8 <- aov(weight ~ group, data = PlantGrowth)
summary(result_8)

library(emmeans)
emmeans(result_8, specs = pairwise ~ group, adjust = 'bonferroni')

# GGplot ------------------------------------------------------------------


# scatterplot of weight on height
ggplot(weight_df, aes(x = height, y = weight)) + geom_point()

# reduce down the size of the points ...
ggplot(weight_df, aes(x = height, y = weight)) + geom_point(size = 0.5)

# include the line of best fit too
ggplot(weight_df, aes(x = height, y = weight)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = 'lm', colour = 'red') 

# scatterplot of 100 randomly selected points
ggplot(sample_n(weight_df,100), aes(x = height, y = weight)) + 
  geom_point(size = 0.5) +
  geom_smooth(method = 'lm', colour = 'red', se = FALSE) 

# fit nonlinear curve
ggplot(weight_df, aes(x = height, y = weight)) + 
  geom_point(size = 0.5) +
  geom_smooth() 

# fit nonlinear curve, method = loess
ggplot(weight_df, aes(x = height, y = weight)) + 
  geom_point(size = 0.5) +
  geom_smooth(method='loess') 

# colour code gender
ggplot(weight_df, 
       aes(x = height, y = weight, colour = gender)
) + geom_point(size = 0.5)

# use another palette
ggplot(weight_df, 
       aes(x = height, y = weight, colour = gender)
) + geom_point(size = 0.5) +
  # scale_colour_brewer(palette = 'Set1')
  scale_colour_brewer(palette = 'PiYG')

# use another palette
ggplot(weight_df, 
       aes(x = height, y = weight, colour = gender)
) + geom_point(size = 0.5) +
  scale_colour_manual(values = c('yellow', 'black'))


# histogram
ggplot(weight_df, aes(x = weight)) + geom_histogram()
ggplot(weight_df, aes(x = weight)) + geom_histogram(binwidth = 1, color='white')
ggplot(weight_df, aes(x = weight)) + geom_histogram(binwidth = 2.5, color='white')
ggplot(weight_df, aes(x = weight)) + 
  geom_histogram(binwidth = 2.5, color='white', fill = 'blue')

# separate histograms for men and women
ggplot(weight_df, aes(x = weight, fill = gender)) + geom_histogram(colour = 'white')

ggplot(weight_df, aes(x = weight, fill = gender)) + 
  geom_histogram(colour = 'white', position = 'dodge')

ggplot(weight_df, aes(x = weight, fill = gender)) + 
  geom_histogram(colour = 'white', position = 'identity', alpha = 0.65)

ggplot(weight_df, aes(x = weight)) +
  geom_histogram(colour = 'white') + 
  facet_wrap(~gender)

ggplot(weight_df, aes(x = weight)) +
  geom_histogram(colour = 'white') + 
  facet_wrap(~gender, scales = 'free_y')









