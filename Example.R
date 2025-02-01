##################
# BI Book Example
##################

#####################
# Regression example
#####################

library(ggplot2)
library(haven)
library(tidyverse)


fatherson = read_sav("fatherson.sav")

ggplot(fatherson, aes(father, son)) + 
  geom_jitter(aes(colour = cut(father, c(-Inf, 165, 180, Inf))), alpha = 0.5, size = 3) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_color_manual(name = "father", values = c("(-Inf,165]" = "navy", 
                                                          "(165, 180]" = "steelblue",
                                                          "(180, Inf]" = "darkred")) + 
  theme(legend.position="none") +
  geom_vline(xintercept = 161, color = "blue", linewidth = 1) +
  geom_vline(xintercept = 183, color = "red", linewidth = 1) +
  geom_hline(yintercept = 169, color = "blue", linewidth = 1) +
  geom_hline(yintercept = 180, color = "red", linewidth = 1) +
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  annotate("text", x = 161, y = Inf, label = "161", vjust = 1, hjust = 1.2, color = "blue") +
  annotate("text", x = 183, y = Inf, label = "183", vjust = 1, hjust = 1.2, color = "red") +
  annotate("text", x = 190, y = 169, label = "169", vjust = 1, hjust = -0.5, color = "blue") +
  annotate("text", x = 190, y = 180, label = "180", vjust = -0.5, hjust = -0.5, color = "red") +
  labs(x = "Fathers' Height (cm)", y = "Sons' Height (cm)")


ggplot(fatherson, aes(father, son)) + 
  geom_jitter(colour = "steelblue", alpha = 0.5, size = 3) + 
  geom_smooth(method = 'lm', formula = y~x, se = FALSE, colour = "red", fullrange = TRUE) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(plot.margin = unit(c(1,1,1,1), "cm")) +
  labs(x = "Fathers' Height (cm)", y = "Sons' Height (cm)")





########################
# Skewness and Kurtosis
########################

# skewness plot

par(mgp=c(1,1,0))
par(mar=c(5,4,4,2)+0.1)
par(mfrow=c(1,3))
# negative skewness
curve(dbeta(x,10,2), xlab = "x", ylab = "f(x)", xaxt = "n", yaxt = "n", lwd = 2, cex.lab = 2)
title(expression(paste("Left skewed: ", gamma[1], " < 0")), line = 1, cex.main = 2)
# normal
curve(dnorm(x,0,1), xlim = c(-5,5), xlab = "x", ylab = "f(x)", xaxt = "n", yaxt = "n", lwd = 2, cex.lab = 2)
title(expression(paste("Symmetric: ", gamma[1], " = 0")), line = 1, cex.main = 2)
# positive skewness
curve(dbeta(x,2,10), xlab = "x", ylab = "f(x)", xaxt = "n", yaxt = "n", lwd = 2, cex.lab = 2)
title(expression(paste("Right skewed: ", gamma[1], " > 0")), line = 1, cex.main = 2)

# Kurtosis plot

par(mar=c(5,5,5,2)+0.1)

curve(dnorm(x,0,1), xlim = c(-5,5), xlab = "x", ylab = "f(x)", lwd = 2, cex.lab = 2)

u = seq(-5,5,by=.05)
u1 = seq(-3,3,by=.05)
v = c(rep(0, 40), sqrt(9-u1^2)*2/pi/10, rep(0,40))
lines(u,v, type = "l", lwd = 2, col = 4)

curve(dcauchy(x, 0, 1), xlim = c(-5,5), col = 2, add = TRUE, lwd = 2)

legend("topright", col = c(2,1,4), lwd = c(2,2,2), c(expression(paste("Leptokurtic: ", gamma[2], " > 0")), expression(paste("Mesokurtic: ", gamma[2], " = 0")), expression(paste("Platykurtic: ", gamma[2], " < 0"))), cex = 1.5)

####################################
# House Price with data from Kaggle
####################################

library(tidyverse)
library(qcc)
# Read in data
housing <- read.csv("D:/OneDrive - The University of Newcastle/BI Book/domain_properties.csv")

housing_selected = housing %>% 
  mutate(type = case_when(type == "Apartment / Unit / Flat" ~ "Apartment", TRUE ~ type)) %>%
  filter(type %in% c("House", "Townhouse", "Apartment") & property_size < 1000 & num_bed > 0 & num_bed < 10)

housing_apartment = housing %>% 
  mutate(type = case_when(type == "Apartment / Unit / Flat" ~ "Apartment", TRUE ~ type)) %>%
  filter(type %in% c("Apartment") & property_size < 500 )

  
######################
# Data visualisations
######################

par(cex = 1.8)
par(mar = c(3.5, 3.5, 1.5, 1))
par(mgp = c(2.5, 1, 0))

## Number of bedrooms

plot(as.factor(housing_selected$num_bed), xlab = "Number of Bedrooms", ylab = "Frequency")

## Property size
hist(housing_selected$property_size, xlab = "Property Size", main = expression(Histogram ~ of ~ Property ~ Size ~ (m^2)))

boxplot(housing_selected$property_size, main = expression(Boxplot ~ of ~ Property ~ Size ~ (m^2)))

## Property type

df = as.numeric(table(housing_selected$type))
names(df) =  c("Apartment", "House", "Townhouse")
                
pareto.chart(df, main = "")

            
######################
# Regression analysis
######################

## overall model
overall_lm = lm(log(housing_apartment$price) ~ housing_apartment$property_size)
summary(overall_lm)

par(cex = 1.8)
par(mar = c(3.5, 3.5, 1.5, 1))
par(mgp = c(2.5, 1, 0))


## less than 2 beds
two_bed_price = housing_apartment$price[housing_apartment$num_bed <= 2]
two_bed_size  = housing_apartment$property_size[housing_apartment$num_bed <= 2]
two_bed_lm = lm(log(two_bed_price) ~ two_bed_size)
summary(two_bed_lm)


## less than 5 beds
five_bed_price = housing_apartment$price[housing_apartment$num_bed <= 5 & housing_apartment$num_bed > 3]
five_bed_size  = housing_apartment$property_size[housing_apartment$num_bed <= 5 & housing_apartment$num_bed > 3]
five_bed_lm = lm(log(five_bed_price) ~ five_bed_size)
summary(five_bed_lm)


## less than 9 beds
nine_bed_price = housing_apartment$price[housing_apartment$num_bed > 6]
nine_bed_size  = housing_apartment$property_size[housing_apartment$num_bed > 6]
nine_bed_lm = lm(log(nine_bed_price) ~ nine_bed_size)
summary(nine_bed_lm)


library(ggplot2)

housing_apartment = housing_apartment %>% mutate(Design = case_when(num_bed <= 2 ~ "1to2Bedrooms",
                                                num_bed > 2 ~ "3+Bedrooms"))




ggplot(housing_apartment, aes(property_size, log(price), colour = Design)) + geom_point() + geom_smooth(method = "lm", se = FALSE, fullrange = T) + 
  labs(x = "Property Size", y = "Log Price") + theme_grey(base_size = 22)


