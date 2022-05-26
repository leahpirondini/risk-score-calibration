###############################################################################################################
### Name: Calibration Plots
### Description: Produce calibration plots
###############################################################################################################

# load packages
library(survival)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(viridis)
library(gridExtra)
library(grid)
library(ipred)
library(rms)

# load data
data <- data

# event time and indicator
data$time <- data$time
data$event <- data$event

# risk prediction (from risk score)
data$p <- data$p

# group predictions into deciles
data <- mutate(data, bin = ntile(p,10)) 

# make calibration plots
km.fit <- survfit(Surv(time = time, event = event) ~ bin, data = data)
s10 <- unlist(summary(km.fit, times = 10)[6])
se_s10 <- unlist(summary(km.fit, times = 10)[7])
r10 <- 1 - s10
bin <- c(1:10)
km.bins <- as.data.frame(cbind(bin,r10,s10,se_s10))
km.bins$ul <- 1 - (km.bins$s10 - 1.96 * km.bins$se_s10)
km.bins$ll <- pmax(1 - (km.bins$s10 + 1.96 * km.bins$se_s10),0)

data <- merge(data, km.bins, by="bin")

# axis limits
axl <- 0.4

calplot <- mutate(data, bin=ntile(p,10)) %>%
  group_by(bin) %>%
  mutate(n=n(),
         bin_pred = mean(p),
         ll = ll,
         ul = ul) %>%
  ungroup() %>%
  ggplot(aes(x = bin_pred, y = r10, ymin = ll, ymax = ul)) +
  geom_pointrange(size = 0.2, color = "black") +
  scale_y_continuous(limits = c(-0.05, axl), breaks = seq(0, axl, by = 0.1)) +
  scale_x_continuous(limits = c(0, axl), breaks = seq(0, axl, by = 0.1)) +
  geom_abline() +
  xlab("Predicted probability") +
  ylab("Observed Probability") +
  ggtitle("") +
  theme_minimal()
calplot

predplot <- ggplot(data, aes(x = p)) +
  geom_histogram(fill="blue", bins = 200) +
  scale_x_continuous(limits = c(0, axl), breaks = seq(0, axl, by = 0.1)) +
  xlab("Predicted Probability") +
  ylab("") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        axis.text.y=element_text(color="White"))

g <- arrangeGrob(calplot, predplot, respect = TRUE, heights = c(1, 0.25), ncol = 1)
grid.newpage()
grid.draw(g)
