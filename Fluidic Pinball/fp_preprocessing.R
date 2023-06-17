library("reshape2")
library(tseries)
library(ggplot2)
library(factoextra)
library(sindyr)
library(forecast)
library(devtools)
library(stringr)
library(latex2exp)
library(progress)
library(roxygen2)
library(tidyr)
library("mltsp")
library("e1071")
library(narmax)
library(caret)
library(tidyverse)
library(purrr)
library(dplyr)
library(MTS)
library(vars)
library(igraph)
library(bdsmatrix)
library(TTR)
library(plotly)
library(MLmetrics)
library(feasts)
library(tsibble)
library(lubridate)
library(rtweet)
library(graphics)
library("plot3D")
library(ggrepel)
library(tikzDevice)
library(ggpubr)
library(ggridges)
library(hydroGOF)
library(boot)
library(nptest)
library(npreg)
library(PLRModels)


####################
### Read in data ###
####################
Cl = read.table('C:/Users/evsop/OneDrive/Documents/Master/4th Term/TFM/Code/FluidicPinball/Input/ClValues.txt', header = FALSE)
lift = apply(Cl[, 2:4], 1, sum)

Cd = read.table('C:/Users/evsop/OneDrive/Documents/Master/4th Term/TFM/Code/FluidicPinball/Input/CdValues.txt', header = FALSE)
drag = apply(Cd[, 2:4], 1, sum)

ClCd = data.frame(Cl = lift, Cd = drag)

# Drop the first 101 lines
ClCd = ClCd[401:nrow(ClCd), ]

# Vector of rotation values in training data
u_list = c(1.01, 1.02, 1.03, 1.04, 1.05, 1.06, 1.07, 1.08, 1.09, 1.1, 
           1.11, 1.12, 1.13, 1.14, 1.15, 1.16, 1.17, 1.18, 1.19, 1.2, 1.21, 
           1.22, 1.23, 1.24, 1.25, 1.26, 1.27, 1.28, 1.29, 1.3, 1.31, 1.32, 
           1.33, 1.34, 1.35, 1.36, 1.37, 1.38, 1.39)

u = NULL

for (i in u_list) {
  u_temp = rep(i, 300)
  u = c(u, u_temp)
}

ClCd_final = data.frame(ClCd, u)

xyzu_train = ClCd_final[1:10400, ]
xyzu_pred = ClCd_final[10401:nrow(ClCd_final), ]

# Generate random initial points between 200
nb_trials = 500              # number of different trials/initial prediction points
nb_series = 2                # number of univariate time series
initial_points = sort(sample(1:1000, nb_trials), decreasing = FALSE)
setwd('C:/Users/evsop/OneDrive/Documents/Master/4th Term/TFM/Code/FluidicPinball')


#############
### Plots ###
#############
# Plot significant compomenets
for(i in 1:ncol(ClCd_final)) {
  ts = ts(ClCd_final[, i], start = c(0,1), frequency = 1)
  plot.ts(ts)
}

for(i in 1:ncol(ClCd_final)) {
  ts = ts(ClCd_final[1:1000, i], start = c(0,1), frequency = 1)
  plot.ts(ts)
}
