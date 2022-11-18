
# source the pulse height tool script 

source("Pulse_Height.R")

library(ggplot2)
library(tidyverse)
library(patchwork)


df = read.csv("Clean_simsigs_50k.csv")

paste("Total signals present in df : ", nrow(df))


avgpulse2(df)

imgph(df)


get_imgphsm(df)


