library(quantmod)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(DT)
library(tidyverse)
library(lubridate)

master_df <- read.csv('daily-website-visitors.csv')
day_list <- c('Sunday', 'Monday', 'Tuesday', 'Wednesday','Thursday', 'Friday', 'Saturday')



master_df$X <- NULL

master_df <- master_df %>% drop_na()
master_df$dt <- as.Date(master_df$dt)