library(shiny)
library(DT)
library(lpSolve)
library(tidyverse)

source('lib.R')

initialDf <- readr::read_csv('data/nutrition_lp.csv')
