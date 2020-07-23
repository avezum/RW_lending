##=============================================================================##
## Project:  RW Arbitrage and Bank Lending                                     ##
## Author: Lucas Avezum, Tilburg University                                    ##
## Description: this file cleans the raw dataset in .R format                  ##
##=============================================================================##

##=============================================================================##
## Code setup                                                                  ##
##=============================================================================##

rm(list = ls())  
ptm <- proc.time()
library(tidyverse)                ## Data manipulation, pipe operator
library(zoo)                      ## Apply function to rolling margins of data
library(DescTools)                ## Command to winsorize variables
library(readxl)                   ## Command to open xlsx files                                                     
#library(Hmisc)                    ## Weighted variance and mean functions
#library(moments)                  ## Moments functions
#library(splitstackshape)          ## Command to expand data


# Open dirt data frame
load("Data/Temp/BankScope.Rda")
load("Data/Temp/Pillar3Data.Rda")
load("Data/Temp/AuxiliarData.Rda")


##============================================================================##
## Pillar-III reports                                                         ##
##============================================================================##

#------------------------------------------------------------------------------#
# Basic cleaning and data creation of raw datasets                             #
#------------------------------------------------------------------------------#

