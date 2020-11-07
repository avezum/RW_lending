##=============================================================================##
## Project:  RW Arbitrage and Bank Lending                                     ##
## Author: Lucas Avezum, Tilburg University                                    ##
## Description: this file cleans the raw dataset in .R format                  ##
##=============================================================================##

##=============================================================================##
## Code setup                                                                  ##
##=============================================================================##

rm(list = ls())  
library(tidyverse)                ## Data manipulation, pipe operator
#library(zoo)                      ## Apply function to rolling margins of data
library(DescTools)                ## Command to winsorize variables
library(readxl)                   ## Command to open xlsx files                                                     
#library(Hmisc)                    ## Weighted variance and mean functions
#library(moments)                  ## Moments functions
#library(splitstackshape)          ## Command to expand data
#source("Auxiliar/Function.R")     ## Capital requirements functions

# Open dirt data frame
load("Data/Temp/BankScope.Rda")
load("Data/Temp/Pillar3Data.Rda")
load("Data/Temp/DealScanData.Rda")
load("Data/Temp/AuxiliarData.Rda")


##============================================================================##
## Merge datasets                                                             ##
##============================================================================##

loan.data <- lender.data %>%
  select(tranche_id, bvdid, lender_parent_name, lender_share, primary_role) %>%
  filter(between(lender_share, 0, 100)) %>%
  inner_join(
    select(tranche.data, tranche_id, tranche_active_date, tranche_amount_converted_musd, country),
    by = c("tranche_id")) %>%
  mutate_if(is.numeric,Winsorize,probs = c(0.01, 0.99) ,na.rm = TRUE)%>%
  mutate(month              = as.numeric(sapply(tranche_active_date, str_sub, start= 6, end=7)),
         year               = as.numeric(sapply(tranche_active_date, str_sub, start= 1, end=4)),
         year               = ifelse(is.na(month), year, ifelse(month<6, year-1, year)),
         lender_amount_musd = tranche_amount_converted_musd*lender_share/100)%>%
  rename(country_name_borrower = country) %>%
  group_by(tranche_id, bvdid, lender_parent_name, country_name_borrower, year, month)%>%
  summarise(lender_amount_musd = sum(lender_amount_musd, na.rm = TRUE)) %>%
  ungroup()%>%
  full_join(select(pillar3.data, -c("country_code_lender", "bank_name")), by = c("bvdid", "year")) %>%
  full_join(select(bankscope, -c("country_code_lender", "bank_name")), by = c("bvdid", "year")) %>%
  left_join(select(IRB.indicator, bvdid = bvdid_new, year, IRB, country_code_lender, bank_name), by = c("bvdid", "year"))%>% 
  left_join(basel.indicator, by = c("country_code_lender"))%>%
  left_join(SSM.indicator, by = c("bvdid", "year"))%>%
  left_join(rename(BRSS, country_code_lender = country_code), by = c("country_code_lender", "year"))%>%
  left_join(rename(WDI, country_code_lender = country_code), c("country_code_lender", "year")) %>%
  left_join(rename(IFS, country_code_lender = country_code), c("country_code_lender", "year", "month")) %>%
  left_join(select(filter(WDI, country_code=="US"), year, cpi_us = consumer_price_index), by = c("year")) %>%
  group_by(lender_parent_name, year)%>%
  mutate(RW                = ifelse(IRB==0, 1, RW),
         mean.RW           = ifelse(IRB==0, 1, mean.RW),
         mean.RW_adj       = ifelse(IRB==0, 1, mean.RW_adj),
         basel             = ifelse(year>=basel, 1, 0),
         SSM               = ifelse(is.na(SSM), 0, 1),
         us_indicator      = ifelse(country_code_lender == "US", "US", "Other"),
         year.factor       = as.factor(year),
         n_loans           = n(),
         lender_amount     = lender_amount_musd/consumer_price_index,
         log.lender_amount = ifelse(lender_amount>0, log(lender_amount), NA)
    )%>%
  group_by(tranche_id)%>%
  mutate(tranche_SA_indicator = mean(RW, na.rm = TRUE)) %>%
  group_by(lender_parent_name)%>% 
  arrange(lender_parent_name, year) %>%
  fill(RW, .direction = "up")

save(loan.data, file=paste0("Data/Datasets/LoanData.Rda"))

