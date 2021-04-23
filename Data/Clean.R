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
  mutate(month              = as.numeric(sapply(tranche_active_date, str_sub, start= 6, end=7)),
         year               = as.numeric(sapply(tranche_active_date, str_sub, start= 1, end=4)),
         year               = ifelse(is.na(month), year, ifelse(month<6, year-1, year)),
         lender_share       = lender_share/100,
         lender_amount_musd = tranche_amount_converted_musd*lender_share)%>%
  rename(country_name_borrower = country) %>%
  group_by(tranche_id, bvdid, lender_parent_name, country_name_borrower, year, month)%>%
  summarise(lender_amount_musd = sum(lender_amount_musd, na.rm = TRUE),
            lender_share       = sum(lender_share, na.rm = TRUE)) %>%
  ungroup()%>%
  full_join(select(bankscope, -c("country_code_lender", "bank_name")), by = c("bvdid", "year")) %>%
  mutate(tranche_id = as.character(tranche_id), year = as.character(year)) %>%
  mutate_if(is.numeric,Winsorize,probs = c(0.01, 0.99) ,na.rm = TRUE)%>%
  mutate(year = as.numeric(year)) %>%
  full_join(select(pillar3.data, -c("country_code_lender", "bank_name")), by = c("bvdid", "year")) %>%
  left_join(select(IRB.indicator, bvdid = bvdid_new, year, IRB, country_code_lender, bank_name), by = c("bvdid", "year"))%>% 
  left_join(basel.indicator, by = c("country_code_lender"))%>%
  left_join(SSM.indicator, by = c("bvdid", "year"))%>%
  left_join(rename(BRSS, country_code_lender = country_code), by = c("country_code_lender", "year"))%>%
  left_join(rename(supervision, country_code_lender = country_code), by = c("country_code_lender"))%>%
  left_join(rename(country.names, country_code_lender = country_code), by = c("country_code_lender"))%>%
  left_join(rename(WDI, country_code_lender = country_code), c("country_code_lender", "year")) %>%
  left_join(rename(IFS, country_code_lender = country_code), c("country_code_lender", "year", "month")) %>%
  left_join(select(filter(WDI, country_code=="US"), year, cpi_us = consumer_price_index), by = c("year")) %>%
  group_by(lender_parent_name, year)%>%
  mutate(RW                = ifelse(IRB==0, 1, RW),
         mean.RW           = ifelse(IRB==0, 1, mean.RW),
         mean.RW_adj       = ifelse(IRB==0, 1, mean.RW_adj),
         basel             = ifelse(year>=basel, 1, 0),
         SSM               = ifelse(is.na(SSM), 0, SSM),
         us_indicator      = ifelse(country_code_lender == "US", "US", "Other"),
         year.factor       = as.factor(ifelse(year>2006, year, 100)),
         n_loans_bank      = n(),
         constant.lender_amount     = 100*lender_amount_musd/consumer_price_index,
         log.lender_amount          = ifelse(lender_amount_musd>0, log(lender_amount_musd), NA),
         log.constant.lender_amount = ifelse(constant.lender_amount>0, log(constant.lender_amount), NA),
         log.asset     = log(totalassetsmillcu*exchangeratefromoriginalcurrencyusd),
         log.equity     = log(equitymillcu*exchangeratefromoriginalcurrencyusd),
         deposit.ratio = totalcustomerdepositsmillcu/totalassetsmillcu,
         loans.ratio   = grossloansmillcu/totalassetsmillcu,
         NII.ratio     = netinterestrevenuemillcu/operatingrevenueturnovermillcu,
         income.ratio  = plforperiodnetincomemillcu/totalassetsmillcu,
         ROE           = plbeforetaxmillcu/equitymillcu,
         #ROA           = plbeforetaxmillcu/totalassetsmillcu,
         #leverage      = equitymillcu/totalassetsmillcu,
         subordinated  = subordinateddebtsmemomillcu/(totalassetsmillcu - equitymillcu),
         NPL.ratio     = nonperfloansgrossloans/100,
         LLR.ratio     = loanlossresgrossloans/100,
         capital.ratio = totalcapitalratio/100,
         asset_share.gdp = totalassetsmillcu*10^6/gdp,
         asset_share.credit = asset_share.gdp*100/domestic_credit_to_private_sector_by_banks,
         supervisors_share.credit = number_supervisors*10^11/(domestic_credit_to_private_sector_by_banks*gdp),
         bank.year.id = paste0(bvdid,year)
    )%>%
  # IRB banks indicator
  group_by(bvdid)%>%
  mutate(IRB.indicator = ifelse(mean(IRB, na.rm =TRUE)>0, 1, 0))%>%
  group_by(country_code_lender, year) %>%
  mutate(mean.RW = ifelse(!is.na(bvdid), mean(mean.RW, na.rm=TRUE), NA),
         mean.RW_adj = ifelse(is.na(mean.RW_adj), mean.RW, mean.RW_adj))%>%
  group_by(tranche_id)%>%
  mutate(tranche_SA_indicator = mean(RW, na.rm = TRUE)) %>%
  group_by(lender_parent_name)%>% 
  arrange(lender_parent_name, year) %>%
  fill(c(RW, n_banks), .direction = "up")%>%
  ungroup() 

         
         loan.data <- loan.data %>%
  filter(bvdid %in% pillar3.data$bvdid, !is.na(lender_amount_musd)) %>%
  select(bvdid, country_code_lender, year, RW) %>%
  group_by(country_code_lender,bvdid, year) %>%
  summarise(mean.RW_bank = mean(RW, na.rm = TRUE),
            n_loans_bank = n(),
            w.mean.RW_bank = mean.RW_bank*n_loans_bank) %>% 
  group_by(country_code_lender, year) %>%
  mutate(n_loans_country = sum(n_loans_bank, na.rm = TRUE),
         w.mean.RW_adj   = (sum(w.mean.RW_bank, na.rm = TRUE)-w.mean.RW_bank)/(n_loans_country-n_loans_bank)) %>%
  ungroup() %>%
  select(bvdid, year, w.mean.RW_adj) %>%
  full_join(loan.data, by = c("bvdid", "year"))
  
loan.data <- loan.data %>%
  #filter(country_name_borrower != country_name, !is.na(lender_amount_musd)) %>%
  distinct(bvdid, year, .keep_all = TRUE) %>%
  select(bvdid, country_code_lender, year, RW, totalassetsmillcu) %>%
  group_by(country_code_lender, year) %>%
  mutate(w.RW = RW*(totalassetsmillcu/sum(totalassetsmillcu, na.rm = TRUE)),
         w.mean.RW   = sum(w.RW, na.rm = TRUE),
         mean.RW     = mean(RW, na.rm = TRUE),
         n_banks     = n(),
         mean.RW_with_SA   = (n_banks*mean.RW-RW)/(n_banks-1),
         w.mean.RW_with_SA = (w.mean.RW*sum(totalassetsmillcu, na.rm = TRUE)-RW*totalassetsmillcu)/(sum(totalassetsmillcu, na.rm = TRUE)-totalassetsmillcu)) %>%
  ungroup() %>%
  select(bvdid, year, mean.RW_with_SA, w.mean.RW_with_SA) %>%
  full_join(loan.data, by = c("bvdid", "year"))
  



save(loan.data, file=paste0("Data/Datasets/LoanData.Rda"))




min_asset <- loan.data %>%
  select(bvdid, year, exchangeratefromoriginalcurrencyusd, totalassetsmillcu, IRB, lender_amount_musd,bank_name) %>%
  mutate(asset =  exchangeratefromoriginalcurrencyusd*totalassetsmillcu) %>%
  filter( !is.na(lender_amount_musd)) %>%
  group_by(bank_name) %>%
  summarise(min_asset = min(asset, na.rm = TRUE)) %>%
  as.numeric()

test <- loan.data %>%
  select(bvdid, year, exchangeratefromoriginalcurrencyusd, totalassetsmillcu) %>%
  full_join(select(IRB.indicator, bvdid = bvdid_new, year, IRB, country_code_lender, bank_name), by = c("bvdid", "year"))%>% 
  group_by(bvdid)%>%
  mutate(asset =  exchangeratefromoriginalcurrencyusd*totalassetsmillcu,
         IRB_indicator = mean(IRB, na.rm = TRUE),
         missing_indicator = mean(IRB[year==2018])) %>%
  filter(asset>min_asset, IRB_indicator == 0, missing_indicator == 0) %>%
  distinct(bvdid, .keep_all = TRUE)




test2<-loan.data%>%

  filter(year > 2000, country_name_borrower != country_name)%>%
  select(bvdid, bank_name, IRB, country_code_lender, year ,log.lender_amount, RW, basel,
         mean.RW_with_SA, gdp_growth, policy_rate, log.asset, capital.ratio,
           ROE, deposit.ratio, loans.ratio, NII.ratio,  NPL.ratio, subordinated)%>%
 # filter(across(where(is.numeric), ~ !is.na(.x)))%>%
  #group_by(IRB)%>%
  #summarise(n = n())
  distinct(bvdid, year, .keep_all = TRUE)


test <-loan.data %>%
  filter(year > 2000, country_name_borrower != country_name)%>%
  distinct(country_code_lender)


test <-loan.data %>%
  filter(year > 2000, country_name_borrower != country_name)%>%
  mutate(aux = (domestic_credit_to_private_sector_by_banks*gdp))%>%
  distinct(country_code_lender, supervisors_share.credit, number_supervisors, aux, domestic_credit_to_private_sector_by_banks, gdp )

