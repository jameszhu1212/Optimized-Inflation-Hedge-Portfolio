library(tidyverse)
library(lubridate)
library(readxl)
library(highcharter)
library(tidyquant)
library(timetk)
library(tibbletime)
library(quantmod)
library(PerformanceAnalytics)
library(scales)
library(broom)


# 
# Multiple Sharpe Ratio Calculation
Stock_Data_Files <- dir(path = "~/Desktop/CEF Research/Opt P Docs/data", full.names = TRUE)
stock_data_list <- vector("list",length(Stock_Data_Files))
for (i in seq_along(Stock_Data_Files))
{
  stock_data_list[[i]] = read_xlsx(Stock_Data_Files[[i]])
}
stock_data_list

Treasury_Bill_Rate_Files <- dir(path = "~/Desktop/CEF Research/Opt P Docs/billrate", full.names = TRUE)
treasury_bill_rate_list <- vector("list",length(Treasury_Bill_Rate_Files))
for (i in seq_along(Treasury_Bill_Rate_Files))
{
  treasury_bill_rate_list[[i]] = read_xls(Treasury_Bill_Rate_Files[[i]])
}
treasury_bill_rate_list

Portfolio_Optimization <- function(x,y) {
  # #N/A was converted to 0 when bill rate was imported from excel 
  One_Month_Bill_Rate <- y[-c(1:10),] %>%
    rename("Bill_Rate"="...2")%>%
    select(Bill_Rate)%>%
    mutate(Bill_Rate = as.double(Bill_Rate))
  Risk_Free_Rate <- colMeans(One_Month_Bill_Rate)/100
  No_NA <- x%>%
    # Use =, don't use == or <-
    replace_na(list(`Cash Dividends - Daily`=0,
                    `Special Cash Dividends - Daily`=0))
  Calculate_HPR <- No_NA %>%
    mutate(
      Adj_Closing_Price = `Price - Close - Daily` / 
        `Adjustment Factor (Issue)-Cumulative by Ex-Date`,
      # 1L is integer 1
      HPR = (Adj_Closing_Price + `Special Cash Dividends - Daily` +
               `Cash Dividends - Daily` - lag(Adj_Closing_Price, n=1L))
      / lag(Adj_Closing_Price, n=1L))
  
  Stock_HPR_Matrix <- Calculate_HPR %>%
    filter(`Data Date - Dividends`>Calculate_HPR$`Data Date - Dividends`
           [1])%>%
    select(`Data Date - Dividends`,`Ticker Symbol`, HPR)%>%
    pivot_wider(names_from = `Ticker Symbol`,values_from = HPR)%>%
    select(-(`Data Date - Dividends`))
  
  Stock_HPR_Mean<-colMeans(Stock_HPR_Matrix)%>%
    as.vector()
  Transposed_Demeaned<-t(Stock_HPR_Matrix)-Stock_HPR_Mean
  Demeaned<-t(Transposed_Demeaned)
  
  # %*% is for matrix multiplication IRL
  Variance_Covariance_Matrix<-((Transposed_Demeaned%*%Demeaned)/
                                 (nrow(Stock_HPR_Matrix)-1))
  
  # solve() to calculate matrix inverse
  Inverse_Variance_Covariance_Matrix<-solve(Variance_Covariance_Matrix)
  
  
  Stock_Premium <- Stock_HPR_Mean - Risk_Free_Rate
  
  Zj <- Inverse_Variance_Covariance_Matrix %*% Stock_Premium
  Zj_Sum <- colSums(Zj)
  Optimal_Weights <- Zj/Zj_Sum
  
  Expected_Return <- t(Stock_HPR_Mean)%*%Optimal_Weights
  Portfolio_Variance <- (
    (t(Optimal_Weights)%*%Variance_Covariance_Matrix)
    %*%Optimal_Weights)
  Sharpe_Ratio <- ((Expected_Return - Risk_Free_Rate)/
                     (sqrt(Portfolio_Variance)))
  Sharpe_Ratio
}
map2(stock_data_list, treasury_bill_rate_list, Portfolio_Optimization)

# or the loop version (same thing though)
# all_calculations_2 <- function (x, y, f)
# {
  all_calculations_2_vector <- vector("list", length(df_stock_data_list))
  for (i in seq_along(df_stock_data_list))
  {
  all_calculations_2_vector[[i]] = f(x[[i]], y[[i]])
  }
  all_calculations_2_vector

# all_calculations_2(stock_data_list, treasury_bill_rate_list, Portfolio_Optimization)

# Multiple Sharpe Ratio Calculation
# 

  
# 
# Optimal Weights
Weights <- function(x, y) {
  # #N/A was converted to 0 when bill rate was imported from excel 
  One_Month_Bill_Rate <- y[-c(1:10),] %>%
    rename("Bill_Rate"="...2")%>%
    select(Bill_Rate)%>%
    mutate(Bill_Rate = as.double(Bill_Rate))
  Risk_Free_Rate <- colMeans(One_Month_Bill_Rate)/100
  No_NA <- x%>%
    # Use =, don't use == or <-
    replace_na(list(`Cash Dividends - Daily`=0,
                    `Special Cash Dividends - Daily`=0))
  Calculate_HPR <- No_NA %>%
    mutate(
      Adj_Closing_Price = `Price - Close - Daily` / 
        `Adjustment Factor (Issue)-Cumulative by Ex-Date`,
      # 1L is integer 1
      HPR = (Adj_Closing_Price + `Special Cash Dividends - Daily` +
               `Cash Dividends - Daily` - lag(Adj_Closing_Price, n=1L))
      / lag(Adj_Closing_Price, n=1L))
  
  Stock_HPR_Matrix <- Calculate_HPR %>%
    filter(`Data Date - Dividends`>Calculate_HPR$`Data Date - Dividends`
           [1])%>%
    select(`Data Date - Dividends`,`Ticker Symbol`, HPR)%>%
    pivot_wider(names_from = `Ticker Symbol`,values_from = HPR)%>%
    select(-(`Data Date - Dividends`))
  
  Stock_HPR_Mean<-colMeans(Stock_HPR_Matrix)%>%
    as.vector()
  Transposed_Demeaned<-t(Stock_HPR_Matrix)-Stock_HPR_Mean
  Demeaned<-t(Transposed_Demeaned)
  
  # %*% is for matrix multiplication IRL
  Variance_Covariance_Matrix<-((Transposed_Demeaned%*%Demeaned)/
                                 (nrow(Stock_HPR_Matrix)-1))
  
  # solve() to calculate matrix inverse
  Inverse_Variance_Covariance_Matrix<-solve(Variance_Covariance_Matrix)
  
  
  Stock_Premium <- Stock_HPR_Mean - Risk_Free_Rate
  
  Zj <- Inverse_Variance_Covariance_Matrix %*% Stock_Premium
  Zj_Sum <- colSums(Zj)
  Optimal_Weights <- Zj/Zj_Sum
}
Calculate_Weights <- map2(stock_data_list, treasury_bill_rate_list, Weights)
# Optimal Weights
# 



# Visualization

asset_returns_xts <- stock_data_list[[2]]%>%
  replace_na(list(`Cash Dividends - Daily`=0,
                  `Special Cash Dividends - Daily`=0))%>%
  mutate(
    Adj_Closing_Price = `Price - Close - Daily` / 
      `Adjustment Factor (Issue)-Cumulative by Ex-Date`,
    HPR = (Adj_Closing_Price + `Special Cash Dividends - Daily` +
             `Cash Dividends - Daily` - lag(Adj_Closing_Price, n=1L))
    / lag(Adj_Closing_Price, n=1L))%>%
  rename(date = "Data Date - Dividends",
         asset = "Ticker Symbol")%>%
  filter(date >= "2021-06-01")%>%
  select(date, asset, HPR)%>%
  pivot_wider(names_from = asset, values_from = HPR)%>%
  tk_xts(date_var = date)

w <- (Calculate_Weights[[1]])[c(2,1,3:8),]
 #make sure weights align with each stock 
portfolio_return_xts <- Return.portfolio(asset_returns_xts, 
                                         weights = w, 
                                         rebalance_on = "days")%>%
  `colnames<-`("HPR")
# Line graph of portfolio return
highchart(type = "stock")%>%
  hc_title(text = "HPR")%>%
  hc_add_series(portfolio_return_xts, color = "red",
                name = "Daily" )%>%
  hc_add_theme(hc_theme_bloom()) %>%
  hc_navigator(enabled = FALSE) %>%
  hc_scrollbar(enabled = FALSE) %>%
  hc_legend(enabled = TRUE) %>%
  hc_exporting(enabled = TRUE)%>%
  hc_tooltip(pointFormat = "{point.y: .6f}")

# Histogram of portfolio return and assets returns
# facet(~asset) and remove geom_hist of portfolio return
asset_return_tbltime_index_false <- asset_returns_xts%>%
  tk_tbl(preserve_index = FALSE)%>%
  pivot_longer(everything(), names_to = "asset", values_to = "HPR")
ggplot(asset_return_tbltime_index_false, aes(x = HPR))+
  geom_histogram(aes(fill = asset), alpha = .5, binwidth = .005)+
  geom_histogram(data = portfolio_return_xts, aes(x = HPR), fill = "red",
                 binwidth = .005)

# Density distribution of asset returns and portfolio returns 
ggplot(asset_return_tbltime_index_false, aes(x = HPR))+
  geom_density(aes(color = asset), alpha = .5)+
  geom_density(data = portfolio_return_xts, color ="red")+
  facet_wrap(~asset)

# HPR line graph of all the assets in the portfolio
asset_return_tbltime_index_true <- asset_returns_xts%>%
  tk_tbl(preserve_index = TRUE, rename_index = "date")%>%
  pivot_longer(c("CPER", "GLD", "QQQ", "SLV", "SPY", "XLF", "XOP", "XRT"),
               names_to = "asset", 
               values_to = "HPR")
ggplot(asset_return_tbltime_index_true, aes(x = date, y = HPR))+
  geom_line(aes(group = asset, color = asset))+
  geom_line(data = portfolio_return_xts, aes())
  scale_y_continuous(labels = scales::percent)+
  scale_x_datetime(date_labels = "%m/%d")
  

  # Portfolio return scatter plot
portfolio_return_tbltime_index_true <- portfolio_return_xts%>%
    tk_tbl(preserve_index = TRUE, rename_index = "date")
mean_plot <- mean(portfolio_return_tbltime_index_true$HPR)
sd_plot <- sd(portfolio_return_tbltime_index_true$HPR)
portfolio_return_tbltime_index_true%>%
  mutate( hist_green_color = if_else(HPR > (mean_plot+sd_plot), HPR, 
                                       as.numeric(NA)),
            hist_red_color = if_else(HPR < (mean_plot-sd_plot), HPR,
                                     as.numeric(NA)),
            hist_blue_color = if_else (HPR >= (mean_plot-sd_plot) &
                                         HPR <= (mean_plot+sd_plot), HPR,
                                       as.numeric(NA)))%>%
  ggplot(aes(x = date))+
    geom_point(aes(y = hist_green_color), color = "green")+
    geom_point(aes(y = hist_red_color), color = "red")+
    geom_point(aes(y = hist_blue_color), color = "blue")+
    geom_hline(yintercept = sd_plot+mean_plot, color = "purple",
               linetype = "dashed")+
    geom_hline(yintercept = mean_plot-sd_plot, color = "purple",
               linetype = "dashed")+
    labs(title = "Scatter Plot", y = "HPR", x = "Date")+
    scale_x_datetime(date_breaks = "5 days", date_labels = "%m/%d")


# Portfolio sd calculations
portfolio_sd_xts <- StdDev(asset_returns_xts, weights = w)%>%
  as.numeric()
portfolio_sd_tq <- portfolio_return_tbltime_index_true%>%
  tq_performance(Ra = HPR,
                 performance_fun = table.Stats)%>%
  select(Stdev)%>%
  as.numeric()
portfolio_sd_tidyverse <- portfolio_return_tbltime_index_true%>%
  summarize(sd =sd(HPR))%>%
  as.numeric()

# Plot of Portfolio and Asset Sd
portfolio_and_asset_sd_tibble <- asset_return_tbltime_index_false%>%
  group_by(asset)%>%
  summarise(sd = sd(HPR))%>%
  add_row(asset  = "Portfolio", sd = portfolio_sd_tidyverse)
ggplot(portfolio_and_asset_sd_tibble, aes(asset, sd))+
  geom_point(aes(color = asset))+
  geom_text(aes(x = "Portfolio", y = portfolio_sd_tidyverse + .0005,
            label = "Portfolio"), color = "red")+
  scale_y_continuous(labels = scales::percent)+
  labs(y = "Standard Deviation", x ="Asset")
  
# PLot of Avg Return vs Sd 

avg_return_and_sd_of_port_and_assets <- asset_return_tbltime_index_false%>%
  group_by(asset)%>%
  summarize(mean = mean(HPR),
            sd = sd(HPR))%>%
  add_row(asset = "Portfolio", 
          mean = mean(portfolio_return_tbltime_index_true$HPR),
          sd = sd(portfolio_return_tbltime_index_true$HPR))
ggplot(avg_return_and_sd_of_port_and_assets, aes(sd, mean, color = asset))+
  geom_point()+
  scale_y_continuous(labels = scales::percent)+
  labs(x = "Standard Deviation", y ="Average Return")+
  geom_text(x = sd(portfolio_return_tbltime_index_true$HPR),
            y = mean(portfolio_return_tbltime_index_true$HPR)+.0005,
            label = "Portfolio", color = "red")











