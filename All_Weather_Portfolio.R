# Libraries ----
library(tidyquant)
library(tidyverse)
library(scales)
library(kableExtra)
library(riskParityPortfolio)
library(gt)

# Local Variables ----
investment_rp    <- 100000
lookback_period  <- 22 # Historical Period to be used for calculating the weights (Risk Parity)
static_Weights   <- c(0.3, 0.4, 0.15, 0.075, 0.075)
symbols          <- c("VTI", "TLT", "IEF", "GLD", "DBC")
Risk_Parity_Flag <- TRUE # If you want to calculate dynamic weights using Risk Parity type TRUE, if you want to use the static weights, type FALSE
Rebal_Frequency  <- "months" # can be: "years", "quarters", "months", "weeks", "days"

# Calculating the Returns ----
db_returns <- symbols %>% # Type as many tickers as you want
  tq_get(get  = "stock.prices",
         from = Sys.Date() - lubridate::years(100),
         to   = Sys.Date()) %>% 
  select(symbol, date, adjusted) %>% 
  group_by(symbol) %>% 
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily") %>%
  spread(symbol, daily.returns) %>%
  na.omit() 

# Defining the weights ----
if(Risk_Parity_Flag == TRUE){
  i <- 1
  db_weights_rp <- NULL
  while(i < nrow(db_returns)){ # i <- 1
    if(i + lookback_period > nrow(db_returns)){
      break
    }else{
      
      d_returns_slice <- db_returns %>% dplyr::slice(i:(lookback_period + i))
      
      res <- riskParityPortfolio(cov(d_returns_slice %>% dplyr::select(-date)),
                                 method_init   = "newton",
                                 method        = "alabama",
                                 use_gradient  = TRUE, 
                                 use_qp_solver = TRUE,
                                 maxiter       = 500)
      
      db_weights_rp <- bind_rows(db_weights_rp, res$w)
    }
    
    i <- i + 1
  }
  
  AA_Risk_Parity <- db_weights_rp %>%
    dplyr::mutate(date = (db_returns %>% 
                            dplyr::slice((lookback_period + 1):n()) %>% 
                            dplyr::select(date)) %>% 
                    pull(1)) %>%
    dplyr::select(date, everything())
  
  # Adjusting the dates of the Returns to the dates of the AA_Risk_Parity
  db_returns <- db_returns %>%
    dplyr::filter(date %in% AA_Risk_Parity$date)
  
}else{
  AA_Risk_Parity <- static_Weights
}

# Changing the format to xts
db_returns <- db_returns %>%
  column_to_rownames(var = "date") %>%
  as.xts()

AA_Risk_Parity <- AA_Risk_Parity %>%
  column_to_rownames(var = "date") %>%
  as.xts() 

# Setting the first row as ZERO (first day of trading)
db_returns[1,] <- 0

# Portfolio calculations
data_Portfolio <- PerformanceAnalytics::Return.portfolio(db_returns,
                                                         weights      = AA_Risk_Parity,
                                                         wealth.index = TRUE,
                                                         verbose      = TRUE,
                                                         geometric    = TRUE,
                                                         rebalance_on = Rebal_Frequency) %>%
  as.data.frame() %>%
  rownames_to_column(var = "date")

# Splitting data_Portfolio according to the type of the data
Portfolio_Returns <- data_Portfolio %>%
  dplyr::mutate(date = as.Date(date)) %>%
  dplyr::select(date, portfolio.returns) %>%
  as_tibble()

Portfolio_Weights <- data_Portfolio %>%
  dplyr::select(date, starts_with("EOP.Weight.")) %>%
  purrr::set_names(str_remove(colnames(.), "EOP.Weight.")) %>%
  column_to_rownames(var = "date") %>%
  dplyr::rename_with( ~ paste("Dynamic_AA", .x, sep = "_")) %>%
  rownames_to_column(var = "date") %>%
  dplyr::mutate(date = as.Date(date)) %>%
  as_tibble()

# Calendar Returns ----
YTD_Returns <- Portfolio_Returns %>%
  dplyr::mutate(Year = lubridate::year(date),
                Portfolio_Return_Log = log(1 + portfolio.returns)) %>%
  dplyr::select(Year, Portfolio_Return_Log) %>%
  replace(is.na(.), 0) %>%
  group_by(Year) %>%
  summarise(across(everything(), sum), .groups = "drop") %>%
  dplyr::mutate(Performance = round((exp(Portfolio_Return_Log) - 1)*100, digits = 2)) %>%
  dplyr::select(-Portfolio_Return_Log)

Portfolio_Returns %>%
  dplyr::mutate(Year = lubridate::year(date),
                Month = lubridate::month(date),
                Portfolio_Return_Log = log(1 + portfolio.returns)) %>%
  dplyr::select(Year, Month, Portfolio_Return_Log) %>%
  group_by(Year, Month) %>%
  replace(is.na(.), 0) %>%
  summarise(across(everything(), sum), .groups = "drop") %>%
  ungroup() %>%
  dplyr::mutate(Port_Monthly_Return = exp(Portfolio_Return_Log) - 1,
                Port_Monthly_Return = round(Port_Monthly_Return*100, digits = 2)) %>%
  dplyr::select(-Portfolio_Return_Log) %>%
  spread(Month, Port_Monthly_Return) %>%
  purrr::set_names(c("Year","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) %>%
  left_join(YTD_Returns, by = "Year") %>%
  gt() %>%
  tab_header(
    title    = md("**All Weather Portfolio**"),
    subtitle = "Monthly Performance (%)"
  ) %>%
  fmt_missing(
    columns = 1:13,
    missing_text = ""
  )

# Equity Chart ----
data_Portfolio %>%
  dplyr::select(date, portfolio.wealthindex) %>%
  dplyr::mutate(portfolio.wealthindex = portfolio.wealthindex * investment_rp,
                date = as.Date(date)) %>%
  purrr::set_names(c("date", "NAV")) %>%
  ggplot(aes(x = date, y = NAV)) + 
    geom_line(alpha = 0.70, size = 1, colour = "steelblue") +
    theme_tq() +
    labs(title    = str_glue("Equity Curve - Initial NAV: {dollar_format(prefix = '', suffix = '$')(investment_rp)}"),
         subtitle = "Daily Scale",
         caption  = "By: Carlos Jimenez",
         x        = "",
         y        = "Net Asset Value") +
    scale_y_continuous(labels = scales::dollar)

# Drawdown ----
PerformanceAnalytics::Drawdowns(Portfolio_Returns %>% 
                                  dplyr::select(date, portfolio.returns) %>%
                                  column_to_rownames(var = "date") %>%
                                  as.xts()) %>%
  as.data.frame() %>%
  purrr::set_names("DD") %>%
  rownames_to_column(var = "date") %>%
  dplyr::mutate(date = as.Date(date)) %>%
  ggplot(aes(x = date, y = DD)) + 
  geom_line(alpha = 0.70, size = 1, colour = "steelblue") +
  theme_tq() +
  labs(title    = str_glue("Drawdown Curve - Initial NAV: {dollar_format(prefix = '', suffix = '$')(investment_rp)}"),
       subtitle = "Daily Scale",
       caption  = "By: Carlos Jimenez",
       x        = "",
       y        = "Drawdown") +
  scale_y_continuous(labels = scales::percent)

# Key KPI ----
# Total MTD 
Total_MTD <- Portfolio_Returns %>%
  dplyr::mutate(Year = lubridate::year(date),
                Month = lubridate::month(date),
                Quarter = lubridate::quarter(date),
                Portfolio_Return_Log = log(1 + portfolio.returns)) %>%
  dplyr::filter(Year == year(Sys.Date())) %>%
  dplyr::select(Year, Month, Portfolio_Return_Log) %>%
  dplyr::group_by(Month) %>% 
  summarise(MTD = sum(Portfolio_Return_Log), .groups = 'drop') %>%
  tail(n = 1) %>%
  dplyr::mutate(MTD = exp(MTD) - 1) %>%
  dplyr::select(MTD) %>%
  pull(1) %>%
  percent(accuracy = 0.01)

# Total QTD 
Total_QTD <- Portfolio_Returns %>%
  dplyr::mutate(Year = lubridate::year(date),
                Month = lubridate::month(date),
                Quarter = lubridate::quarter(date),
                Portfolio_Return_Log = log(1 + portfolio.returns)) %>%
  dplyr::filter(Year == year(Sys.Date())) %>%
  dplyr::select(Year, Quarter, Portfolio_Return_Log) %>%
  dplyr::group_by(Quarter) %>% 
  summarise(QTD = sum(Portfolio_Return_Log), .groups = 'drop') %>%
  tail(n = 1) %>%
  dplyr::mutate(QTD = exp(QTD) - 1) %>%
  dplyr::select(QTD) %>%
  pull(1) %>%
  percent(accuracy = 0.01)

# Total YTD 
Total_YTD <- Portfolio_Returns %>%
  dplyr::mutate(Year = lubridate::year(date),
                Month = lubridate::month(date),
                Quarter = lubridate::quarter(date),
                Portfolio_Return_Log = log(1 + portfolio.returns)) %>%
  dplyr::filter(Year == year(Sys.Date())) %>%
  dplyr::select(Year, Portfolio_Return_Log) %>%
  dplyr::group_by(Year) %>% 
  summarise(YTD = sum(Portfolio_Return_Log), .groups = 'drop') %>%
  tail(n = 1) %>%
  dplyr::mutate(YTD = exp(YTD) - 1) %>%
  dplyr::select(YTD) %>%
  pull(1) %>%
  percent(accuracy = 0.01)

# Total Returns
db_portfolio <- data_Portfolio %>%
  dplyr::select(date, portfolio.wealthindex) %>%
  dplyr::mutate(portfolio.wealthindex = portfolio.wealthindex * investment_rp,
                date = as.Date(date)) %>%
  purrr::set_names(c("date", "NAV"))

Total_Returns <- (db_portfolio %>% dplyr::select(NAV) %>% tail(n = 1) %>% pull(1) / 
                    db_portfolio %>% dplyr::select(NAV) %>% head(n = 1) %>% pull(1) - 1) %>%
  percent(accuracy = 0.01)

# Annualize data (Return - Volatility - CAGR)
data_annualized <- PerformanceAnalytics::table.AnnualizedReturns(Portfolio_Returns %>% 
                                                                   dplyr::select(date, portfolio.returns) %>%
                                                                   column_to_rownames(var = "date") %>%
                                                                   as.xts(),
                                                                 scale = 252,
                                                                 Rf    = 0)
# Max Drawdown
Max_DD <- PerformanceAnalytics::Drawdowns(Portfolio_Returns %>% 
                                            dplyr::select(date, portfolio.returns) %>%
                                            column_to_rownames(var = "date") %>%
                                            as.xts()) %>%
  as.data.frame() %>%
  purrr::set_names("DD") %>%
  dplyr::select(DD) %>%
  pull() %>%
  min()

# Max Return
Max_Return_Monthly <- Portfolio_Returns %>%
  dplyr::mutate(Year = lubridate::year(date),
                Month = lubridate::month(date),
                Portfolio_Return_Log = log(1 + portfolio.returns)) %>%
  dplyr::select(Year, Month, Portfolio_Return_Log) %>%
  group_by(Year, Month) %>%
  replace(is.na(.), 0) %>%
  summarise(across(everything(), sum), .groups = "drop") %>%
  ungroup() %>%
  dplyr::mutate(Port_Monthly_Return = exp(Portfolio_Return_Log) - 1) %>%
  dplyr::select(Port_Monthly_Return) %>%
  max() %>%
  percent(accuracy = 0.01)

# Min Return
Min_Return_Monthly <- Portfolio_Returns %>%
  dplyr::mutate(Year = lubridate::year(date),
                Month = lubridate::month(date),
                Portfolio_Return_Log = log(1 + portfolio.returns)) %>%
  dplyr::select(Year, Month, Portfolio_Return_Log) %>%
  group_by(Year, Month) %>%
  replace(is.na(.), 0) %>%
  summarise(across(everything(), sum), .groups = "drop") %>%
  ungroup() %>%
  dplyr::mutate(Port_Monthly_Return = exp(Portfolio_Return_Log) - 1) %>%
  dplyr::select(Port_Monthly_Return) %>%
  min() %>%
  percent(accuracy = 0.01)

# Positive_Months
Positive_Months <- Portfolio_Returns %>%
  dplyr::mutate(Year = lubridate::year(date),
                Month = lubridate::month(date),
                Portfolio_Return_Log = log(1 + portfolio.returns)) %>%
  dplyr::select(Year, Month, Portfolio_Return_Log) %>%
  group_by(Year, Month) %>%
  replace(is.na(.), 0) %>%
  summarise(across(everything(), sum), .groups = "drop") %>%
  ungroup() %>%
  dplyr::mutate(Port_Monthly_Return = exp(Portfolio_Return_Log) - 1) %>%
  dplyr::select(Port_Monthly_Return) %>%
  pull()

Positive_Months <- (sum(Positive_Months > 0) / sum(Positive_Months > -1000)) %>% percent(accuracy = 0.01)

# Show KPI ----
data.frame(Total_Returns         = Total_Returns,
           CAGR                  = data_annualized$portfolio.returns[1] %>% percent(accuracy = 0.01),
           Annualized_Volatility = data_annualized$portfolio.returns[2] %>% percent(accuracy = 0.01),
           Annualized_Sharpe     = data_annualized$portfolio.returns[3],
           Max_DD                = Max_DD %>% percent(accuracy = 0.01),
           Max_Return_Monthly    = Max_Return_Monthly,
           Min_Return_Monthly    = Min_Return_Monthly,
           Positive_Months       = Positive_Months,
           MTD                   = Total_MTD,
           QTD                   = Total_QTD,
           YTD                   = Total_YTD) %>%
  gather() %>%
  dplyr::mutate(key = c("Total Returns", "CAGR", "Annualized Volatility", "Annualized Sharpe Ratio", "Max Drawdown", "Best Monthly Return", "Worst monthly return", "% Positive Months", "Month-to-Date", "Quarter-to-Date", "Year-to-Date")) %>%
  purrr::set_names(c("Metric", "Value")) %>%
  kable(escape = F, align = c("l", "c")) %>%
  kable_styling("striped", full_width = F)

# Curent AA ----
if(Risk_Parity_Flag == TRUE){
  AA_Risk_Parity %>%
    tail(n = 1) %>%
    t() %>%
    as.data.frame() %>%
    purrr::set_names("Asset Allocation") %>%
    dplyr::mutate(`Asset Allocation` = percent(`Asset Allocation`, accuracy = 0.01)) %>%
    kable(escape = F, align = c("c", "c")) %>%
    kable_styling("striped", full_width = F) %>%
    add_footnote(str_glue("Asset Allocation for {AA_Risk_Parity %>%
                                                    tail(n = 1) %>%
                                                    as.data.frame() %>%
                                                    rownames_to_column(var = 'date') %>%
                                                    dplyr::select(date) %>%
                                                    pull(1)}"))
}else{
  data.frame(Tickers = symbols,
             Weights = AA_Risk_Parity) %>%
    column_to_rownames(var = "Tickers") %>%
    purrr::set_names("Asset Allocation") %>%
    dplyr::mutate(`Asset Allocation` = percent(`Asset Allocation`, accuracy = 0.01)) %>%
    kable(escape = F, align = c("c", "c")) %>%
    kable_styling("striped", full_width = F) %>%
    add_footnote(str_glue("Asset Allocation for {AA_Risk_Parity %>%
                                                    tail(n = 1) %>%
                                                    as.data.frame() %>%
                                                    rownames_to_column(var = 'date') %>%
                                                    dplyr::select(date) %>%
                                                    pull(1)}"))    
}


