library(httr)
library(rvest)
library(purrr)
library(stringr)
library(magrittr)
library(dplyr)
library(knitr)


# LOAD DATA
data_t <- read.csv("data.csv")
data_t$X = NULL
data_t$date %<>% as.Date
data_t <- head(data_t, 10)

# LOAD TEST
t_items = read.csv("test.csv")
t_items$X = NULL
t_items$date %<>% as.Date
item <- t_items %>% filter(., .$fuel == "DIESEL")


data_t %>% filter(., .$date == item$date, .$fuel == item$fuel)

d <- item$date
f <- item$fuel
data_t %>% filter(., .$date == d,          .$fuel == f)
data_t %>% filter(., .$date == item$date, .$fuel == item$fuel)

#### !!!!!! bug in r, impossible to spot it 
#### filter(.,.$date==date,.$fuel==fuel) -> 'date' is evaluated to "2022-11-24" yet the filter still won't work
#### it returns unfiltered table
#### date is defined as date <- item$date; item$date is evaluated as "2022-11-24
#### the filter works with item$date but not with (date=item$date)
#### it has to be: .$date==date[1] to work

#### once more:
#### item$date <- "2022-11-24"
#### date = item$date
#### filter(.,.$date==date,.$fuel==fuel) -> won't work
#### filter(.,.$date==item$date,.$fuel==fuel) -> this works
#### filter(., .$date==date[1], .$fuel==fuel) -> this works
fn <- function(t) {

  function(item) {
    vendor_now <- item$vendor # warning, has to provide new var, table$vendor == vendor would not work in filter
    fuel_now <- item$fuel
    table_now <- filter(t, t$vendor==vendor_now, t$fuel==fuel_now)  
    data_now <- data.frame(item$vendor,
                          item$date,
                          week = format(date,"%y/w%w"),
                          day = weekdays(date),
                          item$fuel,
                          item$price,
                          week_rank = format(date-1,"%y%w"),
                          item$origin)

    if (date %in% table_now$date) {

      currentprices <- table_now %>% filter(.,.$date==item$date[1], .$fuel==item$fuel[1]) %>% .$price
      if (item$price %in% currentprices) {
        print(paste0("[warning] zaznam jiz existuje: "))
        null
      } else {
        # todo d-1 zmena....
        print(paste("[attention] zmena existujici ceny"))
        null
      }
    } else {
      print(paste(date, "nova cena", fuel_now, ":", price_now))
      data_now
    }
  }
}

f <- fn(data_t)
f(item)
