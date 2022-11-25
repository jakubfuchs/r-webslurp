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
t_items = read.csv("test/test.csv")
t_items$X = NULL
t_items$date %<>% as.Date
item <- t_items %>% filter(., .$fuel == "DIESEL")


data_t %>% filter(., .$date == item$date, .$fuel == item$fuel)

# This works
d <- item$date
f <- item$fuel
data_t %>% filter(., .$date == d,          .$fuel == f)
data_t %>% filter(., .$date == item$date, .$fuel == item$fuel)

# This dos not work
date <- item$date
fuel <- item$fuel
data_t %>% filter(., .$date == date,          .$fuel == fuel)

# Yet this work again
data_t %>% filter(., .$date == date[1],          .$fuel == fuel[1])


check_fn <- function(dt) {

  function(item) {
    table_now <- filter(dt, dt$vendor == item$vendor, dt$fuel == item$fuel)  
    data_now <- data.frame(item$vendor,
                          item$date,
                          week = format(item$date,"%Y/w%W"),
                          day = weekdays(item$date),
                          item$fuel,
                          item$price,
                          week_rank = format(item$date-1,"%Y%W"),
                          item$origin)

    if (item$date %in% table_now$date) {

      currentprices <- table_now %>% filter(.,.$date == item$date[1], .$fuel == item$fuel[1]) %>% .$price
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


f <- check_fn(data_t)
f(item)                   # => Zaznam jiz existuje

item$price = round(item$price)-1
f(item)                   # => Zmena existujici ceny
