# rm(list = ls())
# setwd('~/Programming/Python/Learning/MIPT/Homeworks/Project_Otzyvy')

library(XML)
library(xml2)
library(rvest)
library(data.table)
library(purrr)

# Парсим список отзывов
pg <- read_html("http://www.banki.ru/services/responses/list/?is_countable=on&page=1")

html_nodes(pg, ".responses__item__message a") %>% html_attrs() %>% map_df(., as.data.frame)

# парсим полный отзыв на страничке
pg <- read_html("http://www.banki.ru/services/responses/bank/response/10031749/")

# название отзыва
html_nodes(pg, ".response-page__title") %>% html_text()

# текст отзыва - нужно чистить до букв - удалять табуляции и переносы строк
html_nodes(pg, ".markup-inside--bullet") %>% html_text()

# оценка
html_nodes(pg, ".rating-grade") %>% html_text()

# банк
html_nodes(pg, ".margin-right-x-small") %>% html_text()

# дата
html_nodes(pg, "time") %>% html_text()

# # число просмотров - сложновато, забьем
# html_nodes(pg, "span") %>% html_text()

# try.error function
try.error = function(x)
{
  y = list(node = NA, doc = NA)
  try_error = tryCatch(read_html(x), error = function(e) e)
  if (!inherits(try_error, "error"))
    y = read_html(x)
  return(y)
}

data <- list()

# парсинг в минус
for (i in 10031749:(10031749-31750)) {
  url <- paste0('http://www.banki.ru/services/responses/bank/response/', i, '/')
  pg <- try.error(url)
  cat('\nNumber ', i, ' Status = ', !(is.na(pg)[[1]]))
  if (!(is.na(pg)[[1]])) {
    data[[i]] <- data.frame(title = html_nodes(pg, ".response-page__title") %>% html_text(),
                            text = html_nodes(pg, ".markup-inside--bullet") %>% html_text(),
                            rating = ifelse(length(html_text(html_nodes(pg, ".rating-grade"))) == 0, NA, html_text(html_nodes(pg, ".rating-grade"))),
                            bank = html_nodes(pg, ".margin-right-x-small") %>% html_text(),
                            date = html_nodes(pg, "time") %>% html_text())
  }
  
}

# Number  10026106  Status =  TRUE
# Show Traceback
# 
# Rerun with Debug
# Error in open.connection(x, "rb") : HTTP error 504. In addition: There were 50 or more warnings (use warnings() to see the first 50)
# 
# 
library(data.table)
data_all <- data.table::rbindlist(data)

# 3353 отзыва с оценками
save.image('Banks_parsed.RData')

######################################
# Выкачка hrefs
######################################
hrefs_list <- list()

for (i in 2894:4864) {
  cat('\nPage', i)
  url <- paste0('http://www.banki.ru/services/responses/list/?is_countable=on&page=', i)
  pg <- read_html(url)
  hrefs_list[[i]] <- html_nodes(pg, ".responses__item__message a") %>% html_attrs() %>% map_df(., as.data.frame)
}

hrefs_df <- rbindlist(hrefs_list)
save(hrefs_df, file = "hrefs_df.rda")

######################################
# Основная выкачка
######################################
load("hrefs_df.rda")

# try.error function
try.error = function(x)
{
  y = list(node = NA, doc = NA)
  try_error = tryCatch(read_html(x), error = function(e) e)
  if (!inherits(try_error, "error"))
    y = read_html(x)
  return(y)
}

data <- list()

# парсинг
for (i in hrefs_df[[1]]) {
  url <- paste0('http://www.banki.ru', i)
  pg <- try.error(url)
  cat('\nNumber ', i, ' Status = ', !(is.na(pg)[[1]]))
  if (!(is.na(pg)[[1]])) {
    data[[i]] <- data.frame(id = as.integer(regmatches(i, regexpr('([0-9]+)', i), invert = FALSE)),
                            title = html_nodes(pg, ".response-page__title") %>% html_text(),
                            text = html_nodes(pg, ".markup-inside--bullet") %>% html_text(),
                            rating = ifelse(length(html_text(html_nodes(pg, ".rating-grade"))) == 0, NA, html_text(html_nodes(pg, ".rating-grade"))),
                            bank = html_nodes(pg, ".margin-right-x-small") %>% html_text(),
                            date = html_nodes(pg, "time") %>% html_text())
  }
  
}

data_df_1 <- rbindlist(data)
save(data_df_1, file = "data_df_1.rda")

match('/services/responses/bank/response/7540543/', hrefs_df[[1]])
# [1] 40086
hrefs_df[match('/services/responses/bank/response/7540543/', hrefs_df[[1]]), ]
# 1: /services/responses/bank/response/7540543/
hrefs_df[40087:120554, ][[1]]

data <- list()

# парсинг
for (i in hrefs_df[40087:120554, ][[1]]) {
  url <- paste0('http://www.banki.ru', i)
  pg <- try.error(url)
  cat('\nNumber ', i, ' Status = ', !(is.na(pg)[[1]]))
  if (!(is.na(pg)[[1]])) {
    data[[i]] <- data.frame(id = as.integer(regmatches(i, regexpr('([0-9]+)', i), invert = FALSE)),
                            title = html_nodes(pg, ".response-page__title") %>% html_text(),
                            text = html_nodes(pg, ".markup-inside--bullet") %>% html_text(),
                            rating = ifelse(length(html_text(html_nodes(pg, ".rating-grade"))) == 0, NA, html_text(html_nodes(pg, ".rating-grade"))),
                            bank = html_nodes(pg, ".margin-right-x-small") %>% html_text(),
                            date = html_nodes(pg, "time") %>% html_text())
  }
  
}

data_df_2 <- rbindlist(data)
save(data_df_2, file = "data_df_2.rda")
