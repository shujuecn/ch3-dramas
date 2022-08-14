#!/usr/bin/Rscript

library(tidyverse)
library(RCurl)
library(XML)
library(stringr)

####################################################

# 伪装报头
myheader <- c(
  "user-agent" = "Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.5060.134 Mobile Safari/537.36 Edg/103.0.1264.71", # nolint
  "accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9", # nolint
  "accept-encoding" = "gzip, deflate, br",
  "accept-language" = "zh-CN,zh;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6"
)

# URL
my_url = "https://polls.polldaddy.com/vote-js.php?p=11173662"

####################################################

ch3d <- \() {
  # 读取网页
  webpage <- getURL(
    url = my_url,
    header = myheader,
    .encoding = "UTF-8"
  )

  # 整理树形结构
  pagetree <- htmlTreeParse(
    webpage,
    encoding = "UTF-8",
    error = function(...) {},
    useInternalNodes = TRUE,
    trim = TRUE
  )

  # 提取剧名
  dramas_name <- pagetree %>%
    getNodeSet("//span[@class='pds-answer-text']/text()") %>%
    sapply(xmlValue) %>%
    str_sub(., 2, nchar(.) - 1)

  # 戏剧票数占比
  dramas_votes_per <- pagetree %>%
    getNodeSet("//span[@class='pds-feedback-per']/text()") %>%
    sapply(xmlValue) %>%
    str_sub(., 2, nchar(.) - 1) %>%
    as.numeric()
  dramas_votes_per <- dramas_votes_per / 100

  # 提取戏剧票数
  dramas_votes <- pagetree %>%
    getNodeSet("//span[@class='pds-feedback-votes']/text()") %>%
    sapply(xmlValue) %>%
    str_sub(., 4, nchar(.) - 7) %>%
    gsub(",", "", .) %>%
    as.numeric()

  output <- tibble(
    rank = 1:length(dramas_name),
    dramas = dramas_name,
    votes = dramas_votes,
    per = dramas_votes_per
  )

  return(output)
}

df = ch3d()

# 时间
df$date <- format(Sys.time(), "%F")
df$time <- format(Sys.time(), "%T") %>%
  str_sub(1, 5)

# 将per_group的小数点保留四位
df$per <- round(df$per, 4)

print(df, n = 20)

####################################################

rio::export(
  df,
  file = paste0(
    "output/best-ch3-dramas ",
    gsub(":", ".", Sys.time()),
    ".csv"
  )
)
