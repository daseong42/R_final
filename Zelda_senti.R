setwd("C:/Git/R_final")

library(dplyr)

raw_inven <- readLines("Zelda_inven.txt", encoding = "UTF-8")
inven <- raw_inven %>% as_tibble() %>% mutate(site = "inven")
raw_gamemeca <- readLines("Zelda_gamemeca.txt", encoding = "UTF-8")
gamemeca <- raw_gamemeca %>% as_tibble() %>% mutate(site = "gamemeca")

bind_review <- bind_rows(inven, gamemeca) %>% select(site, value)

library(stringr)
review <- bind_review %>% mutate(value = str_replace_all(value, "[^가-힣]", " "), value = str_squish(value))

library(tidytext)
library(KoNLP)
review <- review %>% unnest_tokens(input = value, output = word, token = extractNoun)
review

freq <- review %>% count(site, word) %>% filter(str_count(word) > 1)
freq

#리뷰에 포함되지 않는 주석, 쓸데없는 문자 임의로 제외
words_to_exclude <- c("게임", "메카", "사진", "플레이", "조나", "하게", "정도", "버레", "하지", "하이")

top10 <- freq %>% group_by(site) %>% filter(!(word %in% words_to_exclude)) %>% slice_max(n, n=10)
top10

#단어 빈도
library(ggplot2)
ggplot(top10, aes(x=reorder_within(word, n, site), y=n, fill=site)) + geom_col() + coord_flip() + facet_wrap(~site, scales = "free_y") + scale_x_reordered()

#감정 분석
library(readr)
dic <- read_csv("knu_sentiment_lexicon.csv")
word_comment <- bind_review %>% unnest_tokens(input = value, output = word, token = "words", drop = F)
word_comment <- word_comment %>% left_join(dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity), 0, polarity))
word_comment <- word_comment %>% mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))
word_comment %>% count(sentiment)
