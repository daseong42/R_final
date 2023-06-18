setwd("C:/Git/R_final")

#리뷰를 불러오기
inven <- readLines("Zelda_inven.txt", encoding = "UTF-8")

#형태소 분석
library(KoNLP)
useNIADic()
library(stringr)
inven_new <- inven %>% str_replace_all("[^가-힣]", " ") %>% str_squish() %>% as_tibble()
inven_new
#명사 기준 토큰화
inven_word_noun <- inven_new %>% unnest_tokens(input = value, output = word, token = extractNoun)
inven_word_noun
#한글자 제외하고 빈도 계산
inven_word_noun <- inven_word_noun %>% count(word, sort = T) %>% filter(str_count(word) > 1)
inven_word_noun
top20 <- inven_word_noun %>% head(20)
top20
#워드 클라우드
library(ggwordcloud)
ggplot(inven_word_noun, aes(label = word, size=n)) + geom_text_wordcloud(seed=1234) + scale_radius(limits = c(3,NA), range = c(3,30))

#감정
library(dplyr)
library(readr)
dic <- read_csv("knu_sentiment_lexicon.csv")
word_comment <- inven_new %>% unnest_tokens(input = value, output = word, token = "words", drop = F)
word_comment <- word_comment %>% left_join(dic, by = "word") %>% mutate(polarity = ifelse(is.na(polarity), 0, polarity))
word_comment %>% select(word, polarity)
word_comment <- word_comment %>% mutate(sentiment = ifelse(polarity == 2, "pos", ifelse(polarity == -2, "neg", "neu")))
word_comment %>% count(sentiment)
