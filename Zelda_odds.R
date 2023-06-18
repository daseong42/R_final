library(dplyr)
library(tidyr)

setwd("C:/Git/R_final")

#인벤, 게임메카 글 단어 빈도를 와이드 폼으로 변환
freq_wide <- freq %>% pivot_wider(names_from = site, values_from = n, values_fill = list(n=0))
freq_wide

#단어 비중 변수 추가 및 빈도0 없애기
freq_wide <- freq_wide %>% mutate(ratio_inven = ((inven+1)/(sum(inven+1))), ratio_gamemeca = ((gamemeca+1)/(sum(gamemeca+1))))
freq_wide

#오즈비 변수 추가
freq_wide <- freq_wide %>% mutate(odds_ratio = ratio_inven/ratio_gamemeca)
freq_wide

#오즈비 분석
freq_wide %>% arrange(-odds_ratio) #inven 높은 순
freq_wide %>% arrange(odds_ratio) #gamemeca 높은 순
freq_wide %>% arrange(abs(1-odds_ratio)) #비중이 같을 수록 1에 가까움
