library(multilinguer)
library(KoNLP)
library(tidytext)
library(tidyverse)

theme_set(theme_gray(base_family = "AppleGothic"))

raw_park = readLines("Data/speech_park.txt", encoding = "UTF-8")

raw_park

# 1. speech_park.txt 를 불러와 분석에 적합하게 전처리, 명사 추출

sentence_park = raw_park %>% 
  str_squish() %>% 
  as_tibble() %>% 
  unnest_tokens(input = value,
                output = sentence,
                token = "sentences")

word_noun = sentence_park %>% 
  unnest_tokens(input = sentence,
                output = word,
                token = extractNoun)

# 2. 가장 자주 사용된 단어 20개 추출

word_noun %>% 
  count(word, sort = TRUE) %>% 
  head(20) %>% 
  ggplot() +
  geom_col(aes(x = reorder(word, n), y = n)) + 
  coord_flip()

# 4,5. 

raw_park %>% 
  str_squish() %>% 
  as_tibble() %>% 
  unnest_tokens(input = value,
                output = sentence,
                token = "sentences") %>% 
  filter(str_detect(sentence, "경제"))
  
