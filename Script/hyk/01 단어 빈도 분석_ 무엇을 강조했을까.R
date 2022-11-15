library(tidytext)
library(tidyverse)

### Q1 speech_park.txt 를 불러와 문서에 적합하게 전처리한 다음 띄어쓰기 기준으로 토큰화하세요.

raw_park = readLines("Data/speech_park.txt", encoding = "UTF-8")

raw_park

park = raw_park %>% 
  str_replace_all("[^가-힣]", " ") %>% 
  str_squish()  %>% 
  as_tibble()

word_space = park %>% 
  unnest_tokens(input = value,
                output = word,
                token = "words")

### Q2 가장 자주 사용된 단어 20개를 추출하세요.

word_space = word_space %>%
  count(word, sort = TRUE) %>% 
  filter(str_count(word) > 1)

top20 = word_space %>% 
  head(20)

### Q3 가장 자주 사용된 단어 20개의 빈도를 나타낸 막대 그래프를 만드세요. 그래프의 폰트는 나눔고딕으로 설정하세요.

theme_set(theme_gray(base_family = "AppleGothic"))
top20 %>% 
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col() +
  labs(title = "박근혜 대통령 출마 연설문 단어 빈도", x = NULL, y = NULL) +
  geom_text(aes(label = n), hjust = -0.3) +
  coord_flip()
