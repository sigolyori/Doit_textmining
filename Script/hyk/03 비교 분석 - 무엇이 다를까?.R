library(tidyverse)

# 03-1 --------------------------------------------------------------------

library(dplyr)

# 문재인 대통령 연설문 불러오기
raw_moon <- readLines("Data/speech_moon.txt", encoding = "UTF-8")
moon <- raw_moon %>%
  as_tibble() %>%
  mutate(president = "moon")

# 박근혜 대통령 연설문 불러오기
raw_park <- readLines("Data/speech_park.txt", encoding = "UTF-8")
park <- raw_park %>%
  as_tibble() %>%
  mutate(president = "park")


# -------------------------------------------------------------------------
bind_speeches <- bind_rows(moon, park) %>%
  select(president, value)

head(bind_speeches)
tail(bind_speeches)


# -------------------------------------------------------------------------
# 기본적인 전처리
library(stringr)
speeches <- bind_speeches %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))

speeches


# 토큰화
library(tidytext)
library(KoNLP)

speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)
speeches


# -------------------------------------------------------------------------
df <- tibble(class = c("a", "a", "a", "b", "b", "b"),
             sex = c("female", "male", "female", "male", "male", "female"))
df

df %>% count(class, sex)


# -------------------------------------------------------------------------
frequency <- speeches %>%
  count(president, word) %>%   # 연설문 및 단어별 빈도
  filter(str_count(word) > 1)  # 두 글자 이상 추출

head(frequency)


# -------------------------------------------------------------------------
df <- tibble(x = c(1:100))
df

df %>% slice_max(x, n = 3)


# -------------------------------------------------------------------------
top10 <- frequency %>%
  group_by(president) %>%  # president별로 분리
  slice_max(n, n = 10)     # 상위 10개 추출

top10


# -------------------------------------------------------------------------
top10 %>%
  filter(president == "park") %>%
  print(n = Inf)


# -------------------------------------------------------------------------
df <- tibble(x = c("A", "B", "C", "D"), y = c(4, 3, 2, 2))

df %>% 
  slice_max(y, n = 3)

df %>% 
  slice_max(y, n = 3, with_ties = F)


# -------------------------------------------------------------------------
top10 <- frequency %>%
  group_by(president) %>%
  slice_max(n, n = 10, with_ties = F)

top10


# -------------------------------------------------------------------------
library(ggplot2)
theme_set(theme_gray(base_family = "AppleGothic"))
ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president)


# -------------------------------------------------------------------------
ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president,         # president별 그래프 생성
             scales = "free_y")  # y축 통일하지 않음


# -------------------------------------------------------------------------
top10 <- frequency %>%
  filter(word != "국민") %>%
  group_by(president) %>%
  slice_max(n, n = 10, with_ties = F)

top10


# -------------------------------------------------------------------------
ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y")


# -------------------------------------------------------------------------
ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y")


# -------------------------------------------------------------------------
ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free_y") +
  scale_x_reordered() +
  labs(x = NULL) +                                    # x축 삭제
  theme(text = element_text(family = "nanumgothic"))  # 폰트

df_long = frequency %>% 
  group_by(president) %>% 
  slice_max(n, n = 10) %>% 
  filter(word %in% c("국민", "우리", "정치", "행복"))

library(tidyr)
df_wide = df_long %>% 
  pivot_wider(names_from = president, values_from = n,
              values_fill = list(n = 0))

frequency_wide = frequency %>% 
  pivot_wider(names_from = president, values_from = n,
              values_fill = list(n = 0))

frequency_wide = frequency_wide %>% 
  mutate(ratio_moon = ((moon + 1) / (sum(moon + 1))),
         ratio_park = ((park + 1) / (sum(park + 1))),
         odds_ratio = ratio_moon / ratio_park
         ) %>% 
  arrange(-odds_ratio)

top10 = frequency_wide %>% 
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10)

top10 %>% 
  arrange(-odds_ratio) %>% 
  print(n = Inf)

top10 = top10 %>% 
  mutate(president = ifelse(odds_ratio > 1, "moon", "park"),
         n = ifelse(odds_ratio > 1, moon, park))
top10         

# 막대 그래프 만들기 
ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~president, scales = "free_y") +
  scale_x_reordered() +
  labs(x = NULL)

# 주요 단어가 사용된 문장 살펴보기
speeches_sentence = bind_speeches %>% 
  as_tibble() %>% 
  unnest_tokens(input = value,
                output = sentence,
                token = "sentences")

speeches_sentence %>% 
  filter(president == "moon" & str_detect(sentence, "복지국가"))

speeches_sentence %>% 
  filter(president == "park" & str_detect(sentence, "행복"))

frequency_wide %>% 
  arrange(abs(1 - odds_ratio)) %>% 
  head(10)

frequency_wide %>% 
  filter(moon >= 5 & park >= 5) %>% 
  arrange(abs(1 - odds_ratio)) %>% 
  head(10)

frequency_wide = frequency_wide %>% 
  mutate(log_odds_ratio = log(odds_ratio))

frequency_wide %>% 
  arrange(-log_odds_ratio)
frequency_wide %>% 
  arrange(log_odds_ratio)
frequency_wide %>% 
  arrange(abs(log_odds_ratio))

top10 = frequency_wide %>% 
  group_by(president = ifelse(log_odds_ratio > 0, "moon", "park")) %>% 
  slice_max(abs(log_odds_ratio), n = 10, with_ties = FALSE)

ggplot(top10, aes(x = reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL)

# TF-IDF

raw_speeches = read_csv("Data/speeches_presidents.csv")
raw_speeches

speeches = raw_speeches %>% 
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value)) 

speeches = speeches %>% 
  unnest_tokens(input = value,
                output = word, 
                token = extractNoun)

frequency = speeches %>% 
  count(president, word) %>% 
  filter(str_count(word) > 1)

frequency = frequency %>% 
  bind_tf_idf(term = word,
              document = president,
              n = n)

frequency %>% 
  filter(president == "문재인") %>% 
  arrange(-tf_idf)

top10 = frequency %>% 
  group_by(president) %>% 
  slice_max(tf_idf, n = 10, with_ties = FALSE)

top10 = top10 %>% 
  mutate(president = factor(president,
                               levels = c("문재인", "박근혜", "이명박", "노무현")))

ggplot(top10, aes(x = reorder_within(word, tf_idf, president),
                  y = tf_idf,
                  fill = president)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~president, scales = "free", ncol = 2) +
  scale_x_reordered() +
  labs(x = NULL)
