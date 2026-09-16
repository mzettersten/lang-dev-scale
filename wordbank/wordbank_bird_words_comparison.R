library(tidyverse)
library(cowplot)
library(here)
library(ggthemes)

#read in data
#data downloaded from wordbank.stanford.edu
english_items <- read_csv(here("data","wordbank_by_word_english_american_ws.csv"))
german_items <- read_csv(here("data","wordbank_by_word_german_ws.csv"))
dutch_items <- read_csv(here("data","wordbank_by_word_dutch_ws.csv"))
japanese_items <- read_csv(here("data","wordbank_by_word_japanese_ws.csv"))
mandarin_items <- read_csv(here("data","wordbank_by_word_mandarin_beijing_ws.csv"))
korean_items <- read_csv(here("data","wordbank_by_word_korean_ws.csv"))
turkish_items <- read_csv(here("data","wordbank_by_word_turkish_ws.csv"))
norwegian_items <- read_csv(here("data","wordbank_by_word_norwegian_ws.csv"))
french_items <- read_csv(here("data","wordbank_by_word_french_french_ws.csv"))
russian_items <- read_csv(here("data","wordbank_by_word_russian_ws.csv"))



english_bird_items <- c(
  "bird",
  "chicken (animal)",
  "cockadoodledoo",
  "duck",
  "egg",
  "goose",
  "hen",
  "owl",
  "penguin",
  "quack quack",
  "rooster",
  "turkey"
)

english_items %>%
  filter(item_definition %in% english_bird_items) %>%
  select(item_definition,uni_lemma) %>%
  distinct()

#get german items based on uni lemmas of english items
german_bird_items <- german_items %>%
  filter(uni_lemma %in% english_bird_items) %>%
  select(item_definition,uni_lemma) %>%
  distinct()

#items that are in English and German
intersection_bird_items <- german_bird_items %>%
  #select and extract uni lemma items
  pull(uni_lemma)

#create combined data
bird_crosslanguage_items_ws <- english_items %>%
  mutate(language = "English") %>%
  bind_rows(
    german_items %>%
      mutate(language = "German")
  ) %>%
  #add other languages
  bind_rows(
    norwegian_items %>%
      mutate(language = "Norwegian")
  ) %>%
  # bind_rows(
  #   japanese_items %>%
  #     mutate(language = "Japanese")
  # ) %>%
  #French
  # bind_rows(
  #   french_items %>%
  #     mutate(language = "French")
  # ) %>%
  #Mandarin
  bind_rows(
    mandarin_items %>%
      mutate(language = "Mandarin")
  ) %>%
  #Korean
  bind_rows(
    korean_items %>%
      mutate(language = "Korean")
  ) %>%
  #Turkish
  bind_rows(
    turkish_items %>%
      mutate(language = "Turkish")
  )  %>%
  #Russian
  # bind_rows(
  #   russian_items %>%
  #     mutate(language = "Russian")
  # ) %>%
  filter(uni_lemma %in% english_bird_items) 
#plot
ggplot(filter(bird_crosslanguage_items_ws,age<=30&age>=18),aes(x = age, y = prop_produces, color = uni_lemma)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~language) +
  labs(x = "Age (months)", y = "Proportion of children producing word") +
  theme_bw()+
  scale_x_continuous(breaks = seq(18, 30, by = 2)) +
  scale_color_brewer(name="English Lemma",palette="Paired")+
  #place legend on bottom
  #make legend wider
  theme(legend.position = "bottom", legend.box = "horizontal")+
  guides(color = guide_legend(nrow = 2)) +
  #make facet names larger
  theme(strip.text = element_text(size = 16))+
  #make axis label and text larger
  theme(axis.title = element_text(size = 16), axis.text = element_text(size = 12))+
  #make legend text larger
  theme(legend.text = element_text(size = 14),legend.title = element_text(size=16))

#save
ggsave(here("figures","bird_crosslanguage_items_ws.png"), width = 10, height = 6)
