
setwd("/Users/IIMS Bloomberg ONE/Downloads/Kaggle_Do_NOT_Delete/FPM003/Hackathon/EA/FaceData/")
df.bjp <- read.csv("bjp.csv")

text <- as.character(df.bjp$message)
library(dplyr)
text_df <- tibble(line = 1:39986, text = text)

text_df

library(tidytext)

tidy_books <- text_df %>%
  unnest_tokens(word, text)

######## n- gram #### 


austen_bigrams <- text_df %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

bigrams_separated <- austen_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts


library(igraph)

# original counts
#bigram_counts

# filter for only relatively common combinations
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

library(ggraph)
set.seed(1024)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


###### End


data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

tidy_books %>%
  count(word, sort = TRUE) 

require(ggplot2)

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

df.cong <- read.csv("cong.csv")

text.cong <- as.character(df.cong$message)
library(dplyr)
text_df.cong <- tibble(line = 1:31759, text = text.cong)

tidy_hgwells <-  text_df.cong%>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

require(tidyr)
require(stringr)
frequency <- bind_rows(mutate(tidy_hgwells, author = "Congress"), 
                       mutate(tidy_books, author = "BJP")) %>% 
  mutate(word = str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n / sum(n)) %>% 
  select(-n) %>% 
  spread(author, proportion) %>% 
  gather(author, proportion, `Congress`)

library(scales)

# expect a warning about rows with missing values being removed
ggplot(frequency, aes(x = proportion, y = `BJP`, color = abs(`BJP` - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "BJP", x = NULL)
