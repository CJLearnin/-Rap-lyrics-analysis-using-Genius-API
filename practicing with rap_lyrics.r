library(geniusr)
library(genius)
library(dplyr)
library(tidytext) # For sentimential analysis
library(purrr)
library(ggplot2)


genius_token <- function(force = FALSE) {
  
  env <- Sys.getenv('YOUR API KEY')
  if (!identical(env, "") && !force) return(env)
  
  if (!interactive()) {
    stop("Please set env var GENIUS_API_TOKEN to your Genius API key",
         call. = FALSE)
  }
  
  message("Couldn't find env var GENIUS_API_TOKEN See ?genius_token for more details.")
  message("Please enter your Genius Client Access Token and press enter:")
  pat <- readline(": ")
  
  if (identical(pat, "")) {
    stop("Genius API key entry failed", call. = FALSE)
  }
  
  message("Updating GENIUS_API_TOKEN env var to PAT")
  Sys.setenv(GENIUS_API_TOKEN = pat)
  
  pat
  
}
GENIUS_API_TOKEN=genius_token()
# set lexicon
bing <- get_sentiments("bing")

jcole <- genius_album(artist = "J.Cole", album = "KOD") # Grabs all the lyrics from the KOD album

View(jcole)

sentiment <- jcole %>%
  unnest_tokens(word, lyric) %>%
  # remove stop words
  anti_join(stop_words) %>%
  # join afinn score
  inner_join(bing) %>%
  # count negative / positive words
  count(word, sentiment, sort = TRUE) %>%
  ungroup()





























# BELOW IS THE CORRECT WAY TO DO THIS
# NOW I NEED TO FIGURE OUT WHAT TO DO WITH THIS INFORMATION



cole= search_song(search_term = "J.Cole")

tracklist <- scrape_tracklist(album_id = 491200) # FIND ALBUM ID USIING GENIUS API DOC


lyrics <- map_df(tracklist$song_lyrics_url, scrape_lyrics_url)
View(lyrics)

sentiment <- lyrics %>%
  unnest_tokens(word, line) %>%
  # remove stop words
  anti_join(stop_words) %>%
  # join afinn score
  inner_join(bing) %>%
  # count negative / positive words
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

sentiment %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Coloring Book: Words that contribute the most to positive and negative sentiment",
       x = NULL) +
  coord_flip() +
  theme_minimal()

most_words_gkmc = lyrics %>% # FIND THE MOST USED WORDS IN THE ALBUM
  unnest_tokens(word, line) %>%
  count(song_name,word,sort=TRUE)

total_words = most_words_gkmc %>% # SUMS THOSE WORDS TO SEE THE MOST USED WORDS FOR EACH SONG
  group_by(song_name) %>%
  summarize(total=sum(n)) # HOW MANY WORDS ARE IN THE song

most_words_gkmc = left_join(most_words_gkmc, total_words)

library(wordcloud)

most_words_gkmc %>%
  anti_join(stop_words)%>%
  count(word)%>%
  with(wordcloud(word,n,max.words=100,scale=c(4,.5),min.freq=1,
          random.order=FALSE, random.color=FALSE, rot.per=.1,
          colors="black",ordered.colors=TRUE,use.r.layout=FALSE,
          fixed.asp=TRUE))



write.csv(lyrics, file="lyrics.csv", row.names = FALSE)







