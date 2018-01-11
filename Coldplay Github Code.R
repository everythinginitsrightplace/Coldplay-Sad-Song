### Coldplay's saddest song

# Firstly, Let's download all necessary packages.
library(tidyverse)
library(httr)
library(stringr)
library(curl)
library(devtools)
library(rvest)
library(tidytext)
library(lubridate)
library(spotifyr)
library(scales)
library(RColorBrewer)
library(highcharter)


# Then you need to create client_id and client_secret from https://developer.spotify.com/web-api/. 


Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXXXXXXXXXXXXXXX')

# The next step is to get a lot of track's features
spotify_df <- get_artist_audio_features('Coldplay')


# Filter out remixes and EPs
non_studio_albums <- c('Ghost Stories Live 2014', 'A Head Full Of Dreams Tour Edition', 'Viva La Vida - Prospekts March Edition', 'A Head Full Of Dreams Tour Edition')
spotify_df <- filter(spotify_df, !album_name %in% non_studio_albums)


# It's time to deal with Genius.com ! Please, create your own token from here https://docs.genius.com/. 

token <- 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'

genius_get_artists <- function(artist_name, n_results = 10) {
  baseURL <- 'https://api.genius.com/search?q=' 
  requestURL <- paste0(baseURL, gsub(' ', '%20', artist_name),
                       '&per_page=', n_results,
                       '&access_token=', token)
  
  res <- GET(requestURL) %>% content %>% .$response %>% .$hits
  
  map_df(1:length(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    list(
      artist_id = tmp$id,
      artist_name = tmp$name
    )
  }) %>% unique
}

##### We get Coldplay from Genius.com
genius_artists <- genius_get_artists('Coldplay')
genius_artists
genius_artists <- genius_artists %>% 
  filter(artist_name == 'Coldplay')




#Next, We looped through the contents of the songs endpoint 

baseURL <- 'https://api.genius.com/artists/' 
requestURL <- paste0(baseURL, genius_artists$artist_id[1], '/songs')

## while loop
track_lyric_urls <- list()
i <- 1
while (i > 0) {
  tmp <- GET(requestURL, query = list(access_token = token, per_page = 50, page = i)) %>% content %>% .$response
  track_lyric_urls <- c(track_lyric_urls, tmp$songs)
  if (!is.null(tmp$next_page)) {
    i <- tmp$next_page
  } else {
    break
  }
}

length(track_lyric_urls)


summary(track_lyric_urls[[1]])

# Please, be careful with "lyrics" node.

lyric_scraper <- function(url) {
  read_html(url) %>% 
    html_node('div.lyrics') %>% 
    html_text
}


genius_df <- map_df(1:length(track_lyric_urls), function(x) {
  # add in error handling
  lyrics <- try(lyric_scraper(track_lyric_urls[[x]]$url))
  if (class(lyrics) != 'try-error') {
    # strip out non-lyric text and extra spaces
    lyrics <- str_replace_all(lyrics, '\\[(Verse [[:digit:]]|Pre-Chorus [[:digit:]]|Hook [[:digit:]]|Chorus|Outro|Verse|Refrain|Hook|Bridge|Intro|Instrumental)\\]|[[:digit:]]|[\\.!?\\(\\)\\[\\],]', '')
    lyrics <- str_replace_all(lyrics, '\\n', ' ')
    lyrics <- str_replace_all(lyrics, '([A-Z])', ' \\1')
    lyrics <- str_replace_all(lyrics, ' {2,}', ' ')
    lyrics <- tolower(str_trim(lyrics))
  } else {
    lyrics <- NA
  }
  
  tots <- list(
    track_name = track_lyric_urls[[x]]$title,
    lyrics = lyrics
  )
  
  return(tots)
})


str(genius_df)

###Track names in Spotify and Genius are different. But it's not a problem to match them for each other.

genius_df$track_name[genius_df$track_name == "Everything's Not Lost"] <- "Everything's Not Lost - Includes Hidden Track 'Life Is For Living'"
genius_df$track_name[genius_df$track_name == "X&Y [Album Art + Tracklist]"] <- "X & Y"
genius_df$track_name[genius_df$track_name == "Lovers in Japan / Reign of Love"] <- "Lovers In Japan"
genius_df$track_name[genius_df$track_name == "Fun"] <- "Fun (feat. Tove Lo)"



genius_df <- genius_df %>% 
  mutate(track_name_join = tolower(str_replace(track_name, '[[:punct:]]', ''))) %>% 
  filter(!duplicated(track_name_join)) %>% 
  select(-track_name)

track_df <- spotify_df %>%
  mutate(track_name_join = tolower(str_replace(track_name, '[[:punct:]]', ''))) %>%
  left_join(genius_df, by = 'track_name_join') %>%
  select(track_name, valence, duration_ms, lyrics, album_name, album_release_year, album_img)

str(track_df)


# Here we define the lowest valence song. It's name - "Colour Spectrum"

track_df %>% 
  select(valence, track_name) %>%
  arrange(valence) %>% 
  slice(1:10)



#### Let's check lyrics!!!!

sad_words <- sentiments %>% 
  filter(lexicon == 'nrc', sentiment == 'sadness') %>% 
  select(word) %>% 
  mutate(sad = T)


#### Stopwords are out
sent_df <- track_df %>% 
  unnest_tokens(word, lyrics) %>%
  anti_join(stop_words, by = 'word') %>%
  left_join(sad_words, by = 'word') %>%
  group_by(track_name) %>% 
  summarise(pct_sad = round(sum(sad, na.rm = T) / n(), 4),
            word_count = n()) %>% 
  ungroup


#### Rating of saddest songs by lyrics
sent_df %>% 
  select(pct_sad, track_name) %>%
  arrange(-pct_sad) %>% 
  head(10)





#### Gloom Index is the main task in this script
track_df <- track_df %>% 
  left_join(sent_df, by = 'track_name') %>% 
  mutate_at(c('pct_sad', 'word_count'), funs(ifelse(is.na(.), 0, .))) %>% 
  mutate(lyrical_density = word_count / duration_ms * 1000,
         gloom_index = round(rescale(1 - ((1 - valence) + (pct_sad * (1 + lyrical_density))) / 2, to = c(1, 100)), 2)) 

track_df %>%
  select(gloom_index, track_name) %>%
  arrange(gloom_index) %>%
  head(10)




plot_df <- track_df %>% 
  rowwise %>% 
  mutate(tooltip = paste0('<a style = "margin-right:', max(max(nchar(track_name), nchar(album_name)) * 7, 55), 'px">', # dynamic sizing
                          '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                          '<b>Album:</b> ', album_name,
                          '<br><b>Track:</b> ', track_name)) %>% 
  ungroup

avg_line <- plot_df %>% 
  group_by(album_release_year, album_name, album_img) %>% 
  summarise(avg = mean(gloom_index)) %>% 
  ungroup %>% 
  transmute(x = as.numeric(as.factor(album_release_year)), 
            y = avg,
            tooltip = paste0('<a style = "margin-right:55px">',
                             '<img src=', album_img, ' height="50" style="float:left;margin-right:5px">',
                             '<b>Album:</b> ', album_name,
                             '<br><b>Average Gloom Index:</b> ', round(avg, 2),
                             '</a>'))
plot_track_df <- plot_df %>% 
  mutate(tooltip = paste0(tooltip, '<br><b>Gloom Index:</b> ', gloom_index, '</a>'),
         album_number = as.numeric(as.factor(album_release_year))) %>% 
  ungroup

album_chart <- hchart(plot_track_df, 'scatter', hcaes(x = as.numeric(as.factor(album_release_year)), y = gloom_index, group = album_name)) %>% 
  hc_add_series(data = avg_line, type = 'line') %>%
  hc_tooltip(formatter = JS(paste0("function() {return this.point.tooltip;}")), useHTML = T) %>% 
  hc_colors(c(sample(brewer.pal(n_distinct(track_df$album_name), 'Paired')), 'black')) %>% 
  hc_xAxis(title = list(text = 'Album'), labels = list(enabled = F)) %>% 
  hc_yAxis(max = 100, title = list(text = 'Индекс уныния (или мрака)')) %>% 
  hc_title(text = 'Анализ текстов и музыки группы Coldplay на основе Spotify и Genius') %>% 
  hc_subtitle(text = 'Грусть в альбомах группы') %>% 
  hc_add_theme(hc_theme_smpl())
album_chart$x$hc_opts$series[[8]]$name <-  'Средний показатель по альбомам' 
album_chart
