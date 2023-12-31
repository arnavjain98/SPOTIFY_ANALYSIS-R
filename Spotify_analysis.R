# List the files in the current working directory
list.files()


library(jsonlite)


# Read in the Spotify streaming history from a JSON file
streamHistory <- fromJSON("StreamingHistory.json", flatten = TRUE)


# Load necessary libraries
library(tidyverse)
library(lubridate)
library(jsonlite)
library(ggplot2)
# Read in streaming history data and convert to tibble format
streamHistory <- fromJSON("StreamingHistory.json", flatten = TRUE)
mySpotify <- streamHistory %>% 
  as_tibble()

# Convert date-time column to proper format and adjust for timezone
mySpotify <- mySpotify %>% 
  mutate(endTime = ymd_hm(endTime) - hours(6)) 

# Add columns for date, seconds, and minutes
mySpotify <- mySpotify %>% 
  mutate(date = as_date(floor_date(endTime, "day")),
         seconds = msPlayed / 1000, 
         minutes = seconds / 60)

# Calculate total hours of playback per week
streamingHours <- mySpotify %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(date = floor_date(date, "week")) %>% 
  summarize(hours = sum(minutes) / 60)

# Create a bar plot of streaming hours per week
ggplot(streamingHours, aes(x = date, y = hours, fill = hours)) +
  geom_col() +
  scale_fill_gradient(low = "green", high = "blue") +
  labs(x = "Date", y = "Hours of playback",
       title = "Playback activity per week",
       subtitle = "Shows how many hours of music were played each week")





# Create a new dataframe with data for "Danna Paola" and "Bersuit Vergarabat"
artistsData <- mySpotify %>% 
  filter(artistName %in% c("Danna Paola", "Bersuit Vergarabat")) %>% 
  group_by(artistName, date = floor_date(date, "month")) %>% 
  summarize(hours = sum(minutes) / 60)

# Plot the data
ggplot(artistsData, aes(x = date, y = hours, group = artistName)) +
  geom_line() +
  labs(x = "Date", y = "Hours of music playback") +
  ggtitle("Playback activity for specific artists") +
  facet_wrap(~ artistName, ncol = 2, scales = "free_y") +
  theme_bw()



# Activity by Data and time of the Day
mySpotify <- mySpotify %>% 
  arrange(desc(msPlayed))

timeDay <- mySpotify %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(date, hour = hour(endTime)) %>% 
  summarise(minutesListened = sum(minutes)) %>% 
  ggplot(aes(x = hour, y = date, fill = minutesListened)) + 
  geom_tile() + 
  labs(x= "Time of day", y= "Date", title = "Activity by date and time of day") +
  scale_fill_gradient(low = "green", high = "yellow")
timeDay

# PLAYBACK ACTIVITY BY TIME OF THE DAY
hoursDay <- mySpotify %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(date, hour = hour(endTime), weekday = wday(date, label = TRUE))%>% 
  summarize(minutesListened = sum(minutes))
hoursDay %>% 
  ggplot(aes(x = hour, y = minutesListened, group = date)) +
  geom_col()



# Top 10 most streamed artists
topArtists <- mySpotify %>% 
  group_by(artistName) %>% 
  summarise(totalMinutes = sum(minutes)) %>% 
  arrange(desc(totalMinutes)) %>% 
  top_n(10)

ggplot(topArtists, aes(x = reorder(artistName, totalMinutes), y = totalMinutes)) +
  geom_col(fill = "green") +
  coord_flip() +
  labs(x = "Artist", y = "Minutes of music playback") +
  ggtitle("Top 10 most streamed artists", "Based on total minutes of playback")


# Top 10 most streamed songs
topSongs <- mySpotify %>% 
  group_by(trackName) %>% 
  summarise(totalMinutes = sum(minutes)) %>% 
  arrange(desc(totalMinutes)) %>% 
  top_n(10)

ggplot(topSongs, aes(x = reorder(trackName, totalMinutes), y = totalMinutes)) +
  geom_col(fill = "green") +
  coord_flip() +
  labs(x = "Song", y = "Minutes of music playback") +
  ggtitle("Top 10 most streamed songs", "Based on total minutes of playback")





# Top Genres 
genres <- c("Pop", "Hip Hop", "Rock") # List of genres to include
songsPerMonth <- mySpotify %>% 
  filter(date >= "2020-01-01") %>%
  mutate(genre = str_extract(trackName, paste(genres, collapse = "|", sep = ""))) %>% 
  filter(!is.na(genre)) %>% # Remove rows with no genre
  group_by(genre, date = floor_date(date, "month")) %>% # 
  summarize(totalSongs = n()) %>% 
  ggplot(aes(x = date, y = totalSongs, group = genre)) + 
  geom_line(aes(color = genre), size = 1.2) + 
  scale_color_manual(values = c("Pop" = "green", "Hip Hop" = "black", "Rock" = "white")) + 
  labs(x= "Date", y= "Total songs played", color = "Genre") + 
  ggtitle("Total songs played per month by genre", "Shows the number of songs played each month for specific genres")
songsPerMonth



