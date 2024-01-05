library(fplscrapR)
library(ggplot2)
library(gganimate)
library(dplyr)
library(jsonlite)

# Modified R script to accept league ID as a command-line argument
args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop("Please provide the league ID as a command-line argument.")
}

# Convert the league ID to an integer
league_id <- as.integer(args[1])

# Display a message indicating that the script is starting
cat("Fetching data for league ID:", league_id, "\n")
# First you need to get the league information, including the id for each team/manager.  # nolint
league_data <- get_league(league_id)

league_df <- as.data.frame(league_data$standings$results)

entries <- league_df %>%
  select(entry, entry_name, player_name) %>%
  rename(name = player_name)

ids <- (entries$entry)

# Use lapply to fetch data for each entry using the list of ids created in previous step. # nolint
all_entries <- lapply(ids, get_entry_season)

# Combine the list of data frames into a single data frame
all_entries_df <- do.call(rbind, all_entries)
# Display the resulting list

nat_league <- all_entries_df %>%
  select(event, total_points, name)

nat_league <- left_join(nat_league, entries, by = "name")

# Select relevant columns (e.g., entry_name, player_name, total)
nat_league_sl <- nat_league %>%
  select("name", "entry_name", "total_points", "event")

# Create the rank variable within each gameweek
nat_league_ranked <- nat_league_sl %>%
  arrange(event, desc(total_points)) %>%
  group_by(event) %>%
  mutate(rank = row_number()) %>%
  ungroup()

#creating the anim object (which is a combo of ggplot and gganimate)
anim <- ggplot(nat_league_ranked, aes(x = rank,
                                      y = entry_name,
                                      label = entry_name,
                                      group = entry_name,
                                      fill = entry_name)) +
  geom_tile(aes(y = total_points/2,
                height = round(total_points, 0),
                width = 0.8,
                fill = entry_name),
                alpha = 0.6, show.legend = TRUE) +
  geom_text(aes(y = round(total_points, 0),
                label = paste (entry_name, round(total_points, 0)),
                hjust = ifelse(total_points > 700, 1, 0))) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  theme_minimal() + # removes a lot of plot elements
  theme(plot.title = element_text(color = "#01807e", face = "bold", hjust = 0, size = 30),
  axis.ticks.y = element_blank(), #removes axis ticks
  axis.text.y = element_blank(), #removes axis ticks
  panel.grid.major = element_blank(), #removes grid lines
    panel.grid.minor = element_blank(), #removes grid lines
    legend.position = "none",
    legend.justification = "none") +
  labs(title = "Gameweek : {closest_state}",
       subtitle = "FPL - National League",
       y = "FPL team",
       x = "Total points",
       caption = "Source: fplscrapR v0.2.6") +
  transition_states(event,
                    transition_length = 4,
                    state_length = 1, 
                    wrap = TRUE) + 
  ease_aes("cubic-in-out")
  
animate(anim,
        nframes = 200,
        fps = 8,
        height = 600,
        width = 500,
        renderer = gifski_renderer("fpl_mini_league_animation.gif"))

# Display a message indicating that the script has finished
cat("Script completed successfully!\n")
