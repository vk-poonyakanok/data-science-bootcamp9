library(nycflights13)
library(tidyverse)

View(nycflights13)

data("flights")

?flights

View(flights)

View(airlines)

flights %>%
  mutate()

?mean()

flights %>%
  # Filter out flights with NA values in dep_delay or arr_delay
  filter(!is.na(dep_delay) & !is.na(arr_delay)) %>%
  mutate(arr_delayed = ifelse(arr_delay > 0, 1, 0)) %>%
  group_by(carrier) %>%
  summarise(total_flights = n(),
            avg_arr_delay = mean(arr_delay, na.rm = TRUE),
            arr_delayed_flights = sum(arr_delayed, na.rm = TRUE),
            arr_delay_percentage = (arr_delayed_flights / total_flights) * 100) %>%
  arrange(desc(arr_delay_percentage)) %>%
  left_join(airlines, by = "carrier") %>%
  select(name, arr_delay_percentage, total_flights, avg_arr_delay)


# Calculate the average flight delay per month
flights %>%
  # Filter out flights with NA values in dep_delay or arr_delay
  filter(!is.na(dep_delay) & !is.na(arr_delay)) %>%
  # Create a new column 'total_delay' as the sum of dep_delay and arr_delay
  mutate(total_delay = dep_delay + arr_delay) %>%
  # Group the data by 'month'
  group_by(month) %>%
  # Summarise to calculate the average total delay per month
  summarise(avg_delay = mean(total_delay, na.rm = TRUE)) %>%
  # Arrange the results by month
  arrange(month)

# Define a function to map months to seasons
map_season <- function(month) {
  if (month %in% c(3, 4, 5)) {
    return("Spring")
  } else if (month %in% c(6, 7, 8)) {
    return("Summer")
  } else if (month %in% c(9, 10, 11)) {
    return("Fall")
  } else {
    return("Winter")
  }
}

# Calculate the average flight delay by season
flights %>%
  # Filter out flights with NA values in dep_delay or arr_delay
  filter(!is.na(dep_delay) & !is.na(arr_delay)) %>%
  # Create a new column 'total_delay' as the sum of dep_delay and arr_delay
  mutate(total_delay = dep_delay + arr_delay) %>%
  # Apply the map_season function to create a new 'season' column
  mutate(season = sapply(month, map_season)) %>%
  # Group the data by 'season'
  group_by(season) %>%
  # Summarise to calculate the average total delay per season
  summarise(avg_delay = mean(total_delay, na.rm = TRUE)) %>%
  # Arrange the results by season
  arrange(season)

View(airports)

# Calculate the average arrival delay for each destination
flights %>%
  # Filter out flights with NA values in arr_delay
  filter(!is.na(arr_delay)) %>%
  # Group data by destination
  group_by(dest) %>%
  # Calculate average arrival delay for each destination
  summarise(avg_arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  # Arrange in descending order of average delay
  arrange(desc(avg_arr_delay)) %>%
  # Get only the top 5 destinations with the highest average delay
  head(5) %>%
  # Join with the airports data to get the name of the airport for each 'dest'
  left_join(airports, by = c("dest" = "faa")) %>%
  # Select only airport destination name and average arrival delay
  select(name, avg_arr_delay)

# Calculate the correlation between flight distance and arrival delay
flights %>%
  # Filter out flights with NA values in arr_delay
  filter(!is.na(arr_delay)) %>%
  # Use cor() function to find the correlation between distance and arr_delay
  summarise(correlation = cor(distance, arr_delay, use = "complete.obs"))

# Create a scatter plot of distance vs. arrival delay
ggplot(data = flights, aes(x = distance, y = arr_delay)) +
  geom_point(alpha = 0.3) +  # Use alpha to make points semi-transparent
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a linear regression line
  labs(title = "Scatter Plot of Flight Distance vs Arrival Delay",
       x = "Distance (miles)",
       y = "Arrival Delay (minutes)") +
  theme_minimal()

flights %>%
  filter(distance > 4500) %>%
  select(distance, origin, dest)

# Analyze delays by time of day
flights %>%
  # Remove flights with NA values in dep_delay
  filter(!is.na(dep_delay)) %>%
  # Extract hour from dep_time
  mutate(hour = sched_dep_time %/% 100, minute = sched_dep_time %% 100) %>%
  # Group by hour of the day
  group_by(hour) %>%
  # Calculate average delay per hour
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  # Arrange by average delay in descending order to find the time with the most delay
  arrange(desc(avg_dep_delay))

# Analyze and visualize delays by time of day
delays_by_time_of_day <- flights %>%
  # Remove flights with NA values in dep_delay
  filter(!is.na(dep_delay)) %>%
  # Extract hour from dep_time
  mutate(hour = sched_dep_time %/% 100, minute = sched_dep_time %% 100) %>%
  # Group by hour of the day
  group_by(hour) %>%
  # Calculate average delay per hour
  summarise(avg_dep_delay = mean(dep_delay, na.rm = TRUE))

# Create the plot
ggplot(delays_by_time_of_day, aes(x = hour, y = avg_dep_delay)) +
  geom_line(group = 1, color = "blue") +
  geom_point(color = "red") +
  labs(title = "Average Flight Departure Delay by Time of Day",
       x = "Hour of Day",
       y = "Average Delay (minutes)") +
  theme_minimal() +
  scale_x_continuous(breaks = 0:23)  # Assuming 'hour' is in 24-hour format
