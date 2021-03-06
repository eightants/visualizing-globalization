library(tidyverse)
library(rvest)
library(sf)
library(glue)
library(rnaturalearth)
library(gifski)

url <- "https://en.wikipedia.org/wiki/Spotify"
# confirms url
url

# reads in the HTML table from wikipedia
table <- url %>%
  read_html() %>%
  html_nodes(xpath = '/html/body/div[3]/div[3]/div[4]/div/table[6]') %>%
  html_table()  %>% .[[1]]

# extracts the country and year columns from the data
table <- table[1:2]

table

# learning: checks type of data the table is, changes the column name to something more friendly
class(table[2])
colnames(table)[1] <- "Year"
colnames(table)[2] <- "Country"

# separates multiple countries in one row into their own row with date
table <- separate_rows(table, Country, sep="\n ")

# extracts only the opening year from the full date
# table[1]$Year <- as.numeric(str_sub(table[1]$Year, -4, -1))

### Section to take both months and year of the expansion
### Can't get function to iterate by month
# replaces first few ints to get month and year only
# table[1]$Year <- str_replace(table[1]$Year, "\\d+\\s+", "")
# table[1]$Year <- paste0("1 ", table[1]$Year)
# table[1]$Year <- as.Date(table[1]$Year, "%d %B %Y")

table

# TO DO: 
# Obtain the month and year https://www.statmethods.net/input/dates.html
# plot using month and year

table[is.na(table)] = "Soon"
# removes any invalid cells
table = table[ !grepl("Soon", table$Year) , ]

table

countries_sf <- ne_countries(scale = "medium", returnclass = "sf")

mismatches <- table %>%
  anti_join(countries_sf, by  = c("Country" = "name_en"))
mismatches

country_match <- tribble(
  ~Country, ~sf_country,
  "United States", "United States of America")
country_match


# joins the data frames to fix name mismatches
joined_data <- 
  table %>%
  left_join(country_match) %>%
  mutate(country = ifelse(is.na(sf_country), Country, sf_country)) %>%
  left_join(countries_sf, by  = c("country" = "name_en"))

ggplot() + geom_sf(data = countries_sf) 

# plots spotify on the map
plot_fun <- function(open_year){
  p <- ggplot() + 
    ## Add the background countries, but filter away Antarctica because it is unecessary.
    geom_sf(data = filter(countries_sf, name_en != "Antarctica")) + 
    ## Add the countries data, filtering so countries only appear after spotify has joined
    geom_sf(data = filter(joined_data, Year <= open_year), fill = rgb(30, 215, 96, maxColorValue = 255)) + 
    ## Change the theme so we don't get a background plot or axes. The coord_sf part is a workaround to a current bug that makes gridlines appear.
    theme_void() + coord_sf(datum=NA) + guides(fill=FALSE) + 
    ## Stitch the year to the title with glue
    labs(title = glue("    Year: {open_year}"),
         subtitle = glue("      Countries with Spotify: {nrow(filter(joined_data, Year <= open_year))}"))
  print(p)
}

# saves visualization as a gif
save_gif(walk(min(joined_data$Year):max(joined_data$Year), plot_fun), delay = 0.5, gif_file = "spotify.gif")
