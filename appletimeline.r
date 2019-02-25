library(tidyverse)
library(rvest)
library(sf)
library(glue)
library(rnaturalearth)
library(gifski)

url <- "https://en.wikipedia.org/wiki/Apple_Store"
# confirms url
url

# reads in the HTML table from wikipedia
table <- url %>%
read_html() %>%
html_nodes(xpath = '/html/body/div[3]/div[3]/div[4]/div/table[2]') %>%
html_table()  %>% .[[1]]

# extracts the country and year columns from the data
table <- table[2:3]

# learning: checks type of data the table is, changes the column name to something more friendly
class(table[2])
colnames(table)[1] <- "Country"
colnames(table)[2] <- "Year"

# removes any additional details about countries in parenthesis
table[1]$Country <- str_replace(table[1]$Country, " \\(.*", "")
table[1]$Country <- str_replace(table[1]$Country, "\\(.*", "")
# remove citations [13]
table[2]$Year <- str_replace(table[2]$Year, "\\[.*", "")
# extracts only the opening year from the full date
table[2]$Year <- as.numeric(str_sub(table[2]$Year, -4, -1))

table[is.na(table)] = "Soon"
# removes any cells that aren't open yet
table = table[ !grepl("Soon", table$Year) , ]

table

countries_sf <- ne_countries(scale = "medium", returnclass = "sf")

mismatches <- table %>%
  anti_join(countries_sf, by  = c("Country" = "name_en"))
mismatches

country_match <- tribble(
  ~Country, ~sf_country,
  "United States", "United States of America",
  "China", "People's Republic of China")
country_match


# joins the data frames to fix name mismatches
joined_data <- 
  table %>%
  left_join(country_match) %>%
  mutate(country = ifelse(is.na(sf_country), Country, sf_country)) %>%
  left_join(countries_sf, by  = c("country" = "name_en"))

ggplot() + geom_sf(data = countries_sf) 

# plots the opening of each apple store on the map, code from https://r-mageddon.netlify.com/post/the-burger-king-pandemic/
plot_fun <- function(open_year){
  p <- ggplot() + 
    ## Add the background countries, but filter away Antarctica because it is unecessary.
    geom_sf(data = filter(countries_sf, name_en != "Antarctica")) + 
    ## Add the countries data, filtering so countries only appear after it opened
    geom_sf(data = filter(joined_data, Year <= open_year), fill = rgb(0, 0, 0, maxColorValue = 255)) + 
    ## Change the theme so we don't get a background plot or axes. The coord_sf part is a workaround to a current bug that makes gridlines appear.
    theme_void() + coord_sf(datum=NA) + guides(fill=FALSE) + 
    ## Stitch the year to the title with glue
    labs(title = glue("    Year: {open_year}"),
         subtitle = glue("      Countries with Apple Stores: {nrow(filter(joined_data, Year <= open_year))}"))
  print(p)
}

# saves visualization as a gif
save_gif(walk(min(joined_data$Year):max(joined_data$Year), plot_fun), delay = 0.6, gif_file = "applestore.gif")
