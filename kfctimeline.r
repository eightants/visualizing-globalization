library(tidyverse)
library(rvest)
library(sf)
library(glue)
library(rnaturalearth)
library(gifski)

url <- "https://en.wikipedia.org/wiki/List_of_countries_with_KFC_franchises"
# confirms url
url

# function that reads in the HTML table from wikipedia
extract_data <- function(table_num){
  # sets up the xpath of the tables with a variable table indexs
  pathstring <- paste("/html/body/div[3]/div[3]/div[4]/div/table[", table_num, sep = "")
  pathstring <- paste(pathstring, "]", sep = "")
  # scrapes the data
  table <- url %>%
  read_html() %>%
  html_nodes(xpath = pathstring) %>%
  html_table(fill = TRUE)  %>% .[[1]]
  return(table)
}

# DEBUG: checks if dimensions of the table are valid, that the table was read in
table <- extract_data(2)
dim(table)

# runs function for data tables for each region, appending it to the dataframe with rbind
table1 <- extract_data(2)
for (num in 3:8) {
  table2 <- extract_data(num)
  table <- rbind(table1, table2)
  table1 <- table
}

# extracts the country and year columns from the data
table <- table[1:2]
table


# DEBUG: checks type of data the table is
class(table[2])

# changes the column name to something more friendly
colnames(table)[1] <- "Country"
colnames(table)[2] <- "Year"

# removes any additional details about countries in parenthesis
table[1]$Country <- str_replace(table[1]$Country, " \\(.*", "")
table[1]$Country <- str_replace(table[1]$Country, "\\(.*", "")
# remove citations [13]
table[1]$Country <- str_replace(table[1]$Country, "\\[.*", "")
table[2]$Year <- str_replace(table[2]$Year, "\\[.*", "")
# extracts only the opening year from the full date
table[2]$Year <- as.numeric(str_sub(table[2]$Year, -4, -1))

table

# inserts year for Guyana
table[2]$Year[70] <- 1994
table[is.na(table)] = "Soon"
# removes any cells with no opening date
table = table[ !grepl("Soon", table$Year) , ]

table

countries_sf <- ne_countries(scale = "medium", returnclass = "sf")

mismatches <- table %>%
  anti_join(countries_sf, by  = c("Country" = "name_en"))
mismatches

country_match <- tribble(
  ~Country, ~sf_country,
  "United States", "United States of America",
  "Eswatini", "eSwatini",
  "Bahamas", "The Bahamas",
  "North Macedonia", "Republic of Macedonia",
  "China", "People's Republic of China")
country_match


# joins the data frames to fix name mismatches
joined_data <- 
  table %>%
  left_join(country_match) %>%
  mutate(country = ifelse(is.na(sf_country), Country, sf_country)) %>%
  left_join(countries_sf, by  = c("country" = "name_en"))

ggplot() + geom_sf(data = countries_sf) 

# plots the opening of each kfc on the map, code from https://r-mageddon.netlify.com/post/the-burger-king-pandemic/
plot_fun <- function(open_year){
  p <- ggplot() + 
    ## Add the background countries, but filter away Antarctica because it is unecessary.
    geom_sf(data = filter(countries_sf, name_en != "Antarctica")) + 
    ## Add the countries data, filtering so countries only appear after it appeared in the country
    geom_sf(data = filter(joined_data, Year <= open_year), fill = rgb(163, 8, 12, maxColorValue = 255)) + 
    ## Change the theme so we don't get a background plot or axes. The coord_sf part is a workaround to a current bug that makes gridlines appear.
    theme_void() + coord_sf(datum=NA) + guides(fill=FALSE) + 
    ## Stitch the year to the title with glue
    labs(title = glue("    Year: {open_year}"),
         subtitle = glue("      Countries with KFC restaurants: {nrow(filter(joined_data, Year <= open_year))}"))
  print(p)
}

# saves visualization as a gif
save_gif(walk(min(joined_data$Year):max(joined_data$Year), plot_fun), delay = 0.5, gif_file = "kfc.gif")
