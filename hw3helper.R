if (!require("XML")) {
  install.packages("XML") # do this once per lifetime
  stopifnot(require("XML")) # do this once per session
}
if (!require("RCurl")) {
  install.packages("RCurl") # do this once per lifetime
  stopifnot(require("RCurl")) # do this once per session
}

land_table = readHTMLTable(getURL("https://simple.wikipedia.org/wiki/List_of_U.S._states_by_area"), stringsAsFactors=FALSE)
land_area <- land_table[[1]]$V3[3:52];
land_area <- gsub(pattern = ",", replacement = "", x = land_area)

farm_areas <- read.csv("farm.csv")

land_table = readHTMLTable(getURL("https://simple.wikipedia.org/wiki/List_of_U.S._states_by_area"))
state <- land_table[[1]]$V1[3:52]
land <- land_table[[1]]$V3[3:52]
state_order <- order(state)
state <- state[state_order]
land <- land_area[state_order]
land <- as.numeric(gsub(pattern = ",", replacement = "", x = land))
farm <- farm_areas$sq.miles
farm <- gsub(pattern = ",", replacement = "", x = farm)
area <- data.frame(state,farm,land)

plot(x=land, y=farm)

land_index = 2
farm_index = 43

reg_line <- lm(farm ~ land)
plot(land, farm)
abline(reg_line)
#area1 <- subset(area, state != "Alaska")
x <- land[-index.2]
y <- farm[-index.2]
reg_line_sub <- lm(y~x)
#reg_line_sub <- lm(farm ~ land, data = area1)
abline(reg_line_sub, col = "red")

plot(reg_line$fitted.values, reg_line$residuals)
abline(0, 0)

r.jack <- as.numeric()
for (i in unique(area$state)) {
  temp_sub <- subset(area, state != i)
  removed_index <- subset(area, state == i)
  
  temp_reg <- lm(farm ~ land, temp_sub)
  predicted_val <- predict(temp_reg, newdata = data.frame(land = removed_index$land))
  jack_value <- abs(removed_index$farm - predicted_val)
  r.jack[i] <- jack_value
}

plot(area$land, r.jack)

movies <- readLines("http://www.imdb.com/chart/top")
titles <- grep("titleColumn", movies) + 2
links_full <- movies[titles]
links_specific <- substring(links_full, 16, 32)

# create list of producers
web_links <- paste0("http://www.imdb.com", links_specific, "fullcredits")
web_links <- gsub(pattern = "http", replacement = "https", x = web_links)

producers <- list()
#producer_names <- list()
#tables <- readHTMLTable(web_links)
tables <- list(length = 250)

for (i in 1:250) {
  tables[i] <- readHTMLTable(web_links[i])
}

for (i in 1:250) {
  producers[[i]] <- (tables[[i]][4])
  df <- as.data.frame(producers[[i]])
  producer_names[[i]] <- as.vector(df$NULL.V1)
}

unlisted <- unlist(producer_names)
name_table <- table(unlisted)
name_table <- sort(name_table, decreasing = TRUE)
print(name_table[1:5])


# own
movies = readLines("http://www.imdb.com/chart/top")
movie_lines = grep(pattern = "titleColumn",x = movies) + 2
link = movies[movie_lines]
select = substring(link,first = 16,last = 32)
title = paste0("http://www.imdb.com",select, "fullcredits")
#title <- gsub(pattern = "http", replacement = "https", x = title)
producers = list()
table = list()

for (i in 1:250){
  tab = getURL(title[i])
  table[[i]] = readHTMLTable(rawToChar(tab$content))  # which = 1
}

for (i in 1:250) {
  producers[[i]] = table[[i]][4]
  #producers[[i]] = table[[i]]$V1
  df = as.data.frame(producers[[i]])
  producers[[i]] = as.vector(df$NULL.V1)
}

producer_name = unlist(producers)
producer_table = table(producer_name)
sample(producer_table,5)

######################################
land_area <- land_table[[1]]$V3[3:52];
land_area <- gsub(pattern = ",", replacement = "", x = land_area)

land_table = readHTMLTable(getURL("https://simple.wikipedia.org/wiki/List_of_U.S._states_by_area"))
state <- land_table[[1]]$V1[3:52]
land <- land_table[[1]]$V3[3:52]
state_order <- order(state)
state <- state[state_order]
land <- land_area[state_order]
land <- as.numeric(gsub(pattern = ",", replacement = "", x = land))
farm <- farm_areas$sq.miles
farm <- as.numeric(farm)
area <- data.frame(state,farm,land)