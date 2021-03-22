library(tidyverse)

# Add Y tibble for general
y <- read_csv("QuotaLB2")
# ADD Z networth
z <- read_csv("nw.csv")
# SET DATE
date_today <- as.character(Sys.Date())
# date_today <- "2021-03-20"

for (i in seq_len(nrow(z))) {
  if (sum(y$ID == z$userid[i]) > 0) {
    nickname <- unique((y[y$ID == z$userid[i],])$Name)
    color <- unique((y[y$ID == z$userid[i],])$Color)
  } else {
    nickname <- substr(z$display_name[i], 1, 4)
    color <- "Black"
  }
  y <- y %>%
    rbind(c(Date = date_today, Name = nickname, Quota = as.numeric(z$networth[i]), Color = color, ID = z$userid[i]))
}
# Arrange Y
y$Quota <- as.numeric(y$Quota)
y <- y %>%
  arrange(Date, desc(Quota))

write_csv(y, "QuotaLB2")
