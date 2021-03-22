

# ADD TO X for GRAPHS
x <- y %>%
  filter(Date == date_today) %>%
  select("Name", "Quota", "Color") %>%
  as.data.frame()
x <- x[rev(order(x$Quota)), ]



# DISTRIBUTION
plot(x$Quota ~ seq(1, nrow(x)), xaxt = "n", col = x$Color, pch = 15, ylab = "Quota", xlab = "Workers")
axis(1, at = seq(1, nrow(x)), labels = x$Name)
abline(h = mean(x$Quota), col = "green", lwd = 2)
# text(2, mean(x$Quota) + max(x$Quota) / 30, paste("Mean:", round(mean(x$Quota), 1)))
abline(h = median(x$Quota), col = "orange", lwd = 2)
# text(2, median(x$Quota) + max(x$Quota) / 30, paste("Median:", round(median(x$Quota), 1)))
title("Quota Wealth Distribution")

# LABELS FOR DISTRIBUTION
text(1, x$Quota[1] - max(x$Quota) / 30, x$Quota[1])
for (i in seq(2, nrow(x))) {
  text(i, x$Quota[i] + max(x$Quota) / 30, x$Quota[i])
}
title(sub = paste("Total Quota Circulation", sum(x$Quota)))

# LEGEND
legend("topright", 
        legend = c(paste0("Mean: ", round(mean(x$Quota), 1)), 
        paste0("Median: ", round(median(x$Quota), 1)), 
        paste0("Workers: ", nrow(x))),
       col = c("green", "orange"), lwd = c(2, 2, NA), inset = 0.05)





# BARGRAPH
scale <- list(
  "The Top Earner" = x$Quota[1],
  "2nd and 3rd" = x$Quota[2:3], 
  "The Rest" = x$Quota[4:nrow(x)]
)
names(scale)[1] <- x$Name[x$Quota == max(x$Quota)]
names(scale)[2] <- paste(x$Name[x$Quota == x$Quota[2]], "and",
                         x$Name[x$Quota == x$Quota[3]])
a <- as.table(unlist(lapply(scale, sum)))
barplot(a, main = "Wealth Inequality Compared",
        ylim = c(0, max(x$Quota) * 1.25),
        xlab = "Quota Class", ylab = "Wealth",
        border = "Red", density = 50, col = c("Green", "Yellow", "Red")
)
text(0.66, x$Quota[1] - max(x$Quota) / 30, x$Quota[1])
text(1.9, sum(x$Quota[2:3]) - max(x$Quota) / 30, sum(x$Quota[2:3]))
text(3.10, sum(x$Quota[4:nrow(x)]) - max(x$Quota) / 30,  sum(x$Quota[4:nrow(x)]))
abline(h = sum(x$Quota[2:3]) + sum(x$Quota[4:nrow(x)]), col = "orange")
text(3, sum(x$Quota[2:3]) + sum(x$Quota[4:nrow(x)]) + max(x$Quota) / 20, 
     paste("Everyone except the 0.1%: ", sum(x$Quota[2:3]) + sum(x$Quota[4:nrow(x)])))





# CHECK HISTORY QUOTA OF A PERSON
person <- "Chic"
a <- y %>%
  filter(Name == person) %>%
  select("Quota")
attributes(a) <- NULL
a <- as.table(unlist(a))
person_max <- max(filter(y, Name == person)$Quota)
person_low <- min(filter(y, Name == person)$Quota)
person_quota <- filter(y, Name == person)$Quota

# Basic
barplot(a, names.arg = y$Date[y$Name == person], main = paste(person, "'s Distribution"))

# Premium
barplot(a, names.arg = y$Date[y$Name == person], main = paste(person, "'s Distribution"),
        col = filter(y, y$Name == person)$Color[[1]], 
        ylim = c(0, person_max + person_max / 3)
)
# Mean
# abline(h = mean(person_quota), col = "Red", lwd = 2)
# text(2, mean(person_quota) + person_max / 30, paste("Mean:", round(mean(person_quota)), 1))
# Median
# abline(h = median(person_quota), col = "Blue", lwd = 2)
# text(2, median(person_quota) + person_max / 30, paste("Median:", round(median(person_quota), 1)))
# Max
abline(h = person_max, col = "Green", lwd = 2)
text(2, max(person_quota) + person_max / 30, paste("Peak:", round(max(person_quota), 1)))
# Sum
#title(sub = paste("Total Lifetime Gain: ", sum(person_quota)))
# Low
# abline(h = person_min, col = "Red", lwd = 2)
# text(2, min(person_quota) + person_low / 30, paste("Low:", round(max(person_quota), 1)))



# FOR FIRST PLOT
person <- "Flex"
# CHANGE PERSON TO BE ADDED
a <- y %>%
  filter(Name == person) %>%
  select("Quota")
attributes(a) <- NULL
a <- as.table(unlist(a))
person_max <- max(filter(y, Name == person)$Quota)
person_quota <- filter(y, Name == person)$Quota

# Premium DISTRIBUTION
barplot(a, names.arg = unique(y$Date), main = "Multiple Distribution",
        col = filter(y, y$Name == person)$Color[[1]],
        ylim = c(0, person_max + person_max / 3)
)
# Add lines
abline(h = person_max, col = "Green", lwd = 2)
text(2, max(person_quota) + person_max / 30, paste(person, "'s max: ", round(max(person_quota), 1), sep = ""))

# ADD PEOPLE -------
person <- "Chic"
# CHANGE PERSON TO BE ADDED
a <- y %>%
  filter(Name == person) %>%
  select("Quota")
attributes(a) <- NULL
a <- as.table(unlist(a))
person_max <- max(filter(y, Name == person)$Quota)
person_quota <- filter(y, Name == person)$Quota
# ADD EXTRA GRAPH
barplot(a, names.arg = unique(y$Date), 
        main = "Multiple Distribution", density = 75,
        col = filter(y, y$Name == person)$Color[[1]], 
        add = TRUE)
abline(h = person_max, col = "Green", lwd = 2)
text(2, max(person_quota) + person_max / 30, paste(person, "'s max: ", round(max(person_quota), 1), sep = ""))



