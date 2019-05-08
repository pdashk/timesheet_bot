## Timesheet Simulation
## Each block may either repeat, sample from other projects, or end day
library(dplyr)
library(tidyr)

## config ----
hours_block_mean <- 4
hours_block_sd <- 2
sd_slope <- 0.1
sd_int <- -4

## input ----
projects <- data.frame(Project = c("A","B","C","D","E"),
                       Hours = c(7.5,3.5,8,5,10),
                       stringsAsFactors = F)

## simulate ----
pool <- c(rep(projects$Project, projects$Hours / 0.25))
hours_daily_mean <- sum(projects$Hours)/5
hours_daily_sd <- abs(sum(projects$Hours)*sd_slope + sd_int)

results <- vector()
p_ends <- vector()
p_repeats <- vector()
p_samples <- vector()
path_selections <- vector()
hours_blocks <- vector()
hours_dailies <- vector()
indices <- vector()
pools <- list()

pick <- "end"
ends <- 4
path <- "sample"
results[1] <- pick
index <- 2

while (sum(!is.na(pool)) > 0) {

  if (path == "repeat") {
    pick <- pick
    pool <- pool[-which(pool == pick)[1]]
    hours_block <- (length(results) - tail(which(results != pick), n = 1) + 1) * 0.25
  } else if (path == "sample") {
    if (length(which(pool != pick)) > 0) {
        pick <- sample(pool[which(pool != pick)], 1)
        hours_block <- 0.25    
    } else {
        pick <- pick
        hours_block <- (length(results) - tail(which(results != pick), n = 1) + 1) * 0.25
    }
    pool <- pool[-which(pool == pick)[1]]
  } else if (path == "end") {
    pick <- "end"
    ends <- ends - 1
  }
  
  
  
  if (pick == "end") {
    path <- "sample"
  } else {
    hours_daily <- (length(results) - tail(which(results == "end"), n = 1) + 1) * 0.25
    if (pick %in% pool) {
      p_repeat <- pnorm(hours_block,hours_block_mean,hours_block_sd, lower.tail = F)
    } else {
      p_repeat <- 0
    }
    if (ends > 0) {
      p_end <- pnorm(hours_daily,hours_daily_mean, hours_daily_sd, lower.tail = T)
    } else {
      p_end <- 0
    }
    p_sample <- ifelse(p_repeat + p_end >= 1, 0.00001, 1 - p_repeat - p_end)
    probs <- c(p_repeat, p_sample, p_end)
    paths <- c("repeat","sample","end")
    path <- sample(paths, 1, prob = probs)
  }
  
  results[index] <- pick  
  p_ends[index] <- p_end
  p_repeats[index] <- p_repeat
  p_samples[index] <- p_sample
  path_selections[index] <- path
  hours_blocks[index] <- hours_block 
  hours_dailies[index] <- hours_daily
  indices[index] <- index
  pools[[index]] <- pool
  
  index <- index + 1
}

mark_days <- vector()
skip <- 1
days <- sample(c("Mon","Tues","Wed","Thur","Fri"),5,replace = F)
for (i in 2:length(results)) {
  mark_days[i] <- days[skip]
  if (results[i] == "end") {
    skip <- skip + 1
  }
}

#test matrix
final <- data.frame(Index = indices,
                    Project = results, 
                    Day = mark_days, 
                    p_ends,
                    p_repeats,
                    p_samples,
                    path_selections,
                    hours_blocks,
                    hours_dailies,
                    stringsAsFactors = F)
    
# day-wise
final2 <- data.frame(Project = results, Day = mark_days, stringsAsFactors = F)[-1,] %>%
  filter(Project != "end") %>%
  group_by(Day, Project) %>%
  summarize(Total = n() * 0.25) %>%
  spread(Day, Total) %>%
  replace(is.na(.), 0) %>%
  rbind(c("Total",colSums(.[,-1])))

print(final2)

# rm(list=ls())
