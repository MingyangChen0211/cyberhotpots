# author: Chen, Mingyang
# prepare the data format basic codes: 
# file_list <- list.files(path = "/Users/chenmingyang/Desktop/commentsAnalysis/society", full.names = T)
# data_list <- lapply(file_list, read.csv)
# all_data <- do.call(rbind,data_list)
# data <- subset(all_data, label3 == 1)
# use control + f to replace label names into label 2to get robustness results

###########################
# library packages
###########################
library(lubridate)
library(ggplot2)
library(hms)
library(gridExtra)
options(warn = -1)

###########################
# compare time distribution (raw data)
###########################
plot_list <- list()
target <- c("society", "sport", "business", "politics", "entertainment")
for (i in 1:length(target)) {
  file_list <- list.files(path = paste0("/Users/chenmingyang/Desktop/commentsAnalysis/", target[i]), full.names = T)
  data_list <- lapply(file_list, read.csv)
  all_data <- do.call(rbind,data_list)
  data_bul <- subset(all_data, label3 == 1)
  data_nonbul <- subset(all_data, label3 == 0)
  
  data_bul$time_posix <- as.POSIXct(data_bul$timestamp, format = "%Y/%m/%d %H:%M")
  data_bul$time_hour <- hour(data_bul$time_posix) + minute(data_bul$time_posix)/60
  
  data_nonbul$time_posix <- as.POSIXct(data_nonbul$timestamp, format = "%Y/%m/%d %H:%M")
  data_nonbul$time_hour <- hour(data_nonbul$time_posix) + minute(data_nonbul$time_posix)/60
  
  data_bul$group <- "bul"
  data_nonbul$group <- "nonbul"
  data_combined <- rbind(data_bul, data_nonbul)
  
  p1 <- ggplot(data_combined, aes(x = time_hour, fill = group)) +
    geom_density(alpha = 0.5) +
    scale_x_continuous(
      breaks = seq(0, 24),
      labels = function(x) sprintf("%02d", floor(x))
    ) + 
    labs(title = paste0("Topic: ",target[i]), x = "time", y = "density") +
    theme_minimal() + 
    theme_bw()
  
  plot_list[[i]] <- p1
  
}
# plot
grid.arrange(grobs = plot_list, ncol = 3, nrow = 2)

###########################
# compare proportion in different topics
###########################
# chi-squere table
topic_table <- data.frame(topic = character(),
                            nonbul = integer(),
                            bul = integer(),
                            stringsAsFactors = FALSE)

for (i in 1:length(target)) {
  file_list <- list.files(path = paste0("/Users/chenmingyang/Desktop/commentsAnalysis/", target[i]), full.names = T)
  data_list <- lapply(file_list, read.csv)
  all_data <- do.call(rbind,data_list)
  data_bul <- subset(all_data, label3 == 1)
  data_nonbul <- subset(all_data, label3 == 0)

  proportion <- length(data_bul$diff)/length(all_data$diff)
  print(paste0("the results of ", target[i], ": ", proportion))
  
  n_bul <- length(data_bul$diff)
  n_nonbul <- length(data_nonbul$diff)
  topic_table <- rbind(topic_table, 
                         data.frame(topic = target[i],
                                    nonbul = n_nonbul,
                                    bul = n_bul,
                                    stringsAsFactors = FALSE))
}

####
# final chi-sq. 
###
print(topic_table)
counts <- topic_table[, c("nonbul", "bul")] 
chisq.test(counts)


###########################
# compare proportion in different platforms
###########################
combined_all <- data.frame()
for (i in 1:length(target)) {
  file_list <- list.files(path = paste0("/Users/chenmingyang/Desktop/commentsAnalysis/", target[i]), full.names = T)
  data_list <- lapply(file_list, read.csv)
  all_data <- do.call(rbind,data_list)
  combined_all <- rbind(combined_all, all_data)
}
# total data
data_all_bul <- subset(combined_all, label3 == 1)
length(data_all_bul$timestamp) / length(combined_all$timestamp)

# platform analysis
plat_table <- data.frame(plat = character(),
                          nonbul = integer(),
                          bul = integer(),
                          stringsAsFactors = FALSE)

plat <- c("wb", "dy", "xhs", "bili")
for (i in 1:length(plat)) {
  data_plat <- subset(combined_all, platform == plat[i])
  data_bul <- subset(data_plat, label3 == 1)
  data_nonbul <- subset(data_plat, label3 == 0)

  proportion <- length(data_bul$diff)/length(data_plat$diff)
  print(paste0("the results of ", plat[i], ": ", proportion))
  
  n_bul <- length(data_bul$diff)
  n_nonbul <- length(data_nonbul$diff)
  plat_table <- rbind(plat_table, 
                       data.frame(plat = plat[i],
                                  nonbul = n_nonbul,
                                  bul = n_bul,
                                  stringsAsFactors = FALSE))
}
####
# final chi-sq. 
###
print(plat_table)
counts <- plat_table[, c("nonbul", "bul")] 
chisq.test(counts)

###########################
# compare proportion in different field * platform
###########################
target <- c("society", "sport", "business", "politics", "entertainment")
plat <- c("wb", "dy", "xhs", "bili")
combinations <- expand.grid(platform = plat, target = target, stringsAsFactors = FALSE)
inter_results <- data.frame(combinations, nonbul = 0, bul = 0, stringsAsFactors = FALSE)

for (i in 1:length(target)) {
  file_list <- list.files(path = paste0("/Users/chenmingyang/Desktop/commentsAnalysis/", target[i]), full.names = T)
  data_list <- lapply(file_list, read.csv)
  all_data <- do.call(rbind,data_list)
  for (s in 1:length(plat)) {
    data_plat <- subset(all_data, platform == plat[s])
    data_bul <- subset(data_plat, label3 == 1)
    data_nonbul <- subset(data_plat, label3 == 0)
    
    proportion <- length(data_bul$diff)/length(data_plat$diff)
    print(paste0("the results of ", plat[s], " - ", target[i], ": ", proportion))
    
    idx <- which(inter_results$platform == plat[s] & inter_results$target == target[i])
    inter_results$nonbul[idx] <- length(data_nonbul$timestamp)
    inter_results$bul[idx]    <- length(data_bul$timestamp)
  }
}

print(inter_results)
result_use <- inter_results[-16,]
count_matrix <- as.matrix(result_use[, c("nonbul", "bul")])
rownames(count_matrix) <- paste(result_use$platform, result_use$target, sep = "_")
chisq.test(count_matrix)


###########################
# compare time distribution (platform)
###########################
plot_list <- list()
plot_list_notsmooth <- list()
for (i in 1:length(plat)) {
  data_plat <- subset(combined_all, platform == plat[i])
  
  data_plat$time_posix <- as.POSIXct(data_plat$timestamp, format = "%Y/%m/%d %H:%M")
  data_plat$time_hour <- hour(data_plat$time_posix) + minute(data_plat$time_posix)/60
  
  # hour variable
  data_plat$hour_bin <- floor(data_plat$time_hour)
  
  # calculate proportion
  library(dplyr)
  hourly_prop <- data_plat %>%
    group_by(hour_bin) %>%
    summarise(
      total = n(),
      bul_count = sum(label3 == 1, na.rm = TRUE),
      proportion = bul_count / total
    ) %>%
    filter(!is.na(proportion))
  
  # plot
  p1 <- ggplot(hourly_prop, aes(x = hour_bin, y = proportion)) +
    geom_smooth(method = "loess", se = T, color = "steelblue", size = 1, alpha = 0.2) +
    geom_point(alpha = 0.3, color = "steelblue") +  
    scale_x_continuous(breaks = seq(0, 23, by = 3)) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste0("Platform: ", plat[i]),
         x = "Hour of Day",
         y = "Bul Proportion") +
    theme_minimal() + theme_bw()
  p2 <- ggplot(hourly_prop, aes(x = hour_bin, y = proportion)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(alpha = 0.3, color = "steelblue") + 
    scale_x_continuous(breaks = seq(0, 23, by = 3)) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste0("Platform: ", plat[i]),
         x = "Hour of Day",
         y = "Bul Proportion") +
    theme_minimal() + theme_bw()
  
  plot_list[[i]] <- p1
  plot_list_notsmooth[[i]] <- p2
}

grid.arrange(grobs = plot_list, ncol = 2, nrow = 2)
grid.arrange(grobs = plot_list_notsmooth, ncol = 2, nrow = 2)

###########################
# compare time distribution: topics (bul proportion per hour)
###########################
plot_list <- list()
plot_list_notsmooth <- list()
target <- c("society", "sport", "business", "politics", "entertainment")

for (i in 1:length(target)) {
  file_list <- list.files(path = paste0("/Users/chenmingyang/Desktop/commentsAnalysis/", target[i]), full.names = TRUE)
  data_list <- lapply(file_list, read.csv)
  all_data <- do.call(rbind, data_list)
  all_data <- all_data[complete.cases(all_data),]
  
  all_data$time_posix <- as.POSIXct(all_data$timestamp, format = "%Y/%m/%d %H:%M")
  all_data$time_hour <- hour(all_data$time_posix) + minute(all_data$time_posix)/60
  
  # hour variable
  all_data$hour_bin <- floor(all_data$time_hour)
  
  # calculate proportion
  library(dplyr)
  hourly_prop <- all_data %>%
    group_by(hour_bin) %>%
    summarise(
      total = n(),
      bul_count = sum(label3 == 1, na.rm = TRUE),
      proportion = bul_count / total
    ) %>%
    filter(!is.na(proportion))
  
  # plot
  p1 <- ggplot(hourly_prop, aes(x = hour_bin, y = proportion)) +
    geom_smooth(method = "loess", se = T, color = "steelblue", size = 1, alpha = 0.2) +
    geom_point(alpha = 0.3, color = "steelblue") +  
    scale_x_continuous(breaks = seq(0, 23, by = 3)) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste0("Topic: ", target[i]),
         x = "Hour of Day",
         y = "Bul Proportion") +
    theme_minimal() + theme_bw()
  p2 <- ggplot(hourly_prop, aes(x = hour_bin, y = proportion)) +
    geom_line(color = "steelblue", size = 1) +
    geom_point(alpha = 0.3, color = "steelblue") + 
    scale_x_continuous(breaks = seq(0, 23, by = 3)) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste0("Topic: ", target[i]),
         x = "Hour of Day",
         y = "Bul Proportion") +
    theme_minimal() + theme_bw()
  
  plot_list[[i]] <- p1
  plot_list_notsmooth[[i]] <- p2
}

grid.arrange(grobs = plot_list, ncol = 3, nrow = 2)
grid.arrange(grobs = plot_list_notsmooth, ncol = 3, nrow = 2)


###########################
# compare time in different field * platform
###########################
plot_list <- list()
plot_list_notsmooth <- list()
target <- c("society", "sport", "business", "politics", "entertainment")
plat <- c("wb", "dy", "xhs", "bili")
for (i in 1:length(target)) {
  file_list <- list.files(path = paste0("/Users/chenmingyang/Desktop/commentsAnalysis/", target[i]), full.names = T)
  data_list <- lapply(file_list, read.csv)
  all_data <- do.call(rbind,data_list)
  for (s in 1:length(plat)) {
    data_plat <- subset(all_data, platform == plat[s])
    
    data_plat$time_posix <- as.POSIXct(data_plat$timestamp, format = "%Y/%m/%d %H:%M")
    data_plat$time_hour <- hour(data_plat$time_posix) + minute(data_plat$time_posix)/60
    
    # hour variable
    data_plat$hour_bin <- floor(data_plat$time_hour)
    
    # calculate proportion
    library(dplyr)
    hourly_prop <- data_plat %>%
      group_by(hour_bin) %>%
      summarise(
        total = n(),
        bul_count = sum(label3 == 1, na.rm = TRUE),
        proportion = bul_count / total
      ) %>%
      filter(!is.na(proportion))
    
    # plot
    p1 <- ggplot(hourly_prop, aes(x = hour_bin, y = proportion)) +
      geom_smooth(method = "loess", se = T, color = "steelblue", size = 1, alpha = 0.2) +
      geom_point(alpha = 0.3, color = "steelblue") +  
      scale_x_continuous(breaks = seq(0, 23, by = 3)) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = paste0("Topic: ", target[i], " Plat: ",plat[s]),
           x = "Hour of Day",
           y = "Bul Proportion") +
      theme_minimal() + theme_bw()
    p2 <- ggplot(hourly_prop, aes(x = hour_bin, y = proportion)) +
      geom_line(color = "steelblue", size = 1) +
      geom_point(alpha = 0.3, color = "steelblue") + 
      scale_x_continuous(breaks = seq(0, 23, by = 3)) +
      scale_y_continuous(labels = scales::percent) +
      labs(title = paste0("Topic: ", target[i], " Plat: ",plat[s]),
           x = "Hour of Day",
           y = "Bul Proportion") +
      theme_minimal() + theme_bw()
    
    plot_list <- append(plot_list, list(p1))
    plot_list_notsmooth <- append(plot_list_notsmooth, list(p2))
    
  }
}

grid.arrange(grobs = plot_list, ncol = 4, nrow = 5)
grid.arrange(grobs = plot_list_notsmooth, ncol = 4, nrow = 5)
