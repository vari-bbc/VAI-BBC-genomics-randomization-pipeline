# this is the same as the plot_arrays_function.R except that I'm attempting to use the switch function
# instead of ifelse to minimize lines of code
library(tidyverse)
library(ggplate) # for plate_plot function 
# for colorblind friendly palettes - can condense these
library(rcartocolor)
library(viridis)
library(RColorBrewer)
library(roxygen2)


#### use roxygen2

# add a section to the app that tells the client how many BCs they'll need
fill_BCs <- function(data, dynamic_array_type = c("GSA", "EPIC", "Mouse_Methyl")) {
  
  if(dynamic_array_type == "GSA") {
    # one beachip can hold 24 samples
    numBCs <- length(data$ID) / 24 # <- the 24 comes from per beadchip
    # so with 96 samples, we can fill 4 BCs
    # so we want to select that many unique BCs to be in our rotation
  } else if (dynamic_array_type == "EPIC") {
    numBCs <- length(data$ID) / 8
    
  } else if (dynamic_array_type == "Mouse_Methyl") {
    numBCs <- length(data$ID) / 12
  }
  # needs to be ceiling bc if we're using a fraction of a BC, then we're using all of it
  return(ceiling(numBCs))
}






plot_array_switch <- function(data, 
                       dynamic_array_type = c("GSA", "EPIC", "Mouse_Methyl"), 
                       dynamic_condition, 
                       plot_type= c("simple", "randed"), 
                       seed = 666){
# these are the only options allowed for array type c("GSA", "EPIC", "Mouse Methyl")  
# colorblind friendly color palettes below 

  col_vector_small <-
    c(
      palette.colors(palette = "Okabe-Ito"),
      rcartocolor::carto_pal(name = "Safe"),
      viridis::viridis(8)
    )
  
  
  col_vector <- c(
    "#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7",
    "#332288", "#88CCEE", "#44AA99", "#117733", "#999933", "#DDCC77", "#661100", "#AA4466",
    "#882255", "#6699CC", "#888888", "#FFD700", "#ADFF2F", "#00CED1", "#40E0D0", "#DA70D6",
    "#8A2BE2", "#5F9EA0", "#CD5C5C", "#8B0000", "#3CB371", "#B8860B", "#4682B4", "#FF69B4",
    "#7CFC00", "#DC143C", "#8B4513", "#A0522D", "#B0C4DE", "#556B2F", "#6B8E23", "#00FA9A",
    "#7B68EE", "#00FF7F", "#20B2AA", "#D2B48C", "#DAA520", "#B22222", "#FF4500", "#2E8B57",
    "#FA8072", "#6A5ACD", "#F08080", "#9ACD32", "#6495ED", "#FF6347", "#BA55D3", "#A9A9A9",
    "#7FFF00", "#D2691E", "#BC8F8F", "#F5DEB3", "#5F9EA0", "#E9967A", "#AFEEEE", "#BDB76B",
    "#3CB371", "#FFE4B5", "#F4A460", "#8FBC8F", "#B0E0E6", "#CD853F", "#4169E1", "#D8BFD8",
    "#9932CC", "#8B008B", "#8A2BE2", "#9370DB", "#DA70D6", "#EE82EE", "#FF00FF", "#C71585",
    "#DB7093", "#FF1493", "#FF69B4", "#FFB6C1", "#FFC0CB", "#DC143C", "#B22222", "#A52A2A",
    "#800000", "#8B0000", "#FA8072", "#E9967A", "#FFA07A", "#F08080", "#CD5C5C", "#BC8F8F",
    "#DEB887", "#D2B48C", "#F5DEB3", "#FFE4B5", "#FFD700", "#FFA500", "#FF8C00", "#FF7F50",
    "#FF6347", "#FF4500", "#FF0000", "#DC143C", "#B22222", "#A52A2A", "#800000", "#8B0000",
    "#556B2F", "#6B8E23", "#808000", "#9ACD32", "#ADFF2F", "#7FFF00", "#7CFC00", "#00FF00",
    "#32CD32", "#98FB98", "#90EE90", "#00FA9A", "#00FF7F", "#3CB371", "#2E8B57", "#228B22",
    "#008000", "#006400", "#66CDAA", "#8FBC8F", "#20B2AA", "#008B8B", "#008080", "#00FFFF",
    "#00CED1", "#40E0D0", "#48D1CC", "#00BFFF", "#1E90FF", "#6495ED", "#4682B4", "#5F9EA0",
    "#7B68EE", "#6A5ACD", "#483D8B", "#4169E1", "#0000FF", "#0000CD", "#00008B", "#000080",
    "#191970", "#8A2BE2", "#9932CC", "#9400D3", "#BA55D3", "#DA70D6", "#EE82EE", "#D8BFD8",
    "#DDA0DD", "#E6E6FA", "#F0F8FF", "#F8F8FF", "#F5F5F5", "#FFF5EE", "#F5F5DC", "#FDF5E6",
    "#FFFAF0", "#FFFFF0", "#FAFAD2", "#FFFFE0", "#FFFACD", "#FFEFD5", "#FFE4B5", "#FFDAB9",
    "#EEE8AA", "#F0E68C", "#BDB76B", "#E0FFFF", "#AFEEEE", "#B0E0E6", "#ADD8E6", "#87CEEB",
    "#87CEFA", "#00BFFF", "#1E90FF", "#6495ED", "#4682B4", "#5F9EA0", "#7B68EE", "#6A5ACD",
    "#483D8B", "#4169E1", "#8A2BE2", "#9932CC", "#9400D3", "#BA55D3", "#DA70D6", "#D8BFD8"
  )
  
  
# -----
  # the df below creates data that can be used directly in plate_plot() to create plots of the 
  # array types at a baseline 
  # this will be useful to plot examples of what our arrays look like when a user selects their
  # array type
  my_plate_data_96 <-
    data.frame(Row = paste0(rep(LETTERS[1:8], each = 12), rep(1:12, times = 8)),
               GSA = as.character(c(
                 rep(c(1:3), times = 8),
                 rep(c(1, 2, 4), times = 8),
                 rep(c(1, 3, 4), times = 8),
                 rep(c(2, 3, 4), times = 8)
               )),
               EPIC = as.character(rep(1:8, each = 12)), 
               Mouse_Methyl = 
                 as.character(c(
                   rep(c(1:3), times = 8),
                   rep(c(1, 2, 4), times = 8),
                   rep(c(1, 3, 4), times = 8),
                   rep(c(2, 3, 4), times = 8)
                 )))
  
  ### this setup below indicates the spatial aspects of BCs wrt. their plate:
  
  BC_GSA <- data.frame(Row = c(paste0(rep(LETTERS[1:8],each = 3), rep(1:3, times = 8)),
                               paste0(rep(LETTERS[1:8],each = 3), rep(4:6, times = 8)),
                               paste0(rep(LETTERS[1:8],each = 3), rep(7:9, times = 8)),
                               paste0(rep(LETTERS[1:8],each = 3), rep(10:12, times = 8))), 
                       BC_GSA = rep(c("BC1", "BC2", "BC3", "BC4"), each = 24))
  
  BC_EPIC <- data.frame(
    Row = c(paste0(rep(LETTERS[1:8], times = 8), rep(1:12, each = 8))), 
    BC_EPIC = rep(paste0("BC", 1:12), each = 8))
  
  BC_Methyl <- data.frame(
    Row = paste0(rep(LETTERS[1:8], times = 8), rep(1:12, each = 8)),
    BC_Methyl = rep(paste0("BC", 1:8), each = 12))
  
  
  m1 <- merge(my_plate_data_96, BC_GSA, by = "Row") # merge one gets GSA BC info onto dummy data
  m2 <- merge(m1, BC_EPIC, by = "Row") # merge 2 includes EPIC data
  m3 <- merge(m2, BC_Methyl, by = "Row") # merge 3 includes Mouse methyl data
  # we need this part bc Tracey wants to see which position a sample goes to on a beadchip
  # when we have fewer than 96 samples fitted onto a beadchip, we need to prioritize BCs to the left of the plate 
  
## ------ 
if(plot_type == "simple")  { ### ----
  p <- plate_plot(
    data = my_plate_data_96,
    position = Row,
    value = !!sym(dynamic_array_type), # the EXPR in the array function would be dynamic_array_type
    plate_size = 96,
    plate_type = "round",
    colour = col_vector,
    label_size = 4,
    show_legend = FALSE,
    silent = T,
    title = paste(dynamic_array_type)
  ) 
  
  q <- switch(dynamic_array_type,
              "GSA" = p + geom_vline(xintercept = c(3.5, 6.5, 9.5)),
              "EPIC" = p +  geom_vline(xintercept = seq(1.5, 11.5, by = 1)), 
              "Mouse_Methyl"= p + geom_segment(aes(x = 1.49, xend = 2.51, y = 4.5, yend= 4.5), linewidth = 1) +
                geom_segment(aes(x = 4.49, xend = 5.51, y = 4.5, yend= 4.5), linewidth = 1) +
                geom_segment(aes(x = 7.49, xend = 8.51, y = 4.5, yend= 4.5), linewidth = 1) +
                geom_segment(aes(x = 10.49, xend = 11.51, y = 4.5, yend= 4.5), linewidth = 1) +
                geom_segment(aes(x = 1.5, xend = 1.5, y = 0, yend= 4.5), linewidth = 1) +
                geom_segment(aes(x = 2.5, xend = 2.5, y = 4.5, yend= 9), linewidth = 1) +
                geom_vline(xintercept = 3.5, linewidth = 1) +
                geom_segment(aes(x = 4.5, xend = 4.5, y = 0, yend= 4.5), linewidth = 1) +
                geom_segment(aes(x = 5.5, xend = 5.5, y = 4.5, yend= 9), linewidth = 1) +
                geom_vline(xintercept = 6.5, linewidth = 1) +
                geom_segment(aes(x = 7.5, xend = 7.5, y = 0, yend= 4.5), linewidth = 1) +
                geom_segment(aes(x = 8.5, xend = 8.5, y = 4.5, yend= 9), linewidth = 1) +
                geom_vline(xintercept = 9.5, linewidth = 1) +
                geom_segment(aes(x = 10.5, xend = 10.5, y = 0, yend= 4.5), linewidth = 1) +
                geom_segment(aes(x = 11.5, xend = 11.5, y = 4.5, yend= 9), linewidth = 1))
  return(q)
  
## NOW WE BEGIN USING ROW DATA THAT HAS BEEN RANDOMIZED ----
} else if (plot_type == "randed") {
  ### NEXT we merge the actual data based on users length of data -----
  # if less than 96 samps, just fill in the rest with NA
  # if greater than, count how many greater than and assign to new plate
  # this means adding another statement before the plot code to check for the need of more than 1 plate
  ## User data adaptation - NEEDS to have Row
  ## need to apply checks to see how many samples we have
  ## if > 96, then we need more than 1 plate
  # length(data$ID) <- more specific to data, but nrow(data) would do the same
  if(length(data$ID) == 96){
    
    d2 <- cbind(data, m3) # use m3 since it has our BC positions
    set.seed(seed)
    rand_row <- sample(d2$Row[d2[[paste0("BC_", dynamic_array_type)]] %in% unique(d2[[paste0("BC_", dynamic_array_type)]][1:sum(!is.na(d2$ID))]) & !is.na(d2$ID)], sum(!is.na(d2$ID)))
    # print(rand_row)
  } else if(length(data$ID) < 96) {
    
    # this should work
    d2 <- str2str::cbind_fill(data, m3 %>% arrange(paste0("BC_", dynamic_array_type))) # so this would have to be arranged in this step to account for the BCs we want to fill 
    # so for this, we need to make sure to prioritize the BCs entire to the left of the plate
    # rand_row should then just be the length of IDs 
    # put the type of BC in order from BC1-BC[length(IDs)]
    # and only grab rows that correspond to those slots  
    set.seed(seed)
    rand_row <- sample(d2$Row[d2[[paste0("BC_", dynamic_array_type)]] %in% unique(d2[[paste0("BC_", dynamic_array_type)]][1:sum(!is.na(d2$ID))]) & !is.na(d2$ID)], sum(!is.na(d2$ID)))
    # print(rand_row)
  } else if (length(data$ID > 96)) {
    # we will then need more than 1 plate
    # this d0 to d1 portion should be generalized at some point 
    d0 <- cbind(data[1:96,], m3)
    d1 <- cbind(data[97:length(data$ID),], m3[1:(length(data$ID)-96),]) # this will be an issue if we need more than 2 plates
    d2 <- rbind(d0,d1)
    N <- length(d2$ID) # total number of samples
    num_plates <- ceiling(N/96) # how many plates we'll need
    plate_labels <- paste("Plate", 1:num_plates) # plate labels
    nms <- list()
    
    for (i in 1:num_plates) {
      # If it's the last plate, determine how many samples remain
      if (i == num_plates) {
        n_samples <- N - (96 * (num_plates - 1))
      } else {
        n_samples <- 96
      }
      nms[[i]] <- rep(plate_labels[i], times = n_samples)
    }
    plate_assignment <- unlist(nms)
    d2$Plate <- plate_assignment
    d2 <- d2 %>% arrange(Plate, paste0("BC_", dynamic_array_type))
    
    ## Apply a randomization to the IDs and observe how this changes -----
    
    # take a sample of rows, but entirely from these BC positions 
    # this wont work
    # rand_row <- sample(d2$Row[d2$BC_GSA %in% unique(d2$BC_GSA[1:sum(!is.na(d2$ID))]) & !is.na(d2$ID)], sum(!is.na(d2$ID)))
    # code below is modeled after the code above 
    set.seed(seed)
    rand_row <- sample(d2$Row[d2[[paste0("BC_", dynamic_array_type)]] %in% unique(d2[[paste0("BC_", dynamic_array_type)]][1:sum(!is.na(d2$ID))]) & !is.na(d2$ID)], sum(!is.na(d2$ID)))
    # print(rand_row)
  }
  


  # -------  
    p <-  plate_plot(
      data = d2, # this changes to user data 
      position = rand_row,
      value = !!sym(dynamic_condition), 
      label = ID,
      plate_size = 96,
      plate_type = "round",
      colour = col_vector,
      label_size = 4,
      show_legend = FALSE,
      silent = T,
      title = paste(dynamic_array_type)
    )  
    q <- switch(dynamic_array_type,
                "GSA" = p + geom_vline(xintercept = c(3.5, 6.5, 9.5)),
                "EPIC" = p +  geom_vline(xintercept = seq(1.5, 11.5, by = 1)), 
                "Mouse_Methyl"= p + geom_segment(aes(x = 1.49, xend = 2.51, y = 4.5, yend= 4.5), linewidth = 1) +
                  geom_segment(aes(x = 4.49, xend = 5.51, y = 4.5, yend= 4.5), linewidth = 1) +
                  geom_segment(aes(x = 7.49, xend = 8.51, y = 4.5, yend= 4.5), linewidth = 1) +
                  geom_segment(aes(x = 10.49, xend = 11.51, y = 4.5, yend= 4.5), linewidth = 1) +
                  geom_segment(aes(x = 1.5, xend = 1.5, y = 0, yend= 4.5), linewidth = 1) +
                  geom_segment(aes(x = 2.5, xend = 2.5, y = 4.5, yend= 9), linewidth = 1) +
                  geom_vline(xintercept = 3.5, linewidth = 1) +
                  geom_segment(aes(x = 4.5, xend = 4.5, y = 0, yend= 4.5), linewidth = 1) +
                  geom_segment(aes(x = 5.5, xend = 5.5, y = 4.5, yend= 9), linewidth = 1) +
                  geom_vline(xintercept = 6.5, linewidth = 1) +
                  geom_segment(aes(x = 7.5, xend = 7.5, y = 0, yend= 4.5), linewidth = 1) +
                  geom_segment(aes(x = 8.5, xend = 8.5, y = 4.5, yend= 9), linewidth = 1) +
                  geom_vline(xintercept = 9.5, linewidth = 1) +
                  geom_segment(aes(x = 10.5, xend = 10.5, y = 0, yend= 4.5), linewidth = 1) +
                  geom_segment(aes(x = 11.5, xend = 11.5, y = 4.5, yend= 9), linewidth = 1))

   return(q)
}
}

## ----



