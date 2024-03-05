# Packages
library(tidyverse)
library(readxl)

#------------------------------------------------------------------------------
# Data import & wrangling
#------------------------------------------------------------------------------

path <- Sys.getenv("data_path")
sheets <- excel_sheets(path = path)

prodigy_controls <- list()

for (sheet in sheets){
  raw <- read_excel(
    path,
    sheet = sheet,
    skip = 1,
    col_types = c("text",
                  rep("numeric", ncol(read_excel(path, sheet = sheet)) - 1)))
  days <- t(raw)[1,]
  data <- t(raw)[2:length(raw),]
  ids <- row.names(data)
  colnames(data) <- days
  df <- as_tibble(data) %>% 
    mutate(id=ids) %>% 
    mutate(centre=if_else(row_number() <= 46, "HCB", "CUN")) %>% 
    relocate(c(id,centre)) %>% 
    pivot_longer(cols = days,
                 names_to = "visit_day",
                 values_to = "value") %>% 
    mutate(value = as.double(value))
  
  # Filter day 6 and 13
  prodigy_controls[[sheet]] <- df[df$visit_day!="6*" & df$visit_day!="13*",] %>% 
    mutate(visit_day = as.double(visit_day))
}

#------------------------------------------------------------------------------
# Calculate stats. tables
#------------------------------------------------------------------------------

# Function to build df with interquartile ranges of values grouped by visits
iqr_table <- function(data, time, value){
  data %>% 
    filter(!is.na({{ value }})) %>% 
    group_by({{ time }}) %>% 
    dplyr::summarise(
      q2 = median({{ value }}, na.rm=TRUE),
      q1 = quantile({{ value }}, 0.25, na.rm=TRUE),
      q3 = quantile({{ value }}, 0.75, na.rm=TRUE),
      n = n())
}

# Loop to obtain stats. for all procedure controls
prodigy_iqr <- list()

iqr_groups <- list()

for (sheet in sheets){
  iqr_all <- iqr_table(prodigy_controls[[sheet]],visit_day,value) # All
  iqr_hcb <- prodigy_controls[[sheet]] %>%  # HCB
    filter(centre == "HCB") %>% 
    iqr_table(visit_day, value)
  iqr_cun <- prodigy_controls[[sheet]] %>%  # CUN
    filter(centre == "CUN") %>% 
    iqr_table(visit_day, value)
  
  iqr_groups[["all"]] <- iqr_all
  iqr_groups[["hcb"]] <- iqr_hcb
  iqr_groups[["cun"]] <- iqr_cun
  
  prodigy_iqr[[sheet]] <- iqr_groups
}

#------------------------------------------------------------------------------
# Plots
#------------------------------------------------------------------------------

# Individual plots + IQR shaded ribbon
prodigy_plots_individual <- list()
for (sheet in sheets){
  
  if(sheet=="Count BCMACAR+x10e6KG")  {
    y_upper_lim<-35
    y_lower_lim<-0
    breaks<-5
  }
  
  if(sheet=="Viability") {
    y_upper_lim<-100
    y_lower_lim<-70
    breaks<-5
  }
  
  if(sheet=="CD3") {
    y_upper_lim<-100
    y_lower_lim<-90
    breaks<-2
  }
  
  if(sheet=="%CAR+") {
    y_upper_lim<-100
    y_lower_lim<-0
    breaks<-10
  }
  # y_lower_lim <- round(min(prodigy_iqr[[sheet]][["all"]]$q1))
  
  # Individual plots + IQR shaded ribbon
  this_plot <- ggplot() +
    # Individual evolutions
    geom_line(data =
                prodigy_controls[[sheet]] %>% 
                filter(!is.na(value), value != 0),
              aes(x = visit_day, y = value,
                  group = id, color = centre),
              alpha = 0.4) + 
    geom_point(data =
                 prodigy_controls[[sheet]] %>% 
                 filter(!is.na(value), value != 0),
               aes(x=visit_day, y = value,
                   color = centre),
               size=2.5) +
    # Ranges ribbon
    geom_line(data =  prodigy_iqr[[sheet]][["hcb"]],
              aes(x = visit_day, y = q2),
              color = "#008000", linewidth = 0.9) +
    geom_ribbon(data = prodigy_iqr[[sheet]][["hcb"]],
                aes(x = visit_day,
                    ymin = q1, ymax = q3),
                fill = "#008000", alpha = 0.2) +
    # Ranges ribbon
    geom_line(data =  prodigy_iqr[[sheet]][["cun"]],
              aes(x = visit_day, y = q2),
              color = "#0000c0", linewidth = 0.9) +
    geom_ribbon(data = prodigy_iqr[[sheet]][["cun"]],
                aes(x = visit_day,
                    ymin = q1, ymax = q3),
                fill = "#0000c0", alpha = 0.2) +
    # Legend
    scale_colour_manual(values = c("#0000c0","#008000")) +
    # Aesthetics
    theme(axis.line = element_line(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.title = element_blank(),
          legend.key = element_blank()
    ) +
    xlab("Days IN-process control") +
    ylab(sheet) +
    scale_x_continuous(breaks = c(7,8,9,10,11,12)) +
    scale_y_continuous(limits = c(y_lower_lim,y_upper_lim),
                       breaks = seq(y_lower_lim, y_upper_lim, breaks))
  
  prodigy_plots_individual[[sheet]] <- this_plot
}

# IQR with error bars (All patients)
prodigy_plots_error_bars_all <- list()
for (sheet in sheets){
  
  if(sheet=="Count BCMACAR+x10e6KG")  {
    y_upper_lim<-18
    y_lower_lim<-0
    breaks<-2
    annotation_position <- -4
  }
  
  if(sheet=="Viability") {
    y_upper_lim<-100
    y_lower_lim<-70
    breaks<-5
    annotation_position <- 63.4
  }
  
  if(sheet=="CD3") {
    y_upper_lim<-100
    y_lower_lim<-90
    breaks<-2
    annotation_position <- 87.8
  }
  
  if(sheet=="%CAR+") {
    y_upper_lim<-100
    y_lower_lim<-0
    breaks<-10
    annotation_position <- -21.8
  }
  
  this_plot <- ggplot() +
    geom_errorbar(data = prodigy_iqr[[sheet]][["all"]],
                  aes(x=visit_day,
                      ymin=q1, ymax=q3),
                  colour = "black",
                  width = 0.2) +
    geom_point(data = prodigy_iqr[[sheet]][["all"]],
               aes(x=visit_day, y=q2),
               colour = "black", size=2.5) +
    geom_line(data = prodigy_iqr[[sheet]][["all"]],
              aes(x=visit_day, y=q2),
              colour = "black",
              size = 0.8) +
    # Aesthetics
    theme(plot.margin = ggplot2::margin(b = 30), # Increase bottom margin for n values
          axis.line = element_line(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.title = element_blank(),
          legend.key = element_blank()
    ) +
    xlab("Days IN-process control") +
    ylab(sheet) +
    scale_x_continuous(breaks = c(7,8,9,10,11,12)) +
    scale_y_continuous(limits = c(y_lower_lim,y_upper_lim),
                       breaks = seq(y_lower_lim,y_upper_lim, breaks)) +
    # Add label with number of patients used to calclulate median & IQR
    geom_text(data = prodigy_iqr[[sheet]][["all"]],
              aes(x=visit_day, label=n),
              colour = "black",
              y=annotation_position,
              size=4, show.legend = F) + 
    coord_cartesian(ylim = c(y_lower_lim,y_upper_lim), clip = "off")
  
  prodigy_plots_error_bars_all[[sheet]] <- this_plot
}

# IQR with error bars (Grouped by Centre)
prodigy_plots_error_bars_groups <- {}
for (sheet in sheets){
  if(sheet=="Count BCMACAR+x10e6KG")  {
    y_upper_lim<-18
    y_lower_lim<-0
    breaks<-2
    annotation_position <- -4
    annotation_position_2 <- -5.2
  }
  
  if(sheet=="Viability") {
    y_upper_lim<-100
    y_lower_lim<-70
    breaks<-5
    annotation_position <- 63.4
    annotation_position_2 <- 61.5
  }
  
  if(sheet=="CD3") {
    y_upper_lim<-100
    y_lower_lim<-90
    breaks<-2
    annotation_position <- 87.8
    annotation_position_2 <- 87.2
  }
  
  if(sheet=="%CAR+") {
    y_upper_lim<-100
    y_lower_lim<-0
    breaks<-10
    annotation_position <- -21.8
    annotation_position_2 <- -28
  }
  
  this_plot <-
    ggplot() +
    # HCB
    geom_errorbar(data = prodigy_iqr[[sheet]][["hcb"]],
                  aes(x=visit_day,
                      ymin=q1, ymax=q3, colour = "HCB"),
                  width = 0.2) +
    geom_point(data = prodigy_iqr[[sheet]][["hcb"]],
               aes(x=visit_day, y=q2, colour = "HCB"),
               size=2.5) +
    geom_line(data = prodigy_iqr[[sheet]][["hcb"]],
              aes(x=visit_day, y=q2, colour = "HCB"),
              size = 0.8) +
    # CUN
    geom_errorbar(data = prodigy_iqr[[sheet]][["cun"]],
                  aes(x=visit_day,
                      ymin=q1, ymax=q3, colour = "CUN"),
                  width = 0.2) +
    geom_point(data = prodigy_iqr[[sheet]][["cun"]],
               aes(x=visit_day, y=q2, colour = "CUN"),
               size=2.5) +
    geom_line(data = prodigy_iqr[[sheet]][["cun"]],
              aes(x=visit_day, y=q2, colour = "CUN"),
              size = 0.8) +
    # Legend
    scale_colour_manual("", 
                        breaks = c("HCB", "CUN"),
                        values = c("#008000","#0000c0")) +
    # Aesthetics
    theme(plot.margin = ggplot2::margin(b = 50), # Increase bottom margin for n values
          axis.line = element_line(),
          panel.grid = element_blank(),
          panel.background = element_blank(),
          legend.title = element_blank(),
          legend.key = element_blank()
    ) +
    xlab("Days IN-process control") +
    ylab(sheet) +
    scale_x_continuous(breaks = c(7,8,9,10,11,12)) +
    scale_y_continuous(limits = c(y_lower_lim,y_upper_lim),
                       breaks = seq(y_lower_lim,y_upper_lim, breaks)) +
    # Add label with number of patients used to calclulate median & IQR
    geom_text(data = prodigy_iqr[[sheet]][["hcb"]],
              aes(x=visit_day, label=n, colour = "HCB"),
              y=annotation_position,
              size=4, show.legend = F) +
    geom_text(data = prodigy_iqr[[sheet]][["cun"]],
              aes(x=visit_day, label=n, colour = "CUN"),
              y=annotation_position_2, size=4, show.legend = F) +
    # geom_text(label="Num. of patients:", x=-6, y=-2.6, size=3, colour = "black") +   
    coord_cartesian(ylim = c(y_lower_lim,y_upper_lim), clip = "off")
  
  prodigy_plots_error_bars_groups[[sheet]] <- this_plot
}

#------------------------------------------------------------------------------
# Save plots
#------------------------------------------------------------------------------

plot_names <- c("CountBCMA", "Viability", "CD3", "CAR")

# Individual plots + IQR shaded ribbon
for (i in 1:4){
  ggsave(paste0("figures/","iqr_individual_",plot_names[i],".png"),
         plot = prodigy_plots_individual[[sheets[i]]],
         width = 16, height = 12, units = "cm", dpi = 300)
}
# IQR with error bars (All patients)
sheet<-"CD3"
for (i in 1:4){
  ggsave(paste0("figures/","iqr_all_",plot_names[i],".png"),
         plot = prodigy_plots_error_bars_all[[sheets[i]]],
         width = 16, height = 12, units = "cm", dpi = 300)
}
# IQR with error bars (All patients)
for (i in 1:4){
  ggsave(paste0("figures/","iqr_",plot_names[i],".png"),
         plot = prodigy_plots_error_bars_groups[[sheets[i]]],
         width = 16, height = 12, units = "cm", dpi = 300)
}
