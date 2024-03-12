# Packages
library(tidyverse)
library(readxl)
library(zoo) # to interpolate missing values

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
# Approach A: Interpolate missing data points in time-series
#------------------------------------------------------------------------------

prodigy_controls_interp <- list()

for (sheet in sheets) {
  
  bag_ids <- prodigy_controls[[sheet]]$id %>% unique()
  interp_output <- tibble()
  
  for (bag_id in bag_ids) {
    this_bag <- prodigy_controls[[sheet]][prodigy_controls[[sheet]]$id==bag_id,]
    
    this_bag_interp <- this_bag %>% 
      mutate(value = na.approx(value, rule=2))
    
    interp_output <- rbind(interp_output,this_bag_interp)
  }
  
  prodigy_controls_interp[[sheet]] <- interp_output
  
}

# When column ends with NA data tendency is lost if we interpolate

#------------------------------------------------------------------------------
#  Approach B: Fit trend line to individual time-series
#------------------------------------------------------------------------------

prodigy_controls_imputed <- list()

for (sheet in sheets) {
  
  bag_ids <- prodigy_controls[[sheet]]$id %>% unique()
  imputed_output <- tibble()
  
  for (bag_id in bag_ids) {
    
    # df of this pateint
    this_bag <- prodigy_controls[[sheet]][prodigy_controls[[sheet]]$id==bag_id,]
    # vector with missing days
    missing_days <- this_bag$visit_day[is.na(this_bag$value)]
    
    # Data vectors to fit the model
    value<-this_bag$value # dependent variable
    time<-this_bag$visit_day # independent variable
    
    # Linear model
    model <- lm(value~time, na.action = na.omit)
    
    # Use the previous model to predict values for 'missing_days'
    predictions <- predict(object = model,
                           newdata = data.frame(time = missing_days))
    imputed_values <- data.frame(visit_day=missing_days, value=predictions)
    
    # Replace 'NA' values from original df with the imputed values
    imputed_df <- this_bag
    imputed_df$value <-
      ifelse(is.na(this_bag$value),
             imputed_values$value[match(this_bag$visit_day,
                                        imputed_values$visit_day)],
             this_bag$value)
    
    imputed_df$value <- ifelse(imputed_df$value>100, 100, imputed_df$value)
    
    imputed_output <- rbind(imputed_output,imputed_df)
  }
  
  prodigy_controls_imputed[[sheet]] <- imputed_output
  
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

# Function with loop to obtain stats. for all procedure controls
iqr_calculate <- function(data){
  
  prodigy_iqr_output <- list()
  iqr_groups <- list()
  
  for (sheet in sheets){
    iqr_all <- iqr_table(data[[sheet]],visit_day,value) # All
    iqr_hcb <- data[[sheet]] %>%  # HCB
      filter(centre == "HCB") %>% 
      iqr_table(visit_day, value)
    iqr_cun <- data[[sheet]] %>%  # CUN
      filter(centre == "CUN") %>% 
      iqr_table(visit_day, value)
    
    iqr_groups[["all"]] <- iqr_all
    iqr_groups[["hcb"]] <- iqr_hcb
    iqr_groups[["cun"]] <- iqr_cun
    
    prodigy_iqr_output[[sheet]] <- iqr_groups
  }
  return(prodigy_iqr_output)
}

# Stats. of non-modified data
prodigy_iqr <- iqr_calculate(prodigy_controls)
# Stats. of interpolated data
prodigy_iqr_interp <- iqr_calculate(prodigy_controls_interp)
# Stats. of imputed data
prodigy_iqr_imputed <- iqr_calculate(prodigy_controls_imputed)

#------------------------------------------------------------------------------
# Plots
#------------------------------------------------------------------------------

# Individual plots + IQR shaded ribbon
build_individual_plots <- function(data, iqr_stats){
  
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
    
    this_plot <- ggplot() +
      # Individual evolutions
      geom_line(data =
                  data[[sheet]] %>% 
                  filter(!is.na(value), value != 0),
                aes(x = visit_day, y = value,
                    group = id, color = centre),
                alpha = 0.3) + 
      geom_point(data =
                   data[[sheet]] %>% 
                   filter(!is.na(value), value != 0),
                 aes(x=visit_day, y = value,
                     color = centre),
                 size=2.5, alpha = 0.3) +
      # Ranges ribbon
      geom_line(data =  iqr_stats[[sheet]][["hcb"]],
                aes(x = visit_day, y = q2),
                color = "#008000", linewidth = 0.9) +
      geom_ribbon(data = iqr_stats[[sheet]][["hcb"]],
                  aes(x = visit_day,
                      ymin = q1, ymax = q3),
                  fill = "#008000", alpha = 0.3) +
      # Ranges ribbon
      geom_line(data =  iqr_stats[[sheet]][["cun"]],
                aes(x = visit_day, y = q2),
                color = "#0000c0", linewidth = 0.9) +
      geom_ribbon(data = iqr_stats[[sheet]][["cun"]],
                  aes(x = visit_day,
                      ymin = q1, ymax = q3),
                  fill = "#0000c0", alpha = 0.3) +
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
  
  return(prodigy_plots_individual)
}

# IQR with error bars (All patients)
build_error_bars_all_plots <- function(iqr_stats){
  
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
      geom_errorbar(data = iqr_stats[[sheet]][["all"]],
                    aes(x=visit_day,
                        ymin=q1, ymax=q3),
                    colour = "black",
                    width = 0.2) +
      geom_point(data = iqr_stats[[sheet]][["all"]],
                 aes(x=visit_day, y=q2),
                 colour = "black", size=2.5) +
      geom_line(data = iqr_stats[[sheet]][["all"]],
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
      geom_text(data = iqr_stats[[sheet]][["all"]],
                aes(x=visit_day, label=n),
                colour = "black",
                y=annotation_position,
                size=4, show.legend = F) + 
      coord_cartesian(ylim = c(y_lower_lim,y_upper_lim), clip = "off")
    
    prodigy_plots_error_bars_all[[sheet]] <- this_plot
  }
  
  return(prodigy_plots_error_bars_all)
}

# IQR with error bars (Grouped by Centre)
build_error_bars_groups_plots <- function(iqr_stats){
  
  prodigy_plots_error_bars_groups <- list()
  
  for (sheet in sheets){
    if(sheet=="Count BCMACAR+x10e6KG")  {
      y_upper_lim<-22
      y_lower_lim<-0
      breaks<-2
      annotation_position <- -5
      annotation_position_2 <- -6.2
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
      geom_errorbar(data = iqr_stats[[sheet]][["hcb"]],
                    aes(x=visit_day,
                        ymin=q1, ymax=q3, colour = "HCB"),
                    width = 0.2) +
      geom_point(data = iqr_stats[[sheet]][["hcb"]],
                 aes(x=visit_day, y=q2, colour = "HCB"),
                 size=2.5) +
      geom_line(data = iqr_stats[[sheet]][["hcb"]],
                aes(x=visit_day, y=q2, colour = "HCB"),
                size = 0.8) +
      # CUN
      geom_errorbar(data = iqr_stats[[sheet]][["cun"]],
                    aes(x=visit_day,
                        ymin=q1, ymax=q3, colour = "CUN"),
                    width = 0.2) +
      geom_point(data = iqr_stats[[sheet]][["cun"]],
                 aes(x=visit_day, y=q2, colour = "CUN"),
                 size=2.5) +
      geom_line(data = iqr_stats[[sheet]][["cun"]],
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
      geom_text(data = iqr_stats[[sheet]][["hcb"]],
                aes(x=visit_day, label=n, colour = "HCB"),
                y=annotation_position,
                size=4, show.legend = F) +
      geom_text(data = iqr_stats[[sheet]][["cun"]],
                aes(x=visit_day, label=n, colour = "CUN"),
                y=annotation_position_2, size=4, show.legend = F) +
      # geom_text(label="Num. of patients:", x=-6, y=-2.6, size=3, colour = "black") +   
      coord_cartesian(ylim = c(y_lower_lim,y_upper_lim), clip = "off")
    
    prodigy_plots_error_bars_groups[[sheet]] <- this_plot
  }
  
  return(prodigy_plots_error_bars_groups)
}

# Individual plots + IQR shaded ribbon
prodigy_plots_individual <-
  build_individual_plots(prodigy_controls, prodigy_iqr)
prodigy_plots_individual_interp <-
  build_individual_plots(prodigy_controls_interp, prodigy_iqr_interp)
prodigy_plots_individual_imputed <-
  build_individual_plots(prodigy_controls_imputed, prodigy_iqr_imputed)
# IQR with error bars (All patients)
prodigy_plots_error_bars_all <-
  build_error_bars_all_plots(prodigy_iqr)
prodigy_plots_error_bars_all_interp <-
  build_error_bars_all_plots(prodigy_iqr_interp)
prodigy_plots_error_bars_all_imputed <-
  build_error_bars_all_plots(prodigy_iqr_imputed)
# IQR with error bars (Grouped by Centre)
prodigy_plots_error_bars_groups <-
  build_error_bars_groups_plots(prodigy_iqr)
prodigy_plots_error_bars_groups_interp <-
  build_error_bars_groups_plots(prodigy_iqr_interp)
prodigy_plots_error_bars_groups_imputed <-
  build_error_bars_groups_plots(prodigy_iqr_imputed)

#------------------------------------------------------------------------------
# Save plots
#------------------------------------------------------------------------------

plot_names <- c("CountBCMA", "Viability", "CD3", "CAR")

# Individual plots + IQR shaded ribbon
for (i in 1:4){
  ggsave(paste0("figures/missing_values/","iqr_individual_",plot_names[i],".png"),
         plot = prodigy_plots_individual[[sheets[i]]],
         width = 16, height = 12, units = "cm", dpi = 300)
  ggsave(paste0("figures/interpolation/","iqr_individual_",plot_names[i],".png"),
         plot = prodigy_plots_individual_interp[[sheets[i]]],
         width = 16, height = 12, units = "cm", dpi = 300)
  ggsave(paste0("figures/regression/","iqr_individual_",plot_names[i],".png"),
         plot = prodigy_plots_individual_imputed[[sheets[i]]],
         width = 16, height = 12, units = "cm", dpi = 300)
}
# IQR with error bars (All patients)
for (i in 1:4){
  ggsave(paste0("figures/missing_values/","iqr_all_",plot_names[i],".png"),
         plot = prodigy_plots_error_bars_all[[sheets[i]]],
         width = 16, height = 12, units = "cm", dpi = 300)
  ggsave(paste0("figures/interpolation/","iqr_all_",plot_names[i],".png"),
         plot = prodigy_plots_error_bars_all_interp[[sheets[i]]],
         width = 16, height = 12, units = "cm", dpi = 300)
  ggsave(paste0("figures/regression/","iqr_all_",plot_names[i],".png"),
         plot = prodigy_plots_error_bars_all_imputed[[sheets[i]]],
         width = 16, height = 12, units = "cm", dpi = 300)
}
# IQR with error bars (Grouped by Centre)
for (i in 1:4){
  ggsave(paste0("figures/missing_values/","iqr_",plot_names[i],".png"),
         plot = prodigy_plots_error_bars_groups[[sheets[i]]],
         width = 16, height = 12, units = "cm", dpi = 300)
  ggsave(paste0("figures/interpolation/","iqr_",plot_names[i],".png"),
         plot = prodigy_plots_error_bars_groups_interp[[sheets[i]]],
         width = 16, height = 12, units = "cm", dpi = 300)
  ggsave(paste0("figures/regression/","iqr_",plot_names[i],".png"),
         plot = prodigy_plots_error_bars_groups_imputed[[sheets[i]]],
         width = 16, height = 12, units = "cm", dpi = 300)
}
