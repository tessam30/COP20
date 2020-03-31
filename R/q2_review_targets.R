##  PROJECT: Q2 review target analysis
##  AUTHOR:  jdavis | USAID
##  PURPOSE: munge historic targets to fy21 targets, viz
##  LICENCE: MIT
##  DATE:    2020-03-30
##  UPDATE:

#Dependancies----------------------------------------------------------
library(tidyverse)
library(ICPIutilities)
library(tameDP)
#install.packages("janitor")
library(janitor)
library(ggplot2)
library(skimr)
library(scales)
library(extrafont)
library(patchwork)

indc <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "OVC_SERV", "KP_PREV")

#folders---------------------------------------------------------------
data_in <- "Data"
data_out <- "Dataout"
viz_folder <- "Images"

#create cop20 targets--------------------------------------------------

files <- list.files(dps, full.names = TRUE)

#read in all DPs and combine into one data frame
df_all <- map_dfr(.x = files,
                  .f = ~ tame_dp(.x))




# GLOBALS -----------------------------------------------------------------

  # Collapse and summarize function that can be called at beginning to
  # group by a dfined 
 sum_targets <- function(df, group) {
     df %>%
     group_by_at({{group}}) %>% 
     summarise(targets = sum(targets, na.rm = TRUE)) %>% 
     ungroup() %>% 
     filter(targets != 0)
  }


  # Print all rows
  prinf <- function(df) {
      print(df, n = Inf)
  }

  # Grouping call
  grp_ou <- c("operatingunit", "fiscal_year", "indicator", "fundingagency")
  grp_ou_agency <- c("fiscal_year", "indicator", "agency_other") 


#munge historic targets------------------------------------------------

  df_15_16 <- readRDS(file.path(data_in, "MER_Structured_Datasets_OU_IM_FY15-17_20200320_v2_1.rds"))
  
  df_15_16_sum <- 
    df_15_16 %>% 
    filter(standardizeddisaggregate == "Total Numerator") %>% 
    sum_targets(., group = grp_ou)


#read_msd("C:/Users/Josh/Documents/data/fy20_q1_v1/MER_Structured_Datasets_OU_IM_FY18-20_20200320_v2_1.zip")

  df_curr <- readRDS(file.path(data_in, "MER_Structured_Datasets_OU_IM_FY18-20_20200320_v2_1.rds")) 
  
  df_curr_sum <- 
    df_curr %>% 
    filter(standardizeddisaggregate == "Total Numerator") %>% 
    sum_targets(., grp_ou)
  

  # Combine together for making beautifu plots
  df_mer <-
    bind_rows(df_curr_sum, df_15_16_sum) %>%
    mutate(agency_other = case_when(fundingagency == "USAID" ~ "USAID",
                                  fundingagency == "HHS/CDC" ~ "CDC",
                                  TRUE ~ "Other"),
         agency_other = factor(agency_other, c("USAID", "CDC", "Other")))

  
  # What OUs are missing targets?
  # Few targets in 2015 so may want to drop that out
  df_mer %>% filter(indicator == "TX_CURR",
                     fundingagency %in% c("USAID", "HHS/CDC")) %>% 
    distinct(agency_other, fiscal_year, targets, operatingunit) %>% 
    spread(fiscal_year, targets) %>% 
    prinf()

  # What indicators are missing targets?
  df_mer %>% 
    group_by(fiscal_year, indicator, agency_other) %>% 
    summarise(targets = sum(targets)) %>% 
    spread(fiscal_year, targets) %>% 
    prinf()

#Viz munge-----------------------------------------------------------

  # Create two types of shares:
  # Share 1: What are the total targets for the Fiscal year, across all agencies
  # Share 2: What are the total targets across the Fiscal year, within agences (OU shares)
  # To make it easier, we'll create two data frames versus carry constants around at OU level

  
  
  df_viz_agency <- 
    df_mer %>% 
    filter(indicator %in% indc, 
      fiscal_year != 2015) %>% 
    sum_targets(., group = grp_ou_agency) %>% 
    group_by(indicator, fiscal_year) %>% 
    mutate(tot_targets = sum(targets, na.rm = TRUE),
      agency_share = targets / tot_targets,
      ymax = cumsum(agency_share)) %>% 
  ungroup() %>% 
    mutate(agency_label = if_else(agency_other == "USAID" & fiscal_year %in% c(2016, 2020), 
      round(agency_share, 2), NA_real_))
    
    
    
  df_viz_ou <- 
    df_mer %>% 
    filter(indicator %in% indc,
      fiscal_year != 2015) %>% 
    group_by(indicator, fiscal_year) %>% 
    mutate(tot_targets = sum(targets, na.rm = TRUE),
      ou_share = targets / tot_targets) %>% 
    ungroup()


#viz-----------------------------------------------------------------
  
  # Basic area plots of shares across time by indicator, sorting on those with
  # the most targets first

  
  # Where did things grow and shrink?
  df_viz_agency %>% 
    select(agency_other, fiscal_year, agency_share, indicator) %>% 
    spread(fiscal_year, agency_share) %>% 
    mutate(diff = `2020` - `2016`,
      change_label = if_else(diff > 0, "grow", "shrink"))
  
    
  # Funcationalize ggplot
  area_share_plot <- function(df, share_var) {
    df %>% 
      group_by(indicator) %>% 
      mutate(agency_order = fct_reorder(agency_other, {{share_var}})) %>% 
      ungroup() %>% 
      ggplot(aes(x = fiscal_year, y = {{share_var}}, fill = agency_order, group = agency_order)) +
      geom_area() + 
      #geom_text(aes(label = scales::percent(agency_label), fill = agency_order),
      #position = position_stack()) +
      facet_wrap(~indicator, nrow = 2) +
      theme_minimal() +
      si_style() +
      theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank()) +
      scale_y_continuous(label = percent_format(accuracy = 1)) +
      scale_fill_manual(values = c("CDC" = '#a6bddb', "USAID" = "#045a8d", "Other" = "#E8E8E8"))
  }
  
  # Agency shares
  area_share_plot(df_viz_agency, agency_share) +
    labs(x = NULL, y = NULL,
      fill = "Agency share",
      title = "USAID'S SHARE OF TESTING TARGETS HAS GROWN BY 5 PERCENTAGE POINTS FROM FY16 - FY20",
      subtitle = "USAID'S SHARE OF KP_PREV TARGETS SHRANK THE MOST, FROM ABOUT 73% TO 51%") 
  
  ggsave(file.path(viz_folder, "Q1Review_target_share.png"),
    plot = agency_plot, dpi = 330, width = 10, height = 5.66)
  
  
  # See which ou's grew/shrank the most - this is based on the share across all agencies
  df_viz_ou %>%
    filter(agency_other == "USAID") %>% 
    select(operatingunit, agency_other, fiscal_year, ou_share, indicator) %>% 
    mutate(ou_share = round(ou_share, 3)) %>% 
    spread(fiscal_year, ou_share) %>% 
    mutate(diff = `2020` - `2016`,
      change_label = if_else(diff > 0, "grow", "shrink")) %>% 
    arrange(diff) %>% 
    prinf()
  
  
  df_viz_ou %>% 
    filter(operatingunit == "Nigeria") %>% 
    area_share_plot(., ou_share)
  
  
  
  

# Compute the cumulative percentages (top of each rectangle)
  df_viz_agency$ymax <- cumsum(df_viz_agency$agency_share)

# Compute the bottom of each rectangle
  df_viz_gl$ymin <- c(0, head(df_viz_gl$ymax, n=-1))

# Compute label position
  df_viz_gl$labelPosition <- (df_viz_gl$ymax + df_viz_gl$ymin) / 2

# Compute a good label
  df_viz_gl$label <- paste0(df_viz_gl$indicator, "\n value: ", df_viz_gl$share_gl)


  df_viz_agency %>% 
    filter(indicator == "HTS_TST") %>% 
    ggplot(aes(y = ))
  
  
  
df_viz_gl %>% 
  filter(indicator == "HTS_TST") %>% 
ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=share_gl)) +
  geom_rect() +
  facet_grid(. ~ fiscal_year) +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(legend.position = "none")

## viz part two
df_viz %>% filter(indicator == "HTS_TST") %>% 
ggplot(aes(x=fiscal_year, y=agency_indic_share, fill=agency_other)) + 
  geom_area(alpha=0.6 , size=0.5, colour="black")










