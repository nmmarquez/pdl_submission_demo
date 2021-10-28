rm(list = ls())
# for libraries which are not in either cran or biocunductor
# please include a line on how installation may be achieved
# remotes::install_github("nmmarquez/smoothHP")
library(smoothHP)
library(dplyr)
library(tidyr)

# source other files which should be placed in the R folder
# we will assume that the project folder is the root directory and all files
# should be read relative to it
source("./R/anlyz_groups.R")

# same thing with data
kc_sub_data <- fread("./Data/kc_sub_data.csv") %>%
    mutate(Age5 = as.factor(Age5))

# everything else is just about running your code to create output that should
# be in your submission the more comments in you r code the better so
# other readers can get an idea of whats happening. In general try to make the
# main portion of your code ~100 lines long with additional code being placed
# in the R subfolder for sourcing.

# make a set of projections using no smoothing
unsmooth_DF <- kc_sub_data  %>%
    multi_stage_group_HP_project(stages = list(c("County", "Race")))

# now using smoothing
smooth_DF <- kc_sub_data  %>%
    multi_stage_group_HP_project(stages = list("County", "Race"))

# aggregate up the original data
raceth_DF <- kc_pop_data[
    ,.(value = sum(value)), by = list(Year, Sex, Race, Age5, County)]

# attach names to each data set for ggplot
raceth_DF[,Type:="Data"]
smooth_DF[,Type:="Smoothed"]
unsmooth_DF[,Type:="HP Method"]

# make a plot comparing the data to the two projection methods
bind_rows(raceth_DF, smooth_DF, unsmooth_DF) %>%
    group_by(Year, Race, Type) %>%
    summarize(Population = sum(value) / 1000000) %>%
    ggplot(aes(x = Year, y = Population, color = Race, linetype = Type)) +
    geom_line() +
    theme_classic() +
    labs(
        y = "Population Estimate (in millions)",
        color = "Racial and Ethnic Categories") +
    ggtitle(
        "Population Forecasts by Race and Ethnicity",
        "Comparison of Smoothing Approaches")

# now we want to show the effects of smoothing on CCR
# recall a CCR value of 1 means the population size remains the same, 2 means
# the population doubled, and 1.5 means the population was halved.
smooth_results <- bind_rows(
    # again show HP method
    multi_stage_CCR_estimates(
        kc_pop_data[kc_pop_data$Year %in% c(2010,2015)],
        stages = list(c("County", "Race", "GEOID"))) %>%
        mutate(Method = "HP Method"),
    # a single stage smoothing which wil over smooth
    multi_stage_CCR_estimates(
        kc_pop_data[kc_pop_data$Year %in% c(2010,2015)],
        stages = list("County", c("Race", "GEOID"))) %>%
        mutate(Method = "Single Stage\nSmoothing"),
    # and two stage smoothing
    multi_stage_CCR_estimates(
        kc_pop_data[kc_pop_data$Year %in% c(2010,2015)],
        stages = list("County", "Race", "GEOID")) %>%
        mutate(Method = "Multi Stage\nSmoothing"))

# plot the distribution of the results for a single age groups CCR across all
# census tracts in King County
smooth_results %>%
    filter(Sex == "Male" & Race %in% anlyz_groups & Age5 == "25-29") %>%
    filter(is.finite(CCR)) %>%
    ggplot(aes(x = CCR, fill = Race, color = Race)) +
    geom_density(alpha = .4) +
    facet_wrap(~Method, scales = "free_y") +
    theme_classic() +
    xlim(c(1, 2.4)) +
    labs(x = "Cohort Change Ratio", y = "Density") +
    ggtitle(
        "Distribution of Male CCR Estimates by Race",
        "Comparison of Smoothing Approaches")
