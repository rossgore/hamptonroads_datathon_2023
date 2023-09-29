###################################################
# Libraries
###################################################

library(shiny)
library(leaflet)
library(sf)
library(here)
library(tidyverse)
library(paletteer)
library(gt)
library(psych)
library(ggcorrplot)

###################################################
# Shape files
###################################################

# Read in shapefiles and convert to large spatial polygon dataframe
norfolk_tract <- st_read(here::here("data", "shp", "norfolk_tract.shp"), quiet = TRUE)
norfolk_tract_sp <- sf:::as_Spatial(norfolk_tract)

vector_of_cts_in_map = norfolk_tract_sp@data$GEOID %>% unique() %>% as.character()

###################################################
# Color Scheme
###################################################

light_blue_color <- "#5DA3B2"
magenta_color <- "#8B008B"
dark_blue_color <- "#174A7E"
dark_gray_color <- "#636466"
light_gray_color <- "#C7C8CA"


###################################################
# CSVs
###################################################

places_data_col_types = cols(
  census_tract = col_double(),
  Measure = col_character(),
  health_outcome_value = col_double(),
  TotalPopulation = col_integer(),
  Geolocation = col_character(),
  CategoryID = col_factor(),
  health_outcome_label = col_character()
)


acs_data_col_types = cols(
  .default = col_double()
)

acs_data = read_csv("data/acs_data/census_tract_demos.csv", col_types = acs_data_col_types)
acs_data = acs_data %>% mutate(census_tract = as.character(census_tract))

norva_places_data = read_csv("data/norva_places/norfolk_places_data.csv", col_types = places_data_col_types)
norva_places_data = norva_places_data %>% mutate(census_tract = as.character(census_tract))


food_choice = read.csv("data/correlation_work/wide_format/health_food_choice_find_correlations_wide.csv")
habit_routine = read.csv("data/correlation_work/wide_format//habit_and_routine_find_correlations_wide.csv")
med_selfcare = read.csv("data/correlation_work/wide_format/med_self_care_find_correlations_wide.csv")
sleep_rest = read.csv("data/correlation_work/wide_format/sleep_and_rest_find_correlations_wide.csv")
stress_mgmt = read.csv("data/correlation_work/wide_format/stress_mgmt_find_correlations_wide.csv")
phy_activity = read.csv("data/correlation_work/wide_format/physical_activity_find_correlations_wide.csv")

peggy_swarbrick_tbl = read_csv("data/places_tbl.csv")
norva_places_data = norva_places_data %>% inner_join(peggy_swarbrick_tbl)
norva_places_data = norva_places_data %>% filter(physical_category != "OUTCOME")
norva_places_data = norva_places_data %>% filter(census_tract %in% vector_of_cts_in_map)
unique_physical_category_choices = norva_places_data$physical_category %>% unique()
################# Globals ###################################

subtitle_methodology = "/Learn More about [Methodology](https://www.cdc.gov/places/methodology/index.html) and [Validation.](https://www.cdc.gov/pcd/issues/2017/17_0281.htm)"

physical_category_choices = c("Medical Self-Care & Screenings" = unique_physical_category_choices[1],
                              "Habits and Routines" = unique_physical_category_choices[2],
                              "Healthy Food Choices" = unique_physical_category_choices[3],
                              "Physical Activity" = unique_physical_category_choices[4],
                              "Sleep and Rest" = unique_physical_category_choices[5],
                              "Stress Management & Relaxation" = unique_physical_category_choices[6])

default_physical_category_choice = physical_category_choices[1]

far_below_avg_label = "Far Below Avg."
below_avg_label = "Below Avg."
avg_label = "About Avg."
above_avg_label = "Above Avg."
far_above_avg_label = "Far Above Avg."

prct_symbol = "%"

note_about_corr_matrix = "<h5>NOTE: For clarity only Pearson Correlations <b><u>greater than 0.8</u></b> or <b><u>less than -0.8</u></b> are shown in the pictured matrix.</h5>"

vector_of_labels_to_flip = c("ACCESS2", "CSMOKING", "GHLTH", "LPA", "PHLTH", "BINGE", "SELFCARE", "SLEEP", "MHLTH", "INDEPLIVE")

vector_of_flipped_measures = c("Adults with health insurance aged 18-64 years",
                               "Currently not smoking among adults aged >=18 years",
                               "Good or better self-rated health status among adults aged >=18 years",
                               "Participate in leisure-time physical activity among adults aged >=18 years",
                               "Physical health  good for >360 days a year among adults aged >=18 years",
                               "Do not binge drink among adults aged >=18 years",
                               "Able to dress, bathe & get around by oneself adults aged >=18 years",
                               "Sleep at least 7 hours a night among adults aged >=18 years",
                               "Mental health good or better for >360 days a year among adults aged >=18 years",
                               "Able to live independently among adults aged >=18 years")


########## DATA WRANGLING OPS ################################

# break norva down into pieces
norva_places_data_that_need_to_be_flipped = norva_places_data %>% filter(health_outcome_label %in% vector_of_labels_to_flip)
norva_places_data_that_need_to_be_flipped = norva_places_data_that_need_to_be_flipped %>% mutate(health_outcome_value = 100 - health_outcome_value)
norva_places_data_that_need_to_be_flipped_ACCESS2 = norva_places_data_that_need_to_be_flipped %>% filter(health_outcome_label == vector_of_labels_to_flip[1]) %>% mutate(Measure = vector_of_flipped_measures[1])
norva_places_data_that_need_to_be_flipped_CSMOKING = norva_places_data_that_need_to_be_flipped %>% filter(health_outcome_label == vector_of_labels_to_flip[2]) %>% mutate(Measure = vector_of_flipped_measures[2])
norva_places_data_that_need_to_be_flipped_GHLTH = norva_places_data_that_need_to_be_flipped %>% filter(health_outcome_label == vector_of_labels_to_flip[3]) %>% mutate(Measure = vector_of_flipped_measures[3])
norva_places_data_that_need_to_be_flipped_LPA = norva_places_data_that_need_to_be_flipped %>% filter(health_outcome_label == vector_of_labels_to_flip[4]) %>% mutate(Measure = vector_of_flipped_measures[4])
norva_places_data_that_need_to_be_flipped_PHLTH = norva_places_data_that_need_to_be_flipped %>% filter(health_outcome_label == vector_of_labels_to_flip[5]) %>% mutate(Measure = vector_of_flipped_measures[5])
norva_places_data_that_need_to_be_flipped_BINGE = norva_places_data_that_need_to_be_flipped %>% filter(health_outcome_label == vector_of_labels_to_flip[6]) %>% mutate(Measure = vector_of_flipped_measures[6])
norva_places_data_that_need_to_be_flipped_SELFCARE = norva_places_data_that_need_to_be_flipped %>% filter(health_outcome_label == vector_of_labels_to_flip[7]) %>% mutate(Measure = vector_of_flipped_measures[7])
norva_places_data_that_need_to_be_flipped_SLEEP = norva_places_data_that_need_to_be_flipped %>% filter(health_outcome_label == vector_of_labels_to_flip[8]) %>% mutate(Measure = vector_of_flipped_measures[8])
norva_places_data_that_need_to_be_flipped_MHLTH = norva_places_data_that_need_to_be_flipped %>% filter(health_outcome_label == vector_of_labels_to_flip[9]) %>% mutate(Measure = vector_of_flipped_measures[9])
norva_places_data_that_need_to_be_flipped_INDEPLIVE = norva_places_data_that_need_to_be_flipped %>% filter(health_outcome_label == vector_of_labels_to_flip[10]) %>% mutate(Measure = vector_of_flipped_measures[10])

norva_places_data_that_do_not_need_to_be_flipped = norva_places_data %>% filter(!health_outcome_label %in% vector_of_labels_to_flip)

norva_places_data_that_need_to_be_flipped = norva_places_data_that_need_to_be_flipped_ACCESS2 %>% bind_rows(norva_places_data_that_need_to_be_flipped_CSMOKING,
                                                                                                            norva_places_data_that_need_to_be_flipped_GHLTH,
                                                                                                            norva_places_data_that_need_to_be_flipped_LPA,
                                                                                                            norva_places_data_that_need_to_be_flipped_PHLTH,
                                                                                                            norva_places_data_that_need_to_be_flipped_BINGE,
                                                                                                            norva_places_data_that_need_to_be_flipped_SELFCARE,
                                                                                                            norva_places_data_that_need_to_be_flipped_SLEEP,
                                                                                                            norva_places_data_that_need_to_be_flipped_MHLTH,
                                                                                                            norva_places_data_that_need_to_be_flipped_INDEPLIVE)

norva_places_data = norva_places_data_that_do_not_need_to_be_flipped %>% bind_rows(norva_places_data_that_need_to_be_flipped)

# 100 - minus for all flipped pieces

# 

###################### FUNCTIONS ###########################

formatDecimal <- function(x, k) format(round(x, k), trim=T, nsmall=k)


append_numeric_suffix <- function(numbers){
  
  suff <- case_when(numbers %in% c(11,12,13) ~ "th",
                    numbers %% 10 == 1 ~ 'st',
                    numbers %% 10 == 2 ~ 'nd',
                    numbers %% 10 == 3 ~'rd',
                    TRUE ~ "th")
  to_return = paste0(numbers, suff)
  return(to_return)
}

get_long_form_description = function(short_desc, long_desc)
{
  one_row_df = norva_places_data %>% filter(health_outcome_label == short_desc) %>% unique()
  to_return = one_row_df$Measure[1]
  return(to_return)
}

get_pretty_print_of_physical_category = function(physical_category_id)
{
  pretty_print_physical_category_index = physical_category_id %>% match(physical_category_choices)
  pretty_print_physical_category_index = pretty_print_physical_category_index[1]
  pretty_print_physical_categories = physical_category_choices %>% names()
  pretty_print_physical_category = pretty_print_physical_categories[pretty_print_physical_category_index]
  return(pretty_print_physical_category)
}


gtify_ct_overview_results = function(df, physical_category, selected_ct, census_tract_population)
{
  tbl_to_return = get_place_holder_table()
  if (df %>% is.null() == FALSE)
  {
    if (df %>% nrow() > 0)
    {
      pretty_print_physical_category = physical_category %>% get_pretty_print_of_physical_category()
      df = df %>% group_by(Health_Measure)
      tbl_to_return = df %>% gt(rowname_col = "Measure") %>%
        tab_header(
          title = md(paste0(pretty_print_physical_category, " Related data estimate(s) % for ",census_tract_population," residents of **",selected_ct,"**")),
          subtitle = md("Data comes from August 25, 2023 release of [CDC Places Project.](https://data.cdc.gov/500-Cities-Places/PLACES-Local-Data-for-Better-Health-Census-Tract-D/cwsq-ngmh) Learn More about the CDC Places Project [Methodology](https://www.cdc.gov/places/methodology/index.html) and [Validation.](https://www.cdc.gov/pcd/issues/2017/17_0281.htm)")
        ) %>%
        tab_options(
          row_group.background.color = "#FFEFDB80",
          heading.background.color = "#EFFBFC",
          column_labels.background.color = "#EFFBFC",
          stub.background.color = "#EFFBFC",
          table.font.color = "#323232",
          table_body.hlines.color = "#989898",
          table_body.border.top.color = "#989898",
          heading.border.bottom.color = "#989898",
          row_group.border.top.color = "#989898",
          row_group.border.bottom.style = "none",
          stub.border.style = "dashed",
          stub.border.color = "#989898",
          stub.border.width = "1px",
          summary_row.border.color = "#989898",
        ) %>%
        opt_all_caps()  %>%
        opt_align_table_header(align = "left") %>% fmt_markdown(columns = everything())
      tbl_to_return = tbl_to_return %>% cols_align(align = "right", columns = value)
    }
  }
  return (tbl_to_return)
}

mult_by_100_and_round = function(x)
{
  x = x * 100
  x = x %>% round(2)
  return(x)
}


get_place_holder_table = function()
{
  
  to_return = tibble(demographic_type = character(), value=double()) %>% 
    gt() %>%
    tab_options(
      summary_row.background.color = "#ACEACE80",
      grand_summary_row.background.color = "#990000",
      row_group.background.color = "#FFEFDB80",
      heading.background.color = "#EFFBFC",
      column_labels.background.color = "#EFFBFC",
      stub.background.color = "#EFFBFC",
      table.font.color = "#323232",
      table_body.hlines.color = "#989898",
      table_body.border.top.color = "#989898",
      heading.border.bottom.color = "#989898",
      row_group.border.top.color = "#989898",
      row_group.border.bottom.style = "none",
      stub.border.style = "dashed",
      stub.border.color = "#989898",
      stub.border.width = "1px",
      summary_row.border.color = "#989898",
      table.width = "60%"
    ) %>%
    opt_all_caps()
  
  return(to_return)
}

pctl = function(vector, value){
  
  out = sum(value >= vector)/ length(vector)
  
  return(out)
  
}



get_harmonic_mean = function(vector_of_nums)
{
  to_return = harmonic.mean(vector_of_nums) * 100
  to_return = to_return %>% round(0)
  if (to_return %>% is.nan())
  {
    to_return = NA
  }
  return(to_return)
}

get_score = function(values, labels)
{
  if (values %>% is.null())
  {
    return(NA)
  }
  if (values %>% length() == 0)
  {
    return(NA)
  }
  to_return = c()
  for (i in 1:length(labels))
  {
    this_label_data_for_all_cts = norva_places_data %>% filter(health_outcome_label == labels[i])
    values_for_this_label = this_label_data_for_all_cts$health_outcome_value
    this_percentile = pctl(values_for_this_label, values[i])
    this_percentile = (this_percentile * 100) %>% round(0)
    to_return = c(to_return, this_percentile)
  }
  return(to_return)
}

get_score_category = function(score)
{
  to_return = NA
  if (score %>% is.na())
  {
    to_return = NA
  }
  else if (score < 20)
  {
    to_return =far_below_avg_label
  } else if (score >= 20 & score <= 40)
  {
    to_return =below_avg_label
  } else if (score >  40 & score <= 60)
  {
    to_return = avg_label
  } else if (score >  60 & score <= 80)
  {
    to_return = above_avg_label
  } else if (score >  80)
  {
    to_return = far_above_avg_label
  }
  return (to_return)
}

get_summary_tbl = function(physical_category, labels, health_values, health_scores)
{
  summary_tibble = tibble(Health_Measure = character(), Measure_Type = character(), value=character())
  if (labels %>% is.null())
  {
    return(summary_tibble)
  }
  if (labels %>% length() == 0)
  {
    return(summary_tibble)
  }
  for (i in 1:length(labels))
  {
    this_label = get_long_form_description(labels[i])
    this_measure_type = "Percentage Of Residents In Census Tract"
    this_value = paste0(health_values[i] %>% formatDecimal(2), prct_symbol)
    summary_tibble = summary_tibble %>% add_row(Health_Measure = this_label, Measure_Type = this_measure_type, value = this_value)
  }
  for (i in 1:length(labels))
  {
    this_label = get_long_form_description(labels[i])
    this_measure_type = "Percentile Of Census Tract For This Health Measure In Norfolk"
    this_value = health_scores[i] %>% append_numeric_suffix()
    summary_tibble = summary_tibble %>% add_row(Health_Measure = this_label, Measure_Type = this_measure_type, value = this_value)
  }
  summary_tibble = summary_tibble %>% arrange(Health_Measure)
  
  ### this goes at bottom
  pretty_print_physical_category = physical_category %>% get_pretty_print_of_physical_category()
  overall_domain_value = mean(health_scores)
  overall_domain_score = get_score_category(overall_domain_value)
  overall_domain_value = paste(overall_domain_value %>% round(0), "/ 100")
  
  
  summary_tibble = summary_tibble %>% add_row(Health_Measure = paste("Overall", pretty_print_physical_category, "Score"), 
                                              Measure_Type = "Mean Percentile of Variables", value = paste0(overall_domain_value," (",overall_domain_score,")"))

  return (summary_tibble)
}

get_medical_self_care_plot = function()
{
  MS <- subset(med_selfcare, select = -c(census_tract, DEPRESSION, STROKE, HIGHCHOL, HEARING, CANCER, CHOLSCREEN, ARTHRITIS, CHD, COREW, COGNITION, CASTHMA, KIDNEY, COPD, BPHIGH, MOBILITY, MAMMOUSE, BPMED, DIABETES, CERVICAL, TEETHLOST))
  
  #pearson
  med_selfcare_corr_P <- as.data.frame(cor(MS, method="pearson"))
  med_selfcare_corr_P <- med_selfcare_corr_P %>% mutate_all( ~ifelse(abs(.) < 0.8, 0, .))
  
  to_return = ggcorrplot(med_selfcare_corr_P, hc.order = TRUE, type = "lower", insig = "blank")
  return(to_return)
}

get_habit_and_routine_plot = function()
{
  HR <- subset(habit_routine, select = -c(census_tract, DEPRESSION, ARTHRITIS, CANCER, HIGHCHOL, HEARING, COGNITION, STROKE, CASTHMA, CHD, KIDNEY, MOBILITY, COPD))
  
  #pearson
  habit_routine_corr_P <- as.data.frame(cor(HR, method="pearson"))
  habit_routine_corr_P <- habit_routine_corr_P %>% mutate_all( ~ifelse(abs(.) < 0.8, 0, .))
  
  to_return = ggcorrplot(habit_routine_corr_P, hc.order = TRUE, type = "lower", insig = "blank")
  return(to_return)
}

get_healthy_food_choice_plot = function()
{
  FC <- subset(food_choice, select = -c(census_tract, STROKE, ARTHRITIS, DEPRESSION, HEARING, CANCER, HIGHCHOL, COGNITION, KIDNEY, CHD, BPHIGH, OBESITY, CASTHMA))
  
  #pearson
  food_choice_corr_P <- as.data.frame(cor(FC, method="pearson"))
  food_choice_corr_P <- food_choice_corr_P %>% mutate_all( ~ifelse(abs(.) < 0.8, 0, .))
  to_return = ggcorrplot(food_choice_corr_P, hc.order = TRUE, type = "lower", insig = "blank")
  return(to_return)
}

get_physical_activity_plot = function()
{
  PA <- subset(phy_activity, select = -c(census_tract, ARTHRITIS, DEPRESSION, CANCER, STROKE, HEARING, HIGHCHOL, COGNITION, KIDNEY, CHD, CASTHMA, BPHIGH, OBESITY, TEETHLOST, VISION))
  
  #pearson
  phy_activity_corr_P <- as.data.frame(cor(PA, method="pearson"))
  phy_activity_corr_P <- phy_activity_corr_P %>% mutate_all( ~ifelse(abs(.) < 0.8, 0, .))
  
  to_return = ggcorrplot(phy_activity_corr_P, hc.order = TRUE, type = "lower", insig = "blank")
  return(to_return)
}

get_sleep_and_rest_plot = function()
{
  SR <- subset(sleep_rest, select = -c(census_tract, HEARING, DEPRESSION, ARTHRITIS, COGNITION, CHD, HIGHCHOL, CANCER, KIDNEY, BPHIGH, OBESITY, CASTHMA, COPD, STROKE, MOBILITY))
  
  #pearson 
  sleep_rest_corr_P <- as.data.frame(cor(SR, method="pearson"))
  sleep_rest_corr_P <- sleep_rest_corr_P %>% mutate_all( ~ifelse(abs(.) < 0.8, 0, .))
  
  to_return = ggcorrplot(sleep_rest_corr_P, hc.order = TRUE, type = "lower", insig = "blank")
  
  return(to_return)
}
get_stress_mgmt_and_relax_plot = function()
{
  SM <- subset(stress_mgmt, select = -c(census_tract, COGNITION, CASTHMA, DEPRESSION, HIGHCHOL, CANCER, HEARING, ARTHRITIS, CHD, KIDNEY, BPHIGH, OBESITY))
  
  #pearson
  stress_mgmt_corr_P <- as.data.frame(cor(SM, method="pearson"))
  stress_mgmt_corr_P <- stress_mgmt_corr_P %>% mutate_all( ~ifelse(abs(.) < 0.8, 0, .))
  
  to_return = ggcorrplot(stress_mgmt_corr_P, hc.order = TRUE, type = "lower", insig = "blank")
  return(to_return)
}

get_plot_for_domain = function(physical_category, labels, health_values, health_scores)
{
  to_return = iris %>% ggplot(aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(colour = "black") +ggtitle("Unidentified")
  if (physical_category == "MEDICAL_SELF_CARE")
  {
    to_return = get_medical_self_care_plot()
  }
  if (physical_category == "HABIT_AND_ROUTINE")
  {
    to_return = get_habit_and_routine_plot()
  }
  if (physical_category == "HEALTHY_FOOD_CHOICE")
  {
    to_return = get_healthy_food_choice_plot()
  }
  if (physical_category == "PHYSICAL_ACTIVITY")
  {
    to_return = get_physical_activity_plot()
  }
  if (physical_category == "SLEEP_AND_REST")
  {
    to_return = get_sleep_and_rest_plot()
  }
  if (physical_category == "STRESS_MGMT_AND_RELAX")
  {
    to_return = get_stress_mgmt_and_relax_plot()
  }
  return(to_return)
}

get_medical_self_care_text = function()
{
  
  header = "<h2>Medical Self-Care & Screenings Recommendations</h2>"
  prompt_of_rec = "<h5>TThe medical and selfcare matrix shows high correlations between having access to health insurance and doing important routine checks (i.e. colon screenings, flu shots, cancer screenings, etc.) and low likelihood of having:</h5>" 
  points_of_rec = "<ol><li>Not Having Vision Disabilities</li><li>Not Being Obese</li><li>Not Self-Reporting To Have a Disability</li></ol>"
  to_return = paste(header, prompt_of_rec, points_of_rec, note_about_corr_matrix)
  return(HTML(to_return))
}

get_habit_and_routine_text = function()
{

  header = "<h2>Habits and Routines Recommendations</h2>"
  prompt_of_rec = "<h5>The habits and routine matrix, shows correlation between a healthy lifestyle/routines (i.e., going to the dentist, self-reporting that you feel in good health), and overall wellbeing, such as:</h5>" 
  points_of_rec = "<ol><li>Not Having Vision Disabilities</li><li>Not Having Issues With Teeth Loss</li><li>Not Self-Reporting To Have a Disability</li></ol>"
  to_return = paste(header, prompt_of_rec, points_of_rec, note_about_corr_matrix)
  return(HTML(to_return))
}

get_healthy_food_choice_text = function()
{
  header = "<h2>Healthy Food Choices Recommendations</h2>"
  prompt_of_rec = "<h5>The healthy food choice matrix, highlights that there is a high correlation between being a nonsmoker and wellbeing, such as:</h5>" 
  points_of_rec = "<ol><li>Not Having Vision Disabilities</li><li>Not Having Chronic Obstructive Pulmonary Disease (COPD)</li><li>Maintaining Mobility</li></ol>"
  to_return = paste(header, prompt_of_rec, points_of_rec, note_about_corr_matrix)
  return(HTML(to_return))
}

get_physical_activity_text = function()
{
  header = "<h2>Physical Activity Recommendations</h2>"
  prompt_of_rec = "<h5>The physical activity matrix highlight how physical health and leisure-time physical activities are correlated to the low incidence of having health issues related to:</h5>" 
  points_of_rec = "<ol><li>Diabetes</li><li>Mobility</li><li>Chronic Obstructive Pulmonary Disease (COPD)</li></ol>"
  to_return = paste(header, prompt_of_rec, points_of_rec, note_about_corr_matrix)
  return(HTML(to_return))
}

get_sleep_and_rest_text = function()
{
  header = "<h2>Sleep and Rest Recommendations</h2>"
  br = "<br>"
  prompt_of_rec = "<h5>The sleep and rest matrix shows that sleeping at least 7 hours is highly correlated to reducing the incidence of:</h5>" 
  points_of_rec = "<ol><li>Diabetes</li><li>Self-Reporting To Have a Disability</li><li>Teeth Loss</li></ol>"
  to_return = paste(header, prompt_of_rec, points_of_rec, note_about_corr_matrix)
  return(HTML(to_return))
}
get_stress_mgmt_and_relax_text = function()
{
  header = "<h2>Stress Management & Relaxation Recommendations</h2>"
  br = "<br>"
  prompt_of_rec = "<h5>The stress management matrix shows that good mental health is correlated to low levels of:</h5>" 
  points_of_rec = "<ol><li>Vision Disabilities</li><li>Self-Reporting To Have a Disability</li><li>Teeth Loss</li></ol>"
  final_line = "<h5>Being able to live independently also reduces the incidence rate of a myriad of negative health outcomes.</h5?"
  to_return = paste(header, prompt_of_rec, points_of_rec, final_line, note_about_corr_matrix)
  return(HTML(to_return))
}

get_text_for_domain = function(physical_category, labels, health_values, health_scores)
{
  to_return = "unidentified text recs"
  if (physical_category == "MEDICAL_SELF_CARE")
  {
    to_return = get_medical_self_care_text()
  }
  if (physical_category == "HABIT_AND_ROUTINE")
  {
    to_return = get_habit_and_routine_text()
  }
  if (physical_category == "HEALTHY_FOOD_CHOICE")
  {
    to_return = get_healthy_food_choice_text()
  }
  if (physical_category == "PHYSICAL_ACTIVITY")
  {
    to_return = get_physical_activity_text()
  }
  if (physical_category == "SLEEP_AND_REST")
  {
    to_return = get_sleep_and_rest_text()
  }
  if (physical_category == "STRESS_MGMT_AND_RELAX")
  {
    to_return = get_stress_mgmt_and_relax_text()
  }
  return(to_return)
}

