# NCVS Code

# Required packages:
#install.packages("tidyverse","haven","gt","gtExtras","forcats", "wbstats","caret")
#remotes::install_github('lukaswallrich/timesaveR')

library(tidyverse)
library(haven)
library(gt)
library(gtExtras)
library(forcats)

# Data import-------------------------------------------------------------------

## DATASET TOO LARGE FOR SHARING, trimmed data used for github

# orginial trimming
# all_ncvs <- read_sav("./Data/ncvs/39273-0003-Data.sav")
# ncvs <- all_ncvs|>
#  select("month" = V3025, "day" = V3026, "year" = V3027, "age" = V3014, 
#         "marital_status" = V3015, "sex" = V3018, "education" = V3020,
#         "race" = V3023, "raceB" = V3023A, "type_of_crime" = V4529,
#         "unwanted_sex" = V3046, "unwanted_sex_times" = V3047,"job" = V3072,
#         "job_type" = V3074, "incident_month" = V4014,"incident_year" = V4015,
#         "location" = V4024, "distance_from_home" = V4043, "weapon" = V4049,
#        "activity" = V4478, "offender_sex" = V4236, "offender_age" = V4237,
#         "offender_impaired" = V4239, "drinking_drugs" = V4240,
#         "stranger" = V4241, "closeness" =  V4243, "relationship" = V4245,
#         "number_of_offenders" = V4248, "reported" = V4399,
#         "how_reported" = V4400, "reason_not_rptd" = V4422,
#         "reason_rptd" = V4437, "arrests" = V4466, "weight" = V4529)
# library(data.table)
# fwrite(ncvs, "Data\\ncvs\\ncvs.csv") #saving trimmed data

# trimmed data
ncvs <- read.csv("../data/ncvs/ncvs.csv")

## Cleaning----

# remotes::install_github('lukaswallrich/timesaveR')
library(timesaveR)
# combine race variable
ncvs <- ncvs|>
  mutate(raceB = 
           case_when(raceB %in% 1 ~ "White",
                      raceB %in% 2 ~ "Black",
                      raceB %in% 3 ~ "American Indian/Alaska Native",
                      raceB %in% 4:5 ~ "Asian/Pacific Islander",
                      raceB %in% 6:20 ~ "Two or more races"))

ncvs$race <- factor(ncvs$race)
ncvs$raceB <- factor(ncvs$raceB)
levels(ncvs$race) <- c("White", "Black", 
                       "American Indian/Alaska Native", 
                       "Asian/Pacific Islander", 
                       "Two or more races (since 2003)")
ncvs <- ncvs|>
  mutate(race = paste_(race, raceB))|>
  subset(select= -raceB)

# age bins
ncvs$age <- as.numeric(ncvs$age)

ncvs <- ncvs|>
  mutate(age5 = case_when(age %in% 12:24 ~ "Youth (12-24)",
                          age %in% 25:34 ~ "Young Adult (25-34)",
                          age %in% 35:49 ~ "Prime Working Age (35-49)",
                          age %in% 50:64 ~ "Mature Working Age (50-64)",
                          age %in% 65:90 ~ "Seniors (65+)"))

ncvs$age5 <- factor(ncvs$age5, levels = c("Youth (12-24)",
                                          "Young Adult (25-34)",
                                          "Prime Working Age (35-49)",
                                          "Mature Working Age (50-64)",
                                          "Seniors (65+)"))

# sexual violence subset
ncvs <- ncvs|>
  mutate(sexual_violence = 
           case_when(type_of_crime %in% c(1:4, 15:16, 18:19) ~ "Yes",
                     type_of_crime %in% c(5:14, 17, 20:59) ~ "No"))
ncvs$sexual_violence = factor(ncvs$sexual_violence)

ncvs_sv <- ncvs|>
  filter(sexual_violence == "Yes")
ncvs_sv$type_of_crime <- recode(factor(ncvs_sv$type_of_crime), 
                             "1" = "Completed Rape", "2" = "Attempted Rape", 
                             "3" = "Sexual Assault",
                             "4" = "Sexual Assault",
                             "15" = "Sexual Assault",
                             "16" = "Sexual Coercion",
                             "18" = "Sexual Harassment",
                             "19" = "Sexual Harassment")
ncvs_sv <- ncvs_sv|>
  mutate(toc4 = recode(factor(ncvs_sv$type_of_crime), 
                       "Completed Rape" = "Rape", "Attempted Rape" = "Rape", 
                       "Sexual assault" = "Sexual assault",
                       "Sexual Coercion" = "Sexual Coercion",
                       "Sexual Harassment" = "Sexual harassment"))
#_______________________________________________________________________________

# III. Distribution------------------------------------------------------------------

# count
ncvs|>
  count(sexual_violence)

# gender
ncvs_sv|>
  count(sex)
ncvs_sv|>
  count(sex)

  ## Incident type table----

incident_type <- ncvs_sv|>
  count(type_of_crime)|>
  arrange(desc(n))
 
incident_type|>
  gt()|>
  tab_header(
    title = md("**Sexual Violence Frequencies,<br/>by Incident Type**"))|>
  cols_label(
    type_of_crime = md("*Incident Type*"), n = md("*Frequecy*"))|>
  tab_source_note(
    source_note = "Source: NCVS 1992-2024")|>
  
  cols_align(
    align = "left", columns = c(type_of_crime, n))|>
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_source_notes())|>
  opt_table_font(
    font = "serif", size = 18, color = "#543e34")|>
  
  tab_options(
    heading.background.color = "#7B8F56",
    heading.title.font.size = 25,
    heading.padding = px(10),
    heading.padding.horizontal = px(15),
    
    column_labels.background.color = "#B0C48A",
    column_labels.font.size = 21,
    column_labels.padding = px(7),
    column_labels.padding.horizontal = px(10),
    
    
    column_labels.border.top.style = "hidden",
    column_labels.border.bottom.style = "hidden",
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    
    table_body.hlines.color = "#B0C48A",
    table_body.border.bottom.color = "#7B8F56")|>
  
  gt_add_divider(
    columns = type_of_crime,
    color = "#7B8F56",
    style = "solid",
    weight = 1)|>
  tab_style(
    style = cell_fill(color = "#F2F9E7"), 
    locations = list(cells_body(rows = seq(1, nrow(incident_type), 2))))
# gtsave("sv_freq.png", expand = 0)

  ## Race----

race2 <-
  ncvs_sv|> 
  count(race)|>
  rename(sv_incidents = n)
race <- 
  ncvs|> 
  count(race)|>
  rename(total_incidents = n)|>
  left_join(race2)|>
  mutate(percentage = sv_incidents/total_incidents)

    ### Figure 1----

ggplot(race, aes(x= reorder(race, desc(percentage)), y=percentage))+
  geom_col(fill= "#B0C48A")+
  scale_x_discrete(labels=c('Mixed Race', 'Native','White', 'Black','Asian'))+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent_format()) +
  labs(x = "\nRace", 
       y='Percentage\n', 
       title='Percentage of people who have experienced sexual violence,\nby race',
       caption= 'Source: NCVS 1992-2024')+
  theme(axis.title = element_text(colour = "#543e34", size = 14,
                                  family="serif"),
        plot.title = element_text(colour = "#543e34", size = 17,face = "bold",
                                  hjust = .5, family="serif"),
        axis.text = element_text(colour = "#837165", size=12,
                                 family="serif"),
        plot.caption = element_text(colour = "#837165", size=12,
                                    family="serif"))

  ## Age----
age2 <- 
  ncvs|> 
  count(age5)|>
  rename(total_incidents = n)

age <-
  ncvs_sv|> 
  group_by(toc4)|>
  count(age5)|>
  rename(sv_incidents = n)|>
  left_join(age2)|>
  mutate(percentage = sv_incidents/total_incidents)

    ### Figure 2----
ggplot(age, aes(x = age5, y = percentage, fill = toc4))+
  geom_col()+
  scale_fill_manual(values = c("#E59B89","#B0C48A","#e5dacf","#543e34"))+
  theme_minimal()+
  facet_wrap(~toc4, ncol=1, scales = "free_y")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels=c('12-24', '25-34', '35-49','50-64', '65+'))+
  labs(x = "\nAge", 
       y='Age-adjusted incident rate \n', 
       title='Sexual Violence by Age and Type of Incident',
       subtitle = "incidents by race and type/total incidents by age",
       caption= 'Source: NCVS 1992-2024')+
  theme(axis.title = element_text(colour = "#837165", size = 14,
                                  family="serif"),
        plot.title = element_text(colour = "#543e34", size = 17,
                                  hjust = .5, face="bold", family="serif"),
        plot.subtitle = element_text(colour = "#837165", size = 13,
                                     hjust = .5, family="serif"),
        axis.text = element_text(colour = "#c38e81", size=11,
                                 face = "bold", family="serif"),
        strip.text = element_text(color = "#644f44", size = 13,
                                  family = "serif"),
        plot.caption = element_text(colour = "#837165", size=12,
                                    family="serif"))

#_______________________________________________________________________________

# IV. Time Series-------------------------------------------------------------------

yearly2 <- 
  ncvs_sv|> count(incident_year)|>
  rename(sv_incidents = n)

yearly <-ncvs|> 
  count(incident_year)|>
  rename(total_incidents = n)|>
  left_join(yearly2)|>
  mutate(percentage = (sv_incidents/total_incidents))

    ### Figure 3----

ggplot(yearly, aes(x= incident_year, y=sv_incidents))+
  geom_point(color= "#543e34")+
  theme_minimal()+
  geom_smooth(se = FALSE, color = "#B0C48A")+
  labs(x = "Year", 
       y='Number of Incidents\n', 
       title='Sexual Violence Incidents',
       caption= 'Source: NCVS 1992-2024')+
  theme(axis.title = element_text(colour = "#837165", size = 14,
                                  family="serif"),
        plot.title = element_text(colour = "#543e34", size = 17,
                                  hjust = .5, face="bold", family="serif"),
        axis.text = element_text(colour = "#c38e81", size=11,
                                 face = "bold", family="serif"),
        plot.caption = element_text(colour = "#837165", size=12,
                                    family="serif"))

ggplot(yearly, aes(x= incident_year, y=percentage))+
  geom_point(color= "#543e34")+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent_format()) +
  geom_smooth(se = FALSE, color = "#B0C48A")+
  labs(x = "Year", 
       y='Percentage of SV Incidents \n', 
       title='Sexual Violence Incidents, Controlling for Total Incidents',
       subtitle = 'Sexual Violence Incidents/Total Incidents',
       caption= 'Source: NCVS 1992-2024')+
  theme(axis.title = element_text(colour = "#837165", size = 14,
                                  family="serif"),
        plot.title = element_text(colour = "#543e34", size = 17,
                                  hjust = .5, face="bold", family="serif"),
        plot.subtitle = element_text(colour = "#837165", size = 13,
                                     hjust = .5, family="serif"),
        axis.text = element_text(colour = "#c38e81", size=11,
                                 face = "bold", family="serif"),
        plot.caption = element_text(colour = "#837165", size=12,
                                    family="serif"))

  ## Yearly Reports----

yearly3 <- ncvs_sv|>
  filter(reported == 1)|>
  count(incident_year)|>
  rename(sv_reported = n)

yearly <- yearly|>
  left_join(yearly3)|>
  mutate(percentage_sv_reported = sv_reported/sv_incidents) 

    ### Figure 4----
ggplot(yearly, aes(x=incident_year, y=percentage_sv_reported))+
  geom_point(color= "#543e34")+
  theme_minimal()+
  scale_y_continuous(labels = scales::percent_format()) +
  geom_smooth(se = FALSE, color = "#644f44")+
  labs(x = "year", 
       y='percentage of SV incidents reported\n', 
       title='Percentage of sexual violence incidents reported to police',
       subtitle ='Sexual violence incidents reported/Sexual violence incidents',
       caption='Source: NCVS 1992-2024')+
  theme(axis.title = element_text(colour = "#837165", size = 14,
                                  family="serif"),
        plot.title = element_text(colour = "#543e34", size = 17,
                                  hjust = .5, face="bold", family="serif"),
        plot.subtitle = element_text(colour = "#837165", size = 13,
                                     hjust = .5, family="serif"),
        axis.text = element_text(colour = "#c38e81", size=11,
                                 face = "bold", family="serif"),
        plot.caption = element_text(colour = "#837165", size=12,
                                    family="serif"))

#_______________________________________________________________________________

# V. Reporting---------------------------------------------------------------------

reports <- ncvs|>
  select(incident_year, type_of_crime, reported, how_reported, 
         reason_not_rptd, reason_rptd)

reports_sv <- ncvs_sv|>
  select(incident_year, toc4, reported, how_reported, 
         reason_not_rptd, reason_rptd)

# proportions who reported to police
reports|>
  count(reported)
reports_sv|>
  count(reported)

  ## No report reason----
no_report_reason <- reports_sv|>
  filter(reported == 2)|>
  count(reason_not_rptd)|>
  drop_na()|>
  mutate(percent = n/sum(n))|>
  arrange(desc(percent))|>
  filter(reason_not_rptd !=18)|> #remove "other"
  slice_head(n=6)|>
  mutate(reason_not_rptd = recode(factor(reason_not_rptd),
                                   "2" = "Took care of it informally",
                                   "16" = "Afraid of reprisal ",
                                   "14" = "Don't want offender in trouble",
                                   "1" = "Reported to another offical",
                                   "3" = "Minor or unsucessful crime",
                                   "10" = "Police would think unimportant"))|>
  select(!n)

no_report_reason|>
  gt()|>
  tab_header(
    title = md("**Why was the crime not reported?**"))|>
  tab_source_note(
    source_note = "Source: NCVS 1992-2024")|>
  
  cols_align(
    align = "left", columns = c(reason_not_rptd, percent))|>
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_source_notes())|>
  cols_width(percent ~ px(75))|>
  opt_table_font(
    font = "serif", size = 18, color = "#543e34")|>
  fmt_percent(columns = percent, decimals = 1)|>
  
  tab_options(
    heading.background.color = "#DD8672",
    heading.title.font.size = 25,
    heading.padding = px(10),
    heading.padding.horizontal = px(15),
    
    column_labels.hidden = TRUE,
    
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    heading.border.bottom.style = "hidden",
    
    table_body.hlines.color = "#E59B89",
    table_body.border.bottom.color = "#E59B89")|>
  
  gt_add_divider(
    columns = reason_not_rptd,
    color = "#E59B89",
    style = "solid",
    weight = 1)|>
  tab_style(
    style = cell_fill(color = "#FCE7E1"), 
    locations = list(cells_body(rows = seq(1, nrow(no_report_reason), 2))))
# gtsave("no_report_reason.png", expand = 0)

  ## Report reason----

report_reason <- reports_sv|>
  filter(reported == 1)|>
  count(reason_rptd)|>
  drop_na()|>
  mutate(percent = n/sum(n))|>
  filter(reason_rptd != 11)|>
  filter(reason_rptd != 21)

report_reason$n[4] <- report_reason$n[4]+report_reason$n[5]
report_reason$percent[4] <- report_reason$percent[4]+report_reason$percent[5]
report_reason <- report_reason[-5,]

report_reason <- report_reason|>
  arrange(desc(percent))|>
  slice_head(n=6)|>
  mutate(reason_rptd = recode(factor(reason_rptd),
                                  "5" = "Prevent future crimes by offender",
                                  "1" = "To stop THIS incident from happening",
                                  "22" = "Because it was a crime",
                                  "7" = "To punish offender",
                                  "2" = "Needed help after incident (injured)",
                                  "8" = "Catch or find offender"))|>
  select(!n)


report_reason|>
  gt()|>
  tab_header(
    title = md("**Why was the crime reported?**"))|>
  tab_source_note(
    source_note = "Source: NCVS 1992-2024")|>
  
  cols_align(
    align = "left", columns = c(reason_rptd, percent))|>
  cols_width(percent ~ px(75))|>
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_source_notes())|>
  opt_table_font(
    font = "serif", size = 18, color = "#543e34")|>
  fmt_percent(columns = percent, decimals = 1)|>
  
  tab_options(
    heading.background.color = "#B0C48A",
    heading.title.font.size = 25,
    heading.padding = px(10),
    heading.padding.horizontal = px(35),
    
    column_labels.hidden = TRUE,
    
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    heading.border.bottom.style = "hidden",
    
    table_body.hlines.color = "#B0C48A",
    table_body.border.bottom.color = "#B0C48A")|>
  
  gt_add_divider(
    columns = reason_rptd,
    color = "#B0C48A",
    style = "solid",
    weight = 1)|>
  tab_style(
    style = cell_fill(color = "#F2F9E7"), 
    locations = list(cells_body(rows = seq(1, nrow(report_reason), 2))))
# gtsave("report_reason.png", expand = 0)

  ## Who reported----

report_who <- reports_sv|>
  filter(reported == 1)|>
  count(how_reported)|>
  drop_na()|>
  mutate(percent = n/sum(n))|>
  arrange(desc(percent))|>
  slice_head(n=5)|>
  mutate(how_reported = recode(factor(how_reported),
                              "1" = "Victim",
                              "2" = "Household member",
                              "4" = "Someone else",
                              "3" = "Someone official",
                              "5" = "Police were already at the scene"))|>
  select(!n)


report_who|>
  gt()|>
  tab_header(
    title = md("**Who reported the crime to police?**"))|>
  tab_source_note(
    source_note = "Source: NCVS 1992-2024")|>
  
  cols_align(
    align = "left", columns = c(how_reported, percent))|>
  cols_width(percent ~ px(75))|>
  tab_style(
    style = cell_text(align = "right"),
    locations = cells_source_notes())|>
  opt_table_font(
    font = "serif", size = 18, color = "#543e34")|>
  fmt_percent(columns = percent, decimals = 1)|>
  
  tab_options(
    heading.background.color = "#7B8F56",
    heading.title.font.size = 25,
    heading.padding = px(10),
    heading.padding.horizontal = px(15),
    
    column_labels.hidden = TRUE,
    
    table.border.top.style = "hidden",
    table.border.bottom.style = "hidden",
    heading.border.bottom.style = "hidden",
    
    table_body.hlines.color = "#7B8F56",
    table_body.border.bottom.color = "#7B8F56")|>
  
  gt_add_divider(
    columns = how_reported,
    color = "#7B8F56",
    style = "solid",
    weight = 1)|>
  tab_style(
    style = cell_fill(color = "#E4EAD8"), 
    locations = list(cells_body(rows = seq(1, nrow(report_who), 2))))
# gtsave("report_who.png", expand = 0)

  

  ## Comparison ----

reports$type_of_crime = recode(factor(
  reports$type_of_crime),
  "1" = "Rape/sexual assault", "2" = "Rape/sexual assault",
  "3" = "Rape/sexual assault", "4" = "Rape/sexual assault",
  "5" = "Completed robbery", "6" = "Completed robbery",
  "7" = "Completed robbery", "8" = "Attempted robbery",
  "9" = "Attempted robbery", "10" = "Attempted robbery",
  "11" = "Aggravated assault", "12" = "Aggravated assault",
  "13" = "Assault", "14" = "Assault", "15" = "Rape/sexual assault",
  "16" = "Sexual coercion", "17" = "Assault", "18" = "Sexual harassment", 
  "19" = "Sexual harassment", "20" = "Assault", "21" = "Purse snatching",
  "22" = "Purse snatching", "23" = "Pocket picking",
  "31" = "Completed burglary, forcible entry",
  "32" = "Completed burglary, unlawful entry without force",
  "33" = "Attempted forcible entry", "40" = "Completed motor vehicle theft",
  "41" = "Attempted motor vehicle theft", "54" = "Theft",
  "55" = "Minor Theft (under $250)", "56" = "Minor Theft (under $250)",
  "57" = "Theft", "58" = "Theft", "59" = "Theft")

report_comparison <- reports|>
  filter(reported %in% c("1","2"))|>
  group_by(type_of_crime)|>
  count(reported)|>
  pivot_wider(names_from = reported, values_from = n)|>
  ungroup()|>
  rename("reported" = "1", "not_reported" = "2")|>
  mutate(total = reported + not_reported,
         percent_reported = reported/total,
         sv = NA)|>
  filter(type_of_crime %in% c("Rape/sexual assault",
                              "Completed robbery",
                              "Aggravated assault",
                              "Sexual coercion",
                              "Sexual harassment",
                              "Completed burglary, forcible entry",
                              "Attempted forcible entry",
                              "Completed motor vehicle theft",
                              "Minor Theft (under $250)"))|>
  arrange(desc(percent_reported))

for(i in 1:nrow(report_comparison)){
  toc <- report_comparison[i, "type_of_crime"]
  if(toc == "Rape/sexual assault"){sv <- "yes"}
  else if(toc == "Sexual coercion"){sv <- "yes"}
  else if(toc == "Sexual harassment"){sv <- "yes"}
  else{sv <- "no"}
  report_comparison[i, "sv"] <- sv
}

    ### Figure 5----

ggplot(report_comparison, aes(x = fct_reorder(type_of_crime, percent_reported),
                         y = percent_reported, fill = sv)) +
  geom_col()+
  theme_minimal()+
  scale_fill_manual(name = "",
                    values = c("#B0C48A","#543e34"),
                    labels = c('Other crimes', 'Sexual Violence'))+
  scale_y_continuous(labels = scales::percent_format())+
  scale_x_discrete(labels=c('Minor Theft\n(under $250)', 'Sexual\nCoercion', 'Rape/\nAssault',
                            'Sexual\nHarassment', 'Attempted\nBurglary',
                            'Aggrevated\nAssault', 'Robbery',
                            'Burglary\n(forced entry)', 'Vehicle Theft'))+
  labs(x = "\nType of Incident", 
       y= 'Percent\n', 
       title='Percentage of Incidents Reported',
       caption= 'Source: NCVS 1992-2024')+
  theme(axis.title = element_text(colour = "#837165", size = 14,
                                  family="serif"),
        plot.title = element_text(colour = "#543e34", size = 17,
                                  hjust = .5, face="bold", family="serif"),
        axis.text = element_text(colour = "#644f44", size= 10,
                                 family="serif"),
        legend.text = element_text(colour = "#644f44", size= 10, face = "bold",
                                   family="serif"),
        plot.caption = element_text(colour = "#837165", size=12,
                                    family="serif"))

#_______________________________________________________________________________

# VI. Predictive model---------------------------------------------------------------

  ## World Bank Api----

library(wbstats)

indicators <- c('EN.POP.DNST','FP.CPI.TOTL','IT.NET.USER.ZS','NY.GDP.PCAP.CD')

call <- wb_data(
  indicator = indicators,
  country = "USA",
  start_date = 1992,
  end_date = 2023,
  return_wide = FALSE)

options(scipen = 999)
options(digits = 5)

#by year
wb_yearly <- call|>
  select(date, indicator_id, value)|>
  rename(year = date)|>
  pivot_wider(names_from = indicator_id, values_from = value)|>
  select(year, pop_density = EN.POP.DNST, cpi = FP.CPI.TOTL,
         internet = IT.NET.USER.ZS,gdp_percap = NY.GDP.PCAP.CD)

    ### WB variable codebook----

new_vars <- wb_yearly|>
  select(!year)|>
  pivot_longer(everything(), names_to = "variable")|>
  select(variable)|>
  unique()
all_indicators <- glimpse(wb_cachelist$indicators)|>
  select(indicator_id, indicator_desc)
wb_indicators <- call|>
  select(indicator_id, indicator)|>
  unique()|>
  left_join(all_indicators)
wb_indicators <- cbind(new_vars, wb_indicators)


  ## Combine data----

model_data <- wb_yearly|>
  left_join(yearly, join_by("year" == "incident_year"))|>
  arrange(year)

  ## Build model-----
library(caret)

model.loocv <- train(sv_incidents ~ pop_density + cpi + internet + gdp_percap,
               data = model_data, method = "lm",
               trControl =  trainControl(method = "LOOCV"))

model.loocv$results

model_predictions <- model.loocv$pred|>
  mutate(rowIndex = rowIndex + 1991)|>
  rename(year=rowIndex)|>
  select(year, pred, obs)|>
  pivot_longer(cols = c(pred, obs), names_to = "data_type")

model <- lm(sv_incidents ~ pop_density + cpi + internet + gdp_percap,
            model_data)
summary(model)
    ### Figure 6&7---- 

#predictions
ggplot(model_predictions, aes(x=year, y=value, group = data_type)) +
  geom_point(size=4, aes(shape = data_type, color= data_type))+
  theme_minimal()+
  labs(x = "\nYear", 
       y='Number of SV Incidents \n',
       title='Model Results',
       subtitle = 'Sexual Violence Incidents, Observed and Predicted',
       caption= 'Sources: World Bank & NCVS 1992-2023')+
  scale_color_manual(values = c("#E59B89","#543e34"), 
                     labels = c("Observation","Prediction"))+
  scale_shape_manual(values = c(19,1),
                     labels = c("Observation","Prediction"))+
  theme(axis.title = element_text(colour = "#837165", size = 14,
                                  family="serif"),
        plot.title = element_text(colour = "#543e34", size = 17,
                                  hjust = .5, face="bold", family="serif"),
        plot.subtitle = element_text(colour = "#837165", size = 13,
                                     hjust = .5, family="serif"),
        axis.text = element_text(colour = "#644f44", size= 10,
                                 family="serif"),
        legend.text = element_text(colour = "#644f44", size= 10, face = "bold",
                                   family="serif"),
        legend.title = element_blank(),
        plot.caption = element_text(colour = "#837165", size=12,
                                    family="serif"))


#residuals 
residuals <- model.loocv$pred|>
  mutate(rowIndex = rowIndex + 1991,
         residuals = pred-obs)|>
  rename(year=rowIndex)|>
  select(year, pred, obs, residuals)

ggplot(residuals, aes(x=year, y=residuals))+
  geom_point(color = "#B0C48A", size = 3)+
  theme_minimal()+
  labs(x = "\nYear", 
       y= 'Value\n', 
       title='Residuals (LOOCV Model)')+
  theme(axis.title = element_text(colour = "#543e34", size = 14,
                                  family="serif"),
        plot.title = element_text(colour = "#543e34", size = 17,
                                  hjust = .5, face="bold", family="serif"),
        axis.text = element_text(colour = "#644f44", size= 10,
                                 family="serif"))
#_______________________________________________________________________________