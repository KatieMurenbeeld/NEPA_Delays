library(tidyverse)
library(forcats)
library(lubridate)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(fastDummies)
library(vtable)
library(stringr)

# Load the data

# PALS 
pals <- read_csv("pals_ongoing_projects_11-2022_2.csv")
# Forest Service Project Overview
proj_overview <- read_csv("forest_service_project_overview.csv")
# Forest Service Document Record
proj_docs <- read_csv("forest_service_document_record.csv")

# Filter for EAs and purpose of TM and VM (timber and vegetation management)
# Possibly HF (hazardous fuels) later

pals_ea <- pals %>%
  filter(`DECISION TYPE` == 'DN') %>% 
  filter(`TM Forest products – purpose` == 1 | `VM Vegetation management (non-forest products) – purpose` == 1)


# Join pals_ea `PROJECT NUMBER` to proj_docs by Project_Num
# or make a list of the project numbers in pals_ea and filter the proj_docs (and proj_overview) by that list
ea_list <- unique(pals_ea$`PROJECT NUMBER`)

#proj_docs_ea <- proj_docs[proj_docs$Project_Num %in% ea_list ,]
proj_docs_ea <- proj_docs %>%
  filter(Project_Num %in% ea_list)

length(unique(proj_docs_ea$Project_Num))

# We have 1563 NEPA projects from PALS that also have documents associated with them. 
# Print out the distribution by region, litigated, and appealed. 

# First need to do a little regex to create a Project_num column in proj_overview.
# Should be all the numbers after the 'project=' character string in the Page column

proj_overview$Project_Num <- as.numeric(str_extract_all(proj_overview$Page, regex("(?<=(project=))[0-9]+")))


proj_overview_ea <- proj_overview %>%
  filter(Project_Num %in% ea_list)


# What would be some interesting things to look at right away?
# Average number of documents per project?
# Average number of documents per stage of the process (scoping, supporting, analysis, etc.)
# Number of documents by region, appeals, litigation, etc?
# Number of documents through time?
# Relationship between number of documents and elapsed days?

# Want an aggregated data frame with project_num in one column total number of documents in the second column.
# Average number of documents per project?

proj_doc_count <- proj_docs_ea %>%
  count(Project_Num)
mean(proj_doc_count$n)
median(proj_doc_count$n)

ggplot(proj_doc_count, aes(x=n)) + 
  geom_histogram(binwidth = 10) + 
  ggtitle("Histogram of Number of Documents Per Project") + 
  xlab("Number of Documents")

# Outlier is NEPA Project #52541, End of the World project in Grangeville, ID


# Average number of documents per stage of the process (scoping, supporting, analysis, etc.)
proj_doc_count_stage <- proj_docs_ea %>%
  count(Project_Num, Stage)
aggregate(proj_doc_count_stage$n, list(proj_doc_count_stage$Stage), FUN=sum)

agg_df_mean <- aggregate(proj_doc_count_stage$n, list(proj_doc_count_stage$Stage), FUN=mean)
agg_df_median <- aggregate(proj_doc_count_stage$n, list(proj_doc_count_stage$Stage), FUN=median)

proj_doc_count_stage %>% 
  ggplot(aes(x=Stage, y=n)) +
  geom_bar(stat="identity") + 
  ggtitle("Number of Documents by NEPA Stage") + 
  xlab("NEPA Stage") + ylab("Count")

agg_df_mean %>% 
  ggplot(aes(x=Group.1, y=x)) +
  geom_bar(stat="identity") + 
  ggtitle("Average Number of Documents per Project by NEPA Stage") + 
  xlab("NEPA Stage") + ylab("Ave. Number of Documents by Stage") # Make sure that is actually what you are averaging.

agg_df_median %>% 
  ggplot(aes(x=Group.1, y=x)) +
  geom_bar(stat="identity") + 
  ggtitle("Average Number of Documents per Project by NEPA Stage") + 
  xlab("NEPA Stage") + ylab("Median Number of Documents by Stage per Project")

# Ave. number of documents by region, lititgation, appeals
# Need to join the proj_docs_ea to the pals data by project number

pals_doc_ea <- left_join(proj_docs_ea, pals_ea, by=c('Project_Num'='PROJECT NUMBER'))

proj_doc_count_region <- pals_doc_ea %>%
  count(Project_Num, REGION_ID)

aggregate(proj_doc_count_region$n, list(proj_doc_count_region$REGION_ID), FUN=mean)
aggregate(proj_doc_count_region$n, list(proj_doc_count_region$REGION_ID), FUN=median)

proj_doc_count_lit <- pals_doc_ea %>%
  count(Project_Num, `LITIGATED?`)
proj_doc_count_app <- pals_doc_ea %>%
  count(Project_Num, `APPEALED OR OBJECTED?`)

aggregate(proj_doc_count_lit$n, list(proj_doc_count_lit$`LITIGATED?`), FUN=mean)
aggregate(proj_doc_count_lit$n, list(proj_doc_count_lit$`LITIGATED?`), FUN=median)

aggregate(proj_doc_count_app$n, list(proj_doc_count_app$`APPEALED OR OBJECTED?`), FUN=mean)
aggregate(proj_doc_count_app$n, list(proj_doc_count_app$`APPEALED OR OBJECTED?`), FUN=median)

ggplot(data = aggregate(proj_doc_count_region$n, list(proj_doc_count_region$REGION_ID), FUN=mean), aes(x=Group.1, y=x)) +
  geom_bar(stat="identity") + 
  ggtitle("Average Number of Documents per Project by NEPA Stage") + 
  xlab("FS Region") + ylab("Ave. Number of Documents by Region")

ggplot(data = aggregate(proj_doc_count_region$n, list(proj_doc_count_region$REGION_ID), FUN=median), aes(x=Group.1, y=x)) +
  geom_bar(stat="identity") + 
  ggtitle("Average Number of Documents per Project by NEPA Stage") + 
  xlab("FS Region") + ylab("Medain Number of Documents by Region")

ggplot(data = aggregate(proj_doc_count_lit$n, list(proj_doc_count_lit$`LITIGATED?`), FUN=mean), aes(x=Group.1, y=x)) +
  geom_bar(stat="identity") + 
  ggtitle("Average Number of Documents per Project by NEPA Stage") + 
  xlab("Litigated? (1 = yes, 0 = no") + ylab("Ave. Number of Documents by Litigated?")

ggplot(data = aggregate(proj_doc_count_lit$n, list(proj_doc_count_lit$`LITIGATED?`), FUN=median), aes(x=Group.1, y=x)) +
  geom_bar(stat="identity") + 
  ggtitle("Average Number of Documents per Project by NEPA Stage") + 
  xlab("Litigated? (1 = yes, 0 = no") + ylab("Medain Number of Documents by Litigated?")

ggplot(data = aggregate(proj_doc_count_app$n, list(proj_doc_count_app$`APPEALED OR OBJECTED?`), FUN=mean), aes(x=Group.1, y=x)) +
  geom_bar(stat="identity") + 
  ggtitle("Average Number of Documents per Project by NEPA Stage") + 
  xlab("Appealed? (1 = yes, 0 = no") + ylab("Ave. Number of Documents by Litigated?")

ggplot(data = aggregate(proj_doc_count_app$n, list(proj_doc_count_app$`APPEALED OR OBJECTED?`), FUN=median), aes(x=Group.1, y=x)) +
  geom_bar(stat="identity") + 
  ggtitle("Average Number of Documents per Project by NEPA Stage") + 
  xlab("Appealed? (1 = yes, 0 = no") + ylab("Medain Number of Documents by Appealed?")

# Number of documents through time? Look at both Initiation FY and Signed FY
# Group by year. Want mean or median documents per project by year? 

proj_doc_count_init <- pals_doc_ea %>%
  count(Project_Num, `INITIATION FY`)
  
proj_doc_count_sign <- pals_doc_ea %>%
  count(Project_Num, `SIGNED FY`)

ggplot(data = aggregate(proj_doc_count_init$n, list(proj_doc_count_init$`INITIATION FY`), FUN=sum), aes(x = Group.1, y = x)) + 
  geom_line() + 
  ggtitle("Total Number of Documents by Fiscal Year NEPA Project Initiated") + 
  xlab("Fiscal Year") + ylab("Total Number of Documents Related to EAs")

ggplot(data = aggregate(proj_doc_count_sign$n, list(proj_doc_count_sign$`SIGNED FY`), FUN=sum), aes(x = Group.1, y = x)) + 
  geom_line() + 
  ggtitle("Total Number of Documents by Fiscal Year NEPA Project Signed") + 
  xlab("Fiscal Year") + ylab("Total Number of Documents Related to EAs")

# Relationship between number of documents and elapsed days?
# Total number of documents for a project on x axis, the elapsed days for that project on the y axis.
# need proj_doc_count and elapsed days for each project (from pals)

proj_doc_elapsed <- left_join(proj_doc_count, pals, by=c('Project_Num'='PROJECT NUMBER')) %>%
  select(Project_Num, n, `ELAPSED DAYS`)

proj_doc_elapsed %>%
  ggplot(aes(x = n, y = `ELAPSED DAYS`)) + 
  geom_point() + 
  scale_y_continuous(trans='log10') +
  scale_x_continuous(trans='log10') +
  ggtitle("Elapsed Days by Number of NEPA Documents in Project") + 
  xlab("Number of Documents") + ylab("Days to Complete NEPA Analysis")

