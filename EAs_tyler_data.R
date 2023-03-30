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

# First need to do a little regex to create a Project_num column in proj_overview.
# Should be all the numbers after the 'project=' character string in the Page column

proj_overview$Project_Num <- as.numeric(str_extract_all(proj_overview$Page, regex("(?<=(project=))[0-9]+")))


proj_overview_ea <- proj_overview %>%
  filter(Project_Num %in% ea_list)


# What would be some interesting things to look at right away?
# Average number of documents per project?
# Average number of documents per stage of the process (scoping, supporting, analysis, etc.)
# Number of documents by region, appeals, litigation, etc?
# Relationship between number of documents and elapsed days?

# Want an aggregated data frame with project_num in one column total number of documents in the second column.
# Average number of documents per project?

proj_doc_count <- proj_docs_ea %>%
  count(Project_Num)
mean(proj_doc_count$n)

ggplot(proj_doc_count, aes(x=n)) + 
  geom_histogram(binwidth = 10) + 
  ggtitle("Histogram of Number of Documents Per Project") + 
  xlab("Number of Documents")

# Outlier is NEPA Project #52541, End of the World project in Grangeville, ID


# Average number of documents per stage of the process (scoping, supporting, analysis, etc.)
proj_doc_count_stage <- proj_docs_ea %>%
  count(Project_Num, Stage)
aggregate(proj_doc_count_stage$n, list(proj_doc_count_stage$Stage), FUN=sum)

test_df <- aggregate(proj_doc_count_stage$n, list(proj_doc_count_stage$Stage), FUN=mean)

proj_doc_count_stage %>% 
  ggplot(aes(x=Stage, y=n)) +
  geom_bar(stat="identity") + 
  ggtitle("Number of Documents by NEPA Stage") + 
  xlab("NEPA Stage") + ylab("Count")

test_df %>% 
  ggplot(aes(x=Group.1, y=x)) +
  geom_bar(stat="identity") + 
  ggtitle("Average Number of Documents per Project by NEPA Stage") + 
  xlab("NEPA Stage") + ylab("Ave. Number of Documents by Stage per Project") # Make sure that is actually what you are averaging.


