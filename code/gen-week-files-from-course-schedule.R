library(dplyr)
library(readxl)
library(purrr)
library(stringr)
library(glue)

plan <- read_xlsx("course-schedule.xlsx", "Week-plan")


plan_bits <- c(
  Week = "# Week {Week}: {Title}

",
  Date_First_Class = "",
  Title = "",
  Read = "{if(!is.na(Read))
  '## 📖 Reading

'}{if(!is.na(Read)) Read}

",
  Reading_Quiz = "{if(!is.na(Reading_Quiz))
  '### 🎯 Check your understanding

'}{if(!is.na(Reading_Quiz)) Reading_Quiz}

",
  Prepare = "{if(!is.na(Prepare))
  '## 🥣 Prepare for class

'}{if(!is.na(Prepare)) Prepare}

",
  Tuesday_Class = "{if(!is.na(Tuesday_Class))
  '## 🌮 Tuesday

'}{if(!is.na(Tuesday_Class)) Tuesday_Class}

",
  Thursday_Class = "{if(!is.na(Thursday_Class))
  '## 🔨️ Thursday 🌩

'}{if(!is.na(Thursday_Class)) Thursday_Class}

",
  Assignments = "{if(!is.na(Assignments))
  '##  🏋️ Practice your skills

'}{if(!is.na(Assignments)) Assignments}

"
)

templates <- purrr::map(split(plan, 1:nrow(plan)), ~ paste(plan_bits[names(.)[!is.na(.)]], collapse = ""))

md <- map2_chr(split(plan, 1:nrow(plan)), templates, glue_data)

md <- set_names(md, sprintf("weeks/week-%02d.qmd", plan$Week))

walk2(md, names(md), ~ writeLines(.x, con = .y))
