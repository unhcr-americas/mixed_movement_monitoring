# load packages
if (!require(tidyverse)) install.packages("tidyverse")
if (!require(janitor)) install.packages("janitor")
if (!require(rstudioapi)) install.packages("rstudioapi")
if (!require(scales)) install.packages("scales")
if (!require(directlabels)) install.packages("directlabels")
if (!require(pak)) install.packages("pak")
if (!require(riddle)) pak::pkg_install("edouard-legoupil/riddle")
if (!require(unhcrthemes)) pak::pkg_install("vidonne/unhcrthemes")


Sys.unsetenv("USE_UAT")
# Set ridl resource ---------------------------------------------------

# rstudioapi::showDialog(title = "Set the parameters for ridl resource!",
#                        message = "You will now enter   <b>country_name</b> and the precise URL for the dataset in RIDL  <b>repo_ridl</b>" )

# country_name <- rstudioapi::showPrompt(title = "country_name", message = "Enter Country name", default = "")
country_name <- "Guatemala"

# repo_ridl <- rstudioapi::showPrompt(title = "repo_ridl", message = "Enter precise URL for the dataset in RIDL", default = "")
repo_ridl <- "https://ridl.unhcr.org/dataset/encuesta-de-movimientos-mixtos"

# month_analysis_begin <- "January"
# month_analysis_end <- "March"

# read data ---------------------------------------------------------------

clean_data_ridl <- riddle::dataset_show(id = gsub("https://ridl.unhcr.org/dataset/", "", repo_ridl)) |> 
    select(resources) |>
    unnest(cols = c(resources)) |> 
    filter(stringr::str_detect(name, "Mixed movement - Cleaned")) |>
    mutate(month_year = ymd(date_range_end)) |>
    slice(which.max(month_year)) |> 
    pull(url)





# create a dataframe with the data
     


df_clean <- resource_fetch(clean_data_ridl) |> 
  read_csv() |> 
  clean_names()

# create indicator variables ---------------------------------------------------

# create a plot of how many people were interviewed in each month

tmp <- df_clean |> 
    select(today) |> 
    group_by(month(today), year(today)) |>
    count() |> 
    ungroup() |> 
    mutate(month_data = dmy(paste0("01-", `month(today)`, "-", `year(today)`))) |>
    select(month_data, n)


ggplot(tmp) +
    geom_line(aes(
      x = month_data,
      y = n,
      ),
    linewidth = 1,
    color = unhcr_pal(n = 1, "pal_blue")) +
  labs(
    title = paste0(country_name, ": Number of people interviewed in each month | 2022-2023"),
    y = "",
    caption = "Source: Mixed movement monitoring"
  ) +
  scale_x_date(breaks = pretty_breaks()) +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale()),
    limits = c(0,max(tmp$n)+200)
  ) +
  theme_unhcr(
    grid = "Y",
    axis = "x",
    axis_title = "y"
  )




#### Gender


tmp <- df_clean |> 
  select(today,b001_sex) |> 
  group_by(month(today), year(today)) |>
  count(b001_sex) |> 
  ungroup() |> 
  mutate(month_data = dmy(paste0("01-", `month(today)`, "-", `year(today)`))) |>
  select(month_data, b001_sex, n)



ggplot(tmp, aes(
  x = month_data,
  y = n,
  color = b001_sex
)) +
  geom_line(linewidth = 1) +
  geom_dl(aes(label = b001_sex),
          method = list(
            dl.trans(x = x + 0.1),
            "last.points"
          ),
          size = 8 / .pt
  ) +
  labs(
    title = paste0(country_name, ": Number of people interviewed in each month by sex | 2022-2023"),
    y = "",
    caption = "Source: Mixed movement monitoring"
  ) +
  scale_x_date(breaks = pretty_breaks()) +
  scale_y_continuous(
    expand = expansion(c(0, 0.1)),
    breaks = pretty_breaks(),
    labels = label_number(scale_cut = cut_short_scale()),
    limits = c(0,max(tmp$n)+200)
  ) +
  scale_color_unhcr_d(
    palette = "pal_unhcr",
    order = c(3, 2,1)
  ) +
  coord_cartesian(clip = "off") +
  theme_unhcr(
    grid = "Y",
    axis = "x",
    axis_title = "y",
    legend = FALSE
  ) +
  theme(plot.margin = margin(r = 50))


#### Age

tmp <- df_clean |>
  select(today, b002_age) |>
  mutate(
    age_cat = case_when(
      b002_age <= 17 ~ "12-17",
      b002_age >= 18 & b002_age <= 29 ~ "18-29",
      b002_age >= 30 & b002_age <= 39 ~ "30-39",
      b002_age >= 40 & b002_age <= 49 ~ "40-49",
      b002_age >= 50 & b002_age <= 59 ~ "50-59",
      b002_age >= 60 ~ "60+",
    )
  ) |> 
  group_by(month(today), year(today)) |>
  count(age_cat) |> 
  ungroup() |> 
  mutate(month_data = dmy(paste0("01-", `month(today)`, "-", `year(today)`))) |>
  select(month_data, age_cat, n)




ggplot(tmp, aes(
  x = month_data,
  y = n,
  color = age_cat
)) +
  geom_line(linewidth = 1) +
  geom_dl(aes(label = age_cat),
          method = list(
            dl.trans(x = x + 0.1),
            "last.points"
          ),
          size = 8 / .pt
  ) +
  labs(
    title = paste0(country_name, ": Number of people interviewed in each month by age | 2022-2023"),
    y = "",
    caption = "Source: Mixed movement monitoring"
  ) +
  scale_x_date(breaks = pretty_breaks()) +
  scale_y_continuous(
    expand = expansion(c(0, 0.1)),
    breaks = pretty_breaks(),
    labels = label_number(scale_cut = cut_short_scale()),
    limits = c(0,max(tmp$n)+200)
  ) +
  scale_color_unhcr_d(
    palette = "pal_unhcr",
    order = c(1:6)
  ) +
  coord_cartesian(clip = "off") +
  theme_unhcr(
    grid = "Y",
    axis = "x",
    axis_title = "y",
    legend = TRUE
  )



#### Top 3 Nationalities


tmp <- df_clean |>
  select(today, b004_nationality) |>
  group_by(month(today), year(today)) |>
  count(b004_nationality) |> 
  ungroup() |> 
  mutate(month_data = dmy(paste0("01-", `month(today)`, "-", `year(today)`))) |>
  select(month_data, b004_nationality, n)


ggplot(tmp) +
  geom_col(aes(
    x = n,
    y = reorder(b004_nationality, n)
  ),
  fill = unhcr_pal(n = 1, "pal_blue"),
  width = 0.8
  ) +
  geom_text(aes(
    x = n,
    y = reorder(b004_nationality, n),
    label = scales::label_number(accuracy = 1,
                                 scale_cut = scales::cut_short_scale())(n)
  ),
  hjust = -0.5,
  size = 8 / .pt
  ) +
  labs(
    title = paste0(country_name, ": Number of people interviewed in each month by nationality | 2022-2023"),
    x = "Number of people",
    caption = "Source: Mixed movement monitoring"
  ) +
  scale_x_continuous(
    expand = expansion(c(0, 0.1)),
    breaks = pretty_breaks(n = 7),
    labels = label_number(scale_cut = cut_short_scale())
  ) +
  theme_unhcr(
    grid = "X",
    axis = "y",
    axis_title = ""
  ) +
  facet_grid(cols = vars(factor(month.abb[month(tmp$month_data)],levels=month.abb)))




month(tmp$month_data)
factor(month(tmp$month_data), levels = month)

factor(month.abb[month(tmp$month_data)],levels=month.abb)
