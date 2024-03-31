library(httr)
library(rvest)
library(tidyverse)
library(vistime)

supreme_court <- read_csv("https://epstein.usc.edu/s/justicesdata2022.csv")

supreme_court_final <- supreme_court |>
  select(justice = name,
         nominated = datenom,
         terminated = datesere,
         president = presname) |>
  filter(terminated != "888. NOMINEE NOT CONFIRMED OR DIDN'T SERVE") |>
  mutate(
    nominated = as.Date(nominated, format = "%m/%d/%Y"),
    terminated = as.Date(terminated, format = "%m/%d/%Y"),
    id = readr::parse_number(president),
    president = stringr::str_remove_all(president, "\\d+\\. ")
  ) |>
  select(-president)

# url <- "https://www.supremecourt.gov/about/members_text.aspx"
# 
# supreme_court_page_1 <- httr::GET(url, add_headers('user-agent' = 'Gov employment data scraper (duncan.gates123@gmail.com)'))
# 
# supreme_court_page_2 <- rvest::read_html(supreme_court_page)
# 
# supreme_court <- supreme_court_page_2 |>
#   html_elements(".justicetable")
# 
# chief_justices <- supreme_court[[1]] |>
#   html_table()
# 
# associate_justices <- supreme_court[[2]] |>
#   html_table()
# 
# all_justices <- chief_justices |>
#   bind_rows(associate_justices) |>
#   select(-1) |>
#   mutate(`Judicial Oath Taken` = stringr::str_remove_all(`Judicial Oath Taken`, "\\(a\\) "),
#          `Judicial Oath Taken` = as.Date(`Judicial Oath Taken`, format = "%b %d, %Y"),
#          `Date Service Terminated` = as.Date(`Date Service Terminated`, format = "%b %d, %Y")) |>
#   rename(justice = Name,
#          state_appointed_from = `State App't From`,
#          appointed_by = `Appointed by President`,
#          justice_start = `Judicial Oath Taken`,
#          justice_end = `Date Service Terminated`)
#   
# all_justices |> 
#   group_by(appointed_by) |>
#   count(sort = T)

presidents_page <- rvest::read_html("https://en.wikipedia.org/wiki/List_of_presidents_of_the_United_States")

presidents_terms <- presidents_page |>
  html_elements(".wikitable") |>
  html_table()

presidents_terms_final <- presidents_terms[[1]] |>
  select(-5) |>
  rename(no = `No.[a]`,
         name = `Name(Birth–Death)`,
         term = `Term[14]`,
         party = `Party[b][15]`,
         election = Election,
         vice_president = `Vice President[16]`) |>
  tidyr::separate_wider_delim(cols = term, delim = "–", names = c("start_pres", "end_pres")) |>
  mutate(name = stringr::str_remove_all(name, "\\(.*?\\).*"),
         start_pres = as.Date(start_pres, format = "%b %d, %Y"),
         end_pres = as.Date(end_pres, format = "%b %d, %Y"))

supreme_presidents <- presidents_terms_final |>
  select(no, name, start_pres, end_pres, party) |>
  left_join(supreme_court_final, by = c("no" = "id")) |>
  # mutate(party = case_when(party == "National Union[n]Democratic" ~ "Democratic",
  #                          party == "Whig[j]Unaffiliated" ~ "Whig",
  #                          party == "Republican\nNational Union[l]" ~ "Republican",
  #                          party == "Democratic-Republican[f]National Republican" ~ "Democratic-Republican",
  #                          !is.na(party) ~ party)) |>
  mutate(party = ifelse(!party %in% c("Democratic", "Republican"), "Other", party),
         color = case_when(party == "Democratic" ~ "blue",
                           party == "Republican" ~ "red",
                           party == "Other" ~ "gray30")#,
         # justice = stringr::str_replace_all(justice, ", ", "\n")
         )

supreme_presidents |> distinct(party, .keep_all = TRUE) |> gt::gt()

# Title: Who gets to Appoint Supreme Court Justices? Power in PReJudice

supreme_presidents |>
  group_by(name) |>
  count(sort = T)

just_pres <- supreme_presidents |>
  distinct(name, .keep_all = TRUE) |>
  select(name, start_pres, end_pres)

p <- gg_vistime(supreme_presidents |> filter(party != "Other") |> distinct(justice, .keep_all = TRUE),
        col.event = "name",
        col.start = "nominated",
        col.end = "terminated",
        col.group = "party",
        linewidth = 5,
        show_labels = FALSE,
        optimize_y = TRUE,
        col.color = "color",
        title = "Supreme Court Appointments by President and Party") +
  # geom_text(data = just_pres, aes(label = name, x = start_pres, y = 1)) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Fira Sans Bold", size = 20, face = "bold"),
    # text = element_text(family = "Fira Sans SemiBold", size = 5),
    axis.text.y = element_text(size = 17, family = "Fira Sans"),
    axis.text.x = element_text(size = 17, family = "Fira Sans")
  )

p

p |>
  ggsave(filename = "~/Downloads/supreme_court_appointments.png",
         bg = "white",
         width = 10, height = 8,
         dpi = 500)

# In days
average_justice_tenure <- supreme_presidents |>
  mutate(tenure = terminated - nominated) |>
  summarise(mean_tenure = mean(tenure, na.rm = TRUE))

average_justice_tenure |> pull() |> as.numeric() / 365 # In years

modern_supreme_presidents <- supreme_presidents |>
  filter(party != "Other")

# Group every 15 years and see how many republican and how many democratic nominees there are
modern_supreme_presidents |>
  mutate(tenure_)

# adjust the marker size by directly accessing the aes_params
# p$layers[[3]]$aes_params$size <- 20

# add a new geom_text_repel layer for the labels where you can specify appearance
# p + 
#   ggrepel::geom_text_repel(data=p$layers[[3]]$data,
#                            label=p$layers[[3]]$data$label, 
#                            size =10, 
#                            color="black")
