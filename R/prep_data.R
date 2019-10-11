suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(sf))

#### Prepare the data ####
mapdf <- st_read(dsn = "data-raw/North Macedonia_AL2-AL8.shp")
title_csv <-read_lines("data-raw/sample_data.csv", n_max=1)
example_data <- read_csv("data-raw/sample_data.csv", skip = 1, col_types = "cn", col_names = c("locname", "values"))
stat_reg <- mapdf %>% filter(adminlevel == 4)


sk_municipalities <-
  mapdf %>% filter(adminlevel == 7) %>% filter(is.na(ISO3166_2)) %>%
  separate(
    rpath,
    into = c("mun", "skcode", "region"),
    sep = ",",
    extra = "drop"
  )

other_municipalities <-
  mapdf %>% filter(adminlevel == 7) %>% filter(!is.na(ISO3166_2)) %>%
  separate(rpath,
           into = c("mun", "region"),
           sep = ",",
           extra = "drop") %>%
  mutate(skcode = NA)

combined_municipalities <-
  rbind(sk_municipalities, other_municipalities) %>%
  mutate(mun_sk_code = ifelse(is.na(skcode), mun, skcode))

combined_municipalities <-
  combined_municipalities %>% mutate(
    locname_reg =
      case_when(
        region == "2572069" ~ "Источен регион",
        region == "2570958" ~ "Североисточен регион",
        region == "2572701" ~ "Пелагониски регион",
        region == "2573935" ~ "Полошки регион",
        region == "2460455" ~ "Скопски регион",
        region == "2572240" ~ "Југоисточен регион",
        region == "2573888" ~ "Југозападен регион",
        region == "2572311" ~ "Вардарски регион"
      )
  ) %>%
  mutate(locname_sk = as.character(locname)) %>%
  mutate(locname_sk = ifelse(is.na(ISO3166_2), "Град Скопје", locname_sk))

map_joined_with_data <-
  inner_join(combined_municipalities, example_data, by="locname")

map_joined_with_data <- select (map_joined_with_data,-c(id,country,
                adminlevel, name, enname, offname, boundary, wikidata, wikimedia, timestamp, note))

saveRDS(map_joined_with_data, file = "data-raw/default-map.Rds")
saveRDS(stat_reg, file = "data-raw/stat-reg.Rds")
