# ===============================
# PACKAGES
# ===============================
library(bibliometrix)
library(tidyverse)
library(stringr)
library(ggplot2)
library(countrycode)
library(writexl)
library(stringr)

# ===============================
# LOAD CLEAN DATA
# ===============================
load("data_clean.RData")   # loads object: data_clean

# ===============================
# RUN BIBLIO ANALYSIS
# ===============================
S <- biblioAnalysis(data_clean)
summary(S, k = 20)


# ===============================
# PUBLICATIONS PER YEAR PLOT
# ===============================

pubs_year <- data_clean %>%
  mutate(PY = as.numeric(PY)) %>%
  group_by(PY) %>%
  summarise(Articles = n()) %>%
  ungroup()

ggplot(pubs_year, aes(x = PY, y = Articles)) +
  geom_line(color = "tomato4", linewidth = 1.5) +
  geom_point(color = "tomato4", size = 2) +
  geom_smooth(color = "tomato2", fill = "tomato2", alpha = 0.1,linewidth = .3) +
  scale_x_continuous(
    breaks = seq(min(pubs_year$PY), max(pubs_year$PY), by = 1)
  ) +
  labs(
    x = NULL,                # remove label do eixo X
    y = "Number of Articles" # eixo Y em inglês
  ) +
  theme_minimal(base_size = 14)


# ===============================
# GEOGRAPHYCAL EVOLUTION PLOT
# ===============================

M <- metaTagExtraction(data_clean, Field = "AU_CO", sep = ";") # following github

M <- M %>%
  separate_rows(AU_CO, sep = ";") %>%
  mutate(AU_CO = trimws(AU_CO)) %>%  
  filter(AU_CO != "") %>%
  count(PY, AU_CO, name = "n_appearances") %>%
  mutate(
    iso3 = countrycode(AU_CO, "country.name", "iso3c")
  ) %>%
  filter(!is.na(iso3))  

# Get Years 
all_years <- sort(unique(M$PY))
all_countries <- unique(M$iso3)

df_full <- expand_grid(
  PY = all_years,
  iso3 = all_countries
) %>%
  left_join(M %>% select(PY, iso3, n_appearances),
            by = c("PY", "iso3")) %>%
  mutate(n_appearances = ifelse(is.na(n_appearances), 0, n_appearances))

df_cum <- df_full %>%
  arrange(iso3, PY) %>%
  group_by(iso3) %>%
  mutate(n_cum = cumsum(n_appearances)) %>%
  ungroup()

world <- map_data("world") %>%
  mutate(iso3 = countrycode(region, "country.name", "iso3c"))

plot_df <- df_cum %>%
  left_join(world, by = "iso3")

# 5. Plot final
ggplot(plot_df, aes(long, lat, group = group, fill = n_cum)) +
  geom_polygon(color = "white", size = 0.05) +
  coord_fixed(1.3) +
  facet_wrap(~ PY, nrow = 5, ncol = 2) +
  scale_fill_gradient(
    low = "#f0f0f0",
    high = "#081d58",
    na.value = "#f0f0f0",
    name = "Number of Publications"   
  ) +
  guides(
    fill = guide_colorbar(
      title.position = "top",     
      title.hjust = 0.5,          
      barwidth = unit(8, "cm"),   
      barheight = unit(0.4, "cm") 
    )
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    strip.text = element_text(size = 8, face = "bold"),
    legend.position = "bottom"     
  ) +
  labs(
    title = NULL,
    subtitle = NULL,
    caption = NULL
  )

# Most cited paper per year
top_cited_per_year <- data_clean %>%
  filter(!is.na(PY), !is.na(Z9)) %>%
  mutate(Z9 = as.numeric(Z9)) %>%   
  group_by(PY) %>%
  slice_max(Z9, n = 3, with_ties = FALSE) %>% 
  ungroup() %>%
  select(
    Ano = PY,
    Titulo = TI,
    Autores = AF,
    Journal = SO,
    Citacoes_no_ano = Z9,
    Citacoes_totais = TC,
    DOI = DI,
    ID_WoS = UT
  )

max_title   <- 35  
max_authors <- 35   
max_journal <- 35   

top_cited_per_year <- top_cited_per_year %>%
  mutate(
    Title_short = ifelse(
      str_length(Titulo) > max_title,
      paste0(str_sub(Titulo, 1, max_title), " (...)"),
      Titulo
    ),
    Autores_short = ifelse(
      str_length(Autores) > max_authors,
      paste0(str_sub(Autores, 1, max_authors), " (...)"),
      Autores
    ),
    Journal_short = ifelse(
      str_length(Journal) > max_journal,
      paste0(str_sub(Journal, 1, max_journal), " (...)"),
      Journal
    )
  )

write_xlsx(top_cited_per_year, "top_cited_per_year.xlsx")


