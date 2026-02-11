# packages
library(ggplot2)
library(ggfun)
library(ggtext)
library(dplyr)
library(readr)
library(glue)
library(ggh4x)

# get new copies of raw atlas data
data_dir <- getwd()
node_metadata <- paste0(data_dir, "/data-raw/node_metadata.csv") |>
  read_csv(show_col_types = FALSE) |>
  filter(supported == TRUE) |>
  select(-supported)

df <- paste0(data_dir, "/data-raw/node_config.csv") |>
  read_csv(show_col_types = FALSE) |> 
  filter(atlas %in% node_metadata$region,
         functional == TRUE,
         !is.na(type) # this happens rn because of GBIF collectory search not being properly integrated
         ) |> 
  select(-functional, -url) |>
  bind_rows( 
    # add cached gbif data
    tibble(
      atlas = "Global",
      type = c("data/species", "metadata/fields", "metadata/assertions")),
    # some atlases link directly to media rather than support an API; add those here
    tibble(
      atlas = c("Austria", "Brazil", "Guatemala", "Portugal", "Spain", "United Kingdom"),
      type = "files/media"
    )
  ) |>
  arrange(atlas, type) |>
  left_join(
    select(node_metadata, region, url),
    by = c("atlas" = "region")
  ) |>
  mutate(atlas_label = glue::glue("<span style='font-size:9pt'>{atlas}</span><br>
                                  <span style='font-size:6pt'>{gsub('^https://', '', url)}</span>"),
         api_group = case_when(
           grepl("^data/occurrences", type) ~ "occurrences",
           grepl("^data/species|lists", type) ~ "species",
           grepl("collections|datasets|providers", type) ~ "sources",
           grepl("assertions|profiles|fields|licences", type) ~ "filters",
           grepl("taxa|identifiers", type) ~ "taxa",
           # grepl("media", type) ~ "media",
           # grepl("reasons", type) ~ "other",
           .default = "other"
         )) 

# order groups
df$group <- factor(
  case_when(grepl("^metadata/", df$type) ~ 1,
            grepl("^data/", df$type) ~ 2,
            grepl("^files/", df$type) ~ 3),
  levels = seq_len(3),
  labels = c("Metadata", 
             "Data", 
             "Files"))

df$subgroup <- factor(
  case_match(df$api_group, 
             "sources" ~ 1, 
             "taxa" ~ 2, 
             "filters" ~ 3,
             "occurrences" ~ 4,
             "species" ~ 5,
             "other" ~ 6
             ),
  levels = seq_len(6),
  labels = c("Sources",
             "Taxa",
             "Filters",
             "Occurrences",
             "Species",
             "Other"))

df$atlas_label <- factor(
  case_match(df$atlas,
             "Global" ~ 1,
             "Australia" ~ 2,
             "Spain" ~ 3,
             "Sweden" ~ 4,
             "Flanders" ~ 5,
             "United Kingdom" ~ 6,
             "Kew" ~ 7,
             "Austria" ~ 8,
             "Brazil" ~ 9,
             "Guatemala" ~ 10,
             "Portugal" ~ 11,
             "France" ~ 12
  ),
  levels = seq_len(12),
  labels = c(
    "<span style='font-size:9pt'>Global</span><br>\n<span style='font-size:6pt'>gbif.org</span>",
    "<span style='font-size:9pt'>Australia</span><br>\n<span style='font-size:6pt'>www.ala.org.au</span>",
    "<span style='font-size:9pt'>Spain</span><br>\n<span style='font-size:6pt'>gbif.es</span>",
    "<span style='font-size:9pt'>Sweden</span><br>\n<span style='font-size:6pt'>biodiversitydata.se</span>" ,
    "<span style='font-size:9pt'>Flanders</span><br>\n<span style='font-size:6pt'>natuurdata.inbo.be</span>",
    "<span style='font-size:9pt'>United Kingdom</span><br>\n<span style='font-size:6pt'>nbn.org.uk</span>",
    "<span style='font-size:9pt'>Kew</span><br>\n<span style='font-size:6pt'>data.kew.org</span>",
    "<span style='font-size:9pt'>Austria</span><br>\n<span style='font-size:6pt'>biodiversityatlas.at</span>",
    "<span style='font-size:9pt'>Brazil</span><br>\n<span style='font-size:6pt'>sibbr.gov.br</span>",
    "<span style='font-size:9pt'>Guatemala</span><br>\n<span style='font-size:6pt'>snib.conap.gob.gt</span>",
    "<span style='font-size:9pt'>Portugal</span><br>\n<span style='font-size:6pt'>www.gbif.pt</span>",
    "<span style='font-size:9pt'>France</span><br>\n<span style='font-size:6pt'>openobs.mnhn.fr</span>"
  )
)

df$type_label <- factor(
  case_match(df$type, 
             "metadata/providers" ~ 1,
             "metadata/collections" ~ 2,
             "metadata/datasets" ~ 3,
             "metadata/taxa-single" ~ 4,
             "metadata/taxa-unnest" ~ 5,
             "metadata/taxa-multiple" ~ 6,
             "metadata/identifiers" ~ 7,
             "metadata/fields" ~ 8,
             "metadata/fields-unnest" ~ 9,
             "metadata/assertions" ~ 10,
             "metadata/licences" ~ 11,
             "metadata/profiles" ~ 12,
             "metadata/profiles-unnest" ~ 13,
             "metadata/lists" ~ 14,
             "metadata/lists-unnest" ~ 15,
             "metadata/media" ~ 16,
             "metadata/reasons" ~ 17,
             "metadata/config" ~ 18,
             "data/occurrences" ~ 19,
             "data/occurrences-doi" ~ 20,
             "data/occurrences-count" ~ 21,
             "data/occurrences-count-groupby" ~ 22,
             "data/species" ~ 23,
             "data/species-count" ~ 24,
             "files/media" ~ 25
),
  levels = seq_len(25),
  labels = c("providers", 
             "collections",
             "datasets",
             "taxa-single",
             "taxa-unnest",
             "taxa-multiple",
             "identifiers",
             "fields",
             "fields-unnest",
             "assertions",
             "licences",
             "profiles",
             "profiles-unnest",
             "lists",
             "lists-unnest",
             "media",
             "reasons",
             "authentication",
             "occurrences",
             "occurrences-doi",
             "occurrences-count",
             "occurrences-count-groupby",
             "species",
             "species-count",
             "media"
             ))

# plot
p <- ggplot(df, 
       aes(x = type_label, y = atlas_label, color = group)) + 
  geom_point() +
  scale_x_discrete(position = "bottom") +
  scale_y_discrete(limits = rev(levels(df$atlas_label))) +
  ggh4x::facet_nested(~ group + subgroup,
                      scales = "free_x",
                      space = "free_x") +
  scale_color_manual(
    values = c("#eb2d83", "grey40", "#279c9c")) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_markdown(hjust = 0,
                                   vjust = 0.6),
    axis.text.x.bottom = element_text(angle = 45, 
                                      hjust = 1,
                                      size = 8),
    axis.ticks = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey90",
                                      lineend = "round",
                                      linewidth = 7),
    # panel.grid.major.y = element_blank(),
    panel.background = element_blank(),
    strip.background = element_roundrect(color = "white",
                                         fill = "#fce7f0",
                                         r = grid::unit(2, "mm")),
    strip.text = element_text(hjust = 0,
                              size = 8),
    plot.margin = margin(2, 8, 0, 2, unit = "mm"),
    legend.position = "none")


# recolor the strip elements
## code modified from https://github.com/tidyverse/ggplot2/issues/2096
# g <- ggplot_build(p) |>
#   ggplot_gtable()
# 
# strip_elements <- which(grepl('strip-t', g$layout$name))
# fills <- c("#fce7f0", "#d9dbdb", "#defafa",
#   "#eb2d83", "#eb2d83", "#eb2d83", "#eb2d83", "#eb2d83",
#   "grey40", "grey40",
#   "#279c9c")
# ## length(fills) == length(strip_elements)
# for (i in strip_elements) {
#   j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$children))
#   g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[i]
# }
# grid::grid.draw(g)
#
## fails

ggsave("./man/figures/atlases_plot.png",
       width = 8,
       height = 6.0,
       units = "in")
