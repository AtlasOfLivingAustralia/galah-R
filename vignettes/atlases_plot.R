# packages
library(ggplot2)
library(ggfun)
library(ggtext)
library(dplyr)
library(readr)

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
  bind_rows( # add cached gbif data
    tibble(
      atlas = "Global",
      type = c("data/species", "metadata/fields", "metadata/assertions"))) |>
  left_join(
    select(node_metadata, region, url),
    by = c("atlas" = "region")
  ) |>
  mutate(atlas_label = paste(atlas, url, sep = "\n"))

# clean up
df_functions <- lapply(strsplit(df$type, "/"),
                   function(a){
                     tibble(group = a[1], type = a[2])
                     }) |>
  bind_rows() |>
  mutate(atlas = df$atlas_label)

# order groups
df_functions$group <- factor(
  case_match(df_functions$group, "metadata" ~ 1, "data" ~ 2, "files" ~ 3),
  levels = seq_len(3),
  labels = c("metadata", "data", "files"))

# order types
count_df <- df_functions |>
  group_by(type) |>
  summarize(count = n()) |>
  arrange(desc(count)) |>
  mutate(order = seq_len(23))

type_seq <- lapply(df_functions$type, function(a){
  count_df$order[which(count_df$type == a)]
}) |> unlist()

df_functions$type <- factor(type_seq, 
                            levels = count_df$order,
                            labels = count_df$type) 

# order atlases in descending order of availability
count_df <- df_functions |>
  group_by(atlas) |>
  summarize(count = n())
count_df$count[grepl("Global", count_df$atlas)] <- 100 # put GBIF on top
count_df <- count_df |>
  arrange(count) |>
  mutate(order = seq_len(11))

atlas_seq <- lapply(df_functions$atlas, function(a){
  count_df$order[which(count_df$atlas == a)]
}) |> unlist()

df_functions$atlas <- factor(atlas_seq, 
                             levels = count_df$order,
                             labels = count_df$atlas) 

# plot
p <- ggplot(df_functions, 
       aes(x = type, y = atlas, color = group)) + 
  geom_point() +
  scale_x_discrete(position = "top") +
  facet_grid(cols = vars(group), 
             scales = "free_x",
             space = "free_x") +
  scale_color_manual(
    values = c("#eb2d83", "grey40", "#279c9c")) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_text(hjust = 0),
    axis.text.x = element_text(angle = 45, 
                               hjust = 0),
    axis.ticks = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey90",
                                      lineend = "round",
                                      linewidth = 7),
    panel.background = element_blank(),
    strip.background = element_roundrect(fill = "#fce7f0",
                                         r = grid::unit(2, "mm")),
    plot.margin = margin(2, 8, 0, 2, unit = "mm"),
    legend.position = "none")

ggsave("./vignettes/atlases_plot.png", 
       width = 8, 
       height = 6, 
       units = "in")