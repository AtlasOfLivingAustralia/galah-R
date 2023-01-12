# packages
library(ggplot2)
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
         api_name != "records_query")

# split into one row per function
df_functions <- lapply(split(df, df$atlas), 
                       function(a){
                         x <- do.call(c, strsplit(a$called_by, ", "))
                         tibble(atlas = a$atlas[1], functions = x)
                       }) |>
  bind_rows()

# add gbif species and fields - these use different apis and so are not shown
df_functions <- df_functions |>
  filter(!(functions %in% c("search_all-collections", "search_all-datasets", "doi_download"))) |>
  bind_rows(
    tibble(atlas = "Global", functions = c("atlas_species", 
                                           "show_all-fields")))

df_functions$functions[df_functions$functions == "media_metadata"] <- "atlas_media"

# create group headings
df_functions$group <- 1 # "show_all | search_all"
df_functions$group[grepl("^show_values", df_functions$functions)] <- 2 # "show_values"
df_functions$group[grepl("^atlas|doi_download", df_functions$functions)] <- 3 # "atlas"
df_functions$group <- factor(df_functions$group, 
                             levels = seq_len(3),
                             labels = c("show_all | search_all", "show_values", "atlas"))

# create function substrings
df_functions$subfunction <- gsub("^atlas_|^show_all-|^search_all-|^search_|^show_values-", "", 
                                 df_functions$functions)

# set up row order (atlases)
# count number of supported functions
number_supported <- df_functions %>% 
  group_by(atlas) %>%
  count()

# merge
df_functions <- df_functions %>% 
  left_join(number_supported, by = c("atlas" = "atlas")) %>%
  mutate(n = ifelse(atlas == "Australia", n + 1, n)) # make sure AU is first

df_functions$atlas_numeric <- as.numeric(reorder(df_functions$atlas, df_functions$n))

# set up column order (functions)
col_lookup <- lapply(
  split(df_functions, df_functions$group), 
  function(a){
    a |> 
      group_by(subfunction) |> 
      count() |> 
      arrange(desc(n)) |>
      mutate(group = a$group[1])
  }) |>
  bind_rows()

col_lookup$order <- seq_len(nrow(col_lookup))
col_lookup$x <- col_lookup$order + as.numeric(col_lookup$group) - 1

df_functions <- df_functions |>
  left_join(col_lookup, by = c("group", "subfunction"))

df_functions$function_numeric <- df_functions$x + 1

# # add font
# font_add_google("Roboto", "roboto")
# showtext_auto(enable = TRUE)

# create plot objects
# horizontal lines
df_lines <- tibble(
  x = rep(c(-4, 22), 6),
  y = rep((seq_len(6) * 2) - 1, each = 2))

# atlas text
df_atlases <- df_functions |> 
  group_by(atlas) |> 
  summarize(y = atlas_numeric[1]) |>
  left_join(node_metadata, by = c("atlas" = "region"))

# function text
df_function_names <- df_functions |>
  group_by(functions) |>
  summarize(x = function_numeric[1],
            label = subfunction[1],
            group = group[1])

# group headers
df_groups <- df_functions |>
  group_by(group) |>
  summarize(
    x0 = min(function_numeric),
    x1 = max(function_numeric),
    y = 12,
    x = mean(c(x0, x1)))

df_groups$x0[2] <- 13.5
df_groups$x1[2] <- 16.5

# plot
p <- ggplot() + 
  # background (grey) lines
  geom_line(data = df_lines,
            mapping = aes(x = x, y = y, group = y),
            linewidth = 22, 
            lineend = "round", 
            color = "grey90")+
  # group header bars
  geom_segment(data = df_groups,
               mapping = aes(x = x0, xend = x1, y = y, yend = y, color = group),
               lineend = "round",
               linewidth = 10) +
  # group header text
  geom_text(data = df_groups,
            mapping = aes(x = x, y = y, label = group),
            color = "white",
            size = 5,
            fontface = "bold") +
  # points
  geom_point(data = df_functions, 
             mapping = aes(x = function_numeric, 
                           y = atlas_numeric,
                           color = group),
             size = 4, 
             shape = 19) +
  # atlas regions
  geom_text(data = df_atlases,
            mapping = aes(y = y + 0.15, label = atlas),
            x = -4,
            size = 5,
            hjust = 0,
            fontface = "bold") +
  # atlas urls
  geom_text(data = df_atlases,
            mapping = aes(y = y - 0.15, label = url),
            x = -4,
            size = 4,
            hjust = 0) +
  # function names
  geom_text(data = df_function_names,
            mapping = aes(x = x, label = label, color = group),
            y = 12.5,
            size = 5,
            hjust = 0,
            angle = 45) +
  # "Atlas" label
  annotate("text", x = -4, y = 12, label = "Atlas", size = 5, hjust = 0, fontface = "bold") +
  scale_color_manual(
    values = c("#eb2d83", "grey40", "#279c9c")) +
  lims(x = c(-4, 22), y = c(0.5, 13.5)) +
  theme_void() +
  theme(legend.position = "none")

ggsave("./vignettes/atlases_plot.png", width = 12, height = 10, units = "in")
