# Visualization ----

# Call packages
library(dplyr)
library(tidyr)
library(networkD3)
library(stringr)
library(ggplot2)
# remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)
library(ggalluvial)
library(readxl)
library(ComplexHeatmap)
library(circlize)

# Climate Regulation ----

top5_CR_parameters     = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/top5_CR_parameters.csv")
top5_CR_parameters_red = top5_CR_parameters %>% select(type,rank,Agricultural_pertinence_Parameter,
                                                       Forest_pertinence_Parameter,
                                                       Urban_pertinence_Parameter,
                                                       Agricultural_pertinence_value,
                                                       Forest_pertinence_value,
                                                       Urban_pertinence_value)


# Reshape your table into long format

df_long = top5_CR_parameters_red %>%
  pivot_longer(
    cols = ends_with("_Parameter"),
    names_to = "metric",
    values_to = "parameter"
  ) %>%
  mutate(
    metric = sub("_pertinence_Parameter", "", metric),
    parameter = str_trim(parameter)
  ) %>%
  filter(!is.na(parameter))

# Add the weights

df_long = df_long %>%
  left_join(
    df_long %>%
      pivot_longer(
        cols = ends_with("_value"),
        names_to = "metric_value",
        values_to = "value"
      ) %>%
      mutate(metric_value = sub("_pertinence_value", "", metric_value)),
    by = c("type", "rank", "metric" = "metric_value")
  ) %>%
  filter(!is.na(value))

# Build the node list

nodes = data.frame(
  name = unique(c(df_long$type, df_long$parameter.x, df_long$metric))
)

# Build the links (flows)

# type → parameter
links1 = df_long %>%
  transmute(
    source = match(type, nodes$name) - 1,
    target = match(parameter.x, nodes$name) - 1,
    value = value
  )

# parameter → metric
links2 = df_long %>%
  transmute(
    source = match(parameter.x, nodes$name) - 1,
    target = match(metric, nodes$name) - 1,
    value = value
  )

links = bind_rows(links1, links2)

# Plot the Sankey diagram

df_wide <- df_long %>%
  select(parameter.x, type, metric, value) %>%
  group_by(parameter.x, type, metric) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = metric,
    values_from = value,
    values_fill = 0
  )

CR_sankey = ggplot(df_wide,
       aes(axis1 = parameter.x,
           axis2 = Agricultural,
           axis3 = Forest,
           axis4 = Urban,
           y = Agricultural + Forest + Urban)) +
  geom_alluvium(aes(fill = type), width = 1/12) +
  geom_stratum(width = 1/12, fill = NA, color = NA, show.legend = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.2) +
  scale_x_discrete(limits = c("Parameter", "Agricultural", "Forest", "Urban")) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", size = 0.2)
  )

ggsave(
  "C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/BIOSIS_tool/figures/CR_sankey.pdf",
  CR_sankey,
  device = cairo_pdf,
  bg = "transparent",
  width = 10,
  height = 6
)

# Habitat Provision ----

top5_HP_parameters     = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/top5_HP_parameters.csv")
top5_HP_parameters_red = top5_HP_parameters %>% select(type,rank,Agricultural_pertinence_Parameter,
                                                       Forest_pertinence_Parameter,
                                                       Urban_pertinence_Parameter,
                                                       Agricultural_pertinence_value,
                                                       Forest_pertinence_value,
                                                       Urban_pertinence_value)


# Reshape your table into long format

df_long = top5_HP_parameters_red %>%
  pivot_longer(
    cols = ends_with("_Parameter"),
    names_to = "metric",
    values_to = "parameter"
  ) %>%
  mutate(
    metric = sub("_pertinence_Parameter", "", metric),
    parameter = str_trim(parameter)
  ) %>%
  filter(!is.na(parameter))

# Add the weights

df_long = df_long %>%
  left_join(
    df_long %>%
      pivot_longer(
        cols = ends_with("_value"),
        names_to = "metric_value",
        values_to = "value"
      ) %>%
      mutate(metric_value = sub("_pertinence_value", "", metric_value)),
    by = c("type", "rank", "metric" = "metric_value")
  ) %>%
  filter(!is.na(value))

# Build the node list

nodes = data.frame(
  name = unique(c(df_long$type, df_long$parameter.x, df_long$metric))
)

# Build the links (flows)

# type → parameter
links1 = df_long %>%
  transmute(
    source = match(type, nodes$name) - 1,
    target = match(parameter.x, nodes$name) - 1,
    value = value
  )

# parameter → metric
links2 = df_long %>%
  transmute(
    source = match(parameter.x, nodes$name) - 1,
    target = match(metric, nodes$name) - 1,
    value = value
  )

links = bind_rows(links1, links2)

# Plot the Sankey diagram

df_wide <- df_long %>%
  select(parameter.x, type, metric, value) %>%
  group_by(parameter.x, type, metric) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = metric,
    values_from = value,
    values_fill = 0
  )

HP_sankey = ggplot(df_wide,
                   aes(axis1 = parameter.x,
                       axis2 = Agricultural,
                       axis3 = Forest,
                       axis4 = Urban,
                       y = Agricultural + Forest + Urban)) +
  geom_alluvium(aes(fill = type), width = 1/12) +
  geom_stratum(width = 1/12, fill = NA, color = NA, show.legend = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.2) +
  scale_x_discrete(limits = c("Parameter", "Agricultural", "Forest", "Urban")) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", size = 0.2)
  )

ggsave(
  "C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/BIOSIS_tool/figures/HP_sankey.pdf",
  HP_sankey,
  device = cairo_pdf,
  bg = "transparent",
  width = 10,
  height = 6
)

# Nutrient Regulation ----

top5_NC_parameters     = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/top5_NC_parameters.csv")
top5_NC_parameters_red = top5_NC_parameters %>% select(type,rank,Agricultural_pertinence_Parameter,
                                                       Forest_pertinence_Parameter,
                                                       Urban_pertinence_Parameter,
                                                       Agricultural_pertinence_value,
                                                       Forest_pertinence_value,
                                                       Urban_pertinence_value)


# Reshape your table into long format

df_long = top5_NC_parameters_red %>%
  pivot_longer(
    cols = ends_with("_Parameter"),
    names_to = "metric",
    values_to = "parameter"
  ) %>%
  mutate(
    metric = sub("_pertinence_Parameter", "", metric),
    parameter = str_trim(parameter)
  ) %>%
  filter(!is.na(parameter))

# Add the weights

df_long = df_long %>%
  left_join(
    df_long %>%
      pivot_longer(
        cols = ends_with("_value"),
        names_to = "metric_value",
        values_to = "value"
      ) %>%
      mutate(metric_value = sub("_pertinence_value", "", metric_value)),
    by = c("type", "rank", "metric" = "metric_value")
  ) %>%
  filter(!is.na(value))

# Build the node list

nodes = data.frame(
  name = unique(c(df_long$type, df_long$parameter.x, df_long$metric))
)

# Build the links (flows)

# type → parameter
links1 = df_long %>%
  transmute(
    source = match(type, nodes$name) - 1,
    target = match(parameter.x, nodes$name) - 1,
    value = value
  )

# parameter → metric
links2 = df_long %>%
  transmute(
    source = match(parameter.x, nodes$name) - 1,
    target = match(metric, nodes$name) - 1,
    value = value
  )

links = bind_rows(links1, links2)

# Plot the Sankey diagram

df_wide <- df_long %>%
  select(parameter.x, type, metric, value) %>%
  group_by(parameter.x, type, metric) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = metric,
    values_from = value,
    values_fill = 0
  )

NC_sankey = ggplot(df_wide,
                   aes(axis1 = parameter.x,
                       axis2 = Agricultural,
                       axis3 = Forest,
                       axis4 = Urban,
                       y = Agricultural + Forest + Urban)) +
  geom_alluvium(aes(fill = type), width = 1/12) +
  geom_stratum(width = 1/12, fill = NA, color = NA, show.legend = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.2) +
  scale_x_discrete(limits = c("Parameter", "Agricultural", "Forest", "Urban")) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", size = 0.2)
  )

ggsave(
  "C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/BIOSIS_tool/figures/NC_sankey.pdf",
  NC_sankey,
  device = cairo_pdf,
  bg = "transparent",
  width = 10,
  height = 6
)

# Water Regulation ----

top5_WR_parameters     = read.csv("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/outputs/top5_WR_parameters.csv")
top5_WR_parameters_red = top5_WR_parameters %>% select(type,rank,Agricultural_pertinence_Parameter,
                                                       Forest_pertinence_Parameter,
                                                       Urban_pertinence_Parameter,
                                                       Agricultural_pertinence_value,
                                                       Forest_pertinence_value,
                                                       Urban_pertinence_value)


# Reshape your table into long format

df_long = top5_WR_parameters_red %>%
  pivot_longer(
    cols = ends_with("_Parameter"),
    names_to = "metric",
    values_to = "parameter"
  ) %>%
  mutate(
    metric = sub("_pertinence_Parameter", "", metric),
    parameter = str_trim(parameter)
  ) %>%
  filter(!is.na(parameter))

# Add the weights

df_long = df_long %>%
  left_join(
    df_long %>%
      pivot_longer(
        cols = ends_with("_value"),
        names_to = "metric_value",
        values_to = "value"
      ) %>%
      mutate(metric_value = sub("_pertinence_value", "", metric_value)),
    by = c("type", "rank", "metric" = "metric_value")
  ) %>%
  filter(!is.na(value))

# Build the node list

nodes = data.frame(
  name = unique(c(df_long$type, df_long$parameter.x, df_long$metric))
)

# Build the links (flows)

# type → parameter
links1 = df_long %>%
  transmute(
    source = match(type, nodes$name) - 1,
    target = match(parameter.x, nodes$name) - 1,
    value = value
  )

# parameter → metric
links2 = df_long %>%
  transmute(
    source = match(parameter.x, nodes$name) - 1,
    target = match(metric, nodes$name) - 1,
    value = value
  )

links = bind_rows(links1, links2)

# Plot the Sankey diagram

df_wide <- df_long %>%
  select(parameter.x, type, metric, value) %>%
  group_by(parameter.x, type, metric) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from = metric,
    values_from = value,
    values_fill = 0
  )

WR_sankey = ggplot(df_wide,
                   aes(axis1 = parameter.x,
                       axis2 = Agricultural,
                       axis3 = Forest,
                       axis4 = Urban,
                       y = Agricultural + Forest + Urban)) +
  geom_alluvium(aes(fill = type), width = 1/12) +
  geom_stratum(width = 1/12, fill = NA, color = NA, show.legend = FALSE) +
  geom_text(stat = "stratum", aes(label = after_stat(stratum)), size = 2.2) +
  scale_x_discrete(limits = c("Parameter", "Agricultural", "Forest", "Urban")) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", size = 0.2)
  )

ggsave(
  "C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/BIOSIS_tool/figures/WR_sankey.pdf",
  WR_sankey,
  device = cairo_pdf,
  bg = "transparent",
  width = 10,
  height = 6
)

# Test Heatmap ----

process_scores      = read_excel("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/process_scores.xlsx")
process_heatmap     = process_scores %>% select(Function,Subfunction,level,name,Full_medians,
                                                Agr_medians,Agr_ATC_medians,Agr__BOR_medians,
                                                Agr__CON_medians,Agr__MDN_medians,Agr__MDD_medians,
                                                Agr__PAN_medians,
                                                For_medians,For__ATC_medians,
                                                For__BOR_medians,For__CON_medians,
                                                For__MDN_medians,For__MDD_medians,
                                                For__PAN_medians,
                                                Urb_medians,Urb__ATC_medians,
                                                Urb__BOR_medians,Urb__CON_medians,
                                                Urb__MDN_medians,
                                                ATC_medians,BOR_medians,CON_medians,
                                                MDN_medians,MDS_medians,PAN_medians
                                                ) %>% 
  filter(level == "process" & Function == "Climate regulation")
# parameter_scores    = read_excel("C:/Users/lucia/OneDrive - Wageningen University & Research/Wageningen/Research_Projects/Benchmark/Logical_sieve_data/parameter_scores.xlsx")

# Plot
test.2 = as.matrix(process_heatmap[,5:30])
rownames(test.2) = process_heatmap$name

function_colors = c(
  "Decomposition" = "#525252",
  "Organic matter transfer" = "#238b45",
  "Organic matter stabilisation" = "#ae017e",
  "Biochemical transformations" = "#4292c6"
)

col_fun = colorRamp2(
  breaks = c(0, 2, 4),
  colors = c("blue", "yellow", "red")
)

Heatmap(
  test.2,
  col = col_fun,
  name = "Median",
  show_row_names = TRUE,
  show_column_names = TRUE,
  row_names_gp = gpar(fontsize = 6),
  column_names_gp = gpar(fontsize = 8),
  column_names_rot = 90,
  show_heatmap_legend = TRUE,
  right_annotation = rowAnnotation(Sub_function = process_heatmap$Subfunction,
                                   col = list(Sub_function = function_colors)),
  cluster_columns = FALSE,
  cluster_rows = FALSE
)



