# Example agglomerative hierarchical clustering
library(tidyverse)
library(dendextend)
library(ggdendro)
library(cowplot)
library(gganimate)


set.seed(2020)
n_groups <- 10
n_obs <- 5
df <- data.frame(
    variable = rep(sample(LETTERS[1:n_groups]), each = n_obs),
    id = rep(1:n_obs, n_groups),
    value = sort(rnorm(n_groups * n_obs))) %>%
    arrange(variable, id) %>%
    mutate(id = sprintf("M%s", id))


# Convert to matrix data
mat <- df %>%
    pivot_wider(names_from = "id", values_from = "value") %>%
    column_to_rownames("variable") %>%
    as.matrix()

# Hierarchical clustering of standardised active power
hc <- hclust(d = dist(x = mat), method = "mcquitty")
dendro <- as.dendrogram(hc)

names_in_order <- row.names(mat)[order.dendrogram(dendro)]

# Extract dendrogram data from `dendrogram` object
data_dendro <- dendro %>% dendro_data()
df_segment <- segment(data_dendro)

# Create dendrogram grob
gg_dendro <- ggplot(df_segment) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
    theme_minimal() +
    scale_x_continuous(
        limits = c(0, length(names_in_order) + 1),
        expand = c(0, 0)) +
    theme(
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())

# Create heatmap grob
gg_heatmap <- df %>% mutate(x = match(variable, names_in_order)) %>%
    ggplot(aes(x, id, fill = value)) +
    geom_tile() +
    scale_fill_distiller(palette = "RdBu") +
    scale_x_continuous(
        limits = c(0, length(names_in_order) + 1),
        expand = c(0, 0),
        breaks = 1:length(names_in_order),
        labels = names_in_order) + 
    theme_minimal() +
    theme(
        panel.grid.minor.x = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "bottom") +
    guides(fill = guide_colourbar(
        title.position = "left", title.vjust = 0.8))

# Plot aligned dendrogram and heatmap
plot_grid(
    gg_dendro, gg_heatmap,
    ncol = 1, align = "v", rel_heights = c(0.3, 1.1))


labels <- rownames(mat)
idx <- order.dendrogram(dendro)

i <- 1
ord <- seq_along(idx)
print(labels[ord])
lst <- list()
lst[[1]] <- df %>% mutate(x = match(variable, labels))
while (i < length(idx)) {
    if (ord[i] == idx[i]) {
        i <- i + 1
    } else{
        j <- which(ord == idx[i])
        tmp <- ord[i]
        ord[i] <- idx[i]
        ord[j] <- tmp
        print(labels[ord])
        lst[[length(lst) + 1]] <- df %>% 
            mutate(x = match(variable, labels[ord]))
    }
}
lst <- lst %>% setNames(1:length(lst))
data <- lst %>% bind_rows(.id = "Iteration") %>%
    mutate(Iteration = sprintf("Step %s", Iteration))

gg_base <- ggplot(data, aes(x, id, fill = value, group = variable)) + 
    geom_tile() +
    geom_text(
        aes(x = x, y = -0.5, label = variable, group = variable), hjust = 0.5) +
    theme_minimal() +
    coord_cartesian(clip = "off") +
    scale_fill_distiller(palette = "RdBu") +
    theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom") +
    labs(x = "", y = "")

gg_base +
    facet_wrap(~ Iteration, scale = "free_x") 


anim_plot <- gg_base +
    ggtitle("{closest_state}") +
    transition_states(Iteration, transition_length = 1, state_length = 1) +
    exit_fly(x_loc = 0, y_loc = 0) + 
    enter_fly(x_loc = 0, y_loc = 0)

anim <- animate(anim_plot, height = 8, width = 12, res = 150, units = "in")
anim_save("animation.gif", anim)
