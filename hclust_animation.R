# Example agglomerative hierarchical clustering
library(tidyverse)
library(gganimate)


# Generate random data
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
labels <- rownames(mat)
idx <- order.dendrogram(dendro)


# Create data for animation
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
data <- lst %>%
    setNames(seq_along(lst)) %>%
    bind_rows(.id = "Iteration") %>%
    mutate(Iteration = sprintf("Step %s", Iteration))


# Static base plot
gg_base <- ggplot(data, aes(x, id, fill = value, group = variable)) +
    geom_tile() +
    geom_text(
        data = data %>%
            group_by(Iteration) %>%
            distinct(variable, x) %>%
            ungroup() %>%
            mutate(value = 0, id = ""),
        aes(x = x, y = -0.5, label = variable, group = variable),
        hjust = 0.5) +
    theme_minimal() +
    coord_cartesian(clip = "off") +
    scale_fill_distiller(palette = "RdBu") +
    theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 12),
        legend.position = "bottom") +
    labs(x = "", y = "")


# Make sure everything looks ok before animating
#gg_base +
#        facet_wrap(~ Iteration, scale = "free_x")


# Animate
anim_plot <- gg_base +
    ggtitle("{closest_state}") +
    transition_states(Iteration, transition_length = 1, state_length = 1) +
    exit_fly(x_loc = 0, y_loc = 0) +
    enter_fly(x_loc = 0, y_loc = 0) +
    theme(plot.title = element_text(size = 16))


# Save animation as animated GIF
animate(
    anim_plot,
    width = 960, width = 540, units = "px",
    renderer = gifski_renderer("animation.gif"))
