#CODED IN R
library(ggplot2)
library(dplyr)
library(tibble)
library(grid)

set.seed(123)

# Simulated values
aux_time <- rgamma(1000, shape = 2.5, scale = 6)
aux_time <- pmax(aux_time, 1)

emerg_time <- rgamma(1000, shape = 2.5, scale = 8)
emerg_time <- pmin(emerg_time, 45)

aux_df <- tibble(
  time = aux_time,
  type = "Aux engines\n(load-sharing)"
)

emerg_df <- tibble(
  time = emerg_time,
  type = "Emergency\ngenerator (≤45 s)"
)

# Sample points for display
aux_pts <- aux_df %>%
  slice_sample(n = 80) %>%
  mutate(y = -0.0025)

emerg_pts <- emerg_df %>%
  slice_sample(n = 80) %>%
  mutate(y = -0.005)

p <- ggplot() +
  # Aux density
  geom_density(
    data = aux_df,
    aes(x = time, fill = type, colour = type),
    alpha = 0.30,
    linewidth = 1,
    trim = TRUE
  ) +
  # Emergency generator density with hard bounds
  geom_density(
    data = emerg_df,
    aes(x = time, fill = type, colour = type),
    alpha = 0.30,
    linewidth = 1,
    trim = TRUE,
    bounds = c(0, 45)
  ) +
  # Rug marks
  geom_rug(
    data = aux_df,
    aes(x = time, colour = type),
    sides = "b",
    alpha = 0.15,
    length = unit(0.025, "npc")
  ) +
  geom_rug(
    data = emerg_df,
    aes(x = time, colour = type),
    sides = "b",
    alpha = 0.15,
    length = unit(0.025, "npc")
  ) +
  # Visible points
  geom_point(
    data = aux_pts,
    aes(x = time, y = y, colour = type),
    size = 1.7,
    alpha = 0.75,
    position = position_jitter(width = 0.12, height = 0.00035, seed = 123)
  ) +
  geom_point(
    data = emerg_pts,
    aes(x = time, y = y, colour = type),
    size = 1.7,
    alpha = 0.75,
    position = position_jitter(width = 0.12, height = 0.00035, seed = 123)
  ) +
  geom_vline(xintercept = 45, linetype = "dashed", colour = "black", linewidth = 0.8) +
  annotate(
    "text",
    x = 45.2,
    y = 0.028,
    label = "45 s (SOLAS limit)",
    hjust = 1,
    size = 4
  ) +
  scale_x_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, by = 5),
    name = "Time to restore essential services (seconds)"
  ) +
  scale_y_continuous(
    name = "Density",
    limits = c(-0.007, NA),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_fill_manual(
    values = c(
      "Aux engines\n(load-sharing)" = "#0077BB",
      "Emergency\ngenerator (≤45 s)" = "#FF8800"
    ),
    name = "Recovery path"
  ) +
  scale_colour_manual(
    values = c(
      "Aux engines\n(load-sharing)" = "#0077BB",
      "Emergency\ngenerator (≤45 s)" = "#FF8800"
    ),
    guide = "none"
  ) +
  labs(
    title = "Blackout recovery time: two layers of safety",
    subtitle = "Sample points shown below.
    Emergency generator density bounded at 45 seconds"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    plot.title = element_text(size = 16, face = "bold"),
    panel.grid.minor = element_blank()
  )

print(p)

ggsave(
  "blackout_recovery_bounded.png",
  plot = p,
  width = 9,
  height = 5.5,
  dpi = 180,
  bg = "white"
)
