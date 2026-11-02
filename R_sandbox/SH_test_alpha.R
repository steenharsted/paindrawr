library(devtools)
library(tidyverse)

load_all()

col_alpha_data <- pdr_import_json("data-raw/col_alpha_01_to_10.json")

col_alpha_data |> pd_recreate_drawing()

grid::grid.newpage()
col_alpha_data |> pd_add_rgba_single() |> grid::grid.raster()


## Explore gglot

plot_data <- tibble(
    x = c(rep(1, 10), rep(2, 10)),
    y = c(1:10, 1:10),
    alpha = c(rep(0.1, 10), seq(0.1, 1, by = 0.1)),
    n = c(1:10, rep(1, 10))
) |>
    mutate(label = paste0(alpha, " * ", n)) |>
    uncount(n)

plot_data |>
    ggplot(aes(x = x, y = y, alpha = alpha)) +
    geom_point(color = "red", size = 40) +
    theme_void() +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_x_continuous(limits = c(0.5, 2.5)) +
    geom_text(aes(label = label), alpha = 1)


# Resulting alpha should follow this formula
# α = F_alpha + B_alpha * (1 - F_alpha)

plot_data_extra <- tibble(
    x = rep(1.5, 10),
    y = c(1:10),
    alpha_base = rep(0.1, 10),
    nn = 1:10,
    n = rep(1, 10)
) |>
    mutate(
        alpha = 1 - (1 - alpha_base)^nn,
        label = paste0(round(alpha, 3), " * ", n),

        # Other ways of calucating alpha (for my understanding)
        alpha_purr = purrr::accumulate(alpha_base, \(bg, fg) {
            fg + bg * (1 - fg)
        })
    )

plot_data_extra

bind_rows(plot_data, plot_data_extra) |>
    ggplot(aes(x = x, y = y, alpha = alpha)) +
    geom_point(color = "red", size = 40) +
    theme_void() +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_x_continuous(limits = c(0.5, 2.5)) +
    geom_text(aes(label = label), alpha = 1)


# Lets try compare alphas

## Explore gglot

plot_data_cum <- tibble(
    x = rep(1, 10),
    y = 1:10,
    alpha = rep(0.1, 10),
    n = 1:10
) |>
    uncount(n)

plot_cum <- plot_data_cum |>
    ggplot(aes(x = x, y = y, alpha = alpha)) +
    geom_point(color = "red", size = 40, stroke = 0, shape = 19) +
    theme_void() +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_x_continuous(limits = c(0.5, 2.5))


plot_data_mul <- tibble(
    x = rep(1, 10),
    y = 1:10,
    alpha_base = rep(0.1, 10),
    nn = 1:10,
    n = rep(1, 10)
) |>
    mutate(
        alpha = 1 - (1 - alpha_base)^nn,
    )

plot_data_mul

plot_mul <- plot_data_mul |>
    ggplot(aes(x = x, y = y, alpha = alpha)) +
    geom_point(color = "red", size = 40, stroke = 0, shape = 19) +
    theme_void() +
    scale_size_identity() +
    scale_alpha_identity() +
    scale_x_continuous(limits = c(0.5, 2.5))


plot_cum
plot_mul

ggsave("R_sandbox/cum.png", plot = plot_cum)
ggsave("R_sandbox/mul.png", plot = plot_mul)

# Read Arrays
cum_png <- png::readPNG("R_sandbox/cum.png")
mul_png <- png::readPNG("R_sandbox/mul.png")

identical(cum_png[,, 4], mul_png[,, 4])


# Compare with tolerance
tolerance <- 1e-5
comparison <- abs(cum_png[,, 4] - mul_png[,, 4]) < tolerance
all(comparison) # Should be TRUE if all values are close enough

# Extract all alpha values for comparison
diff <- cum_png[,, 4] - mul_png[,, 4]
print(diff[abs(diff) > 1e-5])

diff |> as.double() |> quantile(probs = seq(0, 1, 0.05))


# How many dots will it take to reach 90% satutation?

tibble(
    alpha = seq(0.05, 0.9, 0.05)
) |>
    mutate(
        n_of_overlaps_to_reach_0.9 = log(0.1) / log(1 - alpha)
    )


## Functions

# Usage

pd <- pd_import_json(c("data-raw/two_geoms.json", "data-raw/four_geoms.json"))
pd <- pd |> mutate(rgba = pd_add_rgba(pd))
pd


pd |>
    mutate(
        intensity = pd_alpha_intensity(rgba),
        area = pd_alpha_area(rgba),
        area_over_50 = pd_alpha_area(rgba, alpha_range = c(0.5, 1))
    )
