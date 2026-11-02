# Genereate coords
library(tidyverse)

# for some fixed real a, b
a <- 2
b <- 3
theta <- seq(0, 6 * pi, 0.01)
r <- a + b * theta
df <- tibble(x = r * cos(theta), y = r * sin(theta)) # Cartesian coords

df

ggplot(df, aes(x, y)) +
  geom_point(col = 'black') +
  coord_equal() +
  theme_void()


# Mock data that ends close to staring point

# add a row_number
df1 <- df |>
  mutate(row = row_number())


# Create a flipped data
df2 <- df1 |>
  mutate(row = row + max(row)) |>
  arrange(desc(row)) |>
  mutate(row = row_number() + max(df1$row))

# move the flipped data a tiny bit up
df2_moved <- df2 |>
  mutate(
    y = y + 0.5,
    x = x + 0.5
  )

# Alternative, generate data with theta multiplied
# for some fixed real a, b
a <- 2
b <- 3
theta <- seq(0, 6 * pi, 0.01)
r <- a + b * theta
df3 <- tibble(x = r * cos(theta) * 1.01, y = r * sin(theta) * 1.01) # Cartesian coords


ggplot(df1, aes(x, y)) +
  geom_path(col = 'black', linewidth = 5) +
  geom_path(data = df2_moved, col = 'blue', linewidth = 1) +
  geom_path(data = df3, col = 'red', linewidth = 1) +
  coord_equal() +
  theme_void()


# Problems
# df2 - adding same value causes outside line to cross inside line when
# df3 - distance becomes greater as theta increases

# lets try to moved it based on sign
df4 <- df2 |>
  mutate(
    x = case_when(
      x > 0 ~ x + 0.5,
      x < 0 ~ x - 0.5,
      .default = x
    ),
    y = case_when(
      y > 0 ~ y + 0.5,
      y < 0 ~ y - 0.5,
      .default = y
    )
  )


ggplot(df1, aes(x, y)) +
  geom_path(col = 'black', linewidth = 5) +
  #geom_path(data = df2_moved, col = 'blue', linewidth = 1) +
  #geom_path(data = df3, col = 'red', linewidth = 1) +
  geom_path(data = df4, col = 'green', linewidth = 1) +
  coord_equal()


# Problem
# df4 - jitter when x and y cross 0, otherwise reasonable

# preliminary conclusion
# spiral coords where the spiral is a polygon with constant width is hard...

# 11/8 solution for now
# Create .png as background image
# use substraction method to remove pixels inside/outside background drawing
# Then calculate area using pdr_get_alpha_area

spiral_out <- ggplot(df1, aes(x, y)) +
  geom_path(col = 'black', linewidth = 1.5, lineend = "round") +
  coord_equal(xlim = c(-65, 75), ylim = c(-70, 65)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = "white"))

ggsave(
  filename = "inst/extdata/spiral.png",
  plot = spiral_out,
  width = 500,
  height = 500,
  units = "px"
)


spiral_out_grey <- ggplot(df1, aes(x, y)) +
  geom_path(col = 'grey50', linewidth = 1.5, lineend = "round") +
  coord_equal(xlim = c(-65, 75), ylim = c(-70, 65)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = "white"))

ggsave(
  filename = "inst/extdata/spiral_grey.png",
  plot = spiral_out_grey,
  width = 500,
  height = 500,
  units = "px"
)
