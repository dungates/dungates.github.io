library(ggplot2)
library(tibble)

fake_data <- tibble::tibble(
  x = 1:10,
) |>
  dplyr::mutate(
    y = x^2,
    x2 = lead(x),
    y2 = lead(y)
  )

fake_data

fake_end_data <- tibble::tibble(
  x = 10,
  x2 = 10.01,
  y = 100,
  y2 = 100.2
)

ggplot(fake_data, aes(x = x, y = y)) +
  geom_segment(aes(xend = x2, yend = y2),
               linewidth = 3) +
  geom_segment(data = fake_end_data,
               aes(x = x, y = y, xend = x2, yend = y2),
               arrow = grid::arrow(length = unit(0.4, "inches")),
               linewidth = 3) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1)
  ) +
  labs(title = "Interest in Duncan's Blog",
       caption = "*Chart inspired by the creative ingenuity of SoftBank, see their WeWork deck for more",
       x = "Quality of Writing",
       y = "% of Users Interested") +
  theme_duncan(
    caption_family = "Fira Sans Italic"
  )

ggsave("imgs/banner.png",
       bg = "white",
       height = 12,
       width = 10)
