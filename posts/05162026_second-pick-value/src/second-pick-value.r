#load packages
library(tidyverse)
library(rvest)

# load data
## function to pull draft tables from bbref
function_load_draft_data <- function(year) {

  url <- paste0(
    "https://www.basketball-reference.com/draft/NBA_",
    year,
    ".html"
  )

  df <- url |>
    read_html() |>
    html_table() |>
    pluck(1)

  colnames(df) <- make.unique(as.character(unlist(df[1, ])))

  df <- df[-1, ] |>
    mutate(
      draft_year = year,
      pick = as.numeric(Pk),
      yrs = ifelse(Yrs == "", 0, as.numeric(Yrs)),
      win_shares = ifelse(WS == "", 0, as.numeric(WS)),
      vorp = ifelse(VORP == "", 0, as.numeric(VORP))
    ) |>
    filter(
      !is.na(pick),
      Player != ""
    ) |>
    select(
      draft_year,
      pick,
      player = Player,
      yrs,
      win_shares,
      vorp
    )

}

draft_history <- map_dfr(
  2000:2025,
  function_load_draft_data
)

pick_win_shares <- draft_history |>
  group_by(pick) |>
  summarise(
    total_win_shares = sum(win_shares),
    avg_win_shares = mean(win_shares),
    med_win_shares = median(win_shares),
    #total_vorp = sum(vorp),
    #avg_vorp = mean(vorp),
    #med_vorp = median(vorp),
    .groups = "drop"
  ) |>
  mutate(
    label_avg = round(avg_win_shares, 1),
    label_med = round(med_win_shares, 1),
    highlight = ifelse(pick == 2, "yes", "no")
  ) |>
  filter(pick <= 30) # just first round

# chart for average win shares
chart_pick_win_shares <- 
  pick_win_shares |>
  ggplot(aes(x = as.factor(pick), y = avg_win_shares)) +
  geom_col(aes(fill = highlight)) +
  geom_text(
    aes(label = label_avg),
    vjust = -0.5,
    size = 5,
    color = "black",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "yes" = "#3E2680",
      "no" = "darkgray"
    )
  ) +
  labs(
    title = "Average Win Shares by First-Round Draft Pick",
    subtitle = "Data from 2000-2025 NBA Drafts",
    x = "Pick"
  ) +
  theme(
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14, face = "bold", margin = margin(t = -20)),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    plot.title.position = "plot",
    legend.position = "none",
    #panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, linetype = "dashed"),
  )

ggsave(
  "posts/05162026_second-pick-value/images/pick_win_shares.png",
  chart_pick_win_shares,
  width = 18,
  height = 8
)

# chart for median win shares
chart_pick_win_shares_median <- 
  pick_win_shares |>
  ggplot(aes(x = as.factor(pick), y = med_win_shares)) +
  geom_col(aes(fill = highlight)) +
  geom_text(
    aes(label = label_med),
    vjust = -0.5,
    size = 5,
    color = "black",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "yes" = "#3E2680",
      "no" = "darkgray"
    )
  ) +
  labs(
    title = "Median Win Shares by First-Round Draft Pick",
    subtitle = "Data from 2000-2025 NBA Drafts",
    x = "Pick"
  ) +
  theme(
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14, face = "bold", margin = margin(t = -20)),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    plot.title.position = "plot",
    legend.position = "none",
    #panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, linetype = "dashed"),
  )

ggsave(
  "posts/05162026_second-pick-value/images/pick_win_shares_median.png",
  chart_pick_win_shares_median,
  width = 18,
  height = 8
)

# create chart for average rank within draft class
pick_win_shares_rank <- draft_history |>
  filter(yrs > 0) |> # remove players who did not play in the NBA
  group_by(draft_year) |>
  mutate(win_share_rank = dense_rank(desc(win_shares))) |>
  ungroup() |>
  group_by(pick) |>
  summarise(
    avg_rank = mean(win_share_rank),
    med_rank = median(win_share_rank),
    .groups = "drop"
  ) |>
  filter(pick <= 30) |> # just first round
  mutate(
    label_avg = round(avg_rank, 1),
    label_med = round(med_rank, 1),
    highlight = ifelse(pick == 2, "yes", "no")
  )

chart_pick_win_rank <- 
  pick_win_shares_rank |>
  ggplot(aes(x = as.factor(pick), y = avg_rank)) +
  geom_col(aes(fill = highlight)) +
  geom_text(
    aes(label = label_avg),
    vjust = -0.5,
    size = 5,
    color = "black",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "yes" = "#3E2680",
      "no" = "darkgray"
    )
  ) +
  labs(
    title = "Average Rank within Draft Class by First-Round Draft Pick",
    subtitle = "Data from 2000-2025 NBA Drafts",
    x = "Pick"
  ) +
  theme(
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14, face = "bold", margin = margin(t = -20)),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    plot.title.position = "plot",
    legend.position = "none",
    #panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, linetype = "dashed"),
  )

ggsave(
  "posts/05162026_second-pick-value/images/pick_win_rank.png",
  chart_pick_win_rank,
  width = 18,
  height = 8
)

# now do by median
chart_pick_win_rank_median <- 
  pick_win_shares_rank |>
  ggplot(aes(x = as.factor(pick), y = med_rank)) +
  geom_col(aes(fill = highlight)) +
  geom_text(
    aes(label = label_med),
    vjust = -0.5,
    size = 5,
    color = "black",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "yes" = "#3E2680",
      "no" = "darkgray"
    )
  ) +
  labs(
    title = "Median Rank within Draft Class by First-Round Draft Pick",
    subtitle = "Data from 2000-2025 NBA Drafts",
    x = "Pick"
  ) +
  theme(
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 15)),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 14, face = "bold", margin = margin(t = -20)),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14, face = "italic"),
    plot.title.position = "plot",
    legend.position = "none",
    #panel.grid.major.x = element_line(color = "gray", linewidth = 0.5, linetype = "dashed"),
  )

ggsave(
  "posts/05162026_second-pick-value/images/pick_win_rank_median.png",
  chart_pick_win_rank_median,
  width = 18,
  height = 8
)