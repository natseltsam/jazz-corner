#load packages
library(tidyverse)
library(rvest)
library(scales)
library(stringi)
library(gt)
library(tools)

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
    # create name_id
    mutate(
      player_clean = stri_trans_general(Player, "Latin-ASCII"),
      player_clean = str_remove_all(player_clean, "[^A-Za-z ]"),
      first_name = word(player_clean, 1),
      last_name = word(player_clean, 2),
      base_id = paste0(
        str_sub(str_to_lower(last_name), 1, 5),
        str_sub(str_to_lower(first_name), 1, 2)
      )
    ) |>
    group_by(base_id) |>
    mutate(
      player_id = if (n() == 1) {
        base_id
      } else {
        paste0(base_id, row_number())
      }
    ) |>
    ungroup() |>
    ## finish name_id
    filter(
      !is.na(pick),
      Player != ""
    ) |>
    select(
      draft_year,
      pick,
      player = Player,
      base_id,
      yrs,
      win_shares,
      vorp
    )

}

draft_history <- map_dfr(
  2000:2025,
  function_load_draft_data
)

draft_history <- draft_history |>
  mutate(
    player_clean = stri_trans_general(player, "Latin-ASCII"),
    player_clean = str_remove_all(player_clean, "[^A-Za-z ]"),
    first_name = word(player_clean, 1),
    last_name = word(player_clean, -1),
    base_id = paste0(
      str_sub(str_to_lower(last_name), 1, 5),
      str_sub(str_to_lower(first_name), 1, 2)
    )
  ) |>
  group_by(base_id) |>
  mutate(
    player_id = if (n() == 1) {
      base_id
    } else {
      paste0(base_id, row_number())
    }
  ) |>
  ungroup()

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

# try to identify boom and bust players
boom_bust <- draft_history |>
  filter(
    pick <= 14, # only lottery
    draft_year <= year(Sys.Date()) - 4 # must make it four years
  ) |>
  mutate(
    ws_per_year = win_shares / yrs
  ) |>
  mutate(
    mean_ws = mean(ws_per_year, na.rm = TRUE),
    sd_ws = sd(ws_per_year, na.rm = TRUE),
    boom_bust = case_when(
      yrs == 0 ~ "mega_bust",
      ws_per_year < mean_ws - 1 * sd_ws ~ "mega_bust",
      ws_per_year < mean_ws - 0.5 * sd_ws ~ "bust",
      ws_per_year < mean_ws + 0.5 * sd_ws ~ "role_player",
      ws_per_year < mean_ws + 1 * sd_ws ~ "boom",
      .default = "mega_boom"
    )
  ) |>
  group_by(
    pick,
    boom_bust
  ) |>
  summarise(
    count = n(),
    .groups = "drop"
  ) |>
  group_by(pick) |>
  mutate(
    prop = count / sum(count),
    boom_bust = factor(
      boom_bust,
      levels = c("mega_boom", "boom", "role_player", "bust", "mega_bust")
    ),
    label = percent(prop, accuracy = 0.1)
  )

chart_boom_bust <- boom_bust |>
  ggplot(aes(x = as.factor(pick), y = prop, fill = boom_bust)) +
  geom_col() +
  geom_text(
    aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 5,
    color = "black",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "mega_boom" = "#2E8B57",
      "boom" = "#66CDAA",
      "role_player" = "darkgray",
      "bust" = "#FF6347",
      "mega_bust" = "#8B0000"
    ),
    labels = c(
      "mega_boom" = "Mega Boom",
      "boom" = "Boom",
      "role_player" = "Role Player",
      "bust" = "Bust",
      "mega_bust" = "Mega Bust"
    )
  ) +
  labs(
    title = "Proportion of Boom and Bust Players by Lottery Pick",
    subtitle = "Data from 2000-2022 NBA Drafts",
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
    legend.title = element_blank(),
    legend.text = element_text(size = 14, face = "bold"),
  )

ggsave(
  "posts/05162026_second-pick-value/images/chart_boom_bust.png",
  chart_boom_bust,
  width = 18,
  height = 10
)

# try to pull in headshots
pick_examples <- draft_history |>
  filter(
    draft_year <= year(Sys.Date()) - 4,
    pick <= 14,
    yrs > 0
  ) |>
  mutate(
    ws_per_year = win_shares / yrs
  ) |>
  group_by(pick) |>
  arrange(ws_per_year, .by_group = TRUE) |>
  mutate(
    middle_dist = abs(row_number() - (n() + 1) / 2)
  )

pick_examples_combined <- bind_rows(
  pick_examples |>
    slice_min(ws_per_year, n = 3, with_ties = FALSE) |>
    mutate(category = "Bottom 3"),

  pick_examples |>
    slice_max(ws_per_year, n = 3, with_ties = FALSE) |>
    mutate(category = "Top 3"),

  pick_examples |>
    slice_min(middle_dist, n = 3, with_ties = FALSE) |>
    mutate(category = "Middle 3")
) |>
  ungroup() |>
  mutate(category = factor(category, levels = c("Top 3", "Middle 3", "Bottom 3")))

headshot_paths <- tibble(
  path = list.files(
    "posts/images/headshots",
    full.names = TRUE
  )
) |>
  mutate(
    player_id = file_path_sans_ext(basename(path))
  )

function_ranking_table <- function(pick, title, html) {
  table <- pick_examples_combined |>
    filter(pick == {{ pick }}) |>
    arrange(
      category,
      desc(ws_per_year)
    ) |>
    left_join(
      headshot_paths,
      by = "player_id"
    ) |>
    mutate(path = ifelse(is.na(path), "posts/images/headshots/placeholder.jpg", path)) |>
    select(
      category,
      path,
      player,
      draft_year,
      yrs,
      win_shares,
      ws_per_year
    ) |>
    gt(
      groupname_col = "category",
      row_group_as_column = TRUE
    ) |>
    text_transform(
      locations = cells_body(columns = path),
      fn = function(x) {
        local_image(x, height = px(75))
      }
    ) |>
    tab_header(
      title = paste0("Top, Middle, and Bottom 3 ", title, " Overall Picks"),
      subtitle = "Data from 2000-2022 NBA Drafts"
    ) |>
    fmt_number(
      columns = c(win_shares, ws_per_year),
      decimals = 1
    ) |>
    cols_label(
      path = "",
      player = "",
      draft_year = "Draft Year",
      yrs = "Experience",
      win_shares = "Total Win Shares",
      ws_per_year = "Win Shares per Year"
    ) |>
    cols_align(
      align = "center",
      columns = c(path, draft_year, yrs, win_shares, ws_per_year)
    ) |>
    cols_width(
      category ~ px(100),
      path ~ px(100),
      player ~ px(150),
      draft_year ~ px(75),
      yrs ~ px(75),
      win_shares ~ px(100),
      ws_per_year ~ px(100)
    ) |>
    tab_style(
      style = list(
        cell_text(
          weight = "bold",
          size = px(16),
          v_align = "middle",
          align = "center"
        )
      ),
      locations = cells_row_groups()
    ) |>
    tab_style(
      style = list(
        cell_text(
          weight = "bold",
          size = px(12),
          v_align = "middle"
        )
      ),
      locations = cells_column_labels()
    ) |>
    tab_style(
      style = list(
        cell_text(
          weight = "bold",
          size = px(15)
        )
      ),
      locations = cells_body()
    ) |>
    tab_style(
      style = list(
        cell_text(
          weight = "bold",
          size = px(20)
        )
      ),
      locations = cells_title(groups = "title")
    ) |>
    tab_style(
      style = list(
        cell_fill(
          color = "green",
          alpha = 0.1
        )
      ),
      locations = list(
        cells_row_groups(groups = "Top 3"),
        cells_body(rows = category == "Top 3")
      )
    ) |>
    tab_style(
      style = list(
        cell_fill(
          color = "yellow",
          alpha = 0.1
        )
      ),
      locations = list(
        cells_row_groups(groups = "Middle 3"),
        cells_body(rows = category == "Middle 3")
      )
    ) |>
    tab_style(
      style = list(
        cell_fill(
          color = "red",
          alpha = 0.1
        )
      ),
      locations = list(
        cells_row_groups(groups = "Bottom 3"),
        cells_body(rows = category == "Bottom 3")
      )
    )

  gtsave(
    table,
    paste0("posts/05162026_second-pick-value/images/", html, "_pick_table.html")
  )
}

table_first_pick <- function_ranking_table(1, "First", "first")
table_second_pick <- function_ranking_table(2, "Second", "second")
table_third_pick <- function_ranking_table(3, "Third", "third")
table_fourth_pick <- function_ranking_table(4, "Fourth", "fourth")
table_fifth_pick <- function_ranking_table(5, "Fifth", "fifth")
table_sixth_pick <- function_ranking_table(6, "Sixth", "sixth")
table_seventh_pick <- function_ranking_table(7, "Seventh", "seventh")
table_eighth_pick <- function_ranking_table(8, "Eighth", "eighth")
table_ninth_pick <- function_ranking_table(9, "Ninth", "ninth")
table_tenth_pick <- function_ranking_table(10, "Tenth", "tenth")
table_eleventh_pick <- function_ranking_table(11, "Eleventh", "eleventh")
table_twelfth_pick <- function_ranking_table(12, "Twelfth", "twelfth")
table_thirteenth_pick <- function_ranking_table(13, "Thirteenth", "thirteenth")
table_fourteenth_pick <- function_ranking_table(14, "Fourteenth", "fourteenth")