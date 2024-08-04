#library(janitor)
#library(magick)
library(devtools)
library(extrafont)
library(cowplot)
library(paletteer)
library(tidyverse)
#library(jsonlite)
library(httr)
library(ggplot2)
library(dplyr)
library(ggtext)
library(reticulate)
library(gridExtra)
library(hexbin)
library(formattable)
library(prismatic)
library(gtable)
library(gt)
library(gtExtras)

#Game codes of 2023/24 LBA RS
partite = c(24086, 24322, 24321, 24083, 24320, 24087, 24085, 24084, 24093, 24094, 
            24090, 24095, 24092, 24089, 24088, 24091, 24096, 24099, 24100, 24098, 
            24097, 24102, 24103, 24101, 24107, 24109, 24108, 24110, 24105, 24106, 
            24111, 24104, 24113, 24115, 24119, 24112, 24117, 24114, 24116, 24118, 
            24121, 24127, 24124, 24120, 24123, 24126, 24125, 24122, 24128, 24134, 
            24131, 24135, 24130, 24133, 24132, 24129, 24142, 24143, 24140, 24141, 
            24138, 24137, 24139, 24136, 24148, 24147, 24144, 24150, 24151, 24145, 
            24149, 24146, 24158, 24154, 24153, 24159, 24155, 24157, 24156, 24152, 
            24163, 24162, 24167, 24161, 24165, 24160, 24164, 24166, 24170, 24168, 
            24172, 24171, 24173, 24175, 24174, 24169, 24177, 24181, 24182, 24178,
            24183, 24180, 24179, 24176, 24189, 24187, 24185, 24190, 24186, 24191, 
            24188, 24184, 24196, 24192, 24195, 24198, 24197, 24199, 24194, 24193, 
            24204, 24203, 24205, 24201, 24202, 24206, 24207, 24200, 24214, 24215, 
            24212, 24213, 24208, 24209, 24210, 24211, 24220, 24223, 24218, 24217, 
            24221, 24216, 24219, 24222, 24229, 24225, 24224, 24231, 24228, 24230, 
            24227, 24226, 24235, 24236, 24233, 24239, 24237, 24232, 24234, 24238, 
            24242, 24241, 24245, 24240, 24244, 24247, 24246, 24243, 24253, 24250, 
            24254, 24251, 24255, 24248, 24252, 24249, 24262, 24258, 24256, 24263, 
            24257, 24261, 24259, 24260, 24270, 24269, 24268, 24266, 24267, 24271, 
            24264, 24265, 24275, 24273, 24279, 24278, 24276, 24274, 24277, 24272, 
            24281, 24283, 24286, 24284, 24285, 24282, 24280, 24287, 24293, 24291, 
            24292, 24290, 24288, 24289, 24294, 24295, 24298, 24301, 24300, 24297, 
            24299, 24302, 24303, 24296, 24307, 24310, 24304, 24308, 24309, 24306,
            24305, 24311,24315, 24318, 24317, 24314, 24312, 24319, 24313, 24316)


#HEX CHART FUNCTIONS---------------------------------------------------------------------
hex_bounds <- function(x, binwidth) {
  c(
    plyr::round_any(min(x), binwidth, floor) - 1e-5,
    plyr::round_any(max(x), binwidth, ceiling) + 1e-5
  )
}

fraction_to_percent_format = function(frac, digits = 1) {
  paste0(format(round(frac * 100, digits), nsmall = digits), "%")
}

percent_formatter = function(x) {
  scales::percent(x, accuracy = 1)
}

points_formatter = function(x) {
  scales::comma(x, accuracy = 0.01)
}

calculate_hex_coords = function(shots, binwidths) {
  xbnds = hex_bounds(shots$loc_x, binwidths[1])
  xbins = diff(xbnds) / binwidths[1]
  ybnds = hex_bounds(shots$loc_y, binwidths[2])
  ybins = diff(ybnds) / binwidths[2]
  
  hb = hexbin(
    x = shots$loc_x,
    y = shots$loc_y,
    xbins = xbins,
    xbnds = xbnds,
    ybnds = ybnds,
    shape = ybins / xbins,
    IDs = TRUE
  )
  
  shots = mutate(shots, hexbin_id = hb@cID)
  
  hexbin_stats = shots %>%
    group_by(hexbin_id) %>%
    summarize(
      hex_attempts = n(),
      hex_pct = mean(shot_made_numeric),
      hex_points_scored = sum(shot_made_numeric * shot_value),
      hex_points_per_shot = mean(shot_made_numeric * shot_value),
      .groups = "drop"
    )
  
  hexbin_ids_to_zones = shots %>%
    group_by(hexbin_id, side_area_code) %>%
    summarize(attempts = n(), .groups = "drop") %>%
    arrange(hexbin_id, desc(attempts)) %>%
    group_by(hexbin_id) %>%
    filter(row_number() == 1) %>%
    select(hexbin_id, side_area_code)
  
  hexbin_stats = inner_join(hexbin_stats, hexbin_ids_to_zones, by = "hexbin_id")
  
  # from hexbin package, see: https://github.com/edzer/hexbin
  sx = hb@xbins / diff(hb@xbnds)
  sy = (hb@xbins * hb@shape) / diff(hb@ybnds)
  dx = 1 / (2 * sx)
  dy = 1 / (2 * sqrt(3) * sy)
  origin_coords = hexcoords(dx, dy)
  
  hex_centers = hcell2xy(hb)
  
  hexbin_coords = bind_rows(lapply(1:hb@ncells, function(i) {
    tibble(
      x = origin_coords$x + hex_centers$x[i],
      y = origin_coords$y + hex_centers$y[i],
      center_x = hex_centers$x[i],
      center_y = hex_centers$y[i],
      hexbin_id = hb@cell[i]
    )
  }))
  
  inner_join(hexbin_coords, hexbin_stats, by = "hexbin_id")
}

calculate_hexbins_from_shots = function(shots, league_average, binwidths = c(0.5, 0.5), 
                                        min_radius_factor = 0.6, 
                                        fg_diff_limits = c(-0.15, 0.15), 
                                        fg_pct_limits = c(0.1, 0.9),
                                        pps_limits = c(0.2, 2)) {
  if (nrow(shots) == 0) {
    return(list())
  }
  
  grouped_shots = group_by(shots, side_area_code)
  
  zone_stats = grouped_shots %>%
    summarize(
      zone_attempts = n(),
      zone_pct = mean(shot_made_numeric),
      zone_points_scored = sum(shot_made_numeric * shot_value),
      zone_points_per_shot = mean(shot_made_numeric * shot_value),
      .groups = "drop"
    )
  
  league_zone_stats = league_average
  
  hex_data = calculate_hex_coords(shots, binwidths = binwidths)
  
  #join_keys = c("side_area_code",'mean_value')
  
  hex_data = hex_data %>%
    inner_join(zone_stats, by = 'side_area_code') %>%
    inner_join(league_zone_stats, by = 'side_area_code')
  
  max_hex_attempts = max(hex_data$hex_attempts)
  
  hex_data = mutate(hex_data,
                    radius_factor = min_radius_factor + (1 - min_radius_factor) * log(hex_attempts + 1) / log(max_hex_attempts + 1),
                    adj_x = center_x + radius_factor * (x - center_x),
                    adj_y = center_y + radius_factor * (y - center_y),
                    bounded_fg_diff = pmin(pmax(zone_pct - league_pct, fg_diff_limits[1]), fg_diff_limits[2]),
                    bounded_fg_pct = pmin(pmax(zone_pct, fg_pct_limits[1]), fg_pct_limits[2]),
                    bounded_points_per_shot = pmin(pmax(zone_points_per_shot, pps_limits[1]), pps_limits[2]))
  
  list(hex_data = hex_data, fg_diff_limits = fg_diff_limits, fg_pct_limits = fg_pct_limits, pps_limits = pps_limits)
}

generate_hex_chart = function(hex_data, court_theme = court_themes$dark, metric = 'bounded_fg_diff', alpha_range = c(0.85, 0.98)) {
  if (length(hex_data) == 0) {
    return(plot_court)
  }
  
  
  if (metric == 'bounded_fg_diff') {
    fill_limit = hex_data$fg_diff_limits
    fill_label = "FG% vs. Role Avg"
    fill_hex = hex_data$hex_data$zone_pct- hex_data$hex_data$league_pct
    label_formatter = percent_formatter
    
  } else if (metric == "bounded_fg_pct") {
    fill_limit = hex_data$fg_pct_limits
    fill_label = "FG%"
    fill_hex = hex_data$hex_data$zone_pct
    label_formatter = percent_formatter
  } else if (metric == "bounded_points_per_shot") {
    fill_limit = hex_data$pps_limits
    fill_label = "Points Per Shot"
    fill_hex = hex_data$hex_data$zone_points_per_shot
    label_formatter = points_formatter
  } else {
    stop("invalid metric")
  }
  
  plot_court(court_theme) +
    geom_polygon(
      data = hex_data$hex_data,
      aes(
        x = adj_x,
        y = adj_y,
        group = hexbin_id,
        fill = fill_hex,
        alpha = hex_attempts,
        color = after_scale(clr_darken(fill, 0.3))
      ),
      size = court_theme$hex_border_size
      
    )+
    scale_fill_stepsn(
      colors = c('darkred','yellow','darkgreen'),
      #colors = c('#00bfc4','white','#f8766d')
      limits = fill_limit,
      paste0(fill_label,' '),
      labels = label_formatter,
      guide = guide_colorbar(barwidth = 13,keyheight = unit(0.5, "cm"), family = 'Bahnschrift'),
    ) +
    scale_alpha_continuous(guide = FALSE, range = alpha_range, trans = "sqrt") +
    theme(legend.text = element_text(size = rel(0.6)), family = "Bahnschrift")
  
} 


circle_points = function(center = c(0, 0), radius = 1, npoints = 360) {
  angles = seq(0, 2 * pi, length.out = npoints)
  return(data_frame(x = center[1] + radius * cos(angles),
                    y = center[2] + radius * sin(angles)))
}


# Court themes
court_themes = list(
  light = list(
    court = 'white',
    lines = 'black',
    text = '#222222',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0.3,
    hex_border_color = "white"
  ),
  dark = list(
    court = '#000004',
    lines = '#999999',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 0,
    hex_border_color = "#000000"
  ),
  ppt = list(
    court = 'gray15',
    lines = 'white',
    text = '#f0f0f0',
    made = '#00bfc4',
    missed = '#f8766d',
    hex_border_size = 1,
    hex_border_color = "gray15"
  )
)

# Function to create court based on given dimensions
plot_court = function(court_theme = court_themes$light) {
  # Court Dimenons & lines
  width = 15
  height = 28 / 2
  key_height = 5.8
  inner_key_width = 3.6
  outer_key_width = 4.9
  backboard_width = 1.8
  backboard_offset = 1.2
  hoop_radius = 0.45/2
  hoop_center_y = 1.575
  neck_length = hoop_center_y-hoop_radius-backboard_offset
  three_point_radius = 6.75
  three_point_side_radius = 6.6
  three_point_side_height = 3
  
  
  
  court_points = data_frame(
    x = c(width / 2, -width / 2, -width/2,width/2, width/2),
    y = c( 0, 0, height,height,0),
    desc = "perimeter"
  )
  
  court_points = bind_rows(court_points , data_frame(
    x = c(outer_key_width / 2, outer_key_width / 2, -outer_key_width / 2, -outer_key_width / 2),
    y = c(0, key_height, key_height, 0),
    desc = "outer_key"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(-backboard_width / 2, backboard_width / 2),
    y = c(backboard_offset, backboard_offset),
    desc = "backboard"
  ))
  
  court_points = bind_rows(court_points , data_frame(
    x = c(0, 0), y = c(backboard_offset, backboard_offset + neck_length), desc = "neck"
  ))
  
  foul_circle = circle_points(center = c(0, key_height), radius = inner_key_width / 2)
  
  foul_circle_top = filter(foul_circle, y > key_height) %>%
    mutate(desc = "foul_circle_top")
  
  foul_circle_bottom = filter(foul_circle, y < key_height) %>%
    mutate(
      angle = atan((y - key_height) / x) * 180 / pi,
      angle_group = floor((angle - 5.625) / 11.25),
      desc = paste0("foul_circle_bottom_", angle_group)
    ) %>%
    filter(angle_group %% 2 == 0) %>%
    select(x, y, desc)
  
  hoop = circle_points(center = c(0, hoop_center_y), radius = hoop_radius) %>%
    mutate(desc = "hoop")
  
  restricted = circle_points(center = c(0, hoop_center_y), radius = 1.25) %>%
    filter(y >= hoop_center_y) %>%
    mutate(desc = "restricted")
  
  three_point_circle = circle_points(center = c(0, hoop_center_y), radius = three_point_radius) %>%
    filter(y >= three_point_side_height, y >= hoop_center_y)
  
  three_point_line = data_frame(
    x = c(three_point_side_radius, three_point_side_radius, three_point_circle$x, -three_point_side_radius, -three_point_side_radius),
    y = c(0, three_point_side_height, three_point_circle$y, three_point_side_height, 0),
    desc = "three_point_line"
  )
  
  court_points = bind_rows(
    court_points,
    foul_circle_top,
    foul_circle_bottom,
    hoop,
    restricted,
    three_point_line
  )
  
  
  court_points <- court_points
  
  # Final plot creation
  ggplot() +
    geom_path(
      data = court_points,
      aes(x = x, y = y, group = desc),
      color = court_theme$lines
    ) +
    #coord_fixed(ylim = c(0, 35), xlim = c(-25, 25))+
    theme_minimal(base_size = 10) +
    theme(
      text = element_text(color = court_theme$text, family = 'Bahnschrift'),
      plot.background = element_rect(fill = court_theme$court , color =  court_theme$court),
      panel.background = element_rect(fill = court_theme$court, color = court_theme$court),
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.background = element_rect(fill = court_theme$court, color = court_theme$court),
      legend.margin = margin(-1, 0, 0, 0, unit = "lines"),
      legend.position = "bottom",
      legend.key = element_blank(),
      legend.text = element_text(size = rel(1.0),  family = 'Bahnschrift')
    )
}



#Advanced stats-------------------------------------------------------------------
setwd('C:/Users/nicol/OneDrive/Desktop/Progetti/lbaPBP/DAT')

#df is the dataset with all the stats from the game

lista_df <- list()

for (i in 1:length(partite)){
  tmp = read.csv(paste0(partite[i],'.dat'), header=T, sep ='\t')
  lista_df[[i]] <- tmp
}


# Concatena tutti i dataframe in un unico dataframe
df_tot <- do.call(rbind, lista_df)
df_tot = df_tot %>% select(-c('Team'))
rm(lista_df)


df_somma <- df_tot %>%
  group_by(Player,X) %>%
  summarise_all(sum)

df = df_somma

rm(df_somma)
rm(df_tot)
rm(tmp)

setwd('C:/Users/nicol/OneDrive/Desktop/Progetti/Euro/DAT')
stat =  read.csv('PlayerStat.dat',header=T)

names(stat)[11] = 'Points'
names(stat)[c(17,18,20:26,28:30,33)] = c('FGM2','FGA2','FGM3','FGA3',
                                         'FTM','FTA','OREB','DREB','BLK',
                                         'TO','STL','AST','plus_minus')

stat = stat[stat$championship_name == 'Regular Season',]


df = rename(df, code = X)
df = df %>% select(-c(1,4:15))
df = df[df$code %in% stat$code,]



df = df %>% 
  left_join(stat, by = 'code')



#Ratings
df$ORTG = df$team_Points/df$team_POSS *100
df$DRTG = df$opp_Points/df$opp_POSS *100
df$NETRTG = df$ORTG - df$DRTG

#Shooting
df$efg = (df$FGM2 + 1.5*(df$FGM3)) / (df$FGA2 + df$FGA3) * 100

#FT rate
df$ft_rate = df$FTA/df$scoring_chances *100

#Rebounding
df$oreb_pct = df$OREB/(df$team_OREB + df$opp_DREB) *100
df$dreb_pct = df$DREB/(df$team_DREB + df$opp_OREB) *100

#Playmaking
df$ast_pct = df$AST/(df$team_FGM2 + df$team_FGM3 - df$FGM2 - df$FGM3)*100
df$to_pct = df$TO/df$scoring_chances * 100
df$ast_to = df$AST/df$TO

#Steal
df$steal_pct = df$STL/df$opp_POSS * 100

#Block
df$blk_pct = df$BLK/df$opp_FGA2 * 100


#PACE
df$PACE = df$team_POSS/df$played_minutes * 40
df$usg = df$scoring_chances/df$team_scoring_chances *100
df$points_per_shot = (df$Points-df$FTM)/(df$FGA2+ df$FGA3)

df$fg2_pct = df$FGM2 / df$FGA2 * 100
df$fg3_pct = df$FGM3 / df$FGA3 * 100
df$ft_pct = df$FTM / df$FTA * 100

df <- df[rowSums(is.na(df)) != ncol(df), ]


players = df %>%  select(c('Player','team_name','code','played_minutes','played_matches',
                           'points_per_shot','efg','ft_rate','oreb_pct','dreb_pct','ast_pct',
                           'to_pct','ast_to','steal_pct','blk_pct','usg','fg2_pct','fg3_pct','ft_pct','role'))



#We take only players with more at least 5 games and at least 100 minutes played
players = players[players$played_minutes/players$played_matches >=5,]
players = players[players$played_minutes >= 100,]


df_players = players

#We consider only guards, playmaker and guards/forwards
players = df_players[df_players$role %in% c('Playmaker','Guardia','Play/Guardia','Guardia/Ala'),]
#players = players[players$role %in% c('Ala','Ala/Centro','Centro'),]
players[is.na(players) == T] = 0

#Percentile calculation
colonne_statistiche <- names(players)[c(5:19)]  # Aggiungi altre statistiche se necessario

# Calcola il percentile per ogni statistica per ciascun giocatore
for (colonna in colonne_statistiche) {
  # Calcola il rank della colonna per ogni giocatore
  rank_colonna <-  rank(players[[colonna]])
  
  # Calcola il percentile della colonna per ogni giocatore
  players[[paste0("P", colonna)]] <- rank_colonna / (length(rank_colonna) + 1)
}

players$Pto_pct = 1 - players$Pto_pct
players = players %>% select(-c('role'))

players[,c(5:34)] = round(players[,c(5:34)],2) 
players[,c(20:34)] = round(players[,c(20:34)]*100, 0)

#GT table shooting ----------------------------------------------------------------------
setwd('C:/Users/nicol/OneDrive/Desktop/Progetti/Euro')

players = players[,c(1:5,20,6,21,7,22,8,23,9,24,10,25,11,26,12,27,13,28,14,29,15,
                     30,16,31,17,32,18,33,19,34)]


players_adv = players[,c(1,2,7,9,10,29:32,11,12,33:34,13:22,23:28)]


giocatore = players_adv[players_adv$Player == 'Kamar Baldwin',]


gt(giocatore[,c(3:13)]) %>%
  gt_theme_538() %>%
  gt_color_rows(colnames(players_adv[,c(5,7,9,11,13)]),
                palette = "RColorBrewer::RdYlGn", domain = c(1:99), direction = 1) %>%
  gt_color_rows(points_per_shot, domain = c(min(players_adv$points_per_shot),
                                            max(players_adv$points_per_shot)),
                palette = "RColorBrewer::RdYlGn", direction = 1) %>%
  cols_align(align = "center") %>%
  cols_label(
    points_per_shot = "PPS",
    efg = 'EFG%',
    Pefg = '',
    fg2_pct = 'FG2%',
    Pfg2_pct = '',
    fg3_pct = 'FG3%',
    Pfg3_pct = '',
    ft_rate = 'FT RATE',
    Pft_rate = '',
    ft_pct = 'FT%',
    Pft_pct = ''
  ) %>%
  tab_spanner(
    label = "Shooting",
    columns = c(1:11)
  ) %>%
  tab_footnote(
    footnote = 'Percentile among LBA players with similar role', 
    placement = 'right'
  ) %>%
  cols_width(
    c('points_per_shot', 'efg', 'fg2_pct', 'fg3_pct', 'ft_rate', 'ft_pct') ~ px(65),
    everything() ~ px(25)
  ) %>%
  tab_style(
    locations = cells_title(groups = c("title", "subtitle")),
    style = list(
      cell_text(weight = "bold", size = 30, font = google_font(name = "Bahnschrift"))
    )
  ) %>%
  tab_options(
    table.background.color = "#FFFFFF",
    heading.title.font.size = 22,
    heading.subtitle.font.size = 14,
    source_notes.font.size = 12,
    footnotes.font.size = 12,
    column_labels.font.weight = "bold",
    table_body.border.bottom.color = "grey",
    table_body.border.top.color = "black",
    data_row.padding = px(3.5)
  ) %>%
  tab_style(
    style = cell_text(color = "black", font = google_font(name = "Bahnschrift")),
    locations = cells_column_labels()
  ) %>%
  opt_table_font(
    font = google_font(name = "Bahnschrift")
  ) %>%
  opt_align_table_header(align = "center")# %>% 
  gtsave(filename = paste0(reywer2$Player,"shot_table.png"))
  


#GT table advanced--------------------------------------------------------------
gt(giocatore[,c(14:29)]) |> 
  gt_theme_538() %>% 
  gt_color_rows(colnames(giocatore[,c(15,17,19,21,23,25,27,29)]),
                palette = "RColorBrewer::RdYlGn", domain = c(1:99), direction = 1)%>% 
  cols_align(
    align = "center") |> 
  
  cols_label(
    dreb_pct = 'DREB%',
    Pdreb_pct = '',
    oreb_pct = 'OREB%',
    Poreb_pct = '',
    ast_pct = 'AST%',
    Past_pct = '',
    to_pct = 'TOV%',
    Pto_pct ='',
    ast_to = 'AST/TO',
    Past_to = '',
    steal_pct = 'STL%',
    Psteal_pct ='',
    blk_pct = 'BLK%',
    Pblk_pct='',
    usg = 'USG%',
    Pusg = ''
    
    
  ) |> 
  tab_footnote(footnote = 'Percentile among LBA players with similar role', placement = 'right') %>% 
  ## setting column width for each column
  cols_width(
    c('dreb_pct','steal_pct','blk_pct','oreb_pct','ast_pct','to_pct','ast_to','usg') ~ px(65),
    everything() ~ px(25)
  ) |> 
  tab_spanner(
    label = "Rebounding",
    columns = c(1:4)
  ) %>%
  tab_spanner(
    label = "Playmaking",
    columns = c(5:10)
  ) %>%
  tab_spanner(
    label = "Defense",
    columns = c(11:14)
  ) %>%
  tab_spanner(
    label = "Usage",
    columns = c(15,16)
  ) %>%
  ## applying style to column headers (changing fonts and size)
  tab_style(
    locations = cells_title(groups = c("title", "subtitle")),
    style = list(
      cell_text(weight = "bold", size = 30, font = google_font(name = "Bahnschrift"))
    )) |> 
  ## additional theme features
  tab_options(table.background.color = "#FFFFFF", # change table background colour
              heading.title.font.size = 22, # change title font
              heading.subtitle.font.size = 14, # change subtitle font
              source_notes.font.size = 12, # change source note font
              footnotes.font.size = 12,
              column_labels.font.weight = "bold", # make column labels bold
              #heading.padding = px(12), # add space between title and table header
              #container.width = 920, # change table width
              #container.height = 740, # change table height
              table_body.border.bottom.color = "grey", # make bottom border grey colour
              table_body.border.top.color = "black", # make top border black
              data_row.padding = px(3.5)) |> # reduce space between rows
  ## change column labels text colour and font
  tab_style(
    style = cell_text(color = "black",font = google_font(name = "Bahnschrift")),
    locations = cells_column_labels()
  ) |> 
  ## change font of all texts
  opt_table_font(
    font = google_font(name = "Bahnschrift")
  ) |> 
  ## align table title to the centre
  opt_align_table_header(align = "center")#%>% 
  gtsave(filename = paste0(reywer2$Player,"adv_table.png"))
  
#GT table traditional-----------------------------------------------------------
setwd('C:/Users/nicol/OneDrive/Desktop/Progetti/Euro/DAT')
statavg =  read.csv('PlayerStatAvg.dat',header=T)
statavg = statavg[statavg$championship_name == 'Regular Season',]
statavg = statavg[,-26]
names(statavg)[11] = 'Points'
names(statavg)[c(17,18,20:26,28:30,33)] = c('FGM2','FGA2','FGM3','FGA3',
                                              'FTM','FTA','OREB','DREB','BLK',
                                              'TO','STL','AST','plus_minus')
  
statavg = statavg[,-c(31,32,34)]
players2 = statavg[statavg$code %in% players$code,]
  
colonne_statistiche <- names(statavg)[c(10:31)]  
  
# Calcola il percentile per ogni statistica per ciascun giocatore
for (colonna in colonne_statistiche) {
    # Calcola il rank della colonna per ogni giocatore
    rank_colonna <-  rank(players2[[colonna]])
    
    # Calcola il percentile della colonna per ogni giocatore
    players2[[paste0("P", colonna)]] <- rank_colonna / (length(rank_colonna) + 1)
  }
  
players2[,c(10:53)] = round(players2[,c(10:53)],2) 
players2[,c(32:53)] = round(players2[,c(32:53)]*100, 0)
  
players2$PTO = 100 - players2$PTO
  
players2 = players2[c(1:10,32,11,33,12,34,13,35,14,36,15,37,16,38,17,39,18,40,19,41,
                        20,42,21,43,22,44,23,45,24,46,25,47,26,48,27,49,28,50,29,51,30,52,
                        31,53)]
players2$name = paste(players2$name, players2$surname)
players2 = players2[c(2,9:53)]
players2 = players2[c(1:2,9,10,7,8,5,6,11:46)]  

giocatore2 = players2[players2$name == 'Kamar Baldwin',]
  
gt(giocatore2[,-c(1,2,4,9:14,19,20,35,36,43,44)]) |> 
    gt_theme_538() %>% 
    gt_color_rows(colnames(players2[,c(6,8,16,18,22,24,26,28,30,32,34,38,40,42)]),
                  palette = "RColorBrewer::RdYlGn", domain = c(1:99), direction = 1) %>% 
    cols_align(
      align = "center") |> 
    cols_label(
      played_matches = 'GM',
      played_minutes = "MP",
      Pplayed_minutes = '',
      Points = 'PTS',           
      PPoints = '',         
      PFGM2 = '',       
      PFGA2 = '', 
      PFGM3 = '',       
      PFGA3 = '',
      PFTM = '',       
      PFTA = '', 
      POREB = '',
      PDREB = '',
      PBLK = '',
      PTO = '',
      PAST = '',
      PSTL =''
      
      
    ) |> 
    
    tab_footnote(footnote = 'Percentile among players with similar role', placement = 'right') %>% 
    ## setting column width for each column
    cols_width(
      c("played_matches","Points","FGM2","FGA2","FGM3","FGA3",
        "FTM","FTA","OREB","DREB","BLK","TO","STL","AST") ~ px(40),
      played_matches ~ px(18),
      played_minutes ~ px(45),
      everything() ~ px(25)
    ) |> 
    ## applying style to column headers (changing fonts and size)
    tab_style(
      locations = cells_title(groups = c("title", "subtitle")),
      style = list(
        cell_text(weight = "bold", size = 30, font = google_font(name = "Bahnschrift"))
      )) |> 
  tab_style(
    style = cell_text(color = "black",font = google_font(name = "Bahnschrift")),
    locations = cells_column_labels()
    ## change text weight of player column
  ) |> 
    ## change font of all texts
    opt_table_font(
      font = google_font(name = "Bahnschrift")
    ) |> 
    ## align table title to the centre
    opt_align_table_header(align = "center")# %>% 
    gtsave(filename = paste0(reywer2$name,"trad_table.png"))
  
  
#Shooting chart and distribution------------------------------------------------
setwd('C:/Users/nicol/OneDrive/Desktop/Progetti/Euro/DAT')

data = read.csv('lbaPBP.dat', header=T)
data = data %>% select(-id)
data = data[data$id_game %in% partite,]

nomi_aree <- c("Left Corner", "Left Mid Range", "Restricted Area", "Right Mid Range", "Right Corner",
               "Paint", "Top of the key", "Left 3 Wing", "Center 3", "Right 3 Wing")

# Rinomina i livelli di side_area_code
data$side_area_code <- factor(data$side_area_code, levels = 1:10, labels = nomi_aree)

fg = c('3 punti segnato',
       '3 punti sbagliato',
       '2 punti segnato',
       '2 punti sbagliato'
)

hoop_center_y = 1.575
data = data[data$description %in% fg,]  
data$isShotMade = ifelse(data$description %in% c('3 punti segnato','2 punti segnato'),TRUE,FALSE)
data = data %>%  
  mutate(
    loc_x =  (y-50) *15/100 * ifelse(x < 50, -1, 1),
    loc_y =  ifelse(x<50, x, -(x-100)) * 28/100 ,
    shot_distance = sqrt(loc_x^2 + (loc_y-hoop_center_y)^2),
    shot_made_numeric = as.numeric(isShotMade),
    shot_value = isShotMade * ifelse(description == '2 punti segnato', 2, 3)
  )

data = data[data$player_id %in% players$code,]

league_average = data %>% 
  select(c('shot_made_numeric','shot_value','side_area_code')) %>% 
  group_by(side_area_code) %>% 
  summarize(league_pct = mean(shot_made_numeric),
            league_shot_value = mean(shot_value),
            league_fga = n(),
            league_fgm = sum(shot_made_numeric))

league_average = league_average[c(1:10),]
league_average$league_freq = league_average$league_fga/sum(league_average$league_fga)

league_type = data %>% 
  select(c('shot_made_numeric','shot_value','action_1_qualifier_description')) %>% 
  group_by(action_1_qualifier_description) %>% 
  summarize(league_pct = mean(shot_made_numeric),
            league_shot_value = mean(shot_value),
            league_fga = n(),
            league_fgm = sum(shot_made_numeric))
league_type$league_freq = league_type$league_fga/sum(league_type$league_fga)

#Shots is the dataset with the shots of the player we want to analyze
shots =  data[data$player_surname == "Baldwin" & data$player_name == 'Kamar',]

name = unique(shots$player_name)
surname = unique(shots$player_surname)

shots_type = shots %>% 
  select(c('shot_made_numeric','shot_value','action_1_qualifier_description')) %>% 
  group_by(action_1_qualifier_description) %>% 
  summarize(team_pct = mean(shot_made_numeric),
            team_shot_value = mean(shot_value),
            team_fga = n(),
            team_fgm = sum(shot_made_numeric))
shots_type$team_freq = shots_type$team_fga/sum(shots_type$team_fga)

shots_average = shots %>% 
  select(c('shot_made_numeric','shot_value','side_area_code')) %>% 
  group_by(side_area_code) %>% 
  summarize(team_pct = mean(shot_made_numeric),
            team_shot_value = mean(shot_value),
            team_fga = n(),
            team_fgm = sum(shot_made_numeric))

#shots_average = shots_average[c(1:10),]
shots_average$team_freq = shots_average$team_fga/sum(shots_average$team_fga)


#Here we create the labels for the chart
player_pct_labels <- sapply(shots_average$team_pct, function(pct) {
  paste0(formatC(pct * 100, format = "f", digits = 0), "%")
})

# Similarly, format fgm and fga
fgm_fga <- sapply(1:nrow(shots_average), function(i) {
  paste(shots_average$team_fgm[i], shots_average$team_fga[i], sep = "/")
})



x_labels <- c(7.5, 4, 0,   -4, -7.5,  0,  0,  5, 0, -5)  
y_labels <- c(0,   3, 0.5,  3,    0,  3, 5,   7, 9, 7) 

shotchart =
  generate_hex_chart(calculate_hexbins_from_shots(shots, league_average), court_theme = court_themes$light)+
  theme(plot.title = element_text(hjust = .5, size = 22, family = 'Bahnschrift',face = "bold", vjust = -1),
        plot.subtitle = element_text(hjust = .5, size = 10, family = 'Bahnschrift',face = "bold", vjust = -3))+
  ggtitle(label = paste(name,surname),
          subtitle = "23/24 RS vs LBA players with similar role ")+
  #labs(caption = "@lefty_curly") +
  geom_label(
    aes(x = x_labels, y = y_labels, label = paste0(player_pct_labels, "\n", fgm_fga)),
    fill = "black", alpha = 0.8, color = "white", size = 3, hjust = 0.5, vjust = -1,
    family = 'Bahnschrift'
  )

ggdraw(shotchart) 

setwd('C:/Users/nicol/OneDrive/Desktop/Progetti/Euro')
ggsave(paste0(surname,"_shot_chart.png"), height = 5, width = 6, dpi = "retina")


#Here we create the data for the histograms

new_data <- data.frame(
  Shot_Type = c('Rim','Paint', 'Mid Range',"3 point"),
  freq = c(shots_average[shots_average$side_area_code == 'Restricted Area', ]$team_freq,
           sum(shots_average[shots_average$side_area_code == 'Paint', ]$team_freq),
           sum(shots_average[shots_average$side_area_code %in% 
                               c('Left Mid Range','Right Mid Range','Top of the key'), ]$team_freq),
           sum(shots_average[shots_average$side_area_code %in% 
                               c('Left Corner','Right Corner','Left 3 Wing',
                                 'Right 3 Wing', 'Center 3'), ]$team_freq)),
  perc = c(shots_average[shots_average$side_area_code == 'Restricted Area', ]$team_pct,
           sum(shots_average[shots_average$side_area_code == 'Paint', ]$team_pct),
           sum(shots_average[shots_average$side_area_code %in% 
                               c('Left Mid Range','Right Mid Range','Top of the key'), ]$team_fgm)/
             sum(shots_average[shots_average$side_area_code %in% 
                                 c('Left Mid Range','Right Mid Range','Top of the key'), ]$team_fga),
           sum(shots$description %in% c('3 punti segnato'))/
             sum(shots$description %in% c('3 punti segnato','3 punti sbagliato'))
  )
)
league_data <- data.frame(
  Shot_Type = c('Rim','Paint', 'Mid Range',"3 point"),
  freq = c(league_average[league_average$side_area_code == 'Restricted Area', ]$league_freq,
           sum(league_average[league_average$side_area_code == 'Paint', ]$league_freq),
           sum(league_average[league_average$side_area_code %in% 
                                c('Left Mid Range','Right Mid Range','Top of the key'), ]$league_freq),
           sum(league_average[league_average$side_area_code %in% 
                                c('Left Corner','Right Corner','Left 3 Wing',
                                  'Right 3 Wing', 'Center 3'), ]$league_freq)),
  perc = c(league_average[league_average$side_area_code == 'Restricted Area', ]$league_pct,
           sum(league_average[league_average$side_area_code == 'Paint', ]$league_pct),
           sum(league_average[league_average$side_area_code %in% 
                                c('Left Mid Range','Right Mid Range','Top of the key'), ]$league_fgm)/
             sum(league_average[league_average$side_area_code %in% 
                                  c('Left Mid Range','Right Mid Range','Top of the key'), ]$league_fga),
           sum(league_average[league_average$side_area_code %in% 
                                c('Left Corner','Right Corner','Left 3 Wing',
                                  'Right 3 Wing', 'Center 3'), ]$league_fgm)/
             sum(league_average[league_average$side_area_code %in% 
                                  c('Left Corner','Right Corner','Left 3 Wing',
                                    'Right 3 Wing', 'Center 3'), ]$league_fga))
)



shot_distribution = ggplot() +
  # bar chart function from ggplot
  geom_bar(new_data, mapping = aes(x = Shot_Type, y = freq), 
           stat = "identity", position = position_dodge(), fill = "#FC0FC0", color = "black", width = .6) + 
  # set y-axis limits and start ay-xis at 0
  scale_y_continuous(limits=c(0, 1), expand = c(0,0), labels = scales::percent_format()) +
  # set different labels for plots
  labs(title = paste(surname,"Shot Selection Distribution"),
       #subtitle = "@lefty_curly || Data: legabasket.it\n",
       x = "",
       y = "Frequency\n") +
  # add points per game text values inside the bars for easier understanding
  geom_segment(data = league_data, aes(x = as.numeric(factor(Shot_Type)) - 0.3, xend = as.numeric(factor(Shot_Type)) + 0.3, y = freq, yend = freq), color = "grey70", size = 1.5) +
  geom_text(data = league_data, aes(x = as.numeric(factor(Shot_Type)) -0.4, y = freq, label = paste0(round(freq * 100, 0),' %')), hjust = 0, vjust = -0.1, color = "grey70",family = "Bahnschrift") +
  geom_text(new_data, mapping = aes(x = Shot_Type, y = freq, label = paste0(round(freq*100,1),'%')), 
            position = position_dodge(width = .9), size = 6, color = "black", vjust = -1, face = "bold",family = "Bahnschrift") +
  
  # different theme settings to customize the chart
  theme(
    # labels/text
    plot.title = element_text(hjust = .5, size = 14, face = "bold", color = "black",family = "Bahnschrift"),
    plot.subtitle = element_text(hjust = .5, size = 8, color = "gray50",family = "Bahnschrift"),
    text = element_text(family = "Bahnschrift"),
    #legend.position = "none",
    # plot background and lines
    plot.background = element_rect(fill="white", color = "white"),
    panel.background = element_rect(fill="white", color = "white"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(colour = "gray70"),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "gray15"),
    # axis
    axis.title.x = element_text(colour = "black", size = 10,family = "Bahnschrift"),
    axis.title.y = element_text(colour = "black", size = 10,family = "Bahnschrift"),
    axis.text.x = element_text(colour = "black", size = 8,family = "Bahnschrift"),
    axis.text.y = element_text(colour = "black", size = 8,family = "Bahnschrift"),
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    # facet 
    strip.background = element_rect(fill="white"),
    strip.text = element_text(hjust = .5, size = 8,face = "bold", color = "black",family = "Bahnschrift"))+
  annotate("segment", x = 2.25, xend = 2.75, y = 0.87, yend = 0.87, linewidth = 1.5,
           colour = "grey70")+
  annotate("text", x = 2.5, y = 0.95, size = 2, label = 'Role Avg Freq',
           colour = "grey70",family = "Bahnschrift")

ggdraw(shot_distribution)
ggsave(paste0(surname,"_distirbution.png"), height = 2.5, width = 6, dpi = "retina")


shot_percentage = ggplot() +
  # bar chart function from ggplot
  geom_bar(new_data, mapping = aes(x = Shot_Type, y = perc), 
           stat = "identity", position = position_dodge(), fill = "#51AFF7", color = "black", width = .6) + 
  # set y-axis limits and start ay-xis at 0
  scale_y_continuous(limits=c(0, 1), expand = c(0,0), labels = scales::percent_format()) +
  # set different labels for plots
  labs(title = paste(surname, "Shooting % by type"),
       #subtitle = "@lefty_curly || Data: legabasket.it\n",
       x = "",
       y = "FG%\n") +
  # add points per game text values inside the bars for easier understanding
  
  geom_segment(data = league_data, aes(x = as.numeric(factor(Shot_Type)) - 0.3, xend = as.numeric(factor(Shot_Type)) + 0.3, y = perc, yend = perc), color = "grey70", size = 1.5) +
  geom_text(data = league_data, aes(x = as.numeric(factor(Shot_Type)) -0.4, y = perc, label = paste0(round(perc * 100, 0),' %')), hjust = 0, vjust = -0.1, color = "grey70",family = "Bahnschrift") +
  geom_text(new_data, mapping = aes(x = Shot_Type, y = perc, label = paste0(round(perc*100,1),'%')), 
            position = position_dodge(width = .9), size = 6, color = "black", vjust = -1, face = "bold",family = "Bahnschrift") +
  
  # different theme settings to customize the chart
  theme(
    # labels/text
    plot.title = element_text(hjust = .5, size = 14, face = "bold", color = "black",family = "Bahnschrift"),
    plot.subtitle = element_text(hjust = .5, size = 8, color = "gray50",family = "Bahnschrift"),
    text = element_text(family = "Bahnschrift"),
    #legend.position = "none",
    # plot background and lines
    plot.background = element_rect(fill="white", color = "white"),
    panel.background = element_rect(fill="white", color = "white"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_line(colour = "gray70"),
    panel.grid.major.x = element_blank(),
    axis.line = element_line(colour = "gray15"),
    # axis
    axis.title.x = element_text(colour = "black", size = 10,family = "Bahnschrift"),
    axis.title.y = element_text(colour = "black", size = 10,family = "Bahnschrift"),
    axis.text.x = element_text(colour = "black", size = 8,family = "Bahnschrift"),
    axis.text.y = element_text(colour = "black", size = 8,family = "Bahnschrift"),
    plot.margin = margin(.5, .5, .5, .5, "cm"),
    # facet 
    strip.background = element_rect(fill="white"),
    strip.text = element_text(hjust = .5, size = 8,face = "bold", color = "black",family = "Bahnschrift"))+
  annotate("segment", x = 2.25, xend = 2.75, y = 0.87, yend = 0.87, linewidth = 1.5,
           colour = "grey70")+
  annotate("text", x = 2.5, y = 0.95, size = 2, label = 'Role Avg FG%',
           colour = "grey70",family = "Bahnschrift")


ggdraw(shot_percentage)
ggsave(paste0(surname,"_percentage.png"), height = 2.5, width = 6, dpi = "retina")



