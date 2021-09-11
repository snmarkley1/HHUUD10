#########################################################################
#########################################################################
###                                                                   ###
###                      Validation Graphs                            ###
###                                                                   ###
#########################################################################
#########################################################################

## PREPARE WORKSPACE
source("scripts/00_preamble.R")
packages(cowplot)  # needed

## LOAD VARIABLES
hu4080 <- read_csv("output/hu4080.csv")
valid <- read_csv("output/valid_90_10.csv")

## CHECK INTERPOLATION TYPE --> separate tracted and not tracted
df_interp <- hu4080 %>%
  filter(yr == 1990) %>%
  ## TP = tracts present; NT = not tracted
  mutate(int_type = ifelse(method %in% c("AW", "TDW-1", "TDW-90"), "TP", "NT")) %>%
  select(GISJOIN10, int_type) %>%
  print()

# PREPARE GRAPH
df <- valid %>%
  filter(YEAR == 1990) %>%
  select(STATE:GISJOIN10, TYPE, HU_EST_CMR, HU_REAL) %>%
  left_join(df_interp, by = "GISJOIN10") %>%
  mutate(
    APE = (HU_REAL - HU_EST_CMR) / ((HU_EST_CMR + HU_REAL)/2),
    APE = ifelse(is.nan(APE), 0, APE)
  ) %>%
  group_by(TYPE, int_type) %>%
  print()


##----------------------------------------
## Make graph
##----------------------------------------

figure_d <- function(x){
  
  d <- ggplot() +
    geom_density(data = x[which(x$TYPE == "D"),], aes(x = APE, color = int_type, fill = int_type), adjust = 1.5, lwd = 1, alpha = 0.4) +
    scale_color_manual(name = "Tract Coverage", labels = c("Not tracted", "Tracts present"), values = c("firebrick4", "dodgerblue4")) +
    geom_vline(xintercept = 0) +
    theme_bw() +
    guides(fill = FALSE) +
    xlim(-1, 1) +
    ylim(0, 18) +
    labs(
      x = "ALPE",
      y = "Density"
    ) +
    facet_grid(. ~ "Declining", scales = "fixed")
  
  return(d)
  
}

figure_g <- function(x){
  
  g <- ggplot() +
    geom_density(data = x[which(x$TYPE == "G"),], aes(x = APE, color = int_type, fill = int_type), adjust = 1.5, lwd = 1, alpha = 0.4) +
    scale_color_manual(name = "Tract Coverage", labels = c("Not tracted", "Tracts present"), values = c("firebrick4", "dodgerblue4")) +
    geom_vline(xintercept = 0) +
    theme_bw() +
    guides(fill = FALSE) +
    xlim(-1, 1) +
    ylim(0, 18) +
    labs(
      x = "ALPE",
      y = "Density"
    ) +
    facet_grid(. ~ "Growing", scales = "fixed")
  
  return(g)
  
}

figure_s <- function(x){
  
  s <- ggplot() +
    geom_density(data = x[which(x$TYPE == "S"),], aes(x = APE, color = int_type, fill = int_type), adjust = 1.5, lwd = 1, alpha = 0.4) +
    scale_color_manual(name = "Tract Coverage", labels = c("Not tracted", "Tracts present"), values = c("firebrick4", "dodgerblue4")) +
    geom_vline(xintercept = 0) +
    theme_bw() +
    guides(fill = FALSE) +
    xlim(-1, 1) +
    ylim(0, 18) +
    labs(
      x = "ALPE",
      y = "Density"
    ) +
    facet_grid(. ~ "Stable", scales = "fixed")
  
  return(s)
  
}

figure_t <- function(x){
  
  t <- ggplot() +
    geom_density(data = x, aes(x = APE, color = int_type, fill = int_type), adjust = 1.5, lwd = 1, alpha = 0.4) +
    scale_color_manual(name = "Tract Coverage", labels = c("Not tracted", "Tracts present"), values = c("firebrick4", "dodgerblue4")) +
    geom_vline(xintercept = 0) +
    theme_bw() +
    guides(fill = FALSE) +
    xlim(-1, 1) +
    ylim(0, 18) +
    labs(
      x = "ALPE",
      y = "Density"
    ) +
    facet_grid(. ~ "Total", scales = "fixed")
  
  return(t)
  
}

# Set panels
panel_d <- figure_d(df)
panel_g <- figure_g(df)
panel_s <- figure_s(df)
panel_t <- figure_t(df)


# Prepare plot
prow <- cowplot::plot_grid(
  panel_d + theme(legend.position = "none"),
  panel_g + theme(legend.position = "none"),
  panel_s + theme(legend.position = "none"),
  panel_t + theme(legend.position = "none"),
  labels = "auto", 
  ncol = 2
)

# Make plot
legend <- get_legend(panel_d)
plot_grid(prow, legend, rel_widths = c(2, 0.5))

