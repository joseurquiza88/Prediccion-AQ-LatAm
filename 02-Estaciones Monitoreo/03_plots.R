# Algunos plots para analisis de variables ----
setwd("D:/Josefina/Proyectos/Tesis/data/merge_tot/")

#Se hace por ciudad
# SP ----
df_sp <- read.csv("01_SP_merge_comp.csv")
scale_factor_sp <- max(df_sp$PM25, na.rm = TRUE) / max(df_sp$AOD, na.rm = TRUE)
df_sp$date <- as.Date(df_sp$date, format = "%d/%m/%Y")

sp_plot <- ggplot(df_sp, aes(x = date)) +
  geom_line(aes(y = PM25, color = "PM2.5")) +
  geom_line(aes(y = AOD_055 * scale_factor_sp, color = "AOD")) +
  scale_y_continuous(
    name =  expression(PM[2.5]),
    limits = c(0, 160),
    sec.axis = sec_axis(~ . / scale_factor_sp, name = "AOD")) +
  scale_color_manual(values = c(
    "PM2.5" = "#005a32", 
    "AOD" = "#74c476")) +
  labs(x = "Fecha", color = "Variable") +
  theme_classic() + 
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y.left = element_text(color = "#3f007d", size = 14),
    axis.title.y.right = element_text(color = "#807dba", size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none") +
  theme(
    axis.title.y.left = element_text(color = "#005a32"),
    axis.title.y.right = element_text(color = "#74c476"))

sp_plot

# --- 
# ST ----
df_st <- read.csv("02_st_merge_comp.csv")
scale_factor_st <- max(df_st$PM25, na.rm = TRUE) /
  max(df_st$AOD, na.rm = TRUE)
df_st$date <- as.Date(df_st$date, format = "%d/%m/%Y")

st_plot <- ggplot(df_st, aes(x = date)) +
  geom_line(aes(y = PM25, color = "PM2.5")) +
  geom_line(aes(y = AOD_055 * scale_factor_st, color = "AOD")) +
  scale_y_continuous(
    name =  expression(PM[2.5]),
    limits = c(0, 160),
    sec.axis = sec_axis(~ . / scale_factor_st, name = "AOD")
  ) +
  scale_color_manual(values = c("PM2.5" = "#d95f0e", "AOD" = "#fec44f" )) +
  labs(x = "Fecha", color = "Variable") +
  theme_classic() +
 theme(
    axis.title.x = element_text(size = 14),
    axis.title.y.left = element_text(color = "#3f007d", size = 14),
    axis.title.y.right = element_text(color = "#807dba", size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none") + 
  theme(
    axis.title.y.left = element_text(color =  "#d95f0e"),
    axis.title.y.right = element_text(color = "#fec44f"))

st_plot

# BA ----
df_ba <- read.csv("03_ba_merge_comp.csv")
scale_factor_ba <- max(df_ba$PM25, na.rm = TRUE) / 
  max(df_ba$AOD, na.rm = TRUE)

df_ba$date <- as.Date(df_ba$date, format = "%d/%m/%Y")
ba_plot<-ggplot(df_ba, aes(x = date)) +
  geom_line(aes(y = PM25, color = "PM2.5")) +
  geom_line(aes(y = AOD_055 * scale_factor_ba, color = "AOD")) +
  scale_y_continuous(
    name =  expression(PM[2.5]),
    limits = c(0, 160),
    sec.axis = sec_axis(~ . / scale_factor_ba, name = "AOD")) +
  scale_color_manual(values = c("PM2.5" = "#67000d",
                                "AOD" = "#cb181d")) +
  labs(x = "Fecha", color = "Variable") +
  theme_classic() +
 theme(
    axis.title.x = element_text(size = 14),
    axis.title.y.left = element_text(color = "#3f007d", 
                                     size = 14),
    axis.title.y.right = element_text(color = "#807dba",
                                      size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none") +
  theme(axis.title.y.left = element_text(color = "#67000d"),
        axis.title.y.right = element_text(color = "#cb181d" ))

ba_plot

# MD ----
df_md <- read.csv("04_md_merge_comp.csv")
scale_factor_md <- max(df_md$PM25, na.rm = TRUE) / max(df_md$AOD, na.rm = TRUE)
df_md$date <- as.Date(df_md$date, format = "%d/%m/%Y")
md_plot<-ggplot(df_md, aes(x = date)) +
  geom_line(aes(y = PM25, color = "PM2.5")) +
  geom_line(aes(y = AOD_055 * scale_factor_md, color = "AOD")) +
  scale_y_continuous(
    name =  expression(PM[2.5]),
    limits = c(0, 160),
    sec.axis = sec_axis(~ . / scale_factor_md, name = "AOD")) +
  scale_color_manual(values = c("PM2.5" = "#023858",
                                "AOD" = "#74a9cf" )) +
  labs(x = "Fecha", color = "Variable") +
  theme_classic() +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y.left = element_text(color = "#3f007d", size = 14),
    axis.title.y.right = element_text(color = "#807dba", size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none")+
  theme(
    axis.title.y.left = element_text(color =  "#023858"),
    axis.title.y.right = element_text(color = "#74a9cf"  ))

md_plot

# MX ----
df_mx <- read.csv("05_mx_merge_comp.csv")
scale_factor_mx <- max(df_mx$PM25, na.rm = TRUE) / max(df_mx$AOD, na.rm = TRUE)
df_mx$date <- as.Date(df_mx$date, format = "%d/%m/%Y")
mx_plot<-ggplot(df_mx, aes(x = date)) +
  geom_line(aes(y = PM25, color = "PM2.5")) +
  geom_line(aes(y = AOD_055 * scale_factor_mx, color = "AOD")) +
  scale_y_continuous(
    name =  expression(PM[2.5]),
    limits = c(0, 160),
    sec.axis = sec_axis(~ . / scale_factor_mx, name = "AOD")) +
  scale_color_manual(values = c("PM2.5" = "#3f007d", 
                                "AOD" = "#807dba")) +
  labs(x = "Fecha", color = "Variable") +
  theme_classic() +
  theme(axis.title.x = element_text(size = 14),
    axis.title.y.left = element_text(color = "#3f007d", size = 14),
    axis.title.y.right = element_text(color = "#807dba", size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none")

# Guardar plot ----
dir <- "D:/Josefina/Proyectos/Tesis/plot/"
ggsave(filename = paste(dir,"01_Serie-Temporal_AOD-PM25_mx.png",
                   sep=""),
  plot = mx_plot,       
  width = 6,
  height = 4,
  dpi = 30)  