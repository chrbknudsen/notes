library(tidyverse)
# data fra:  "https://www.metoffice.gov.uk/hadobs/hadcrut5/data/HadCRUT.5.0.2.0/download.html"
url <- "https://www.metoffice.gov.uk/hadobs/hadcrut5/data/HadCRUT.5.0.2.0/analysis/diagnostics/HadCRUT.5.0.2.0.analysis.summary_series.global.monthly.csv"
download.file(url, "HadCrut5.csv", mode = "wb")
HadCrut5 <- read_csv("HadCrut5.csv")

HadCrut5 <- HadCrut5 %>% 
  separate(Time, "-", into = c("year", "month")) %>% 
  rename(anomaly = `Anomaly (deg C)`,
         lower = `Lower confidence limit (2.5%)`,
         upper = `Upper confidence limit (97.5%)`) %>% 
  mutate(across(year:month, as.numeric),
         month_name = month.abb[month]) %>% 
  select(-lower, -upper)




HadCrut5 <- HadCrut5 %>% 
  bind_rows(HadCrut5 %>% 
  filter(month == 1) %>% 
  mutate(year = year - 1,
         month  = 13,
         month_name = "next_jan") 
  )

HadCrut5 %>% 
  ggplot(aes(x = month, y = anomaly, group = year, color = year)) +
  geom_line() +
  scale_x_continuous(breaks=1:12,
                     labels=month.abb, expand = c(0,0),
                     sec.axis = dup_axis(name = NULL, labels=NULL)) +
  coord_polar(start = 2*pi/12) +
  theme(
    panel.background = element_rect(fill="#444444", size=1),
    plot.background = element_rect(fill = "#444444", color="#444444"),
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_text(color="white", size=13),
    plot.title = element_text(color="white", hjust = 0.5,size = 15)
  )
slice_max

