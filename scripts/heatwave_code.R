library(dplyr)
library(ggplot2)
library(heatwaveR)

sst_me= read.csv(file.choose(), header = T)

sst_me$t= as.Date(sst_me$t, "%Y-%m-%d")

ts <- ts2clm(sst_me, climatologyPeriod = c("1982-01-01", "2020-02-25"))
             
mhw <- detect_event(ts)
mhw

mhw$event %>%
  dplyr::ungroup() %>%
  dplyr::select(event_no, duration, date_start, date_peak, intensity_max, intensity_cumulative) %>%
  dplyr::arrange(-intensity_max) %>%
  head(5)

  write.csv(ts, "C:\\Users\\jgkar\\Desktop\\SML Project\\Mydata.csv")

event_line(mhw, spread = 365, metric = "intensity_max",
           start_date = "2012-01-01", end_date = "2012-12-31")

lolli_plot(mhw, metric = "intensity_max")
lolli_plot(mhw, metric = "duration")

lolli_plot(mcs, metric = "intensity_max")
lolli_plot(mcs, metric = "duration")

write.csv(mhw)


write.csv(mhw$event, file = "", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")



write.csv(mhw$event, file = "mydata_imputed100.csv")
write.csv(mhw2, file = 'test')
------------------

 mhw2 <- mhw$climatology %>%
  slice(1:13514)

ggplot(mhw2, aes(x = t, y = temp, y2 = thresh)) +
  geom_flame() +
  geom_text(aes(x = as.Date("2012-02-25"), y = 25.8, label = "the Destroyer\nof Kelps"))
#########################

ggplot(mhw$event, aes(x = date_start, y = intensity_max)) +
  geom_lolli(colour = "salmon", colour_n = "red", n = 3) +
  geom_text(colour = "black", aes(x = as.Date("2006-08-01"), y = 5,
                                  label = "")) +
  labs(y = expression(paste("Max. intensity [", degree, "C]")), x = NULL)

##################################

mhw_top <- mhw2 %>%
  slice(1000:1110)

ggplot(data = mhw2, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = thresh, fill = "all"), show.legend = T) +
  geom_flame(data = mhw_top, aes(y = temp, y2 = thresh, fill = "top"),  show.legend = T) +
  geom_line(aes(y = temp, colour = "temp")) +
  geom_line(aes(y = thresh, colour = "thresh"), size = 1.0) +
  geom_line(aes(y = seas, colour = "seas"), size = 1.2) +
  scale_colour_manual(name = "Line Colour",
                      values = c("temp" = "black",
                                 "thresh" =  "forestgreen",
                                 "seas" = "grey80")) +
  scale_fill_manual(name = "Event Colour",
                    values = c("all" = "salmon",
                               "top" = "red")) +
  scale_x_date(date_labels = "%b %Y") +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL)
#########################################

mhw3 <- mhw$climatology %>%
  slice(850:950)

ggplot(mhw3, aes(x = t, y = temp, y2 = thresh)) +
  geom_flame(fill = "black", alpha = 0.5) +
  # Note the use of n = 5 and n_gap = 2 below
  geom_flame(n = 5, n_gap = 2, fill = "red", alpha = 0.5) +
  ylim(c(22, 25)) +
  geom_text(colour = "black", aes(x = as.Date("1984-05-16"), y = 24.5,
                                  label = "heat\n\n\n\n\nspike"))

################3

ggplot(mhw$event, aes(x = date_peak, y = intensity_max)) +
  geom_lolli(colour = "firebrick") +
  labs(x = "Peak Date",
       y = expression(paste("Max. intensity [", degree, "C]")), x = NULL) +
  theme_linedraw()
##################################
library(plotly)
ts_res <- heatwaveR::ts2clm(data = heatwaveR::sst_WA,
                            climatologyPeriod = c("2010-01-01", "2018-12-31"))
ts_res_sub <- ts_res[10500:10800,]

# Flame Figure
p <- ggplot(data = ts_res_sub, aes(x = t, y = temp)) +
  heatwaveR::geom_flame(aes(y2 = thresh), n = 5, n_gap = 2) +
  geom_line(aes(y = temp)) +
  geom_line(aes(y = seas), colour = "green") +
  geom_line(aes(y = thresh), colour = "red") +
  labs(x = "", y = "Temperature (°C)")

# Create interactive visuals
ggplotly(p)

###################

# The tMax threshold
# The current WMO standard climatology period is 1981-01-01 to 2010-12-31 and should be used when possible
# We rather use 1961-01-01 to 1990-01-01 as this is the oldest 30 year period available in the data
tMax_clim <- ts2clm(data = sst_me, y = tMax, climatologyPeriod = c("1982-01-01", "2020-12-31"), pctile = 90)

# The tMin exceedance
# Note the use here of 'minDuration = 3' and 'maxGap = 1' as the default atmospheric arguments
# The default marine arguments are 'minDuration = 5' and 'maxGap = 2'
tMin_exc <- exceedance(data = Algiers, y = tMin, threshold = 19, minDuration = 3, maxGap = 1)$threshold



##########################cold 

ts_10th <- ts2clm(sst_me, climatologyPeriod = c("1982-01-01", "2018-12-31"), pctile = 10)
mcs <- detect_event(ts_10th, coldSpells = TRUE)

# Then look at the top few events
mcs$event %>%
  dplyr::ungroup() %>%
  dplyr::select(event_no, duration, date_start,
                date_peak, intensity_mean, intensity_max, intensity_cumulative) %>%
  dplyr::arrange(intensity_cumulative) %>%
  head(5)

event_line(mcs, spread = 200, metric = "intensity_cumulative",
           start_date = "1982-01-01", end_date = "2018-12-31")

##################
ts_res <- heatwaveR::ts2clm(data = heatwaveR::sst_me,
                            climatologyPeriod = c("1982-01-01", "2018-12-31"))
ts_res_sub <- ts_res[1:10800,]

# Flame Figure
p <- ggplot(data = ts_res_sub, aes(x = t, y = temp)) +
  heatwaveR::geom_flame(aes(y2 = thresh), n = 5, n_gap = 2) +
  geom_line(aes(y = temp)) +
  geom_line(aes(y = seas), colour = "green") +
  geom_line(aes(y = thresh), colour = "red") +
  labs(x = "", y = "Temperature (°C)")

# Create interactive visuals
ggplotly(p)




lapply(mhw, function(x) write.table( data.frame(x), 'test.csv'  , append= T, sep=',' ))
