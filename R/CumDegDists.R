library(dplyr)
library(ggplot2)

# January -----------------------------------------------------------------

filenames <- list.files("data/output/degDists/Jan", pattern="*.RData",
                        full.names=TRUE)
lapply(filenames,load,.GlobalEnv)

cCumDist <- data.frame(cDegDists["deg"])
for (i in seq_len(nrow(dates))){
  temp <- cDegDists %>% mutate(cum_frequency=cumsum(.data[[names(cDegDists[i+1])]]))
  cCumDist[names(cDegDists[i+1])] <- temp$cum_frequency / temp$cum_frequency[nrow(temp)] * 100
}

#Find average across January
cCumDist$mean <- rowMeans(subset(cCumDist, select = -1))
cCumDistJan <- cCumDist[, c(1, ncol(cCumDist))]

bCumDist <- data.frame(bDegDists["deg"])
for (i in seq_len(nrow(dates))){
  temp <- bDegDists %>% mutate(cum_frequency=cumsum(.data[[names(bDegDists[i+1])]]))
  bCumDist[names(bDegDists[i+1])] <- temp$cum_frequency / temp$cum_frequency[nrow(temp)] * 100
}

#Find average across January
bCumDist$mean <- rowMeans(subset(bCumDist, select = -1))
bCumDist[nrow(bCumDist) + 1, ] <- c(50, rep(NA, nrow(dates)), 100)
bCumDistJan <- bCumDist[, c(1, ncol(bCumDist))]

rm(list=setdiff(ls(), c("bCumDistJan", "cCumDistJan")))


# April -------------------------------------------------------------------

filenames <- list.files("data/output/degDists/Apr", pattern="*.RData",
                        full.names=TRUE)
lapply(filenames,load,.GlobalEnv)

cCumDist <- data.frame(cDegDists["deg"])
for (i in seq_len(nrow(dates))){
  temp <- cDegDists %>% mutate(cum_frequency=cumsum(.data[[names(cDegDists[i+1])]]))
  cCumDist[names(cDegDists[i+1])] <- temp$cum_frequency / temp$cum_frequency[nrow(temp)] * 100
}

#Find average across April
cCumDist$mean <- rowMeans(subset(cCumDist, select = -1))
cCumDistApr <- cCumDist[, c(1, ncol(cCumDist))]

bCumDist <- data.frame(bDegDists["deg"])
for (i in seq_len(nrow(dates))){
  temp <- bDegDists %>% mutate(cum_frequency=cumsum(.data[[names(bDegDists[i+1])]]))
  bCumDist[names(bDegDists[i+1])] <- temp$cum_frequency / temp$cum_frequency[nrow(temp)] * 100
}

#Find average across April
bCumDist$mean <- rowMeans(subset(bCumDist, select = -1))
bCumDistApr <- bCumDist[, c(1, ncol(bCumDist))]

rm(list=setdiff(ls(), c("bCumDistJan", "cCumDistJan", "bCumDistApr", "cCumDistApr")))


# Plot --------------------------------------------------------------------

library(zoo)
library(reshape2)
cCumDist <- merge(cCumDistJan, cCumDistApr, by = "deg", all = TRUE)
cCumDist$mean.x <- na.locf(cCumDist$mean.x)
cCumDist <- rename(cCumDist, Jan = mean.x)
cCumDist <- rename(cCumDist, Apr = mean.y)
cCumDist <- melt(cCumDist, id='deg')
cCumDist <- rename(cCumDist, Month = variable)

g1 <- ggplot() + geom_step(data = cCumDist, aes(x = deg, y = value, color = Month, group = Month, alpha = Month), size = 1) +
  xlab("Cell-Level Degree") + ylab("Cumulative %") +
  scale_color_manual(values=c("darkred", "cornflowerblue")) + scale_alpha_manual(values = c(1, 0.8)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(cCumDist$deg))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 105)) +
  ggtitle("Cell-Level")

bCumDist <- merge(bCumDistJan, bCumDistApr, by = "deg", all = TRUE)
bCumDist$mean.x[1:7] <- 0
bCumDist$mean.x <- na.locf(bCumDist$mean.x)
bCumDist <- rename(bCumDist, Jan = mean.x)
bCumDist <- rename(bCumDist, Apr = mean.y)
bCumDist <- melt(bCumDist, id='deg')
bCumDist <- rename(bCumDist, Month = variable)

g2 <- ggplot() + geom_step(data = bCumDist, aes(x = deg, y = value, color = Month, group = Month, alpha = Month), size = 1) +
  xlab("Block-Level Degree") + ylab("Cumulative %") +
  scale_color_manual(values=c("darkred", "cornflowerblue")) + scale_alpha_manual(values = c(1, 0.8)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(bCumDist$deg))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 105)) +
  ggtitle("Block-Level")

library(patchwork)
combined <- g1 + g2 & theme(legend.position = "right")
combined + plot_layout(ncol = 2, guides = "collect")
ggsave("CumulativeDegDist.tiff", units="in", width=12, height = 7.6, dpi=300,
       compression = 'lzw')
ggsave("CumulativeDegDist.pdf", units="in", width=12, height = 7.6, dpi=300)
