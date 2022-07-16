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

#Plot for 1 day in January
# g0 <- ggplot(cCumDist, aes(x = deg, y = `2022-01-11`)) +
#   geom_step() + xlab("Cell-Level Degree") + ylab("Cumulative %") +
#   scale_x_continuous(expand = c(0, 0), limits = c(0, max(cCumDist$deg))) +
#   scale_y_continuous(expand = c(0, 0), limits = c(0, 105)) +
#   ggtitle("Cumulative Cell-Level Degree Distribution (1/11)")

#Plot average across January
cCumDist$mean <- rowMeans(subset(cCumDist, select = -1))
g1 <- ggplot(cCumDist, aes(x = deg, y = mean)) +
  geom_step() + xlab("Cell-Level Degree") + ylab("Cumulative %") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(cCumDist$deg))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 105)) +
  ggtitle("Cumulative Cell-Level Degree Distribution (January)")

bCumDist <- data.frame(bDegDists["deg"])
for (i in seq_len(nrow(dates))){
  temp <- bDegDists %>% mutate(cum_frequency=cumsum(.data[[names(bDegDists[i+1])]]))
  bCumDist[names(bDegDists[i+1])] <- temp$cum_frequency / temp$cum_frequency[nrow(temp)] * 100
}

#Plot average across January
bCumDist$mean <- rowMeans(subset(bCumDist, select = -1))
bCumDist[nrow(bCumDist) + 1, ] <- c(50, rep(NA, nrow(dates)), 100)
g2 <- ggplot(bCumDist, aes(x = deg, y = mean)) +
  geom_step() + xlab("Block-Level Degree") + ylab("Cumulative %") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(bCumDist$deg))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 105)) +
  ggtitle("Cumulative Block-Level Degree Distribution (January)")

rm(list=setdiff(ls(), c("g1", "g2")))


# April -------------------------------------------------------------------

filenames <- list.files("data/output/degDists/Apr", pattern="*.RData",
                        full.names=TRUE)
lapply(filenames,load,.GlobalEnv)

cCumDist <- data.frame(cDegDists["deg"])
for (i in seq_len(nrow(dates))){
  temp <- cDegDists %>% mutate(cum_frequency=cumsum(.data[[names(cDegDists[i+1])]]))
  cCumDist[names(cDegDists[i+1])] <- temp$cum_frequency / temp$cum_frequency[nrow(temp)] * 100
}

#Plot average across April
cCumDist$mean <- rowMeans(subset(cCumDist, select = -1))
g3 <- ggplot(cCumDist, aes(x = deg, y = mean)) +
  geom_step() + xlab("Cell-Level Degree") + ylab("Cumulative %") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(cCumDist$deg))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 105)) +
  ggtitle("Cumulative Cell-Level Degree Distribution (April)")

bCumDist <- data.frame(bDegDists["deg"])
for (i in seq_len(nrow(dates))){
  temp <- bDegDists %>% mutate(cum_frequency=cumsum(.data[[names(bDegDists[i+1])]]))
  bCumDist[names(bDegDists[i+1])] <- temp$cum_frequency / temp$cum_frequency[nrow(temp)] * 100
}

#Plot average across April
bCumDist$mean <- rowMeans(subset(bCumDist, select = -1))
g4 <- ggplot(bCumDist, aes(x = deg, y = mean)) +
  geom_step() + xlab("Block-Level Degree") + ylab("Cumulative %") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, max(bCumDist$deg))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 105)) +
  ggtitle("Cumulative Block-Level Degree Distribution (April)")


rm(list=setdiff(ls(), c("g1", "g2", "g3", "g4")))

library(patchwork)
combined <- g1 + g2 + g3 + g4
combined + plot_layout(ncol = 2, guides = "collect")
ggsave("CumulativeDegDist.tiff", units="in", width=12, height = 10, dpi=300,
       compression = 'lzw')
ggsave("CumulativeDegDist.pdf", units="in", width=12, height = 10, dpi=300)
