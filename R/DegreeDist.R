
# January -----------------------------------------------------------------

firstDay <- as.Date("2022-01-11")
lastDay <- as.Date("2022-01-21")

#Run Steps 0 - 6 and first parts of 7 (8-25, 36-46) and 9 (8-38, 50-60)

cMeanDeg.Jan <- cMeanDeg
bMeanDeg.Jan <- bMeanDeg

rm(list=setdiff(ls(), c("bMeanDeg.Jan", "cMeanDeg.Jan")))


# April -------------------------------------------------------------------

firstDay <- as.Date("2022-04-01")
lastDay <- as.Date("2022-04-11")

#Run Steps 0 - 6 and first parts of 7 (8-25, 36-46) and 9 (8-38, 50-60)

cMeanDeg.Apr <- cMeanDeg
bMeanDeg.Apr <- bMeanDeg

rm(list=setdiff(ls(), c("bMeanDeg.Jan", "cMeanDeg.Jan",
                        "bMeanDeg.Apr", "cMeanDeg.Apr")))


# Plot --------------------------------------------------------------------

library(ggplot2)
library(patchwork)

cMeanDeg <- rbind(cMeanDeg.Jan, cMeanDeg.Apr)
cMeanDeg$Month <- c(rep("Jan", 11), rep("Apr", 11))
cMeanDeg$ind <- c(1:11, 14:24)

bMeanDeg <- rbind(bMeanDeg.Jan, bMeanDeg.Apr)
bMeanDeg$Month <- c(rep("Jan", 11), rep("Apr", 11))
bMeanDeg$ind <- c(1:11, 14:24)

g1 <- ggplot(cMeanDeg,aes(x=ind, fill = Month)) +
  geom_boxplot(aes(lower=ifelse((cMeanDeg - sd) < 0, 0, cMeanDeg - sd),
                   upper=(cMeanDeg+sd), middle=cMeanDeg,
                   ymin=min, ymax=max, group = dates.Date),
               stat="identity") +
  scale_fill_manual(values=c("lightblue", "darkred")) +
  ggtitle("Mean degree of cell-level network over time") +
  xlab("Date") + ylab("Degree") + ylim(NA, 20) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
                                14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24),
                     labels = c("1" = "1/11", "2" = "1/12", "3" = "1/13",
                                "4" = "1/14", "5" = "1/15", "6" = "1/16",
                                "7" = "1/17", "8" = "1/18", "9" = "1/19",
                                "10" = "1/20", "11" = "1/21", "14" = "4/1",
                                "15" = "4/2", "16" = "4/3", "17" = "4/4",
                                "18" = "4/5", "19" = "4/6", "20" = "4/7",
                                "21" = "4/8", "22" = "4/9", "23" = "4/10",
                                "24" = "4/11")) +
  guides(fill = guide_legend(reverse = T))

g2 <- ggplot(bMeanDeg,aes(x=ind, fill = Month)) +
  geom_boxplot(aes(lower=(bMeanDeg-sd), upper=(bMeanDeg+sd), middle=bMeanDeg,
                   ymin=min, ymax=max, group = dates.Date),
               stat="identity") +
  scale_fill_manual(values=c("lightblue", "darkred")) +
  ggtitle("Mean degree of block-level network over time") +
  xlab("Date") + ylab("Degree") + ylim(NA, 60) +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
                                14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24),
                     labels = c("1" = "1/11", "2" = "1/12", "3" = "1/13",
                                "4" = "1/14", "5" = "1/15", "6" = "1/16",
                                "7" = "1/17", "8" = "1/18", "9" = "1/19",
                                "10" = "1/20", "11" = "1/21", "14" = "4/1",
                                "15" = "4/2", "16" = "4/3", "17" = "4/4",
                                "18" = "4/5", "19" = "4/6", "20" = "4/7",
                                "21" = "4/8", "22" = "4/9", "23" = "4/10",
                                "24" = "4/11")) +
  guides(fill = guide_legend(reverse = T))

combined <- g1 + g2 & theme(legend.position = "right")
combined + plot_layout(nrow = 2, byrow = FALSE, guides = "collect")

ggsave("DegreeDist.tiff", units="in", width=12, height = 7.6, dpi=300,
       compression = 'lzw')
ggsave("DegreeDist.pdf", units="in", width=12, height = 7.6, dpi=300)
