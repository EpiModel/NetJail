# January -----------------------------------------------------------------

firstDay <- as.Date("2022-01-11")
lastDay <- as.Date("2022-01-21")

#Run Steps 0 - 2 + 1st part of 5 and 6

#CELL-LEVEL SETUP

temp1 <- cEdges[ ,c("head", "tail", "onset", "terminus")]
temp2 <- cEdges[ ,c("head", "tail", "onset", "terminus")]
temp2[1:2] <- temp2[2:1]

doubledEdges <- rbind(temp1, temp2)
remove(temp1, temp2)
doubledEdges <- merge(doubledEdges, ids[, c("id", "InitialAge")],
                      by.x = "head", by.y = "id")
doubledEdges$head.age <- cut(doubledEdges$InitialAge,
                             breaks=c(0, 20, 30, 40, 50, 60, 100),
                             right = FALSE, labels = c("Under 20", "20 - 29",
                                                       "30 - 39", "40 - 49",
                                                       "50 - 59", "60+"))
doubledEdges$InitialAge <- NULL
doubledEdges <- merge(doubledEdges, ids[, c("id", "InitialAge")],
                      by.x = "tail", by.y = "id")
doubledEdges$tail.age <- cut(doubledEdges$InitialAge,
                             breaks=c(0, 20, 30, 40, 50, 60, 100),
                             right = FALSE, labels = c("Under 20", "20 - 29",
                                                       "30 - 39", "40 - 49",
                                                       "50 - 59", "60+"))
doubledEdges$InitialAge <- NULL

#Calculate cell-level age mixing at each time point
cMixList.Jan <- list()
for (i in seq_len(nrow(dates))) {
  currTime <- doubledEdges[doubledEdges$onset <= dates$DayNum[i] &
                             doubledEdges$terminus >= (dates$DayNum[i] + 1), ]
  currTime <- currTime %>% group_by(head.age, tail.age, .drop = FALSE) %>%
    summarise(numEdges = n(), .groups = "drop_last")
  currTime.props <- currTime %>% group_by(head.age) %>%
    mutate(propEdges = numEdges / sum(numEdges))
  currTime.props$numEdges <- NULL

  cMixList.Jan[[length(cMixList.Jan) + 1]] <- currTime.props
}

cMixDF.Jan <- do.call(cbind, cMixList.Jan)
cMixDF.Jan$avg.prop <- rowSums(cMixDF.Jan[sapply(cMixDF.Jan, is.numeric)]) /
  length(cMixList.Jan)

cAgeMixing.Jan <- cMixDF.Jan[ , c(1, 2, ncol(cMixDF.Jan))]


#BLOCK-LEVEL SETUP
temp1 <- bEdges[ ,c("head", "tail", "onset", "terminus")]
temp2 <- bEdges[ ,c("head", "tail", "onset", "terminus")]
temp2[1:2] <- temp2[2:1]

doubledEdges <- rbind(temp1, temp2)
remove(temp1, temp2)
doubledEdges <- merge(doubledEdges, ids[, c("id", "InitialAge")],
                      by.x = "head", by.y = "id")
doubledEdges$head.age <- cut(doubledEdges$InitialAge,
                             breaks=c(0, 20, 30, 40, 50, 60, 100),
                             right = FALSE, labels = c("Under 20", "20 - 29",
                                                       "30 - 39", "40 - 49",
                                                       "50 - 59", "60+"))
doubledEdges$InitialAge <- NULL
doubledEdges <- merge(doubledEdges, ids[, c("id", "InitialAge")],
                      by.x = "tail", by.y = "id")
doubledEdges$tail.age <- cut(doubledEdges$InitialAge,
                             breaks=c(0, 20, 30, 40, 50, 60, 100),
                             right = FALSE, labels = c("Under 20", "20 - 29",
                                                       "30 - 39", "40 - 49",
                                                       "50 - 59", "60+"))
doubledEdges$InitialAge <- NULL

#Calculate block-level age mixing at each time point
bMixList.Jan <- list()
for (i in seq_len(nrow(dates))) {
  currTime <- doubledEdges[doubledEdges$onset <= dates$DayNum[i] &
                             doubledEdges$terminus >= (dates$DayNum[i] + 1), ]
  currTime <- currTime %>% group_by(head.age, tail.age, .drop = FALSE) %>%
    summarise(numEdges = n(), .groups = "drop_last")
  currTime.props <- currTime %>% group_by(head.age) %>%
    mutate(propEdges = numEdges / sum(numEdges))
  currTime.props$numEdges <- NULL

  bMixList.Jan[[length(bMixList.Jan) + 1]] <- currTime.props
}

bMixDF.Jan <- do.call(cbind, bMixList.Jan)
bMixDF.Jan$avg.prop <- rowSums(bMixDF.Jan[sapply(bMixDF.Jan, is.numeric)]) /
  length(bMixList.Jan)

bAgeMixing.Jan <- bMixDF.Jan[ , c(1, 2, ncol(bMixDF.Jan))]

rm(list=setdiff(ls(), c("bAgeMixing.Jan", "cAgeMixing.Jan")))


# April -------------------------------------------------------------------
firstDay <- as.Date("2022-04-01")
lastDay <- as.Date("2022-04-11")

#Run Steps 0 - 2 + 1st part of 5 and 6

#CELL-LEVEL SETUP

temp1 <- cEdges[ ,c("head", "tail", "onset", "terminus")]
temp2 <- cEdges[ ,c("head", "tail", "onset", "terminus")]
temp2[1:2] <- temp2[2:1]

doubledEdges <- rbind(temp1, temp2)
remove(temp1, temp2)
doubledEdges <- merge(doubledEdges, ids[, c("id", "InitialAge")],
                      by.x = "head", by.y = "id")
doubledEdges$head.age <- cut(doubledEdges$InitialAge,
                             breaks=c(0, 20, 30, 40, 50, 60, 100),
                             right = FALSE, labels = c("Under 20", "20 - 29",
                                                       "30 - 39", "40 - 49",
                                                       "50 - 59", "60+"))
doubledEdges$InitialAge <- NULL
doubledEdges <- merge(doubledEdges, ids[, c("id", "InitialAge")],
                      by.x = "tail", by.y = "id")
doubledEdges$tail.age <- cut(doubledEdges$InitialAge,
                             breaks=c(0, 20, 30, 40, 50, 60, 100),
                             right = FALSE, labels = c("Under 20", "20 - 29",
                                                       "30 - 39", "40 - 49",
                                                       "50 - 59", "60+"))
doubledEdges$InitialAge <- NULL

#Calculate cell-level age mixing at each time point
cMixList.Apr <- list()
for (i in seq_len(nrow(dates))) {
  currTime <- doubledEdges[doubledEdges$onset <= dates$DayNum[i] &
                             doubledEdges$terminus >= (dates$DayNum[i] + 1), ]
  currTime <- currTime %>% group_by(head.age, tail.age, .drop = FALSE) %>%
    summarise(numEdges = n(), .groups = "drop_last")
  currTime.props <- currTime %>% group_by(head.age) %>%
    mutate(propEdges = numEdges / sum(numEdges))
  currTime.props$numEdges <- NULL

  cMixList.Apr[[length(cMixList.Apr) + 1]] <- currTime.props
}

cMixDF.Apr <- do.call(cbind, cMixList.Apr)
cMixDF.Apr$avg.prop <- rowSums(cMixDF.Apr[sapply(cMixDF.Apr, is.numeric)]) /
  length(cMixList.Apr)

cAgeMixing.Apr <- cMixDF.Apr[ , c(1, 2, ncol(cMixDF.Apr))]


#BLOCK-LEVEL SETUP
temp1 <- bEdges[ ,c("head", "tail", "onset", "terminus")]
temp2 <- bEdges[ ,c("head", "tail", "onset", "terminus")]
temp2[1:2] <- temp2[2:1]

doubledEdges <- rbind(temp1, temp2)
remove(temp1, temp2)
doubledEdges <- merge(doubledEdges, ids[, c("id", "InitialAge")],
                      by.x = "head", by.y = "id")
doubledEdges$head.age <- cut(doubledEdges$InitialAge,
                             breaks=c(0, 20, 30, 40, 50, 60, 100),
                             right = FALSE, labels = c("Under 20", "20 - 29",
                                                       "30 - 39", "40 - 49",
                                                       "50 - 59", "60+"))
doubledEdges$InitialAge <- NULL
doubledEdges <- merge(doubledEdges, ids[, c("id", "InitialAge")],
                      by.x = "tail", by.y = "id")
doubledEdges$tail.age <- cut(doubledEdges$InitialAge,
                             breaks=c(0, 20, 30, 40, 50, 60, 100),
                             right = FALSE, labels = c("Under 20", "20 - 29",
                                                       "30 - 39", "40 - 49",
                                                       "50 - 59", "60+"))
doubledEdges$InitialAge <- NULL

#Calculate block-level age mixing at each time point
bMixList.Apr <- list()
for (i in seq_len(nrow(dates))) {
  currTime <- doubledEdges[doubledEdges$onset <= dates$DayNum[i] &
                             doubledEdges$terminus >= (dates$DayNum[i] + 1), ]
  currTime <- currTime %>% group_by(head.age, tail.age, .drop = FALSE) %>%
    summarise(numEdges = n(), .groups = "drop_last")
  currTime.props <- currTime %>% group_by(head.age) %>%
    mutate(propEdges = numEdges / sum(numEdges))
  currTime.props$numEdges <- NULL

  bMixList.Apr[[length(bMixList.Apr) + 1]] <- currTime.props
}

bMixDF.Apr <- do.call(cbind, bMixList.Apr)
bMixDF.Apr$avg.prop <- rowSums(bMixDF.Apr[sapply(bMixDF.Apr, is.numeric)]) /
  length(bMixList.Apr)

bAgeMixing.Apr <- bMixDF.Apr[ , c(1, 2, ncol(bMixDF.Apr))]

rm(list=setdiff(ls(), c("bAgeMixing.Jan", "cAgeMixing.Jan",
                        "bAgeMixing.Apr", "cAgeMixing.Apr")))




# Plot --------------------------------------------------------------------

library(ggplot2)
#library(gridExtra)
library(patchwork)

cols = rev(colorRampPalette(c('darkred','red','blue','lightblue'))(24))
minval <- min(min(bAgeMixing.Jan$avg.prop), min(cAgeMixing.Jan$avg.prop),
              min(bAgeMixing.Apr$avg.prop), min(cAgeMixing.Apr$avg.prop))
maxval <- max(max(bAgeMixing.Jan$avg.prop), max(cAgeMixing.Jan$avg.prop),
              max(bAgeMixing.Apr$avg.prop), max(cAgeMixing.Apr$avg.prop))

g1 <- ggplot(cAgeMixing.Jan, aes(x = head.age...1, y = tail.age...2,
                                 z = avg.prop, fill = avg.prop)) +
  geom_raster(interpolate = T) +
  scale_fill_gradientn(colors = cols, limits = c(minval, maxval)) +
  ggtitle("Age Mixing in Cell-Level Network (January)") +
  xlab("Age of Individual") + ylab("Age of Contact") +
  labs(fill = "Proportion of Edges")
#print(g1)

g2 <- ggplot(bAgeMixing.Jan, aes(x = head.age...1, y = tail.age...2,
                                 z = avg.prop, fill = avg.prop)) +
  geom_raster(interpolate = T) +
  scale_fill_gradientn(colors = cols, limits = c(minval, maxval)) +
  ggtitle("Age Mixing in Block-Level Network (January)") +
  xlab("Age of Individual") + ylab("Age of Contact") +
  labs(fill = "Proportion of Edges")
#print(g2)

g3 <- ggplot(cAgeMixing.Apr, aes(x = head.age...1, y = tail.age...2,
                                 z = avg.prop, fill = avg.prop)) +
  geom_raster(interpolate = T) +
  scale_fill_gradientn(colors = cols, limits = c(minval, maxval)) +
  ggtitle("Age Mixing in Cell-Level Network (April)") +
  xlab("Age of Individual") + ylab("Age of Contact") +
  labs(fill = "Proportion of Edges")
#print(g3)

g4 <- ggplot(bAgeMixing.Apr, aes(x = head.age...1, y = tail.age...2, z = avg.prop,
                                 fill = avg.prop)) +
  geom_raster(interpolate = T) +
  scale_fill_gradientn(colors = cols, limits = c(minval, maxval)) +
  ggtitle("Age Mixing in Block-Level Network (April)") +
  xlab("Age of Individual") + ylab("Age of Contact") +
  labs(fill = "Proportion of Edges")
#print(g4)

combined <- g1 + g2 + g3 + g4 & theme(legend.position = "bottom")
combined + plot_layout(guides = "collect")
ggsave("AgeMixing.tiff", units="in", width=12, height = 10, dpi=300,
       compression = 'lzw')
ggsave("AgeMixing.pdf", units="in", width=12, height = 10, dpi=300)

