
# JANUARY -----------------------------------------------------------------

filenames <- list.files("data/output/edges/Jan", pattern="*.RData",
                        full.names=TRUE)
lapply(filenames,load,.GlobalEnv)

ids_s <- ids[ids$DaysOfData == nrow(dates), ]

cCumContacts <- data.frame(matrix(NA, nrow = nrow(dates), ncol = nrow(ids_s)))
bCumContacts <- data.frame(matrix(NA, nrow = nrow(dates), ncol = nrow(ids_s)))
for (j in seq_len(nrow(ids_s))){
  cCurrNode <- cEdges[cEdges$head == ids_s$id[j] | cEdges$tail == ids_s$id[j], ]
  bCurrNode <- bEdges[bEdges$head == ids_s$id[j] | bEdges$tail == ids_s$id[j], ]
  for (i in seq_len(nrow(dates))){
    cCumContacts[i,j] <- sum(unique(cCurrNode[cCurrNode$onset <= i, ]$head) != ids_s$id[j]) + sum(unique(cCurrNode[cCurrNode$onset <= i, ]$tail) != ids_s$id[j])
    bCumContacts[i,j] <- sum(unique(bCurrNode[bCurrNode$onset <= i, ]$head) != ids_s$id[j]) + sum(unique(bCurrNode[bCurrNode$onset <= i, ]$tail) != ids_s$id[j])
  }
}

cCumContacts$mean <- rowMeans(cCumContacts)
cCumContactsJan <- data.frame(date = dates$Date, cCumContacts = cCumContacts[, ncol(cCumContacts)])

bCumContacts$mean <- rowMeans(bCumContacts)
bCumContactsJan <- data.frame(date = dates$Date, bCumContacts = bCumContacts[, ncol(bCumContacts)])

rm(list=setdiff(ls(), c("bCumContactsJan", "cCumContactsJan")))


# APRIL -------------------------------------------------------------------
filenames <- list.files("data/output/edges/Apr", pattern="*.RData",
                        full.names=TRUE)
lapply(filenames,load,.GlobalEnv)

ids_s <- ids[ids$DaysOfData == nrow(dates), ]

cCumContacts <- data.frame(matrix(NA, nrow = nrow(dates), ncol = nrow(ids_s)))
bCumContacts <- data.frame(matrix(NA, nrow = nrow(dates), ncol = nrow(ids_s)))
for (j in seq_len(nrow(ids_s))){
  cCurrNode <- cEdges[cEdges$head == ids_s$id[j] | cEdges$tail == ids_s$id[j], ]
  bCurrNode <- bEdges[bEdges$head == ids_s$id[j] | bEdges$tail == ids_s$id[j], ]
  for (i in seq_len(nrow(dates))){
    cCumContacts[i,j] <- sum(unique(cCurrNode[cCurrNode$onset <= i, ]$head) != ids_s$id[j]) + sum(unique(cCurrNode[cCurrNode$onset <= i, ]$tail) != ids_s$id[j])
    bCumContacts[i,j] <- sum(unique(bCurrNode[bCurrNode$onset <= i, ]$head) != ids_s$id[j]) + sum(unique(bCurrNode[bCurrNode$onset <= i, ]$tail) != ids_s$id[j])
  }
}

cCumContacts$mean <- rowMeans(cCumContacts)
cCumContactsApr <- data.frame(date = dates$Date, cCumContacts = cCumContacts[, ncol(cCumContacts)])

bCumContacts$mean <- rowMeans(bCumContacts)
bCumContactsApr <- data.frame(date = dates$Date, bCumContacts = bCumContacts[, ncol(bCumContacts)])

rm(list=setdiff(ls(), c("bCumContactsJan", "cCumContactsJan", "bCumContactsApr", "cCumContactsApr")))


# PLOT --------------------------------------------------------------------
library(zoo)
library(reshape2)
cCumContactsApr$date <- 1:nrow(cCumContactsApr)
cCumContactsApr <- rbind(cCumContactsApr, c(nrow(cCumContactsApr) + 1, cCumContactsApr[nrow(cCumContactsApr), 2]))
cCumContactsJan$date <- 1:nrow(cCumContactsJan)
cCumContactsJan <- rbind(cCumContactsJan, c(nrow(cCumContactsJan) + 1, cCumContactsJan[nrow(cCumContactsJan), 2]))
cCumContacts <- merge(cCumContactsJan, cCumContactsApr, by = "date", all = TRUE)
cCumContacts <- rename(cCumContacts, Jan = cCumContacts.x)
cCumContacts <- rename(cCumContacts, Apr = cCumContacts.y)
cCumContacts <- melt(cCumContacts, id='date')
cCumContacts <- rename(cCumContacts, Month = variable)

g1 <- ggplot() + geom_step(data = cCumContacts, aes(x = date, y = value, color = Month, group = Month, alpha = Month), size = 1) +
  xlab("Day") + ylab("Avg. Cumulative Cell-Level Contacts") +
  scale_color_manual(values=c("darkred", "cornflowerblue")) + scale_alpha_manual(values = c(1, 0.8)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 12),
                     breaks = c(1, 3, 5, 7, 9, 11),
                     labels = c("1" = "Day 1", "3" = "Day 3",
                                "5" = "Day 5",
                                "7" = "Day 7", "9" = "Day 9",
                                "11" = "Day 11")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4)) +
  ggtitle("Cumulative Cell-Level Contacts")

bCumContactsApr$date <- 1:nrow(bCumContactsApr)
bCumContactsApr <- rbind(bCumContactsApr, c(nrow(bCumContactsApr) + 1, bCumContactsApr[nrow(bCumContactsApr), 2]))
bCumContactsJan$date <- 1:nrow(bCumContactsJan)
bCumContactsJan <- rbind(bCumContactsJan, c(nrow(bCumContactsJan) + 1, bCumContactsJan[nrow(bCumContactsJan), 2]))
bCumContacts <- merge(bCumContactsJan, bCumContactsApr, by = "date", all = TRUE)
bCumContacts <- rename(bCumContacts, Jan = bCumContacts.x)
bCumContacts <- rename(bCumContacts, Apr = bCumContacts.y)
bCumContacts <- melt(bCumContacts, id='date')
bCumContacts <- rename(bCumContacts, Month = variable)

g2 <- ggplot() + geom_step(data = bCumContacts, aes(x = date, y = value, color = Month, group = Month, alpha = Month), size = 1) +
  xlab("Day") + ylab("Avg. Cumulative Block-Level Contacts") +
  scale_color_manual(values=c("darkred", "cornflowerblue")) + scale_alpha_manual(values = c(1, 0.8)) +
  scale_x_continuous(expand = c(0,0),
                     limits = c(1, 12),
                     breaks = c(1, 3, 5, 7, 9, 11),
                     labels = c("1" = "Day 1", "3" = "Day 3",
                                "5" = "Day 5",
                                "7" = "Day 7", "9" = "Day 9",
                                "11" = "Day 11")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
  ggtitle("Cumulative Block-Level Contacts")


library(patchwork)
combined <- g1 + g2 & theme(legend.position = "right")
combined + plot_layout(ncol = 2, guides = "collect")
ggsave("CumulativeContacts.tiff", units="in", width=12, height = 7.6, dpi=300,
       compression = 'lzw')
ggsave("CumulativeContacts.pdf", units="in", width=12, height = 7.6, dpi=300)
