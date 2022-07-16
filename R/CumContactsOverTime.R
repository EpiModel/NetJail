
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

g1 <- ggplot(rbind(cCumContactsJan, data.frame(date = as.Date("2022-01-22"), cCumContacts = tail(cCumContactsJan$cCumContacts, 1))), aes(x = date, y = cCumContacts)) +
  geom_step() + xlab("Date") + ylab("Avg. Cumulative Cell-Level Contacts") +
  scale_x_date(expand = c(0, 0), limits = as.Date(c("2022-01-11","2022-01-22"))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4)) +
  ggtitle("Cumulative Cell-Level Contacts (January)")

g2 <- ggplot(rbind(bCumContactsJan, data.frame(date = as.Date("2022-01-22"), bCumContacts = tail(bCumContactsJan$bCumContacts, 1))), aes(x = date, y = bCumContacts)) +
  geom_step() + xlab("Date") + ylab("Avg. Cumulative Block-Level Contacts") +
  scale_x_date(expand = c(0, 0), limits = as.Date(c("2022-01-11","2022-01-22"))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
  ggtitle("Cumulative Block-Level Contacts (January)")

g3 <- ggplot(rbind(cCumContactsApr, data.frame(date = as.Date("2022-04-12"), cCumContacts = tail(cCumContactsApr$cCumContacts, 1))), aes(x = date, y = cCumContacts)) +
  geom_step() + xlab("Date") + ylab("Avg. Cumulative Cell-Level Contacts") +
  scale_x_date(expand = c(0, 0), limits = as.Date(c("2022-04-01","2022-04-12"))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4)) +
  ggtitle("Cumulative Cell-Level Contacts (April)")

g4 <- ggplot(rbind(bCumContactsApr, data.frame(date = as.Date("2022-04-12"), bCumContacts = tail(bCumContactsApr$bCumContacts, 1))), aes(x = date, y = bCumContacts)) +
  geom_step() + xlab("Date") + ylab("Avg. Cumulative Block-Level Contacts") +
  scale_x_date(expand = c(0, 0), limits = as.Date(c("2022-04-01","2022-04-12"))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 50)) +
  ggtitle("Cumulative Block-Level Contacts (April)")

library(patchwork)
combined <- g1 + g2 + g3 + g4
combined + plot_layout(ncol = 2, guides = "collect")
ggsave("CumulativeContacts.tiff", units="in", width=12, height = 10, dpi=300,
       compression = 'lzw')
ggsave("CumulativeContacts.pdf", units="in", width=12, height = 10, dpi=300)
