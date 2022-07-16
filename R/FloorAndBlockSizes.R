firstDay <- as.Date("2021-10-27")
lastDay <- as.Date("2022-05-27")

#Run Step 0 Lines 1 thru 105

locs <- as.data.frame(unique(m_stdLoc$Location))
locs <- separate(data = locs, col = 1, into = c("FloorTower", "Block", "Cell"), sep = c(2, 3))
BlocksPerFloor <- locs %>% group_by(FloorTower) %>% summarise(numBlocks = n_distinct(Block), .groups = "drop_last")
CellsPerBlock <- locs %>% group_by(FloorTower, Block) %>% summarise(numCells = n_distinct(Cell), .groups = "drop_last")

write.csv(CellsPerBlock, "CellsPerBlock.csv")
write.csv(BlocksPerFloor, "BlocksPerFloor.csv")
