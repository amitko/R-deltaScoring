test_that("bootstrapping Deltas",{

  itemData = read.csv('item_scores.csv')

db<-deltaBootstrap(itemData)
expect_type(db,'list')
}
)
