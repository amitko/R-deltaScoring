test_that("bootstrapping Deltas",{

  itemData = read.csv('item_scores.csv')

db<-dS.deltaBootstrap(itemData)
PS<-dS.personDscore(itemData,db$delta)
expect_type(db,'list')
expect_type(PS,'list')
}
)
