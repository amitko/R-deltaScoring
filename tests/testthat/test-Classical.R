test_that("bootstrapping Deltas",{

  itemData = read.csv('item_scores.csv')

db<-dS.deltaBootstrap(itemData)
db
PS<-dS.personDscore(itemData,db$delta)
PS
expect_type(db,'list')
expect_type(PS,'list')
}
)
