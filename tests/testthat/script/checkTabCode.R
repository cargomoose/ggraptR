browser()

waitFor({ text(root %>% getEl('#generateCode')) != '' })
expect_true(text(root %>% getEl('#generateCode')) == gsub(
  '\\n *', '', 'ggplot(diamonds, aes(y=price, x=carat)) + 
  geom_point(aes(colour=color), stat=\"identity\", position=\"jitter\", 
  alpha=0.5, size=3) + theme_grey() + theme(text=element_text(family=\"sans\", 
  face=\"plain\", color=\"#000000\", size=15, hjust=0.5, vjust=0.5)) + 
  scale_size(range=c(1, 3)) + xlab(\"carat\") + ylab(\"price\")'))