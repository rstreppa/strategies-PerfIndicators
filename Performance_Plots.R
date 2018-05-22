library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)

#####################
# Performance Plots #
#####################

perf_plot <- function(oos_df, cumspx, cumperf, aSR, aRoR, freq_days, tCost, tRate)
{
  # data
  perftable <- data.frame(indicators=c("Sharpe Ratio","Annualized Return %",
                                       "Frequency (days)","Transaction Cost","VWAP"),
                          values=c(aSR,aRoR,freq_days,tCost,tRate))

  perf_df   <- data.frame(date = as.Date(rownames(oos_df)),
                          SPX = cumspx,
                          Strategy = cumperf)
  Molten    <- melt(perf_df, id.vars = "date")

  # plot items
  tGrob <- tableGrob(perftable, rows=NULL)
  plt   <- ggplot(Molten, aes(x = date, y = value, colour = variable)) + geom_line()

  # layout
  vp.layout <- grid.layout(nrow=2, ncol=1, heights=unit(1, "null"),
                           widths=unit(c(1,9), c("null","line")) )
  # start drawing
  grid.newpage()
  pushViewport(viewport(layout=vp.layout, name="layout"))
  # plot
  pushViewport(viewport(layout.pos.row=1, layout.pos.col=1, name="plot"))
  print(plt, newpage=FALSE)
  upViewport()
  # table
  pushViewport(viewport(layout.pos.row=2, layout.pos.col=1, name="table"))
  grid.draw(tGrob)
  upViewport()
}
