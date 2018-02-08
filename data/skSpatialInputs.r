# figuring out the SK spatial runs input data
# goal is to reproduce the runs 
# CBoisvenue
# Feb.7, 2018

dev()
clearPlot()
eventDiagram(spadesCBMout)
completed(spadesCBMout)
end(spadesCBMout)
end(spadesCBMout) <- 2010
spadesCBMout1 <- spades(spadesCBMout, debug = TRUE)

end(spadesCBMout1)
inputs(spadesCBMout)
outputs(spadesCBMout)

# seems like the spadesCBMdefault modules does not need to be changed
# the main 