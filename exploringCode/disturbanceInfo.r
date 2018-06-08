# exploring disturbances
# Scott's "hard coded a disturbance in the base code (see spadesCBMcore.R line 448)
# here trying to run it past that disturbance to see if it runs
# CBoisvenue June 6, 2018

# 1st I ran the code chunk in spades in spadesCBM.Rmd parent module
# here (below) I change the end time from 1999 (set on line 54 of spadesCBM.Rmd parent module)
# to 2052
end(spadesCBMout) <- 2052

# now I run a spades call again and create a new outsim
spadesCBMout1 <- spades(spadesCBMout,debug=TRUE)

##CONCLUSION: this disturbances is not being applied...need to check into this
# looks like the disturbance events where not made into spades events...might be easy to fix
# look into the annual event in the spadesCBMcore.r 'yearsEvents" not found...
