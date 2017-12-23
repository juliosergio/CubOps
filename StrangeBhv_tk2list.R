library(tcltk2)

win1 <- tktoplevel()
win1$env$combo <- tk2combobox(win1)
tkgrid(win1$env$combo, padx = 10, pady = 15)

# A couple of functions to interact with the combobox:
# Fill the combobox list
fruits <- c("Apple", "Orange", "Banana")
tk2list.set(win1$env$combo, fruits)
