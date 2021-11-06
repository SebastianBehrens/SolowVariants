```
call <- function(){
my.name <- readline(prompt="Enter name: ")
my.age <- readline(prompt="Enter age: ")
# convert character into integer
my.age <- as.integer(my.age)
print(paste("Hi,", my.name, "next year you will be", my.age+1, "years old."))
}
call()
```

# create such an interactive function to easily simulate teh solow models

# ESSRO only has one ss variable in steadystate_checker => where are others?
# ESSROL only has gY for steadystate_checker
# add the transition diagrams for various models
# simplify the numerous calls of `which(sim_table$period == 0)` (occuring in all simulation functions)
==> example from ESEGRomer
```
aux_index <- which(sim_table$period == 0)
    sim_table[[aux_index, "L"]] <- startvals$L
    sim_table[[aux_index, "K"]] <- startvals$K
    sim_table[[aux_index, "TFP"]] <- startvals$A
    sim_table[[aux_index, "Y"]] <- ESEGRomer_MF_Y(
        sim_table[["TFP"]][[which(sim_table$period == 0)]],
        sim_table[["K"]][[which(sim_table$period == 0)]],
        sim_table[["L"]][[which(sim_table$period == 0)]],
        paragrid[["alpha"]][[which(paragrid$period == 0)]],
        paragrid[["phi"]][[which(paragrid$period == 0)]])
```

*Sketch of transition diagrams*
have transition equation in form of BS_TE
have function entered pairs of parameter values 
fill object with all possible inputs to the xx_TE

do a doCall on the BS_TE two times
first for first parameter entries
then for second parameter entries
=> one can do comp. statics in one graph with this function

*sketch solow diagrams*
have solow equations broken down into two parts
BS_SE => a) BS_SE_P1 b) BS_SE_P2

same do parameter entry to the draw_solow_diagram function
and then provide for comparative statics by drawing four lines (first set of parameters in dashed lines and second set of parameters in non-dashed lines)
