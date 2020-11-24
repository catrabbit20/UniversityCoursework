app([], X, X).
app([F|L1], L2, [F|L3]) :- app(L1, L2, L3).
