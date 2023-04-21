# Remove export directory 
unlink("export", recursive = TRUE)

# Create export folder structure 
dir.create(file.path("export"), recursive = TRUE)

# figures
dir.create(file.path("export/figures"), recursive = TRUE)
dir.create(file.path("export/figures/Paper1"), recursive = TRUE)
dir.create(file.path("export/figures/Thesis"), recursive = TRUE)
dir.create(file.path("export/figures/Paper1/ChoiceProportions"), recursive = TRUE)
dir.create(file.path("export/figures/Paper1/DefriefQuestions"), recursive = TRUE)
dir.create(file.path("export/figures/Paper1/LogitResponse"), recursive = TRUE)
dir.create(file.path("export/figures/Paper1/Simulations"), recursive = TRUE)
dir.create(file.path("export/figures/Paper1/TimeValuation"), recursive = TRUE)

dir.create(file.path("export/figures/Paper1/Simulations/FixedCTimePerception"), recursive = TRUE)
dir.create(file.path("export/figures/Paper1/Simulations/FreeCTimePerception"), recursive = TRUE)
dir.create(file.path("export/figures/Paper1/Simulations/GridSearch"), recursive = TRUE)
dir.create(file.path("export/figures/Paper1/Simulations/LogitScaleParameter"), recursive = TRUE)
dir.create(file.path("export/figures/Paper1/Simulations/RelationCScaleParameter"), recursive = TRUE)
dir.create(file.path("export/figures/Paper1/Simulations/WaitingTimeFiller"), recursive = TRUE)


#tables
dir.create(file.path("export/tables"), recursive = TRUE)
# dir.create(file.path("export/tables/HypothesisTesting"), recursive = TRUE)
# dir.create(file.path("export/tables/Models"), recursive = TRUE)
dir.create(file.path("export/tables/Paper1"), recursive = TRUE)
# dir.create(file.path("export/tables/Results"), recursive = TRUE)
dir.create(file.path("export/tables/Paper1/Latex/LogisticRegression"), recursive = TRUE)
# dir.create(file.path("export/tables/HypothesisTesting/BLR"), recursive = TRUE)
# dir.create(file.path("export/tables/HypothesisTesting/McNemar/Csv"), recursive = TRUE)
# dir.create(file.path("export/tables/HypothesisTesting/McNemar/Txt"), recursive = TRUE)
# dir.create(file.path("export/tables/HypothesisTesting/"), recursive = TRUE)
# dir.create(file.path("export/tables/HypothesisTesting/"), recursive = TRUE)



