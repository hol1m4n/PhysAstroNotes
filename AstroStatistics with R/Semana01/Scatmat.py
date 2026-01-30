import seaborn as sns

penguins = sns.load_dataset("penguins")
g = sns.PairGrid(penguins, hue="species")
g.map_diag(sns.histplot)
g.map_offdiag(sns.scatterplot)
g.add_legend()

penguins = sns.load_dataset("penguins")
g = sns.PairGrid(penguins, hue="species")
g.map_diag(sns.distplot)
g.map_offdiag(sns.scatterplot)
g.add_legend()