#setwd("~/Dropbox (Personal)/UofT/canadian/essay2/replication_material/")

# Instructions file

# Load a 338 x 5 file. It's VC 2015 data with `Mean score 1D` `Mean self placement` by riding.
source("code/make_prelim_figures.R")
# This is slightly convoluted.
# I first clean the data a bit. Get theta estimates theta through wordfish for each policy subsection.
# Then model_code_linear is a linear IRT. Which is actually a 1D bayesian factor analysis.
source("code/wordfish_lipad.R")
# Prepare data for gensim's doc2vec
source("code/make_data_doc2vec.R")

# Run it in python
#python3 doc2vec_model.py

# Main Analyses. Doc2Vec. PCA, etc.
source("code/pca_analyses.R")