# Library
install.packages("keras")
install.packages("devtools")
install.packages("Rcpp")
install.packages("NMF")
install.packages("bigmemory")
install.packages("e1071")
devtools::install_github("rstudio/keras")
install.packages("caret")
install.packages("recommenderlab")
install_keras()
install.packages("listviewer")
install.packages("reshape")
library(keras)
library(reticulate)
library(nnet)
library(caret)
library(e1071)
library(recommenderlab)
library(listviewer)
library(dplyr)
library(reshape)

source("D:\\Sabermetrics\\Function.R")
source("D:\\Sabermetrics\\RShiny.R")

# Main_function
main_simulator_function()

# Init_function
load_variable_function()
load_lineup_function()
inning_setting_function()
load_player_function()

# Predict_function
predict_multinom_function()
predict_nnet_function()
predict_keras_function()

# Hit_function
base_hit_function()
two_base_hit_function()
three_base_hit_function()
home_run_function()

# Out_function
single_out_function()
double_out_function()

# Result_function
end_function()
create_result_function()
reset_test_function()

# Test_function
test_function()
