#main_simulator_function
main_simulator_function <- function() {
    while(DATA.OUT_CNT < 3) {
        cat("INNING           : ", INNING, "\n") 
        cat("HOME_AWAY        : ", HOME_AWAY, "\n") 
        #loadPlayerData()
        #predict_keras_function()
        predict_nnet_function()
        cat("DATA.HIT_OUT     : ", DATA.HIT_OUT, "\n")
        if(DATA.HIT_OUT == 1 || DATA.HIT_OUT == 4) {
            base_hit_function()
        } else if(DATA.HIT_OUT == 2) {
            two_base_hit_function()
        } else if(DATA.HIT_OUT == 3) {
            three_base_hit_function()
        } else if(DATA.HIT_OUT == 6) {
            home_run_function()
        } else if(DATA.HIT_OUT == 0) {
            single_out_function()
        } else if(DATA.HIT_OUT == 5) {
            double_out_function()
        }  
        cat("AWAY_BATTING_NUM : ", AWAY_BATTING_NUM, "\n")
        cat("HOME_BATTING_NUM : ", HOME_BATTING_NUM, "\n")
        cat("DATA.OUT_CNT     : ", DATA.OUT_CNT, "\n")
        if(DATA.OUT_CNT == 3) {
            end_function()
        }
    }
}

# load_variable_function
load_variable_function <- function() {
    HOME_AWAY <<- "AWAY"
    HOME_BATTING_NUM <<- 1
    HOME_BATTING_ORDER <<- 1
    AWAY_BATTING_NUM <<- 1
    AWAY_BATTING_ORDER <<- 1
    BASE <<- 0
    HOME_SCORE <<- c(0,0,0,0,0,0,0,0,0)
    AWAY_SCORE <<- c(0,0,0,0,0,0,0,0,0)
    INNING <<- 1
    DATA.HIT_OUT <<- 0
    DATA.OUT_CNT <<- 0
    AGAINST_TEAM_HOME <<- 6
    AGAINST_TEAM_AWAY <<- 5
}

# load_lineup_function
load_lineup_function <- function() {
    setwd("D:\\Sabermetrics\\LineUp")
    hometeam_line_up <<- read.csv("두산_HOME (2).csv")
    awayteam_line_up <<- read.csv("롯데_AWAY.csv")
}

# inning_setting_function
inning_setting_function <- function() {
  
}

#load_player_function
load_player_function <- function() {
  
}

# Predict_function
#predict_multinom_function
predict_multinom_function <- function() {
    if(HOME_AWAY == "AWAY") {
        for(i in 1:nrow(awayteam_line_up)){
            if(AWAY_BATTING_NUM == awayteam_line_up$BATTING_ORDER[i]) {
                file_name <<- paste(awayteam_line_up$PLAYER_NAME[i],"_2017.csv",sep="")
                print(file_name)
                setwd("D:\\Sabermetrics\\Player")
                test_data_main <<- read.csv("Test_data.csv", header = FALSE)
                away_score_temp <<- as.data.frame(AWAY_SCORE)
                home_score_temp <<- as.data.frame(HOME_SCORE)
                inning_diff <<- away_score_temp[INNING,1] - home_score_temp[INNING,1]
                temp_data <<- c(as.character.factor(awayteam_line_up$PLAYER_NAME[i]), ifelse(awayteam_line_up$BATTING_HAND[i] =="우타", "0", ifelse(awayteam_line_up$BATTING_ORDER[i] == "좌타", "1", "2")),
                                AGAINST_TEAM_HOME, as.character.factor(hometeam_line_up$PLAYER_NAME[10]), ifelse(hometeam_line_up$BATTING_ORDER[10] == "우투", "0", "1"), ifelse(HOME_AWAY == "HOME", "0", "1"), 
                                awayteam_line_up$BATTING_ORDER[i], INNING, 0, DATA.OUT_CNT, inning_diff, BASE, 0,"" )
                test_data_main <<- as.data.frame(test_data_main)
                temp_data <<- t(temp_data)
                temp_data <<- as.data.frame(temp_data)
                temp_data_end <<- rbind(test_data_main, temp_data)
                temp_data <<- rename(temp_data, c(V1="PLAYER_NAME", V2="BATTING_HAND8", V3="AGAINST_TEAM", V4="AGAINGST_PICHER", V5="PITCHING_HAND", V6="HOME_AWAY", V7="PLAYER_ORDER",
                                                 V8="INNING", V9="INNING_BATTING_ORDER", V10="OUT_CNT", V11="DIFF_SCORE_GRP", V12="BASE_SITUATION", V13="CUM_PITCHING_CNT", V14="HIT_OUT4"))
                write.csv(temp_data, file="Test_data.csv", row.names = FALSE )
                player_data <<- read.csv(file_name, header=TRUE)
                player_data_sub <<- read.csv(file_name, header=TRUE)
                AWAY_BATTING_ORDER <<- AWAY_BATTING_ORDER + 1
                train_data <<- player_data[,c(6:14)]
                HIT_OUT4 <<- player_data_sub[,c(18)]
                train_data <<- cbind(train_data, HIT_OUT4)
                train_data <<- multinom(HIT_OUT4~., data=train_data)
                test_data <<- read.csv("Test_data.csv", header=T)
                test_data2 <<- test_data[,c(5:13)]
                result <<- predict(train_data, newdata = test_data2)
                cat("Result        : ", result, "\n")
                DATA.HIT_OUT <<- result
                test_data$HIT_OUT4 <<- result
                setwd("D:\\Sabermetrics\\Result")
                write.table(test_data, col.names = FALSE, file="Test_data_result.csv", append=TRUE)       
            }    
        }
        if(AWAY_BATTING_NUM >= 9) {
            AWAY_BATTING_NUM <<- 1
        } else {
            AWAY_BATTING_NUM <<- AWAY_BATTING_NUM + 1  
        }
    } else if(HOME_AWAY == "HOME") {
    for(i in 1:nrow(hometeam_line_up)){
        if(HOME_BATTING_NUM == hometeam_line_up$BATTING_ORDER[i]) {
            file_name <- paste(hometeam_line_up$PLAYER_NAME[i],"_2017.csv",sep="")
            setwd("D:\\SaberMetrics\\Player")
            test_data_main <- read.csv("Test_data.csv", header = FALSE)
            away_score_temp <- as.data.frame(AWAY_SCORE)
            home_score_temp <- as.data.frame(HOME_SCORE)
            inning_diff <- home_score_temp[INNING,1] - away_score_temp[INNING,1]
            temp_data <- c(as.character.factor(hometeam_line_up$PLAYER_NAME[i]), ifelse(hometeam_line_up$BATTING_HAND[i] =="우타", "0", ifelse(hometeam_line_up$BATTING_ORDER[i] == "좌타", "1", "2")),
                            AGAINST_TEAM_AWAY, as.character.factor(awayteam_line_up$PLAYER_NAME[10]), ifelse(awayteam_line_up$BATTING_ORDER[10] == "우투", "0", "1"), ifelse(HOME_AWAY == "HOME", "0", "1"), 
                            hometeam_line_up$BATTING_ORDER[i], INNING, 0, DATA.OUT_CNT, inning_diff, BASE, 0,"" )
            test_data_main <- as.data.frame(test_data_main)
            temp_data <- t(temp_data)
            temp_data <- as.data.frame(temp_data)
            temp_data_end <- rbind(test_data_main, temp_data)
            temp_data <- rename(temp_data, c(V1="PLAYER_NAME", V2="BATTING_HAND", V3="AGAINST_TEAM", V4="AGAINGST_PICHER", V5="PITCHING_HAND", V6="HOME_AWAY", V7="PLAYER_ORDER",
                                             V8="INNING", V9="INNING_BATTING_ORDER", V10="OUT_CNT", V11="DIFF_SCORE_GRP", V12="BASE_SITUATION", V13="CUM_PITCHING_CNT", V14="HIT_OUT4"))
            write.csv( temp_data, file="Test_data.csv", row.names = FALSE )
            player_data <- read.csv(file_name, header=TRUE)
            player_data_sub <- read.csv(file_name, header=TRUE)
            HOME_BATTING_ORDER <<- HOME_BATTING_ORDER + 1
            train_data <- player_data[,c(6:14)]
            HIT_OUT4 <- player_data_sub[,c(18)]
            train_data <- cbind(train_data, HIT_OUT4)
            train_data <- multinom(HIT_OUT4~., data=train_data)
            test_data <- read.csv("Test_data.csv", header=T)
            #print(nrow(test_data))
            #print("nrowPlus")
            test_data2 <- test_data[,c(5:13)]
            print(train_data)
            result <- predict(train_data, newdata = test_data2)
            cat("Result        : ", result, "\n")
            #predict_data <- data.frame(awayteam_line_up$PLAYER_NAME[i], awayteam_line_up$BATTING_HAND[i], awayteam_line_up$HOME_AWAY[i], hometeam_line_up$PLAYER_NAME[10], hometeam_line_up$BATTING_HAND[10])
            #print(predict_data)
            DATA.HIT_OUT <<- result
            test_data$HIT_OUT4 <- result
            setwd("D:\\SaberMetrics\\Result")
            write.table(test_data, col.names = FALSE, file="Test_data_result.csv", append=TRUE)       
            }
        }
        if(HOME_BATTING_NUM >= 9) {
            HOME_BATTING_NUM <<- 1
        } else {
            HOME_BATTING_NUM <<- HOME_BATTING_NUM + 1
        }
    }
}

# predict_nnet_function
predict_nnet_function <- function() {
    if(HOME_AWAY == "AWAY") {
        for(i in 1:nrow(awayteam_line_up)){
            if(AWAY_BATTING_NUM == awayteam_line_up$BATTING_ORDER[i]) {
                file_name <- paste(awayteam_line_up$PLAYER_NAME[i],"_2017_2018.csv",sep="")
                setwd("D:\\SaberMetrics\\Player")
                test_data_main <- read.csv("Test_data.csv", header = FALSE)
                away_score_temp <- as.data.frame(AWAY_SCORE)
                home_score_temp <- as.data.frame(HOME_SCORE)
                inning_diff <- away_score_temp[INNING,1] - home_score_temp[INNING,1]
                temp_data <- c(as.character.factor(awayteam_line_up$PLAYER_NAME[i]), ifelse(awayteam_line_up$BATTING_HAND[i] =="우타", "0", ifelse(awayteam_line_up$BATTING_ORDER[i] == "좌타", "1", "2")),
                                AGAINST_TEAM_HOME, as.character.factor(hometeam_line_up$PLAYER_NAME[10]), ifelse(hometeam_line_up$BATTING_ORDER[10] == "우투", "0", "1"), ifelse(HOME_AWAY == "HOME", "0", "1"), 
                                awayteam_line_up$BATTING_ORDER[i], INNING, 0, DATA.OUT_CNT, inning_diff, BASE, 0,"" )
                test_data_main <- as.data.frame(test_data_main)
                temp_data <- t(temp_data)
                temp_data <- as.data.frame(temp_data)
                temp_data_end <- rbind(test_data_main, temp_data)
                temp_data <- rename(temp_data, c(V1="PLAYER_NAME", V2="BATTING_HAND", V3="AGAINST_TEAM", V4="AGAINGST_PICHER", V5="PITCHING_HAND", V6="HOME_AWAY", V7="PLAYER_ORDER",
                                                 V8="INNING", V9="INNING_BATTING_ORDER", V10="OUT_CNT", V11="DIFF_SCORE_GRP", V12="BASE_SITUATION", V13="CUM_PITCHING_CNT", V14="HIT_OUT4"))
                write.csv(temp_data, file="Test_data.csv", row.names = FALSE )
                player_data <- read.csv(file_name, header=TRUE)
                player_data_sub <- read.csv(file_name, header=TRUE)
                AWAY_BATTING_ORDER <<- AWAY_BATTING_ORDER + 1
                train_data <- player_data[,c(6:14)]
                HIT_OUT4 <- player_data_sub[,c(18)]
                train_data <- cbind(train_data, HIT_OUT4)
                hitout.ind <- class.ind(train_data$HIT_OUT4)
                train_data_nn <- cbind(train_data, hitout.ind)
                test_num <- ncol(train_data_nn)
                length <- length(train_data_nn$HIT_OUT4)
                train.nn  <-  nnet(x=train_data_nn[,c(1:9)], y=train_data_nn[,c(11:test_num)], size=7, softmax=TRUE, maxit=length)
                train.pred <- predict(train.nn, train_data_nn[,c(1:9)], type="class")
                test_data <- read.csv("Test_data.csv", header=T)
                test_data2 <- test_data[,c(5:13)]
                test.pred <- predict(train.nn, test_data2, type="class")
                #result <- predict(train_data, newdata = test_data2)
                summary(train.pred)
                summary(train_data_nn$HIT_OUT4)
                predicted <- as.factor(train.pred) 
                actual <- as.factor(train_data_nn$HIT_OUT4) 
                #confusionMatrix
                model.confusion.matrix <- table(predicted, actual)
                tt<-model.confusion.matrix
                print(confusionMatrix(predicted, actual))
                #confusionMatrix
                table(train.pred, train_data_nn$HIT_OUT4)
                result <- test.pred
                cat("Result        : ", result, "\n")
                DATA.HIT_OUT <<- result
                temp_data$HIT_OUT4 <- result
                test_data_result <- temp_data
                setwd("D:\\SaberMetrics\\Result")
                write.table(test_data_result, col.names = FALSE, file="Test_data_result.csv", append=TRUE)       
          }
    }
    if(AWAY_BATTING_NUM >= 9) {
        AWAY_BATTING_NUM <<- 1
    } else {
        AWAY_BATTING_NUM <<- AWAY_BATTING_NUM + 1  
    }
    } else if(HOME_AWAY == "HOME") {
        for(i in 1:nrow(hometeam_line_up)){
            if(HOME_BATTING_NUM == hometeam_line_up$BATTING_ORDER[i]) {
                file_name <- paste(hometeam_line_up$PLAYER_NAME[i],"_2017_2018.csv",sep="")
                setwd("D:\\SaberMetrics\\Player")
                test_data_main <- read.csv("Test_data.csv", header = FALSE)
                away_score_temp <- as.data.frame(AWAY_SCORE)
                home_score_temp <- as.data.frame(HOME_SCORE)
                inning_diff <- home_score_temp[INNING,1] - away_score_temp[INNING,1]
                temp_data <- c(as.character.factor(hometeam_line_up$PLAYER_NAME[i]), ifelse(hometeam_line_up$BATTING_HAND[i] =="우타", "0", ifelse(hometeam_line_up$BATTING_ORDER[i] == "좌타", "1", "2")),
                                AGAINST_TEAM_AWAY, as.character.factor(awayteam_line_up$PLAYER_NAME[10]), ifelse(awayteam_line_up$BATTING_ORDER[10] == "우투", "0", "1"), ifelse(HOME_AWAY == "HOME", "0", "1"), 
                                hometeam_line_up$BATTING_ORDER[i], INNING, 0, DATA.OUT_CNT, inning_diff, BASE, 0,"" )
                test_data_main <- as.data.frame(test_data_main)
                temp_data <- t(temp_data)
                temp_data <- as.data.frame(temp_data)
                temp_data_end <- rbind(test_data_main, temp_data)
                temp_data <- rename(temp_data, c(V1="PLAYER_NAME", V2="BATTING_HAND", V3="AGAINST_TEAM", V4="AGAINGST_PICHER", V5="PITCHING_HAND", V6="HOME_AWAY", V7="PLAYER_ORDER",
                                                 V8="INNING", V9="INNING_BATTING_ORDER", V10="OUT_CNT", V11="DIFF_SCORE_GRP", V12="BASE_SITUATION", V13="CUM_PITCHING_CNT", V14="HIT_OUT4"))
                write.csv(temp_data, file="Test_data.csv", row.names = FALSE )
                player_data <- read.csv(file_name, header=TRUE)
                player_data_sub <- read.csv(file_name, header=TRUE)
                HOME_BATTING_ORDER <<- HOME_BATTING_ORDER + 1
                train_data <- player_data[,c(6:14)]
                HIT_OUT4 <- player_data_sub[,c(18)]
                train_data <- cbind(train_data, HIT_OUT4)
                hitout.ind <- class.ind(train_data$HIT_OUT4)
                train_data_nn <- cbind(train_data, hitout.ind)
                test_num <- ncol(train_data_nn)
                train.nn  <-  nnet(x=train_data_nn[,c(1:9)], y=train_data_nn[,c(11:test_num)], size=7, softmax=TRUE)
                train.pred <- predict(train.nn, train_data_nn[,c(1:9)], type="class")
                test_data <- read.csv("Test_data.csv", header=T)
                test_data2 <- test_data[,c(5:13)]
                test_data2
                test.pred <- predict(train.nn, test_data2, type="class")
                #result <- predict(train_data, newdata = test_data2)
                summary(train.pred)
                summary(train_data_nn$HIT_OUT4)
                predicted <- as.factor(train.pred) 
                actual <- as.factor(train_data_nn$HIT_OUT4) 
                #confusionMatrix
                #model.confusion.matrix <- table(predicted, actual)
                #model.confusion.matrix
                #print(confusionMatrix(predicted, actual))
                #confusionMatrix
                table(train.pred, train_data_nn$HIT_OUT4)
                result <- test.pred
                cat("Result        : ", result, "\n")
                DATA.HIT_OUT <<- result
                temp_data$HIT_OUT4 <- result
                test_data_result <- temp_data
                setwd("D:\\SaberMetrics\\Result")
                write.table(test_data_result, col.names = FALSE, file="Test_data_result.csv", append=TRUE)    
            }
        }   
        if(HOME_BATTING_NUM >= 9) {
            HOME_BATTING_NUM <<- 1
        } else {
            HOME_BATTING_NUM <<- HOME_BATTING_NUM + 1
        }
    }
}

#predict_keras_function
predict_keras_function <- function() {
    if(HOME_AWAY == "AWAY") {
        for(i in 1:nrow(awayteam_line_up)){
            if(AWAY_BATTING_NUM == awayteam_line_up$BATTING_ORDER[i]) {
                file_name <- paste(awayteam_line_up$PLAYER_NAME[i],"_2017_2018.csv",sep="")
                setwd("D:\\SaberMetrics\\Player")
                test_data_main <- read.csv("Test_data.csv", header = FALSE)
                away_score_temp <- as.data.frame(AWAY_SCORE)
                home_score_temp <- as.data.frame(HOME_SCORE)
                inning_diff <- away_score_temp[INNING,1] - home_score_temp[INNING,1]
                temp_data <- c(as.character.factor(awayteam_line_up$PLAYER_NAME[i]), ifelse(awayteam_line_up$BATTING_HAND[i] =="우타", "0", ifelse(awayteam_line_up$BATTING_ORDER[i] == "좌타", "1", "2")),
                               AGAINST_TEAM_HOME, as.character.factor(hometeam_line_up$PLAYER_NAME[10]), ifelse(hometeam_line_up$BATTING_ORDER[10] == "우투", "0", "1"), ifelse(HOME_AWAY == "HOME", "0", "1"), 
                               awayteam_line_up$BATTING_ORDER[i], INNING, 0, DATA.OUT_CNT, inning_diff, BASE, 0,"" )
                test_data_main <- as.data.frame(test_data_main)
                temp_data <- t(temp_data)
                temp_data <- as.data.frame(temp_data)
                temp_data_end <- rbind(test_data_main, temp_data)
                temp_data <- rename(temp_data, c(V1="PLAYER_NAME", V2="BATTING_HAND", V3="AGAINST_TEAM", V4="AGAINGST_PICHER", V5="PITCHING_HAND", V6="HOME_AWAY", V7="PLAYER_ORDER",
                                                 V8="INNING", V9="INNING_BATTING_ORDER", V10="OUT_CNT", V11="DIFF_SCORE_GRP", V12="BASE_SITUATION", V13="CUM_PITCHING_CNT", V14="HIT_OUT4"))
                write.csv(temp_data, file="Test_data.csv", row.names = FALSE )
                player_data <- read.csv(file_name, header=TRUE)
                player_data_sub <- read.csv(file_name, header=TRUE)
                AWAY_BATTING_ORDER <<- AWAY_BATTING_ORDER + 1
                train_data <- player_data[,c(6:14)]
                HIT_OUT4 <- player_data_sub[,c(18)]
                train_data <- cbind(train_data, HIT_OUT4)
                train_data_matrix <- as.matrix(train_data[,1:9])
                dimnames(train_data_matrix) <- NULL
                train_data_mat <- cbind(train_data_matrix, train_data[,10])
                bs.training <- train_data_mat[, 1:9]
                test_data <- read.csv("Test_data.csv", header=T)
                test_data_result <- test_data
                test_data_temp <- test_data[,c(5:13)]
                HIT_OUT4 <- test_data[,c(14)]
                test_data <- cbind(test_data_temp, HIT_OUT4)
                test_data_matrix <- as.matrix(test_data[,1:9])
                dimnames(test_data_matrix) <- NULL
                test_data_mat <- cbind(test_data_matrix, test_data[,10])
                bs.test <- test_data_mat[, 1:9]
                bs.trainingtarget <- train_data_mat[, 10]
                bs.trainLabels <- to_categorical(bs.trainingtarget)
                bs.testtarget <- test_data_mat[, 10]
                #bs.testLabels <- to_categorical(bs.testtarget)
                model <- keras_model_sequential()
                test_units <- ncol(bs.trainLabels)
                model %>% 
                    layer_dense(units = 8, activation = 'relu', input_shape = c(9)) %>% 
                    layer_dropout(rate = 0.5) %>% 
                    layer_dense(units = test_units, activation = 'softmax')
                summary(model)
                model %>% compile(
                    loss = 'categorical_crossentropy',
                    #loss = 'mean_squared_error',
                    optimizer = 'sgd',
                    metrics = 'accuracy'
                )
                model %>% fit(
                    bs.training, 
                    bs.trainLabels, 
                    epochs = 15, 
                    batch_size = 5,
                    #verbose = 1,
                    validation_split = 0.1
                )
                result <- model %>% predict_classes(test_data_matrix)
                result_predict <- model %>% predict(test_data_matrix)  
            print(result)
            print(result_predict)
            cat("Result        : ", result, "\n")
            DATA.HIT_OUT <<- result
            test_data_result$HIT_OUT4 <- result
            setwd("D:\\SaberMetrics\\Result")
            write.table(test_data_result, col.names = FALSE, file="Test_data_result.csv", append=TRUE)       
            }
        }
        if(AWAY_BATTING_NUM >= 9) {
            AWAY_BATTING_NUM <<- 1
        } else {
            AWAY_BATTING_NUM <<- AWAY_BATTING_NUM + 1  
        }
    } else if(HOME_AWAY == "HOME") {
        for(i in 1:nrow(hometeam_line_up)){
            if(HOME_BATTING_NUM == hometeam_line_up$BATTING_ORDER[i]) {
                file_name <- paste(hometeam_line_up$PLAYER_NAME[i],"_2017_2018.csv",sep="")
                setwd("D:\\SaberMetrics\\Player")
                test_data_main <- read.csv("Test_data.csv", header = FALSE)
                away_score_temp <- as.data.frame(AWAY_SCORE)
                home_score_temp <- as.data.frame(HOME_SCORE)
                inning_diff <- home_score_temp[INNING,1] - away_score_temp[INNING,1]
                temp_data <- c(as.character.factor(hometeam_line_up$PLAYER_NAME[i]), ifelse(hometeam_line_up$BATTING_HAND[i] =="우타", "0", ifelse(hometeam_line_up$BATTING_ORDER[i] == "좌타", "1", "2")),
                                AGAINST_TEAM_AWAY, as.character.factor(awayteam_line_up$PLAYER_NAME[10]), ifelse(awayteam_line_up$BATTING_ORDER[10] == "우투", "0", "1"), ifelse(HOME_AWAY == "HOME", "0", "1"), 
                                hometeam_line_up$BATTING_ORDER[i], INNING, 0, DATA.OUT_CNT, inning_diff, BASE, 0,"" )
                test_data_main <- as.data.frame(test_data_main)
                temp_data <- t(temp_data)
                temp_data <- as.data.frame(temp_data)
                temp_data_end <- rbind(test_data_main, temp_data)
                temp_data <- rename(temp_data, c(V1="PLAYER_NAME", V2="BATTING_HAND", V3="AGAINST_TEAM", V4="AGAINGST_PICHER", V5="PITCHING_HAND", V6="HOME_AWAY", V7="PLAYER_ORDER",
                                                 V8="INNING", V9="INNING_BATTING_ORDER", V10="OUT_CNT", V11="DIFF_SCORE_GRP", V12="BASE_SITUATION", V13="CUM_PITCHING_CNT", V14="HIT_OUT4"))
                write.csv(temp_data, file="Test_data.csv", row.names = FALSE )
                player_data <- read.csv(file_name, header=TRUE)
                player_data_sub <- read.csv(file_name, header=TRUE)
                HOME_BATTING_ORDER <<- HOME_BATTING_ORDER + 1
                train_data <- player_data[,c(6:14)]
                HIT_OUT4 <- player_data_sub[,c(18)]
                train_data <- cbind(train_data, HIT_OUT4)
                train_data_matrix <- as.matrix(train_data[,1:9])
                dimnames(train_data_matrix) <- NULL
                train_data_mat <- cbind(train_data_matrix, train_data[,10])
                bs.training <- train_data_mat[, 1:9]
                test_data <- read.csv("Test_data.csv", header=T)
                test_data_result <- test_data
                test_data_temp <- test_data[,c(5:13)]
                HIT_OUT4 <- test_data[,c(14)]
                test_data <- cbind(test_data_temp, HIT_OUT4)
                test_data_matrix <- as.matrix(test_data[,1:9])
                dimnames(test_data_matrix) <- NULL
                test_data_mat <- cbind(test_data_matrix, test_data[,10])
                bs.test <- test_data_mat[, 1:9]
                bs.trainingtarget <- train_data_mat[, 10]
                bs.trainLabels <- to_categorical(bs.trainingtarget)
                bs.testtarget <- test_data_mat[, 10]
                #bs.testLabels <- to_categorical(bs.testtarget)
                model <- keras_model_sequential()
                test_units <- ncol(bs.trainLabels)
                model %>% 
                    layer_dense(units = 8, activation = 'relu', input_shape = c(9)) %>% 
                    layer_dropout(rate = 0.5) %>% 
                    layer_dense(units = test_units, activation = 'softmax')
                summary(model)
                model %>% compile(
                    loss = 'categorical_crossentropy',
                    optimizer = 'adam',
                    metrics = 'accuracy'
                )
                model %>% fit(
                    bs.training, 
                    bs.trainLabels, 
                    epochs = 15, 
                    batch_size = 5,
                    validation_split = 0.1
                )
                result <- model %>% predict_classes(test_data_matrix)  
                cat("Result        : ", result, "\n")
                #predict_data <- data.frame(awayteam_line_up$PLAYER_NAME[i], awayteam_line_up$BATTING_HAND[i], awayteam_line_up$HOME_AWAY[i], hometeam_line_up$PLAYER_NAME[10], hometeam_line_up$BATTING_HAND[10])
                #print(predict_data)
                DATA.HIT_OUT <<- result
                test_data_result$HIT_OUT4 <- result
                setwd("D:\\SaberMetrics\\Result")
                write.table(test_data_result, col.names = FALSE, file="Test_data_result.csv", append=TRUE)       
            }
        }
        if(HOME_BATTING_NUM >= 9) {
            HOME_BATTING_NUM <<- 1
        } else {
            HOME_BATTING_NUM <<- HOME_BATTING_NUM + 1
        }
    }
}

# Hit_function
# base_hit_function
base_hit_function <- function() {
    print("base_hit_function")
    if(BASE == 0) {
        BASE <<- 1
    } else if(BASE == 1) {
        BASE <<- 4
    } else if(BASE == 2) {
        BASE <<- 5
    } else if(BASE == 3) {
        BASE <<- 1
    } else if(BASE == 4) {
        BASE <<- 7
    } else if(BASE == 5) {
        if(HOME_AWAY == "HOME"){
            HOME_SCORE[INNING] <<- HOME_SCORE[INNING] + 1
        } else {
            AWAY_SCORE[INNING] <<- AWAY_SCORE[INNING] + 1
        }
        BASE <<- 4
    } else if(BASE == 6) {
        if(HOME_AWAY == "HOME"){
            HOME_SCORE[INNING] <<- HOME_SCORE[INNING] + 1
        } else {
            AWAY_SCORE[INNING] <<- AWAY_SCORE[INNING] + 1
        }
        BASE <<- 5
    } else if(BASE == 7) {
        if(HOME_AWAY == "HOME"){
            HOME_SCORE[INNING] <<- HOME_SCORE[INNING] + 1
        } else {
            AWAY_SCORE[INNING] <<- AWAY_SCORE[INNING] + 1
        }
        BASE <<- 7
    }
}

# two_base_hit_function
two_base_hit_function <- function() {
    print("two_base_hit_function")
    if(BASE == 0) {
        BASE <<- 2
    } else if(BASE == 1) {
        BASE <<- 6
    } else if(BASE == 2) {
        if(HOME_AWAY == "HOME"){
            HOME_SCORE[INNING] <<- HOME_SCORE[INNING] + 1
        } else {
            AWAY_SCORE[INNING] <<- AWAY_SCORE[INNING] + 1
        }
        BASE <<- 2
    } else if(BASE == 3) {
        if(HOME_AWAY == "HOME"){
            HOME_SCORE[INNING] <<- HOME_SCORE[INNING] + 1
        } else {
            AWAY_SCORE[INNING] <<- AWAY_SCORE[INNING] + 1
        }
        BASE <<- 2
    } else if(BASE == 4) {
        if(HOME_AWAY == "HOME"){
            HOME_SCORE[INNING] <<- HOME_SCORE[INNING] + 1
        } else {
            AWAY_SCORE[INNING] <<- AWAY_SCORE[INNING] + 1
        }
        BASE <<- 6
    } else if(BASE == 5) {
        if(HOME_AWAY == "HOME"){
            HOME_SCORE[INNING] <<- HOME_SCORE[INNING] + 1
        } else {
            AWAY_SCORE[INNING] <<- AWAY_SCORE[INNING] + 1
        }
        BASE <<- 6
    } else if(BASE == 6) {
        if(HOME_AWAY == "HOME"){
            HOME_SCORE[INNING] <<- HOME_SCORE[INNING] + 2
      } else {
            AWAY_SCORE[INNING] <<- AWAY_SCORE[INNING] + 2
      }
      BASE <<- 2
    } else if(BASE == 7) {
        if(HOME_AWAY == "HOME"){
            HOME_SCORE[INNING] <<- HOME_SCORE[INNING] + 2
        } else {
            AWAY_SCORE[INNING] <<- AWAY_SCORE[INNING] + 2
        }
        BASE <<- 6
    }
}

# three_base_hit_function
three_base_hit_function <- function() {
    print("three_base_hit_function")
    if(BASE == 0) {
        BASE <<- 3
    } else if(BASE == 1 || BASE == 2 || BASE == 3) {
        if(HOME_AWAY == "HOME"){
            HOME_SCORE[INNING] <<- HOME_SCORE[INNING] + 1
        } else {
            AWAY_SCORE[INNING] <<- AWAY_SCORE[INNING] + 1
        }
        BASE <<- 3
    } else if(BASE == 4 || BASE == 5 || BASE == 6) {
        if(HOME_AWAY == "HOME"){
            HOME_SCORE[INNING] <<- HOME_SCORE[INNING] + 2
        } else {
            AWAY_SCORE[INNING] <<- AWAY_SCORE[INNING] + 2
        }
        BASE <<- 3
    } else if(BASE == 7) {
        if(HOME_AWAY == "HOME"){
            HOME_SCORE[INNING] <<- HOME_SCORE[INNING] + 3
        } else {
            AWAY_SCORE[INNING] <<- AWAY_SCORE[INNING] + 3
        }
        BASE <<- 3
    }
}

#home_run_function
home_run_function <- function() { 
    print("homerunFunction")
    if(BASE == 0) {
        if(HOME_AWAY == "HOME"){
            HOME_SCORE[INNING] <<- HOME_SCORE[INNING] + 1
        } else {
            AWAY_SCORE[INNING] <<- AWAY_SCORE[INNING] + 1
        }
    } else if(BASE == 1 || BASE == 2 || BASE == 3) {
        if(HOME_AWAY == "HOME"){
            HOME_SCORE[INNING] <<- HOME_SCORE[INNING] + 2
        } else {
            AWAY_SCORE[INNING] <<- AWAY_SCORE[INNING] + 2
        }
    } else if(BASE == 4 || BASE == 5 || BASE == 6) {
        if(HOME_AWAY == "HOME"){
            HOME_SCORE[INNING] <<- HOME_SCORE[INNING] + 3
        } else {
            AWAY_SCORE[INNING] <<- AWAY_SCORE[INNING] + 3
        }
    } else if(BASE == 7) {
        if(HOME_AWAY == "HOME"){
            HOME_SCORE[INNING] <<- HOME_SCORE[INNING] + 4
        } else {
            AWAY_SCORE[INNING] <<- AWAY_SCORE[INNING] + 4
        }
    }
}

# Out_function
# single_out_function
single_out_function <- function() {
    print("OneOutFunction")
    DATA.OUT_CNT <<- DATA.OUT_CNT + 1
}

# double_out_function
double_out_function <- function() {
    print("DoubleOutFunction")
    BASE = BASE - 1
    DATA.OUT_CNT <- DATA.OUT_CNT + 2
}

# end_function
end_function <- function() {
    if(INNING == 9) {
        if(HOME_AWAY == "HOME") {
            createResultFile()
            print("게임 종료")
            cat("HOME : ",HOME_SCORE)
            cat("AWAY : ",AWAY_SCORE)
            break()
        }
    }       
    if(HOME_AWAY == "HOME") {
        INNING <<- INNING + 1
        HOME_AWAY <<- "AWAY"
        DATA.OUT_CNT <<- 0
        DATA.HIT_OUT <<- 0
        HOME_BATTING_ORDER <<- 1
        AWAY_BATTING_ORDER <<- 1
        BASE <<- 0
    } else {
        HOME_AWAY <<- "HOME"
        DATA.OUT_CNT <<- 0
        DATA.HIT_OUT <<- 0
        HOME_BATTING_ORDER <<- 1
        AWAY_BATTING_ORDER <<- 1
        BASE <<- 0
    }
}

# create_result_function
create_result_function <- function() {
    setwd("D:\\SaberMetrics\\Result")
    result_data <- read.csv("Test_data_result.csv", header = FALSE,sep = "")
    result_data <- result_data[,c(2:15)]
    result_data <- rename(result_data, c(V2="PLAYER_NAME", V3="BATTING_HAND", V4="AGAINST_TEAM", V5="AGAINGST_PICHER", V6="PITCHING_HAND", V7="HOME_AWAY", V8="PLAYER_ORDER",
                                         V9="INNING", V10="INNING_BATTING_ORDER", V11="OUT_CNT", V12="DIFF_SCORE_GRP", V13="BASE_SITUATION", V14="CUM_PITCHING_CNT", V15="HIT_OUT_PREDICT"))
    write.csv( result_data, file="Test_data_result_fianl.csv", row.names = FALSE )
}

# reset_test_function
reset_test_function <- function() {
    empty_data <- c()
    setwd("D:\\SaberMetrics\\Player")
    test_data <- read.csv("Test_data_fix.csv", header = TRUE)
    write.csv( test_data, file="Test_data.csv", row.names = FALSE )
    setwd("D:\\SaberMetrics\\Result")
    write.csv( empty_data, file="Test_data_result.csv", row.names = FALSE )
}

# test_function
test_function <- function() {
    print("Test")
}
