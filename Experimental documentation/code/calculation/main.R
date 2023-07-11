library("randomForest")
library("naivebayes")
library("ROSE")
library("smotefamily")
  
source("code/packages/measures.R")
source("code/packages/imbalance.R")
DIR_PATH = ""
setwd(DIR_PATH)
# This path is used to store result data
root_path <- "data_results/"
# set month period 
gap <- 2
# set whether to use parameter optimization
optimize_rf = FALSE 
collinearity_fn <- paste(root_path, "collinearity.csv", sep="")
collinearity_features <- read.csv(collinearity_fn, check.names = FALSE)
szz_labels <- c("buggy_B2", "buggy_AG", "buggy_MA", "buggy_RA")
szz_baseline <- "buggy_RA"

projects <- c('AntennaPod', 'AnySoftKeyboard', 'atmosphere', 'Conversations', 
              'kiwix-android','owncloud-android','AndroidAPS', 'k-9','SeriesGuide')
			  

level <- c("buggy", "clean")
classifiers <- c("naive_bayes", "logistic_regression", "random_forest")

study_methods <- c("imbalance", "balance")

sample_methods <- c("under", "over", "rose", "smote")

calculated_measures <- c("auc","mcc", "Pf", "precision", "recall", "f1measure",
                         "gmean", "recall20", "f20", "IFA", "fp", "fn", "waste_effort", "all_effort")

store_result_to_frame<-function(result_frame, scores_vector){
  temp_frame <- data.frame(scores_vector)
  if (is.null(result_frame)){
    result_frame <- temp_frame
  }
  else {
    result_frame <- cbind(result_frame, temp_frame)
  }
  return(result_frame)
}

for (sample in sample_methods) {
  
  for (method in study_methods){
    print(method)
    for (classifier in classifiers[3:3]){
      for (p in projects){
        # result preparation
        
        importance_rank_frame <- NULL
        
        filter_features <- as.vector(collinearity_features[p][,1])
        filter_features <- append(filter_features, szz_labels)
        
        fn <- paste(c(root_path, "data_csv_time_ln/", p, ".csv"), collapse="")
        fn2 <- paste(c(root_path, "data_csv_time/", p, ".csv"), collapse = "")
        print(paste("filename: ", fn, sep=""))
        data <- read.csv(fn)
        raw_data <- read.csv(fn2)
        raw_data$lt <- raw_data$lt * raw_data$nf
        raw_data$nuc <- raw_data$nuc * raw_data$nf
        
        var_names <- names(data)
        metrics <- var_names[!var_names %in% szz_labels]
        metrics <- metrics[!metrics %in% c("la", "ld", "commit_id", "is_merge","commit_date")]
        
        var_names1 <- var_names[!var_names %in% filter_features]
        var_names_form <- var_names1[!var_names1 %in% "commit_date"]
        var_names_str <- paste(var_names_form, collapse="+")
        print(var_names_str)
        print(szz_labels)
        
        raw_data$commit_date <- strftime(raw_data$commit_date, format="%Y/%m")
        ###  Characters are converted to date format, and only the year / month is reserved for the date
        raw_data <- raw_data[order(raw_data$commit_date), ]
		##filter unclassifiable folds
        ##filter begin 
        fold_filter <- c()
        for (szz_label in szz_labels){
          cat("\n", "filter:",szz_label,"\n")
          form <- as.formula(paste(szz_label, var_names_str, sep=" ~ "))
          var_names2 <- append(var_names1, szz_label)
          var_names2 <- append(var_names2, szz_baseline)
          temp_data <- data[var_names2]
          temp_data$real_la <- raw_data$la
          temp_data$real_ld <- raw_data$ld
          
          # factorise labels
          buggy_labels <- factor(temp_data[szz_label][,1], order=TRUE, levels=c("clean", "buggy"))
          temp_data[szz_label][,1] <- buggy_labels
          buggy_real_labels <- factor(temp_data[szz_baseline][,1], order=TRUE, levels=c("clean", "buggy"))
          temp_data[szz_baseline][,1] <- buggy_real_labels
          
          ##time-order
          cat("(BEG)Timewise cross-validation for", p, "\n")
          temp_data$commit_date <- strftime(temp_data$commit_date, format="%Y/%m")
          ###  Characters are converted to date format, and only the year / month is reserved for the date
          temp_data <- temp_data[order(temp_data$commit_date), ]
          ### Sort data by commitTime date
          unimon <- unique(temp_data$commit_date)
          ###  Using unimon to save the number of different commitTime 
          unimon <- unimon[order(unimon)]
          ###  Sort unimon
          totalFolds <- length(unimon)#group by date  
          ###totalFolds，The number of divisions depends on the number of different dates
          sub <- NULL 
          ### dive data into totalFolds parts, each part corresponding to changes within one month
          for (fold in seq(totalFolds)) {
            sub[[fold]] <- temp_data[which(temp_data$commit_date==unimon[fold]), ]
          }
          
          cat("\n", p, "has", totalFolds, "folds, length of each fold:\n", unlist(lapply(sub, nrow)), "\n")
          for (fold in seq(totalFolds)) {
            if (fold %in% fold_filter){next}
            if (gap == 2) {
              if (fold+5 > totalFolds) { next }
			  ###fit--Data of the first and second groups
			  ###est--Data of the forth and fifth groups
              train_data <- rbind(sub[[fold]], sub[[fold+1]])
              test_data <- rbind(sub[[fold+2+gap]], sub[[fold+3+gap]])
            }
            if (gap == 6) {
              if (fold+13 > totalFolds) { next }
              train_data <- rbind(sub[[fold]], sub[[fold+5]])
              test_data <- rbind(sub[[fold+2+gap]], sub[[fold+2+gap+5]])
            }
            
            train_data <- subset(train_data,select = -commit_date)
            test_data <- subset(test_data,select = -commit_date)
            
            if (!(level[1] %in% train_data[[szz_label]]) | !(level[2] %in% train_data[[szz_label]]) | sum(train_data[[szz_label]] == "buggy") < 2){
              cat("\n", p, "has filter fold:", fold, "\n")
              fold_filter <- append(fold_filter, fold)
              next
            }
            if (!(level[1] %in% test_data[[szz_baseline]]) | !(level[2] %in% test_data[[szz_baseline]])){
              cat("\n", p, "has filter fold:", fold, "\n")
              fold_filter <- append(fold_filter, fold)
              next
            }
          }
        }
        ## filter end
        for (szz_label in szz_labels){
          fold_count <- 1
          print(szz_label)
          result_frame <- NULL
          form <- as.formula(paste(szz_label, var_names_str, sep=" ~ "))
          var_names2 <- append(var_names1, szz_label)
          var_names2 <- append(var_names2, szz_baseline)
          temp_data <- data[var_names2]
          temp_data$real_la <- raw_data$la
          temp_data$real_ld <- raw_data$ld
          auc_scores <- c()
          mcc_scores <- c()
          Pf_scores <- c()
          precision_scores <- c()
          recall_scores <- c()
          F1_scores <- c()
          recall20_scores <- c()
          f20_scores <- c()
          IFA_scores <- c()
          gmean_scores <- c()
          fp_scores <- c()
          fn_scores <- c()
          waste_lines_scores <- c()
          all_lines_scores <- c()
          
          importance_matrix <- NULL
          
          # factorise labels
          buggy_labels <- factor(temp_data[szz_label][,1], order=TRUE, levels=c("clean", "buggy"))
          temp_data[szz_label][,1] <- buggy_labels
          buggy_real_labels <- factor(temp_data[szz_baseline][,1], order=TRUE, levels=c("clean", "buggy"))
          temp_data[szz_baseline][,1] <- buggy_real_labels
          ##time-order
          ##begin
          cat("(BEG)Timewise cross-validation for", p, "\n")
          temp_data$commit_date <- strftime(temp_data$commit_date, format="%Y/%m")
          
          ###  Characters are converted to date format, and only the year / month is reserved for the date
          temp_data <- temp_data[order(temp_data$commit_date), ]
          
          ### Sort data by commitTime date
          unimon <- unique(temp_data$commit_date)
          
          ###  Using unimon to save the number of different commitTime 
          unimon <- unimon[order(unimon)]
          ###  Sort unimon
          totalFolds <- length(unimon)#group by date  eg:1998/08
          ###totalFolds，The number of divisions depends on the number of different dates
          sub <- NULL 
          ### dive data into totalFolds parts, each part corresponding to changes within one month
          for (fold in seq(totalFolds)) {
            sub[[fold]] <- temp_data[which(temp_data$commit_date==unimon[fold]), ]
          }
          
          cat("\n", p, "has", totalFolds, "folds, length of each fold:\n", unlist(lapply(sub, nrow)), "\n")
          for (fold in seq(totalFolds)) {
            print(fold)
            
            if (fold %in% fold_filter) {
              cat("\n", p, "has skiped fold:", fold,"\n")
              next
            }
            if (gap == 2) {
              if (fold+5 > totalFolds) { next }
              # if (fold+13 > totalFolds) { next }
              train_data <- rbind(sub[[fold]], sub[[fold+1]])
              test_data <- rbind(sub[[fold+2+gap]], sub[[fold+3+gap]])
            }
            if (gap == 6) {
              if (fold+13 > totalFolds) { next }
              train_data <- rbind(sub[[fold]], sub[[fold+5]])
              test_data <- rbind(sub[[fold+2+gap]], sub[[fold+2+gap+5]])
            }
            ###fit--Data of the first and second groups
            ###est--Data of the forth and fifth groups
            train_data <- subset(train_data,select = -commit_date)
            test_data <- subset(test_data,select = -commit_date)
            if (method == "balance"){
              if (table(train_data[[szz_label]])[1] != table(train_data[[szz_label]])[2])
              {
                if (sample == "over")
                {
                  #Oversampling
                  train_data[[szz_label]] <- factor(train_data[[szz_label]],order = FALSE,levels=c("clean", "buggy"))
                  train_data[[length(train_data)-2]] <- factor(train_data[[length(train_data)-2]],order = FALSE ,levels=c("clean", "buggy"))
                  #train_data <- train_data[,-c(length(train_data) - 2)]
                  train_data$real_la <- as.numeric(train_data$real_la)
                  train_data$real_ld <- as.numeric(train_data$real_ld)
                  f = as.formula(paste(szz_label, "~.", sep = ""))
                  train_data <- ovun.sample(f, train_data, method = "over",seed=1, )$data
                }
                if (sample == "under")
                { # Undersampling
                  set.seed(1)
                  train_data <- undersampling(train_data, szz_label)
                }
                if (sample == "rose")
                {
                  # Rose
                  ##Ԥ????
                  train_data[[szz_label]] <- factor(train_data[[szz_label]],order = FALSE,levels=c("clean", "buggy"))
                  train_data[[length(train_data)-2]] <- factor(train_data[[length(train_data)-2]],order = FALSE ,levels=c("clean", "buggy"))
                  #train_data <- train_data[,-c(length(train_data) - 2)]
                  train_data$real_la <- as.numeric(train_data$real_la)
                  train_data$real_ld <- as.numeric(train_data$real_ld)
                  f = as.formula(paste(szz_label, "~.", sep = ""))
                  print(f)
                  train_data <- ovun.sample(f, train_data, method = "both",seed=1)$data
                  
                }
                if (sample == "smote")
                {
                  # # SMOTE
                  
                  train_data[[szz_label]] = as.character(train_data[[szz_label]])
                  train_data[[length(train_data)-2]] = as.character(train_data[[length(train_data)-2]])
                  train_data[[szz_label]][which(train_data[[szz_label]] == "clean")] <- 0
                  train_data[[szz_label]][which(train_data[[szz_label]] == "buggy")] <- 1
                  train_data[[length(train_data)-2]][which(train_data[[length(train_data)-2]] == "clean")] <- 0
                  train_data[[length(train_data)-2]][which(train_data[[length(train_data)-2]] == "buggy")] <- 1
                  train_data[[szz_label]] = as.numeric(train_data[[szz_label]])
                  train_data[[length(train_data)-2]] = as.numeric(train_data[[length(train_data)-2]])
                  ##smote
                  train_data <- smotefamily::SMOTE(train_data[,-(length(train_data)-3)], train_data[,length(train_data)-3],K = 1)$data
                  names(train_data)[names(train_data) =="class"] <- szz_label
                  train_data[[szz_label]][which(train_data[[szz_label]] == 0)] <- "clean"
                  train_data[[szz_label]][which(train_data[[szz_label]] == 1)] <- "buggy"
                  train_data[[length(train_data)-3]][which(train_data[[length(train_data)-3]] == 0)] <- "clean"
                  train_data[[length(train_data)-3]][which(train_data[[length(train_data)-3]] == 1)] <- "buggy"                  
                  train_data[[szz_label]] <- factor(train_data[[szz_label]],order = TRUE,levels=c("clean", "buggy"))
                  train_data[[length(train_data)-3]] <- factor(train_data[[length(train_data)-3]],order = TRUE ,levels=c("clean", "buggy"))
                  train_data$real_la <- as.integer(train_data$real_la)
                  train_data$real_ld <- as.integer(train_data$real_ld)
          
                }
                }
              }
            }
            ##save every fold train_data
            fn_folds <- ""
            if (method != "balance") {
              fn_folds <- paste(c(root_path,"time_order_results/", "results_imbalance", "/", szz_label, "_folds/",p,"_folds/", fold_count, ".csv"), collapse="")
              
            }
            else{
              if(sample == "smote"){
                fn_folds <- paste(c(root_path,"time_order_results/", "results_balance", "/smote_folds/", szz_label, "_folds/",p,"_folds/", fold_count, ".csv"), collapse="")
              }
            }
            write.csv(train_data, fn_folds, row.names = FALSE)
            fold_count = fold_count + 1
            # calculate the likelihood scores being "buggy" for changes in testing set
            if (classifier == "random_forest"){
              set.seed(666)
              if (optimize_rf) {
                rf <- rf_optimal(form, train_data, valid_data, szz_baseline)
                mtry <- append(mtry, rf$best_mtry)
                ntree <- append(ntree, rf$best_ntree)
                prediction <- predict(rf$best_fit, test_data, type="prob")
                prob <- prediction[,2]
              }
              else{
                fit <- randomForest(form, train_data, ntree=100)
                prediction <- predict(fit, test_data, type="prob")
                prob <- prediction[,2]
              }
            }

            if (classifier == "logistic_regression"){
              fit <- glm(form, train_data, family=binomial)
              prediction <- predict(fit, test_data, type="response")
              prob <- prediction
            }

            if (classifier == "naive_bayes"){

              fit <- naive_bayes(form, train_data)
              prediction <- predict(fit, test_data, type="prob")
              prob <- prediction[,2]
            }
            # calculate auc
            result <- roc(test_data[szz_baseline][,1], prob)
            auc_scores <- append(auc_scores, result["auc"][[1]][1])
            
            # calculate mcc
            mcc_score <- mcc(test_data, prob, szz_baseline)
            mcc_scores <- append(mcc_scores, mcc_score)
            
            # calculate pf
            Pf_score <- Pf(test_data, prob, szz_baseline)
            Pf_scores <- append(Pf_scores, Pf_score)
            
            # calculate precision
            precision_score <- precision(test_data, prob, szz_baseline)
            precision_scores <- append(precision_scores, precision_score)

            # calculate recall
            recall_score <- recall(test_data, prob, szz_baseline)
            recall_scores <- append(recall_scores, recall_score)

            # calculate f1
            f_score <- F1(test_data, prob, szz_baseline)
            F1_scores <- append(F1_scores, f_score)

            # calculate gomeric mean
            gmean_score <- gmean(test_data, prob, szz_baseline)
            gmean_scores <- append(gmean_scores, gmean_score)

            # calculate cost effectiveness measure
            ordered_data <- get_ordered_data(test_data, prob) ##cbs

            LOC = ordered_data$la + ordered_data$ld

            PRE <- ordered_data[[szz_label]]
            PRE <- as.character(PRE)
            PRE[which(PRE == "clean")] <- 0
            PRE[which(PRE == "buggy")] <- 1
            PRE <- as.numeric(PRE)
            NUM <- ordered_data[[szz_baseline]]
            NUM <- as.character(NUM)
            NUM[which(NUM == "clean")] <- 0
            NUM[which(NUM == "buggy")] <- 1
            NUM <- as.numeric(NUM)
            #popt_value <- Popt2(ordered_data, total_churn2, "la", "ld", label, "buggy")
            tmp_data <- data.frame(NUM, LOC, PRE)
            tmp_data[["density"]] <- tmp_data$NUM/(tmp_data$LOC+1)
            cbs_measure <- ComputeACC(data = tmp_data, sorted = TRUE)
            #NOM <- cbs_measure$NOM
            IFA <- cbs_measure$IFA
            IFA_scores <- append(IFA_scores, IFA)
            total_churn <- sum(test_data$real_la+test_data$real_ld)

            results <- calculate_cost_effectiveness2(ordered_data, total_churn, 0.2, "real_la", "real_ld", szz_baseline, "buggy")
            recall20 <- results[2]
            precision20 <- results[1]
            F1_score20 <- 2 * precision20 * recall20 / (precision20 + recall20)
            recall20_scores <- append(recall20_scores, recall20)
            f20_scores <- append(f20_scores, F1_score20)
            # calculate wastes and misses
            waste_miss_results <- waste_miss(test_data, prob, szz_baseline)
            fp_scores <- append(fp_scores, waste_miss_results[1])
            fn_scores <- append(fn_scores, waste_miss_results[2])
            waste_lines_scores <- append(waste_lines_scores, waste_miss_results[3])
            all_lines_scores <- append(all_lines_scores, waste_miss_results[4])
            #fit$commit_date <- est$commit_date <- NULL
            
          }
          ##end
          
          # store auc results
          result_frame <- store_result_to_frame(result_frame, auc_scores)

          # store mcc results
          result_frame <- store_result_to_frame(result_frame, mcc_scores)

          # store Pf results
          result_frame <- store_result_to_frame(result_frame, Pf_scores)

          # store precision
          result_frame <- store_result_to_frame(result_frame, precision_scores)

          # store recall
          result_frame <- store_result_to_frame(result_frame, recall_scores)

          # store F1
          result_frame <- store_result_to_frame(result_frame, F1_scores)

          # store gmean
          result_frame <- store_result_to_frame(result_frame, gmean_scores)

          # store recall20
          result_frame <- store_result_to_frame(result_frame, recall20_scores)
          #store f20
          result_frame <- store_result_to_frame(result_frame, f20_scores)
          #store IFA
          result_frame <- store_result_to_frame(result_frame, IFA_scores)
          # store false positive, false negative, waste effort and overall effort
          result_frame <- store_result_to_frame(result_frame, fp_scores)
          result_frame <- store_result_to_frame(result_frame, fn_scores)
          result_frame <- store_result_to_frame(result_frame, waste_lines_scores)
          result_frame <- store_result_to_frame(result_frame, all_lines_scores)

          names(result_frame) <- calculated_measures
          if (method == "imbalance"){
            result_fn <- paste(c(root_path,"time_order_results/", "results_", method, "/", p, "_", classifier, "_", szz_label, ".csv"), collapse="")
          }
          else{
            result_fn <- paste(c(root_path,"time_order_results/", "results_", method, "/", sample, "/", p, "_", classifier, "_", szz_label, ".csv"), collapse="")
          }
          write.csv(result_frame, result_fn, row.names=FALSE)
          
        }
      }
    }
  }
}
