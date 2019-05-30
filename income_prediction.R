




in_groups = c(5:14, 16:17)
in_names = c("IN_P_05", "IN_P1020", "IN_P2030", "IN_P3040", "IN_P4050", "IN_P5060", 
             "IN_P6070", "IN_P7080", "IN_P8090", "IN_P90100", "IN_P100150", "IN_P150_")


groups = list(age_gender_groups, ethno_groups, relig_groups, imm_groups, ed_groups, field_groups, lf_groups,
              ms_groups, hh_groups, dw_groups, in_groups)
names = list(age_gender_names, ethno_names, relig_names, imm_names, ed_names, field_names, lf_names,
             ms_names, hh_names, dw_names, in_names)

path = "C:/Users/Able/Documents/winstonli1513/data/"
repo_path = 'C:/Users/Able/Documents/winstonli1513/arima2/'


IN1 = read_csv(paste(path, 'IN1.csv', sep = ''))
IN1[,19]


train_model = function(core, target, input, postal, var_name){
  supp = read_csv(paste(path, 'supp.csv', sep = ''))
  data = cbind(supp[, 2:4], core)
  ind = sample(1:nrow(data), round(0.3*nrow(data)))
  target[target > 1] = 1
  train_x = as.matrix(data[-ind, ])
  train_y = as.matrix(target[-ind, ])
  val_x = as.matrix(data[ind, ])
  val_y = as.matrix(target[ind, ])
  model <- keras_model_sequential()
  if(ncol(target) > 1){
    model %>%
      layer_dense(units = max(powerTwo(ncol(train_x))/1, 4), 
                  input_shape = ncol(train_x), activation = 'relu') %>%
      layer_dense(units = max(powerTwo(ncol(train_x))/2, 2), activation = 'relu') %>%
      layer_dense(units = ncol(train_y), activation = 'linear') %>%
      compile(
        optimizer = 'adam',
        loss = 'categorical_crossentropy',
        metrics = list('accuracy')
      )
  } else{
    model %>%
      layer_dense(units = max(powerTwo(ncol(train_x))/1, 4), 
                  input_shape = ncol(train_x), activation = 'relu') %>%
      layer_dense(units = max(powerTwo(ncol(train_x))/2, 2), activation = 'relu') %>%
      layer_dense(units = ncol(train_y), activation = 'linear') %>%
      compile(
        optimizer = 'adam',
        loss = 'mean_squared_error',
        metrics = list('mean_absolute_percentage_error')
      )    
  }
  model %>% fit(
    train_x,
    train_y,
    epochs = 10,
    batch_size = 128,
    validation_data = list(val_x, val_y)
  )
  if(!missing(var_name))
    model %>% save_model_hdf5(paste(repo_path, "models/", var_name, ".h5", sep = ''))
}

predict_from_model = function(input, var_name){
  model = load_model_hdf5(paste(repo_path, "models/", var_name, ".h5", sep = ''))
  test_y = model %>% predict(as.matrix(input), batch_size = 32)
  return(test_y)
}


process_file = function(file_path, var_name, postal, index, excl_age = c("PP_FM0_14", "PP_M0_14"),
                        alternative_name, ref_col = 2, write = TRUE){
  if(!missing(file_path)){
    raw = read_csv(file_path)
    core = read_csv(paste(repo_path, 'data/core.csv', sep = ''))
  }
  
  
  if(ref_col == 0)
    target = raw[, groups[[11]]]
  else
    target = raw[, groups[[11]]]/unlist(raw[, ref_col])
  
  
  if(!missing(alternative_name) & !all(rowSums(target) == 1)){
    print('Additional variable added')
    target[, alternative_name] = 1 - rowSums(target)
  }
  
  
  
  individual = read_csv(paste(repo_path, 'data/individual.csv', sep = ''))
  input = c()
  for(i in 7:ncol(individual)){
    input = cbind(input, toDummy(individual[, colnames(individual)[i]], names[[i-6]]))
  }
  input = cbind(individual[, 4:6], input)
  print('Done file reading')
  
  
  
  
  if(!file.exists(paste(repo_path, 'models/', var_name, '.h5', sep = ''))){
    train_model(core, target, input, postal, var_name)
  }
  output = predict_from_model(input, var_name)
  print('Done prediction')
  
  
  
  individual$matched = ''
  for(pos in postal){
    print(paste("Matching for", pos))
    marginals = target[raw$PostCode == pos,]
    individual[individual$PostCode == pos & !(individual$Demographics %in% excl_age), ncol(individual)] = 
      marginals
    
  }
  colnames(individual)[ncol(individual)] = var_name
  print('Done Matching')
  
  
  if(write){
    write_csv(individual, paste(repo_path, 'data/individual.csv', sep = ''))
    write_csv(cbind(core, target), paste(repo_path, 'data/core.csv', sep = ''))
  } else{
    return(pull(individual, var_name))
  }
}

process_file(file_path = paste(path, 'IN1.csv', sep = ''), var_name = 'Income', 
             postal = postal, index = groups[[11]])


individual = read_csv(paste(repo_path, 'data/individual.csv', sep = ''))




pos = 'M6S3H8'
individual = read_csv(paste(repo_path, 'data/individual.csv', sep = ''))

  average = round(unlist(IN1[IN1$PostCode == pos, 19]))
  
  total = unlist(average*IN1[IN1$PostCode == pos, "IN_W15_"])
  
  c = unlist(IN1[IN1$PostCode == pos, "IN_W15_"])
  
  marginals = IN1[IN1$PostCode == pos, groups[[11]]]
  
  marginals = unlist(marginals)
  
  median = round(unlist(IN1[IN1$PostCode == pos, 18]))
  
 

counts = c(marginals)

mid_values = c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 125, 175)
data = data.frame(cbind(mid_values, counts))
n = sum(marginals)
mu = sum(mid_values * counts) / n

sigma2 = (sum(mid_values^2 * counts) - n * mu^2) / (n-1) 

sigma = sqrt(sigma2)










  average = round(unlist(IN1[IN1$PostCode == pos, 19]))
  
  total = unlist(average*IN1[IN1$PostCode == pos, "IN_W15_"])
  
  c = unlist(IN1[IN1$PostCode == pos, "IN_W15_"])
  
  marginals = IN1[IN1$PostCode == pos, groups[[11]]]
  marginals = round_marginal(marginals, c)
  
  
  samples = untable(marginals)
  samples = data.frame(samples)
  
  IM = c()
  for(j in 1:nrow(samples)){
    demo = as.character(samples[j,1])
    
    name = strsplit(demo, 'P')[[1]][2]
    
    if (name == "_05"){
      min = 1
      max = 10}
    else if (name == "150_"){
      min = 150
      max = 170}
    else if (nchar(name) == 4){
      min = as.numeric(substr(name,0,2))
      max = as.numeric(substr(name,3,4))
    }
    else if (nchar(name) == 5){
      min = as.numeric(substr(name,0,2))
      max = as.numeric(substr(name,3,5))
    }  
    else if (nchar(name) == 6){
      min = as.numeric(substr(name,0,3))
      max = as.numeric(substr(name,4,6))
    }
    
    samples$IM[j] = round(runif(1, min, max)) *1000
  }
  
  samples$IM = round_marginal(samples$IM, total)
  samples$IM = as.numeric(samples$IM)
  colnames(samples) = c("class", "IM")
  
  samples = data.frame(samples)
  
  individual$average_income[individual$PostCode == pos & (individual$Income %in% names[[11]])] = sample(samples)
}


pos = 'L5M6V9'
write_csv(samples, paste(repo_path, 'data/samples_L5M6V9.csv', sep = ''))
