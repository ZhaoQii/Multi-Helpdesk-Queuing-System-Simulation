### PS: All my times below set in hour not in minute
simulation_times = 2000
open_time <- 9   # open time of this clinic
end_time <- 16   # the time not admit more patients
avg_times <- rep(0, simulation_times)
close_real <- rep(0, simulation_times)
num_wait <- rep(0, simulation_times)
num_total <- rep(0, simulation_times)
num_douctors <- 3  # the number of doctors in this clinic, or generally, the number of service desks in queuing problem
set.seed(666)   # make sure to be reproducible
for (t in 1:simulation_times){
  patients <- NULL
  i <- 1 
  repeat{      ###### generate expotional distribution random values, and control the number of patients
    patients[i] <- rexp(1, 6)
    i <- i + 1
    if((9 + sum(patients)) > 16){     # no more patients later than given time
      break
    }
  }
  patients <- patients[-length(patients)]  # the arrival time of last patient exceeds 16:00
  num <- length(patients)   # num of patients today
  num_total[t] <- num
  patients_arrive_times <-  open_time + cumsum(patients)  ## the real arrive time of each patient
  patients_leave_times <- patients_arrive_times    # initialize the leave time for each patient
  doctors <- matrix(rep(0, num_douctors), ncol = num_douctors)
  doctors <- split(doctors, rep(1:ncol(doctors), each = nrow(doctors)))
  names(doctors) <- paste(rep('d', num_douctors), 1:num_douctors, sep = '')
  serving_times <- runif(num, min = (1/12), max = (1/3))  #every patient's spending time with doctor
  wait_times <- rep(0, num)   # every patient's wait time, 0 means no wait
  line <- NULL  # the current line consisting of the number of the patient
  for (j in 1:num){
    ####### For each new patient, reset the doctors' 'status'#########
    target <- names(doctors)[(sapply(doctors, '[[' , 1) == 1) & (sapply(doctors, length) > 1)] # doctors may needed to be adjusted
    for (each in target){
      if(patients_leave_times[doctors[each][[1]][length(doctors[each][[1]])]] < patients_arrive_times[j]){
        doctors[each][[1]][1] = 0                  ## if the new patient arrives later than the former one's leaving time /
      }
    }
    ######## For each new patient, there are two disjoint situations: not wait or wait #####
    #### No wait  #########
    if ((length(line) == 0) & (0 %in% sapply(doctors, '[[', 1))){  # at least one doctor is free
      target <- names(doctors)[0 == sapply(doctors, '[[', 1)]
      doctors[target[1]][[1]][1] <- 1        # set this doctor to be 'busy'
      doctors[target[1]][[1]] <- c(doctors[target[1]][[1]], j)
      patients_leave_times[j] = patients_leave_times[j] + serving_times[j]   # obtain the leave times
    } else{    ######### Need to wait ########
      line <- c(line, j)     # put the new patient on the line list
      cat('times', t, line, '\n')
      top <- line[1]        # for the first one of current line list, the time to meet doctors depends on the leave times of those who are meeting the doctors                         
      serving_patients <- sapply(doctors, tail, 1)
      start_time <- min(patients_leave_times[serving_patients])
      which_d <- which.min(patients_leave_times[serving_patients])
      wait_times[top] <- start_time - patients_arrive_times[top]   # calculate the wait
      patients_leave_times[top] <- start_time + serving_times[top]
      doctors[which_d][[1]] <- c(doctors[which_d][[1]], top)
      line <- line[-1]
    }
    if ((j == num) & (length(line) > 0)){   # in case the line still exists
      while(length(line) > 0){
        top <- line[1]        # for the first one of current line list, the time to meet doctors depends on the leave times of those who are meeting the doctors                         
        serving_patients <- sapply(doctors, tail, 1)
        start_time <- min(patients_leave_times[serving_patients])
        which_d <- which.min(patients_leave_times[serving_patients])
        wait_times[top] <- start_time - patients_arrive_times[top]   # calculate the wait
        patients_leave_times[top] <- start_time + serving_times[top]
        doctors[which_d][[1]] <- c(doctors[which_d][[1]], top)
        line <- line[-1]
      }
    }
  }
  close_real[t] = patients_leave_times[num]
  num_wait[t] = sum(wait_times != 0)
  avg_times[t] = sum(wait_times) / num_wait[t]
}
avg_times[is.nan(avg_times)] <- 0
quantile(avg_times)   # get quantiles of average waiting time, real close time, number of patients waited today and number of total patients today
quantile(close_real)
quantile(num_wait)
quantile(num_total)
