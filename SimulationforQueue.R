### PS: All my times below set in hour not in minute
simulation_times = 2000   # for getting a more precise number I simulate this process for 2000 times
open_time <- 9   # open time of this clinic
end_time <- 16   # the time not admit more patients
avg_times <- rep(0, simulation_times)
close_real <- rep(0, simulation_times)
num_wait <- rep(0, simulation_times)
num_total <- rep(0, simulation_times)
set.seed(5224)
for (t in 1:simulation_times){
  patients <- NULL
  i <- 1 
  repeat{      ###### generate expotional distribution random values, and control the number of patients
    patients[i] <- rexp(1, 6)
    i <- i + 1
    if((9 + sum(patients)) > 16){
      break
    }
  }
  patients <- patients[-length(patients)]
  num <- length(patients)   # num of patients today
  num_total[t] <- num
  patients_arrive_times <-  open_time + cumsum(patients)  ## the real arrive time of each patient
  patients_leave_times <- patients_arrive_times    # initialize the leave time for each patient
  d1 <- list('status' = 0, 'patient' = numeric())    # d1 to d3 for three doctors, where status = 1 /
  d2 <- list('status' = 0, 'patient' = numeric())    # means busy, and 0 means free
  d3 <- list('status' = 0, 'patient' = numeric())
  serving_times <- runif(num, min = (1/12), max = (1/3))  #every patient's spending time with doctor
  wait_times <- rep(0, num)   # every patient's wait time, 0 means no wait
  line <- NULL  # the current line consisting of the number of the patient
  for (j in 1:num){
  ####### For each new patient, reset the doctors' 'status'#########
    if ((length(d1$patient) > 0) & (d1$status == 1)){
      if(patients_leave_times[d1$patient[length(d1$patient)]] < patients_arrive_times[j]){
        d1$status = 0                  ## if the new patient arrives later than the former one's leaving time /
      }                               ##  that doctor is free
    }
    if ((length(d2$patient) > 0) & (d2$status == 1)){
      if(patients_leave_times[d2$patient[length(d2$patient)]] < patients_arrive_times[j]){
        d2$status = 0
      }
    }
    if ((length(d3$patient) > 0) & (d3$status == 1)){
      if(patients_leave_times[d3$patient[length(d3$patient)]] < patients_arrive_times[j]){
        d3$status = 0
      }
    }
  ######## For each new patient, there are two disjoint situations: not wait or wait #####
  #### No wait  #########
    if ((length(line) == 0) & (0 %in% c(d1['status'], d2['status'], d3['status']))){  # at least one doctor is free
      if ((length(line) == 0) & (d1$status == 0)){
        d1$patient = c(d1$patient, j)             # d1$patient maintains the name lists of patients met by doctor NO.1
        patients_leave_times[j] = patients_leave_times[j] + serving_times[j]   # obtain the leave times
        d1$status = 1      # set this doctor to be 'busy'
      }
      else if ((length(line) == 0) & (d2$status == 0)){
        d2$patient = c(d2$patient, j)
        patients_leave_times[j] = patients_leave_times[j] + serving_times[j]
        d2$status = 1
        }
      else if ((length(line) == 0) & (d3$status == 0)){
        d3$patient = c(d3$patient, j)
        patients_leave_times[j] = patients_leave_times[j] + serving_times[j]
        d3$status = 1
        }
      } else{    ######### Need to wait ########
      line <- c(line, j)     # put the new patient on the line list
      top <- line[1]        # for the first one of current line list, the time to meet doctors depends on the leave times of those who are meeting the doctors                         
      serving_patients <- c(d1$patient[length(d1$patient)], d2$patient[length(d2$patient)], d3$patient[length(d3$patient)])
      start_time <- min(patients_leave_times[serving_patients])
      which_d <- which.min(patients_leave_times[serving_patients])
      wait_times[top] <- start_time - patients_arrive_times[top]   # calculate the wait
      patients_leave_times[top] <- start_time + serving_times[top]
      switch(which_d,       # find which doctor to be met by the first patient in line
             '1' = {
               d1$patient = c(d1$patient, top)
             },
             '2' = {
               d2$patient = c(d2$patient, top)
             },
             '3' = {
               d3$patient = c(d3$patient, top)
             }
      )
      line <- line[-1]
      }
    if ((j == num) & (length(line) > 0)){   # in case the line still exists
      while(length(line) > 0){
        top <- line[1]        # for the first one of current line list, the time to meet doctors depends on the leave times of those who are meeting the doctors                         
        serving_patients <- c(d1$patient[length(d1$patient)], d2$patient[length(d2$patient)], d3$patient[length(d3$patient)])
        start_time <- min(patients_leave_times[serving_patients])
        which_d <- which.min(patients_leave_times[serving_patients])
        wait_times[top] <- start_time - patients_arrive_times[top]   # calculate the wait
        patients_leave_times[top] <- start_time + serving_times[top]
        switch(which_d,       # find which doctor to be met by the first patient in line
               '1' = {
                 d1$patient = c(d1$patient, top)
               },
               '2' = {
                 d2$patient = c(d2$patient, top)
               },
               '3' = {
                 d3$patient = c(d3$patient, top)
               }
        )
        line <- line[-1]
      }
    }
  }
  close_real[t] = patients_leave_times[num]
  num_wait[t] = sum(wait_times != 0)
  avg_times[t] = sum(wait_times) / num_wait[t]
}
avg_times[is.nan(avg_times)] <- 0
quantile(avg_times)
quantile(close_real)
quantile(num_total)
quantile(num_wait)