count_event_upto <- function(event_vec, event_to_count, upto_event) {
  if(upto_event %in% event_vec) {
    upto_event_index <- which(event_vec == upto_event)
    event_vec <- event_vec[1:(upto_event_index-1)]
  }
  return(sum(event_vec==event_to_count))
}

converted_after <- function(event_vec, base_event, convert_event) {
  if(base_event %in% event_vec) {
    event_vec <- event_vec[which(event_vec==base_event):length(event_vec)]
    return(convert_event %in% event_vec)
  } else {
    return(NA)
  }
}

get_paid_period <- function(data) {
  result <- data.table(uid=data$uid[1], date=strftime(data$date_time[1], "%Y-%m-%d"), paid=0, group='a')
  for(user in unique(data$uid)) {
    user_data <- data[uid==user]
    start_date <- user_data[event=='subscribed', date_time]
    if('unsubscribed' %in% user_data$event) {
      end_date <- user_data[event=='unsubscribed', date_time]
    } else {
      end_date <- max(data$date_time)
    }
    diff_time <- end_date - start_date
    if(user_data$group[1] == 'a') {
      weeks <- ceiling(as.numeric(diff_time, units="weeks"))
      days <- weeks*7
      result <- rbind(result,
                      data.table(uid=user,
                                 date=strftime(start_date, "%Y-%m-%d"),
                                 paid=5, 
                                 group='a'))
      for(i in 1:(days-1)) {
        d <- strftime((start_date + (3600*24*i)), "%Y-%m-%d")
        if(i %% 7 == 0) {
          p <- 5
        } else {
          p <- 0
        }
        result <- rbind(result,
                        data.table(uid=user,
                                   date=d,
                                   paid=p, 
                                   group='a'))
      }
    } else {
      days <- ceiling(as.numeric(diff_time, units="days"))
      result <- rbind(result,
                      data.table(uid=user,
                                 date=strftime(start_date, "%Y-%m-%d"),
                                 paid=0.80, 
                                 group='c'))
      for(i in 1:(days-1)) {
        d <- strftime((start_date + (3600*24*i)), "%Y-%m-%d")
        p <- 0.80
        result <- rbind(result,
                        data.table(uid=user,
                                   date=d,
                                   paid=p, 
                                   group='c'))
      }
    }
  }
  result <- result[2:nrow(result)]
  return(result)
}