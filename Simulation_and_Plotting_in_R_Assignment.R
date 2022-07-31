# Potential Questions to Answer:
# 1. Create snacks that the customers can buy and randomize who buys which snack
# 2. Pretend you own multiple theaters and run two simulations to represent each theater and plot the results
# 3. Create conditional statements for movies that may be PG-13 and children are not allowed to watch

# Cost for adults and children
ticket_cost <- 14 
  ticket_cost_child <- 9
    movies <- c('movie1', 'movie2', 'movie3', 'movie4', 'movie5')  # List 5 of your favorite movies
    screens <- 5   # How many screens does the theater have? (assume 1 per movie)
      seats <- 100 # How many seats does each theater hold
      week_days <- rep(0, 7)  # Store totals for each day

###############
      #vars to use in the second graph    
      visitors_adults_in_week_days <- rep(0, 7)  # Store adults visitors totals for each day
      visitors_children_in_week_days <- rep(0, 7)  # Store children visitors totals for each day
############### 
      
    # iterate through the week
    for (day in (1:length(week_days))) {
      
      # Keep track of total revenue for the day
      day_revenue <- 0
      
###############
      #vars to use in the second graph
      #adults visitors for each day
      total_visitors_adults_a_day <- 0
      #total children visitors for each day
      total_visitors_children_a_day <- 0
###############
      
      # iterate through the amount of screens on a particular day
      for (s in (1:screens)) {
        
        # Calculate  how many adults and children are watching the movie
        visitors_adults <- sample(seats, 1)
        visitors_children <- sample((seats - visitors_adults),1)
        
        # Calculate the revenue for adults and children
        revenue_for_adults <- visitors_adults * ticket_cost
        revenue_for_children <- visitors_children * ticket_cost_child
        
        # Calculate revenue, and add to running total for the day
        
        adults_and_children_revenue <- revenue_for_adults + revenue_for_children
        
        day_revenue <- day_revenue + adults_and_children_revenue
        
        ###############
        #for the second graph, add adult visitors to its total var, do the same to the children visitors
        total_visitors_adults_a_day <- total_visitors_adults_a_day + visitors_adults
        total_visitors_children_a_day <- total_visitors_children_a_day + visitors_children
      }
      
      # Save total to the corresponding day
      week_days[day] <- day_revenue
      
###############
      #for the second graph
      visitors_adults_in_week_days[day] <- total_visitors_adults_a_day
      visitors_children_in_week_days[day] <- total_visitors_children_a_day
      
    }
      
#////////////////////////////////////////////////////////////////////////////////////////////////////////////    
# It's handy to create dataframe, it's great to understand the business as well to represent data in graph
      data <- data.frame(total_revenue_per_day = week_days,
                         adults_visitors = visitors_adults_in_week_days,
                         children_visitors = visitors_children_in_week_days,
                         adults_revenue = visitors_adults_in_week_days * ticket_cost,
                         children_revenue = visitors_children_in_week_days * ticket_cost_child)
      
#//////////////////////////////////////////////////////////////////////////////////////////////////////////// 
#############################################Graphs#########################################################
      
    # Make a barchart showing total revenue per day
      total_revenue_per_day_chart <- barplot(week_days,col=c("blue"),
                                             names=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
                                             main="Total Revenue Earned each Day for a Movie Theater",
                                             ylab="Revenue in $",
                                             xlab="Day of Week")
      
    # Make any other chart
      visitors <- t(data[c('adults_visitors','children_visitors')])
      visitors_type_per_day_chart <- barplot(visitors,
                                             beside = TRUE,
                                             col=c("blue", "orange"),
                                             names=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
                                             main="Visitors Number per Type per Day",
                                             ylab="Number of Visitors",
                                             xlab="Day of Week")
      
      # Make any other chart
      revenue <- t(data[c('adults_revenue','children_revenue')])
      revenue_per_type_per_day_chart <- barplot(revenue,
                                             beside = TRUE,
                                             col=c("blue", "orange"),
                                             names=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"),
                                             main="Revenue per Visitors Type per Day",
                                             ylab="Revenue in $",
                                             xlab="Day of Week")
      
    # Which day had the highest revenue?
      
      print(week_days)
      #3549 3175 4801 3144 4024 4342 3956
      
      # Wednesday had the highest revenue with $ 4,801
      #and Thursday had the lowest revenue with $ 3,144
      
#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      #insight on the second graph
      
      print(visitors)
      #                  [,1] [,2] [,3] [,4] [,5] [,6] [,7]
      #adults_visitors    159   95  260  123  236  260  199
      #children_visitors  147  205  129  158   80   78  130
      
      #Wednesday and Saturday had the highest adults visitors with 260 visitors
      #Tuesday had the lowest adults visitors with only 95 visitors
      
      #Tuesday had the highest children visitors with 205 visitors
      #Saturday had the lowest children visitors with only 78 visitors

#///////////////////////////////////////////////////////////////////////////////////////////////////////////////
      #insight on the second graph
      
      print(revenue)
      #                 [,1] [,2] [,3] [,4] [,5] [,6] [,7]
      #adults_revenue   2226 1330 3640 1722 3304 3640 2786
      #children_revenue 1323 1845 1161 1422  720  702 1170     
      
      #Wednesday and Saturday had the highest revenue from adults visitors with $ 3,640
      #Tuesday had the lowest revenue from adults visitors with only $ 1,330
      
      #Tuesday had the highest revenue from children visitors with $ 1,845
      #Saturday had the lowest revenue from children visitors with only $ 720
      
      #It makes a total sense according to the second graph!
      