


compare_table<-function(distr_df, sim1=NA){
    space_vals<-distr_df$space
    probs<-numeric(length(distr_df$space))
    j<-1
    for (i in distr_df$space){
        probs[j]<- sum(sim1==i)/length(sim1)
        j<-j+1
    }
    # print(probs)
    new_df<-data.frame(
        space = distr_df$space,
        pmf = round(distr_df$pmf,4),
        probs = round(probs,4)
    )
    
    new_df
    
}

compare_barplot<-function(distr_df, sim1=NA){
    space_vals<-distr_df$space
    probs<-numeric(length(distr_df$space))
    j<-1
    for (i in distr_df$space){
        probs[j]<- sum(sim1==i)/length(sim1)
        j<-j+1
    }
    # print(probs)
    new_df<-data.frame(
        space = distr_df$space,
        pmf = distr_df$pmf,
        probs = probs
    )
    
    sim1[is.na(sim1)]<-0
    
    ggplot(data = new_df)+
        geom_col(aes(x = space, y = pmf),
                 fill='gray')+
        geom_col(aes(x = space, y = probs),
                 width =.15, fill='blue')+
        ylab("Probabilities")+
        scale_x_continuous(breaks = space_vals)+
        ggtitle("Comparing probabilities to\nsimulated relative frequencies")+
        theme_bw()
    
}


trial_and_error<-function(n,p, samp_dist){
    compare_barplot(
        data.frame(
            space = 0:n,
            pmf = dbinom(0:n,n,p)
        ),
        samp_dist
    )
}


group_guessing<-function(){
    samp_dist2<-sapply(1:10000, function(x){sum(sample(jelly_jar$color,50)=="green")})
    my_guess<-sum(sample(jelly_jar$color,50)=="green")
    class<-sapply(1:25, function(x){sum(sample(jelly_jar$color,50)=="green")})
    college_wide<-sapply(1:500, function(x){sum(sample(jelly_jar$color,50)=="green")})
    p_my_guess <-my_guess/50
    Xdist_my_guess<-data.frame(space = 0:50,
                               pmf = dbinom(0:50, 50, p_my_guess))
    
    p_class_guess <- sum(class)/(length(class)*50) # class guess
    Xdist_class_guess<-data.frame(space = 0:50,
                                  pmf = dbinom(0:50, 50, p_class_guess))
    
    p_EC_guess <- sum(college_wide)/(length(college_wide)*50) # class guess
    Xdist_EC_guess<-data.frame(space = 0:50,
                               pmf = dbinom(0:50, 50, p_EC_guess))
    
    p1<-compare_barplot(Xdist_my_guess, samp_dist2) + 
        labs(title=paste("Single Guess: p = ",round(p_my_guess,4))) + 
        coord_cartesian(xlim=c(0,14))
    p2<-compare_barplot(Xdist_class_guess, samp_dist2) + 
        labs(title=paste("Class Guess: p = ",round(p_class_guess,4)))+ 
        coord_cartesian(xlim=c(0,14))
    p3<-compare_barplot(Xdist_EC_guess, samp_dist2) + 
        labs(title=paste("College-wide Guess: p = ",round(p_EC_guess,4)))+ 
        coord_cartesian(xlim=c(0,14))
    
    (p1+p2)/p3
}

middle_region<-function(samp_dist){
    qs<-quantile(samp_dist, c(0.025, 0.975))
    ggplot()+
        geom_rect(aes(xmin=qs[1], 
                      xmax=qs[2], 
                      ymin=0, 
                      ymax=Inf),
                  col='blue',
                  fill='dodgerblue',
                  alpha=.4)+
        geom_histogram(aes(x = samp_dist),
                       bins = 32,
                       color="white",alpha=.5)+
        annotate("text", x= qs[1]-.01,y=500,label=round(qs[1],3),
                 color='blue')+
        annotate("text", x= qs[2]+.01,y=500,label=round(qs[2],3),
                 color='blue')+
        theme_bw() 
}


# Could use to hide the population, but makes the simulated sampling
# take too long

# get_sample<-function(n){
#     temp<-read.csv("data/lions_and_tigers.csv",header = T, stringsAsFactors = T)
#     s1<-sample(temp$color,n)
#     rm(temp)
#     return(s1)
# }



# used to generate the jelly jar

# generate_color<-function(x){
#     rand<-runif(1)
#     if (rand<0.13){
#         return("yellow")
#     } else if (rand<0.221){
#         return("green")
#     } else if (rand<0.331){
#         return("red")
#     } else if (rand<0.452){
#         return("blue")
#     } else if (rand<0.602){
#         return("pink")
#     } else if (rand<0.823){
#         return("orange")
#     } else if (rand<0.9055){
#         return("white")
#     } else {
#         return("black")
#     }
# }