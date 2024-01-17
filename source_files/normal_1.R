

# Visual for the normal distribution
norm_vis<-function(a, b = NA, mu, sigma, tail = "lower"){
    xvals<-seq(mu -3.5*sigma, 
               mu +3.5*sigma,
               length.out = 1000)
    y_top <- max(dnorm(xvals,mu,sigma))
    df<-data.frame(x = xvals,
                   y= dnorm(xvals,mu,sigma))
    if(tail=="upper"){
        ggplot(data = df,
               aes(x = x, y=y))+
            geom_hline(yintercept = 0)+
            geom_ribbon(data = subset(df,x>a),
                        aes(x=x,ymax =y),ymin =0,
                        color='blue',
                        alpha = .3,
                        fill='dodgerblue')+
            geom_line(color='blue')+
            annotate(geom="text",size =5,x = mu+2*sigma, y = .85*y_top,
                     label=paste("P(X>=",a,")=\n",round(1-pnorm(a,mu,sigma),3),sep=""),
                     color='blue')+
            theme_bw()
    }else if(tail=="middle"){
        ggplot(data = df,
               aes(x = x, y=y))+
            geom_hline(yintercept = 0)+
            geom_ribbon(data = subset(df, (x>a & x<b)),
                        aes(x=x,ymax =y),ymin =0,
                        color='blue',
                        alpha = .3,
                        fill='dodgerblue')+
            geom_line(color='blue')+
            annotate(geom="text",size =5,x = mu+2*sigma, y = .85*y_top,
                     label=paste("P(",a,"<= X <=",b,")=\n",
                                 round(pnorm(b,mu,sigma)-pnorm(a,mu,sigma),3),sep=""),
                     color='blue')+
            theme_bw()
    } else {
        ggplot(data = df,
               aes(x = x, y=y))+
            geom_hline(yintercept = 0)+
            geom_ribbon(data = subset(df,x<a),
                        aes(x=x,ymax =y),ymin =0,
                        color='blue',
                        alpha = .3,
                        fill='dodgerblue')+
            geom_line(color='blue')+
            annotate(geom="text",size =5,x = mu+2*sigma, y = .85*y_top,
                     label=paste("P(X<=",a,")=\n",round(pnorm(a,mu,sigma),3),sep=""),
                     color='blue')+
            theme_bw()
    }
}
