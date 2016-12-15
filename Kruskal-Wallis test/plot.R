library(ggplot2)
library(scales)

mbcd <- read.csv("mbcd_plot.csv", header = TRUE)
cols <- c(2,3,4) # columns containing the replicates
SEM <- function(x) sqrt(var(x)/length(x)) # function to calculate SEM
mbcd.df  <- transform(mbcd, mean=rowMeans(mbcd[cols]), st.err=apply(mbcd[cols],1, SEM)) # dataframe with means and SEMs

# Plotting the graph
ggplot(mbcd.df, aes(x=as.factor(Group), y=mean, fill=Species)) +
   # Bars parameters
   geom_bar(position=position_dodge(), stat="identity", colour='black', width = 0.5) +
   # Error bars
   geom_errorbar(aes(ymin=mean-st.err, ymax=mean+st.err), width=.1,position=position_dodge(0.5)) +
   # Graph parameters
   theme_bw() + 
   labs(x = "MBCD groups", y = "Relative haemolysis") +
   scale_y_continuous(breaks=c(0:10*0.1), limits=c(0, 1.05), labels = percent, expand = c(0,0)) +
   theme(legend.text=element_text(size=13), plot.title=element_text(size=14), axis.text=element_text(size=15), axis.title=element_text(size=14), panel.grid.major.x = element_blank()) +
   geom_text(data=mbcd.df, aes(label=paste0(round(mean*100,0),"%"), y=mean+0.03), size=4, position=position_dodge(1)) +
   scale_fill_manual("Blood type\n", values=c("#CCCCCC","#FFFFFF")) + 
   ggtitle("Effect of MβCD treatment of blood on haemolysis by NetF\nError bars are SEM (N = 3)") +
   # Lines
   annotate(x=c(1,1,4,4),y=c(.92,.95,.95,.92),"path") +
   annotate(x=c(2.5,2.5), y=c(.95,.97), "path") +
   annotate(x=c(2,2,4,4),y=c(.77,.8,.8,.77),"path") +
   annotate(x=c(3,3), y=c(.8,.82), "path") +
   # Text
   annotate("text",x=2.5,y=1,label="P=0.03", size=5) +
   annotate("text",x=3,y=.85,label="P=0.03", size=5)
