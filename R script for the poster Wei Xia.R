##########################################################################################################################
##########################################################################################################################
##CSSS term project Wei Xia 
##########################################################################################################################
##########################################################################################################################

library(statnet)
library(ndtv)
library(htmlwidgets)
library(latticeExtra)
library(igraph)
library(network)


#####################################################################################
#load the vertice atributes to the network
#####################################################################################


#find the unique valid dyad in Panel_list
node_info <- left_join(Edge_list, Panel_list,
                       by = c("LeadVC_ID"="VC1_ID",
                              "VC2_ID" = "VC2_ID",
                              "invest_year" = "year"))

node_info <- node_info %>% 
  select(-PC_name, -LeadVC_name, -VC2_name.x, -VC2_name.y, -VC1_name) %>% 
  distinct()


#######################################################################################
#generate the network objects
#######################################################################################

###first, generate a list, putting edge list by each year into the list. 
edge_network <- list()
g_edge_network <- list() # store the graph of each year

##total 25 years, so 25 seperated data frames

#I only use consective 21 years, to avoid using empty year
scope <- 1:length(1997:2017)

#putting each year's edge list into the edge_network

#This loop may take 10 mins 

# Start the clock!
ptm <- proc.time()

Degree_5y <- list()
Reciprocity_5y <- list()
edge_network_5y <- list()
Betweenness_5y <- list()
Closeness_5y <- list()
Eigenvector_5y <- list()
node_dyad_5y <-list()

for (i in scope) {
  edge_network[[i]] <- node_info %>% 
    filter(invest_year== 1997+i-1) %>% 
    select(LeadVC_ID, VC2_ID) %>% 
    distinct()
  
  ##generate a graph before changing to network object
  ##otherwise will generate many null vertex with number 1, 2,......till the VCID
  
  g_edge  <- graph_from_data_frame(edge_network[[i]], directed = T)
  
  edge_network[[i]] <- network(x=get.edgelist(g_edge ), 
                               matrix.type="edgelist", 
                               directed=TRUE)
  
  #calculate the reciprocity of every 5 years *(including the focal year)*
  #each network object will only have one value of reciprocity
  node_dyad_5y[[i]] <- node_info %>% 
    filter(invest_year==c(1997+i-5, 1997+i-4, 1997+i-3, 1997+i-2, 1997+i-1)) %>% 
    select(LeadVC_ID, VC2_ID) %>% 
    distinct()
  
  ##generate the graph 
  g_edge_5y <- graph_from_data_frame(node_dyad_5y[[i]], directed = TRUE )
  
  edge_network_5y[[i]] <- network(x=get.edgelist(g_edge_5y),
                                  matrix.type="edgelist",
                                  directed = TRUE)
  
  ##node level statistics
  Degree_5y <- c(Degree_5y,  list(degree(g_edge_5y, mode = "all")))
  Betweenness_5y <- c(Betweenness_5y, list(betweenness(g_edge_5y, directed = T)))
  Closeness_5y <- c(Closeness_5y,list(closeness(g_edge_5y, mode="all", weights=NA)))
  Eigenvector_5y <- c(Eigenvector_5y, list(eigen_centrality(g_edge_5y, directed = T)$vector))
  
}


rm(g_edge_5y)
rm(g_edge)


##take a look at the graph
boxplot(Degree_5y)
boxplot(Betweenness_5y)
boxplot(Closeness_5y)
boxplot(Eigenvector_5y)

#to use ggplot2
#change the degree_5y, betweenness_5y, closeness_5y, eigenvector_5y to a data frame
#because all are generated from the same igraph, they have the same sort of VCID. 

Node_statistics <- data.frame("Year"=as.numeric(),
                              "VCID"=as.numeric(), 
                              "Degree"=as.numeric(), 
                              "Betweenness"=as.numeric(), 
                              "Closeness" =as.numeric(),
                              "Eigenvector" =as.numeric())


for (i in 1:21) {
  #unlist to a dataframe, so that I can get the rownames later
  temp_degree <- as.data.frame(unlist(Degree_5y[[i]]))
  
  #degree_5y, closeness_5y, betweenness_5y all have the same rownames (vcid)
  
  temp <- data.frame("Year"=1997+i-1, 
                     "VCID"=rownames(temp_degree),
                     "Degree"=Degree_5y[[i]],
                     "Betweenness"=Betweenness_5y[[i]],
                     "Closeness"=Closeness_5y[[i]],
                     "Eigenvector"=Eigenvector_5y[[i]])
  
  Node_statistics <- rbind(Node_statistics, temp)
}

rm(temp_degree)
rm(temp)


#Plot the node level distribution
Node_statistics$Year <- as.factor(Node_statistics$Year)

#Degree distribution
ggplot(data = Node_statistics, aes(x=Year, y=LogDegree))+ 
  geom_point()+
  #geom_boxplot()+
  theme_bw()+
  #add a losess line # to dense to the degree0, losess no use. 
  #stat_smooth(aes(y=Degree), method ="loess",
  #            formula= y~ x, color="red", linetype=2, se=TRUE)+
  #adjust the labels on x axis, otherwise it's too dense.   
  coord_cartesian(xlim = c(1, 21))+
  #scale_x_discrete( breaks = seq(1,21,by=3))+
  #adjust title and font size
  ggtitle("VC Degree Distribution")+
  theme(plot.title = element_text(size =22, face="bold"))+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20, face="bold"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#Betweenness distribution
#the data select out VCs that have at least 1 syn. 
#higher Betweenness means the focal VC would involve syn with different VCs. 

ggplot(data = Node_statistics, aes(x=Year, y=Betweenness))+ 
  geom_point()+
  #geom_boxplot()+
  theme_bw()+
  #add a losess line # to dense to the degree0, losess no use. 
  #stat_smooth(aes(y=Degree), method ="loess",
  #            formula= y~ x, color="red", linetype=2, se=TRUE)+
  #adjust the labels on x axis, otherwise it's too dense.   
  coord_cartesian(xlim = c(1, 21))+
  #scale_x_discrete( breaks = seq(1,21,by=3))+
  #adjust title and font size
  ggtitle("VC Betweenness Distribution")+
  theme(plot.title = element_text(size =22, face="bold"))+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20, face="bold"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))




#Eigenvector distribution
#it shows VC would like to syn with other important VC
ggplot(data = Node_statistics, aes(x=Year, y=Eigenvector))+ 
  geom_point()+
  #geom_boxplot()+
  theme_bw()+
  #add a losess line # to dense to the degree0, losess no use. 
  #stat_smooth(aes(y=Degree), method ="loess",
  #            formula= y~ x, color="red", linetype=2, se=TRUE)+
  #adjust the labels on x axis, otherwise it's too dense.   
  coord_cartesian(xlim = c(1, 21))+
  #scale_x_discrete( breaks = seq(1,21,by=3))+
  #adjust title and font size
  ggtitle("VC Eigenvalue Distribution")+
  theme(plot.title = element_text(size =22, face="bold"))+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20, face="bold"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Stop the clock
proc.time() - ptm




#node_info includes all observed dyads in 1997-2017
##total 27,256 dyads

#Node_statistics includes all individual nodes in each year of 1997-2017
##total 14,481 nodes*year

#ADD VC_nationality to Node_statistics
Local01_list <- R_input_network_matrix %>% 
  select(VCID2, Local01) %>% 
  group_by(VCID2) %>%  ##remove those VCs that have multiple local01
  slice(1L) %>% 
  distinct()  #remove duplicats
#total 12173 VC (including those individually invest. )

#edge_network_5y is network of each previous 5ys
#add vertex attribute --local01 into each list of network_5y
for (i in 1:21){
  IDs <- as.numeric(network.vertex.names(edge_network_5y[[i]]))
  Local01_data <- data.frame("IDs"=IDs)
  Local01_data <- left_join(Local01_data, Local01_list,
                            by=c("IDs"="VCID2"))
  
  network::set.vertex.attribute(edge_network_5y[[i]],
                                "Local01",
                                as.numeric(Local01_data$Local01)
  )
  
}

rm(IDs)
rm(Local01_data)



##I use node_dyad_5y to generate the edge_network_5y
##so I can add edge attribute directly into the node_dyad_5y data frame. 
##then set.edge.attribute

Local01_dyad <- Panel_list %>% 
  select(VC1_ID, VC2_ID, 
         Local_0_0, Local_0_1,
         Local_1_0, Local_1_1) %>% 
  distinct()

for (i in 1:21){
  node_dyad_5y[[i]] <- left_join(node_dyad_5y[[i]], Local01_dyad, 
                                 by=c("LeadVC_ID"="VC1_ID",
                                      "VC2_ID"="VC2_ID"))
  
  network::set.edge.attribute(edge_network_5y[[i]],"Local_0_0",
                              as.numeric(node_dyad_5y[[i]]$Local_0_0)
  )
  
  network::set.edge.attribute(edge_network_5y[[i]],"Local_0_1",
                              as.numeric(node_dyad_5y[[i]]$Local_0_1)
  )
  
  network::set.edge.attribute(edge_network_5y[[i]],"Local_1_0",
                              as.numeric(node_dyad_5y[[i]]$Local_1_0)
  )
  
  network::set.edge.attribute(edge_network_5y[[i]],"Local_1_1",
                              as.numeric(node_dyad_5y[[i]]$Local_1_1)
  )
  
}



library(GGally)  ##to use ggnet2
library(ggplot2)

set.seed(1)

#1-21 network list, representing 1997-2017
n <- 16

edge_network_5y[[n]] %v% "node_color" <- ifelse(edge_network_5y[[n]]%v%"Local01" ==1, "red", "black")
edge_network_5y[[n]] %e% "edge_color" <- ifelse(edge_network_5y[[n]]%e%"Local_0_0"==1,"gray38",
                                                ifelse(edge_network_5y[[n]]%e%"Local_0_1"==1,"gray68",
                                                       ifelse(edge_network_5y[[n]]%e%"Local_1_0"==1,"gray88",
                                                              "greenyellow" #this is for Local_1_1)
                                                       )))

ggnet2(edge_network_5y[[n]], size=1.2, color="node_color",
       edge.size=0.8, edge.color="edge_color")

summary(edge_network_5y[[n]])




VC2VC1_bylocal <- Panel_list %>% 
  filter(VC2VC1_01==1) %>% 
  select(VC1_ID,VC2_ID,VC2VC1_01,year,
         Local_0_0,Local_0_1,
         Local_1_0,Local_1_1) %>% 
  group_by(year) %>% 
  #aggregate number of the 4 combinations of local
  mutate(Localsum00 = sum(Local_0_0)) %>% 
  mutate(Localsum01 = sum(Local_0_1)) %>% 
  mutate(Localsum10 = sum(Local_1_0)) %>% 
  mutate(Localsum11 = sum(Local_1_1)) %>% 
  #I will only need the four aggregate numbers 
  select(year,Localsum00,Localsum01,
         Localsum10, Localsum11) %>% 
  arrange(year) %>% 
  #each year only need one aggregate number
  distinct() %>% 
  #reshape to a long format so can plot 4lines together
  #Total is a new variable name. 
  gather(Localsum, Total, 
         -year)


ggplot(VC2VC1_bylocal, aes(x=year, y=Total, group=Localsum, color=Localsum))+
  geom_line(aes(linetype = Localsum))+
  geom_point()+
  theme_bw()+
  coord_cartesian(xlim = c(1995, 2017))+
  scale_x_continuous(breaks = seq(1995,2017,1))+
  ggtitle("Number of Reciprocated Tie")+
  theme(plot.title =element_text(size=22, face="bold"))+
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=20, face="bold"))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





##the network list is edge_network_5y, including 1997-2017, by every 5ys.
library(btergm)

fit0 <- btergm(edge_network_5y ~ mutual(same=NULL,
                                        by=NULL, 
                                        diff=TRUE)+
                 edges)

fit1 <- btergm(edge_network_5y ~ mutual+ edges
               +istar(2)+ostar(2))

fit2 <- btergm(edge_network_5y ~ mutual(same=NULL,
                                        by=NULL, 
                                        diff=TRUE)+ 
                 edges +
                 istar(2)+ostar(2)+
                 nodecov("Local01")+
                 edgecov(edge_network_5y,"Local_0_0")+
                 edgecov(edge_network_5y,"Local_0_1")+
                 edgecov(edge_network_5y,"Local_1_0"),
               parallel="snow", ncpus =10
)

summary(fit)

library(texreg)
screenreg(list(fit0,fit1,fit2),
          stars = c(0.001,  0.01, 0.05),
          ci.force=FALSE,
          strong.signif = T)
htmlreg(list(fit0,fit1,fit2),
        stars = c(0.001,  0.01, 0.05),
        ci.force=FALSE,
        strong.signif = T, 
        file="mytable.doc")
texreg(list(fit), use.packages=FALSE, label="tab:4", scriptsize=FALSE,
       strong.signif=TRUE)
