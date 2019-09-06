library(data.table)      #Read csv as data table
library(ggplot2)         #Visualization
setwd("C:\\Users\\user\\Desktop\\Year 2 NBS\\Semester 1\\BC2406 ANALYTICS I VISUAL & PREDICTIVE TECHNIQUES\\Week 4\\4 Data Structures and Visualizations")
Lawsuit.dt <- fread('Lawsuit.csv')
str(Lawsuit.dt)          #Display the structure (261 obs. of 10 variables)
cols <- c('Dept', 'Gender', 'Clin', 'Cert', 'Rank')         #Store columns to be converted to factor
Lawsuit.dt[,(cols):=lapply(.SD, as.factor),.SDcols=cols]    #Convert selected columns to factor
summary(Lawsuit.dt) #Quick summary of data table 
getOption("scipen")        #Show whole number instead of scientific notation
options("scipen" = 100)

ggplot(data = Lawsuit.dt, mapping = aes(x = Gender, y = Sal94, fill = Gender)) +       #Start analysis with a boxplot of salary against gender
  geom_boxplot(show.legend = TRUE)+
  labs(title = "Chart 1: Salary vs Gender")+
  scale_x_discrete(labels = c("Female", "Male"))+
  scale_fill_discrete(labels = c("Female", "Male"))

ggplot(data = Lawsuit.dt, mapping = aes(x = Gender, y = ..count.., fill = Rank)) +   #Analyze Rank against gender
  geom_bar(show.legend = TRUE, aes(fill = Rank))+
  labs(title = "Chart 2: Rank vs Gender")+
  scale_x_discrete(labels = c("Female", "Male"))+
  scale_fill_discrete(labels = c("Assistant", "Associate", "Full professor"))

ggplot(data = Lawsuit.dt, mapping = aes(x = Dept, y = ..count.., fill = Gender)) +            #Analyze Department across gender
  geom_bar(show.legend = TRUE, aes(fill = Gender))+
  labs(title = "Chart 3: Dept vs Gender")+
  scale_x_discrete(labels = c("Biochemistry/Molecular Biology", "Physiology", "Genetics", "Pediatrics", "Medicine", "Surgery"))+
  scale_fill_discrete(labels = c("Female", "Male"))

ggplot(data = Lawsuit.dt, mapping = aes(x = Exper, y = Sal94)) +         
  geom_point()+
  labs(title = "Chart 4: Exp vs Salary by Gender")+
   facet_grid(.~Gender)

ggplot(data = Lawsuit.dt, mapping = aes(x = Rank, y = Cert)) +            
  geom_bar(stat = "identity")+
  labs(title = "Chart 5: Cert vs Rank")+
  facet_grid(.~Gender)
  
ggplot(data = Lawsuit.dt, mapping = aes(x = Cert, y = Sal94, fill = Gender)) +  
  geom_boxplot(show.legend = TRUE)+
  labs(title = "Chart 6: Cert vs Salary")+
  facet_grid(.~Gender)+
  scale_x_discrete(labels = c("wo/Cert", "w/Cert"))+
  scale_fill_discrete(labels = c("Female", "Male"))

ggplot(data = Lawsuit.dt, mapping = aes(x = Exper, y = Sal94)) +  
  geom_point(show.legend = TRUE)+
  labs(title = "Chart 6: Experience vs Salary")+
  facet_grid(.~Gender)+
  scale_fill_discrete(labels = c("Female", "Male"))

Table1 <- Lawsuit.dt[, .(Mean_Salary = mean(Sal94)), keyby = .(Gender, Rank, Dept)]  #Get mean salary based on gender, rank, dept
Table1[order(Rank, Dept, -Mean_Salary)]  #Order table based on rank, dept then mean salary

Table2 <- Lawsuit.dt[Dept=='4', .(Gender, Rank, .N), keyby = Exper]
Table3 <- Lawsuit.dt[, .(Mean_Salary = mean(Sal94)), keyby = .(Exper, Gender)]


