library(readr)
library(dplyr)
library(lattice)
library(ggplot2)
library(data.table)
library(shinydashboard)
library(rmarkdown)
library(flexdashboard)

df<- read_csv("VI.3_Chapter_vi_2017.csv")

#quanity = amount
#value = cost
#other imports =  Pet coke, Paraffin Wax, 
#Petroleum Jelly, LSWR, Aviation Gas, MTBE Reformate;

#other exports = VGO, Benzene, MTO, Pet coke, CBFS Sulphur;

head(df)

#all the attributes are in correct datatype
str(df)

#there is no null value in the attributes
#but there are 0 for the some of the items
summary(df)

names(df) <- c("Category","Item", "q2011", "v2011","q2012","v2012","q2013","v2013","q2014","v2014","q2015","v2015","q2016","v2016")

#getting unique values of category
unique(df$Category)

#subsetting import. total and export seperately

df_import <- df %>%
  filter(Category %in% c("Imports","Product Import"))
df_import$Category[df_import$Category == 'Product Import'] <- 'Imports'

df_export <- df %>% 
  filter(Category %in% "Product Export") 
df_export$Category[df_export$Category == 'Product Export'] <- 'Exports'  

df_total <- df %>%
  filter(Category %in% c("Total Product Export", "Total Import"))

df_import_trans <- as.data.frame(t(df_import))
header <- make.names(df_import_trans[2,])
colnames(df_import_trans) <- header
df_import_trans <- df_import_trans[-c(1,2),]
#new_row <- 
#names(df_import_trans) <- c('year','crudeoil','LPG','Petrol','Naphtha','AviationTurbineFuel','Kerosene','Diesel','Lube oil','Fuel Oil','Bitumen','Others1','productImport')


# visualizing years 
ggplot(df_import, aes(x=q2011)) + geom_histogram(fill="blue",bins=100)
ggplot(df_import, aes(x=q2012)) + geom_histogram(fill="red",bins=100)
ggplot(df_import, aes(x=q2013)) + geom_histogram(fill="blue",bins=100)
ggplot(df_import, aes(x=q2014)) + geom_histogram(fill="red",bins=100)
ggplot(df_import, aes(x=q2015)) + geom_histogram(fill="blue",bins=100)
ggplot(df_import, aes(x=q2016)) + geom_histogram(fill="red",bins=100)

ggplot(df_export, aes(x=q2011)) + geom_histogram(fill="orange",bins=100)
ggplot(df_export, aes(x=q2012)) + geom_histogram(fill='green',bins=100)
ggplot(df_export, aes(x=q2013)) + geom_histogram(fill="orange",bins=100)
ggplot(df_export, aes(x=q2014)) + geom_histogram(fill='green',bins=100)
ggplot(df_export, aes(x=q2015)) + geom_histogram(fill="orange",bins=100)
ggplot(df_export, aes(x=q2016)) + geom_histogram(fill='green',bins=100)

# visualizing over values
#ggplot(df_import, aes(x=v2011)) + geom_histogram(fill="blue",bins=100)
#ggplot(df_import, aes(x=v2012)) + geom_histogram(fill="red",bins=100)
#ggplot(df_import, aes(x=v2013)) + geom_histogram(fill="blue",bins=100)
#ggplot(df_import, aes(x=v2014)) + geom_histogram(fill="red",bins=100)
#ggplot(df_import, aes(x=v2015)) + geom_histogram(fill="blue",bins=100)
#ggplot(df_import, aes(x=v2016)) + geom_histogram(fill="red",bins=100)

#ggplot(df_export, aes(x=v2011)) + geom_histogram(fill="orange",bins=100)
#ggplot(df_export, aes(x=v2012)) + geom_histogram(fill='green',bins=100)
#ggplot(df_export, aes(x=v2013)) + geom_histogram(fill="orange",bins=100)
#ggplot(df_export, aes(x=v2014)) + geom_histogram(fill='green',bins=100)
#ggplot(df_export, aes(x=v2015)) + geom_histogram(fill="orange",bins=100)
#ggplot(df_export, aes(x=v2016)) + geom_histogram(fill='green',bins=100)


#Getting the unit price of all the products

df_unit <- df_import %>% mutate(u2011 = v2011/q2011) %>%
  mutate(u2012 = v2012/q2012) %>%
  mutate(u2013 = v2013/q2013) %>%
  mutate(u2014 = v2014/q2014) %>%
  mutate(u2015 = v2015/q2015) %>%
  mutate(u2016 = v2016/q2016) 

summary(df_unit)
#replacing nan values

df_unit$u2011[is.na(df_unit$u2011)] <- 0
df_unit$u2012[is.na(df_unit$u2012)] <- 0
df_unit$u2013[is.na(df_unit$u2013)] <- 0
df_unit$u2016[is.na(df_unit$u2013)] <- 0

# subsetting only quantity of imports and exports
df_qua_import <- df_import %>%
  dplyr::select(Item,starts_with('q'))
df_qua_export <- df_export %>%
  dplyr::select(Item,starts_with('q'))


# subsetting only value of imports and exports 
df_val_import <- df_import %>%
  dplyr::select(Item,starts_with('v'))
df_val_export <- df_export %>%
  dplyr::select(Item,starts_with('v'))

df_qua_export_trans <- t(df_qua_export)
df_qua_import_trans <- t(df_qua_import)

rownames(df_qua_export_trans) <- c(1,2,3,4,5,6,7)
new_col <- c('year',2011,2012,2013,2014,2015,2016)
df_qua_export_trans <- cbind(df_qua_export_trans,new_col)
df_qua_import_trans <- cbind(df_qua_import_trans,new_col)

colnames(df_qua_export_trans) <- df_qua_export_trans[1,]
colnames(df_qua_import_trans) <- df_qua_import_trans[1,]
df_qua_export_trans <- df_qua_export_trans[-1,]
df_qua_import_trans <- df_qua_import_trans[-1,]
# since df_qua_export_trans is not treating as df
#checking for the dataype

df_qua_export_trans <- as.data.frame(df_qua_export_trans)
df_qua_import_trans <- as.data.frame(df_qua_import_trans)

#import export of bitumen over years
ggplot(data = df_qua_export_trans, aes(x=year, y=Bitumen, group=1)) +
  geom_line()+
  geom_point()

ggplot(data = df_qua_import_trans, aes(x=year, y=Bitumen, group=1)) +
  geom_line()+
  geom_point()

#import of crude oil over years
names(df_qua_export_trans) <- c('lpg','petrol','napthal','AviataionTurbuneFuel','kerosene','diesel','ldo','lubeoil','fueloil','bitumen','others1','year')
names(df_qua_import_trans) <- c('CrudeOil','lpg','petrol','napthal','AviataionTurbuneFuel','kerosene','diesel','lubeoil','fueloil','bitumen','others1','productimport','year')

ggplot(data = df_qua_import_trans, aes(x=year, y=CrudeOil, group=1)) +
  geom_line()+
  geom_point()

#import export of petrol over years

ggplot(data = df_qua_export_trans, aes(x=year, y=petrol, group=1)) +
  geom_line()+
  geom_point()

ggplot(data = df_qua_import_trans, aes(x=year, y=petrol, group=1)) +
  geom_line()+
  geom_point()

#import export of diesel over years

ggplot(data = df_qua_export_trans, aes(x=year, y=diesel, group=1)) +
  geom_line()+
  geom_point()

ggplot(data = df_qua_import_trans, aes(x=year, y=diesel, group=1)) +
  geom_line()+
  geom_point()

####The main reasons of crude oil price fall include rapid expansion in unconventional supplies, shift in the OPEC policy
###after a period of high prices, market sentiments, and rising demand in Asia-Pacific

#import export of aviation turbune oil

df_qua_import_trans$AviataionTurbuneFuel <- as.numeric(df_qua_import_trans$AviataionTurbuneFuel)
bwplot(df_qua_import_trans$AviataionTurbuneFuel)

df_qua_export_trans$AviataionTurbuneFuel <- as.numeric(df_qua_export_trans$AviataionTurbuneFuel)
bwplot(df_qua_export_trans$AviataionTurbuneFuel)

#import export of total products
df_total_q <- df_total %>%
  dplyr::select(Item,starts_with('q'))
df_total_v <- df_total %>%
  dplyr::select(Item,starts_with('v'))

df_total_q_t <- t(df_total_q)
df_total_v_t <- t(df_total_v)

rownames(df_total_q_t) <- c(1,2,3,4,5,6,7)
new_col <- c('year',2011,2012,2013,2014,2015,2016)
df_total_q_t <- cbind(df_total_q_t,new_col)
df_total_v_t<- cbind(df_total_v_t,new_col)

colnames(df_total_q_t) <- df_total_q_t[1,]
colnames(df_total_v_t) <- df_total_v_t[1,]
df_total_q_t <- df_total_q_t[-1,]
df_total_v_t <- df_total_v_t[-1,]
df_total_q_t <- as.data.frame(df_total_q_t)
df_total_v_t <- as.data.frame(df_total_v_t)

names(df_total_v_t) <- c('import','export','year')
df_total_v_t$import <- as.numeric(df_total_v_t$import)
df_total_v_t$export <- as.numeric(df_total_v_t$export)
df_total_v_t$year <- as.numeric(df_total_v_t$year)

ggplot(data = df_total_v_t, aes(x = year)) +
  geom_line(aes(y = import), color = "blue") +
  geom_line(aes(y = export), color = "red")

names(df_total_q_t) <- c('import','export','year')
df_total_q_t$import <- as.numeric(df_total_q_t$import)
df_total_q_t$export <- as.numeric(df_total_q_t$export)
df_total_q_t$year <- as.numeric(df_total_q_t$year)

ggplot(data = df_total_q_t, aes(x = year)) +
  geom_line(aes(y = import), color = "orange") +
  geom_line(aes(y = export), color = "green")








