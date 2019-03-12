#1
###################################################
##################  data scrapy  ##################
###################################################
library("rvest")
library("stringi")

#问题：由于使用网址的编码不是常用编码，所以尝试用以下方法找出网址编码
#1.创建临时文件
f = tempfile()
download.file("http://vip.stock.finance.sina.com.cn/q/go.php/vFinanceAnalyze/kind/mainindex/index.phtml?s_i=&s_a=diyu_3500&s_c=&reportdate=2018&quarter=1", f)
fchars = readChar(f,file.info(f)$size)
stri_enc_detect(fchars)
#2.发现文档是 GB18030 编码
futf8 = stri_encode(fchars,"GB18030","UTF8")
fhtml = read_html(futf8)
#3.这样就可以开始读取数据了

#data scrapy
#1.设置变量
link = NULL
stock_name = NULL
keyword = NULL
keyword1 = NULL
keyword2 = NULL
keyword3 = NULL
per_share_earnings = NULL         #每股收益
ROE = NULL                        #净资产收益率
growth_rate_of_net_assets = NULL  #净资产增长率(成长能力)
LOAR = NULL                       #资产负债率(偿债能力)
CASHCL = NULL                     #现金流量比率(现金流量)

#2.由于每项数据有4页，先找出相同部分的网址，再使用for循环翻页
url1 = "http://vip.stock.finance.sina.com.cn/q/go.php/vFinanceAnalyze/kind/"
url2 = "/index.phtml?s_i=&s_a=diyu_3500&s_c=&reportdate=2018&quarter=1&p="
f1 = "mainindex"
f2 = "grow"
f3 = "debtpaying"
f4 = "cashflow"
num = seq(2,80,2)
for(page in 1:4){
  url = paste(url1,f1,url2,page,sep = "")
  web1 = read_html(url,encoding = "GB18030")
  site = web1 %>%   
    html_nodes(xpath = '//a[@class="keyword"]') %>% 
    html_attr('href')
  stock = web1 %>%
    html_nodes(xpath = '//a[@class="keyword"]') %>% 
    html_text()
  link = c(link,site[-num])
  stock_name = c(stock_name,stock[num])
  keyword = c(keyword,stock[-num])
  per_share_earnings = c(per_share_earnings,web1 %>%
                           html_nodes(xpath = '//table[@class="list_table"]/tr/td[3]') %>% 
                           html_text())
  ROE = c(ROE,web1 %>%
            html_nodes(xpath = '//table[@class="list_table"]/tr/td[6]') %>% 
            html_text())
  
  url = paste(url1,f2,url2,page,sep = "")
  web2 = read_html(url,encoding = "GB18030")
  keyword1 = c(keyword1,web2 %>%
                 html_nodes(xpath = '//table[@class="list_table"]/tr/td[1]') %>% 
                 html_text())
  growth_rate_of_net_assets = c(growth_rate_of_net_assets,web2 %>%
                                  html_nodes(xpath = '//table[@class="list_table"]/tr/td[5]') %>% 
                                  html_text())
  
  url = paste(url1,f3,url2,page,sep = "")
  web3 = read_html(url,encoding = "GB18030")
  keyword2 = c(keyword2,web3 %>%
                 html_nodes(xpath = '//table[@class="list_table"]/tr/td[1]') %>% 
                 html_text())
  LOAR = c(LOAR,web3 %>%
             html_nodes(xpath = '//table[@class="list_table"]/tr/td[8]') %>% 
             html_text())
  
  url = paste(url1,f4,url2,page,sep = "")
  web4 = read_html(url,encoding = "GB18030")
  keyword3 = c(keyword3,web4 %>%
                 html_nodes(xpath = '//table[@class="list_table"]/tr/td[1]') %>% 
                 html_text())
  CASHCL = c(CASHCL,web4 %>%
               html_nodes(xpath = '//table[@class="list_table"]/tr/td[7]') %>% 
               html_text())
}

#######################################################
##################  data-processing  ##################
#######################################################
stock_name
#注意到变量stock_name有160项，这是由于最后一页共11个元素，
#但由于设置的共40个间距，最后会多出29个空字符，
#故合成为数据框时要将NA去除
stock_name = na.omit(stock_name)

#由于每一个网页的顺序不同，故要先排序再合并
a1 = data.frame(keyword,stock_name,per_share_earnings,ROE,link)
a1 = a1[order(a1[,1]),]
a2 = data.frame(keyword1,growth_rate_of_net_assets)
a2 = a2[order(a2[,1]),]
a3 = data.frame(keyword2,LOAR)
a3 = a3[order(a3[,1]),]
a4 = data.frame(keyword3,CASHCL)
a4 = a4[order(a4[,1]),]

cbind(a1[,1],a2[,1],a3[,1],a4[,1])
#然而查看数据时，发现有些数据框都有几处相同，
#打开原网站，仔细查看后发现比如第一个变量所在的第1页与第2页有四个重复股票
#第二次重新爬取查看数据时发现重复的地方又变了
#故将数据储存并进行整合，之后用的都是最后的统一数据

#setwd("E:/作业/R")
#a = list(a1 = a1,a2 = a2,a3 = a3,a4 = a4)
#write.csv(a,file = "stock1.csv")
#write.csv(a,file = "stock2.csv")
a = read.csv("黄家含_2016310814_SCEXAM.csv",header = T)
index = duplicated(a[,1])
any(index)                        #发现没有重复项了    
a = cbind(a[,-5],a[,5])
names(a) = c("keyword","stock_name","per_share_earnings","ROE",
                "growth_rate_of_net_assets","LOAR","CASHCL","link")

#发现有"--"数据，应删去，并将其中需要的变量转成数值型矩阵
rank = c(which(a[,4] == "--"),which(a[,5] == "--"),which(a[,7] == "--"))
data = a[-rank,]
which(data == "--")
x = apply(data[,3:7],2,as.numeric)

########################################################
##################  data description  ##################
########################################################
summary(x)
par(mfrow = c(2,3))
boxplot(x[,1])
boxplot(x[,2])
boxplot(x[,3])
boxplot(x[,4])
boxplot(x[,5])
#可以看出数据存在异常值，接下来检查异常值
par(mfrow = c(1,2))
hist(x[,1])
plot(x[,1])
which(x[,1] > 1)
hist(x[,2])
plot(x[,2])
which(x[,2] < (-150))
hist(x[,3])
plot(x[,3])
which(x[,3] < (-50) | x[,3] > 200) 
hist(x[,4])
plot(x[,4])
hist(x[,5])
plot(x[,5])
which(x[,5] < (-400))
xnew = x[-c(62,67,73,97,102,114),]