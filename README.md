# Analysis-of-Sales-Report-of-a-Clothes-Manufacturing-Outlet
### Question
A high-end fashion retail store is looking to expand its products. It
wants to understand the market and find the current trends in the industry. It has a
database of all products with attributes, such as, style, material, season, and the
sales of the products over a period of two months. There are two files provided,
and the detailed description of each is given below:
Attribute DataSet.csv
Dress_ID: A unique identifier for each dress
Style: Style of dress, can belong to one of 12 styles, including casuals, novelty, etc.
Price: Price category of the dress (low, average, medium, high, and very high)
Rating: A number between 0 and 5, specifying the rating of the dress
Size: Size of the dress (small, medium, large, XL, and free)
Season: Season category of the dress, i.e., summer, spring, etc.
NeckLine: Type of neckline, for example, V-neck, collar, etc.
Sleeve length: Length of the sleeve—full, three-quarters, etc.
Waistline: Waistline of the dress
Material: Material of the dress, for example, silk, cotton, etc.
Fabric type: Fabric type of the dress
Decoration: Decoration of dress, like ruffles, embroidery, etc.
Pattern Type: Pattern type of the dress—dot, animal print, etc.
Recommendation: A binary value suggesting a recommendation (1) or not (0)
Dress Sales.xlsx:
Dress_ID: A unique identifier for each dress The remaining columns depict the
sales for each dress on a particular date. Date ranges from 29/8/2013 to
12/10/2013, and the sales are registered for alternative days.
The goals of this project are: 
 − To automate the process of recommendations, the store needs to analyze the
   given attributes of the product, like style, season, etc., and come up with a model to
   predict the recommendation of products (in binary output – 0 or 1) accordingly.
 − In order to stock the inventory, the store wants to analyze the sales data and
   predict the trend of total sales for each dress for an extended period of three more
   alternative days.
 − To decide the pricing for various upcoming clothes, they wish to find how the
   style, season, and material affect the sales of a dress and if the style of the dress is
   more influential than its price.
 − Also, to increase the sales, the management wants to analyze the attributes of
   dresses and find which are the leading factors affecting the sale of a dress.
 − To regularize the rating procedure and find its efficiency, the store wants to find
   if the rating of the dress affects the total sales.

### Solution
Task 1: To automate the process of recommendations, the store needs to analyze
the given attributes of the product, like style, season, etc., and come up with a
model to predict the recommendation of products (in binary output – 0 or 1)
accordingly.
Solution: I am using any classification model to predict binary output. I compare
the Naïve Bayes, SVM, Decision Tree and Random Forest.
### Naïve Bayes:
    naive_bayes<-naiveBayes(Recommendation~.,data = data1)
    summary(naive_bayes)
    Predictions<-predict(naive_bayes,data1)
    Predictions
    table(Predictions,data1$Recommendation)
    #Accuracy of Model
    table_mat<-table(Predictions,data1$Recommendation)
    accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
    accuracy_Test

### SVM:
    svms<-svm(Recommendation~.,data = data1,method = 'class')
    summary(svms)
    Predictions<-predict(svms,data1,type='class')
    Predictions
    table(Predictions,data1$Recommendation)
    #Accuracy of Model
    table_mat<-table(Predictions,data1$Recommendation)
    accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
    accuracy_Test

### Decision Tree:
    tree<-rpart(Recommendation~.,data = data1,method = 'class')
    summary(tree)
    Predictions<-predict(tree,data1,type = 'class')
    Predictions
    table(Predictions,data1$Recommendation)
    #Accuracy of Model
    table_mat<-table(Predictions,data1$Recommendation)
    accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
    accuracy_Test

### Random Forest:
    forest<-randomForest(x = data1, y = data1$Recommendation,ntree =800)# build model
    summary(forest)
    Predictions<-predict(forest,data1)
    Predictions
    table(Predictions,data1$Recommendation)
    #Accuracy of Model
    table_mat<-table(Predictions,data1$Recommendation)
    accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
    accuracy_Test

I found the random forest has highest accuracy i.e 100% compare to other
classification model.
Task 2: In order to stock the inventory, the store wants to analyze the sales data
and predict the trend of total sales for each dress for an extended period of three
more alternative days.
Solution: I am using the Time Series model ARIMA to predict future value.
### Time serise Analysis using ARIMA
    vect<-c()
    j=0
    for(i in c(1:500)){
     d<-as.numeric(final_df[,i])
     na_mean(d)
     timeseries<-ts(d)
     fit <- auto.arima(timeseries)
     e<-forecast(fit, 3)
     vect=append(vect,sum(e$mean))
    }
    #Add Predicted Result in Sales Data
    result<-data.frame(vect)
    result
 
Task 3: To decide the pricing for various upcoming clothes, they wish to find how
the style, season, and material affect the sales of a dress and if the style of the dress
is more influential than its price.
Solution: I am using Anova to comparing the attributes.
In above picture I found the season is most significant factor which effect the sales.
### Anova Analysis of Sales to Style,Price,Season,Material
    spsm<-aov(Total_Sale~Style+Price+Season+Material,data = Sales)
    spsm
    summary(spsm)
    #Result: The attribute Season is affect sales mostly. The Style and Material not much
    significant to Sales. In the Style and Price, Style has significant value that the price.

Task 4: Also, to increase the sales, the management wants to analyze the attributes
of dresses and find which are the leading factors affecting the sale of a dress.
### Anova Analysis
    result<-
    aov(Sales$Total_Sale~Sales$Style+Sales$Price+Sales$Rating+Sales$Size+Sales$Season
    +Sales$NeckLine+Sales$SleeveLength+Sales$waiseline+Sales$Material+Sales$FabricT
    ype+Sales$Decoration+Sales$`Pattern Type`+Sales$Recommendation)
    result
    summary(result)

Task 5: To regularize the rating procedure and find its efficiency, the store wants to
find if the rating of the dress affects the total sales.
### Linear regression
    #Build Regression Model
    regression<-lm(Sales$Total_Sale~Sales$Rating)
    regression
    summary(regression)
    Predictions<-predict(regression,Sales)
