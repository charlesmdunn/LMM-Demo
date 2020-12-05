library(readr)
kc_house_data <- read.csv("kc_house_data.csv")

### inspect the data 
xyplot(kc_house_data$price ~ kc_house_data$sqft_living)
xyplot(kc_house_data$price ~ kc_house_data$)

### try some simple lms
fitlm <- lm(data = kc_house_data, formula = price ~ sqft_living + bathrooms + bedrooms)
## fix the qq plot by logging the response
fitlm2 <- lm(data = kc_house_data, formula = log(price) ~ sqft_living + bathrooms + bedrooms)

##there are still some high influence points in the data which are worth looking into, 15871

kc_house_data["15871",] ##we can see this is a one floor $640K house with 33 bedrooms and 1.75 bath...
## make a new data table which will serve as the cleaned data 
house_data <- kc_house_data[-15871,] ## since the row names correspond to their position still

fitlm3 <- update(fitlm2, data = house_data) ### looks OK

### before we get into introducing more variables we can try a gls to see if we have heterosc. which
## which can be modeled by zip code



fitlme <- lme(data = house_data, fixed = log(price) ~ sqft_living + bathrooms + bedrooms, random =  ~1|zipcode)

fitlme2 <- lme(data = house_data, fixed = log(price) ~ sqft_living + bathrooms + bedrooms, random =  ~|zipcode)


ranef(fitlme)


#### go back and explore more variables w/ resp to log price

xyplot(log(house_data$price) ~ house_data$condition)




fitlme3 <- lme(data = house_data, fixed = log(price) ~ sqft_living + bathrooms + bedrooms + condition,
               random =  ~condition|zipcode)
### lets check out a couple zip codes and see if any of our predictors do better when restricted to
# only one zip
zipcodes <- levels(factor(house_data$zipcode))
length(house_data$zipcode[which(house_data$zipcode == zipcodes[[1]])])
### get an idea of how many of each zip code there are
f <- function(x){
  l <- length(house_data$zipcode[which(house_data$zipcode == zipcodes[[x]])])
  l
}
zipcount <- vapply(1:70, f, numeric(1))
names(zipcount) <- zipcodes
zipcount #### looks like theres at least fifty of each 


### we can see that some zip codes have very low variation in their lot squarefootage, whereas others have 
## a large variance and demonstrate a reasonable linear relationship with log price

house_98074 <- house_data[which(house_data$zipcode == 98074),]

xyplot(log(house_98074$price) ~ house_98074$sqft_lot)

house_98103 <- house_data[which(house_data$zipcode == 98103),]

xyplot(log(house_98103$price) ~ house_98103$sqft_lot)

xyplot(log(house_data$price) ~ house_data$waterfront)

### another interesting feature is the waterfront feature


## good candidate to use as a grouping feature for random effect? Nope


fitlm4 <- lm(data = house_data, formula = log(price) ~ sqft_living + bathrooms + bedrooms
             + condition + waterfront)

fitlm4 <- lm(data = house_data, formula = log(price) ~ sqft_living + bathrooms + bedrooms
             + condition)
fitlm5 <- lm(data = house_data, formula = log(price) ~  sqft_above + sqft_living + bathrooms + bedrooms
                         + condition + floors + view + lat + long)




s1 <- sample(1:70, 6)

s1_house <- house_data[which(house_data$zipcode %in% zipcodes[s1]),]

water <- house_data[which(house_data$waterfront == 1),]

no_water <- house_data[which(house_data$waterfront == 0),]


fitlme_s <- lme(data = house_data, fixed = log(price) ~ sqft_living + bathrooms + bedrooms + condition + sqft_lot,
                random =  ~sqft_lot|zipcode)




### so zipcodes was a good candidate for random effect whereas we struggled to include waterfront into
#our model, no lets see if we can include waterfront into our residual covariance matrix


fitgls <- gls(data = house_data, model = log(price) ~  sqft_above + sqft_living + bathrooms + bedrooms
             + condition + floors + view + lat + long, weights = varIdent(form = ~1|waterfront))

## so we can see we get substantially different parameter estimates 1.000000 1.554113 for sd

##we can check the qqnorm plots for each strata to ensure their residuals are still normal
qqnorm(fitgls, form =   (~ resid(.,)|waterfront))

fitlme <- lme(data = house_data, fixed = log(price) ~  sqft_above + sqft_living + bathrooms + bedrooms
             + condition + floors + view + lat + long, random = ~1|zipcode, 
             weights = varIdent(form = ~1|waterfront))
#### alternative approach make gls with spatial covariance matrix
fitgls <- gls(data = house_data, model = log(price) ~  sqft_above + sqft_living + bathrooms + bedrooms
              + condition + floors + view, 
              correlation = corGaus(form = ~lat + long, metric = "euclidean", nugget = .02))


##unfortunately in this instance we have repeated (lat,long) observations /which corSpatial does not
# like. Often there is a grouping factor which will separate out such observations as in the case of 
# longitudinal data where the cor structure can be grouped by individual.
# we could add some noise to our data to jitter that lat and long measurements but seeing as how they
# worked quit well in the linear model that probably isn't necessary
## if however there was no overall global linear relationship between lat long and log price such
# a covariance matrix would be a good option.


### get 200 samples from only 6 zipcodes
zsamp <- sample(levels(factor(house_data$zipcode)), 6)
house_s3 <- house_data[which(house_data$zipcode %in% zsamp),]
house_s3 <- house_s3[sample(nrow(house_s3), 200),]
levels(factor(house_s3$zipcode))

###ugly way to see how many of each level without dplyr

f <- function(x){
  l <- length(house_s3$zipcode[which(house_s3$zipcode == zipcodes[[x]])])
  l
}
zipcount <- vapply(1:70, f, numeric(1))
zipcount

### add some noise to jitter lat and long, might need to run these six lines a few times if it doesn't
##jitter everything

n1 <- round(rnorm(200)/100,4)
n2 <- round(rnorm(200)/100,4)

house_s3$lat <- house_s3$lat + n1
house_s3$long <- house_s3$long + n2

nrow(house_s3) - length(unique(house_s3$lat))
nrow(house_s3) - length(unique(house_s3$long))



##now we can fit a model with spatial correlation

fitgls <- gls(data = house_s3, model = log(price) ~  sqft_above + sqft_living + bathrooms + bedrooms
              + condition + floors + view, 
              correlation = corGaus(form = (~lat + long|zipcode), metric = "euclidean", nugget = .01))
## and we can grab the new correlation structures by group as follows

getVarCov(fitgls, individual  = 5)