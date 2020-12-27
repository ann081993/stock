library(quantmod)
# https://finance.yahoo.com/
# https://www.datacamp.com/community/blog/r-xts-cheat-sheet
library(randomForest)
library(pROC)
library(caret)

ks <- read.csv("https://raw.githubusercontent.com/ann081993/stock/main/ks.csv", stringsAsFactors = FALSE)
kq <- read.csv("https://raw.githubusercontent.com/ann081993/stock/main/kq.csv", stringsAsFactors = FALSE)
colnames(ks) <- c("name", "code", "category", "product", "start", "mo", "head", "hp", "region")
colnames(kq) <- c("name", "code", "category", "product", "start", "mo", "head", "hp", "region")

ks$code <- paste0(sprintf("%06d", ks$code), ".KS")
kq$code <- paste0(sprintf("%06d", kq$code), ".KQ")

train <- NULL
n <- 1
for(cd in ks$code) {
        cat(n, ": ", cd, "\n"); n <- n + 1
        
        # load data by code
        stock_data <- getSymbols(cd, auto.assign = FALSE, from = "2020-01-01")
        stock_data <- stock_data[, -c(5, 6)] # remove adjusted, volume
        
        ind_date <- index(stock_data)
        
        # manage by date
        for(d in 1:length(ind_date)) {
                ref_date <- ind_date[d]
                from_date <- ref_date - 6

                part <- stock_data[ind_date[ind_date >= from_date & ind_date <= ref_date], ]
                part <- merge(part, as.Date(from_date:ref_date), fill = na.locf)

                train <- rbind(train, as.vector(t(part)))
        }
}

train <- train[complete.cases(train), ]
dim(train)

train <- train / colMeans(t(train))
plot(density(train[, 28]))

x_train <- train[, 1:24]
y_train <- ifelse(train[, 28] > 1.1, 1,
                  ifelse(train[, 28] < 0.9, -1, 0))

model_input <- cbind(x_train, y_train)
model_input <- data.frame(model_input)
names(model_input)[ncol(model_input)] <- "change"
model_input$change <- factor(model_input$change)

rf_fit <- randomForest(formula = change ~ ., data = model_input, na.action = na.pass)
rf_fit
confusionMatrix(predict(rf_fit, model_input[, -ncol(model_input)]), model_input$change)
predictions <- predict(rf_fit, model_input[, -ncol(model_input)], type = "prob")
rf_roc <- roc(as.numeric(as.character(model_input$change)), predictions[, 3])
plot(rf_roc, col = "red")



image(x_train[y_train == 1, ])

# prediction with ref_date
ref_date <- as.Date("2020-12-28")
test_code <- NULL
test <- NULL
n <- 1
for(cd in ks$code) {
        cat(n, ": ", cd, "\n"); n <- n + 1
        
        from_date <- ref_date - 6
        # load data by code
        stock_data <- try(getSymbols(cd, auto.assign = FALSE, from = from_date))
        if(class(stock_data) != "try-error") {
                stock_data <- stock_data[, -c(5, 6)] # remove adjusted, volume
                
                ind_date <- index(stock_data)
                
                part <- stock_data[ind_date[ind_date >= from_date & ind_date <= ref_date], ]
                part <- merge(part, as.Date(from_date:ref_date), fill = na.locf)
                        
                test <- rbind(test, as.vector(t(part)))
                test_code <- c(test_code, cd)
        }
}

table(complete.cases(test))
#test <- test[complete.cases(test), ]
dim(test)

test <- test / colMeans(t(test))
test <- data.frame(test)
colnames(test) <- paste0("V", 1:24)
predictions <- predict(rf_fit, test, type = "prob")
plot(predictions[, 3])

test_code[predictions[, 3] > 0.1]
test_code[predictions[, 1] > 0.1]


ks <- ks[ks$start < "2019-12-31", ]
plot(as.vector(stock_data$`338100.KS.Open`), as.vector(stock_data$`338100.KS.High`))




stock_data <- getSymbols(Symbols =  "096770.KS", src = "yahoo", from = "2018-01-01", to = "2018-12-31", auto.assign = FALSE)

stock_data <- getSymbols(Symbols =  "096770.KS",
                         src = "yahoo",
                         from = "2020-08-01", to = "2020-10-04", auto.assign = FALSE)

plot(stock_data)
plot(stock_data[, -5])
