###
library("glmnet")
library("ggpmisc")

##### 读取数据
data=read.csv('PM_lasso_input.csv')

##### 查看数据
dim(data)
data[1:5,1:5]

##### 按照7:3的比例随机拆分训练集和测试集
ind <- sample(c(1,2), nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1, ]
test <- data[ind==2, ]

##### 查看训练集和测试集
dim(train)
dim(test)

##### 对每一种表型分别使用训练集训练模型并用测试集进行模型测试
for (n in 215:222){
        ### 使用训练集训练模型
        # 提取自变量和因变量。前214列的214种代谢物为因变量，215列-222列的7种表型数据分别为因变量
        x <- as.matrix(train[, 1:214])
        y <- as.matrix(train[, n])
        # x[is.na(x)]=0
        # y[is.na(y)]=0

        # 提取表型名称
        traits=colnames(data)[n]
        # 使用十折交叉验证训练模型
        cv.fit <- cv.glmnet(x,y,alpha = 1,nfolds = 10)
        # 查看合适的拉马达值，并可视化训练的模型
        cv.fit
        cv.fit$lambda.min
        cv.fit$lambda.1se

        pdf(file=paste(traits,'cv.fit.pdf',sep="_"))
        plot(cv.fit)
        dev.off()

        # 提取模型特征与系数
        tmp_coeffs <- coef(cv.fit, s = "lambda.min")
        output_coef=data.frame(features = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)
        output_coef<-output_coef[order(output_coef[,"coefficient"],decreasing=TRUE),]
        output_coef
        # 统计模型特征数目
        length(output_coef$features)

        ### 使用训练集合检验模型的预测效果
        # 使用模型预测
        y_predicted <- predict(cv.fit,s=cv.fit$lambda.min,newx=x)
        # 合并实际值与预测值
        re=cbind(y ,y_predicted)
        re=as.data.frame(re)
        head(re)
        
        # 设置小数显示科学计数法
        options(scipen = 200)
        options(digits=5)
        # 使用线性回归来拟合实际值与预测值，评估模型预测效果
        fit=lm(re$s1~re$V1)
        summary(fit)
        ### 可视化训练集预测效果
        my.formula <- y ~ x

        pdf(file=paste(traits,'model.evulated.train.pdf',sep="_"))
        ggplot(data = re, aes(x = re$s1, y = re$V1)) +
                geom_smooth(method = "lm", 
                        se=FALSE, color="black", 
                        formula = my.formula) +
                stat_poly_eq(formula = my.formula, 
                        aes(label = paste(..eq.label.., 
                                ..rr.label.., 
                                sep = "~~~")), 
                                parse = TRUE) +         
                geom_point(size=4,
                        alpha=0.7)+
                geom_smooth(method = "lm",
                        color="black",
                        fill="lightgray",
                        alpha=.7,
                        size=0.8,se=T,
                        formula = y~x)+
                theme_bw()+
                theme(panel.grid = element_blank())+
                labs(x=paste(traits,'predicted',sep='_'),y=paste(traits,'real',sep='_'),title=paste(traits,'train',sep='_'))
        
        dev.off()

        ### 使用测试集测试来评估模型预测效果
        x <- as.matrix(test[, 1:214])
        y <- as.matrix(test[, n])

        # 
        y_predicted <- predict(cv.fit,s=cv.fit$lambda.min,newx=x)
        re=cbind(y ,y_predicted)
        re=as.data.frame(re)
        fit=lm(re$s1~re$V1)
        summary(fit)

        pdf(file=paste(traits,'model.evulated.test.pdf',sep="_"))
        ggplot(data = re, aes(x = re$s1, y = re$V1)) +
                geom_smooth(method = "lm", 
                        se=FALSE, color="black", 
                        formula = my.formula) +
                stat_poly_eq(formula = my.formula, 
                        aes(label = paste(..eq.label.., 
                                ..rr.label.., 
                                sep = "~~~")), 
                                parse = TRUE) +         
                geom_point(size=4,
                        alpha=0.7)+
                geom_smooth(method = "lm",
                        color="black",
                        fill="lightgray",
                        alpha=.7,
                        size=0.8,se=T,
                        formula = y~x)+
                theme_bw()+
                theme(panel.grid = element_blank())+
                labs(x=paste(traits,'predicted',sep='_'),y=paste(traits,'real',sep='_'),title=paste(traits,'test',sep='_'))
        
        dev.off()

}
