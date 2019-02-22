
# Part 1 ------------------------------------------------------------------
# Eggs produced

# import the data
df <- read.csv('eggs_produced.csv')

# boxplot of data
boxplot(Number_of_eggs ~ group, data = df)

# create linear model
model <- lm(Number_of_eggs ~ group, data = df)

#check the residuals with the q-q plot
plot(model, which = 2)

# anova 
anova(model)


# Part 2 ------------------------------------------------------------------
# Eggs hatching

# import the data
df <- read.csv('eggs_hatching.csv')

# boxplot of data
boxplot(Proportion_hatched ~ group, data = df)

# create linear model
model <- lm(Proportion_hatched ~ group, data = df)

#check the residuals with the q-q plot
plot(model, which = 2)

# anova 
anova(model)

# check differences between groups
TukeyHSD(aov(model))


# Part 3 ------------------------------------------------------------------
# Offspring produced
df_1 <- read.csv('eggs_produced.csv')
df_2 <- read.csv('eggs_hatching.csv')

n_offspring <- df_1$Number_of_eggs * df_2$Proportion_hatched
group <- df_1$group

df <- data.frame('n_offspring' = n_offspring, 'group' = group)

boxplot(n_offspring ~ group, data = df)

model <- lm(n_offspring ~ group, data = df)

plot(model, which = 2)

anova(model)

TukeyHSD(aov(model))


# 3 boxplots --------------------------------------------------------------
par(mfrow = c(1, 3))

df <- read.csv('eggs_produced.csv')
boxplot(Number_of_eggs ~ group, data = df, col = 'steelblue', main = "Eggs laid")

df <- read.csv('eggs_hatching.csv')
boxplot(Proportion_hatched ~ group, data = df, col = 'tomato', main = "Eggs hatched")

df_1 <- read.csv('eggs_produced.csv')
df_2 <- read.csv('eggs_hatching.csv')
n_offspring <- df_1$Number_of_eggs * df_2$Proportion_hatched
group <- df_1$group
df <- data.frame('n_offspring' = n_offspring, 'group' = group)
boxplot(n_offspring ~ group, data = df, col = "chartreuse3", main = "Fitness")
