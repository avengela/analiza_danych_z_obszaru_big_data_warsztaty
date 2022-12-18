# zadanie 1
# Stwórz wektor złożony z następujących liczb: 2, 6, 8, 6, 4, 3

v1 <- c(2, 6, 8, 6, 4, 3)


# Zadanie 2
# Stwórz wektor złożony z kolejnych liczb naturalnych od 1 do 100

v2 <- seq(1, 100)

# Zadanie 3
# Stwórz wektor złożony z kolejnych parzystych liczb naturalnych od 2 do 100

v3 <- seq(2, 100, by = 2)


# Zadanie 4
# Stwórz wektor złożony z kolejnych liczb nieparzystych od 99 do 47

v4 <- seq(99, 47, by = -2)

# Zadanie 5
# Stwórz wektor złożony z kolejnych nazw miesięcy w języku angielskim

v5 <- month.name

# Zadanie 6
# Stwórz wektor złożony z kolejnych liter alfabetu łacińskiego
  
v6 <- letters

# Zadanie 7
# Wygeneruj wektor kolejnych liczb parzystych 2, 4, ..., 20. W wektorze tym nie powinno być liczb 6 oraz 10. Innymi słowy wygeneruj wektor 2,4, 8, 12, 14, 16, 18, 20.

v7 <- seq(2, 20, by = 2)
v7 <- v7[v7 != 6 & v7 != 10]

# Zadanie 8
# Z wektora kolejnych miesięcy pobierz miesiące należące do II oraz IV kwartału.

v8 <- c(v5[4:6], v5[10:12])

# Zadanie 9
# Student otrzymał następujące oceny z przedmiotów A, B, C i D: A-3, B-4, C-5, D-5. Stwórz wektory przechowujący oceny uzyskane przez studenta wraz z nazwami przedmiotów.

oceny <- c(3, 4, 5, 5)
przedmioty <- c("A", "B", "C", "D")

v9 <- paste(przedmioty, oceny, sep = "-")

# Zadanie 10
# Wygeneruj wektor złożony z kolejnych nazw miesięcy (po angielsku) i zastąp nazwy trzech pierwszych miesięcy ich polskimi odpowiednikami.

v10 <- v5
v10[1:3] <- c("styczeń", "luty", "marzec")

# Zadanie 11
# Zapisz w postaci wektora wiek kolejnych 10 osób: 25, 35, 43, 28, 47, 77, 55, 66, 25, 25. Dla tak stworzonego wektora oblicz następujace statystyki opisowe: minimum, maksimum, średnia, odchylenie standardowe, rozstęp, kwartyle, kurtoza, współczynnik skośności.

library(moments)
wiek <- c(25, 35, 43, 28, 47, 77, 55, 66, 25, 25)

min_wiek <- min(wiek)
max_wiek <- max(wiek)
mean_wiek <- mean(wiek)
sd_wiek <- sd(wiek)
range_wiek <- range(wiek)
quantiles_wiek <- quantile(wiek)
kurtosis_wiek <- kurtosis(wiek)
skewness_wiek <- skewness(wiek)

# Zadanie 12
# Wygeneruj w R 1000 liczb z rozkładu normalnego dla zmiennej losowej X opisującej wzrost dorosłych osób( średnia 176 cm i odchylenie standardowe 9 cm). Dla tak utworzonego wektora oblicz następujące statystyki opisowe: minimum, maksimum, średnia, odchylenie standardowe, rozstęp, kwartyle. kurtoza, współczynnik skośności (asymetrii). Stwórz histogram wzrostu.
library(ggplot2)

x <- rnorm(1000, mean = 176, sd = 9)

min_x <- min(x)
max_x <- max(x)
mean_x <- mean(x)
sd_x <- sd(x)
range_x <- range(x)
quantiles_x <- quantile(x)
kurtosis_x <- kurtosis(x)
skewness_x <- skewness(x)

ggplot(data = data.frame(x), aes(x = x)) + 
  geom_histogram(bins = 30)

# Zadanie 13
# Stwórz wektor a z liczbami od 100 do 103 oraz wektor b z liczbami od 4 do 7. Utwórz nowy wektor d będący połączeniem wektora b i a ( w takiej kolejności). Wyświetl go.

a <- 100:103
b <- 4:7

d <- c(b, a)
print(d)

# Zadanie 14
# Stwórz wektor imię zawierający imiona trójki Twoich przyjaciół. Potem stwórz wektor wiek zawierający kolejno wiek Twojej trójki przyjaciół. Następnie stwórz ramkę danych osób złożoną z wektorów imię oraz wiek. Wylicz średnią wieku przyjaciół.

imie <- c("Anna", "Piotr", "Katarzyna")
wiek <- c(26, 34, 29)

osoby <- data.frame(imie, wiek)
sredni_wiek <- mean(osoby$wiek)

# Zadanie 15
# Utwórz wektor złożony z kolejnych liczb naturalnych 1, 2, 3, ... 1000. Nastepnie elementy tego wektora, które są mniejsze od 10 lub większe od 90, przemnoż przez 5, a pozostałe elementy podziel przez 5.

v15 <- 1:1000

v15[v15 < 10 | v15 > 90] <- v15[(v15 < 10 | v15 > 90)] * 5
v15[!(v15 < 10 | v15 > 90)] <- v15[!(v15 < 10 | v15 > 90)] / 5

# Zadanie 16
# Utwórz wektor postaci: 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3, 1, 2, 3.

v16 <- rep(1:3, 5)


# Zadanie 17
# Utwórz wektor zawierajacy Twoje imię 100 razy.

v17 <- rep("Angelika", 100)

# zadanie 18
# Wygeneruj wektor 20 liczb w równych odstępach między 1 a 100.

v18 <- seq(1, 100, length.out = 20)

# Zadanie 19
# Utwórz wektor zawierający jedna jedynkę, dwie dwójki, ..., dziewięć dziewiątek.

v19 <- rep(1:9, 1:9)

# Zadanie 20 
# Wygeneruj sekwencję zawierającą osiem czwórek, następnie siedem szóstek i na koniec dziewięć trójek. Zapisz ją w macierzy M o sześciu wierszach i czterech kolumnach.

v20 <- rep(c(4, 6, 3), c(8, 7, 9))
M <- matrix(v20, nrow = 6, ncol = 4)

# Zadanie 21
# Utwórz wektor liczb naturalnych od 1 do 1000, a następnie zamień liczby parzyste na ich odwrotności.

v21 <- 1:1000
v21[v21%%2 == 0] <- 1 / v21[v21 %% 2 == 0] 

# Zadanie 22
# Skonstruuj wektor x używajac ponizszego kodu: X <- c(NA, 3, 14, NA, 33, 17, NA, 41). Zlicz liczbę braków, wyznacz średnią arytmetyczną bez braków, usuń braki danych, zastąp braki liczbą 11.

v22 <- c(NA, 3, 14, NA, 33, 17, NA, 41)

num_NA <- sum(is.na(v22))

mean_without_NA <- mean(v22, na.rm = TRUE)

x_without_NA <- na.omit(v22)

num_instead_NA <- ifelse(is.na(v22), 11, v22)


# Zadanie 23
# Walec o wysokości h oraz promieniu podstawy r ma objętość V = pir^2h oraz pole powierzchni P = 2pir(r+h). Dla długości promienia 1:5 oraz wysokości 4:8 oblicz odpowiednie objętości i pola powierzchni. Skonstruuj ramkę danych o kolumnach r, h, V, P.

results <- data.frame()

for (r in 1:5) {
  for (h in 4:8) {
    V <- pi * r^2 * h
    P <- 2 * pi * r * (r + h)
    results <- rbind(results, data.frame(r, h, V, P))
  }
}

# Zadanie 24
# Wygeneruj z rozkładu normalnego N(5000,500) 1000 liczb (x-dochód). Zmienną y (wydatki) utwórz w następujący sposób: y=0.2x + 200 + c, gdzie c ~ N(300,50). Wyznacz współczynnik korelacji liniowej Pearsona pomiędzy dochodem a wydatkami. Sporządź korelacyjny diagram rozrzutu pomiędzy tymi dwiema zmiennymi.

x <- rnorm(1000, mean = 5000, sd = 500)

c <- rnorm(1000, mean = 300, sd = 50)
y <- 0.2 * x + 200 + c

correlation <- cor(x, y)

plot(x, y, xlab = "Dochód", ylab = "Wydatki")
abline(lm(y ~ x))

# Zadanie 25
# Dla zmiennej dochód z zadania 24 utwórz wykres typu boxplot (pudełkowy)

boxplot(x, xlab = "Dochód", ylab = "Zakres wartości")

