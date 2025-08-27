# Anbefalt oppgave til Øving 2, ST1201 høst 2025


# Mål: bruke en T-test på et ekte datasett
# Datasett: Iris - så kjent at det har sin egen wikipedia-artikkel: https://en.wikipedia.org/wiki/Iris_flower_data_set
# Datasettet ligger også tilgengelig i R 

data(iris)  # laster inn datasettet

iris[1:5,] # se på de første fem linjene

summary(iris) # litt oppsummerende tall om hver kolonne

iris$Petal.Length[1:10] # de 10 første observasjonene av Petal Lengths
iris$Species[1:10]  # ser at alle de 10 første målingene er for arten Setosa

mean(iris$Petal.Length[iris$Species == "virginica"])  # gjennomsnittslengde innen arten virginica


# Oppgave 1a) Plott et histogram av Petal Length, bare for arten (species) Setosa

# Tips: hist() for histogram

# Oppgave 1b) Du ønsker å teste om forventet Petal Length i arten Setosa er ulik 1.45 cm
# Hva er H0 og H1? Hvilken testobservator skal du bruke? Hvilken fordeling har den når H0 er sann?
# Les hjelpesiden til t-test i r
?t.test

# Oppgave 1c) Gjennomfør testen ved å bruke den innebygde funksjonen t.test

# Oppgave 1d) Gjør de samme utregningene "for hånd". Du kan bruke innebygde funksjoner mean() og var()
# Finn kritisk verdi i tabell. Sjekk at resultatet blir det samme som over

