child2 = c()
parent2 = c()
year2 = c()
country2 = c()
school2 = c()
thesis2 = c()

for (i in 1:nrow(statGeneal)){
  child2[i] = iconv(statGeneal[i,]$child, "", "UTF-8")
  parent2[i] = iconv(statGeneal[i,]$parent, "", "UTF-8")
  year2[i] = as.numeric(iconv(statGeneal[i,]$year, "", "UTF-8"))
  country2[i] = iconv(statGeneal[i,]$country, "", "UTF-8")
  school2[i] = iconv(statGeneal[i,]$school, "", "UTF-8")
  thesis2[i] = iconv(statGeneal[i,]$thesis, "", "UTF-8")  
}

statGeneal2 = data.frame(child = child2, parent = parent2, year = year2, country = country2, school = school2, thesis = thesis2)

statGeneal2$child <- as.character(statGeneal2$child)
statGeneal2$parent <- as.character(statGeneal2$parent)
statGeneal2$country <- as.character(statGeneal2$country)
statGeneal2$school <- as.character(statGeneal2$school)
statGeneal2$thesis <- as.character(statGeneal2$thesis)

