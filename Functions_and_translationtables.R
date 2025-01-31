##Functions and reference tables

# Create a function to extract and combine the desired columns
combine_with_origin <- function(data, origin_label) {
  data %>%
    dplyr::select(
      Q1,
      Q3,
      Q5,
      Q6,
      Q7,
      Q11,
      matches("^Q\\.13\\.20_"),  # Matches all parts of Q13-Q20
      Q22,
      matches("^Q\\.23\\.38_"),  # Matches all parts of Q23-Q37
      Q39,
      matches("^Q\\.41\\.56_")   # Matches all parts of Q41-Q56
    ) %>%
    mutate(dataOrigin = origin_label) %>%
    mutate(Q5 = as.numeric(Q5))
}

# Define a lookup table for the translations for question 7
Q7response_map <- c(
  "I have never heard of it." = 1,
  "I have heard of it but I do not use it." = 2,
  "I have used GenAI." = 3,
  "Nunca he oído hablar de ello." = 1,
  "He oído hablar de ello, pero no lo uso." = 2,
  "He usado GenAI." = 3,
  "Ich habe noch nie davon gehört." = 1,
  "Ich habe davon gehört, aber ich benutze es nicht." = 2,
  "Ich habe GenAI verwendet." = 3,
  "我從未聽說過它。" = 1,
  "我聽說過，但我不使用它。" = 2,
  "我使用過 GenAI。" = 3
)

# Function to fit ordinal logistic regression by group
fit_model_by_group <- function(data, group, QNumber=Q5) {
  model <- polr(as.factor(Q7_numeric) ~ QNumber, data = data, Hess = TRUE)
  coeff <- coef(summary(model))
  p_values <- pnorm(abs(coeff[, "t value"]), lower.tail = FALSE) * 2
  coeff <- cbind(coeff, "p value" = p_values)
  list(model = model, coefficients = coeff)
}

# Define mapping for standardization of level in Q3
Q3standardized_map <- list(
  "Primary/Elementary School" = c("Primary/Elementary School", "Escuela primaria", "Grundschule", "小學"),
  "Middle School" = c("Middle School", "Escuela media", "Mittelschule", "中學"),
  "Secondary school/High School" = c("Secondary school/High School", "Escuela secundaria", "Sekundarschule/Gymnasium", "高中"),
  "Post high school/University Level" = c("Post high school/University Level", "Nivel universitario", "大學"),
  "Preschool" = c("Preescolar", "幼兒園")
)

# Function to standardize levels
standardize_levels <- function(value) {
  for (level in names(Q3standardized_map)) {
    if (value %in% Q3standardized_map[[level]]) {
      return(level)  # Return the matched level name
    }
  }
  return(NA)  # Return NA if no match is found
}

# Define a mapping for Q11 standardization
Q11_standardized_map <- list(
  "Very accessible" = c(
    "Very accessible (e.g., I can access and use generative AI tools whenever I want to)",
    "Muy accesibles (por ejemplo, puedo acceder y usar las herramientas de IA generativa cuando quiera)",
    "Sehr zugänglich (z.B. Ich kann generative KI-Tools jederzeit verwenden)",
    "非常可訪問（例如，我可以隨時訪問和使用生成式人工智能工具）"
  ),
  "Somewhat accessible" = c(
    "Somewhat accessible (e.g., I can get access to generative AI, but it isn’t always easy)",
    "Algo accesibles (por ejemplo, puedo acceder a la IA generativa, pero no siempre es fácil)",
    "Eingeschränkt  zugänglich (z.B. Ich kann auf generative KI zugreifen, aber es ist nicht immer einfach)",
    "有些可訪問（例如，我可以訪問生成式人工智能，但並不總是容易）"
  ),
  "Not accessible" = c(
    "Not accessible at all (e.g., it is very difficult or impossible for me to get access to generative AI tools)",
    "No accesibles (por ejemplo, es muy difícil o imposible acceder a herramientas de IA generativa)",
    "Überhaupt nicht zugänglich (z.B. es ist sehr schwierig oder unmöglich, auf generative KI-Tools zuzugreifen)",
    "完全不可訪問（例如，我很難或不可能訪問生成式人工智能工具）"
  )
)

# Standardization function
standardize_Q11 <- function(value) {
  for (level in names(Q11_standardized_map)) {
    if (value %in% Q11_standardized_map[[level]]) {
      return(level)
    }
  }
  return(NA)  # For missing or unrecognized entries
}

# Mapping for numeric conversion
Q13_to_20_map <- list(
  "5" = c("5- Very much so", "5- Sehr gut", "5 - Mucho", "5- 是", "5 - 非常"),
  "4" = c("4-Yes", "4- Gut", "4 - Sí", "4- 是", "4 - 很好"),
  "3" = c("3 - Moderately", "3 - Mäßig", "3 - Moderadamente", "3 - 中等程度"),
  "2" = c("2 - Slightly", "2 - Leicht", "2 - Ligeramente", "2 - 略知一二"),
  "1" = c("1 - Not at all", "1 - Gar nicht", "1 - Para nada", "1 - 完全不了解")
)

Q13convert_to_numeric <- function(value) {
  for (num in names(Q13_to_20_map)) {
    if (value %in% Q13_to_20_map[[num]]) {
      return(as.numeric(num))
    }
  }
  return(NA)  # Return NA for missing or unrecognized entries
}

#function from Jonathan Bakker
pairwise.adonis2 <- function(resp, fact, p.method = "none", nperm = 999) {
  require(vegan)
  resp <- as.matrix(resp)
  fact <- factor(fact)
  fun.p <- function(i, j) {
    fact2 <- droplevels(fact[as.numeric(fact) %in% c(i, j)])
    index <- which(fact %in% levels(fact2))
    resp2 <- as.dist(resp[index, index])
    result <- adonis2(resp2 ~ fact2, permutations = nperm)
    result$`Pr(>F)`[1]
  }
  multcomp <- pairwise.table(fun.p, levels(fact), p.adjust.method = p.method)
  return(list(fact = levels(fact), p.value = multcomp, p.adjust.method = p.method))
} 

frequency_map <- list(
  "Daily" = c("Daily", "Diariamente", "每天"),
  "Weekly" = c("Weekly", "Semanalmente", "Wöchentlich", "每週"),
  "Monthly" = c("Monthly", "Mensualmente", "Monatlich", "每月")
)

# Revised function to standardize frequency levels
standardize_frequency <- function(value) {
  for (level in names(frequency_map)) {
    if (value %in% frequency_map[[level]]) {
      return(level)
    }
  }
  return(NA)  # Return NA if the value doesn't match
}

# Mapping for numeric conversion
Q23_1_15_map <- list(
	"1" = c("1 = Para nada", "1= Gar nicht", "1=To no extent", "1=完全不使用"),
    "2" = c("2 = En poca medida", "2= To little extent", "2= Wenig", "2= 很少使用"),
    "3" = c("3 = En alguna medida", "3= Etwas", "3= 一些使用", "3=To some extent"),
    "4" = c("4 = En gran medida", "4= Viel", "4= 大量使用", "4=To a large extent"),
    "5" = c("5 = Con mucha frecuencia", "5= Sehr viel", "5= 非常大量使用", "5=To a very large extent")
    )

Q23convert_to_numeric <- function(value) {
  for (num in names(Q23_1_15_map)) {
    if (value %in% Q23_1_15_map[[num]]) {
      return(as.numeric(num))
    }
  }
  return(NA)  # Return NA for missing or unrecognized entries
}

#Q39 Translation map
# Updated Translation Map
translation_map <- list(
  "Talleres" = "Workshops",
  "Programa de preparación de profesores" = "Teacher preparation program",
  "Colegas" = "Colleagues",
  "Estudiantes/Niños" = "Students/Children",
  "Internet (por ejemplo, Google, YouTube)" = "Internet (e.g., Google, YouTube)",
  "Experimentación" = "Experimentation",
  "Aún no he aprendido cómo usar la inteligencia artificial generativa en mi aula." = 
    "I have not yet learned about how to use generative AI  in my classroom.",
  "Otro (permita escribir la respuesta)" = "Other",
  "Lernende/Kinder" = "Students/Children",
  "Kollegen/Kolleginnen" = "Colleagues",
  "Lehrerausbildung" = "Teacher preparation program",
  "Internet (z.B. Google, YouTube)" = "Internet (e.g., Google, YouTube)",
  "Experimentieren" = "Experimentation",
  "Ich habe noch nicht gelernt, wie man generative KI in meinem Unterricht einsetzt." = 
    "I have not yet learned about how to use generative AI  in my classroom.",
    " Ich habe noch nicht gelernt, wie man generative KI in meinem Unterricht einsetzt." = 
    "I have not yet learned about how to use generative AI  in my classroom.",
  "Andere (bitte eintragen)" = "Other",
  "工作坊" = "Workshops",
  "教師培訓計劃" = "Teacher preparation program",
  "同事" = "Colleagues",
  "學生/孩子" = "Students/Children",
  "互聯網（例如，Google，YouTube）" = "Internet (e.g., Google, YouTube)",
  "實驗" = "Experimentation",
  "我尚未學會如何在課堂上使用生成式人工智能。" = "I have not yet learned about how to use generative AI  in my classroom."
)


# Function to translate responses with enhanced handling for commas and semicolons
# Enhanced Translation Function
Q39translate_response <- function(response) {
  if (is.na(response) || response == "") {
    return(NA)
  }
  
  # Special case for the German response with commas
  special_case <- "Ich habe noch nicht gelernt, wie man generative KI in meinem Unterricht einsetzt."
  if (response == special_case) {
    return("I have not yet learned about how to use generative AI in my classroom.")
  }
  
  # Split the response into components safely
  parts <- str_split(response, ",(?!(?:[^()]*\\)))")[[1]]  # Split at commas outside parentheses
  # Translate each part and clean spaces
  translated_parts <- sapply(parts, function(part) {
    part <- str_trim(part) # Remove whitespace
    translation_map[[part]] %||% part # Use translation or keep original
  })
  
  # Combine translated components
  paste(translated_parts, collapse = ", ")
}

PDresponse_map <- list(
  "5=To a very large extent" = 5,
  "4=To a large extent" = 4,
  "3=To some extent" = 3,
  "2=To little extent" = 2,
  "1=To no extent" = 1,
  "4 = En gran medida" = 4,
  "3 = En alguna medida" = 3,
  "1 = Para nada" = 1,
  "2 = En poca medida" = 2,
  "5 = En grande medida" = 5,
  "1= Gar nicht" = 1,
  "3= Etwas" = 3,
  "4= Viel" = 4,
  "2= Wenig" = 2,
  "4= 大量" = 4,
  "3= 一些" = 3,
  "2= 很少" = 2,
  "empty_string" = NA  # Placeholder for empty strings
)
