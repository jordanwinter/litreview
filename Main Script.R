#Write and conduct naive search
# USING BASE as start 


library(litsearchr)



# IMPORT NAIVE SEARCH

naiveimport <- litsearchr::import_results(directory = "./naive_results/", verbose = T)

# remove dupes

naiveresults <- litsearchr::remove_duplicates(naiveimport, field = "title", method = "string_osa")

#IDENTIFY POTENTIAL KEYWORDS

rakedkeywords <-
  litsearchr::extract_terms(
    text = paste(naiveresults$title, naiveresults$abstract),
    method = "fakerake",
    min_freq = 2,
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )


real_keywords <-
  litsearchr::extract_terms(
    keywords = naiveresults$keywords,
    method = "tagged",
    min_freq = 2,
    ngrams = TRUE,
    min_n = 2,
    language = "English"
  )


all_keywords <- unique(append(taggedkeywords, rakedkeywords))




#### Find IMPORTANT keywords ie coocurance 

#document feature matrix
naivedfm <-
  litsearchr::create_dfm(
    elements = paste(naiveresults$title, naiveresults$abstract),
    features = all_keywords
  )

naivegraph <-
  litsearchr::create_network(
    search_dfm = naivedfm,
    min_studies = 2,
    min_occ = 2
  )

plot_network(graph = naivegraph)


par(las = 1)
plot(
  sort(igraph::strength(naivegraph)),
  ylab = "Node Strength",
  main = "Ranked Node Strengths",
  xlab = "Rank",
  type = "l",
  
)

cutoff <-
  litsearchr::find_cutoff(
    naivegraph,
    method = "cumulative",
    percent = .80,
    imp_method = "strength"
  )

reducedgraph <-
  litsearchr::reduce_graph(naivegraph, cutoff_strength = cutoff[1])

searchterms <- litsearchr::get_keywords(reducedgraph)
