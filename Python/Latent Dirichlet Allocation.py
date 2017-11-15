from nltk.tokenize import RegexpTokenizer
from stop_words import get_stop_words
from nltk.stem.porter import PorterStemmer
from gensim import corpora, models
import gensim

tokenizer = RegexpTokenizer(r'\w+')

# create English stop words list
en_stop = get_stop_words('en')


# Create p_stemmer of class PorterStemmer
p_stemmer = PorterStemmer()
    
# create sample documents: I used Wal-Mart summary and news headlines as an example....
doc_a = "A top executive at Asda's parent company has pledged greater support to the chain's recovery efforts after admitting it was slow to respond to the challenge posed by discounters in the UK."
doc_b = "A Pew Research Center study found that more than sixty percent of Americans had cut back on spending, and one in three said they planned to spend less after the recession ended."
doc_c = "While Wal-Mart is not a pure discounter, its sheer size has continued to attract bargain-hunters and people on limited incomes."
doc_d = "Wal-Mart Stores, Inc. operates retail stores in various formats worldwide. It operates through three segments: Walmart U.S., Walmart International, and Sams Club. The company operates discount stores, supermarkets, supercenters, hypermarkets, warehouse clubs, cash and carry stores, home improvement stores, specialty electronics stores, apparel stores, drug stores, convenience stores, and membership-only warehouse clubs; and retail Websites, such as walmart.com and samsclub.com. It offers grocery products, including meat, produce, natural and organics, deli and bakery, dairy, frozen foods, alcoholic and nonalcoholic beverages, floral and dry grocery, as well as consumables, such as health and beauty aids, baby products, household chemicals, paper goods, and pet supplies; and health and wellness products, which include pharmacy, optical services, clinical services, over-the-counter drugs, and other medical products"
doc_e = "The company also provides electronics, toys, cameras and supplies, photo processing services, cellular phones, cellular service plan contracts and prepaid service, movies, music, video games, and books; stationery, automotive, hardware and paint, and sporting goods, as well as fabrics, crafts, and seasonal merchandise; apparel for women, girls, men, boys, and infants, as well as shoes, jewelry, and accessories; and home furnishings, housewares and small appliances, bedding, home decor, outdoor living, and horticulture products. The company also provides fuel and financial services and related products, including money orders, prepaid cards, wire transfers, money transfers, check cashing, and bill payment. In addition, it offers brand name merchandise, including hardgoods, softgoods, and selected private-label items, such as Members Mark. As of June 20, 2016, it operated 11,527 stores under 63 banners in 28 countries and e-commerce Websites in 11 countries. Wal-Mart Stores, Inc. was founded in 1945 and is headquartered in Bentonville, Arkansas." 

# compile sample documents into a list
doc_set = [doc_a, doc_b, doc_c, doc_d, doc_e]

# list for tokenized documents in loop
texts = []

# loop through document list
for i in doc_set:
    
    # clean and tokenize document string
    raw = i.lower()
    tokens = tokenizer.tokenize(raw)

    # remove stop words from tokens
    stopped_tokens = [i for i in tokens if not i in en_stop]
    
    # stem tokens
    stemmed_tokens = [p_stemmer.stem(i) for i in stopped_tokens]
    
    # add tokens to list
    texts.append(stemmed_tokens)

# turn our tokenized documents into a id <-> term dictionary
dictionary = corpora.Dictionary(texts)
    
# convert tokenized documents into a document-term matrix
corpus = [dictionary.doc2bow(text) for text in texts]

# generate LDA model
ldamodel = gensim.models.ldamodel.LdaModel(corpus, num_topics=5, id2word = dictionary, passes=20)


print(ldamodel.print_topics(num_topics=5, num_words=3))


print(ldamodel.print_topics(num_topics=2, num_words=4))