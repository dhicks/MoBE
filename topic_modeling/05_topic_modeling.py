import json
import numpy as np
import gensim
import pandas as pd
import seaborn as sns
#%matplotlib inline


## Load JSON file with text data
abstracts_file = '../../Eisen-data/04_abstracts.json'
with open(abstracts_file) as fp:
    abstracts = json.load(fp)
abstracts = pd.DataFrame(abstracts)
## Remove empty abstracts
abstracts = abstracts[-pd.isnull(abstracts['abstract'])].reset_index(drop=True)
# abstracts_2008 = abstracts[pd.to_datetime(abstracts['date']) <= pd.to_datetime('2008')]

# In[6]: len(abstracts_2008)
# Out[6]: 3552


## --------------------
## Dictionaries to map between df index and Scopus ID
doc_id_dict = dict(zip(abstracts['scopus_id'], abstracts.index))
id_doc_dict = dict(zip(abstracts.index, abstracts['scopus_id']))

## Extract author metadata
authors = pd.DataFrame({'auid': [value for row in abstracts['auids'] for value in row]})\
            .drop_duplicates()\
            .sort_values('auid')\
            .set_index('auid')
# In[12]: len(authors)
# Out[12]: 551
# authors_2008 = pd.DataFrame({'auid': [value for row in abstracts_2008['auids'] for value in row]})\
#             .drop_duplicates()\
#             .sort_values('auid')\
#             .set_index('auid')
# In[14]: len(authors_2008)
# Out[14]: 333


known_authors_file = '../../Eisen-data/00_author_list.xlsx'
known_authors = pd.read_excel(known_authors_file)
known_authors['auid'] = [url.split('=')[1] for url in known_authors['scopus_page']]
known_authors = known_authors.set_index('auid')[['group', 'id', 'surname', 'given_name']]

authors = authors.join(known_authors)

## Authors-papers mapping
authors['papers'] = [abstracts[[auid in entry for entry in abstracts['auids'].tolist()]]['scopus_id'].tolist() for auid in authors.index]
authors['paper_n'] = [len(row) for row in authors['papers']]


## --------------------
## Build corpus as BOW
abstracts_text = abstracts['abstract'].tolist()
## Split abstracts and convert to lowercase
abstracts_text = [doc.lower().split() for doc in abstracts_text]
## Bag-of-words
dictionary = gensim.corpora.Dictionary(abstracts_text)
# Remove rare and common tokens.
# Filter out words that occur too frequently or too rarely.
dictionary.filter_extremes(no_below=30, no_above=.8)

abstracts_bow = [dictionary.doc2bow(doc) for doc in abstracts_text]
## Initialize `dictionary.id2token`
_ = dictionary[0]
## `dictionary.id2token` is also 3. id2word

## doc2author
docs_2_authors = abstracts['auids'].to_dict()

## Print corpus statistics
print(f'Papers: {len(abstracts_bow)}\nTerms: {len(dictionary)}\nAuthors: {len(authors)}')


## --------------------
## Fit the model
print('Fitting author-topic model')
# %time
model = gensim.models.atmodel.AuthorTopicModel(corpus = abstracts_bow, \
                                                     doc2author = docs_2_authors, \
                                                     id2word = dictionary.id2token, \
                                                     num_topics = 3, \
                                                     alpha = 'auto', \
                                                     random_state = 42)
print('Finished fitting author-topic model')


## --------------------
## Extract author-topic distribution
gamma = model.state.gamma[[model.author2id[auid] for auid in authors.index], :]
gamma = gamma / gamma.sum(axis = 1)[:, np.newaxis]
## Convert array to dataframe
author_topics = pd.concat([pd.DataFrame({'auid': authors.index}),
                           pd.DataFrame(gamma).rename(columns=lambda x: 'topic_' + str(x))],
                          axis = 1)\
    .set_index('auid')
author_topics = author_topics.join(authors)

known_author_topics = author_topics[-pd.isnull(author_topics['group'])]


## Plots
sns.pairplot(data = known_author_topics, hue = 'group')
sns.swarmplot(x = 'group', y = 'topic_2', data = author_topics)

print(known_author_topics[(known_author_topics['topic_2'] < .7) & (known_author_topics['group'] == 'building')])
print(known_author_topics[(known_author_topics['topic_2'] > .7) & (known_author_topics['group'] == 'microbial')])

author_topics.to_csv('../../Eisen-data/05-lda.csv')
