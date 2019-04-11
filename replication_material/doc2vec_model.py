#Import all the dependencies
from gensim.models.doc2vec import Doc2Vec, TaggedDocument
import pandas as pd
import gensim
from gensim.models.phrases import Phrases, Phraser
import numpy as np

temp_data = pd.read_csv("~/Dropbox (Personal)/UofT/canadian/essay2/file_for_doc2vec_v2.csv")

temp_data = temp_data

data = temp_data.speechtext.tolist()

tokenized_corpus = [gensim.utils.simple_preprocess(i,min_len=4) for i in data]

bigrams = Phrases(tokenized_corpus, min_count=10, threshold=1)  # train model
tokenized_corpus = bigrams[tokenized_corpus]

#tagged_data = [TaggedDocument(words=_d, tags=[str(i)]) for i, _d in enumerate(tokenized_corpus)]
tagged_data = [TaggedDocument(words=_d, tags=[temp_data.speakerparty2[i],temp_data.speakername[i]]) for i, _d in enumerate(tokenized_corpus)]

max_epochs = 10
vec_size = 50
alpha = 0.025

model = Doc2Vec(vector_size=vec_size,
                alpha=alpha, 
                min_alpha=0.00025,
                min_count=5,
                dm = 1)
  
model.build_vocab(tagged_data)

for epoch in range(max_epochs):
    print('iteration {0}'.format(epoch))
    model.train(tagged_data,
                total_examples=model.corpus_count,
                epochs=model.iter)
    # decrease the learning rate
    model.alpha -= 0.0002
    # fix the learning rate, no decay
    model.min_alpha = model.alpha

# visualize embeddings

words = model.wv.index2word
wvs = model.wv[words]
wvsdf = pd.DataFrame(wvs)
wvsdf['word'] = words
wvsdf.to_csv("/Users/vpl_001/Dropbox (Personal)/UofT/canadian/essay2/replication_material/saved_models/word_embeddings.csv", index = False)

documents = model.docvecs.index2entity

result_array = np.zeros((len(model.docvecs),vec_size))

for k in (range(len(model.docvecs))):
    result_array[k,] = model.docvecs[k]

docsdf = pd.DataFrame(result_array) 

docsdf['document'] = documents

docsdf.to_csv("/Users/vpl_001/Dropbox (Personal)/UofT/canadian/essay2/replication_material/saved_models/doc_embeddings.csv", index = False)



