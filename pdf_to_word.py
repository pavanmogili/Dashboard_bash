#!/usr/bin/env python
# coding: utf-8

# In[1]:
import sys
print(sys.version)

import nltk
import multiprocessing as mp
import pandas as pd

from nltk.corpus import wordnet
from nltk.stem import WordNetLemmatizer


import copy
import re

import Bio
from Bio import Entrez
from Bio import SeqIO

from tqdm import tqdm

import time

import urllib
from xml.etree import ElementTree as ET

import nltk.data
import scispacy
import spacy
from spacy import displacy

#nlp = spacy.load("en_ner_bionlp13cg_md")

Entrez.email = "pmogili@affiniatx.com"
Entrez.tool = "DataMining_POS.ipynb"

tokenizer = nltk.data.load('tokenizers/punkt/PY3/english.pickle')


# In[2]:


from pdfminer.high_level import extract_text


# In[3]:


with open('paper.pdf','rb') as f:
    text = extract_text(f)


# In[4]:



# In[5]:


import os
dic = []
Sentence = tokenizer.tokenize(str(text))

from nltk import tokenize
sent  =  tokenize.sent_tokenize(text)
y=[]
for i in Sentence:
    x = i.split(2*os.linesep)
    y = y+x

Sentence = y
y=[]
for i in Sentence:
    x = re.split(r'\.+[0-9]+[ \t]', i)
    y = y+x
Sentence = y
for i in Sentence:
    temp = {'Sentence':i}
    temp2 = copy.deepcopy(temp)
    dic.append(temp2)
df_new = pd.DataFrame.from_dict(dic)
df_new.drop_duplicates(inplace=True)
df_new.dropna(inplace=True)


# In[6]:




# In[7]:
print("1")

def get_serotypes(df):
    AAV_notMatchVocab = ["aav","aavs","aavr"]
    dic = []
    
    for index, row in tqdm(df.iterrows()):
        tokens = nltk.word_tokenize(str(row["Sentence"]))
        for word in tokens:
            match = re.search("^\w+?\s\d+$", word)
            if ("AAV".lower() in word.lower() and word.lower() not in AAV_notMatchVocab ): #and word.lower() != "AAV".lower() and word.lower() != "AAVs".lower()) or match:
                temp = row
                temp["Notes"] = word
                temp2 = copy.deepcopy(temp)
                dic.append(temp2)
    df_new = pd.DataFrame.from_dict(dic)
    df_new.drop_duplicates(inplace=True)
    df_new.dropna(inplace=True)
    return df_new
df_serotypes = get_serotypes(df_new)


# In[8]:




# In[9]:
print("2")

Amino_acid_List = ["F","L","I","M","V","S","P","T","A","Y","H","Q","N","K","D","E","C","W","R","S","R","G","Phe","Leu","Ile","Met","Val","Ser","Pro","Thr","Ala","Tyr","His","Gln","Asn","Lys","Asp","Glu","Cys","Trp","Arg","Ser","Gly"]
exclution_list=["PKD", "VP","Ac","x"]
location = []
aminoacid = []
dic_test = []
dic_t = []
temp2=[]
tk_temp = ''
tbar = tqdm(total=len(df_serotypes))
df_test=[]
for index, row in df_serotypes.iterrows():
    location = []
    tokens = nltk.word_tokenize(str(row["Sentence"]))
    for tk in tokens:
        
        if tk_temp.lower()!= "figure" and tk_temp.lower()!= "table":
            #print("-" + tk)
            for st in tk.split("-"):
                m1 = re.findall('([A-Za-z]*)(\d+)[^A-Za-z]*$', st)
                amino = re.findall('^(\D*)', st)
                m = re.findall('(\d+)', st)
                if m1:
                    if "AAV".lower() not in m1[0][0].lower() and m1[0][0]!="" and "AAVR" not in m1[0][0] and "AAVDJ" not in m1[0][0] and m1[0][1]!="" and m1[0][0] not in exclution_list and m1[0][0] in Amino_acid_List and int(m1[0][1])<999 and int(m1[0][1])>50:
                       # if m1[0][0]!="AAV" and m1[0][0]!="(AAV" and m1[0][0]!="" and m1[0][0]!="PKD" and int(m1[0][1])>25 and m1[0][0]!="(PKD" and m1[0][0]!="AAVrh.":
                        dic_t = row
                        dic_t["Position"]=m1[0][1]
                        dic_t["Sequence"]=m1[0][0]
                        temp2 = copy.deepcopy(dic_t)
                if list(temp2):            
                    dic_test.append(temp2)
        tk_temp= copy.deepcopy(tk) 
    tbar.update(1)
        
df_test2 = pd.DataFrame.from_dict(dic_test)
df_test2 = df_test2.dropna()
df_test2 = df_test2.drop_duplicates()



print("3")
Serotype = ["AAV1","AAV2","AAV3","AAV5","AAV6","AAV7",
                                "AAV8","AAV9","AAV10","Anc80","Anc81","Anc82",
                                "Anc83","Anc84","Anc94","Anc110","Anc113",
                                "Anc126","Anc127","Anc80L27","Anc80L33",
                                "Anc80L36","Anc80L44","Anc80L59","Anc80L60",
                                "Anc80L62","Anc80L65","Anc80L1","Anc82DI",
                                "AAVhu.37","AAVrh.10","AAVhu.68","LK03"]
location = []
aminoacid = []
dic_test2 = []
dic_t = []
temp2=[]

tbar = tqdm(total=len(df_test2))
for index, row in df_test2.iterrows():
    tbar.update(1)
    location = []
    tokens = nltk.word_tokenize(str(row["Notes"]))
    dic_t = row
    dic_t["Serotype"]=None
    serotypes = []
    for tk in tokens:
        for rec in Serotype:
            if rec.lower() in tk.lower():

                dic_t["Serotype"]=rec    
        temp2 = copy.deepcopy(dic_t)          
        dic_test2.append(temp2)
df_test = pd.DataFrame.from_dict(dic_test2)

     
receptors = ["galactose", "heparan", "AAVR", "sialic" , "acetylation","methylation","glycosylation","phosphorylation",
              "ubiquitination","sumoylation", "hexnacylation"]
location = []
aminoacid = []
dic_test2 = []
dic_t = []
temp2=[]
tbar = tqdm(total=len(df_test))
for index, row in df_test.iterrows():
    location = []
    tokens = nltk.word_tokenize(str(row["Sentence"]))
    for tk in tokens:
        for rec in receptors:
            if rec.lower() in tk.lower() or "HS" in tk or "GAL" in tk or "SIA" in tk:
                if("HS" in tk):
                  rec = "heparan"
                if("GAL" in tk):
                  rec = "galactose"
                if("SIA" in tk):
                  rec = "sialic"
                dic_t = row
                dic_t["Sub_Function"]=rec
                temp2 = copy.deepcopy(dic_t)          
                dic_test2.append(temp2)
    tbar.update(1)
df_test2 = pd.DataFrame.from_dict(dic_test2)
df_test2 = df_test2.dropna()
col_subset = df_test2.columns.tolist()
col_subset.remove("Sentence")
df_test2 = df_test2.drop_duplicates(subset = col_subset, keep = "first").reset_index(drop = True)
        

print("3")
# In[10]:

df_test2.to_csv("paper.csv",index=False)

print("done")





