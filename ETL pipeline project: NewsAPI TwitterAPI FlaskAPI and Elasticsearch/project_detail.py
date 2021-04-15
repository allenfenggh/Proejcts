#!/usr/bin/env python
# coding: utf-8

# In[2]:


#!pip install newsapi-python
# !pip install twitter


# ## Load SP500 Company data to elasticsearch

# In[2]:


import json 
with open ('sp500.json') as f:
    company = json.load(f)    
cominfo = company['sp500']

from elasticsearch import Elasticsearch
client = Elasticsearch()
for i in cominfo:
    client.index(index='company', body=i)


# In[361]:


## NewsAPI key
### 9f633db6b8124495a82c360e4513cc9d


# ## Newsapi
# ### Extract news top 5 most popular news from 128 sources and transform to list of json objects

# In[3]:


def newsapi(keyword,now):
    import twitter, re, datetime, pandas as pd
    from datetime import timedelta
    import requests
    import os
    import json
    from pprint import pprint
    from newsapi import NewsApiClient
    # initial
    newsapi = NewsApiClient(api_key='')
    # set start time
    # now = datetime.datetime.now().date()-timedelta(days=5); #04-06
    # set end time to yesterday
    day_before_now = now-timedelta(days=1)
    # set source: all 128 sources
    sources = newsapi.get_sources()
    z = [i['id'] for i in sources['sources']]
    sour = ','.join(z)

    # api: get top 5 most popular news from all 128 sources
    all_articles = newsapi.get_everything(
        qintitle=keyword,
        sources=sour,
        from_param=now,
        to= day_before_now,
        language='en',
        sort_by='popularity',
        page=1,
        page_size=5
    )

    
    arjson=all_articles['articles']
    
    # extract useful content from entire json data
    newsclean = []
    for i in arjson:
        newsclean.append({"title": i['title'],
                      "url": i['url'],
                      "publishedAt": i['publishedAt'],
                      "description": i['description']
                     })
    return newsclean


# ## Simulation
# ### Simulate 20days' data for Tesla and Apple
# ### Extract daily stock price and news then extract and combine useful information into one json object and load into elasticsearch for each day

# In[4]:


for com in ['tesla', 'apple']:
   for i in range(14,34):
       import datetime
       from datetime import timedelta
       company =  com
       now = datetime.datetime.now().date()-timedelta(days=i); #04-08
       # get news
       ne = newsapi(company,now)
       # pprint(ne)

       # load stock price data
       import pandas as pd
       path = company+'.csv'
       stock = pd.read_csv(path)
       # get stock price: Date, Open, High, Low, Close, Adj Close, Volume
       st = stock[stock['Date']==str(now)]

       finjson = []
       finjson.append({'Date': now,
                       'Company': company,
                       'News': ne,
                       'Stock':{'Open':st['Open'],
                                'High':st['High'],
                                'Low':st['Low'],
                                'Close':st['Close'],
                                'Adj Close':st['Adj Close'],
                                'Volume':st['Volume'] 
                       }})

       from elasticsearch import Elasticsearch
       client = Elasticsearch()
       for d in finjson:
           client.index(index='daily', body=d)


# In[6]:


# from elasticsearch import Elasticsearch
# client = Elasticsearch()
# for d in finjson:
#     client.index(index='daily', body=d)


# In[7]:


# from pprint import pprint
# pprint(arjson)


# In[8]:


# from elasticsearch import Elasticsearch
# client = Elasticsearch()
# for n in newsclean:
#     client.index(index='news', body=n)


# ## Twitter API
# ### Exatract most recent 10 tweets from a company official account

# In[104]:


def twitterapi(keyword):
    # initial
    import tweepy
    import datetime
    from datetime import timedelta
    consumer_key = ''
    consumer_secret = ''
    access_token =  '' 
    access_token_secret = '' 

    auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
    auth.set_access_token(access_token, access_token_secret)
    api = tweepy.API(auth)
    
    # set start time
    now = datetime.datetime.now()
    # set end time
    week_before_now = now-timedelta(days=7)
    # api
    public_tweets = tweepy.Cursor(api.user_timeline,id=keyword,until=now, 
                                  since=week_before_now,exclude_replies=True).items(10)
    
    data = []
    for item in public_tweets:
        data.append({
            'created_at': item.created_at,
            'tweet': item.text
#             'retweet_count': item.retweet_count,
#             'text': item.text,
#             'mined_at': datetime.datetime.now(),
#             'created_at': item.created_at
            })

    return data


# In[ ]:





# In[ ]:





# In[ ]:





# ### Flask

# In[ ]:


from flask import Flask, request, render_template
import os
import pandas as pd
import json
import datetime
from elasticsearch_dsl import Q
from elasticsearch_dsl import Search
from elasticsearch import Elasticsearch

app = Flask(__name__, template_folder='./')

@app.route('/')
def my_form():
    return render_template('my-form.html')

@app.route('/index.html', methods=['GET'])
def my_form_2():
    companyname = request.args.get('companyname')
    symbol = request.args.get('symbol')
    company_url = request.args.get('company_url')
    SEC_reports = request.args.get('SEC_reports')
    sector = request.args.get('sector')
    sub_industry = request.args.get('sub_industry')
    headquarter = request.args.get('headquarter')
    
    add = {'company':companyname,
          'symbol':symbol,
          'company_url':company_url,
          'SEC_reports':SEC_reports,
          'sector':sector,
          'sub_industry':sub_industry,
          'headquarter':headquarter}
    
    from elasticsearch import Elasticsearch
    client = Elasticsearch()
    client.index(index='company', body=add)

    return render_template('my-form.html', note = 'Company added')



@app.route('/', methods=['POST'])
def my_form_post():

    val = request.form['userinput']
    val = val.strip()
    companyname='empty'
    
    # query company info
    now = datetime.datetime.now().date()
    client = Elasticsearch()
    s0 = Search(using=client, index = 'company')
    f0 = Q('bool', must=Q('match', company = val))
    response1 = s0.filter(f0).execute()
    
    temp_info =[]
    for re1 in response1:
        temp_info.append(re1)
        
    info_company = temp_info[0]['company']
    company_url=temp_info[0]['company_url']
    symbol=temp_info[0]['symbol']
    SEC_reports=temp_info[0]['SEC_reports']
    sector=temp_info[0]['sector']
    sub_industry=temp_info[0]['sub_industry']
    headquarter=temp_info[0]['headquarter']

    
    
    
    # query stock and news for 7 days
    now = datetime.datetime.now().date()-timedelta(days=14)
    before = now-timedelta(days=6)
    client = Elasticsearch()
    s = Search(using=client, index = 'daily')
    f = Q('bool', must=[Q('match', Company=val),
                           Q('range', Date = {'lte': str(now),'gte':str(before)})])
    response4 = s.filter(f).execute()

    tempst = []; tempdate = []; tempnews = []
    for re in response4:
        tempst.append(re.Stock)
        tempdate.append(re.Date)
        tempnews.append(re.News)
        
    k=[]
    for i in tempst:
        k.append(i.to_dict())
    df_st = pd.DataFrame(k)
    df_st['date']=tempdate
    df_st = df_st.set_index('date').to_html()
    
    
    # format news
    news=''
    for temp in tempnews:
        for n in temp:
            news+='<h4>'+"Title:"+n['title']+'</h4>'+                  '<a href='+ n['url']+'>'+"Url:"+n['url']+'</a>'+                  '<p>'+"PublishedAt:"+ str(n['publishedAt'])+'</p>'+                  '<p>'+"Description:"+ n['description']+'</p>'+'</br>'

                    
                    
    # query tweets
    tw = twitterapi(val)
    twl=''
    for i in tw:
        twl+='<h5>'+str(i['created_at'])+'</h5>'+'<h5>'+i['tweet']+'</h5>'
    
    newsl=tempnews
    
    #time
#     t2 = datetime.datetime.now().date()-timedelta(days=5)
    
    table_html = 'xxxxxxxxxxxxxx'+companyname


    return render_template('my-form.html',
                           processed_text = val.upper(),
                           info_company=info_company,
                           company_url=company_url,
                           symbol=symbol,
                           SEC_reports=SEC_reports,
                           sector=sector,
                           sub_industry=sub_industry,
                           headquarter=headquarter,
                           news = news,
                           tweets = twl,
                           stock = df_st,
                           note = 'Result displayed')

if __name__ == '__main__':
	app.run(host='localhost', port=5002)


# # delect db

# In[387]:


# client.indices.delete(index='news', ignore=[400, 404])
client.indices.delete(index='company', ignore=[400, 404])
client.indices.delete(index='daily', ignore=[400, 404])


# In[ ]:





# In[ ]:


# !elasticsearch_loader --index testjson json test.json


# In[ ]:


# !elasticsearch_loader --index testjson --id-field uuid json test.json --json-lines


# In[ ]:





# ## Neo4j
# ### Load sp500 company data and load data into neo4j database

# In[1]:


# load sp500 company data from json file
import json 
with open ('sp500.json') as f:
    company = json.load(f)
# print(company)


# In[168]:


# transform to dataframe and export data csv file
df = pd.DataFrame(company['sp500'])
df[['city','state']] = df.headquarter.str.split(',',expand=True)
df = df.dropna()
df.head()
# df.to_csv('sp500.csv')


# In[56]:


# connect neo4j
from neo4j import GraphDatabase

database_name = "apan5400"
username = "neo4j"
password = "apan5400"
uri = "bolt://localhost:7687/" + database_name

driver = GraphDatabase.driver(uri, auth=(username, password))
session = driver.session()

print("Successfully connected to Neo4j!")


# ### merge by location

# In[61]:


# query= (
#     "LOAD CSV WITH HEADERS FROM 'file:///sp500.csv' AS line "
#     "CREATE (company:Company { \
#                        name: line.company, \
#                   company_url: toInteger(line.company_url),\
#                        symbol: toInteger(line.symbol), \
#                     SEC_report: toFloat(line.SEC_report) \
#                     symbol: toInteger(line.symbol), \
#                             } \
#             )"
#      "MERGE  (city:City {name: line.city})"
#      "MERGE  (state:State {name: line.state})"    
#      "CREATE (company)-[:HEADQUARTERED_IN]->(city)"
#      "CREATE (city)-[:LOCATED_IN]->(state)"
#     )

# result = session.run(query)
# print("All companies are imported from a csv file!")


# ### merge by sector and sub_industry

# In[60]:


query= (
    "LOAD CSV WITH HEADERS FROM 'file:///sp500.csv' AS line "
    "CREATE (company:Company { \
                       name: line.company, \
                  company_url: toInteger(line.company_url),\
                       symbol: toInteger(line.symbol), \
                    SEC_report: toFloat(line.SEC_report), \
                    headquarter: toInteger(line.headquarter) \
                            } \
            )"
     "MERGE  (sector:Sector {name: line.sector})"
     "MERGE  (sub_industry:Sub_industry {name: line.sub_industry})"    
     "CREATE (company)-[:BELONGTO]->(sub_industry)"
     "CREATE (sub_industry)-[:BELONGTOO]->(sector)"
    )

result = session.run(query)
print("All companies are imported from a csv file!")


# In[54]:


# test run
result = session.run("MATCH (n) RETURN n as nodes")

[record['nodes'] for record in result]


# ## delect neo4j data

# In[58]:


query_end = ("MATCH (n) DETACH DELETE n")
result = session.run(query_end)
print("All Nodes and relationships are deleted!")


# In[ ]:




