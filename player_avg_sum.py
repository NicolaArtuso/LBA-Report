#Script for finding players stats in the season

import pandas as pd
import requests
pd.set_option('display.max_columns',None)
import time
import numpy as np
import json
import re

r = requests.get(url = 'https://www.legabasket.it/api/oauth').json()

header = {
  'Host': 'api-lba.procne.cloud',
  'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:124.0) Gecko/20100101 Firefox/124.0',
  'Accept': 'application/json',
  'Accept-Language': 'it-IT,it;q=0.8,en-US;q=0.5,en;q=0.3',
  'Accept-Encoding': 'gzip, deflate, br',
  'Referer': 'https://www.legabasket.it/',
  'Authorization': 'Bearer {}'.format(r['data']['token']),
  'Origin': 'https://www.legabasket.it',
  'Connection': 'keep-alive',
  'Sec-Fetch-Dest': 'empty',
  'Sec-Fetch-Mode': 'cors',
  'Sec-Fetch-Site': 'cross-site',
}

def find_players():
    url = '	https://api-lba.procne.cloud/api/v1/players?year=2023&items=400'
    r = requests.get(url = url, headers = header).json()
    players = r['players']
    players_ids = [match['id'] for match in players]
    return players_ids

def stat_sum():
    data_rows =[]
    player_ids = find_players()
    for ids in player_ids:
        url = 'https://api-lba.procne.cloud/api/v1/players/{}/stats?s=2023'.format(ids)
        r = requests.get(url = url, headers = header).json()
        name = r['player']['name']
        surname = r['player']['surname']
        role = r['player']['player_role_description']
        id = ids
        specific_row = None
        for row in r['stats']['data']['sum']:
            if row['championship_year'] == 2023 and row['championship_name'] == 'Regular Season':
                specific_row = row
                break
        valori = [name,surname, id,role ] + list(row.values())
        data_rows.append(valori)
    col_names = ['name','surname','code','role']+ list(r['stats']['data']['sum'][0].keys())
    df = pd.DataFrame(data_rows, columns = col_names)
    print(df)
    df.to_csv('PlayerStat.dat', index=True)


def stat_avg():
    data_rows =[]
    player_ids = find_players()
    for ids in player_ids:
        url = 'https://api-lba.procne.cloud/api/v1/players/{}/stats?s=2023'.format(ids)
        r = requests.get(url = url, headers = header).json()
        name = r['player']['name']
        surname = r['player']['surname']
        role = r['player']['player_role_description']
        id = ids
        specific_row = None
        for row in r['stats']['data']['avg']:
            if row['championship_year'] == 2023 and row['championship_name'] == 'Regular Season':
                specific_row = row
                break
        valori = [name,surname, id,role ] + list(row.values())
        data_rows.append(valori)
    col_names = ['name','surname','code','role']+ list(r['stats']['data']['avg'][0].keys())
    df = pd.DataFrame(data_rows, columns = col_names)
    print(df)
    df.to_csv('PlayerStatAvg.dat', index=True)

def main():
    stat_sum()
    stat_avg()

main()