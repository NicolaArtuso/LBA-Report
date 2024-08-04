#Script to create PBP dataset of all the games in LBA

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
  'Sec-Fetch-Site': 'cross-site'
}


def trova_partite_campionato():
    partite = []
    for codice in range(1,31):
        url = 'https://api-lba.procne.cloud/api/v1/championships/530/calendar?d={}'.format(codice)
        r = requests.get(url = url, headers = header).json()
        # Itera su tutte le azioni e aggiungi i loro valori come righe alla lista dei dati
        matches = r['matches']
        match_ids = [match['id'] for match in matches if match['home_final_score'] != 0]
        for match in match_ids:
            partite.append(match)
    return partite


def main():
    data_rows = []
    partite = trova_partite_campionato()
    for partita in partite:
        url_pbp = 'https://api-lba.procne.cloud/api/v1/championships_matches/{}/play_by_play?info=1&sort=asc'.format(partita)
        r_pbp = requests.get(url = url_pbp, headers = header).json()
        # Itera su tutte le azioni e aggiungi i loro valori come righe alla lista dei dati
        for action in r_pbp['pbp']['actions']:
            valori = list(action.values())
            valori.append(partita)
            data_rows.append(valori)
        

	# Crea il DataFrame utilizzando la lista dei dati e i nomi delle colonne
	col_names = list(r_pbp['pbp']['actions'][0].keys()) +['id_game']
	df = pd.DataFrame(data_rows, columns=col_names)


	percorso_file = "C:/Users/nicol/OneDrive/Desktop/Progetti/Euro/DAT/lbaPBP.dat"

	# Salva il DataFrame nel file CSV
	df.to_csv(percorso_file, index=True)

main()