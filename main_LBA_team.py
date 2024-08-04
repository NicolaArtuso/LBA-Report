import pandas as pd
import requests
pd.set_option('display.max_columns',None)
import time
import numpy as np
from teamLBA import TeamStat
from playerLBA import PlayerStat

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


def convert_to_seconds(time_str):
    minutes, seconds = time_str.split(':')
    return int(minutes) * 60 + int(seconds)

def find_home(df): 
    row = df[df['home_club'] == 1 ].iloc[0]
    return row['CODETEAM']
    
#Funzione per trovare il codice squadra del team ospite
def find_guest(df):
    row = df.loc[df['home_club'] == 0].iloc[0]
    return row['CODETEAM']

mapping = {
    'Rimbalzo offensivo': 'O',
    'Rimbalzo difensivo': 'D',
    'Rimbalzi difensivi di squadra':'D',
    'Rimbalzi offensivi di squadra:':'O',
    '2 punti sbagliato':'2FGA',
    '3 punti sbagliato': '3FGA',
    '2 punti segnato': '2FGM',
    '3 punti segnato':'3FGM',
    'Tiro libero segnato':'FTM',
    'Tiro libero sbagliato':'FTA',
    'Fallo commesso':'CM',
    'Fallo Subito':'RV',
    'Stoppata':'BLK',
    'Stoppata subita':'SBLK',
    'Palla recuperata':'ST',
    'Palle recuperate di squadra':'ST',
    'Palla persa':'TO',
    'Palle perse di squadra':'TO',
    'Ingresso':'IN',
    'Uscita':'OUT',
    'Assist':'AS'

}

def apply_mapping(description):
    return mapping.get(description, description)

def stat_partita(partita):
    url= 'https://api-lba.procne.cloud/api/v1/championships_matches/{}/play_by_play?info=1&sort=asc'.format(partita)
    r = requests.get(url = url, headers = header).json()

    col_names = list(r['pbp']['actions'][0].keys())
    row_val = list(r['pbp']['actions'][0].values())
    df = pd.DataFrame([row_val], columns=col_names)
    data_rows = []

    # Itera su tutte le azioni e aggiungi i loro valori come righe alla lista dei dati
    for action in r['pbp']['actions']:
        data_rows.append(list(action.values()))

    # Crea il DataFrame utilizzando la lista dei dati e i nomi delle colonne
    df = pd.DataFrame(data_rows, columns=col_names)



    # Applicare il mapping solo se la descrizione è presente nel mapping definito
    df['description'] = df['description'].map(apply_mapping)

    # Rinominare una colonna
    df = df.rename(columns={'print_time': 'MARKERTIME',
                            'description':'PLAYTYPE',
                            'player_id':'PLAYER',
                            'team_name':'CODETEAM',
                            #'player_surname':'PLAYER',
                            'period':'QUARTER'})

    df.loc[df['action_1_qualifier_code'] == '102', 'PLAYTYPE'] = 'CMU'
    df.loc[df['action_1_qualifier_code'] == '105', 'PLAYTYPE'] = 'SCMU'
    df.loc[df['action_1_qualifier_code'] == '101', 'PLAYTYPE'] = 'CMT'
    df.loc[df['action_1_qualifier_code'] == '106', 'PLAYTYPE'] = 'OF'
    df.loc[(df['action_1_qualifier_code'] == '103') & (df['PLAYTYPE']== 'CM'), 'PLAYTYPE'] = 'SCM'
    
    
    dfFT = pd.DataFrame(df.loc[df['PLAYTYPE'].isin(['FTM','FTA','CMT'])]) 
    condizione = (dfFT['PLAYTYPE'] == 'FTM') & (dfFT['PLAYTYPE'].shift(1) =='CMT' )
    dfFT.loc[condizione, 'PLAYTYPE'] = 'FTMFT'
    df.update(dfFT[dfFT['PLAYTYPE'] == 'FTMFT'])
    del dfFT
    
    dfFT = pd.DataFrame(df.loc[df['PLAYTYPE'].isin(['FTM','FTA','CMU'])]) 
    condizione = (dfFT['PLAYTYPE'] == 'FTM') & (dfFT['PLAYTYPE'].shift(1) =='CMU' )
    dfFT.loc[condizione, 'PLAYTYPE'] = 'FTMFU'
    df.update(dfFT[dfFT['PLAYTYPE'] == 'FTMFU'])
    del dfFT
    
    #Seleziono i "tiri liberi finali" ovvero i tiri liberi che segnalano un cambio di possesso
    dfFT = pd.DataFrame(df.loc[df['PLAYTYPE'].isin(['FTM','FTA'])]) 
    condizione = (dfFT['PLAYTYPE'] == 'FTM') & (dfFT['MARKERTIME'].shift(-1) != dfFT['MARKERTIME'] )
    dfFT.loc[condizione, 'PLAYTYPE'] = 'FTMF'
    df.update(dfFT[dfFT['PLAYTYPE'] == 'FTMF'])
    del dfFT

    #Adatto markertime per le operazioni
    df['MARKERTIME'] = df['MARKERTIME'].apply(convert_to_seconds)

    #Seleziono i "field goal foul" ovvero i tiri segnati subendo fallo, che dunque non segnalano un cambio di possesso
    dfFG = pd.DataFrame(df.loc[df['PLAYTYPE'].isin(['FTM','FTA','3FGM','2FGM','FTMF'])]) 
    condizione1 = (dfFG['PLAYTYPE'] == '2FGM') & (dfFG['PLAYTYPE'].shift(-1).isin(['FTM','FTMF','FTA']))
    condizione2 = (dfFG['PLAYTYPE'] == '3FGM') & (dfFG['PLAYTYPE'].shift(-1).isin(['FTM','FTMF','FTA']))
    condizione3 = condizione1 & (abs(dfFG['MARKERTIME']-dfFG['MARKERTIME'].shift(-1))<2)
    condizione4 = condizione2 & (abs(dfFG['MARKERTIME']-dfFG['MARKERTIME'].shift(-1))<2)
    dfFG.loc[condizione3,'PLAYTYPE']='2FGF'
    dfFG.loc[condizione4,'PLAYTYPE']='3FGF'
    df.update(dfFG[dfFG['PLAYTYPE'] == '2FGF'])
    df.update(dfFG[dfFG['PLAYTYPE'] == '3FGF'])
    del dfFG

    home = find_home(df)
    guest = find_guest(df)
    
    # Dividere la colonna 'score' in due colonne separate
    df[['POINTS_A', 'POINTS_B']] = df['score'].str.split(' - ', expand=True)

    # Convertire i punteggi da stringhe a numeri interi
    #df[['POINTS_A', 'POINTS_B']] = df[['POINTS_A', 'POINTS_B']].astype(int)
    df[['POINTS_A', 'POINTS_B']] = df[['POINTS_A', 'POINTS_B']].fillna(0).astype(int)
    # Rimuovere la colonna 'score' se non è più necessaria
    df = df.drop(columns=['score'])
    
    #Genero l'oggetto di tipo TeamStat
    team_stats = TeamStat(df,home,guest)
    
    #Creo il dataframe relativo alle squadre
    df_team_stats = team_stats.to_dataframe()
    #Arrotondo il risultato alla seconda cifra decimale
    df_team_stats = df_team_stats.applymap(lambda x: round(x, 2))
    #Stampo il risultato
    print(df_team_stats)
    
    #Giocatori
    #players_names = list(filter(lambda x: x is not None and x != '', df['PLAYER'].unique()))
    players_names = list(filter(lambda x: pd.notna(x) and x != '', df['PLAYER'].unique()))
    player_stats_list = []
    for code in players_names:
        #Genero un oggetto di tipo PlayerStat per ciascun giocatore
        player_stat = PlayerStat(code, df, home, guest)
        #Converto in un dataframe ogni oggetto creato e li inserisco in una lista
        player_stats_list.append(player_stat.to_dataframe())
    
    #Creo il dataframe finale concatenando tutti i dataframe della lista
    all_player_stats = pd.concat(player_stats_list)
    #Ordino il dataframe in base alla squadra di appartenenza
    all_player_stats = all_player_stats.sort_values(by='Team')
    #Arrotondo il risultato alla seconda cifra decimale
    #all_player_stats = all_player_stats.round(2)
    
    #all_player_stats = all_player_stats[all_player_stats['Team'] == 'Umana Reyer Venezia']

    #Stampo il risultato
    print (all_player_stats)
    #all_player_stats.to_csv('players_'+home+'-'+guest+'.dat', sep='\t', index=True)
    return all_player_stats



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
    #print(partite)
    return partite

    


def main():
    df_tot = pd.DataFrame()
    #codice = 1609
    #partite = trova_partite_team(codice)
    partite = trova_partite_campionato()
    print(len(partite))
    for partita in partite:
        print(partita)
        df = stat_partita(partita)
        print(df)
        percorso_file = "C:/Users/nicol/OneDrive/Desktop/Progetti/lbaPBP/DAT/" + str(partita)+".dat"
        # Salva il DataFrame nel file CSV
        df.to_csv(str(partita)+".dat", sep='\t',index=True)
main() 