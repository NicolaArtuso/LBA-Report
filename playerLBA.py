import pandas as pd
import time
from teamLBA import TeamStat


class PlayerStat:
    def __init__(self,code,df,home,guest): 
    
    #Codici giocatori e team
        self.code = code  #nome del giocatore 
        self.home = home  #squadra di casa 
        self.guest = guest #squadra ospite
        self.df = self.c_df(df) #dataframe relativo al giocatore in campo
        #self.minutes = self.minutes(df) #minuti giocati
        self.team_stat = TeamStat(self.df,self.home,self.guest) #Statistiche di squadra mentre il giocatore è in campo
        self.team = self.find_team() #team del giocatore secondo codice 1=ospite 0=casa
        self.player = self.find_player()
     
    #Stat giocatore
        self.sc = self.c_sc() #"Shooting chances" o "plays" del gicatore
        self.points = self.c_points() #Punti del giocatore
        self.fgm2 = self.count_stat(['2FGM','2FGF']) # Corretto l'utilizzo di self.calc_stat
        self.fga2 = self.count_stat(['2FGA','2FGM','2FGF']) 
        self.fgm3 = self.count_stat(['3FGM','3FGF']) # Corretto l'utilizzo di self.calc_stat
        self.fga3 = self.count_stat(['3FGA','3FGM','3FGF'])
        self.ftm = self.count_stat(['FTM','FTMF','FTMFT','FTMFU']) # Corretto l'utilizzo di self.calc_stat
        self.fta = self.count_stat(['FTA','FTM','FTMF','FTMFT','FTMFU'])
        self.assist = self.count_stat(['AS']) # Corretto l'utilizzo di self.calc_stat
        self.oreb = self.count_stat(['O']) # Corretto l'utilizzo di self.calc_stat
        self.dreb = self.count_stat(['D'])
        self.to = self.count_stat(['TO'])
        self.steal = self.count_stat(['ST'])
        self.blk = self.count_stat(['BLK'])
        
     #Stat squadra
        self.team_poss = self.team_stat.poss[self.team]
        self.team_sc = self.team_stat.sc[self.team]
        self.team_points = self.team_points()
        self.team_fgm2 = self.team_stat.calc_stat(['2FGM','2FGF'])[self.team] # Corretto l'utilizzo di self.calc_stat
        self.team_fga2 = self.team_stat.calc_stat(['2FGA','2FGM','2FGF'])[self.team]
        self.team_fgm3 = self.team_stat.calc_stat(['3FGM','3FGF'])[self.team] # Corretto l'utilizzo di self.calc_stat
        self.team_fga3 = self.team_stat.calc_stat(['3FGA','3FGM','3FGF'])[self.team]
        self.team_ftm = self.team_stat.calc_stat(['FTM','FTMF','FTMFT','FTMFTU'])[self.team] # Corretto l'utilizzo di self.calc_stat
        self.team_fta = self.team_stat.calc_stat(['FTA','FTM','FTMF','FTMFT','FTMFTU'])[self.team]
        self.team_assist = self.team_stat.calc_stat(['AS'])[self.team] # Corretto l'utilizzo di self.calc_stat
        self.team_oreb = self.team_stat.calc_stat(['O'])[self.team] # Corretto l'utilizzo di self.calc_stat
        self.team_dreb = self.team_stat.calc_stat(['D'])[self.team]
        self.team_to = self.team_stat.calc_stat(['TO'])[self.team]
        self.team_steal = self.team_stat.calc_stat(['ST'])[self.team]
        self.team_blk = self.team_stat.calc_stat(['BLK'])[self.team]
        
     # #Stat opp
        self.opp_poss = self.team_stat.poss[not(self.team)]
        self.opp_sc = self.team_stat.sc[not(self.team)]
        self.opp_points = self.opp_points()
        self.opp_fgm2 = self.team_stat.calc_stat(['2FGM','2FGF'])[not(self.team)] # Corretto l'utilizzo di self.calc_stat
        self.opp_fga2 = self.team_stat.calc_stat(['2FGA','2FGM','2FGF'])[not(self.team)]
        self.opp_fgm3 = self.team_stat.calc_stat(['3FGM','3FGF'])[not(self.team)] # Corretto l'utilizzo di self.calc_stat
        self.opp_fga3 = self.team_stat.calc_stat(['3FGA','3FGM','3FGMF'])[not(self.team)]
        self.opp_ftm = self.team_stat.calc_stat(['FTM','FTMF','FTMFT','FTMFU'])[not(self.team)] # Corretto l'utilizzo di self.calc_stat
        self.opp_fta = self.team_stat.calc_stat(['FTA','FTM','FTMF','FTMFT','FTMFU'])[not(self.team)]
        self.opp_assist = self.team_stat.calc_stat(['AS'])[not(self.team)] # Corretto l'utilizzo di self.calc_stat
        self.opp_oreb = self.team_stat.calc_stat(['O'])[not(self.team)] # Corretto l'utilizzo di self.calc_stat
        self.opp_dreb = self.team_stat.calc_stat(['D'])[not(self.team)]
        self.opp_to = self.team_stat.calc_stat(['TO'])[not(self.team)]
        self.opp_steal = self.team_stat.calc_stat(['ST'])[not(self.team)]
        self.opp_blk = self.team_stat.calc_stat(['BLK'])[not(self.team)]

    #Funzione che conta le statistiche richieste del giocatore 
    def count_stat(self,stat): 
        dfp = self.df.loc[self.df['PLAYER']== self.code]
        return len(dfp.loc[self.df['PLAYTYPE'].isin(stat)])

    
    #Fornisce in output due liste: la prima contiene gli indici di riga in cui il giocatore è entrato,
    #la seconda gli indici di riga in cui il giocatore è uscito
    def IN_OUT(self,df):
        playerI = list(df.loc[(df['PLAYTYPE']=='IN') & (df['PLAYER']==self.code)].index) #Entrata del giocatore in campo
        playerO = list(df.loc[(df['PLAYTYPE']=='OUT') & (df['PLAYER']==self.code)].index) #Uscita del giocatore
        if (playerI==[] or (playerO!=[] and playerI[0]>playerO[0])):
            playerI.insert(0,0) #Inserimento dell'indice 0 nella lista delle entrate in caso di partecipazione nel quintetto iniziale
        if (playerO==[] or (playerI!=[] and playerI[-1]>playerO[-1])): 
            playerO.append(len(df)-1) #inserimento di un indice finale in caso di partita terminata in campo
        
        return sorted(playerI), sorted(playerO) 
    
    #Costruisce il datframe della partita mentre il giocatore era in campo
    def c_df(self,df):
        try:
            playerI = self.IN_OUT(df)[0]
            playerO = self.IN_OUT(df)[1]
            dfplayer = pd.DataFrame(columns=df.columns)
            for i in range(len(playerI)):
                df_subset = pd.DataFrame(df.loc[playerI[i]:playerO[i]])
                dfplayer = pd.concat([dfplayer, df_subset], axis=0)
            return dfplayer
        except Exception as e:
            raise RuntimeError(f"An error occurred in c_df: {e}")

        
        
   #Calcola i minuti di gioco
    def minutes(self,df):
        playerI = self.IN_OUT(df)[0]
        playerO = self.IN_OUT(df)[1]
        total = 0 
        quarters = list(self.df['QUARTER'].unique())
        for i in range(len(playerO)):
            qI = self.df.loc[playerI[i]]['QUARTER']
            qO = self.df.loc[playerO[i]]['QUARTER']
            
            if qI == qO:
                total += self.df.loc[playerI[i]]['MARKERTIME'] - self.df.loc[playerO[i]]['MARKERTIME']
            else:
                total += self.df.loc[playerI[i]]['MARKERTIME']- min(self.df.loc[(self.df['QUARTER']== qI)]['MARKERTIME'])
                total += max(self.df.loc[(self.df['QUARTER']== qO)]['MARKERTIME'])-self.df.loc[playerO[i]]['MARKERTIME']
                total += 600 * (quarters.index(qO) - quarters.index(qI) - 1)

        minuti = int(total // 60)
        secondi = int(total % 60)
        return f"{minuti:02d}:{secondi:02d}"                                                                    
    
    #Restituisce 0 se il giocatore gioca per la squadra di casa, 1 altrimenti
    def find_team(self):
            row = self.df.loc[self.df['PLAYER'] == self.code].iloc[0]
            if row['CODETEAM'] == self.team_stat.home:
                return 0
            return 1
    
    def find_player(self):
            row = self.df.loc[self.df['PLAYER'] == self.code].iloc[0]
            return row['player_name'] + ' ' + row['player_surname']
            #return row['PLAYER']
    
    #Calcola le 'shooting chances' o 'plays' del giocatore
    def c_sc(self):
        return (self.count_stat(['2FGA','3FGA','RV','2FGM','3FGM','SRV','TO']))
    
    
    #Calcola i punti del giocatore
    def c_points(self):
        op = self.count_stat(['FTM','FTMF'])
        twp = self.count_stat(['2FGM','2FGF'])
        thp = self.count_stat(['3FGM','3FGF'])
        return (op+2*twp+3*thp)
    
    #Calcola i punti della squadra avversaria
    def opp_points(self):
        op = self.team_stat.count_stat(['FTM','FTMF'],self.opp())
        twp = self.team_stat.count_stat(['2FGM','2FGF'],self.opp())
        thp = self.team_stat.count_stat(['3FGM','3FGF'],self.opp())
        return (op+2*twp+3*thp)
    
    #Calcola i punti della squadra del giocatore
    def team_points(self):
        op = self.team_stat.count_stat(['FTM','FTMF'],self.to_team())
        twp = self.team_stat.count_stat(['2FGM','2FGF'],self.to_team())
        thp = self.team_stat.count_stat(['3FGM','3FGF'],self.to_team())
        return (op+2*twp+3*thp)
    
    
    
    #Trova il codice della squadra del giocatore
    def to_team(self):
        if self.team == 0:
            return self.home
        return self.guest
    
    #Trova il codice della squadra avversaria
    def opp(self):
        if self.team == 1:
            return self.home
        return self.guest
    
    #Conversione di un'istanza dell'oggetto di un dataframe
    def to_dataframe(self):
        data = {
                'Player': self.find_player(),
                'Team': [self.to_team()],
                #'Minutes':[self.minutes],
                'scoring_chances': [self.sc],
                'Points': [self.points],
                'FGM2': self.fgm2,
                'FGA2': self.fga2,
                'FGM3': self.fgm3,
                'FGA3': self.fga3,
                'FTM' : self.ftm,
                'FTA': self.fta,
                'AST': self.assist,
                'OREB': self.oreb,
                'DREB': self.dreb,
                'TO' : self.to,
                'STL': self.steal,
                'BLK':self.blk,
                
                'team_POSS': self.team_poss,
                'team_scoring_chances': self.team_sc,
                'team_Points': self.team_points,
                'team_FGM2': self.team_fgm2,
                'team_FGA2': self.team_fga2,
                'team_FGM3': self.team_fgm3,
                'team_FGA3': self.team_fga3,
                'team_FTM' : self.team_ftm,
                'team_FTA': self.team_fta,
                'team_AST': self.team_assist,
                'team_OREB': self.team_oreb,
                'team_DREB': self.team_dreb,
                'team_TO' : self.team_to,
                'team_STL': self.team_steal,
                'team_BLK': self.team_blk,
                
                'opp_POSS': self.opp_poss,
                'opp_scoring_chances': self.opp_sc,
                'opp_Points': self.opp_points,
                'opp_FGM2': self.opp_fgm2,
                'opp_FGA2': self.opp_fga2,
                'opp_FGM3': self.opp_fgm3,
                'opp_FGA3': self.opp_fga3,
                'opp_FTM' : self.opp_ftm,
                'opp_FTA': self.opp_fta,
                'opp_AST': self.opp_assist,
                'opp_OREB': self.opp_oreb,
                'opp_DREB': self.opp_dreb,
                'opp_TO' : self.opp_to,
                'opp_STL': self.opp_steal,
                'opp_BLK': self.opp_blk
                }
        return pd.DataFrame(data,index = [self.code])