import pandas as pd

class TeamStat:
    def __init__(self,df,home,guest):
        self.df = df #Dataframe della partita
        self.home = home #Squadra di casa
        self.guest = guest #Squadra ospite
        self.poss = self.c_poss() #Possessi
        self.sc = self.c_sc() #Plays
        self.points = self.c_points() #Punti
        self.fgm2 = self.calc_stat(['2FGM','2FGF'])
        self.fga2 = self.calc_stat(['2FGA','2FGM','2FGF']) 
        self.fgm3 = self.calc_stat(['3FGM','3FTMF'])
        self.fga3 = self.calc_stat(['3FGA','3FGM','3FGF'])
        self.ftm = self.calc_stat(['FTM','FTMF','FTMFT','FTMFU'])
        self.fta = self.calc_stat(['FTA','FTM','FTMF','FTMFT','FTMFU'])
        self.assist = self.calc_stat(['AS']) 
        self.oreb = self.calc_stat(['O']) 
        self.dreb = self.calc_stat(['D'])
        self.to = self.calc_stat(['TO'])
        self.steal = self.calc_stat(['ST'])
        self.blk = self.calc_stat(['BLK'])

        
    def count_stat(self, stat, code):
        try:
            dft = self.df[self.df['CODETEAM'] == code]
            return len(dft.loc[self.df['PLAYTYPE'].isin(stat)])
        except Exception as e:
            print(f"An error occurred in count_stat: {e}")
            return 1
    
    def calc_stat(self,stat):
        home_stat = self.count_stat(stat,self.home)
        guest_stat = self.count_stat(stat,self.guest)
        return [home_stat, guest_stat]
        
    def c_poss(self):
        home_poss = (self.count_stat(['2FGM','3FGM','FTMF','TO'],self.home)+ 
                     self.count_stat(['D'],self.guest)) 
        guest_poss = (self.count_stat(['2FGM','3FGM','FTMF','TO'],self.guest)+ 
                      self.count_stat(['D'],self.home))
        return [home_poss,guest_poss]
    
    def c_sc(self):
        home_sc = self.poss[0] + (self.count_stat(['O','RV','SRV'],self.home))
        guest_sc = self.poss[1] + (self.count_stat(['O','RV','SRV'],self.guest))
        return [home_sc,guest_sc]
    
    def c_points(self):
        home_points = max(self.df['POINTS_A']) - min(self.df['POINTS_A'])
        guest_points = max(self.df['POINTS_B']) - min(self.df['POINTS_B'])
        return [home_points,guest_points]


    
    def to_dataframe(self):
        data = {'POSS': self.poss,
                'Scoring chance': self.sc,
                'Points': self.points,
                '2FGM': self.fgm2,
                '2GFA': self.fga2,
                '3FGM': self.fgm3,
                '3FGA': self.fga3,
                'FTM' : self.ftm,
                'FTA': self.fta,
                'AST': self.assist,
                'OREB': self.oreb,
                'DREB': self.dreb,
                'TO' : self.to,
                'STL': self.steal,
                'BLK': self.blk
                }
        return pd.DataFrame(data, index=[self.home, self.guest])