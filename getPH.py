from avgph import * 

# get the dataframe 
sdf = pd.read_csv("Clean_simsigs_50k.csv")

rdf = pd.read_csv("../Stephan_Raw_Data/Clean_realsigs_LHC22o.csv")

# get the average pulse height plot 

ph_plot(sdf, fname="avgnew")


dual_ph(rdf, sdf, fname="comparison")

ph_ratio(rdf, sdf, fname="r_s")