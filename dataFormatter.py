### this script reads the raw data and creates si tables

import sys

# 0. user defined variables
rawFile='data/P4-HS_raw_AUC.csv'
tableFile='tables/table.txt'

# 1. reading file
print 'reading file...'

distribution={}

with open(rawFile,'r') as f:
    f.next()
    for line in f:
        vector=line.split(',')

        station=vector[0].split('_')[0].replace('"','')
        species=vector[0].split('_')[1]
        year=int(vector[0].split('_')[2].replace('"',''))
        code=int(vector[6])
        if code == 3:
            age='young'
        elif code == 4:
            age='adult'
        elif code == 0:
            age='undefined'
        else:
            print
            print vector
            print age
            print 'error defining age code. Exiting...'
            sys.exit()

        # filling up the dictionary as follows:  distribution[species][station][year][age]
        if species not in distribution.keys():
            distribution[species]={}

        if station not in distribution[species].keys():
            distribution[species][station]={}

        if year not in distribution[species][station].keys():
            distribution[species][station][year]={}

        if age not in distribution[species][station][year].keys():
            distribution[species][station][year][age]=0

        distribution[species][station][year][age]=distribution[species][station][year][age]+1
            
# 2. creating table
print 'creating tables...'
allSpecies=distribution.keys()
allSpecies.sort()

with open(tableFile,'w') as f:
    f.write('species (# stations)\tstation\tNjuvenile\tNadult\tNundetermined\tNtotal\tyears\n')
    for species in allSpecies:

        stations=distribution[species].keys()
        stations.sort()
        
        for station in stations:
            
            f.write(species+' (%s)\t'%(str(len(stations)))+station+'\t')

            # finding the numbers over the years
            Nyoung=0
            Nadult=0
            Nundefined=0
            Ntotal=0

            localYears=distribution[species][station].keys()
            localYears.sort()
            
            for year in localYears:
                localAges=distribution[species][station][year].keys()
                for age in localAges:
                    value=distribution[species][station][year][age]
                    
                    if age == 'young':
                        Nyoung=Nyoung+value
                    elif age == 'adult':
                        Nadult=Nadult+value
                    elif age == 'undefined':
                        Nundefined=Nundefined+value
                    else:
                        print 'error working with ages. Exiting...'
                        sys.exit()
            Ntotal=Nyoung+Nadult+Nundefined
            f.write('%s\t%s\t%s\t%s\t'%(Nyoung,Nadult,Nundefined,Ntotal))

            yearsString=','.join([str(element) for element in localYears])
            f.write(yearsString)
                        
            f.write('\n')
        
# 3. last message
print '... analysis completed.'
