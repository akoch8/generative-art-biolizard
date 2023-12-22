# 
#     ,--,                                                                 
# ,---.'|                    ,----,                                       
# |   | :      ,---,       .'   .`|  ,---,       ,-.----.       ,---,     
# :   : |   ,`--.' |    .'   .'   ; '  .' \      \    /  \    .'  .' `\   
# |   ' :   |   :  :  ,---, '    .'/  ;    '.    ;   :    \ ,---.'     \  
# ;   ; '   :   |  '  |   :     ./:  :       \   |   | .\ : |   |  .`\  | 
# '   | |__ |   :  |  ;   | .'  / :  |   /\   \  .   : |: | :   : |  '  | 
# |   | :.'|'   '  ;  `---' /  ;  |  :  ' ;.   : |   |  \ : |   ' '  ;  : 
# '   :    ;|   |  |    /  ;  /   |  |  ;/  \   \|   : .  / '   | ;  .  | 
# |   |  ./ '   :  ;   ;  /  /--, '  :  | \  \ ,';   | |  \ |   | :  |  ' 
# ;   : ;   |   |  '  /  /  / .`| |  |  '  '--'  |   | ;\  \'   : | /  ;  
# |   ,/    '   :  |./__;       : |  :  :        :   ' | \.'|   | '` ,/   
# '---'     ;   |.' |   :     .'  |  | ,'        :   : :-'  ;   :  .'     
#           '---'   ;   |  .'     `--''          |   |.'    |   ,.'       
#                   `---'                        `---'      '---'         
#                                                                         
# 

setwd('~/Documents/biolizard/generative_art/')

library('aRtsy')
library('httr')
library('jsonlite')
library('xml2')

alphabet = c('A', 'T', 'G', 'C', 'N')

server = 'https://rest.ensembl.org'

# Common wall lizard
# https://www.ensembl.org/Podarcis_muralis/Info/Index
ext = '/sequence/region/podarcis_muralis/1:1000000..1000200:1'
r = GET(paste(server, ext, sep = ''), content_type('text/plain'))
sequence_lizard = content(r)
sequence_lizard = unlist(strsplit(sequence_lizard, split=''))

# Bonobo
ext = '/sequence/region/pan_paniscus/1:1000000..1000200:1'
r = GET(paste(server, ext, sep = ''), content_type('text/plain'))
sequence_bonobo = content(r)
sequence_bonobo = unlist(strsplit(sequence_bonobo, split=''))

# Human
ext = '/sequence/region/homo_sapiens/1:1000000..1000200:1'
r = GET(paste(server, ext, sep = ''), content_type('text/plain'))
sequence_human = content(r)
sequence_human = unlist(strsplit(sequence_human, split=''))

calculate_matrix = function(seq_1, seq_2, alphabet) {
	seq_matrix = matrix(nrow=200, ncol=200)
	for (i in 1:200) {
		for (j in 1:200) {
			if (seq_1[i] == seq_2[j]) {
				seq_factor = match(seq_1[i], alphabet) * match(seq_2[j], alphabet)
				seq_matrix[i, j] = jitter(seq_factor, factor=1, amount=10)
			} else {
				seq_factor = match(seq_1[i], alphabet) * match(seq_2[j], alphabet) / 2
				seq_matrix[i, j] = jitter(seq_factor, factor=1, amount=10)
			}
		}
	}
	seq_matrix = seq_matrix / max(seq_matrix)
	return(seq_matrix)
}

background_color = '#121212'

seq_matrix_lizard = calculate_matrix(sequence_lizard, sequence_human, alphabet)
png('lizard.png', width=20, height=20, units='in', res=300)
set.seed(1)
canvas_flow(
	colors=colorPalette('sky'), 
	background=background_color, 
	outline='none', 
	polar=FALSE, 
	iterations=200, 
	lines=1000, 
	lwd=0.3, 
	stepmax=0.01, 
	angles=seq_matrix_lizard)
dev.off()

seq_matrix_bonobo = calculate_matrix(sequence_bonobo, sequence_human, alphabet)
png('bonobo.png', width=20, height=20, units='in', res=300)
set.seed(1)
canvas_flow(
	colors=colorPalette('tuscany3'), 
	background=background_color, 
	outline='none', 
	polar=FALSE, 
	iterations=200, 
	lines=1000, 
	lwd=0.3, 
	stepmax=0.01, 
	angles=seq_matrix_bonobo)
dev.off()

seq_matrix_human = calculate_matrix(sequence_human, sequence_human, alphabet)
png('human.png', width=20, height=20, units='in', res=300)
set.seed(1)
canvas_flow(
	colors=colorPalette('azul'), 
	background=background_color, 
	outline='none', 
	polar=FALSE, 
	iterations=200, 
	lines=1000, 
	lwd=0.3, 
	stepmax=0.01, 
	angles=seq_matrix_human)
dev.off()
