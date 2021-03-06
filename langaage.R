library(rworldmap)
ddf = read.table(text="
country Number_of_Languages
ATG 1
AFG 3
ALB 11
DZA 2
AND 3
AGO 1
ARG 5
ARM 2
ASM 2
ABW 3
AUS 3
AUT 4
AZE 3
BHS 1
BHR 3
BGD 2
BRB 1
BLR 2
BEL 3
BLZ 3
BEN 1
BMU 2
BTN 1
BOL 2
BIH 3
BWA 1
BRA 4
VGB 1
BRN 3
BGR 3
BFA 1
MMR 1
BDI 3
KHM 2
CMR 2
CAN 2
CPV 1
CYM 1
CAF 1
TCD 3
CHL 1
CHN 4
CXR 3
CCK 2
COL 1
COM 2
COD 2
COG 1
COK 1
CRI 1
HRV 7
CUB 1
CYP 3
CZE 1
CIV 1
DNK 2
DJI 3
DMA 2
DOM 1
TLS 3
ECU 2
EGY 3
SLV 1
GNQ 2
ERI 1
EST 2
ETH 4
FLK 1
FJI 2
FIN 3
GUF 1
PYF 1
GAB 1
GMB 2
GEO 3
DEU 1
GHA 1
GIB 4
GRC 3
GRL 1
GRD 2
GLP 2
GUM 1
GTM 1
GGY 3
GIN 1
GUY 5
VAT 3
HND 2
HKG 2
HUN 1
ISL 3
IND 15
IDN 4
IRN 5
IRQ 3
IRL 3
ISR 3
ITA 3
JAM 2
JPN 1
JES 2
JOr 2
KAZ 2
KEN 1
KIR 1
KOR 1
PRK 2
KWT 2
KGZ 1
LAO 2
LAV 3
LBN 4
LSO 3
LBR 1
LBY 3
LIE 2
LTU 3
LUX 2
MAC 3
MKD 5
MDG 2
MYS 8
MDV 1
MLI 1
MLT 2
IMN 2
MTQ 2
MRT 1
MUS 3
MYT 1
MEX 1
FSM 1
MDA 3
MCO 3
MNG 1
MSR 1
MAR 2
MOZ 1
NAM 3
NRU 1
NPL 3
NLD 2
ANT 4
NCL 1
NZL 1
NIC 1
NER 2
NGA 4
NFK 1
MNP 2
NOR 3
OMN 17
PAK 5
PLW 5
PAN 2
PNG 1
PRY 1
PER 2
PHL 5
POL 1
PRT 1
PRI 2
QAT 1
ROU 3
RUS 5
RWA 3
REU 2
SHN 1
KNA 1
LCA 1
SPM 1
VCT 1
ESP 2
WSM 1
SMR 1
SAU 1
SEN 1
SYC 2
SLE 1
SGP 6
SVK 4
SVN 2
SLB 1
SOM 3
ZAF 1
LKA 1
SDN 2
FRA 2
SUR 4
SJM 2
SWZ 1
SWE 2
CHE 8
SYR 4
ZWE 2
ZMB 1
YEM 1
ESH 2
WLF 1
VGB 4
VIR 4
VNM 5
VEN 1
VUT 3
UZA 3
UZB 2
URY 2
USA 34
UK 3
ARE 5
UKR 5
UGA 3
TUV 1
TCA 1
TKM 2
TUR 5
TUN 2
TTO 5
TON 1
TKL 1
TGO 1
THA 2
TZA 3
TJK 1
TWN 2
STP 1
KGZ 1
", header=T)

#create a map-shaped window
mapDevice('x11')
#join to a coarse resolution map
spdf <- joinCountryData2Map(ddf, joinCode="NAME", nameJoinColumn="country")

mapParams<-mapCountryData(spdf, nameColumnToPlot="Number_of_Languages", catMethod="categorical",mapTitle="",colourPalette="terrain", addLegend=FALSE)
do.call(addMapLegendBoxes
        ,c(mapParams
           ,cex=0.8
           ,ncol=2
           ,x='bottomleft'
           ,title='Count of languages'))



