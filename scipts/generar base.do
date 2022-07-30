clear all
cd "C:\Users\USER\OneDrive - Universidad de los Andes\Escritorio\birthweight\Dta"

use nacimientos_2019_2020

drop OTRO_SIT TALLA_NAC MES IDHEMOCLAS IDFACTORRH CODPRES CODPTORE CODMUNRE ///
AREA_RES FECHA_NACM NIV_EDUP NIV_EDUM PROFESION year_month COD_MPIO AREANAC

export delimited "microdata_nacimientos.csv"