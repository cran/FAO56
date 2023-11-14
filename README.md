# FAO56
## A package to calculate evapotranspiration
FAO56 is a package including some functions and datasets to compute the reference evapotranspiration and
    crop evapotranspiration. In addition, it contains some functions to compute the relevant meteorological variables
    such as the variables related to the solar radiation and vapour pressure.
The functions have been developed based on the formulas presented in
    [Crop evapotranspiration - Guidelines for computing crop water requirements - FAO Irrigation and drainage paper 56]
    {https://www.fao.org/3/x0490E/x0490e00.htm}. Also, the datasets have been extracted from the mentioned reference.
Here is a simple example:

```
# Computing ET_c of the crop millet planted in Sahiwal, Pakistan
# for a specific day in the initial growth stage
## Loading the relevant Kc dataset
data(Kc_Cereals)
## Latitude in decimal degree
latdeg = 31.685
## Date (2020 June 7)
pdate = "2020-06-07"
## Maximum and minimum temperatures in celsius
temp_max = 38
temp_min = 28
## Actual duration of sunshine and maximum possible duration of sunshine or daylight in hours
actsunshine = 13
maxdaylight = 14
## Elevation above sea level in meter
h = 170
## Wind speed in the height 2m above the ground surface in m/s
ws = 2
##  Evapotranspiration rate from the reference surface (ETo) in mm/day
ET_ref = ETo_FPM(u_2 = ws, e_a = 2.85, T_min = temp_min, T_max = temp_max,
                        phi_deg = latdeg, elev = h, date = pdate, n = actsunshine, N = maxdaylight)
## Crop ET     
CrET = ET_c(Kc = Kc_Cereals$Kc_ini[12], ETo = ET_ref)
```