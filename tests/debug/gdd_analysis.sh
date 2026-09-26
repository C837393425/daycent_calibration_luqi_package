#!/bin/bash

echo "================================================================================"
echo "GDD MATURITY ANALYSIS - Why 2013 thermunits dropped earlier"
echo "================================================================================"
echo ""
echo "CM2_11 Corn GDD Requirements:"
echo "  DDBASE   = 553.3  (Base GDD requirement)"
echo "  MNDDHRV  = 500.2  (Minimum GDD for harvest)"
echo "  MXDDHRV  = 626.0  (Maximum GDD for harvest)"
echo ""
echo "Total GDD requirement for maturity = DDBASE + MXDDHRV = 553.3 + 626.0 = 1,179.3"
echo ""
echo "================================================================================"
echo ""

for year in 2013 2015 2018; do
    echo "Year $year:"
    echo "--------------------------------------------------------------------------------"
    
    # Get planting day
    if [ $year -eq 2013 ]; then
        plant_day=134
        harv_day=271
    else
        plant_day=117
        harv_day=254
    fi
    
    echo "  Planting day: $plant_day"
    echo "  Harvest day:  $harv_day"
    echo ""
    
    # Find when GDD requirement was reached (around 1179)
    awk -v y=$year -v pd=$plant_day -v hd=$harv_day '
    $1 ~ "^"y"\\." && $2 >= pd {
        therm = $9
        day = $2
        
        if (therm >= 1179 && !reached) {
            printf "  GDD requirement (1,179) reached: Day %d (thermunits = %.1f)\n", day, therm
            printf "    -> %d days after planting\n", day - pd
            reached = 1
        }
        
        if (day == hd) {
            printf "  Thermunits at harvest (Day %d): %.1f\n", day, therm
        }
        
        if (therm < 10 && prev_therm > 100 && !dropped) {
            printf "  Thermunits dropped to 0: Day %d (previous: %.1f)\n", day, prev_therm
            printf "    -> %d days BEFORE scheduled harvest\n", hd - day
            dropped = 1
        }
        
        prev_therm = therm
    }
    ' /data/rubelscratch/rubelogle/daycent_calibration/scratch/run_20251031_112845/877088/877088/daily.out
    
    echo ""
done

echo "================================================================================"
echo "CONCLUSION:"
echo "================================================================================"
echo ""
echo "If 2013 reached the GDD requirement (1,179 GDD) significantly earlier than"
echo "2015 and 2018, then DayCent would trigger physiological maturity and stop"
echo "accumulating thermal units, causing the early drop to 0 you observed."
echo ""
echo "This could be due to:"
echo "  1. Higher temperatures in 2013 accumulating GDD faster"
echo "  2. Stress-accelerated maturity (drought stress can speed up senescence)"
echo "  3. Earlier planting relative to temperature patterns"
echo ""
echo "================================================================================"

