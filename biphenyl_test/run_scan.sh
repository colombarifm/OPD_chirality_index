#!/bin/bash

printf "%4s\t%12s\t%12s\n" "#deg" "#OPD_index" "#scaled_OPD_index" > opd_scan.log

for n in $( seq 1 1 37 )
do 
  
  n_value=$( printf "%03d" ${n} )

  ../build/bin/opd --input relax.${n_value}.xyz > results.log

  deg=$( echo "(${n}-1)*5" | bc -l )

  osipov_index=$( grep "Osipov index" results.log | head -1 | awk '{printf $5}' )
  scaled_osipov_index=$( grep "Scaled Osipov index" results.log | awk '{printf $6}' )

  printf "%4i\t%12.6f\t%12.6f\n" ${deg} ${osipov_index} ${scaled_osipov_index} >> opd_scan.log

done
