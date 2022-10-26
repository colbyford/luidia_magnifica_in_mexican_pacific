#! /bin/bash

### Executable name
RAXML_BIN="raxmlHPC-PTHREADS-AVX"
### Number of threads you want to run
### (I set to 64, because steelhead has 64 cores)
NPROCS=4

### Set program options
RAXML_SEQ="LMcat_karen_galvan_RC_v7_Dipsacaster_outDEDUP.Janesssa.phy"
RAXML_OUT="LMcat_karen_galvan_RC_v7_Dipsacaster_outDEDUP.Janesssa.out"
### Additional options
### **DO NOT ENTER -s or -n options
### Those are defined above
RAXML_OPTS="-o EU722960_Dipsacaster_borealis_voucher_CASIZ_143420 -m GTRGAMMA -# 100 -p 72 -f a -x 72"

### run the program
$RAXML_BIN -s $RAXML_SEQ -n $RAXML_OUT -T $NPROCS $RAXML_OPTS
