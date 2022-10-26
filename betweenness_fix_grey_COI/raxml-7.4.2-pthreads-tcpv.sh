#! /bin/bash

### Executable name
RAXML_BIN="raxmlHPC-PTHREADS-AVX"
### Number of threads you want to run
### (I set to 64, because steelhead has 64 cores)
NPROCS=8

### Set program options
RAXML_SEQ="COI_nataly_brenda_JANESSA.DEDUP.trim.phy"
RAXML_OUT="COI_nataly_brenda_JANESSA.DEDUP.trim.out"
### Additional options
### **DO NOT ENTER -s or -n options
### Those are defined above
RAXML_OPTS="-o HM400306_Dipsacaster_borealis_voucher_RBCM_EC00025 -m GTRGAMMA -# 100 -p 626 -f a -x 626"

### run the program
$RAXML_BIN -s $RAXML_SEQ -n $RAXML_OUT -T $NPROCS $RAXML_OPTS
