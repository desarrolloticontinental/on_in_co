
# note: it is recommended to copy the scripts to another local directory
# because this directory and files will be overwritten when upgraded.

DLC=/train/dlc92;export DLC
PATH=$DLC/bin:$PATH
WRKDIR=/tmp;export WRKDIR
PROPATH=/p/ssb/prog,.;export PROPATH

# if run from crontab, term might not be set and will cause progress to fail.
TERM=vt100;export TERM

cd $WRKDIR



DB_SET_NAME=ssb;export DB_SET_NAME
BAK_ROOT_DIR=/backup;export BAK_ROOT_DIR

# DB<n>=<database>,<optional logical name>,<optional port>;export DB<n>
DB01=/p/ssb/data/ssbdata;export DB01
DB02=/p/ssb/para/ssbpara;export DB02
DB03=/train/work/ssbwork;export DB03
DB04=/p/hist/history;export DB04



_progres -b -p slib/baksys/backup-aimage.p

# tail -f $BAK_ROOT_DIR/backup-aimage.lg
