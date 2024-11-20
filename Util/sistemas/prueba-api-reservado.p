DEF VAR pComprometido AS DECI.
DEF VAR pcodmat AS CHAR INIT '082950'.
DEF VAR pcodalm AS CHAR INIT '05'.

FIND almmmate WHERE codcia = 1
    AND codmat = pcodmat
    AND codalm = pcodalm
    NO-LOCK.


RUN gn/stock-comprometido-v2 (pcodmat,
                              pcodalm,
                              YES,
                              OUTPUT pComprometido).


MESSAGE almmmate.stkact - pComprometido.
