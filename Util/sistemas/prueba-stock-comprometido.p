DEF VAR pComprometido AS DECI.
DEF NEW SHARED VAR s-codcia AS INTE INIT 001.
DEF VAR pcodmat AS CHAR.
DEF VAR pcodalm AS CHAR.

DEF VAR a AS INT64.

pcodmat = '200193'.
pcodalm = '05'.

a = ETIME(YES).
RUN gn/stock-comprometido-v2 (pcodmat,
                              pcodalm,
                              YES,
                              OUTPUT pComprometido).

MESSAGE 'normal' ETIME.

a = ETIME(YES).
RUN gn/stock-comprometido-v2-select (pcodmat,
                                     pcodalm,
                                     YES,
                                     OUTPUT pComprometido).

MESSAGE 'select' ETIME.

a = ETIME(YES).
RUN gn/stock-comprometido-v2-query (pcodmat,
                                     pcodalm,
                                     YES,
                                     OUTPUT pComprometido).
MESSAGE 'query' ETIME.
