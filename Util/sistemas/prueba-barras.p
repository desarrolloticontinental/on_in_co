DEF NEW SHARED VAR s-codcia AS INTE INIT 001.

DEF VAR pcodmat AS CHAR.
DEF VAR pcantidad AS DEC.

DEF VAR a AS INT64.
DEF VAR b AS INT64.
DEF VAR c AS INT64.

DEF VAR X AS CHAR.
DEF VAR Y AS CHAR.

pcodmat = "7750822010840".

a = ETIME(YES).

X = pcodmat.
RUN vta2/p-codigo-producto (INPUT-OUTPUT X,
                             NO).

b = ETIME.

a = ETIME(YES).

Y = pcodmat.
RUN alm/p-codbrr (INPUT-OUTPUT Y,
                  INPUT-OUTPUT pcantidad,
                  INPUT s-codcia).

c = ETIME.

MESSAGE b c SKIP pcodmat SKIP X SKIP Y pcantidad .


