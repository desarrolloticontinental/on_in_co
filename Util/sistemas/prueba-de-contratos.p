DEF VAR pMensaje AS CHAR.
DEF NEW SHARED VAR s-user-id AS CHAR.

s-user-id = 'ADMIN'.

RUN pln/p-contratos-word ('000731',
                          01,
                          2022,
                          01,
                          DATE(01,01,2022),
                          DATE(12,31,2022),
                          OUTPUT pMensaje).
MESSAGE RETURN-VALUE pMensaje.
