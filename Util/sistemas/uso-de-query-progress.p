DEFINE VAR a AS INT64 NO-UNDO.

a = ETIME(YES).

DEF TEMP-TABLE t-matg
    FIELD codcia AS INTE
    FIELD codmat AS CHAR.

DEFINE VAR h-query AS HANDLE.

create query h-query. 

h-query:set-buffers(BUFFER almmmatg:HANDLE). 

h-query:query-prepare("for each almmmatg no-lock where codcia = 1 and codfam = '010'"). 

h-query:query-open().
h-query:get-first().
do while not h-query:query-off-end:
  CREATE t-matg.
  t-matg.codcia = almmmatg.codcia.
  t-matg.codmat = almmmatg.codmat.

  h-query:get-next().
END.
h-query:query-close() no-error.

/*
FOR EACH t-matg.
    DISPLAY t-matg.codcia t-matg.codmat.
END.
*/

DELETE OBJECT h-Query.

MESSAGE ETIME.
