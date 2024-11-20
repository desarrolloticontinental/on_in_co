DEF TEMP-TABLE t-vtalistamingn LIKE vtalistamingn
    INDEX Llave01 AS PRIMARY CodCia CodMat.
DEF TEMP-TABLE t-mingn LIKE vtalistamingn
    FIELD logdate AS DATETIME
    INDEX Llave01 AS PRIMARY CodCia CodMat.

FOR EACH logtransaction NO-LOCK WHERE tablename = 'vtalistamingn'
    AND logdate >= DATETIME(STRING(DATE(12,07,2019)) + ' 00:00:00'):
    CREATE t-vtalistamingn.
    RAW-TRANSFER DataRecord TO t-vtalistamingn.
    CREATE t-mingn.
    BUFFER-COPY t-vtalistamingn TO t-mingn
        ASSIGN
        t-mingn.userupdate = logtransaction.usuario
        t-mingn.logdate = logtransaction.logdate.
END.
FOR EACH t-mingn WHERE codcia = 1 AND codmat = '100428':
    DISPLAY codmat preofi userupdate t-mingn.logdate
         WITH STREAM-IO NO-BOX WIDTH 320.
END.

