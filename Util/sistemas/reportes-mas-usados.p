DEF VAR x-username AS CHAR.
OUTPUT TO d:\consumo.txt.
FOR EACH logtabla NO-LOCK WHERE
    codcia = 1 and 
    evento = 'RUN-PROGRAM' and 
    dia >= 01/01/2021 and 
    (
    index(valorllave, 'w-consulta-de-ordenes') > 0 OR 
    index(valorllave, 'wconcotcred') > 0 OR 
    index(valorllave, 'w-listado-picking-ordenes-subordenes-v2') > 0 OR 
    index(valorllave, 'w-listado-picking-ordenes-v3') > 0 OR
    INDEX(valorllave, 'w-rsm-seguimiento-entregas') > 0 OR
    INDEX(valorllave, 'w-seguimiento-de-pedidos') > 0
    )
    :
    x-username = ''.
    FIND _user WHERE _user._USERID = usuario NO-LOCK NO-ERROR.
    IF AVAILABLE _User THEN x-username = _User._User-Name.
    EXPORT DELIMITER ";" tabla dia hora usuario x-username valorllave.
END.
OUTPUT CLOSE.
