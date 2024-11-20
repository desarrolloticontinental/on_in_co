/*
DEF INPUT PARAMETER pCodMat AS CHAR.
DEF INPUT PARAMETER pFchDoc AS DATE.
*/
DEF VAR x-fchdoc AS DATE.
DEF VAR x-codmat AS CHAR FORMAT 'x(6)'.
DEF VAR pMensaje AS CHAR FORMAT 'x(30)' NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF almcmov.
DISABLE TRIGGERS FOR LOAD OF almdmov.
DISABLE TRIGGERS FOR LOAD OF almstkal.
DISABLE TRIGGERS FOR LOAD OF almstkge.
DISABLE TRIGGERS FOR LOAD OF almmmate.
DISABLE TRIGGERS FOR LOAD OF almmmatg.

x-fchdoc = DATE(01,01,2020).
INPUT FROM d:\codigosMax08Ago2023.prn.
REPEAT :
    IMPORT x-codmat.
    IF x-codmat = '' THEN LEAVE.
    /*DISPLAY x-codmat pmensaje. PAUSE 0.*/
    RUN alm/calc-costo-promedio (x-codmat, x-fchdoc,OUTPUT pmensaje).
    
END.
INPUT CLOSE.

MESSAGE "Proceso Terminado".
