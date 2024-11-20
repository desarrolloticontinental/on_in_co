DISABLE TRIGGERS FOR LOAD OF ccbcdocu.
DISABLE TRIGGERS FOR LOAD OF ccbddocu.
DEF BUFFER B-CDOCU FOR ccbcdocu.

FOR EACH ccbcdocu WHERE codcia = 1
    AND coddoc = 'G/R'
    AND nrodoc >= '014070087'
    AND nrodoc <= '014070140'
    AND flgest <> "A":
    DISPLAY coddoc nrodoc codalm fchvto flgest imptot.
    ASSIGN 
       Ccbcdocu.FlgEst = "A"
       Ccbcdocu.SdoAct = 0
       Ccbcdocu.Glosa  = "A N U L A D O"
       Ccbcdocu.FchAnu = TODAY
       Ccbcdocu.Usuanu = "CONTAB". 
    /* extornamos FAC o BOL */
    FIND B-CDOCU WHERE B-CDOCU.codcia = Ccbcdocu.codcia
        AND B-CDOCU.coddoc = Ccbcdocu.codref
        AND B-CDOCU.nrodoc = Ccbcdocu.nroref
        AND B-CDOCU.codref = Ccbcdocu.coddoc
        AND B-CDOCU.nroref = Ccbcdocu.nrodoc
        EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE B-CDOCU THEN
    ASSIGN
        B-CDOCU.codref = ""
        B-CDOCU.nroref = "".
    IF AVAILABLE(B-CDOCU) THEN RELEASE B-CDOCU.
END.

