DEF BUFFER b-cped FOR faccpedi.
DEF BUFFER b-dped FOR facdpedi.

FIND faccpedi WHERE codcia = 1 
    AND coddoc = 'cot'
    AND nroped = '124060719' NO-LOCK.
FIND b-cped WHERE b-cped.codcia = 001
    AND b-cped.coddoc = 'cot'
    AND b-cped.nroped = '124062172'.
FOR EACH b-dped OF b-cped:
    DELETE b-dped.
END.
BUFFER-COPY faccpedi 
    EXCEPT faccpedi.nroped faccpedi.codcli faccpedi.ruccli faccpedi.dircli
    TO b-cped.
FOR EACH facdpedi OF faccpedi NO-LOCK:
    CREATE b-dped.
    BUFFER-COPY facdpedi 
        TO b-dped 
        ASSIGN 
        b-dped.nroped = b-cped.nroped 
        b-dped.codcli = b-cped.codcli
        b-dped.canate = 0.
END.
