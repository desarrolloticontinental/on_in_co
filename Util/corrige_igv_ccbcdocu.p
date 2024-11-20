FOR EACH CCBCDOCU WHERE
    CODCIA = 1 AND
    CODDOC = "FAC" AND
    /*
    FCHDOC = 10/02/07 AND
    */
    NRODOC = "015100994" :
    
    UPDATE
        /*
        CODCIA
        CODDOC
        NRODOC
        FCHDOC
        FLGEST
        IMPTOT
        FMAPGO.
        */
        CCBCDOCU
        WITH 2 COL /* WIDTH STREAM-IO */.
      
      /*
    FOR EACH CCBDDOCU OF CCBCDOCU:
        UPDATE
            CCBDDOCU
            WITH 2 COL /* STREAM-IO  */ .
    END.
    */
    
END.
