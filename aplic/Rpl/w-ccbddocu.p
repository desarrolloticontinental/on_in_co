TRIGGER PROCEDURE FOR REPLICATION-WRITE OF CCBDDOCU.
/*
FIND ccbcdocu OF ccbddocu NO-LOCK NO-ERROR.
IF AVAILABLE ccbcdocu 
    THEN DO:
    {rpl/reptrig.i
    &Table  = ccbddocu
    &Key    =  "string(ccbddocu.codcia,'999') + string(ccbddocu.coddiv,'x(5)') ~
        + string(ccbddocu.coddoc,'x(3)') + string(ccbddocu.nrodoc,'x(15)') ~
        + string(ccbddocu.codmat,'x(6)')"
    &Prg    = r-ccbddocu
    &Event  = WRITE
    &FlgDB0 = NO
    &FlgDB1 = TRUE
    &FlgDB2 = TRUE
    &FlgDB3 = TRUE
    &FlgDB4 = TRUE
    &FlgDB5 = TRUE
    &FlgDB6 = TRUE
    &FlgDB7 = TRUE
    &FlgDB8 = TRUE
    &FlgDB9 = TRUE
    &FlgDB10 = TRUE
    &FlgDB11 = TRUE
    &FlgDB12 = TRUE
    &FlgDB13 = TRUE
    &FlgDB14 = TRUE
    &FlgDB15 = TRUE
    &FlgDB16 = TRUE
    &FlgDB17 = TRUE
    &FlgDB18 = TRUE
    &FlgDB19 = TRUE
    &FlgDB20 = TRUE
    &FlgDB30 = TRUE
    }
END.
    
*/
